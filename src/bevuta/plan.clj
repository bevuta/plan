(ns bevuta.plan
  (:refer-clojure :exclude [def defn])
  (:require [clojure.core :as c]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [bevuta.interceptors :as i]))


;; Snatched from `clojure.spec` and slightly refactored
(c/defn qualify-symbol
  "Qualify symbol s by resolving it or using the current *ns*."
  [sym]
  (if-let [ns-sym (some-> sym namespace symbol)]
    (or (some-> (get (ns-aliases *ns*) ns-sym)
                ns-name
                name
                (symbol (name sym)))
        sym)
    (symbol (name (ns-name *ns*)) (name sym))))

(c/defn unqualify-symbol [s]
  (symbol (name s)))

;; TODO: Introduce bevuta.plan.step ns
(s/def ::step-name
  (s/and symbol? (s/conformer qualify-symbol)))

(s/def ::dep ::step-name)

(s/def ::deps
  (s/coll-of ::dep))

(s/def ::fn ifn?)

(s/def ::value any?)

(s/def ::alias ::step-name)

(s/def ::step
  (s/keys :req-un [(or ::deps ::value (and ::fn ::deps) ::alias)]))

(s/def ::step*
  (s/keys* :opt-un [::deps]))

(s/def ::inputs
  (s/map-of ::step-name ::value
            :conform-keys true))

(s/def ::goals
  (s/and (s/or :multi  (s/coll-of ::step-name :into #{})
               :single (s/and ::step-name (s/conformer vector) ::goals))
         (s/conformer val)))

(s/def ::overrides
  (s/map-of ::step-name ::step
            :conform-keys true))

(def step-registry
  (atom {}))

(c/defn asserting-conform
  [spec value]
  (s/assert spec value)
  (s/conform spec value))

(c/defn quote-value [v]
  (list 'quote v))

(c/defn quote-step [step]
  (if (:deps step)
    (update step :deps (partial mapv quote-value))
    step))

(s/fdef def
        :args (s/cat :name ::step-name
                     :step ::step*))

;; TODO: Detect cycles here?
(defmacro def
  [step-name & definition]
  (let [step-name (s/conform ::step-name step-name)
        step (s/conform ::step* definition)]
    `(swap! step-registry
            assoc
            '~step-name
            ~(quote-step step))))

(s/def ::dep-destructuring
  (s/keys :req [::dep]))

(s/fdef defn
        :args (s/cat :name ::step-name
                     :args (s/coll-of (s/or :symbol ::step-name
                                            :destructuring-map ::dep-destructuring)
                                      :kind vector?)
                     :body (s/+ any?)))

(defmacro defn [step-name args & body]
  (let [deps (map (fn [arg]
                    (if (symbol? arg)
                      arg
                      (::dep arg)))
                  args)
        argv (mapv (fn [arg]
                     (if (symbol? arg)
                       (unqualify-symbol arg)
                       (dissoc arg ::dep)))
                   args)]
    `(do (bevuta.plan/def ~step-name :deps ~deps)
         (c/defn ~step-name ~argv ~@body))))

(c/defn resolve-var [var-name]
  (if-let [var (resolve var-name)]
    @var
    (throw (ex-info (str "Unable to resolve var " var-name)
                    {:name var-name}))))

(c/defn resolve-step-fn [step-name step]
  (or (:fn step)
      (resolve-var (or (:alias step) step-name))))

(def step-fn-interceptor
  {:enter (fn [ctx]
            (let [{::keys [step-fn step-args]} ctx]
              (assoc ctx ::value (apply step-fn step-args))))})

(defprotocol Strategy
  (realize-step [_ ctx])
  (step-result [_ step-ref])
  (step-done? [_ step-ref]))

(defn call-step-fn [ctx]
  (i/execute ctx))

(def in-sequence
  (reify
    Object
    (toString [_] "in-sequence")
    Strategy
    (realize-step [_ ctx]
      (call-step-fn ctx))
    (step-result [_ result]
      result)
    (step-done? [_ _]
      true)))

(def in-parallel
  (reify
    Object
    (toString [_] "in-parallel")
    Strategy
    (realize-step [_ ctx]
      (future (call-step-fn ctx)))
    (step-result [_ step-future]
      (try
        @step-future
        ;; TODO: Is this a good idea? ;-)
        (catch java.util.concurrent.ExecutionException e
          (throw (.getCause e)))))
    (step-done? [_ step-future]
      (future-done? step-future))))

(defn add-interceptors [plan & interceptors]
  (update plan ::interceptors (fnil into []) interceptors))

(defmacro get-or [map key else]
  `(if-let [[_# value#] (find ~map ~key)]
     value#
     ~else))

(defn dependency-val [strategy ctx step-name]
  (get-or (::inputs ctx)
          step-name
          (::value (step-result strategy (get-in ctx [::results step-name])))))

;; NOTE: We rely on `map`s laziness here so that steps are actually
;; only dereferenced when `realize-step` realizes the `step-args`
;; sequence. This allows it to defer potentially blocking derefs to
;; another thread, for example.
(c/defn realize-steps [strategy inputs {::keys [steps interceptors]}]
  (let [interceptors (conj interceptors step-fn-interceptor)]
    (loop [{::keys [steps inputs results] :as ctx}
           {::steps steps
            ::inputs inputs
            ::results {}}]
      (if (seq steps)
        (let [[step-name step] (first steps)]
          (if-let [step-deps (:deps step)]
            (let [step-fn   (resolve-step-fn step-name step)
                  step-args (map (partial dependency-val strategy ctx) step-deps)
                  step-ctx  {::step-name    step-name
                             ::step-deps    step-deps
                             ::step-fn      step-fn
                             ::step-args    step-args
                             ::i/queue      interceptors}
                  result    (realize-step strategy step-ctx)]
              (recur (-> ctx
                         (assoc-in [::results step-name] result)
                         (update ::steps rest))))
            (recur (cond-> (update ctx ::steps rest)
                     (not (contains? inputs step-name))
                     (assoc-in [::inputs step-name]
                               (get-or step
                                       :value
                                       (resolve-var step-name)))))))
        (let [results (into {}
                            (map (fn [[step-name result]]
                                   [step-name (step-result strategy result)]))
                            results)
              values (into inputs
                           (map (fn [[step-name result]]
                                  [step-name (::value result)]))
                           results)]
          (with-meta values {::results results}))))))

(c/defn resolve-step-alias [[step-name step] all-steps]
  (clojure.lang.MapEntry.
   step-name
   (loop [alias (:alias step)
          step step]
     (if alias
       (let [step' (get all-steps alias)]
         (recur (:alias step') (merge step step')))
       step))))

(c/defn devise-1 [all-steps overrides visited inputs goal]
  (loop [steps ()
         visited visited
         inputs inputs
         deps [goal]]
    (if (empty? deps)
      [steps visited inputs]
      (let [dep (first deps)]
        (if (contains? visited dep)
          (recur (remove #(= dep (key %)) steps)
                 (disj visited dep)
                 inputs
                 deps)
          (if-let [dep-step (some-> (or (find overrides dep)
                                        (find all-steps dep))
                                    (resolve-step-alias all-steps))]
            (recur (conj steps dep-step)
                   (conj visited dep)
                   inputs
                   (concat (rest deps)
                           (-> dep-step val :deps)))
            (recur steps
                   (conj visited dep)
                   (conj inputs dep)
                   (rest deps))))))))

;; TODO: Or (also) detect cycles here?
;; TODO: Fail when goals contain (only?) inputs?
(c/defn devise
  ([goals]
   (devise {} goals))
  ([overrides goals]
   (let [overrides (asserting-conform ::overrides overrides)
         all-goals (asserting-conform ::goals goals)
         all-steps @step-registry]
     (loop [steps   ()
            inputs  #{}
            visited #{}
            goals all-goals]
       (if (empty? goals)
         #::{:inputs inputs
             :goals all-goals
             :overrides overrides
             :steps steps}
         (let [[steps' visited inputs] (devise-1 all-steps overrides visited inputs (first goals))]
           (recur (concat steps steps')
                  inputs
                  visited
                  (rest goals))))))))

(c/defn validated-inputs [expected-inputs inputs]
  (let [inputs (asserting-conform ::inputs inputs)]
    (when-let [missing (seq (remove (set (keys inputs)) expected-inputs))]
      (throw (ex-info "Missing plan inputs" {:missing missing})))
    inputs))

(c/defn realize
  ([strategy plan]
   (realize strategy plan {}))
  ([strategy plan inputs]
   (let [inputs (validated-inputs (::inputs plan) inputs)]
     (realize-steps strategy inputs plan))))

(c/defn get-step-spec
  ([step-name step]
   (or (:spec step)
       (when-let [s (s/get-spec step-name)]
         (or (:ret s) s))))
  ([[step-name step]]
   (get-step-spec step-name step)))

(defmacro fdefs []
  (let [all-steps @step-registry
        ns-steps (->> (ns-interns *ns*)
                      (keys)
                      (map #(symbol (name (ns-name *ns*)) (name %)))
                      (filter all-steps))
        ns-plan (devise ns-steps)
        ns-step-specs (->> (::inputs ns-plan)
                           (map (juxt identity (partial get all-steps)))
                           (concat (::steps ns-plan))
                           (map (juxt first get-step-spec))
                           (into {}))]
    (when-let [missing (->> ns-step-specs (remove val) (map first) seq)]
      (throw (ex-info "Missing specs for some steps"
                      {:steps missing})))
    `(do
       ~@(->> (::steps ns-plan)
              (filter (comp :deps second))
              (map (fn [[step-name step]]
                     `(s/fdef ~step-name
                              :args (s/cat ~@(mapcat (fn [dep-name]
                                                       [(keyword (str dep-name))
                                                        (get ns-step-specs dep-name)])
                                                     (:deps step)))
                              ~@(when-let [fn-spec (:fn (s/get-spec step-name))]
                                  [:fn fn-spec])
                              :ret ~(get ns-step-specs step-name))))))))
