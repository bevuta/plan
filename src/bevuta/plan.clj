(ns bevuta.plan
  (:refer-clojure :exclude [def defn])
  (:require [clojure.core :as c]
            [clojure.spec :as s]
            [clojure.set :as set]))


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

(s/def ::step-name
  (s/and symbol? (s/conformer qualify-symbol)))

(s/def ::dep ::step-name)

(s/def ::deps
  (s/coll-of ::dep))

(s/def ::fn ifn?)

(s/def ::value any?)

(s/def ::step
  (s/keys :req-un [(or ::deps ::value (and ::fn ::deps))]))

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
      (resolve-var step-name)))

(defprotocol Strategy
  (realize-step [_ ctx step-fn args])
  (step-result [_ step-ref])
  (step-result-done? [_ step-ref]))

(def in-sequence
  (reify
    Object
    (toString [_] "in-sequence")
    Strategy
    (realize-step [_ ctx step-fn args]
      (apply step-fn args))
    (step-result [_ result]
      result)
    (step-result-done? [_ _]
      true)))

(def in-parallel
  (reify
    Object
    (toString [_] "in-parallel")
    Strategy
    (realize-step [_ ctx step-fn args]
      (future (apply step-fn args)))
    (step-result [_ step-future]
      @step-future)
    (step-result-done? [_ step-future]
      (future-done? step-future))))

(defn wrap [strategy middleware]
  (reify Strategy
    Object
    (toString [_] (str strategy))
    Strategy
    (realize-step [_ ctx step-fn args]
      (middleware ctx #(realize-step strategy %1 step-fn %2) args))
    (step-result [_ step]
      (step-result strategy step))
    (step-result-done? [_ step-ref]
      (step-result-done? strategy step-ref))))

(defmacro get-or [map key else]
  `(if-let [[_# value#] (find ~map ~key)]
     value#
     ~else))

(defn step-val [strategy ctx step-name]
  (get-or (::inputs ctx)
          step-name
          (step-result strategy (get-in ctx [::results step-name]))))

;; NOTE: We rely on `map`s laziness here so that steps are actually
;; only dereferenced when `realize-step` realizes the `args`
;; sequence. This allows it to defer potentially blocking derefs to
;; another thread, for example.
(c/defn realize-steps [strategy inputs steps]
  (loop [{::keys [steps inputs results]
          :as ctx}
         {::steps steps
          ::inputs inputs
          ::results {}}]
    (if (seq steps)
      (let [[step-name step] (first steps)]
        (if-let [deps (:deps step)]
          (let [step-fn (resolve-step-fn step-name step)
                ctx     (assoc ctx ::step-name step-name)
                args    (map (partial step-val strategy ctx) deps)
                result  (realize-step strategy ctx step-fn args)]
            (recur (-> ctx
                       (update ::steps rest)
                       (assoc-in [::results step-name] result))))
          (recur (cond-> (update ctx ::steps rest)
                   (not (contains? inputs step-name))
                   (assoc-in [::inputs step-name]
                             (get-or step
                                     :value
                                     (resolve-var step-name)))))))
      (into inputs
            (map (fn [[k v]]
                   [k (step-result strategy v)]))
            results))))

(c/defn devise-1 [all-steps overrides visited inputs goal]
  (loop [steps ()
         visited visited
         inputs inputs
         deps [goal]]
    (if (empty? deps)
      [steps visited inputs]
      (let [dep (first deps)]
        (if (contains? visited dep)
          (recur steps
                 visited
                 inputs
                 (rest deps))
          (if-let [dep-step (or (find overrides dep)
                                (find all-steps dep))]
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
     (realize-steps strategy inputs (::steps plan)))))

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
