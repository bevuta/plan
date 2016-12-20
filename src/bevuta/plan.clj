(ns bevuta.plan
  (:refer-clojure :exclude [def defn compile])
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
  (s/keys :req-un [(or ::deps (and ::fn ::deps))]
          :opt-un [::value ::spec]))

(s/def ::step*
  (s/keys* :opt-un [::deps]))

(s/def ::inputs
  (s/map-of ::step-name ::value
            :conform-keys true))

(s/def ::goals
  (s/and (s/or :single (s/and ::step-name (s/conformer vector))
               :multi  (s/coll-of ::step-name :into #{}))
         (s/conformer val)))

(s/def ::overrides
  (s/map-of ::step-name (s/and ::step (s/conformer val))
            :conform-keys true))

(defonce step-registry
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
  ([step-name]
   `(bevuta.plan/def ~step-name {}))
  ([step-name & definition]
   (let [step-name (s/conform ::step-name step-name)
         step (s/conform ::step* definition)]
     `(swap! step-registry
             assoc
             '~step-name
             ~(quote-step step)))))

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

(create-ns 'bevuta.plan.strategy)
(alias 'strategy 'bevuta.plan.strategy)

(def in-sequence
  #::strategy{:compile-step cons
              :compile-deref-result identity
              :eval-step apply
              :deref-result identity})

(def in-parallel
  #::strategy{:compile-step (fn [step-fn args]
                              `(future (~step-fn ~@args)))
              :compile-deref-result (fn [result]
                                      `(deref ~result))
              :eval-step (fn [step-fn args]
                           (future (apply step-fn args)))
              :deref-result deref})

(defmacro get-or [map key else]
  `(if-let [[_# value#] (find ~map ~key)]
     value#
     ~else))

;; NOTE: We rely on `map`s laziness here so that steps are actually
;; only dereferenced when `eval-step` realizes the args sequence. This
;; allows it to defer potentially blocking derefs to e.g. another
;; thread.
(c/defn realize-steps [strategy inputs steps]
  (let [{::strategy/keys [eval-step deref-result]} strategy
        deref-arg (fn [inputs results step-name]
                    (get-or inputs
                            step-name
                            (deref-result (get results step-name))))]
    (loop [steps   steps
           inputs  inputs
           results {}]
      (if (seq steps)
        (let [[step-name step] (first steps)]
          (if-let [deps (:deps step)]
            (let [step-fn (resolve-step-fn step-name step)
                  args    (map (partial deref-arg inputs results) deps)
                  result  (eval-step step-fn args)]
              (recur (rest steps)
                     inputs
                     (assoc results step-name result)))
            (recur (rest steps)
                   (if (contains? inputs step-name)
                     inputs
                     (assoc inputs
                            step-name
                            (get-or step
                                    :value
                                    (resolve-var step-name))))
                   results)))
        (into inputs
              (map (fn [[k v]]
                     [k (deref-result v)]))
              results)))))

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

(defmacro compile [strategy plan]
  (let [{::keys [steps inputs overrides]} (eval plan)
        locals (into {}
                     (map (fn [step-name]
                            [step-name (gensym (name step-name))]))
                     (concat inputs (map first steps)))
        static-steps (set (keep (fn [[step-name step]]
                                  (when-not (:deps step)
                                    step-name))
                                steps))
        {::strategy/keys [compile-step compile-deref-result]} (eval strategy)
        compile-deref-step (fn [step-name]
                             (let [local (get locals step-name)]
                               (if (or (contains? inputs step-name)
                                       (contains? static-steps step-name))
                                 local
                                 (compile-deref-result local))))
        inputs-sym (gensym 'inputs)]
    `(let [expected-inputs# '~inputs]
       (fn [~inputs-sym]
         (let [~(->> (select-keys locals inputs)
                     (map (fn [[qname local]]
                            [local (quote-value qname)]))
                     (into {})) (validated-inputs expected-inputs# ~inputs-sym)
               ~@(mapcat (fn [[name step]]
                           [(get locals name)
                            (if (contains? static-steps name)
                              `(or (get ~inputs-sym '~name) ~name)
                              (let [override (get overrides name)]
                                (get-or override
                                        :value
                                        (let [step-fn (or (:fn override) name)
                                              args (map compile-deref-step
                                                        (:deps step))]
                                          (compile-step step-fn args)))))])
                         steps)]
           ~(into {}
                  (map (fn [step-name]
                         [(quote-value step-name)
                          (compile-deref-step step-name)]))
                  (keys locals)))))))

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
