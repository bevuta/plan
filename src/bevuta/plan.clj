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

(s/def ::inject
  (s/or :anonymous (s/keys :req-un [::fn]
                           :opt-un [::deps])
        :named ::step-name))

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

(create-ns 'bevuta.plan.override)
(alias 'override 'bevuta.plan.override)

(s/def ::override/replace
  (s/map-of ::step-name ::step
            :conform-keys true))

(s/def ::override/alias
  (s/map-of ::step-name ::step-name
            :conform-keys true))

(s/def ::override/inject
  (s/map-of ::step-name ::inject
            :conform-keys true))

(s/def ::overrides
  (s/keys :opt-un [::override/replace
                   ::override/alias
                   ::override/inject]))

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

(defmacro def
  [step-name & definition]
  (let [step-name (s/conform ::step-name step-name)
        step (or (s/conform ::step* definition) {})]
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
                    {::var-name var-name}))))

(c/defn resolve-step-fn [step]
  (or (:fn step)
      (resolve-var (:name step))))

(def step-fn-interceptor
  {:enter (fn [ctx]
            (let [{::keys [step-fn step-args]} ctx]
              (assoc ctx ::value (apply step-fn step-args))))})

(defprotocol Strategy
  (realize-step [_ ctx])
  (step-result [_ step-ref])
  (step-done? [_ step-ref]))

(c/defn call-step-fn [ctx]
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

(c/defn add-interceptors [plan & interceptors]
  (update plan ::interceptors (fnil into []) interceptors))

(defmacro get-or [map key else]
  `(if-let [[_# value#] (find ~map ~key)]
     value#
     ~else))

(c/defn dependency-val [strategy ctx step-name]
  (get-or (::inputs ctx)
          step-name
          (::value (step-result strategy (get-in ctx [::results step-name])))))

;; NOTE: We rely on `map`s laziness here so that steps are actually
;; only dereferenced when `realize-step` realizes the `step-args`
;; sequence. This allows it to defer potentially blocking derefs to
;; another thread, for example.
(c/defn realize-steps [strategy inputs plan]
  (let [interceptors (conj (::interceptors plan) step-fn-interceptor)
        steps (::steps plan)]
    (loop [{::keys [order inputs results] :as ctx}
           {::inputs inputs
            ::order (::order plan)
            ::results {}}]
      (if (seq order)
        (let [step-name (first order)
              step      (get steps step-name)]
          (if-let [step-deps (:deps step)]
            (let [step-fn   (resolve-step-fn step)
                  step-args (map (partial dependency-val strategy ctx) step-deps)
                  step-ctx  {::step-name    step-name
                             ::step-deps    step-deps
                             ::step-fn      step-fn
                             ::step-args    step-args
                             ::i/queue      interceptors}
                  result    (realize-step strategy step-ctx)]
              (recur (-> ctx
                         (assoc-in [::results step-name] result)
                         (update ::order rest))))
            (recur (cond-> (update ctx ::order rest)
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

(c/defn resolve-step-alias [step all-steps]
  (if-let [alias (:alias step)]
    (assoc step
           :deps [alias]
           :fn identity)
    step))

(c/defn apply-injections [step injections]
  (let [injected-step (get injections (:name step))]
    (cond-> step
      injected-step
      (assoc :injected-step injected-step)

      (contains? step :deps)
      (update :deps (fn [deps]
                      (mapv (fn [dep]
                              (or (when-let [injected-dep (get injections dep)]
                                    (when-not (= injected-dep (:name step))
                                      injected-dep))
                                  dep))
                            deps))))))

(c/defn process-overrides [overrides]
  (reduce (fn [overrides [step-name [inject-kind inject]]]
            (let [injected-step-name (case inject-kind
                                       :named inject
                                       :anonymous (gensym (str step-name "-inject")))
                  inject (case inject-kind
                           :named inject
                           :anonymous (cond-> inject
                                        (not (contains? inject :deps))
                                        (assoc :deps [step-name])))]
              (cond-> overrides
                true
                (assoc-in [:inject step-name] injected-step-name)

                (= :anonymous inject-kind)
                (assoc-in [:replace injected-step-name] inject))))
          overrides
          (:inject overrides)))

(c/defn resolve-step [all-steps overrides step-name]
  (let [original-step (get all-steps step-name)
        replacement-step (get-in overrides [:replace step-name])
        alias-step    (when-let [alias (get-in overrides [:alias step-name])]
                        {:alias alias})]
    (some-> (or replacement-step
                alias-step
                original-step)
            (assoc :name step-name)
            (resolve-step-alias all-steps)
            (apply-injections (:inject overrides))
            ;; (devise-subplan)
            )))

(c/defn resolve-goals [all-steps overrides goals]
  (loop [deps goals
         steps {}
         inputs #{}]
    (if-let [step-name (first deps)]
      (if (some-> steps (get step-name) (contains? :name))
        (recur (rest deps)
               steps
               inputs)
        (let [dep-step (resolve-step all-steps overrides step-name)
              steps (update steps step-name (fn [dep-step']
                                              (-> dep-step
                                                  (assoc :name step-name)
                                                  (merge dep-step'))))
              steps (reduce (fn [steps dep-name]
                              (update-in steps
                                         [dep-name :rev-deps]
                                         (fnil conj #{})
                                         step-name))
                            steps
                            (:deps dep-step))
              inputs (cond-> inputs
                       (nil? dep-step)
                       (conj step-name))]
          (recur (into (rest deps) (:deps dep-step))
                 steps
                 inputs)))
      [steps inputs])))

;; Using Kahn's algorithm
(c/defn topo-sort [steps]
  (loop [order []
         roots (into #{}
                     (comp (remove (comp seq :deps))
                           (map :name))
                     (vals steps))
         edges (into {}
                     (map (juxt key (comp set :deps val)))
                     steps)]
    (if-let [step-name (first roots)]
      (let [step (get steps step-name)
            rd-edges (into {}
                           (map (fn [rev-dep]
                                  [rev-dep (-> edges (get rev-dep) (disj step-name))]))
                           (:rev-deps step))
            roots (into roots
                        (comp (remove (comp seq val))
                              (map key))
                        rd-edges)]
        (recur (conj order step-name)
               (disj roots step-name)
               (-> edges
                   (merge rd-edges)
                   (dissoc step-name))))
      (if (seq edges)
        (throw (ex-info "Cycle detected" {::cyclic-steps edges}))
        order))))


;; TODO: Fail when goals contain (only?) inputs?
(c/defn devise
  ([goals]
   (devise {} goals))
  ([overrides goals]
   (let [overrides' (->> overrides
                         (asserting-conform ::overrides)
                         (process-overrides))
         goals      (asserting-conform ::goals goals)
         all-steps  @step-registry
         [steps inputs] (resolve-goals all-steps overrides' goals)
         order      (topo-sort steps)]
     #::{:goals     goals
         :overrides overrides
         :inputs    inputs
         :steps     steps
         :order     order})))

(c/defn validated-inputs [expected-inputs inputs]
  (let [inputs (asserting-conform ::inputs inputs)]
    (when-let [missing (seq (remove (set (keys inputs)) expected-inputs))]
      (throw (ex-info "Missing plan inputs" {::missing-inputs missing})))
    inputs))

(c/defn realize
  ([strategy plan]
   (realize strategy plan {}))
  ([strategy plan inputs]
   (let [inputs (validated-inputs (::inputs plan) inputs)]
     (realize-steps strategy inputs plan))))

(c/defn get-step-spec [step]
  (or (:spec step)
      (when-let [s (s/get-spec (:name step))]
        (or (:ret s) s))))

(defmacro fdefs []
  (let [all-steps @step-registry
        ns-steps (->> (ns-interns *ns*)
                      (keys)
                      (map #(symbol (name (ns-name *ns*)) (name %)))
                      (filter all-steps))
        ns-plan (devise ns-steps)
        ns-step-specs (->> (::inputs ns-plan)
                           (map (fn [input]
                                  (assoc (get all-steps input) :name input)))
                           (concat (vals (::steps ns-plan)))
                           (map (juxt :name get-step-spec))
                           (into {}))
        ns-dep-steps (filter :deps (vals (::steps ns-plan)))]
    (when-let [missing (->> ns-step-specs (remove val) (map key) seq)]
      (throw (ex-info "Missing specs for some steps"
                      {::missing-specs missing})))
    `(do
       ~@(->> ns-dep-steps
              (map (fn [step]
                     (let [step-name (:name step)]
                       `(s/fdef ~step-name
                          :args (s/cat ~@(mapcat (fn [dep-name]
                                                   [(keyword (str dep-name))
                                                    (get ns-step-specs dep-name)])
                                                 (:deps step)))
                          ~@(when-let [fn-spec (:fn (s/get-spec step-name))]
                              [:fn fn-spec])
                          :ret ~(get ns-step-specs step-name))))))
       '~(mapv :name ns-dep-steps))))
