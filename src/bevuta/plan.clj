(ns bevuta.plan
  (:refer-clojure :exclude [def defn])
  (:require [clojure.core :as c]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [bevuta.interceptors :as i]
            [bevuta.plan.step :as step]))

(c/defn unqualify-symbol [s]
  (symbol (name s)))

(s/def ::step ::step/step)

(s/def ::step*
  (s/keys* :opt-un [::step/deps ::step/goal ::step/plan ::step/strategy]))

(s/def ::inputs
  (s/map-of ::step/name ::step/value
            :conform-keys true))

(s/def ::goals
  (s/and (s/or :multi  (s/coll-of ::step/name :into #{})
               :single (s/and ::step/name (s/conformer vector) ::goals))
         (s/conformer val)))

(create-ns 'bevuta.plan.override)
(alias 'override 'bevuta.plan.override)

(s/def ::override/replace
  (s/map-of ::step/name (s/or :step ::step
                              :alias ::step/alias)
            :conform-keys true))

(s/def ::override/inject
  (s/map-of ::step/name ::step/inject
            :conform-keys true))

(s/def ::overrides
  (s/keys :opt-un [::override/replace
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
  (cond-> step
    (:deps step)
    (update :deps (partial mapv quote-value))
    (:goal step)
    (update :goal quote-value)))

(s/fdef def
  :args (s/cat :name ::step/name
               :step ::step*))

(defmacro def
  [step-name & definition]
  (let [step-name (s/conform ::step/name step-name)
        step (or (s/conform ::step* definition) {})]
    `(do (swap! step-registry
                assoc
                '~step-name
                ~(quote-step step))
         '~step-name)))

(s/def ::dep ::step/dep)

(s/def ::dep-destructuring
  (s/keys :req [::dep]))

(s/fdef defn
  :args (s/cat :name ::step/name
               :args (s/coll-of (s/or :symbol ::step/name
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

(declare realize-steps)

(c/defn subplan-step-fn [step]
  (fn [& deps]
    (let [inputs  (zipmap (:deps step) deps)
          results (realize-steps inputs (:plan step))
          goal    (:goal step)]
      {::step/results results
       ::step/value (get results goal)})))

(c/defn resolve-step-fn [step]
  (if (:plan step)
    (subplan-step-fn step)
    (or (:fn step)
        (resolve-var (:name step)))))

(def step-fn-interceptor
  {:enter (fn [ctx]
            (let [{::step/keys [fn args plan goal]} ctx]
              (let [result (apply fn args)]
                (if plan
                  (merge ctx result)
                  (assoc ctx ::step/value result)))))})

(c/defn add-interceptors [plan & interceptors]
  (update plan ::interceptors (fnil into []) interceptors))

(defmacro get-or [map key else]
  `(if-let [[_# value#] (find ~map ~key)]
     value#
     ~else))

(c/defn dependency-val [ctx step-name]
  (get-or (::inputs ctx)
          step-name
          (when-let [result (get-in ctx [::results step-name])]
            (::step/value (result)))))

(c/defn process-results [inputs results]
  (let [results (into {}
                      (map (fn [[step-name result]]
                             [step-name (result)]))
                      results)
        values (into inputs
                     (map (fn [[step-name result]]
                            [step-name (::step/value result)]))
                     results)]
    (with-meta values {::results results})))

;; NOTE: We rely on `map`s laziness here so that steps are actually
;; only dereferenced when `plan/realize` realizes the `step-args`
;; sequence. This allows it to defer potentially blocking derefs to
;; another thread, for example.
(c/defn realize-steps [inputs plan]
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
                  step-args (map (partial dependency-val ctx) step-deps)
                  step-ctx  {::step/name     step-name
                             ::step/deps     step-deps
                             ::step/fn       step-fn
                             ::step/args     step-args
                             ::i/queue       interceptors}
                  step-ctx  (if-let [step-plan (:plan step)]
                              (assoc step-ctx
                                     ::step/plan step-plan
                                     ::step/goal (:goal step))
                              step-ctx)
                  strategy  (or (:strategy step) step/in-sequence)
                  result    (step/realize strategy step-ctx)]
              (recur (-> ctx
                         (assoc-in [::results step-name] #(step/deref-result strategy result))
                         (update ::order rest))))
            (recur (cond-> (update ctx ::order rest)
                     (not (contains? inputs step-name))
                     (assoc-in [::inputs step-name]
                               (get-or step
                                       :value
                                       (resolve-var step-name)))))))
        (process-results inputs results)))))

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

(c/defn map-entry [k v]
  (clojure.lang.MapEntry. k v))

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
                (assoc-in [:replace injected-step-name] (map-entry :step inject)))))
          overrides
          (:inject overrides)))

(c/defn resolve-override-replace-step [overrides step-name]
  (when-let [r (get-in overrides [:replace step-name])]
    (case (key r)
      :step (val r)
      :alias {:alias (val r)})))

(declare devise)

(c/defn devise-subplan [step]
  (if-let [goal (:goal step)]
    (let [plan (or (:plan step) (devise goal))
          deps (vec (::inputs plan))]
      (when-not (contains? (::goals plan) goal)
        (throw (ex-info "Subplan doesn't include required goal"
                        {::step/name (:name step)
                         ::step/goal goal
                         ::subplan-goals (::goals plan)})))
      (assoc step
             :deps deps
             :plan plan))
    step))

(c/defn resolve-step [all-steps overrides step-name]
  (let [original-step (get all-steps step-name)
        replace-step (resolve-override-replace-step overrides step-name)]
    (some-> (or replace-step
                original-step)
            (assoc :name step-name)
            (resolve-step-alias all-steps)
            (apply-injections (:inject overrides))
            (devise-subplan))))

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

(c/defn devise
  ([goals]
   (devise {} goals))
  ([overrides goals]
   (let [overrides' (->> overrides
                         (asserting-conform ::overrides)
                         (process-overrides))
         goals      (asserting-conform ::goals goals)
         all-steps  @step-registry
         [steps inputs] (resolve-goals all-steps overrides' goals)]
     (if-let [undefined-goals (seq (filter inputs goals))]
       (throw (ex-info "Some goals are undefined"
                       {::undefined-goals (set undefined-goals)}))
       #::{:goals     goals
           :overrides overrides
           :inputs    inputs
           :steps     steps
           :order     (topo-sort steps)}))))

(c/defn validated-inputs [expected-inputs inputs]
  (let [inputs (asserting-conform ::inputs inputs)]
    (when-let [missing (seq (remove (set (keys inputs)) expected-inputs))]
      (throw (ex-info "Missing plan inputs" {::missing-inputs missing})))
    inputs))

(c/defn realize
  ([plan]
   (realize plan {}))
  ([plan inputs]
   (let [inputs (validated-inputs (::inputs plan) inputs)]
     (realize-steps inputs plan))))

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
