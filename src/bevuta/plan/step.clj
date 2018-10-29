(ns bevuta.plan.step
  (:require [clojure.spec.alpha :as s]
            [bevuta.interceptors :as i]))

;; Snatched from `clojure.spec` and slightly refactored
(defn qualify-symbol
  "Qualify symbol s by resolving it or using the current *ns*."
  [sym]
  (if-let [ns-sym (some-> sym namespace symbol)]
    (or (some-> (get (ns-aliases *ns*) ns-sym)
                ns-name
                name
                (symbol (name sym)))
        sym)
    (symbol (name (ns-name *ns*)) (name sym))))

(defn call-step-fn [ctx]
  (i/execute ctx))

(defprotocol Strategy
  (realize [_ ctx])
  (deref-result [_ step-ref])
  (done? [_ step-ref]))

(defn strategy? [x]
  (satisfies? Strategy x))

(def in-sequence
  (reify
    Object
    (toString [_] "in-sequence")
    Strategy
    (realize [_ ctx]
      (call-step-fn ctx))
    (deref-result [_ result]
      result)
    (done? [_ _]
      true)))

(def in-parallel
  (reify
    Object
    (toString [_] "in-parallel")
    Strategy
    (realize [_ ctx]
      (future (call-step-fn ctx)))
    (deref-result [_ step-future]
      (try
        @step-future
        ;; TODO: Is this a good idea? ;-)
        (catch java.util.concurrent.ExecutionException e
          (throw (.getCause e)))))
    (done? [_ step-future]
      (future-done? step-future))))

(s/def ::step
  (s/keys :req-un [(or ::deps
                       ::value
                       (and ::fn ::deps)
                       ::goal
                       (and ::plan ::goal)
                       (and ::strategy ::deps))]))

(s/def ::name
  (s/and symbol? (s/conformer qualify-symbol)))

(s/def ::goal ::name)

(s/def ::dep ::name)

(s/def ::deps
  (s/coll-of ::dep))

(s/def ::fn ifn?)

(s/def ::strategy strategy?)

(s/def ::value any?)

(s/def ::alias ::name)

(s/def ::args coll?)

(s/def ::inject
  (s/or :anonymous (s/keys :req-un [::fn]
                           :opt-un [::deps])
        :named ::name))
