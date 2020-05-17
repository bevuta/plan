(ns bevuta.plan.step
  (:require [clojure.spec.alpha :as s]
            [bevuta.interceptors :as i]
            [bevuta.plan.util :as util]))

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

#?(:clj
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
         (future-done? step-future)))))

(s/def ::step
  (s/keys :req-un [(or ::deps
                       ::value
                       (and ::fn ::deps)
                       ::goal
                       (and ::plan ::goal)
                       (and ::strategy ::deps))]))

(s/def ::name
  (s/and symbol? (s/conformer util/qualify-symbol)))

(s/def ::goal ::name)

(s/def ::dep ::name)

(s/def ::deps
  (s/coll-of ::dep))

(s/def ::fn ifn?)

(s/def ::strategy any?)

(s/def ::value any?)

(s/def ::alias ::name)

(s/def ::args coll?)

(s/def ::inject
  (s/or :anonymous (s/keys :req-un [::fn]
                           :opt-un [::deps])
        :named ::name))
