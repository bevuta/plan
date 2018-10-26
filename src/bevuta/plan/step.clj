(ns bevuta.plan.step
  (:require [clojure.spec.alpha :as s]))

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

(s/def ::step
  (s/keys :req-un [(or ::deps
                       ::value
                       (and ::fn ::deps)
                       ::goal
                       (and ::plan ::goal))]))

(s/def ::name
  (s/and symbol? (s/conformer qualify-symbol)))

(s/def ::goal ::name)

(s/def ::dep ::name)

(s/def ::deps
  (s/coll-of ::dep))

(s/def ::fn ifn?)

(s/def ::value any?)

(s/def ::alias ::name)

(s/def ::args coll?)

(s/def ::inject
  (s/or :anonymous (s/keys :req-un [::fn]
                           :opt-un [::deps])
        :named ::name))
