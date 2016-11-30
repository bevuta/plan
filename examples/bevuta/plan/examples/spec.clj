(ns bevuta.plan.examples.spec
  (:require [bevuta.plan :as p]
            [clojure.spec :as s]))

(s/fdef alpha :ret int?)
(p/def alpha {:deps [gamma delta]})
(defn alpha [gamma delta]
  (+ gamma delta))

(s/fdef gamma :ret int?)
(p/def gamma
  {:deps [theta zeta]})
(defn gamma [theta zeta]
  (* theta zeta))


(s/fdef delta :ret int?)
(p/def delta
  {:deps [beta theta]})
(defn delta [beta theta]
  (- beta theta))

(s/def theta int?)
(p/def theta)

(s/def zeta int?)
(p/def zeta)

(s/def beta int?)
(p/def beta)
(def beta 10)

(p/fdefs)
