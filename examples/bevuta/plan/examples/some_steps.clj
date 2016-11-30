(ns bevuta.plan.examples.some-steps
  (:require [bevuta.plan :as p]))

(p/defn delta [omega]
  (- omega 20))

(p/defn theta []
  9)
