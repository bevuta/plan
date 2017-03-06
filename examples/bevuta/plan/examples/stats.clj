(ns bevuta.stats
  (:refer-clojure :exclude [count])
  (:require [clojure.core :as c]
            [clojure.spec :as s]
            [bevuta.plan :as p]
            [bevuta.plan.examples.basics :as b]))

;; A step is defined with a name and its dependencies
(p/def count
  :deps [data-points])

;; Upon realization, the function with the same name will be called
;; with the dependencies passed in the same order as in the step
;; definition
(defn count [data-points]
  (c/count data-points))

;; Alternatively, the step function can be defined inline
(p/def count
  :deps [data-points]
  :fn c/count)

;; There is syntactic sugar to define a step and its dependencies in
;; one go - note that the arguments *must* match the dependency names
;; here
(p/defn sum [data-points]
  (reduce + data-points))


(s/fdef square-sum :ret int?)

(p/defn square-sum [data-points]
  (reduce + (map * data-points data-points)))

(p/defn mean [sum count]
  (/ sum count))

(p/defn mean-square [square-sum count]
  (/ square-sum count))

(p/defn variance [mean mean-square]
  (- mean-square (* mean mean)))

;; Steps can be spec'ed, of course
(s/def data-points
  (s/coll-of integer? :min-count 1))

(s/fdef count
        :args (s/cat :data-points `data-points)
        :ret nat-int?)

;; Now we can devise a plan for realizing a given `goal` step
(def variance-plan (p/devise `variance))

;; A plan knows about its required inputs.
(::p/inputs variance-plan)
;; => #{bevuta.stats/data-points}

(def example-inputs
  `{data-points [1 2 3 6]})

;; Plans can be realized via various strategies. Here we realize the
;; steps in sequence (in dependency order). The result is a map which
;; includes the goal as well as all intermediate step results.
(p/realize p/in-sequence variance-plan example-inputs)
;; => #:bevuta.stats{sum 12, mean 3, square-sum 50, variance 7/2, mean-square 25/2, data-points [1 2 3 6], count 4}

;; If you want to depend on those results, be sure to provide them as
;; goals when devising the plan so that you don't accidentally end up
;; depending on transitive dependencies.
(p/realize p/in-sequence
           (p/devise `[variance mean-square])
           example-inputs)

;; You can also override individual steps in a plan. Let's say we want
;; to mock the `mean-square` step with a different function and the
;; `sum` step with a constant value.
(p/realize p/in-sequence
           (p/devise `{mean-square {:deps [count] :fn ~identity}
                       sum  {:value 10}}
                     `variance)
           example-inputs)
;; => #:bevuta.stats{sum 10, mean 5/2, variance -9/4, mean-square 4, data-points [1 2 3 6], count 4}

(def cross-module-example-plan
  (p/devise `{b/gamma {:deps [sum] :fn ~inc}}
            `b/alpha))

;; Can be evaluated according to a strategy
(defn cross-module-example [inputs]
  (p/realize p/in-sequence
             cross-module-example-plan
             inputs))

;; Or compiled accordingly
(def compiled-cross-module-example
  (p/compile p/in-sequence cross-module-example-plan))

;; (compiled-cross-module-example `{b/beta 10, data-points [1 2 3]})
;; (cross-module-example `{b/beta 10, data-points [1 2 3]})

