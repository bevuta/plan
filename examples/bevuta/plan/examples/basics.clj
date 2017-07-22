(ns bevuta.plan.examples.basics
  (:require [bevuta.plan :as p]
            [bevuta.plan.examples.some-steps :as es]))

;; This library provides a dependency based computation mechanism. Its
;; design is heavily influenced by P{ris,lu}matic's Graph and aims to
;; blend well with `clojure.spec`.
;;
;; The two central abstractions in `bevuta.plan` are *plans* and
;; *steps*. Steps are functions which depend on other steps to produce
;; a result. Plans describe a series of steps for reaching a given set
;; of *goals* which are steps themselves.
;;
;; This namespace will walk you through the basics of this library.

;; A step is a plain (named) function.

(defn alpha [x y]
  (* 2 (+ x y)))

;; Its dependencies can be declared with `bevuta.plan/def`. Their
;; order has to correspond to the argument order. For example, here we
;; declare that the step named `alpha` depends on the results of the
;; steps `gamma` and `delta`. The result of `gamma` will be passed to
;; `alpha` as `x` and the result of `delta` will be passed as `y`.

(p/def alpha
  :deps [gamma delta])

;; The shortcut `bevuta.plan/defn` allows to define a function and
;; declare its dependencies in one go. Here, the argument names need
;; to correspond to the dependency names. For example, this form is
;; equivalent to the two forms above:

(p/defn alpha [gamma delta]
  (* 2 (+ gamma delta)))

;; Let's define `gamma` and `delta` as constant steps for now:

(p/defn gamma []
  3)

(p/defn delta []
  5)

;; Now we can *devise* a plan for reaching the goal `alpha`:

(def alpha-plan
  (p/devise `alpha))

;; You can think of a plan as a description of a computation. It tells
;; you what steps you need to take in what order so that you
;; eventually end up with a result for the given goal(s). Devising a
;; plan involves a topological sort of all the given goal's direct and
;; transitive dependencies. Depending on how many steps are involved,
;; it can be an expensive operation, so remembering and re-using a
;; plan once it was devised is a good idea.

;; As you may expect, plans can be *realized*. To that end, you need
;; to provide a *strategy* of how to go about that. The most basic
;; strategy is to realize steps in sequence:

(p/realize p/in-sequence alpha-plan)
;; => #::{delta 5, gamma 3, alpha 16}

;; As you can see, the result is a map from step names to their
;; respective results. A constant plan like this is pretty boring,
;; though, as it will always produce the same result. Let's replace
;; `delta` by the following definition:

(p/defn delta [beta]
  (* beta beta))

;; Since we changed `delta`'s dependencies, we need to re-devise
;; `alpha-plan`:

(def alpha-plan
  (p/devise `alpha))

;; Note that this isn't necessary in real code as you would just
;; change the earlier definition of `delta` in place. It's merely an
;; artifact of this namespace being a walkthough.

;; Now what happens if we realize `alpha-plan` again? We never defined
;; `beta`! Right, we get an error:

(try (p/realize p/in-sequence alpha-plan) (catch Exception e))
;; Exception: Missing plan inputs
;; {:missing (bevuta.plan.examples.basics/beta)}

;; Undefined steps like this are considered *inputs* to the plan which
;; need to be provided upon realization. Try this:

(p/realize p/in-sequence alpha-plan {`beta 6})
;; => #::{beta 6, delta 36, gamma 3, alpha 78}

;; Steps may also depend on steps from other namespaces:

(p/def zeta :deps [es/theta])
(defn zeta [theta]
  (/ theta 5))

;; This also works with `bevuta.plan/defn`. The name part of the
;; qualified symbol will serve as the argument name:

(p/defn zeta [es/theta]
  (/ theta 5))

(p/realize p/in-sequence (p/devise `zeta) {`es/theta 10})
;; => #::{es/theta 10, zeta 2}

;; When you need to depend on multiple steps of the same name from
;; different namespaces, you can use the `::p/dep` destructuring
;; extension to resolve the conflict:
(p/defn zeta [delta {::p/dep es/delta :as other-delta}]
  (+ delta other-delta))

(p/realize p/in-sequence (p/devise `zeta) {`es/omega 10 `beta 3})
;; => #::{es/omega 10, beta 3, es/delta -10, delta 9, zeta -1}

;; You might wonder at this point why you should bother with all of
;; this if a simple function like this would do the trick just as
;; well:

(defn alpha* [beta]
  (alpha (delta beta) (gamma)))

(alpha* 6)
;; => 78

;; One obvious difference is that you don't get the intermediate
;; results which can be quite handy for debugging. However, there's
;; more. To be continued ...
