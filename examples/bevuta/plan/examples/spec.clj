(ns bevuta.plan.examples.spec
  (:require [bevuta.plan :as p]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]))

;; `bevuta.plan` integrates with `clojure.spec` in various ways. For
;; one, since steps are just regular functions, they can be `fdef`ed
;; just like any other function.

(s/fdef alpha
        :args (s/cat :gamma bigdec? :delta bigdec?)
        :ret bigdec?)

(p/defn alpha [gamma delta]
  (+ gamma delta))

(s/fdef delta
        :args (s/cat :beta bigdec?)
        :ret bigdec?)

(p/defn delta [beta]
  (* 2 beta))

(s/fdef gamma
        :args (s/cat)
        :ret bigdec?)

(p/defn gamma []
  10)

(stest/check `delta)

;; Note how the `:args` specs are redundant since they correspond to
;; the respective dependency's `:ret` spec. You can take advantage of
;; this fact. Plan can generate `:args` specs from dependencies for
;; you via the `bevuta.plan/fdefs` form. Just leave off the `:args`
;; specs from the `fspec`s above:

(s/fdef alpha :ret bigdec?)
(s/fdef delta :ret bigdec?)
(s/fdef gamma :ret bigdec?)

;; For plan to be able to derive all `:args` specs for you, you will
;; also have to define specs for all inputs, `beta` in this case:

(s/def beta bigdec?)

;; And once they are all defined, call `fdefs` once in your
;; namespace's top-level:

(p/fdefs)

;; The keywordized qualified input names are used in the `:args` spec:
(s/conform (:args (s/get-spec `alpha)) [10M 20M])
;; => #::{:gamma 10M, :delta 20M}

;; `:fn` specs are preserved by `fdefs`. The following example
;; demonstrates this by intentionally defining an invalid `:fn` spec:

(s/fdef alpha
        :ret bigdec?
        :fn (fn [{:keys [args ret]}]
              (= ret
                 (::gamma args)
                 (::delta args))))

(p/fdefs)

;; Now `stest/check` should find the expected failure:

(-> (stest/check `alpha) first stest/abbrev-result :failure ::s/problems)
;; => [{:path [:fn]
;;      :pred (fn ...)
;;      :val {:args #::{:gamma 1.0M, :delta 1.0M}
;;            :ret 2.0M}
;;      :via [],
;;      :in []}]

