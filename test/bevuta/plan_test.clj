(ns bevuta.plan-test
  (:require [clojure.test :refer :all]
            [bevuta.plan :as p]
            [bevuta.plan.middleware :as pm]
            [bevuta.other-test-ns :as other]
            [clojure.spec :as s]))

(defn alpha [x y]
  (* 2 (+ x y)))

(p/def alpha
  :deps [gamma delta])

(p/defn gamma []
  3)

(p/defn delta [beta]
  (* beta beta))


(def alpha-plan
  (p/devise `alpha))

(p/def zeta :deps [other/theta])
(defn zeta [theta]
  (/ theta 2))

(p/defn zeta2 [other/theta]
  (/ theta 2))

(p/defn delta-sum [delta {::p/dep other/delta :as other-delta}]
  (+ delta other-delta))

(deftest realize-plan-strategies-test []
  (doseq [[desc strategy] {"in sequence" p/in-sequence
                           "in parallel" p/in-parallel}]
    (testing desc
      (is (= (p/realize strategy alpha-plan {`beta 6})
             `#::{alpha 78
                  beta 6
                  delta 36
                  gamma 3})))))

(deftest realize-multiple-goals-test []
  (is (= (p/realize p/in-sequence (p/devise `[gamma other/delta]))
         `#::{gamma 3
              other/delta 8})))

(deftest realize-with-missing-inputs-test []
  (let [e (try (p/realize p/in-sequence alpha-plan)
               (catch Exception e [::thrown e]))]
    (is (= (first e) ::thrown))
    (is (= (ex-data (second e)) {:missing [`beta]}))))

(deftest devise-plan-with-overrides-test []
  (is (= (p/realize p/in-sequence (p/devise `{delta {:value 3}
                                              gamma {:deps [delta] :fn ~inc}}
                                            `alpha))
         `#::{delta 3
              gamma 4
              alpha 14})))

(deftest dependencies-across-namespaces-test []
  (doseq [[desc goal] {"with def" `zeta
                       "with defn" `zeta2}]
    (testing desc
      (is (= (p/realize p/in-sequence (p/devise goal))
             {`other/theta 18
              goal 9})))))

(deftest dependencies-via-destructuring-syntax-test []
  (is (= (p/realize p/in-sequence (p/devise `delta-sum) `{beta 2})
         `#::{beta 2
              delta 4
              other/delta 8
              delta-sum 12})))

(p/defn boom [beta]
  (throw (ex-info "boom" {::boom ::boom})))

(deftest error-handling-test []
  (try
    (p/realize p/in-sequence (p/devise `boom) {`beta 10})
    (is false)
    (catch Exception e
      (is (= (ex-data e) {::boom ::boom})))))

(deftest middleware-test []
  (let [log (atom [])]
    (p/realize (p/wrap-strategy p/in-parallel
                                (fn [ctx]
                                  (swap! log conj (::p/step-name ctx))
                                  (assoc ctx ::test :foo))
                                (fn [ctx]
                                  (swap! log conj (::test ctx))
                                  ctx))
               (p/devise `delta-sum)
               `{beta 2})
    (is (= `[delta :foo delta-sum :foo] @log))))

(p/defn boom-depent [boom]
  ::nope)

(deftest error-context-middleware []
  (try
    (p/realize (p/wrap-strategy p/in-parallel pm/error-context)
               (p/devise `boom-depent)
               {`beta 10})
    (is false)
    (catch Exception e
      (is (= (::p/step-name (ex-data e)) `boom))
      (is (= (ex-data (.getCause e)) {::boom ::boom})))))
