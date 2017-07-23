(ns bevuta.plan-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [bevuta.interceptors :as interceptors]
            [bevuta.plan :as p]
            [bevuta.plan.interceptors :as pi]
            [bevuta.other-test-ns :as other]))

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

(deftest interceptor-test []
  (let [log (atom [])
        plan (-> (p/devise `delta-sum)
                 (p/add-interceptors {:enter (fn [ctx]
                                               (swap! log conj (::p/step-name ctx))
                                               (assoc ctx ::test :foo))}
                                     {:enter (fn [ctx]
                                               (swap! log conj (::test ctx))
                                               ctx)}))
        result (p/realize p/in-sequence plan `{beta 2})]
    (is (= `[delta :foo delta-sum :foo] @log))
    (is (= [:foo :foo] (->> result meta ::p/results vals (map ::test))))))

(deftest interceptor-error-propagation-test []
  (try
    (p/realize p/in-parallel
               (-> (p/devise `boom)
                   (p/add-interceptors {:error (fn [ctx error]
                                                 (throw (ex-info "boom" {:no 1 :ctx ctx})))}
                                       {:error (fn [ctx error]
                                                 (throw (ex-info "boom" {:no 2})))}
                                       {:error (fn [ctx error]
                                                 (throw (ex-info "boom" {:no 3})))}))
               {`beta 10})
    (is false)
    (catch Exception e
      (let [data (ex-data e)]
        (is (= (dissoc data :ctx) {:no 1}))
        (is (= (count (::interceptors/suppressed-errors (:ctx data))) 2))))))

(p/defn boom-dependent [boom]
  ::nope)

(deftest error-context-interceptor-test []
  (try
    (p/realize p/in-sequence
               (-> (p/devise `boom-dependent)
                   (p/add-interceptors pi/error-context))
               {`beta 10})
    (is false "Didn't throw exception")
    (catch Exception e
      (is (= (::p/step-name (ex-data e)) `boom))
      (is (= (ex-data (.getCause e)) {::boom ::boom})))))

(deftest handle-error-interceptor-test []
  (let [result (p/realize p/in-parallel
                          (-> (p/devise `boom-dependent)
                              (p/add-interceptors (pi/handle-error
                                                   RuntimeException
                                                   (fn [ctx error]
                                                     (assoc ctx ::p/value ::no-problem)))))
                          {`beta 10})]
    (is (= (get result `boom) ::no-problem))))


(deftest when-interceptor-test []
  (let [log1 (atom #{})
        log2 (atom #{})
        collect (fn [log]
                  {:enter (fn [ctx]
                            (swap! log conj (::p/step-name ctx))
                            ctx)})
        result (p/realize p/in-parallel
                          (p/add-interceptors alpha-plan
                                              pi/trace
                                              pi/time
                                              (collect log1)
                                              (pi/when (comp `#{delta gamma} ::p/step-name)
                                                (collect log2)))
                          `{beta 2})]
    (is (= `#{alpha delta gamma} @log1))
    (is (= `#{delta gamma} @log2))))

(deftest time-interceptor-test []
  (let [result (p/realize p/in-sequence
                          (p/add-interceptors alpha-plan pi/time)
                          `{beta 2})
        timings (->> result meta ::p/results vals (map ::pi/time-ns))]
    (is (seq timings))
    (is (every? nat-int? timings))))
