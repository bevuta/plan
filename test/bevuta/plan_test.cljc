(ns bevuta.plan-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [bevuta.interceptors :as interceptors]
            [bevuta.plan :as p]
            [bevuta.plan.step :as step]
            [bevuta.plan.interceptors :as pi]
            [bevuta.other-test-ns :as other]))

(s/check-asserts true)

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

(deftest realize-test
  (is (= (p/realize alpha-plan {`beta 6})
         `#::{alpha 78
              beta 6
              delta 36
              gamma 3})))

(deftest realize-multiple-goals-test
  (is (= (p/realize (p/devise `[gamma other/delta]))
         `#::{gamma 3
              other/delta 8})))

(deftest realize-with-missing-inputs-test
  (let [e (try (p/realize alpha-plan)
               (catch #?(:clj Exception :cljs :default) e [::thrown e]))]
    (is (= (first e) ::thrown))
    (is (= (ex-data (second e)) {::p/missing-inputs [`beta]}))))

(deftest devise-goals-which-are-inputs
  (try (p/devise `[alpha an-undefined-step-as-goal another-undefined-step-as-goal])
       (is false "Devise shouldn't have succeeded")
       (catch #?(:clj Exception :cljs :default) e
         (is (= `#{an-undefined-step-as-goal another-undefined-step-as-goal}
                (::p/undefined-goals (ex-data e)))))))

(p/defn cycle-a [cycle-b] :a)
(p/defn cycle-b [cycle-c] :b)
(p/defn cycle-c [cycle-a] :c)
(p/defn cycle-d [cycle-a] :d)

(deftest devise-with-cycles
  (try (p/devise `[cycle-a cycle-d])
       (is false "Devise shouldn't have succeeded")
       (catch #?(:clj Exception :cljs :default) e
         (is (= `{cycle-a #{cycle-b},
                  cycle-b #{cycle-c},
                  cycle-c #{cycle-a},
                  cycle-d #{cycle-a}}
                (::p/cyclic-steps (ex-data e)))))))

(deftest devise-with-overrides-test
  (is (= (p/realize (p/devise `{:replace {delta {:value 3}
                                                        gamma {:deps [delta] :fn ~inc}}}
                                            `alpha))
         `#::{delta 3
              gamma 4
              alpha 14})))


(def ^:dynamic invocation-count)

(p/defn invocation-counting-step []
  (swap! invocation-count inc)
  10)

(deftest devise-with-alias-override-test
  (binding [invocation-count (atom 0)]
    (is (= (p/realize (p/devise `{:replace {delta invocation-counting-step
                                                          gamma invocation-counting-step}}
                                              `alpha))
           `#::{invocation-counting-step 10
                delta 10
                gamma 10
                alpha 40}))
    (is (= 1 @invocation-count))))

(p/defn double-zeta-gamma [zeta gamma]
  (* 2 gamma zeta))

(deftest devise-with-named-inject-override-test
  (is (= (p/realize (p/devise `{:inject {gamma double-zeta-gamma}}
                              `alpha)
                    `{beta 2})
         `#::{beta 2
              other/theta 18
              zeta 9
              gamma 3
              delta 4
              double-zeta-gamma 54
              alpha 116})))

(deftest devise-with-anonymous-inject-override-test
  (let [plan (p/devise `{:inject {gamma {:fn ~inc}}}
                       `alpha)
        injected-step (some :injected-step (vals (::p/steps plan)))]
    (is (= (p/realize plan `{beta 2})
           `#::{beta 2
                delta 4
                gamma 3
                ~injected-step 4
                alpha 16}))))

(deftest devise-inject-and-alias-override-for-the-same-step-test
  (let [plan (p/devise `{:replace {gamma delta}
                         :inject {gamma {:fn ~inc}}}
                       `alpha)
        injected-step (some :injected-step (vals (::p/steps plan)))]
    (is (= (p/realize plan `{beta 2})
           `#::{beta 2
                delta 4
                gamma 4
                ~injected-step 5
                alpha 18}))))

(deftest dependencies-across-namespaces-test
  (doseq [[desc goal] {"with def" `zeta
                       "with defn" `zeta2}]
    (testing desc
      (is (= (p/realize (p/devise goal))
             {`other/theta 18
              goal 9})))))

(deftest dependencies-via-destructuring-syntax-test
  (is (= (p/realize (p/devise `delta-sum) `{beta 2})
         `#::{beta 2
              delta 4
              other/delta 8
              delta-sum 12})))


(p/defn shared-step [beta]
  (* beta 2))

(p/defn sharing-step1 [shared-step]
  (inc shared-step))

(p/defn sharing-step2 [shared-step sharing-step1]
  (+ shared-step sharing-step1))

(deftest dependency-shared-by-dependency-test
  (is (= (p/realize (p/devise `sharing-step2) `{beta 3})
         `#::{beta 3
              shared-step 6
              sharing-step1 7
              sharing-step2 13})))

(deftest step-which-is-both-a-goal-and-another-goals-dependency-is-calculated-once-test
  (let [order (::p/order (p/devise `[sharing-step1 shared-step]))]
    (is (= (distinct order) order))))

(p/defn boom [beta]
  (throw (ex-info "boom" {::boom ::boom})))

(deftest error-handling-test
  (try
    (p/realize (p/devise `boom) {`beta 10})
    (is false)
    (catch #?(:clj Exception :cljs :default) e
      (is (= (ex-data e) {::boom ::boom})))))

(deftest interceptor-test
  (let [log (atom [])
        plan (-> (p/devise `delta-sum)
                 (p/add-interceptors {:enter (fn [ctx]
                                               (swap! log conj (::step/name ctx))
                                               (assoc ctx ::test :foo))}
                                     {:enter (fn [ctx]
                                               (swap! log conj (::test ctx))
                                               ctx)}))
        result (p/realize plan `{beta 2})]
    (is (= `[delta :foo delta-sum :foo] @log))
    (is (= [:foo :foo] (->> result meta ::p/results vals (map ::test))))))

(deftest interceptor-error-propagation-test
  (try
    (p/realize (-> (p/devise `boom)
                   (p/add-interceptors {:error (fn [ctx error]
                                                 (throw (ex-info "boom" {:no 1 :ctx ctx})))}
                                       {:error (fn [ctx error]
                                                 (throw (ex-info "boom" {:no 2})))}
                                       {:error (fn [ctx error]
                                                 (throw (ex-info "boom" {:no 3})))}))
               {`beta 10})
    (is false)
    (catch #?(:clj Exception :cljs :default) e
      (let [data (ex-data e)]
        (is (= (dissoc data :ctx) {:no 1}))
        (is (= (count (::interceptors/suppressed-errors (:ctx data))) 2))))))

(p/defn boom-dependent [boom]
  ::nope)

(deftest error-context-interceptor-test
  (try
    (p/realize (-> (p/devise `boom-dependent)
                   (p/add-interceptors pi/error-context))
               {`beta 10})
    (is false "Didn't throw exception")
    (catch #?(:clj Exception :cljs :default) e
      (is (= (::step/name (ex-data e)) `boom))
      (is (= (ex-data (ex-cause e)) {::boom ::boom})))))


(deftest handle-error-interceptor-test
  (let [result (p/realize (-> (p/devise `boom-dependent)
                              (p/add-interceptors (pi/handle-error
                                                   #?(:clj RuntimeException :cljs :default)
                                                   (fn [ctx error]
                                                     (assoc ctx ::step/value ::no-problem)))))
                          {`beta 10})]
    (is (= (get result `boom) ::no-problem))))

(deftest when-interceptor-test
  (let [log1 (atom #{})
        log2 (atom #{})
        collect (fn [log]
                  {:enter (fn [ctx]
                            (swap! log conj (::step/name ctx))
                            ctx)})
        result (p/realize (p/add-interceptors alpha-plan
                                              pi/trace
                                              pi/time
                                              (collect log1)
                                              (pi/when (comp `#{delta gamma} ::step/name)
                                                (collect log2)))
                          `{beta 2})]
    (is (= `#{alpha delta gamma} @log1))
    (is (= `#{delta gamma} @log2))))

#?(:clj
   (deftest time-interceptor-test
     (let [result (p/realize (p/add-interceptors alpha-plan pi/time)
                             `{beta 2})
           timings (->> result meta ::p/results vals (map ::pi/time-duration))]
       (is (seq timings))
       (is (every? nat-int? timings)))))

;; TODO: Notice that delta is the same step as in the parent plan, so
;; it could be re-used instead of re-calclated for the subplan. This
;; would only work if the step really is the same, though, e.g. the
;; same overrides have to be applied. Right now we take the easy route
;; of re-calculating it.
(p/defn omega [beta tau delta]
  (+ beta tau delta))

(p/def subomega
  :goal omega)

(p/defn alpha-omega [alpha subomega]
  (+ alpha subomega))

(deftest subplan-test
  (is (= `#::{beta 3
              delta 9
              tau 5
              gamma 3
              alpha 24
              alpha-omega 41
              subomega 17}
         (p/realize (p/devise `alpha-omega)
                    `{beta 3
                      tau 5}))))

(p/def subdelta
  :goal delta
  :plan (p/devise {:replace {`beta {:value 2}}} `[delta gamma]))

(deftest subplan-with-explicit-plan-test
  (let [result (is (p/realize (p/devise `subdelta)))]
    (is (= `#::{subdelta 4} result))))


(p/def broken-subdelta
  :goal delta
  :plan (p/devise {:replace {`beta {:value 2}}} `gamma))

(deftest subplan-with-explicit-plan-for-different-goal
  (try (p/devise `broken-subdelta)
       (is false "Devise shouldn't have succeeded")
       (catch #?(:clj Exception :cljs :default) e
         (is (= `broken-subdelta (::step/name (ex-data e))))
         (is (= `delta (::step/goal (ex-data e))))
         (is (= `#{gamma} (::p/subplan-goals (ex-data e)))))))

#?(:clj
   (p/defn slow-alpha [alpha]
     {:strategy step/in-parallel}
     (Thread/sleep 50)
     alpha))
#?(:clj
   (p/defn slow-gamma [gamma]
     {:strategy step/in-parallel}
     (Thread/sleep 50)
     gamma))

#?(:clj
   (deftest step-strategies-test
     (let [start (System/nanoTime)
           results (p/realize (p/devise `[slow-alpha slow-gamma alpha gamma]) `{beta 2})
           {::syms [slow-alpha alpha slow-gamma gamma]} results]
       (is (some? slow-alpha))
       (is (some? slow-gamma))
       (is (= slow-alpha alpha))
       (is (= slow-gamma gamma))
       (is (<= 49
               (long (/ (- (System/nanoTime) start) 1000000))
               70)))))

;; TODO: Test metadata of results

