(ns bevuta.plan.interceptors
  (:require [clojure.core :as c]
            [bevuta.plan :as p]
            [bevuta.interceptors :as interceptors]
            #?(:clj [clojure.tools.logging :as log])
            [bevuta.plan.util :as util])
  (:refer-clojure :exclude [time when])
  #?(:cljs (:require-macros [bevuta.plan.interceptors :refer [trace-log]])))

(def error-context
  {:error
   (fn [ctx error]
     (throw (ex-info (str "Error realizing " (::p/step-name ctx))
                     ctx
                     error)))})

(defn rethrowing [f]
  (fn [ctx error]
    (f ctx)
    (throw error)))

#?(:clj (defn time-now []
          (/ (System/nanoTime) 1000000))
   :cljs (defn time-now []
           (c/when (exists? js/performance)
             (js/performance.now))))

(def time
  (let [enter (fn [ctx]
                (assoc ctx ::time-start (time-now)))
        leave (fn [ctx]
                (let [duration (some->> (::time-start ctx)
                                        (- (time-now))
                                        (long))]
                  (-> ctx
                      (assoc ::time-duration duration)
                      (dissoc ::time-start))))]
    {:enter enter
     :error (rethrowing leave)
     :leave leave}))

#?(:clj 
   (defmacro trace-log [& args]
     (if (util/cljs-macro? &env)
       `(println ~@args)
       `(log/trace ~@args))))

(def trace
  (let [leave (fn [prefix]
                (fn [ctx]
                  (trace-log prefix
                             (::p/step-name ctx)
                             (if-let [time-duration (::time-duration ctx)]
                               (str "(" time-duration " ms)")
                               ""))
                  ctx))]
    {:enter (fn [ctx]
              (trace-log "->" (::p/step-name ctx))
              ctx)
     :error (rethrowing (leave "<- ERROR"))
     :leave (leave "<-")}))

(defn when [pred & interceptors]
  {:enter (fn [ctx]
            (if (pred ctx)
              (update ctx ::interceptors/queue into interceptors)
              ctx))})

(defn handle-error
  ([handler]
   (handle-error :default handler))
  ([error-class handler]
   {:error (fn [ctx error]
             (if (or (= error-class :default)
                     (instance? error-class error)) 
               (handler ctx error)
               (throw error)))}))
