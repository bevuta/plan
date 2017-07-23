(ns bevuta.plan.interceptors
  (:require [bevuta.plan :as p]
            [bevuta.interceptors :as interceptors]
            [clojure.tools.logging :as log])
  (:refer-clojure :exclude [time when]))

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

(def time
  (let [now   #(System/nanoTime)
        enter (fn [ctx]
                (assoc ctx ::time-start (now)))
        leave (fn [ctx]
                (-> ctx
                    (assoc ::time-ns (- (now) (::time-start ctx)))
                    (dissoc ::time-start)))]
    {:enter enter
     :error (rethrowing leave)
     :leave leave}))

(def trace
  (let [leave (fn [prefix]
                (fn [ctx]
                  (log/trace prefix
                             (::p/step-name ctx)
                             (if-let [time-ns (::time-ns ctx)]
                               (str "(" time-ns " ns)")
                               ""))
                  ctx))]
    {:enter (fn [ctx]
              (log/trace "->" (::p/step-name ctx))
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
   (handle-error Throwable handler))
  ([error-class handler]
   {:error (fn [ctx error]
             (if (instance? error-class error)
               (handler ctx error)
               (throw error)))}))
