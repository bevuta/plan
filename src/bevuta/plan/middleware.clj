(ns bevuta.plan.middleware
  (:require [bevuta.plan :as p]
            [clojure.tools.logging :as log]
            [bevuta.interceptors :as interceptors])
  (:refer-clojure :exclude [time when]))

(def error-context
  {:error
   (fn [ctx error]
     (throw (ex-info (str "Error realizing " (::p/step-name ctx))
                     ctx
                     error)))})

(defn trace [continue]
  (fn [ctx]
    (try
      (println "-> " (::p/step-name ctx))
      (continue ctx)
      (finally
        (println "<- " (::p/step-name ctx))))))

(defn time [continue]
  (fn [ctx]
    (println "timing" (::p/step-name ctx))
    (let [start   (System/nanoTime)
          ctx     (continue ctx)
          time-ns (- (System/nanoTime) start)]
      (assoc ctx ::time-ns time-ns))))

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
