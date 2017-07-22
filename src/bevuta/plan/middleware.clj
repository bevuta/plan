(ns bevuta.plan.middleware
  (:require [bevuta.plan :as p]
            [clojure.tools.logging :as log])
  (:refer-clojure :exclude [time when]))

(defn error-context [continue]
  (fn [ctx]
    (try
      (continue ctx)
      (catch Throwable cause
        (throw (ex-info (str "Error realizing " (::p/step-name ctx))
                        ctx
                        cause))))))

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

(defn when [pred middleware]
  (fn [continue]
    (fn [ctx]
      (if (pred ctx)
        ((middleware continue) ctx)
        (continue ctx)))))

(defn handle-error
  ([handler]
   (handle-error Throwable handler))
  ([error-class handler]
   (fn [continue]
     (fn [ctx]
       (try
         (continue ctx)
         (catch Throwable error
           (if (instance? error-class error)
             (handler ctx error)
             (throw error))))))))
