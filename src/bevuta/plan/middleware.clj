(ns bevuta.plan.middleware
  (:require [bevuta.plan :as p]
            [clojure.tools.logging :as log]))

(defn error-context [ctx]
  (assoc ctx ::p/step-fn
         (fn [& args]
           (try
             (p/call-step-fn ctx)
             (catch Throwable cause
               (throw (ex-info (str "Error realizing " (::p/step-name ctx))
                               ctx
                               cause)))))))

(defn trace [ctx]
  (assoc ctx ::p/step-fn
         (fn [& args]
           (try
             (log/info "-> " (::p/step-name ctx))
             (p/call-step-fn ctx)
             (finally
               (log/info "<- " (::p/step-name ctx)))))))
