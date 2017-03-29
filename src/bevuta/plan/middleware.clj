(ns bevuta.plan.middleware
  (:require [bevuta.plan :as p]))

(defn error-context [ctx]
  (assoc ctx ::p/step-fn
         (fn [& args]
           (try
             (p/call-step-fn ctx)
             (catch Throwable cause
               (throw (ex-info (str "Error realizing " (::p/step-name ctx))
                               ctx
                               cause)))))))
