(ns bevuta.plan.middleware
  (:require [bevuta.plan :as p]))

(defn error-context [strategy ctx step-fn args]
  (p/realize-step strategy
                  ctx
                  (fn [& args]
                    (try
                      (apply step-fn args)
                      (catch Throwable cause
                        (throw (ex-info (str "Error realizing " (::p/step-name ctx))
                                        ctx
                                        cause)))))
                  args))
