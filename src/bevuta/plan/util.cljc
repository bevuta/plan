(ns bevuta.plan.util)

#?(:clj
   (def ^:dynamic *macro-env* {}))

#?(:clj
   (defn cljs-macro?
     ([]
      (cljs-macro? *macro-env*))
     ([env]
      (contains? env :js-globals))))

;; Snatched from `clojure.spec` and slightly refactored
(defn qualify-symbol
  "Qualify symbol by resolving it or using the current *ns*."
  [sym]
  (if-let [ns-sym (some-> sym namespace symbol)]
    (or #?(:clj (if (cljs-macro?)
                  (some-> *macro-env*
                          :ns
                          :requires 
                          (get ns-sym)
                          name
                          (symbol (name sym)))
                  (some-> (get (ns-aliases *ns*) ns-sym)
                          ns-name
                          name
                          (symbol (name sym)))))
        sym)
    (symbol (name (ns-name *ns*)) (name sym))))
