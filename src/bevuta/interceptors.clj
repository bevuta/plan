(ns bevuta.interceptors)

(defn call-interceptor-fn [f ctx]
  (try
    (if-let [error (::error ctx)]
      (f (dissoc ctx ::error) error)
      (f ctx))
    (catch Throwable exn
      (assoc ctx ::error exn))))

(defn terminate [ctx]
  (-> ctx
      (assoc ::queue (::stack ctx))
      (dissoc ::stack)))

(defn execute-1 [ctx stage]
  (loop [stage' (if (::error ctx)
                  :error
                  stage)
         ctx (-> ctx
                 (update ::queue seq)
                 (assoc ::stack nil))]
    (if-let [[interceptor] (::queue ctx)]
      (let [interceptor-fn (get interceptor stage')
            ctx' (-> ctx
                     (update ::stack conj interceptor)
                     (update ::queue next)
                     (cond->> interceptor-fn
                       (call-interceptor-fn interceptor-fn)))
            ctx' (if (and (::error ctx') (= :error stage') interceptor-fn)
                   (update ctx' ::suppressed-errors conj (::error ctx))
                   ctx')]
        (cond (::error ctx')
              (if (= :enter stage')
                (terminate ctx')
                (recur :error ctx'))

              (and (= :error stage') (= :enter stage))
              (terminate ctx')

              :else
              (recur stage ctx')))
      (if (= stage' :error)
        (throw (::error ctx))
        (terminate ctx)))))

(defn execute [ctx]
  (-> ctx
      (execute-1 :enter)
      (execute-1 :leave)))
