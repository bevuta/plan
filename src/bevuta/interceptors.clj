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

(defn execute-stage [ctx stage]
  (loop [ctx (-> ctx
                 (update ::queue seq)
                 (assoc ::stack nil))]
    (if-let [[interceptor] (::queue ctx)]
      (let [interceptor-key (if (::error ctx)
                              :error
                              stage)
            interceptor-fn (get interceptor interceptor-key)
            ctx' (-> ctx
                     (update ::stack conj interceptor)
                     (update ::queue next)
                     (cond->> interceptor-fn
                       (call-interceptor-fn interceptor-fn)))
            ctx' (if (and (::error ctx) (::error ctx') interceptor-fn)
                   (update ctx' ::suppressed-errors conj (::error ctx))
                   ctx')]
        (if (and (::error ctx') (= :enter stage))
          (terminate ctx')
          (recur ctx')))
      (if-let [error (::error ctx)]
        (throw error)
        (terminate ctx)))))

(defn execute [ctx]
  (-> ctx
      (execute-stage :enter)
      (execute-stage :leave)
      (dissoc ::queue ::stack ::error)))
