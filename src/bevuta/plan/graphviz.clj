(ns bevuta.plan.graphviz
  (:require [bevuta.plan :as p]
            [clojure.string :as str]))

(defn quoted [s]
  (str \" (str/replace s #"\"" "\\\"") \"))

(defn current-ns-aliases []
  (into {(name (ns-name *ns*)) nil}
        (map (fn [[alias ns]]
               [(name (ns-name ns)) (name alias)]))
        (ns-aliases *ns*)))

(defn aliased-sym [aliases sym]
  (let [ns (namespace sym)]
    (symbol (get aliases ns ns)
            (name sym))))

(defn dot [plan]
  (let [aliases (current-ns-aliases)]
    (str "digraph "
         (quoted (or (::p/name plan) "plan"))
         " {\n"
         (->> (concat
               (->> (::p/inputs plan)
                    (map (fn [input]
                           (str (quoted (aliased-sym aliases input)) " [fillcolor=green,style=filled]"))))
               (->> (::p/steps plan)
                    (mapcat (fn [{:keys [deps name]}]
                              (let [name (quoted (aliased-sym aliases name))]
                                (map (fn [dep]
                                       (str (quoted (aliased-sym aliases dep))
                                            " -> "
                                            name))
                                     deps))))))
              (map (fn [stmt]
                     (str "  " stmt ";")))
              (str/join "\n"))
         "\n}")))
