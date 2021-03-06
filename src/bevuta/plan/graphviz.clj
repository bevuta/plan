(ns bevuta.plan.graphviz
  (:require [bevuta.plan :as p]
            [clojure.string :as str]
            [clojure.java.shell :as shell]
            [clojure.java.io :as io])
  (:import javax.imageio.ImageIO
           javax.swing.ImageIcon
           javax.swing.JLabel
           javax.swing.JPanel
           javax.swing.JFrame
           javax.swing.JOptionPane))


(defn quoted
  ([prefix s]
   (if prefix
     (quoted (str prefix ":" s))
     (quoted s)))
  ([s]
   (str \" (str/replace s #"\"" "\\\"") \")))

(defn current-ns-aliases []
  (into {(name (ns-name *ns*)) nil}
        (map (fn [[alias ns]]
               [(name (ns-name ns)) (name alias)]))
        (ns-aliases *ns*)))

(defn aliased-sym [aliases sym]
  (let [ns (namespace sym)]
    (symbol (get aliases ns ns)
            (name sym))))

(defn step-deps-dot
  ([quoted-step-name aliases deps]
   (step-deps-dot quoted-step-name nil aliases deps))
  ([quoted-step-name prefix aliases deps]
   (map (fn [dep]
          (str (quoted prefix (aliased-sym aliases dep))
               " -> "
               quoted-step-name))
        deps)))

(defn step-names-dot [prefix aliases steps]
  (->> (map :name steps)
       (map (fn [n] (aliased-sym aliases n)))
       (map (fn [aliased-name]
              (str (quoted prefix aliased-name)
                   " [label=" (quoted aliased-name) "]")))))

(defn steps-dot
  ([aliases steps]
   (steps-dot nil aliases steps))
  ([prefix aliases steps]
   (mapcat (fn [{:keys [deps name plan goal]}]
             (let [name (aliased-sym aliases name)
                   qname (quoted prefix name)]
               (concat
                (step-deps-dot qname prefix aliases deps)
                (when plan
                  (let [goal (quoted name (aliased-sym aliases goal))
                        steps (vals (::p/steps plan))]
                    (concat [(str goal " -> " qname)
                             (str "subgraph " (quoted (str "cluster:" name)) "{\n"
                                  "  graph [label=" (quoted (str name " plan")) "]")]
                            (step-names-dot name aliases steps)
                            (steps-dot name aliases steps)
                            ["}"]))))))
           steps)))

(defn plan-name [plan]
  (or (::p/name plan) "plan"))

(defn dot [plan]
  (let [aliases (current-ns-aliases)]
    (str "digraph "
         (quoted (plan-name plan))
         " {\n"
         (->> (concat
               (->> (::p/inputs plan)
                    (map (fn [input]
                           (str (quoted (aliased-sym aliases input)) " [fillcolor=green,style=filled]"))))
               (->> (::p/steps plan) vals (steps-dot aliases)))
              (map (fn [stmt]
                     (str "  " stmt ";")))
              (str/join "\n"))
         "\n}")))

(defn render [plan]
  (with-open [in (java.io.ByteArrayInputStream.
                  (:out (shell/sh "dot" "-Tpng"
                                  :in (dot plan)
                                  :out-enc :bytes)))]
    (ImageIO/read in)))

(defn show [plan]
  (let [png   (render plan)
        image (ImageIcon. png)
        label (JLabel.  image)]
    (JOptionPane/showMessageDialog nil
                                   label
                                   (plan-name plan)
                                   JOptionPane/PLAIN_MESSAGE
                                   nil)))
