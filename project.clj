(defproject bevuta/plan "2-SNAPSHOT"
  :description "A clojure.spec flavored adaption of Prismatic^WPlumatic Graph"
  :url "https://github.com/bevuta/plan"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/tools.logging "1.1.0"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "1.0.0"]]}})
