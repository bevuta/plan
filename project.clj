(defproject bevuta/plan "2-SNAPSHOT"
  :description "A clojure.spec flavored adaption of Prismatic^WPlumatic Graph"
  :url "https://github.com/bevuta/plan"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.logging "0.4.1"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}})
