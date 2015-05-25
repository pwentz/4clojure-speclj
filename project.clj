(defproject fourclojure "0.1.1"
  :description "4clojure problems using speclj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :profiles {:dev {:dependencies [[speclj "3.2.0"]]}}
  :plugins [[speclj "3.2.0"]]
  :test-paths ["spec"])
