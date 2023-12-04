(defproject advent_of_code "1.0.0"
  :description "Advent of code solves"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.1"]
                 [org.clojure/math.combinatorics "0.2.0"]
                 [com.rpl/specter "1.1.4"]
                 [criterium "0.4.6"]
                 [org.clojure/core.rrb-vector "0.1.2"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [clj-http "3.12.3"]
                 [clojure.java-time "1.4.2"]
                 [cheshire "5.12.0"]
                 [org.clj-commons/digest "1.4.100"]] 
  :resource-paths ["resources" "inputs"]
  :jvm-opts ["-Xss500M"]
  :profiles {:dev {:source-paths   ["test" "dev"]
                   :resource-paths ["test-inputs"]}})