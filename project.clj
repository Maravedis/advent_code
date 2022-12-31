(defproject advent_of_code "1.0.0"
  :description "Advent of code solves"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [com.rpl/specter "1.1.4"]
                 [org.clojure/core.rrb-vector "0.1.2"]
                 [org.clojure/math.numeric-tower "0.0.5"]] 
  :resource-paths ["resources" "inputs"]
  :jvm-opts ["-Xss500M"]
  :profiles {:dev {:source-paths   ["test" "dev"]
                   :resource-paths ["test-inputs"]}})