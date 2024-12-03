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
                 [org.clj-commons/digest "1.4.100"]
                 [tesser.core "1.0.6"]
                 [org.clojure/data.priority-map "1.1.0"]
                 [net.mikera/core.matrix "0.63.0"]
                 [com.microsoft/z3 "4.12.4"]
                 [org.jgrapht/jgrapht-core "1.5.2"]
                 [dom-top "1.0.9"]] 
  :resource-paths ["resources" "inputs"]
  :jvm-opts ["-Xss1G" "-Djava.library.path=/home/clement/Projects/z3/build"]
  :profiles {:dev {:source-paths   ["test" "dev"]
                   :resource-paths ["test-inputs"]}})