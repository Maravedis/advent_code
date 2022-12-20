(defproject advent_of_code "1.0.0"
  :description "The 2022 advent of code event"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [com.rpl/specter "1.1.4"]
                 [org.clojure/core.rrb-vector "0.1.2"]]
  :resource-paths ["resources"]
  :profiles {:dev {:source-paths   ["test" "dev"]
                   :resource-paths ["dev-resources"]}})