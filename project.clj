(defproject advent_of_code "1.0.0"
  :description "The 2022 advent of code event"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.0"]]
  :resource-paths ["resources"]
  :profiles {:dev {:source-paths   ["test" "dev"]
                   :resource-paths ["dev-resources"]}})