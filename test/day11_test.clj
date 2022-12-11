(ns day11-test
  (:require [clojure.test :refer :all]
            [advent-of-code.day11 :refer :all]
            [clojure.java.io :as io]
            [clojure.string :refer [split split-lines]]))

(def monkeys (-> (io/resource "day11_test")
                 slurp
                 (split #"\n\n")))

(read-monkey (drop 1 (split-lines (first monkeys))))

(deftest read-monkey-test
  (is ( = (read-monkey (drop 1 (split-lines (first monkeys)))) {})))