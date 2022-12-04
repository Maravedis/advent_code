(ns day4-test
  (:require [advent-of-code.day4 :refer :all]
            [clojure.test :refer :all]))

(deftest read-sections-test
  (are [x y] (= x y)
    [[2 4] [6 8]] (read-sections "2-4,6-8")
    [[1234 5678] [892934 213867]] (read-sections "1234-5678,892934-213867")))

(deftest enclosed?-test
  (are [x y] (= x y)
    true (enclosed? [1 9] [2 8])
    true (enclosed? [2 8] [2 8])
    true (enclosed? [2 8] [1 9])
    false (enclosed? [2 10] [1 9])
    false (enclosed? [1 9] [2 10])
    false (enclosed? [1 3] [4 7])))

(deftest overlap?-test
  (are [x lf ls rf rs] (= x (overlap? [lf ls] [rf rs]))
    true 1 3 2 4
    true 2 4 1 3
    true 2 2 2 4
    true 2 4 2 2
    false 1 3 4 7))

(deftest count-pairs-test
  (is (= 2 (count-pairs "day4_test" enclosed?)))
  (is (= 4 (count-pairs "day4_test" overlap?))))
