(ns day8-test
  (:require [clojure.test :refer :all]
            [advent-of-code.day8 :refer :all]))

(def cluster [[3 0 3 7 3]
              [2 5 5 1 2]
              [6 5 3 3 2]
              [3 3 5 4 9]
              [3 5 3 9 0]])


(deftest is-visible-test
  (is (= (map-indexed
          (fn [col line]
            (map-indexed
             (fn [row _]
               (is-visible? cluster row col))
             line))
          cluster)
         [[true true true true true]
          [true true true false true]
          [true true false true true]
          [true false true false true]
          [true true true true true]])))

(deftest calc-vision-1d-test
  (are [result line idx] (= result (calc-vision-1d line idx))
    0 [3 0 3 7 3] 0
    1 [3 0 3 7 3] 1
    2 [3 0 3 7 3] 2
    3 [3 0 3 7 3] 3
    0 [3 0 3 7 3] 4
    2 [2 5 5 1 2] 2
    2 [3 5 3 5 3] 1
    4 [3 3 5 4 9] 2
    2 [3 5 3 5 3] 3
    ))

(deftest calc-vision-test
  (are [result row col] (= result (calc-vision cluster row col))
    4 1 2
    8 3 2))

(deftest count-visible-test
  (is (= 21 (count-visible "day8_test"))))

(deftest find-highest-vision-test
  (is (= 8 (find-highest-vision "day8_test"))))