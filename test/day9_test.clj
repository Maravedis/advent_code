(ns day9-test
  (:require [advent-of-code.day9 :refer :all]
            [clojure.test :refer :all]))

(deftest count-tail-positions-test
  (is (= 13 (count-tail-positions "day9_test" 1)))
  (is (= 1 (count-tail-positions "day9_test" 9)))
  (is (= 36 (count-tail-positions "day9_test2" 9))))