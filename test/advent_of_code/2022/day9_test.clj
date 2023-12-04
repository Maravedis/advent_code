(ns advent-of-code.2022.day9-test
  (:require [advent-of-code.2022.day9 :refer :all]
            [clojure.test :refer :all]))

(deftest count-tail-positions-test
  (is (= 13 (count-tail-positions "2022/day9_test" 1)))
  (is (= 1 (count-tail-positions "2022/day9_test" 9)))
  (is (= 36 (count-tail-positions "2022/day9_test2" 9))))
