(ns advent-of-code.2022.day7-test
  (:require [advent-of-code.2022.day7 :refer :all]
            [clojure.test :refer :all]))


(deftest run-first-test
  (is (= 95437 (run-first "2022/day7_test"))))

(deftest run-second-test
  (is (= 24933642 (run-second "2022/day7_test"))))

