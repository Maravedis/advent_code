(ns day7-test
  (:require [advent-of-code.day7 :refer :all]
            [clojure.test :refer :all]))


(deftest run-first-test
  (is (= 95437 (run-first "day7_test"))))

(deftest run-second-test
  (is (= 24933642 (run-second "day7_test"))))

