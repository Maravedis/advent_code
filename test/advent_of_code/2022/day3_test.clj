(ns 2022.day3-test
  (:require [clojure.test :refer :all]
            [advent-of-code.2022.day3 :refer :all]))

(deftest ->score-test
  (are [x y] =
    1 (->score (int \a))
    26 (->score (int \z))
    27 (->score (int \A))
    52 (->score (int \Z))))

(deftest ->priority-test
  (is (= #{1 2} (->priority [\a \b \a \b \a \b]))))

(deftest ->compartments-test
  (are [x y] (= x y)
    [#{1 2 3 15} #{27 52 47 46 28 41}] (->compartments "babacocoAZUTAZBO")))

(deftest priority-test
  (is (= 157 (priority "day3_test"))))

(deftest badge-test
  (is (= 70 (badge "day3_test"))))