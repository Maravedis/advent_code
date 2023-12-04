(ns advent-of-code.2022.day6-test
  (:require [clojure.test :refer :all]
            [advent-of-code.2022.day6 :refer :all]))

(deftest find-unique-test
  (are [result input length] (= result (find-unique input length))
    5 "bvwbjplbgvbhsrlpgdmjqwftvncz" 4
    6 "nppdvjthqldpwncqszvftbrmjlhg" 4
    10 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 4
    11 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 4
    19 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14
    23 "bvwbjplbgvbhsrlpgdmjqwftvncz" 14
    23 "nppdvjthqldpwncqszvftbrmjlhg" 14
    29 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 14
    26 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 14))
