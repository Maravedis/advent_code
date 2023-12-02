(ns 2022.day5-test
  (:require [advent-of-code.2022.day5 :refer :all]
            [clojure.test :refer :all]))

(deftest read-stack-line-test
  (are [res input] (= res (read-stack-line {} input))
    {2 [\D]} "    [D]    "
    {1 [\N]
     2 [\C]} "[N] [C]    "
    {1 [\Z]
     2 [\M]
     3 [\P]} "[Z] [M] [P]"
    ))

(deftest read-stacks-test
  (is ( = {1 [\N \Z]
           2 [\D \C \M]
           3 [\P]} (read-stacks ["    [D]    "
                                 "[N] [C]    "
                                 "[Z] [M] [P]"
                                 " 1   2   3 "]))))

(deftest read-instruction-line-test
  (are [result input] (= result (read-instruction-line input))
       [1 2 1] "move 1 from 2 to 1"))

(deftest parse-input-test
  (is (= {:stacks {1 [\N \Z]
                   2 [\D \C \M]
                   3 [\P]}
          :ins [[1 2 1]
                [3 1 3]
                [2 2 1]
                [1 1 2]]}
         (parse-input "day5_test"))))

(deftest move-test
  (is (= 
       {1 [\C]
        2 [\M]
        3 [\Z \N \D \P]}
       (reduce (partial move false)
               {1 [\N \Z]
                2 [\D \C \M]
                3 [\P]}
               [[1 2 1]
                [3 1 3]
                [2 2 1]
                [1 1 2]]))))

(deftest execute-test
  (is (= "CMZ" (execute "day5_test" false)))
  (is (= "MCD" (execute "day5_test" true))))
