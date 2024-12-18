(ns advent-of-code.2015.25
  (:require [advent-of-code.utils :as u]))

(defn calc [row col]
  (let [op (fn [x] (mod (*' 252533 x) 33554393))]
    (loop [r   1
           c   1
           acc 20151125]
      (if (and (= row r) (= c col))
        acc
        (if (= r 1)
          (recur (+ c 1) 1 (op acc))
          (recur (- r 1) (+ c 1) (op acc)))))))

(comment
  (def path (u/get-input 2015 25))

  (let [[row col] (first (u/read-file-list path u/nums))]
    (calc row col)))