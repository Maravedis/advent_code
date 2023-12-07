(ns advent-of-code.2015.10
  (:require [advent-of-code.utils :refer [char->digit]]))

(defn run [input limit]
  (loop [s  (map char->digit input)
         i 0]
    (if (= i limit)
      (count s)
      (recur (flatten (map #(vector (count %) (first %)) (partition-by identity s)))  (inc i)))))

(comment 
  (run "1321131112" 40)
  (run "1321131112" 50)
  )