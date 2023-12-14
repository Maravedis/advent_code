(ns advent-of-code.2023.09
  (:require [advent-of-code.utils :as u]))

(defn extrapolate
  ([line] (extrapolate line 0))
  ([[h :as line] result]
   (if  (every? #(= 0 %) line)
     result
     (recur (map #(apply - %) (partition 2 1 line)) (+ result h)))))

(defn part1 [path]
  (let [data (u/read-file-list path u/nums)]
    (reduce #(+ %1 (extrapolate %2)) 0 (map reverse data))))

(defn part2 [path]
  (let [data (u/read-file-list path u/nums)]
    (reduce #(+ %1 (extrapolate %2)) 0 data)))

(comment
  (def path (u/get-input 2023 9))
  (time (part1 path))
  (time (part2 path))
  )