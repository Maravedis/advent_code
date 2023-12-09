(ns advent-of-code.2023.09
  (:require [advent-of-code.utils :as u]))

(defn extrapolate [[h :as line] result] 
  (if  (every? #(= 0 %) line)
    (apply + result)
    (recur (map #(apply - %) (partition 2 1 line)) (conj result h))))

(defn part1 [path]
  (let [data (u/read-file-list path u/nums)]
    (reduce #(+ %1 (extrapolate %2 [])) 0 (map reverse data))))

(defn part2 [path]
  (let [data (u/read-file-list path u/nums)]
    (reduce #(+ %1 (extrapolate %2 [])) 0 data)))

(comment
  (def path (u/get-input 2023 9))
  (part1 path)
  (part2 path)
  )