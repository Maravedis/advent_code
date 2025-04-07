(ns advent-of-code.2018.01
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2018 1))
(def tpath (u/test-path 2018 1))

(defn part1 [path]
  (->> (u/read-file-list path u/nums)
       flatten
       (reduce +)))

(defn part2 [path]
  (->> (u/read-file-list path u/nums)
       flatten
       cycle
       (reductions +)
       (reduce (fn [acc x] (if (acc x) (reduced x) (conj! acc x))) (transient #{}))))

(comment

  (part1 tpath)
  (part1 path)

  (part2 tpath)
  (part2 path))
