(ns advent-of-code.2024.01
  (:require [advent-of-code.utils :as u]))

(defn part1 [path]
  (let [[l1 l2] (->> (u/read-file-list path u/nums)
                     ((juxt #(->> % (map first) sort) #(->> % (map second) sort))))]
    (apply + (map (comp abs -) l1 l2))))

(defn part2 [path]
  (let [[l1 l2] (->> (u/read-file-list path u/nums)
                     ((juxt #(map first %)
                            #(->> % (map second) frequencies))))]
    (reduce (fn [acc v] (+ acc (* v (get l2 v 0)))) 0 l1)))

(comment
  (def path (u/get-input 2024 1))

  (part1 path)
  (part2 path))