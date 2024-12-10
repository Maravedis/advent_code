(ns advent-of-code.2024.10
  (:require
   [advent-of-code.utils :as u]
   [advent-of-code.points :as p]))

(def path (u/get-input 2024 10))
(def grid (-> (u/read-file-list path vec)
              p/->grid
              (update-vals u/char->digit)))
(def starts (->> grid
                 (filter (fn [[_ v]] (= 0 v)))
                 (map first)))

(defn reachable-ends [grid start]
  (->> (iterate (fn [[level curs]] (->> (mapcat #(map (partial p/move %) p/cardinals) curs)
                                        (filter #(when-let [num (grid %)] (= num (+ 1 level))))
                                        (vector (inc level)))) [0 [start]])
       (drop 9) first second))

(defn part1 []
  (->> (map #(reachable-ends grid %) starts)
       (reduce #(+ %1 (count (set %2))) 0)))

(defn part2 []
  (->> (map #(reachable-ends grid %) starts)
       (reduce #(+ %1 (count %2)) 0)))

(comment
  (part1)
  (part2))
