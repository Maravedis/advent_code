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

(defn reachable-ends [grid start cur]
  (if (= 9 (grid start))
    [start]
    (->> p/cardinals
         (map #(p/move start %))
         (filter #(when-let [num (grid %)] (= num (+ cur 1))))
         (mapcat #(reachable-ends grid % (+ cur 1)))
         set)))

(defn count-distinct-trail [grid start cur]
  (if (= 9 (grid start))
    1
    (->> p/cardinals
         (map #(p/move start %))
         (filter #(when-let [num (grid %)] (= num (+ cur 1))))
         (map #(count-distinct-trail grid % (+ cur 1)))
         (reduce +))))

(defn part1 []
  (->> (map #(reachable-ends grid % 0) starts)
       (reduce #(+ %1 (count %2)) 0)))

(defn part2 []
  (->> (map #(count-distinct-trail grid % 0) starts)
       (reduce +)))

(comment
  (part1)
  (part2))
