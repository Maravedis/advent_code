(ns advent-of-code.2025.04
  (:require
   [advent-of-code.utils :as u]
   [advent-of-code.points :as p]))

(def path (u/get-input 2025 4))
(def test-path (u/test-path 2025 4))

(defn movable-roll? [grid [point v]]
  (and (= v \@)
       (< (count (p/neighbours-box point #(= \@ (get grid %)))) 4)))

(defn part1 [path]
  (let [grid  (p/->grid (u/read-file-list path vec))]
    (reduce-kv #(cond-> %1 (movable-roll? grid [%2 %3]) inc) 0 grid)))

(defn part2 [path]
  (loop [grid    (p/->grid (u/read-file-list path vec))
         removed 0]
    (let [to-remove (keep #(when (movable-roll? grid %) (first %)) grid)]
      (if (empty? to-remove)
        removed
        (recur (reduce dissoc grid to-remove) (+ removed (count to-remove)))))))

(comment
  (part1 test-path)
  (part1 path)

  (part2 test-path)
  (part2 path))