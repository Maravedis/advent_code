(ns advent-of-code.2017.22
  (:require [advent-of-code.utils :as u]
            [advent-of-code.points :as p]))

(def path (u/get-input 2017 22))
(def tpath (u/test-path 2017 22))

(defn part1 [path limit]
  (let [input (vec (u/read-file-list path vec))
        n     (quot (count input) 2)
        grid  (reduce-kv (fn [acc r line] (reduce-kv (fn [acc c v] (cond-> acc (= v \#) (conj! [(- r n) (- c n)]))) acc line)) (transient #{}) input)]
    (loop [curr  p/origin
           dir   p/N
           nodes grid
           step  0
           acc   0]
      (if (>= step limit)
        acc
        (if (nodes curr)
          (recur (p/move curr (p/turn-right dir)) (p/turn-right dir) (disj! nodes curr) (inc step) acc)
          (recur (p/move curr (p/turn-left dir)) (p/turn-left dir) (conj! nodes curr) (inc step) (inc acc)))))))

(defn part2 [path limit]
  (let [input (vec (u/read-file-list path vec))
        n     (quot (count input) 2)
        grid  (reduce-kv (fn [acc r line] (reduce-kv (fn [acc c v] (cond-> acc (= v \#) (assoc! [(- r n) (- c n)] 2))) acc line)) (transient {}) input)]
    (loop [curr  p/origin
           dir   p/N
           nodes grid
           step  0
           acc   0]
      (if (>= step limit)
        acc
        (case (nodes curr 0)
          0 (recur (p/move curr (p/turn-left dir)) (p/turn-left dir) (assoc! nodes curr 1) (inc step) acc)
          1 (recur (p/move curr dir) dir (assoc! nodes curr 2) (inc step) (inc acc))
          2 (recur (p/move curr (p/turn-right dir)) (p/turn-right dir) (assoc! nodes curr 3) (inc step) acc)
          3 (recur (p/move curr (p/turn-around dir)) (p/turn-around dir) (assoc! nodes curr 0) (inc step) acc))))))

(comment
  (part1 tpath 10000)
  (part1 path 10000)
  (part2 tpath 100)
  (part2 path 10000000))
