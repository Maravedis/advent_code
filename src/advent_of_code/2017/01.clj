(ns advent-of-code.2017.01
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2017 1))

(defn add-last-to-first [coll]
  (cons (last coll) coll))

(defn part1 [path]
  (->> (u/read-file-line path identity)
       (map u/char->digit)
       add-last-to-first
       (partition 2 1)
       (reduce (fn [acc [x y]] (cond-> acc (= x y) (+ x))) 0)))

(defn part2 [path]
  (let [input (->> (u/read-file-line path identity)
                   (mapv u/char->digit))
        n     (count input)]
    (reduce-kv (fn [acc i v] (cond-> acc (= v (get input (mod (+ i (/ n 2)) n))) (+ v))) 0 input)))

(comment
  (part1 path)
  (part2 path)

  ;
  )
