(ns advent-of-code.2017.14
  (:require [advent-of-code.2017.10 :refer [dense-knot-hash]]
            [advent-of-code.points :as p]
            [advent-of-code.utils :as u]
            [clojure.core.reducers :as r]))

(def path (u/get-input 2017 14))
(def tpath (u/test-path 2017 14))

(defn ->binary [x]
  (map #(bit-and 1 (bit-shift-right x %)) (range 7 -1 -1)))

(defn ->grid-line [key-string i]
  (->> (dense-knot-hash (str key-string "-" i))
       (mapcat ->binary)
       vec))

(defn part1 [path]
  (let [input (u/read-file-line path identity)]
    (->> (r/map #(->grid-line input %) (vec (range 128)))
         (r/fold 16 + (fn [acc line] (+ acc (u/count-when #(= 1 %) line)))))))

(defn part2 [path]
  (let [key-string (u/read-file-line path identity)
        grid       (reduce (fn [acc r]
                             (reduce-kv (fn [acc c v] (assoc acc [r c] v))
                                        acc
                                        (->grid-line key-string r)))
                           {} (range 128))]
    (loop [open    (->> (filter (fn [[_ v]] (= v 1)) grid)
                        (map first)
                        set)
           regions 0]
      (if-let [curr (first open)]
        (let [region (p/flood-fill grid curr #(= 1 (get grid %)))]
          (recur (apply disj open region) (inc regions)))
        regions))))

(comment

  (part1 tpath)
  (part1 path)
  (part2 tpath)
  (part2 path))
