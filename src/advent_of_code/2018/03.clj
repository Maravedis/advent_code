(ns advent-of-code.2018.03
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2018 3))
(def tpath (u/test-path 2018 3))

(defn ->overlaps [claims]
  (reduce (fn [acc [id col row w h]]
            (->> (for [c (range col (+ col w))
                       r (range row (+ row h))] [r c])
                 (reduce #(update %1 %2 (fnil conj []) id) acc))) {} claims))

(defn part1 [path]
  (->> (u/read-file-list path u/nums)
       ->overlaps
       (u/count-when (fn [[_ v]] (> (count v) 1)))))

(defn part2 [path]
  (let [input    (u/read-file-list path u/nums)
        overlaps (->overlaps input)
        ids      (set (map first input))]
    (reduce (fn [acc [_ vs]] (if (> (count vs) 1) (apply disj acc vs) acc)) ids overlaps)))

(comment
  (part1 tpath)
  (part1 path)

  (part2 tpath)
  (part2 path))
