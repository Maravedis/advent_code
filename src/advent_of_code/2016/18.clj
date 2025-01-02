(ns advent-of-code.2016.18
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2016 18))
(def tpath (u/test-path 2016 18))

(defn safe? [prev-row col]
  (let [left   (get prev-row (dec col) true)
        center (get prev-row col)
        right  (get prev-row (inc col) true)]
    (or (and left center right)
        (not (or left center right))
        (and center (not (or left right)))
        (and right left (not center)))))

(defn next-row [prev-row]
  (->> (range (count prev-row))
       (mapv (fn [c] (safe? prev-row c)))))

(defn parse-line [s]
  (map #(= % \.) s))

(defn solve [path size]
  (->> (u/read-file-line path parse-line)
       vec
       (iterate next-row)
       (take size)
       (reduce #(+ %1 (u/count-when identity %2)) 0)))

(comment

  (solve tpath 10)
  (solve path 40)

  (time (solve path 400000)))
