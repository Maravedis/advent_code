(ns advent-of-code.day8
  (:require [advent-of-code.utils :as u]))

(defn read-height-line [line]
  (mapv #(- (int %) 0x30) line))

(defn count-outer [cluster]
  (+ (count cluster)
     (count (first cluster))))

(defn bi-max [x args]
  (let [as         (map-indexed vector args)
        find-index (fn [coll] (ffirst (drop-while #(< (second %) x) coll)))]
    [(find-index as) (find-index (reverse as))]))

(defn is-visible? [cluster row col]
  (let [tree (get-in cluster [row col])
        [x-col y-col] (bi-max tree (get cluster row))
        [x-row y-row] (bi-max tree (map #(get % col) cluster))]
    (or (>= x-col col) (<= y-col col)
        (>= x-row row) (<= y-row row))))

(defn count-visible [resource]
  (let [cluster (vec (u/read-file-list resource read-height-line))]
       (count (for [row (range (count cluster))
                    col (range (count (first cluster)))
                    :when (is-visible? cluster row col)]
                1))))

(comment
  (count-visible "day8")
  
  )
