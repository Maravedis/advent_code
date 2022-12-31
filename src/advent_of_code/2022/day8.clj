(ns advent-of-code.2022.day8
  (:require [advent-of-code.utils :as u]))

(defn read-height-line [line]
  (mapv #(- (int %) 0x30) line))

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

(defn calc-vision-1d [line idx]
  (let [tree      (get line idx)
        after     (drop (+ 1 idx) line)
        before    (reverse (take idx line))
        vision-fn (fn [coll]
                    (let [[a [h]] (split-with #(< % tree) coll)]
                      (cond-> (count a)
                        (and (some? h) (>= h tree)) (+ 1))))]
    (* (vision-fn before)
       (vision-fn after))))

(defn calc-vision [cluster row col]
  (* (calc-vision-1d (get cluster row) col)
     (calc-vision-1d (mapv #(get % col) cluster) row)))

(defn count-visible [resource]
  (let [cluster (vec (u/read-file-list resource read-height-line))]
       (count (for [row (range (count cluster))
                    col (range (count (first cluster)))
                    :when (is-visible? cluster row col)]
                1))))

(defn find-highest-vision [resource]
  (let [cluster (vec (u/read-file-list resource read-height-line))]
    (apply max (for [row (range (count cluster))
                     col (range (count (first cluster)))]
                 (calc-vision cluster row col)))))

(comment
  (count-visible "2022/day8")
  (find-highest-vision "2022/day8")
  )
