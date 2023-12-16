(ns advent-of-code.2023.16
  (:require [advent-of-code.utils :as u]
            [clojure.core.reducers :as r]))

(defn go [i j d]
  (case d
    0 [i (inc j) d]
    1 [(inc i) j d]
    2 [i (dec j) d]
    3 [(dec i) j d]))

(defn cw [d] (mod (inc d) 4))
(defn ccw [d] (mod (dec d) 4))

(defn mk-next-fn [tiles]
  (fn [[i j d]]
    (case (get-in tiles [i j])
      \. [(go i j d)]
      \- (if (even? d)
           [(go i j d)]
           [(go i j 0) (go i j 2)])
      \| (if (odd? d)
           [(go i j d)]
           [(go i j 3) (go i j 1)])
      \/ (if (even? d) [(go i j (ccw d))] [(go i j (cw d))])
      \\ (if (even? d) [(go i j (cw d))] [(go i j (ccw d))])
      [])))

(defn energize
  ([tiles] (energize tiles [0 0 0]))
  ([tiles start]
   (let [next-fn (mk-next-fn tiles)
         h       (count tiles)
         w       (count (first tiles))]
     (loop [currs   [start]
            visited (transient #{start})]
       (if (empty? currs)
         (count (distinct (map pop (persistent! visited))))
         (let [neighbors (->> (r/mapcat next-fn currs)
                              (r/filter (fn [[i j]] (and (< -1 i h) (< -1 j w))))
                              (r/remove visited)
                              r/foldcat)]
           (recur neighbors (reduce (fn [v e] (conj! v e)) visited neighbors))))))))

(defn part1 [path]
  (let [input (vec (u/read-file-list path vec))]
    (energize input)))

(defn part2 [path]
  (let [input  (vec (u/read-file-list path vec))
        h      (count input)
        w      (count (first input))
        starts (vec (concat (for [i (range 0 h)] [i 0 0])
                            (for [j (range 0 w)] [0 j 1])
                            (for [i (range 0 h)] [i (dec w) 2])
                            (for [j (range 0 w)] [(dec h) j 3])))]
    (r/fold 55 (r/monoid max (constantly 0)) #(max %1 (energize input %2)) starts)))

(comment
  (def path (u/get-input 2023 16))
  (def tpath "2023/16_test.in")

  (time (part1 path))
  (time (part2 path))
)