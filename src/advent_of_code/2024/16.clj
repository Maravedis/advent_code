(ns advent-of-code.2024.16
  (:require [advent-of-code.points :as p]
            [advent-of-code.utils :as u]
            [clojure.data.priority-map :refer [priority-map]]))

(defn retrace-way [seen end]
  (loop [open [end]
         path #{}]
    (if (empty? open)
      path
      (recur (->> (mapcat (comp first seen) open)
                  (remove path))
             (apply conj path open)))))

(defn prev-update [[prevs scorep :as v] [curr score]]
  (cond (or (not v) (< score scorep)) [[curr] score]
        (= score scorep) [(conj prevs curr) score]
        :else v))

(defn solve [path]
  (let [grid  (->> (u/read-file-list path vec)
                   (p/->grid))
        start (ffirst (filter #(= \S (second %)) grid))
        end   (ffirst (filter #(= \E (second %)) grid))]
    (loop [dist {[start p/E] 0}
           prev {}
           open (priority-map [start p/E] 0)]
      (if (empty? open)
        (let [paths (retrace-way prev [end p/N])]
          {:first  (dist [end p/N])
           :second (count (distinct (map first paths)))})
        (let [[[c dir :as curr] scorec] (peek open)
              neighbors                 (->> [[[(p/move c dir) dir] (inc scorec)]
                                              [[(p/move c (p/turn-right dir)) (p/turn-right dir)] (+ 1001 scorec)]
                                              [[(p/move c (p/turn-left dir)) (p/turn-left dir)] (+ 1001 scorec)]]
                                             (remove #(when-let [v (grid (ffirst %))] (= \# v)))
                                             (filter (fn [[coord score]] (<= score (get dist coord Integer/MAX_VALUE)))))]
          (recur (into dist (map (fn [[coord score]] [coord score]) neighbors))
                 (reduce (fn [acc n] (update acc (first n) prev-update [curr (second n)])) prev neighbors)
                 (into (pop open) neighbors)))))))

(comment
  (def path (u/get-input 2024 16))
  (def tpath (u/test-path 2024 16))

  (solve path))