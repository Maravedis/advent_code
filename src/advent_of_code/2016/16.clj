(ns advent-of-code.2016.16
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2016 16))
(def tpath (u/test-path 2016 16))

(defn next-step [left]
  (let [right (->> (reverse left)
                   (map #(mod (+ 1 %) 2)))]
    (concat left [0] right)))

(defn checksum [bits]
  (->> (partition 2 bits)
       (map #(if (apply = %) 1 0))))

(defn solve [path size]
  (let [input (->> (u/read-file-line path identity)
                   (map u/char->digit))]
    (->> (drop-while #(< (count %) size) (iterate next-step input))
         first
         (take size)
         (iterate checksum)
         (drop-while #(even? (count %)))
         first
         (apply str))))

(comment
  (solve tpath 20)
  (solve path 272)
  (solve path 35651584) ; insanely slow. Who cares?
  )
