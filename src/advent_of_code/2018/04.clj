(ns advent-of-code.2018.04
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2018 4))
(def tpath (u/test-path 2018 4))

(defn process [shifts]
  (loop [[[_ _ _ start-h start-m id] & t] shifts
         acc                              {}]
    (if (nil? start-m)
      (map (fn [[gid minutes]]
             (let [[max-m c total] (reduce (fn [[max-m c total] [m v]]
                                             (if (> v c)
                                               [m v (+ total v)]
                                               [max-m c (+ total v)])) [0 0 0] minutes)]
               [gid max-m c total])) acc)
      (let [[coll t]  (split-with #(< (count %) 6) t)
            [_ _ acc] (reduce
                       (fn [[asleep? start-m acc] [_ _ _ _ new-m]]
                         [(not asleep?) new-m
                          (if (not asleep?) acc
                              (reduce #(update-in %1 [id %2] (fnil inc 0)) acc (range start-m new-m)))])
                       [false (if (= 23 start-h) 0 start-m) (cond-> acc (not (acc id)) (assoc id {}))]
                       coll)]
        (recur t acc)))))

(defn solve [path]
  (let [input (->> (u/read-file-list path #(re-seq #"\d+" %))
                   (map #(mapv parse-long %))
                   (sort-by #(subvec % 0 5))
                   process)
        part1 (->> (apply max-key #(nth % 3) input)
                   (take 2)
                   (apply *))
        part2 (->> (apply max-key #(nth % 2) input)
                   (take 2)
                   (apply *))]
    [part1 part2]))

(comment
  (solve tpath)
  (solve path))
