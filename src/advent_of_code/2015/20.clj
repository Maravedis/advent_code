(ns advent-of-code.2015.20
  (:require [clojure.math :refer [sqrt]]
            [advent-of-code.utils :as u]))

(defn factors [x]
  (->> (range 1 (int (sqrt x)))
       (keep (fn [i] (when (= 0 (mod x i)) [i (/ x i)])))
       flatten))

(defn part1 [input]
  (->> (iterate inc 1)
       (pmap #(vector (* 10 (reduce + (factors %))) %))
       (drop-while #(< (first %) input))
       first))

(defn part2 [input]
  (->> (range 1 1000000)
       (pmap #(vector (* 11 (reduce + (filter (fn [f] (<= % (* f 50))) (factors %)))) %))
       (drop-while #(< (first %) input))
       first))

(comment
  (def path (u/get-input 2015 20))
  (def input (ffirst (u/read-file-list path u/nums)))

  (time (part1 input))
  (time (part2 input)))

