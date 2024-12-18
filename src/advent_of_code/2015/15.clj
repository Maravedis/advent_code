(ns advent-of-code.2015.15
  (:require [advent-of-code.utils :as u]
            [clojure.core.matrix :as m]))

(defn score
  ([input factors]
   (apply * (map #(max 0 %) (first (m/mmul factors input)))))
  ([input factors calories]
   (if (= 500 (m/inner-product (first factors) calories))
     (score input factors)
     0)))

(def permutations-4
  (for [i     (range 0 101)
        j     (range 0 101)
        k     (range 0 101)
        l     (range 0 101)
        :when (= 100 (+ i j k l))]
    (vec (repeat 4 [i j k l]))))

(defn part1 [path]
  (let [input (vec (u/read-file-list path (comp pop u/nums)))]
    (loop [[h & t] permutations-4
           curr    -1]
      (if (nil? h)
        curr
        (recur t (max curr (score input h)))))))

(defn part2 [path]
  (let [input    (u/read-file-list path u/nums)
        calories (mapv last input)
        input    (mapv #(vec (take 4 %)) input)]
    (loop [[h & t] permutations-4
           curr    -1]
      (if (nil? h)
        curr
        (recur t (max curr (score input h calories)))))))

(comment
  (def path (u/get-input 2015 15))
  (def tpath "2015/15_test.in")

  (part1 path)
  (part2 path))