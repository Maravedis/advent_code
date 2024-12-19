(ns advent-of-code.2024.11
  (:require [advent-of-code.utils :as u]
            [advent-of-code.math :refer [strlen]]
            [clojure.math :refer [pow]]))

(defn calc-stone [stone]
  (cond (= 0 stone) [1]
        (even? (strlen stone)) (let [d (int (pow 10 (/ (strlen stone) 2)))]
                                 [(quot stone d) (rem stone d)])
        :else [(* 2024 stone)]))

(def count-future-stones
  (memoize (fn [stone depth]
             (if (= 0 depth)
               1
               (apply + (map #(count-future-stones % (dec depth)) (calc-stone stone)))))))

(defn solve [path depth]
  (let [input (->> (u/read-file-list path u/nums) first)]
    (apply + (map #(count-future-stones % depth) input))))

(comment
  (def path (u/get-input 2024 11))
  (def tpath (u/test-path 2024 11))
  (time (solve path 25))
  (time (solve path 75)))