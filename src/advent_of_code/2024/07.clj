(ns advent-of-code.2024.07
  (:require [advent-of-code.utils :as u]
            [clojure.core.reducers :as r]
            [clojure.string :refer [ends-with?]]
            [criterium.core :refer [quick-bench]]))

(def path (u/get-input 2024 7))
(def tpath (u/test-path 2024 7))
(def input (vec (u/read-file-list path u/nums)))

(defn *-1 [x y] (when (zero? (rem x y)) (quot x y)))
(defn ||-1 [x y]
  (let [xs (str x)
        ys (str y)]
    (when (ends-with? xs ys)
      (parse-long (subs xs 0 (- (count xs) (count ys)))))))

(defn calc [part2? [total & factors]]
  (let [ops (apply juxt (if part2? [- *-1 ||-1] [- *-1]))
        fs  (vec (reverse factors))]
    (->> (reduce (fn [states v]
                   (->> (r/mapcat #(ops % v) states)
                        (r/remove #((fnil < -1) % 0))
                        r/foldcat)) [total] fs)
         (some #(= 0 %)))))

(defn solve [input part2?]
  (->> (r/filter (partial calc part2?) input)
       (r/map first)
       (r/fold 100 + +)))

(comment
  (quick-bench (solve input false)) ; ~2.4ms
  ; 1582598718861
  (quick-bench (solve input true))  ; ~4.9ms
  ; 165278151522644
  )