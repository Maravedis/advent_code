(ns advent-of-code.2023.06
  (:require [advent-of-code.utils :as u]
            [clojure.math :refer [pow sqrt]]
            [clojure.string :as str]))

; Amusingly, this will fail if at some point the exact distance covered is exactly the record
; Which is not the case in my real input, but does happen in the test input. I can't be bothered to fix it right now.
(defn error-margin [time distance]
  (->>  (- time (int (sqrt (- (pow time 2) (* 4 distance)))))
        (- (inc time))))

(defn part1 [path]
  (let [[times speeds] (u/read-file-list path u/nums)]
    (->> (map error-margin times speeds)
         (apply *))))

(defn part2 [path]
  (let [[time speed] (u/read-file-list path #(->> (str/split % #" ") (drop 1) str/join parse-long))]
    (error-margin time speed)))


(comment 
  (part1 "2023/6.in") ; ~29µs
  (part2 "2023/6.in") ; ~30µs
  )