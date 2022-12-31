(ns advent-of-code.2022.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn find-unique [input length] 
  (loop [[a & r] input
         i       0
         acc     ""]
    (let [j (str/index-of acc a)]
      (cond (= (count acc) length) i
            (some? j) (recur r (+ i 1) (str (subs acc (+ 1 j)) a))
            :else (recur r (+ i 1) (str acc a))))))

; I realized after finishing you can put a step to partition
; This is way simpler but takes ~2x the time
(defn find-unique-alternative [input length]
  (->> input
       (partition length 1)
       (map (comp count distinct))
       (take-while #(not= length %))
       count
       (+ length)))

(defn run [resource length fn]
  (-> (slurp (io/resource resource))
      (fn length)))


(comment
  (time (run "2022/day6" 4 find-unique))
  (time (run "2022/day6" 4 find-unique-alternative))
  (time (run "2022/day6" 14 find-unique))
  (time (run "2022/day6" 14 find-unique-alternative))
  )