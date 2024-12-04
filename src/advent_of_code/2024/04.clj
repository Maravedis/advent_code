(ns advent-of-code.2024.04
  (:require [advent-of-code.utils :as u]
            [advent-of-code.points :as p]))

(def xmas #{(seq "XMAS") (seq "SAMX")})

(defn part1 [path]
  (let [grid (vec (u/read-file-list path vec))]
    (reduce-kv
     (fn [acc r row]
       (reduce-kv
        (fn [acc c v]
          (if (#{\X \S} v)
            (+ acc (u/count-when xmas (map #(p/extract-line grid [r c] % 4) [p/E p/S p/NE p/SE])))
            acc)) acc row)) 0 grid)))

(defn part2 [path]
  (let [input (vec (u/read-file-list path vec))
        w (count (first input))
        h (count input)]
    (reduce-kv
     (fn [acc r v]
       (reduce-kv
        (fn [acc c v]
          (if (and (= v \A) (< 0 r (dec h)) (< 0 c (dec w))
                   (#{"SSMM" "MMSS" "SMMS" "MSSM"}
                    (apply str (map #(get-in input (p/move [r c] %)) [p/NW p/NE p/SE p/SW]))))
            (inc acc)
            acc)) acc v)) 0 input)))

(comment
  (def path (u/get-input 2024 4))
  (def tpath "2024/4_test.in")
  
  (part1 path)
  (part2 path)
  )

