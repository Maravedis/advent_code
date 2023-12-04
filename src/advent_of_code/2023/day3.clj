(ns advent-of-code.2023.day3
  (:require [advent-of-code.utils :as u]))

(defn read-schematics [path]
  (reduce-kv (fn [[nums syms] i line]
               [(conj nums (u/re-pos #"\d+" line))
                (concat syms (map (fn [[j sym]] [[i j] sym]) (u/re-pos #"[^\.\d]" line)))])
             [[] []] (vec (u/read-file-list path identity))))


(defn in-nums [nums i j]
  (->> (subvec nums (u/dec0 i) (min (+ i 2) (count nums)))
       (map #(u/between % (fn [[y]] (<= (max 0 (- j 4)) y (min (+ j 4))))))
       (apply concat)
       (reduce (fn [res [y num]]
                 (if (<= (dec y) j (+ y (count num)))
                   (conj res (parse-long num))
                   res)) [])))

(defn part1 [path]
  (let [[nums syms] (read-schematics path)]
    (reduce (fn [result [[i j]]]
              (apply + result (in-nums nums i j)))
            0 syms)))

(defn part2 [path]
  (let [[nums syms] (read-schematics path)]
    (reduce (fn [result [[i j] sym]]
              (if (= sym "*")
                (let [out (in-nums nums i j)]
                  (if (= 2 (count out)) (+ result (apply * out)) result))
                result)) 0 syms)))

(comment
  (part1 "2023/3.in") ; ~7.5ms
  (part2 "2023/3.in") ; ~6.6ms
  )