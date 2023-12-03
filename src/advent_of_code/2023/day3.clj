(ns advent-of-code.2023.day3
  (:require [advent-of-code.utils :as u]))

(defn read-schematics [path]
  (loop [[line & t] (u/read-file-list path identity)
         i          0
         nums       {}
         syms       {}]
    (if (nil? line)
      [nums syms]
      (let [n (->> (u/re-pos #"\d+" line)
                   (map (fn [[j num]] [[i j (count num)] (read-string num)])))
            s (->> (u/re-pos #"[^\.\d]" line)
                   (map (fn [[j sym]] [[i j] sym])))]
        (recur t (inc i) (into nums n) (into syms s))))))

(defn in-nums [nums i j]
  (keep (fn [[[x y l] num]] (when (and (<= (dec i) x (inc i)) (<= (dec y) j (+ y l))) num)) nums))

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
  (time (part1 "2023/3.in")) 
  (time (part2 "2023/3.in"))
  )