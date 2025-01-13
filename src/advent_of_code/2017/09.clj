(ns advent-of-code.2017.09
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2017 9))
(def tpath (u/test-path 2017 9))

(defn part1 [path]
  (loop [[h & t]  (u/read-file-line path identity)
         acc      0
         depth    1
         garbage? false]
    (cond (nil? h) acc
          (and (not garbage?) (= h \{)) (recur t (+ acc depth) (inc depth) false)
          (and (not garbage?) (= h \})) (recur t acc (dec depth) false)
          (and (not garbage?) (= h \<)) (recur t acc depth true)
          (and garbage? (= h \!)) (recur (drop 1 t) acc depth true)
          (and garbage? (= h \>)) (recur t acc depth false)
          :else (recur t acc depth garbage?))))

(defn part2 [path]
  (loop [[h & t]  (u/read-file-line path identity)
         acc      0
         garbage? false]
    (cond (nil? h) acc
          (and (not garbage?) (= h \<)) (recur t acc true)
          (and garbage? (= h \!)) (recur (drop 1 t) acc true)
          (and garbage? (= h \>)) (recur t acc false)
          garbage? (recur t (inc acc) true)
          :else (recur t acc garbage?))))

(comment

  (part1 path)
  (part2 path))
