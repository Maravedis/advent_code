(ns advent-of-code.2025.01
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2025 1))
(def tpath (u/test-path 2025 1))

(defn parse-line [line]
  (let [[_ r n] (re-matches #"([LR])(\d+)" line)]
    [(condp = r "L" - "R" +) (parse-long n)]))

(defn part1 [[acc pos] [op n]]
  (let [new-pos (mod (op pos n) 100)]
    [(cond-> acc (= 0 new-pos) inc) new-pos]))

(defn part2 [[acc pos] [op n]]
  (let [res  (op pos n)
        by-0 (cond
               (and (= 0 pos) (< -100 res 100)) 0
               (= 0 res) 1
               (pos? res) (quot res 100)
               (neg? res) (cond-> (quot (* -1 res) 100) (not= 0 pos) inc))]
    [(+ acc by-0) (mod res 100)]))

(defn solve [path f]
  (->> (u/read-file-list path parse-line)
       (reduce f [0 50])
       first))

(comment
  (solve path part1)
  (solve path part2)
  ;;
  )