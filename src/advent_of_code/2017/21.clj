(ns advent-of-code.2017.21
  (:require [advent-of-code.utils :as u]
            [clojure.string :refer [split]]
            [advent-of-code.vectors :as v]))

(def path (u/get-input 2017 21))
(def tpath (u/test-path 2017 21))

(def start-state (->> [".#." "..#" "###"]
                      (mapv vec)))

(defn rotate [matrix]
  (->> (u/transpose matrix)
       (mapv (comp vec rseq))))

(defn flip-vertical [state]
  (vec (rseq state)))

(defn flip-horizontal [state]
  (->> (u/transpose state)
       flip-vertical
       u/transpose))

(defn parse-rule [line]
  (let [[in out] (split line #" => ")
        in-0     (mapv vec (split in #"/"))
        in-90    (rotate in-0)
        in-180   (rotate in-90)
        in-270   (rotate in-180)
        out      (mapv vec (split out #"/"))]
    [[in-0 out]
     [in-90 out]
     [in-180 out]
     [in-270 out]
     [(flip-horizontal in-0) out]
     [(flip-vertical in-0) out]
     [(flip-horizontal in-90) out]
     [(flip-vertical in-90) out]
     [(flip-horizontal in-180) out]
     [(flip-vertical in-180) out]
     [(flip-horizontal in-270) out]
     [(flip-vertical in-270) out]]))

(defn split-2 [state]
  (->> (partition 2 state)
       (map (fn [[r1 r2]]
              (for [i (range 0 (count state) 2)]
                [[(r1 i) (r1 (inc i))]
                 [(r2 i) (r2 (inc i))]])))))

(defn split-3 [state]
  (->> (partition 3 state)
       (map (fn [[r1 r2 r3]]
              (for [i (range 0 (count state) 3)]
                [[(r1 i) (r1 (inc i)) (r1 (+ 2 i))]
                 [(r2 i) (r2 (inc i)) (r2 (+ 2 i))]
                 [(r3 i) (r3 (inc i)) (r3 (+ 2 i))]])))))

(defn next-state [rules state]
  (->> (if (even? (count state)) (split-2 state) (split-3 state))
       (map #(map rules %))
       (map #(apply map v/catvec %))
       (apply concat)))

(defn run [path n]
  (let [rules (->> (u/read-file-list path parse-rule)
                   (apply concat)
                   (into {}))]
    (->> (iterate (partial next-state rules) start-state)
         (drop n)
         first
         (reduce (fn [acc row] (+ acc (u/count-when #(= \# %) row))) 0))))

(comment
  (run tpath 2)
  (run path 5)
  (run path 18))
