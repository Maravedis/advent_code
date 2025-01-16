(ns advent-of-code.2017.18
  (:require [advent-of-code.utils :as u]
            [clojure.core.match :refer [match]]))

(def path (u/get-input 2017 18))
(def tpath (u/test-path 2017 18))

(defn parse-arg [x]
  (if-let [v (parse-long x)]
    v
    (keyword x)))

(defn parse-instruction [line]
  (match (vec (re-seq #"[a-z0-9-]+" line))
    ["snd" v] [0 (keyword v)]
    ["set" x y] [1 (keyword x) (parse-arg y)]
    ["add" x y] [2 (keyword x) (parse-arg y)]
    ["mul" x y] [3 (keyword x) (parse-arg y)]
    ["mod" x y] [4 (keyword x) (parse-arg y)]
    ["rcv" x] [5 (keyword x)]
    ["jgz" x y] [6 (parse-arg x) (parse-arg y)]))

(defn part1 [path]
  (let [instructions (vec (u/read-file-list path parse-instruction))]
    (loop [ip        0
           registers {}
           sound     nil]
      (if-let [[op x y] (get instructions ip)]
        (case op
          0 (recur (inc ip) registers (registers x 0))
          1 (recur (inc ip) (assoc registers x (if (int? y) y (registers y 0))) sound)
          2 (recur (inc ip) (update registers x (fnil + 0) (if (int? y) y (registers y 0))) sound)
          3 (recur (inc ip) (update registers x (fnil * 0) (if (int? y) y (registers y 0))) sound)
          4 (recur (inc ip) (update registers x (fnil mod 0) (if (int? y) y (registers y 0))) sound)
          5 (if (not= 0 (registers x)) sound (recur (inc ip) registers sound))
          6 (recur (if (< 0 (if (int? x) x (registers x 0)))
                     (+ ip (if (int? y) y (registers y 0)))
                     (inc ip))
                   registers
                   sound))
        :error))))

(defn run [instructions ip registers input]
  (loop [ip        ip
         registers registers
         input     input
         output    []]
    (if-let [[op x y] (get instructions ip)]
      (case op
        0 (recur (inc ip) registers input (conj output (if (int? x) x (registers x 0))))
        1 (recur (inc ip) (assoc registers x (if (int? y) y (registers y 0))) input output)
        2 (recur (inc ip) (update registers x (fnil + 0) (if (int? y) y (registers y 0))) input output)
        3 (recur (inc ip) (update registers x (fnil * 0) (if (int? y) y (registers y 0))) input output)
        4 (recur (inc ip) (update registers x (fnil mod 0) (if (int? y) y (registers y 0))) input output)
        5 (if (not-empty input)
            (recur (inc ip) (assoc registers x (first input)) (rest input) output)
            [ip registers output])
        6 (recur (if (< 0 (if (int? x) x (registers x 0)))
                   (+ ip (if (int? y) y (registers y 0)))
                   (inc ip))
                 registers
                 input
                 output))
      :error)))

(defn part2 [path]
  (let [instructions (vec (u/read-file-list path parse-instruction))]
    (loop [[ip0 r0 o0 :as p0] (run instructions 0 {:p 0} [])
           [ip1 r1 o1 :as p1] [0 {:p 1} []]
           acc                0
           cycle              1]
      (cond (and (empty? o0) (empty? o1)) acc
            (= 0 cycle) (recur (run instructions ip0 r0 o1) p1 (+ acc (count o1)) 1)
            (= 1 cycle) (recur p0 (run instructions ip1 r1 o0) acc 0)))))

(comment
  (part1 tpath)
  (part1 path)
  (part2 tpath)
  (part2 path))
