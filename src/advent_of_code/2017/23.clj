(ns advent-of-code.2017.23
  (:require [advent-of-code.utils :as u]
            [clojure.core.match :refer [match]]))

(def path (u/get-input 2017 23))

(defn parse-arg [x]
  (if-let [v (parse-long x)]
    v
    (keyword x)))

(defn parse-instruction [line]
  (match (vec (re-seq #"[a-z0-9-]+" line))
    ["set" x y] [0 (keyword x) (parse-arg y)]
    ["add" x y] [1 (keyword x) (parse-arg y)]
    ["sub" x y] [2 (keyword x) (parse-arg y)]
    ["mul" x y] [3 (keyword x) (parse-arg y)]
    ["jnz" x y] [4 (parse-arg x) (parse-arg y)]))

(defn part1 [path]
  (let [instructions (vec (u/read-file-list path parse-instruction))]
    (loop [ip        0
           registers {:a 0}
           acc       0]
      (if-let [[op x y] (get instructions ip)]
        (case op
          0 (recur (inc ip) (assoc registers x (if (int? y) y (registers y))) acc)
          1 (recur (inc ip) (update registers x (fnil + 0) (if (int? y) y (registers y 0))) acc)
          2 (recur (inc ip) (update registers x (fnil - 0) (if (int? y) y (registers y 0))) acc)
          3 (recur (inc ip) (update registers x (fnil * 0) (if (int? y) y (registers y 0))) (inc acc))
          4 (recur (if (not= 0 (if (int? x) x (registers x 0)))
                     (+ ip (if (int? y) y (registers y 0)))
                     (inc ip))
                   registers
                   acc))
        acc))))

; values extracted from the input Dont @me
(defn part2 []
  (loop [h 0
         b 105700
         c 122700]
    (if (> b c)
      h
      (recur (if (some #(= 0 (mod b %)) (range 2 b))
               (inc h)
               h)
             (+ b 17)
             c))))

(comment
  (part1 path)
  (part2))
