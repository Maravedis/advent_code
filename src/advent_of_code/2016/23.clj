(ns advent-of-code.2016.23
  (:require [advent-of-code.utils :as u]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(def path (u/get-input 2016 23))
(def tpath (u/test-path 2016 23))

(defn parse-arg [a]
  (if-let [value (parse-long a)]
    value
    (keyword a)))

(defn parse-line [line]
  (match (str/split line #" ")
    ["cpy" x y] [0 (parse-arg x) (parse-arg y)]
    ["inc" x] [1 (parse-arg x)]
    ["dec" x] [2 (parse-arg x)]
    ["jnz" x y] [3 (parse-arg x) (parse-arg y)]
    ["tgl" x] [4 (parse-arg x)]))

(def registers {:a 0
                :b 0
                :c 0
                :d 0})

(defn detect-mul [ip registers program]
  (let [program (drop ip program)
        instrs  (vec (take 6 program))]
    (match instrs
      [[0 v y]
       [(:or 1 2) a]
       [(:or 1 2) b]
       [3 c -2]
       [(:or 1 2) d]
       [3 e -5]] (if (and (= y c) (#{a b} c) (= d e))
                   (let [acc (if (= a c) b a)
                         mul (if (int? v) v (registers v))]
                     (-> (update registers acc + (* mul (registers d)))
                         (assoc c 0 d 0)))
                   nil)
      :else nil)))

(defn run [path start-value]
  (let [instructions (vec (u/read-file-list path parse-line))]
    (loop [ip        0
           registers (assoc registers :a start-value)
           program   instructions]
      (if-let [[op x y] (get program ip)]
        (case op
          0 (if-let [regs (detect-mul ip registers program)]
              (recur (+ 5 ip) regs program)
              (recur (inc ip) (if (keyword? y) (assoc registers y (if (int? x) x (registers x))) registers) program))
          1 (recur (inc ip) (update registers x inc) program)
          2 (recur (inc ip) (update registers x dec) program)
          3 (recur (let [chk (if (int? x) x (registers x))
                         ptr (if (int? y) y (registers y))]
                     (if (not= 0 chk) (+ ip ptr) (inc ip)))
                   registers program)
          4 (recur (inc ip) registers (let [ptr (+ ip (registers x))]
                                        (if (< -1 ptr (count program))
                                          (cond-> program
                                            (= 3 (first (program ptr))) (assoc-in [ptr 0] 0)
                                            (= 1 (first (program ptr))) (assoc-in [ptr 0] 2)
                                            (#{2 4} (first (program ptr))) (assoc-in [ptr 0] 1)
                                            (= 0 (first (program ptr))) (assoc-in [ptr 0] 3))
                                          program))))
        (registers :a)))))

(comment

  (run path 7)
  (run path 12))
