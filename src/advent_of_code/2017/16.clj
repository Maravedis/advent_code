(ns advent-of-code.2017.16
  (:require [advent-of-code.utils :as u]
            [advent-of-code.vectors :as v]
            [clojure.core.match :refer [match]]
            [clojure.string :refer [join split]]))

(def path (u/get-input 2017 16))
(def tpath (u/test-path 2017 16))

(defn parse-instruction [line]
  (let [m (re-matcher #"([sxp])(\w+)(?:/(\w+))?" line)]
    (match (re-find m)
      [_ "s" v _] [0 (parse-long v)]
      [_ "x" a b] [1 (parse-long a) (parse-long b)]
      [_ "p" a b] [2 (first a) (first b)])))

(defn run [programs instructions]
  (let [n (count programs)]
    (loop [programs             programs
           [[op a b :as h] & t] instructions]
      (if (nil? h)
        programs
        (recur (case op
                 0 (into (subvec programs (- n a)) (subvec programs 0 (- n a)))
                 1 (v/swap-at programs a b)
                 2 (v/swap-at programs (v/index-of programs a) (v/index-of programs b)))
               t)))))

(defn part1 [path length]
  (->> (u/read-file-line path #(->> (split % #",") (map parse-instruction)))
       (run (vec (take length "abcdefghijklmnopqrstuvwxyz")))
       join))

(def limit 1000000000)

(defn part2 [path]
  (let [instructions (u/read-file-line path #(->> (split % #",") (map parse-instruction)))]
    (loop [programs (vec "abcdefghijklmnop")
           seen     (transient #{})
           step     0
           start    -1]
      (cond (= step limit) (join programs)
            (and (seen programs) (= -1 start)) (recur (run programs instructions) (transient #{programs}) (inc step) step)
            (seen programs) (recur programs
                                   (transient #{})
                                   (loop [i start] (if (< limit (+ i (- step start))) i (recur (+ i (- step start)))))
                                   nil)
            :else (recur (run programs instructions) (conj! seen programs) (inc step) start)))))

(comment

  (part1 tpath 5)
  (part1 path 16)
  (part2 path)

  ;
  )
