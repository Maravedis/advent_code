(ns advent-of-code.2017.17
  (:require [advent-of-code.utils :as u]
            [advent-of-code.vectors :as v]))

(def path (u/get-input 2017 17))
(def tpath (u/test-path 2017 17))

(defn part1 [path]
  (let [step (u/read-file-line path parse-long)]
    (loop [i   1
           pos 0
           l   [0]]
      (if (> i 2017)
        (get l (inc pos))
        (let [idx (+ 1 (mod (+ pos step) i))]
          (recur (inc i) idx (v/insert-at l idx i)))))))

(defn part2 [path]
  (let [step (u/read-file-line path parse-long)]
    (loop [i          1
           pos        0
           pos-post-0 1
           val-post-0 0]
      (if (> i 50000000)
        val-post-0
        (let [idx (+ 1 (mod (+ pos step) i))]
          (cond (= idx pos-post-0) (recur (inc i) idx idx i)
                (< idx pos-post-0) (recur (inc i) idx (inc pos-post-0) val-post-0)
                :else (recur (inc i) idx pos-post-0 val-post-0)))))))

(comment
  (part1 tpath)
  (part1 path)
  (part2 path))
