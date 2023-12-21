(ns advent-of-code.2023.21
  (:require [advent-of-code.utils :as u]
            [clojure.core.reducers :as r]))

(defn find-start [input]
  (first (keep-indexed (fn [r row]
                         (when-let [c (first (keep-indexed (fn [c char] (when (= char \S) c)) row))]
                           [r c])) input)))

(defn mk-neighbors-fn [input]
  (let [h (count input)
        w (count (first input))]
    (fn [[r c] keepers]
      (->> [[-1 0] [0 -1] [1 0] [0 1]]
           (r/map (fn [[x y]] [(+ x r) (+ y c)]))
           (r/filter (fn [[x y]] (and (< -1 x h) (< -1 y w)
                                      (not (keepers [x y]))
                                      (not= \# (get-in input [x y])))))))))

(defn part1 [path]
  (let [input (vec (u/read-file-list path vec))
        start (find-start input)
        input (assoc-in input start \.)
        neighbors-fn (mk-neighbors-fn input)]
    (loop [currs   #{start}
           steps   0
           keepers (transient #{})]
      (if (= 64 steps)
        (+ (count keepers) (count currs))
        (recur (set (r/foldcat (r/mapcat #(neighbors-fn % keepers) currs))) (inc steps) (if (even? steps) (reduce #(conj! %1 %2) keepers currs) keepers))))))

(defn part2 [path]
  (let [input        (vec (u/read-file-list path vec))
        start        (find-start input)
        input        (assoc-in input start \.)
        neighbors-fn (mk-neighbors-fn input)]
    (loop [currs   #{start}
           steps   0
           keepers (transient #{})]
      (if (= 64 steps)
        (+ (count keepers) (count currs))
        (recur (set (r/foldcat (r/mapcat #(neighbors-fn % keepers) currs))) (inc steps) (if (even? steps) (reduce #(conj! %1 %2) keepers currs) keepers))))))


(comment
  (def path (u/get-input 2023 21))
  (def tpath "2023/21_test.in")

  (defn max-i [n] (+ n (* 4 (/ (* n (+ n 1)) 2))) )
  (max-i 1000)
  (part1 path)q
  )