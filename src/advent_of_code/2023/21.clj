(ns advent-of-code.2023.21
  (:require [advent-of-code.utils :as u]
            [clojure.core.reducers :as r]
            [clojure.math :refer [pow]]))

(defn find-start [input]
  (first (keep-indexed (fn [r row]
                         (when-let [c (first (keep-indexed (fn [c char] (when (= char \S) c)) row))]
                           [r c])) input)))

(defn mk-neighbors-fn [input]
  (let [h (count input)]
    (fn [[r c] keepers]
      (->> [[-1 0] [0 -1] [1 0] [0 1]]
           (r/map (fn [[x y]] [(+ x r) (+ y c)]))
           (r/filter (fn [[x y]] (and (< -1 x h) (< -1 y h)
                                      (not (keepers [x y]))
                                      (not= \# (get-in input [x y])))))))))

(defn flood [input start limit]
  (if (< limit 0) 0
      (let [neighbors-fn (mk-neighbors-fn input)
            discern-fn   (if (odd? limit) odd? even?)]
        (loop [currs   #{start}
               steps   0
               keepers (transient #{})]
          (if (= limit steps)
            (+ (count keepers) (count currs))
            (recur (set (r/foldcat (r/mapcat #(neighbors-fn % keepers) (vec currs))))
                   (inc steps)
                   (if (discern-fn steps)
                     (reduce #(conj! %1 %2) keepers currs)
                     keepers)))))))

(defn part1 [path]
  (let [input        (vec (u/read-file-list path vec))
        start        (find-start input)
        input        (assoc-in input start \.)] 
   (flood input start 64)))

; Theorically could have a "peak" going further, but I don't fucking care. This assumes that (limit - (size / 2)) % size == 0. 
; Also assumes the input is the shape of the input and not the fucking example given. I hate everything.
(defn part2 [path limit]
  (let [input          (vec (u/read-file-list path vec))
        start          (find-start input)
        h              (count input)
        input          (assoc-in input start \.)
        n              (quot (- limit (quot h 2)) h)
        center-squares (bigint (if (even? n) (pow (dec n) 2) (pow n 2)))
        adj-squares    (bigint (if (odd? n) (pow (dec n) 2) (pow n 2)))
        center-square  (flood input start (if (odd? n) (* 2 h) (inc (* 2 h))))
        adj-square     (flood input start (if (even? n) (* 2 h) (inc (* 2 h))))
        outside-steps  (+ (rem limit h) (quot h 2))
        border-outside (quot limit h)
        border-inside  (dec border-outside)]
    (+ (* center-square center-squares)
       (* adj-square adj-squares)
               ;peaks
       (flood input [0 (quot h 2)] outside-steps)
       (flood input [(quot h 2) 0] outside-steps)
       (flood input [(dec h) (quot h 2)] outside-steps)
       (flood input [(quot h 2) (dec h)] outside-steps)
               ; upper right diagonals
       (* border-outside (flood input [(dec h) 0] (dec (rem limit h))))
       (* border-inside  (flood input [(dec h) 0] (dec (+ h (rem limit h)))))
               ; upper left diagonals
       (* border-outside (flood input [(dec h) (dec h)] (dec (rem limit h))))
       (* border-inside  (flood input [(dec h) (dec h)] (dec (+ h (rem limit h)))))
               ; lower right
       (* border-outside (flood input [0 0] (dec (rem limit h))))
       (* border-inside  (flood input [0 0] (dec (+ h (rem limit h)))))
               ; lower left
       (* border-outside (flood input [0 (dec h)] (dec (rem limit h))))
       (* border-inside  (flood input [0 (dec h)] (dec (+ h (rem limit h))))))))

(comment 

  (def path (u/get-input 2023 21))
  (def tpath "2023/21_test.in")

  (time (part1 path))
  (time (part2 path 26501365))

  )