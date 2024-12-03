(ns advent-of-code.2015.18
  (:require [advent-of-code.utils :as u]))

(defn parse-line [line]
  (mapv #(= \# %) line))

(defn neighbors [[r c] h w]
  (for [i     (range -1 2)
        j     (range -1 2)
        :let  [x (+ r i)
               y (+ c j)]
        :when (and (not= 0 i j) (< -1 x h) (< -1 y w))]
    [x y]))

(defn next-step-fn-corners [h w part2?]
  (let [h1 (dec h)
        w1 (dec w)]
    (memoize (fn [grid]
               (vec (for [r    (range 0 h)]
                      (vec (for [c    (range 0 w)
                                 :let [n-on (u/count-when #(get-in grid %) (neighbors [r c] h w))]]
                             (or (and part2?
                                      (or (= 0 r c) (and (= h1 r) (= w1 c))
                                          (and (= r 0) (= w1 c)) (and (= h1 r) (= c 0))))
                                 (if (get-in grid [r c])
                                   (or (= 2 n-on)
                                       (= 3 n-on))
                                   (= 3 n-on)))))))))))

(defn run [path limit part2?]
  (let [input     (vec (u/read-file-list path parse-line))
        h         (count input)
        w         (count (first input))
        next-step (next-step-fn-corners h w part2?)]
    (loop [grid input
           i    0]
      (if (>= i limit)
        (apply + (map #(u/count-when true? %) grid))
        ;; grid
        (recur
         (next-step grid)
         (inc i))))))

(comment

  (def path (u/get-input 2015 18))

  (run path 100 false)
  (run path 100 true))
