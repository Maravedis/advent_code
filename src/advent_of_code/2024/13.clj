(ns advent-of-code.2024.13
  (:require [advent-of-code.utils :as u]
            [clojure.core.reducers :as r]))

(defn find-prize [[xa ya] [xb yb] [xg yg]]
  (let [B (/ (- (* xa yg) (* xg ya)) (- (* xa yb) (* xb ya)))
        A (/ (- (* xb yg) (* xg yb)) (- (* xb ya) (* xa yb)))]
    (when (and (not (ratio? A)) (not (ratio? B)))
      (+ (* 3 A) B))))

(defn solve [path k]
  (->> (u/read-file-segmented-list path u/nums)
       (r/map #(update % 2 (fn [[x y]] [(+ x k) (+ y k)])))
       (r/fold + #((fnil + 0) (apply find-prize %2) %1))))

(comment
  (def path (u/get-input 2024 13))
  (def tpath (u/test-path 2024 13))

  (solve path 0)
  (solve path 10000000000000))