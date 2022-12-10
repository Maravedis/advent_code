(ns advent-of-code.day9
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(defn read-direction [line]
  (update (str/split line #" ") 1 read-string))

(defn inc-node [[hx hy] [tx ty :as t]]
  (match [(- hx tx) (- hy ty)]
    [2 0] [(+ 1 tx) ty]
    [0 2] [tx (+ 1 ty)]
    [-2 0] [(- tx 1) ty]
    [0 -2] [tx (- ty 1)]
    [2 1] [(+ 1 tx) (+ 1 ty)]
    [1 2] [(+ 1 tx) (+ 1 ty)]
    [1 2] [(+ 1 tx) (+ 1 ty)]
    [2 2] [(+ 1 tx) (+ 1 ty)]
    [-2 -1] [(- tx 1) (- ty 1)]
    [-1 -2] [(- tx 1) (- ty 1)]
    [-2 -2] [(- tx 1) (- ty 1)]
    [-1 2] [(- tx 1) (+ ty 1)]
    [-2 1] [(- tx 1) (+ ty 1)]
    [-2 2] [(- tx 1) (+ ty 1)]
    [1 -2] [(+ tx 1) (- ty 1)]
    [2 -1] [(+ tx 1) (- ty 1)]
    [2 -2] [(+ tx 1) (- ty 1)]
    :else t))

(defn count-tail-positions [resource tail-size]
  (loop [[[dir i] & r] (u/read-file-list resource read-direction)
         [hx hy :as h]       [0 0]
         knots         (vec (repeat tail-size [0 0]))
         acc           #{[0 0]}]
    (if (nil? dir)
      (count acc)
      (let [h1 (case dir
                 "U" [hx (+ 1 hy)]
                 "R" [(+ 1 hx) hy]
                 "L" [(- hx 1) hy]
                 "D" [hx (- hy 1)])
            k1 (->> (reduce (fn [[previous :as r] next]
                              (cons (inc-node previous next) r))
                            [h1]
                            knots)
                    drop-last
                    reverse)]
        (if (= i 1)
          (recur r h1 k1 (conj acc (last k1)))
          (recur (cons [dir (- i 1)] r) h1 k1 (conj acc (last k1))))))))

(cons [[0 0]] [1 0])
(comment 
  (count-tail-positions "day9" 1)
  (count-tail-positions "day9" 9))
