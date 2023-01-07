(ns advent-of-code.2015.06
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(defn read-instruction [line]
  (let [[_ s]     (str/split line #" ")
        [a b c d] (u/nums line)]
    [(case s "on" 0 "off" 1 2) [a b] [c d]]))


(defn part1 [path]
  (loop [[[s [x y] [x' y'] :as h] & r] (u/read-file-list path read-instruction)
         lights                        {}
         ] 
    (if (nil? h)
      (u/count-if (fn [[_ v]] v) lights)
      (let [f (case s
                0 (fn [_] true)
                1 (fn [_] false)
                2 (fn [light] (not light)))]
        (recur r 
               (reduce (fn [acc coor] (update acc coor f)) lights (for [a (range x (inc x'))
                                                                        b (range y (inc y'))] [a b])))))))

(defn part2 [path]
  (loop [[[s [x y] [x' y'] :as h] & r] (u/read-file-list path read-instruction)
         lights                        {}]
    (if (nil? h)
      (reduce (fn [acc [_ b]] (+ acc b)) 0 lights)
      (let [f (case s
                0 (fn [light] (inc (or light 0)))
                1 (fn [light] (max (dec (or light 0)) 0))
                2 (fn [light] (+ 2 (or light 0))))]
        (recur r
               (reduce (fn [acc coor] (update acc coor f)) lights (for [a (range x (inc x'))
                                                                        b (range y (inc y'))] [a b])))))))

(comment
  (def path (u/get-input 2015 6))
  (part1 path)
  (part2 path)
  )