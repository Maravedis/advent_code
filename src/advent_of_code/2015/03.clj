(ns advent-of-code.2015.03
  (:require [advent-of-code.utils :as u]
            [clojure.java.io :as io]))

(defn update-loc [[x y] dir]
  (case dir
    \^ [(inc x) y]
    \> [x (inc y)]
    \v [(dec x) y]
    \< [x (dec y)]))

(defn part1 [path]
  (loop [presents      #{}
         loc [0 0]
         [h & r]       (slurp (io/resource path))]
    (if (nil? h)
      (count presents) 
      (recur (conj presents loc) (update-loc loc h) r))))

(defn part2 [path]
  (loop [presents      #{[0 0]}
         loc           [0 0]
         loc2          [0 0]
         [[idx h] & r] (map-indexed vector (slurp (io/resource path)))]
    (if (nil? h)
      (count presents)
      (if (= 0 (mod idx 2))
        (recur (conj presents (update-loc loc h)) (update-loc loc h) loc2 r)
        (recur (conj presents (update-loc loc2 h)) loc (update-loc loc2 h)  r)))))

(comment
  (u/get-input 2015 3)
  (part1 "2015/3.in")
  (part2 "2015/3.in"))