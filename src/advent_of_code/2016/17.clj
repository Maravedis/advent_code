(ns advent-of-code.2016.17
  (:require [advent-of-code.points :as p]
            [advent-of-code.utils :as u]
            [clj-commons.digest :refer [md5]]))

(def path (u/get-input 2016 17))
(def tpath (u/test-path 2016 17))

(def grid (set (for [x (range 4)
                     y (range 4)] [x y])))

(defn neighbors [passcode curr p]
  (let [[up down left right] (md5 (str passcode p))]
    (cond-> []
      (#{\b \c \d \e \f} up) (conj [(p/move curr p/N) (str p "U")])
      (#{\b \c \d \e \f} down) (conj [(p/move curr p/S) (str p "D")])
      (#{\b \c \d \e \f} left) (conj [(p/move curr p/W) (str p "L")])
      (#{\b \c \d \e \f} right) (conj [(p/move curr p/E) (str p "R")]))))

(defn part1 [path]
  (let [passcode (u/read-file-line path identity)
        end      [3 3]]
    (loop [open {[0 0] [""]}]
      (if-let [result (open end)]
        (first result)
        (recur (->> (mapcat (fn [[curr paths]]
                              (mapcat (partial neighbors passcode curr) paths)) open)
                    (filter #(grid (first %)))
                    (reduce (fn [acc [p h]] (update acc p conj h)) {})))))))

(defn part2 [path]
  (let [passcode (u/read-file-line path identity)
        end      [3 3]]
    (loop [open     {[0 0] [""]}
           max-path 0
           i        0]
      (if (empty? open)
        max-path
        (recur (->> (remove (fn [[point]] (= point end)) open)
                    (mapcat (fn [[curr paths]]
                              (mapcat (partial neighbors passcode curr) paths)))
                    (filter #(grid (first %)))
                    (reduce (fn [acc [p h]] (update acc p conj h)) {}))
               (apply max max-path (map count (open end)))
               (inc i))))))

(comment
  (part1 path)
  (part2 path))
  ;

