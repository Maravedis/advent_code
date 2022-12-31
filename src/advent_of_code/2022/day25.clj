(ns advent-of-code.2022.day25
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(defn p5 [i]
  (reduce * 1 (repeat i 5)))

(defn ->dec [snafu]
  (reduce-kv (fn [acc idx chr]
            (+ acc (case chr
                     \0 0
                     \1 (p5 idx)
                     \2 (* 2 (p5 idx))
                     \- (* -1 (p5 idx))
                     \= (* -2 (p5 idx))))) 0 (vec (reverse snafu))))

(defn ->snafu [dec]
    (loop [[h & t] (reverse (Long/toString dec 5))
           rem false
           acc (list)]
      (if (nil? h)
        (str/join acc)
        (case h
          \0 (recur t false (cons (if rem \1 \0) acc))
          \1 (recur t false (cons (if rem \2 \1) acc))
          \2 (recur t rem (cons (if rem \= \2) acc))
          \3 (recur t true (cons (if rem \- \=) acc))
          \4 (recur t true (cons (if rem \0 \-) acc))))))

(defn part1 [path]
  (->> (u/read-file-list path ->dec)
       u/sum
       ->snafu))

(comment
  (part1 "2022/day25")
  )