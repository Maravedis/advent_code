(ns advent-of-code.2016.21
  (:require [advent-of-code.utils :as u]
            [advent-of-code.vectors :as v]
            [clojure.core.match :refer [match]]
            [clojure.set :refer [map-invert]]
            [clojure.string :refer [join split]]))

(def path (u/get-input 2016 21))

(defn parse-line [s]
  (match (split s #" ")
    ["swap" "position" a _ _ b] [0 (parse-long a) (parse-long b)]
    ["swap" "letter" a _ _ b] [1 (first a) (first b)]
    ["reverse" _ a _ b] [2 (parse-long a) (parse-long b)]
    ["rotate" "left" a _] [3 (parse-long a)]
    ["rotate" "right" a _] [4 (parse-long a)]
    ["rotate" "based" _ _ _ _ a] [5 (first a)]
    ["move" "position" a _ _ b] [6 (parse-long a) (parse-long b)]))

; new index from starting index for instruction "rotate based on letter"
(def rot {0 1
          1 3
          2 5
          3 7
          4 2
          5 4
          6 6
          7 0})
(def rot-i (map-invert rot))

(defn part1 [path]
  (loop [password             (vec "abcdefgh")
         [[op a b :as h] & t] (u/read-file-list path parse-line)]
    (if (nil? h)
      (join password)
      (case op
        0 (recur (v/swap-at password a b) t)
        1 (recur (v/swap-at password (v/index-of password a) (v/index-of password b)) t)
        2 (recur (into (into (subvec password 0 a) (vec (rseq (subvec password a (inc b))))) (subvec password (inc b))) t)
        3 (recur (v/rotate-left password a) t)
        4 (recur (v/rotate-right password a) t)
        5 (let [idx (v/index-of password a)
                mov (- idx (rot idx))] (recur (if (> mov 0) (v/rotate-left password mov) (v/rotate-right password (* -1 mov))) t))
        6 (recur (v/insert-at (v/evict-at password a) b (password a)) t)))))

(defn part2 [path]
  (loop [password             (vec "fbgdceah")
         [[op a b :as h] & t] (reverse (u/read-file-list path parse-line))]
    (if (nil? h)
      (join password)
      (case op
        0 (recur (v/swap-at password a b) t)
        1 (recur (v/swap-at password (v/index-of password a) (v/index-of password b)) t)
        2 (recur (into (into (subvec password 0 a) (vec (rseq (subvec password a (inc b))))) (subvec password (inc b))) t)
        3 (recur (v/rotate-right password a) t)
        4 (recur (v/rotate-left password a) t)
        5 (let [idx (v/index-of password a)
                mov (- idx (rot-i idx))] (recur (if (> mov 0) (v/rotate-left password mov) (v/rotate-right password (* -1 mov))) t))
        6 (recur (v/insert-at (v/evict-at password b) a (password b)) t)))))

(comment

  (part1 path)
  (part2 path))
