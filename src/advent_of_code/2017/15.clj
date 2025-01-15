(ns advent-of-code.2017.15
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2017 15))
(def tpath (u/test-path 2017 15))

(defn part1 [path]
  (let [[[A] [B]] (->> (u/read-file-list path u/nums))]
    (->> (reduce (fn [[acc a b] _]
                   (let [a (rem (* a 16807) 0x7FFFFFFF)
                         b (rem (* b 48271) 0x7FFFFFFF)]
                     [(cond-> acc (= (bit-and a 0xFFFF) (bit-and b 0xFFFF)) inc) a b])) [0 A B] (range 40e6))
         first)))

(defn part2 [path]
  (let [[[A] [B]] (->> (u/read-file-list path u/nums))]
    (->> (reduce (fn [[acc a b] _]
                   (let [[a] (drop-while #(not= 0 (mod % 4)) (drop 1 (iterate #(rem (* % 16807) 0x7FFFFFFF) a)))
                         [b] (drop-while #(not= 0 (mod % 8)) (drop 1 (iterate #(rem (* % 48271) 0x7FFFFFFF) b)))]
                     [(cond-> acc (= (bit-and a 0xFFFF) (bit-and b 0xFFFF)) inc) a b])) [0 A B] (range 5e6))
         first)))

(comment

  (part1 tpath)
  (part1 path)
  (part2 tpath)
  (part2 path)


  ;
  )
