(ns advent-of-code.2023.day1
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(defn ->number [line]
  (let [arr (->> line
                 seq
                 (map #(Character/digit % 10))
                 (remove neg?))]
    (+ (* 10 (first arr)) (last arr))))

(def spelled-map {"one"   1
                  "two"   2
                  "three" 3
                  "four"  4
                  "five"  5
                  "six"   6
                  "seven" 7
                  "eight" 8
                  "nine"  9})

(def spelled (keys spelled-map))

(defn read-number [line]
  (loop [[h & t :as l] (seq line)
         res           []]
    (if (nil? h)
      (+ (* 10 (first res)) (last res))
      (let [dig (Character/digit h 10)
            sp  (reduce (fn [_ k]
                          (when (str/starts-with? (apply str l) k)
                            (reduced (spelled-map k)))) nil spelled)]
        (cond (> dig 0) (recur t (conj res dig))
              sp (recur t (conj res sp))
              :else (recur t res))))))

(defn part1 [path]
  (->> (u/read-file-list path ->number)
       u/sum))

(defn part2 [path]
  (->> (u/read-file-list path read-number)
       u/sum))

(comment
  (u/get-input 2023 1)
  (part1 "2023/1.in")
  (part2 "2023/1.in")
  )