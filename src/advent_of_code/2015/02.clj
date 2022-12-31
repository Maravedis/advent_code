(ns advent-of-code.2015.02
  (:require [advent-of-code.utils :as u]))


(defn part1 [path] 
  (reduce (fn [acc [l w h]] 
            (let [a (* l w)
                  b (* w h)
                  c (* h l)] 
              (+ acc (* 2 (+ a b c)) (min a b c))))
          0
          (u/read-file-list path u/nums)))

(defn part2 [path] 
  (reduce (fn [acc [l w h]]
            (let [a (* 2 (+ l w))
                  b (* 2 (+ w h))
                  c (* 2 (+ h l))]
              (+ acc (min a b c) (* l w h))))
          0
          (u/read-file-list path u/nums)))

(comment
  (u/get-input 2015 2)

  (part1 "2015/2.in")
  (part2 "2015/2.in"))