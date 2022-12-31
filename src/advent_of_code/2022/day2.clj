(ns advent-of-code.2022.day2
  (:require [advent-of-code.utils :refer [read-file-list]]
            [clojure.string :as str]))

(def score {:rock 1 :paper 2 :scissors 3})

(defn read-guide-line [fn line]
  (->> (str/split line #" ")
       (map fn)))

;; PART 1

(def ->rps {"A" :rock
            "B" :paper
            "C" :scissors
            "X" :rock
            "Y" :paper
            "Z" :scissors})

(defn calc-score [left right]
  (let [r (get score right)
        l (get score left)]
    (cond
      (= r l) (+ 3 r)
      (= 4 (+ r l)) (cond-> r (= r 1) (+ 6))
      (> r l) (+ 6 r)
      :else r)))

;; PART 2

(def ->rps2 {"A" :rock
             "B" :paper
             "C" :scissors
             "X" :loss
             "Y" :draw
             "Z" :win})

(defn calc-score2 [left outcome]
  (let [l (get score left)]
    (case outcome
      :draw (+ l 3)
      :loss (if (= l 1) 3 (- l 1))
      :win (+ 6 (if (= l 3) 1 (+ l 1))))))

;; Global

(defn guide-score [resource parse-line f-calc-score]
  (->> (read-file-list resource (partial read-guide-line parse-line))
       (map #(apply f-calc-score %))
       (reduce +)))

(comment
  (guide-score "2022/day2" ->rps calc-score)
  (guide-score "2022/day2" ->rps2 calc-score2))