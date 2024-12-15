(ns advent-of-code.2015.20
  (:require
   [clojure.core.reducers :as r]
   [clojure.set :as s]
   [advent-of-code.utils :as u]))

(def factors
  (fn [x]
    (->> (range (inc (quot x 2)) 0 -1)
         vec
         (r/filter #(= 0 (mod x %))))))

(defn gifts-at [x]
  (if (= 1 x)
    10
    (* 10 (+ x (r/fold + (factors x))))))

(defn gifts-at-50 [x]
  (if (= 1 x)
    11
    (* 11 (+ x (r/fold + (r/take 49 (factors x)))))))

(defn part1 []
  (->> (range (* 32 10000) 1000000 32)
       (pmap #(vector (gifts-at %) %))
       (drop-while #(< (first %) 34000000))
       first))

(defn part2 []
  (->> (range (* 32 10000) 1000000 32)
       (pmap #(vector (gifts-at-50 %) %))
       (drop-while #(< (first %) 34000000))
       first))

(comment

  (part1)
  (time (gifts-at 786240))

  (time (gifts-at 786240))
  (time (gifts-at-50 720720))

  (time (->> (range 600000 1000000 2)
             (pmap #(vector (gifts-at %) %))
             (drop-while #(< (first %) 34000000))
             first)))

