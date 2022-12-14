(ns advent-of-code.day14
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn read-rocks [line]
  (->> (str/split line #" -> ")
       (map #(str/split % #","))
       (map (partial map read-string))
       (partition 2 1)
       (reduce (fn [acc [[x y] [x' y']]]
                 (let [is (range (min x x') (inc (max x x')))
                       js (range (min y y') (inc (max y y')))]
                   (reduce (fn [acc1 i]
                             (update acc1 i set/union (set js))) acc is)))
               {})))

(defn get-under [rocks x y]
  (some->> (get rocks x)
           (remove #(< % y))
           seq
           (apply min)))


(defn sand-falls [rocks floor]
  (loop [[x y] [500 0]
         cycle 0
         stops rocks]
    (if (contains? (get stops 500) 0)
      cycle
      (if-let [h (or (get-under stops x y) floor)]
        (let [can-left  (not (or (contains? (get stops (dec x)) h)
                                 (= h floor)))
              can-right (not (or (contains? (get stops (inc x)) h)
                                 (= h floor)))]
          (cond can-left (recur [(dec x) h] cycle stops)
                can-right (recur [(inc x) h] cycle stops)
                :else (recur [500 0] (inc cycle) (update stops x set/union #{(dec h)}))))
        cycle))))

(defn run [path with-floor?]
  (let [rocks (->> (u/read-file-list path read-rocks)
                   (reduce (fn [acc line]
                             (reduce (fn [acc1 [k v]]
                                       (update acc1 k set/union v)) acc line))))
        floor (when with-floor? (+ 2 (apply max (apply concat (vals rocks)))))]
    (sand-falls rocks floor)))

  (comment
    (run "day14" false)
    (run "day14" true)
    )