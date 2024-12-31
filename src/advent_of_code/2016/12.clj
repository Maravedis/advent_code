(ns advent-of-code.2016.12
  (:require [advent-of-code.utils :as u]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(def path (u/get-input 2016 12))
(def tpath (u/test-path 2016 12))

(defn parse-line [line]
  (match (str/split line #" ")
    ["cpy" x y] [0 (if-let [value (parse-long x)] value (keyword x)) (keyword y)]
    ["inc" x] [1 (keyword x)]
    ["dec" x] [2 (keyword x)]
    ["jnz" x y] [3 (keyword x) (parse-long y)]))

(def registers {:a 0
                :b 0
                :c 0
                :d 0})
(defn run [path part2?]
  (let [instructions (vec (u/read-file-list path parse-line))]
    (loop [ip        0
           registers (cond-> registers
                       part2? (assoc :c 1))]
      (if-let [[op x y] (get instructions ip)]
        (case op
          0 (recur (inc ip) (assoc registers y (if (int? x) x (registers x))))
          1 (recur (inc ip) (update registers x inc))
          2 (recur (inc ip) (update registers x dec))
          3 (recur (if (= (registers x) 0) (inc ip) (+ ip y)) registers))
        (:a registers)))))

(comment

  (run path false)
  (run path true))
