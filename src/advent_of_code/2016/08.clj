(ns advent-of-code.2016.08
  (:require [advent-of-code.utils :as u]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(def path (u/get-input 2016 8))
(def tpath (u/test-path 2016 8))

(defn parse-line [line]
  (let [values (str/split line #" |x|=")]
    (match values
      ["rect" col row] [0 (parse-long col) (parse-long row)]
      ["rotate" "column" _ _ col _ factor] [1 (parse-long col) (parse-long factor)]
      ["rotate" "row" _  row _ factor] [2 (parse-long row) (parse-long factor)])))

(defn run-program [instructions]
  (loop [[[ins a b] & t] instructions
         grid            #{}]
    (if (nil? ins)
      grid
      (case ins
        0 (recur t (reduce conj grid (for [row (range b)
                                           col (range a)]
                                       [row col])))
        1 (let [current (filter (fn [[_ c]] (= c a)) grid)
                rotated (map (fn [[r c]] [(mod (+ r b) 6) c]) current)]
            (recur t (reduce conj (reduce disj grid current) rotated)))
        2 (let [current (filter (fn [[r _]] (= r a)) grid)
                rotated (map (fn [[r c]] [r (mod (+ c b) 50)]) current)]
            (recur t (reduce conj (reduce disj grid current) rotated)))))))

(defn print-screen [grid]
  (doseq [r (range 6)
          c (range 50)]
    (print (if (grid [r c]) \# \.))
    (when (= c 49) (println))))

(defn both-parts [path]
  (let [pixels (->> (u/read-file-list path parse-line)
                    run-program)]
    (print-screen pixels)
    (count pixels)))

(comment

  (both-parts path)
  ; ZJHRKCPLYJ
  )
