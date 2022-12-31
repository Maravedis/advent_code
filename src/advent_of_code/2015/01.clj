(ns advent-of-code.2015.01
  (:require [advent-of-code.utils :as u]
            [clojure.java.io :as io]))

(defn part1 [path] 
  (reduce (fn [floor h]
            (case h
              \( (inc floor)
              \) (dec floor))) 
          0 (slurp (io/resource path))))

(defn part2 [path] 
  (reduce-kv (fn [floor idx h]
               (if (= -1 floor)
                 (reduced idx)
                 (case h
                   \( (inc floor)
                   \) (dec floor))))
             0 (vec (slurp (io/resource path)))))

(comment
  (u/get-input 2015 1)
  
  (part1 "2015/1.in")
  (part2 "2015/1.in")
  )