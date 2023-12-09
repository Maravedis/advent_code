(ns advent-of-code.2015.12
  (:require [advent-of-code.utils :as u]
            [clojure.java.io :as io]
            [cheshire.core :refer [parse-string]]))

(defn red-sum [obj]
  (cond (number? obj)     obj
        (map? obj)        (if (some #(= "red" (second %)) obj) 0 (reduce + (map red-sum (vals obj))))
        (sequential? obj) (reduce + (map red-sum obj))
        :else             0))

(comment
  (def path (u/get-input 2015 12))

  ;part1
  (reduce + (u/nums (slurp (io/resource path))))
  ;part2
  (red-sum (parse-string (slurp (io/resource path)) true))
  )