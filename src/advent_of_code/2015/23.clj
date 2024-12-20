(ns advent-of-code.2015.23
  (:require [advent-of-code.utils :as u]
            [clojure.core.match :refer [match]]))

(defn run-program [program start]
  (loop [ip 0
         a  start
         b  0]
    (match (get program ip)
      ["hlf" "a"] (recur (inc ip) (quot a 2) b)
      ["hlf" "b"] (recur (inc ip) a (quot b 2))
      ["tpl" "a"] (recur (inc ip) (* a 3) b)
      ["tpl" "b"] (recur (inc ip) a (* b 3))
      ["inc" "a"] (recur (inc ip) (inc a) b)
      ["inc" "b"] (recur (inc ip) a (inc b))
      ["jmp" offset] (recur (+ ip (parse-long offset)) a b)
      ["jie" "a" offset] (recur (if (even? a) (+ ip (parse-long offset)) (inc ip)) a b)
      ["jie" "b" offset] (recur (if (even? b) (+ ip (parse-long offset)) (inc ip)) a b)
      ["jio" "a" offset] (recur (if (= 1 a) (+ ip (parse-long offset)) (inc ip)) a b)
      ["jio" "b" offset] (recur (if (= 1 b) (+ ip (parse-long offset)) (inc ip)) a b)
      :else b)))

(comment
  (def path (u/get-input 2015 23))
  (def tpath "2015/23_test.in")

  (let [input (vec (u/read-file-list path #(vec (re-seq #"[a-z0-9+-]+" %))))]
    {:first  (run-program input 0)
     :second (run-program input 1)}))