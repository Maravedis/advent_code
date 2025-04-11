(ns advent-of-code.2018.02
  (:require [advent-of-code.utils :as u]
            [clojure.string :refer [join]]))

(def path (u/get-input 2018 2))
(def tpath (u/test-path 2018 2))

(defn part1 [path]
  (let [input (->> (u/read-file-list path frequencies)
                   (map #(set (map second %))))]
    (* (u/count-when #(% 2) input) (u/count-when #(% 3) input))))

(defn part2 [path]
  (let [input (u/read-file-list path identity)]
    (loop [[h & t] input]
      (if-let [res (reduce (fn [_ e] (if (->> (map (fn [x y] (if (= x y) 0 1)) h e)
                                              (reduce +)
                                              (= 1))
                                       (reduced e)
                                       false)) false t)]
        (->> (map vector h res)
             (reduce (fn [acc [x y]] (cond-> acc (= x y) (conj x))) [])
             join)
        (recur t)))))

(comment

  (part1 tpath)
  (part1 path)

  (part2 tpath)
  (part2 path))

