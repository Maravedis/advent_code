(ns advent-of-code.2015.09
  (:require [advent-of-code.utils :as u]
            [clojure.string :refer [split]]
            [clojure.math.combinatorics :as c]
            [clojure.set :refer [union]]))

(defn parse [line]
  (let [[s e d] (split line #" (to |= )?")] [#{s e} (parse-long d)]))

(defn run [path f]
  (let [distances (->> (u/read-file-list path parse)
                       (into {}))
        places    (reduce union (keys distances))]
    (->> (c/permutations places)
         (map #(->> (partition 2 1 %)
                    (reduce (fn [res pt] (+ res (distances (set pt)))) 0)))
         (apply f))))

(comment
  (def path (u/get-input 2015 9))
  (run path min)
  (run path max)
  )