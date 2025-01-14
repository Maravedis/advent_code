(ns advent-of-code.2017.12
  (:require [advent-of-code.utils :as u]
            [clojure.set :refer [intersection union]]))

(def path (u/get-input 2017 12))
(def tpath (u/test-path 2017 12))

(defn reduce-groups [groups]
  (loop [[h & t :as base] groups]
    (let [reduced (reduce (fn [gs group]
                            (loop [i 0]
                              (cond (>= i (count gs)) (conj gs group)
                                    (not-empty (intersection (gs i) group)) (update gs i union group)
                                    :else (recur (inc i))))) [h] t)]
      (if (= (count reduced) (count base))
        base
        (recur reduced)))))

(defn part1 [path]
  (->> (u/read-file-list path (comp set u/nums))
       reduce-groups
       first
       count))

(defn part2 [path]
  (->> (u/read-file-list path (comp set u/nums))
       reduce-groups
       count))

(comment

  (part1 tpath)
  (part1 path)

  (part2 path)

  ;
  )
