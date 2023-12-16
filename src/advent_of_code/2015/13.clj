(ns advent-of-code.2015.13
  (:require [advent-of-code.utils :as u]
            [clojure.math.combinatorics :refer [permutations]]))

(def re #"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.")

(defn part1 [path]
  (let [plan    (->> (u/read-file-list path #(re-find re %))
                     (map (fn [[_ x pm h y]] [[x y] (case pm "gain" (parse-long h) "lose" (* -1 (parse-long h)))]))
                     (into {}))
        persons (->> plan keys (map first) distinct
                     permutations
                     (map (fn [xs] (cons (last xs) xs)))
                     (map #(partition 2 1 %)))]
    (apply max (map #(reduce (fn [res [x y]]
                               (+ res (plan [x y]) (plan [y x]))) 0 %) persons))))

(defn part2 [path]
  (let [plan    (->> (u/read-file-list path #(re-find re %))
                     (map (fn [[_ x pm h y]] [[x y] (case pm "gain" (parse-long h) "lose" (* -1 (parse-long h)))]))
                     (into {})) 
        persons (->> plan keys (map first) distinct)
        plan (reduce (fn [res x] (assoc res [x "Maravedis"] 0
                                        ["Maravedis" x] 0)) plan persons)]
    (->> persons
         (cons "Maravedis")
         permutations
         (map (fn [xs] (cons (last xs) xs)))
         (map #(partition 2 1 %))
         (map #(reduce (fn [res [x y]]
                         (+ res (plan [x y]) (plan [y x]))) 0 %))
         (apply max))))

(comment
  (def path (u/get-input 2015 13))

  (part1 path)
  (part2 path)
  )