(ns advent-of-code.2023.12
  (:require [advent-of-code.utils :as u]
            [clojure.string :refer [split join]]))

(defn count-line 
  ([count-line line groups] (count-line line groups 0 false 0))
  ([count-line [h & t] groups res in-group g-idx]
   (if (nil? h)
     1
     (let [gv    (get groups g-idx)
           gleft (drop g-idx groups)
           gsum  (apply + (dec (count gleft)) gleft)]
       (case h
         \. (cond (and in-group (= res gv)) (count-line t groups 0 false (inc g-idx))
                  (and (not in-group) (>= (count t) gsum)) (count-line t groups 0 false g-idx)
                  :else 0)
         \# (if (and gv (>= (get groups g-idx) (inc res))) (count-line t groups (inc res) true g-idx) 0)
         \? (+ (count-line (cons \. t) groups res in-group g-idx)
               (count-line (cons \# t) groups res in-group g-idx)))))))

(def count-m (u/fix (memoize count-line)))

(defn part1 [path]
  (let [lines  (u/read-file-list path #(first (split % #" ")))
        groups (u/read-file-list path (comp vec u/nums))]
    (apply + (pmap count-m lines groups))))

(defn part2 [path]
  (let [lines  (->> (u/read-file-list path #(first (split % #" ")))
                    (map #(join "?" (repeat 5 %))))
        groups (->> (u/read-file-list path (comp vec u/nums))
                    (map #(vec (apply concat (repeat 5 %)))))]
    (apply + (pmap count-m lines groups))))

(comment
  
  (def path (u/get-input 2023 12))
  (def tpath "2023/12_test.in")

  (time (part1 path))
  (time (part2 path))
  )