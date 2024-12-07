(ns advent-of-code.2024.07
  (:require [advent-of-code.utils :as u]
            [clojure.math.combinatorics :as c]
            [clojure.core.reducers :as r]))

(def path (u/get-input 2024 7))
(def tpath "2024/7_test.in")

(defn || [a b]
  (parse-long (str a b)))

(defn calc [part2? [total f1 & fs]]
  (let [ops        (if part2? [+ * ||] [+ *])
        selections (map vec (c/selections ops (count fs)))
        fs         (vec fs)]
    (some #(= total (reduce-kv (fn [acc i v] (if (> acc total) (reduced 0) ((get % i) acc v))) f1 fs)) selections)))

(defn solve [path part2?]
  (let [input (vec (u/read-file-list path u/nums))]
    (->> (r/filter (partial calc part2?) input)
         (r/map first)
         (r/fold 100 + +))))

(comment

  (time (solve path false))
  (time (solve path true)))