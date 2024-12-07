(ns advent-of-code.2024.07
  (:require [advent-of-code.utils :as u]
            [clojure.core.reducers :as r]))

(def path (u/get-input 2024 7))
(def tpath "2024/7_test.in")

(defn || [a b]
  (parse-long (str a b)))

(defn calc [part2? [total f1 & fs]]
  (let [ops (apply juxt (if part2? [+ * ||] [+ *]))]
    (->> (reduce (fn [states v]
                   (->> (r/mapcat #(ops % v) states)
                        (r/remove #(> % total))
                        r/foldcat)) [f1] fs)
         (some #(= total %)))))

(defn solve [path part2?]
  (let [input (vec (u/read-file-list path u/nums))]
    (->> (r/filter (partial calc part2?) input)
         (r/map first)
         (r/fold 100 + +))))

(comment

  (time (solve path false))
  (time (solve path true)))