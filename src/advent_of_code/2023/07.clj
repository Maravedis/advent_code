(ns advent-of-code.2023.07
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(def order-cards (->> (map-indexed #(vector %2 %1) "AKQJT98765432") (into {})))
(def order-cards-2 (assoc order-cards \J 13))

(defn compare-cards [dict x y] (- (get dict y) (get dict x)))

(defn classify-hand [hand part2?]
  (let [grouped       (frequencies hand)
        [[_ m] [_ n]] (->> (if part2? (dissoc grouped \J) grouped)
                           (sort-by second >))
        m             (+ (if-let [j (and part2? (grouped \J))] j 0) (if m m 0))]
    (cond
      (= m 5)               6
      (= m 4)               5
      (and (= m 3) (= n 2)) 4
      (= m 3)               3
      (and (= m 2) (= n 2)) 2
      (= m 2)               1
      :else                 0)))

(defn compare-hands [part2? l r]
  (let [comp (- (classify-hand l part2?) (classify-hand r part2?))]
    (if (= comp 0)
      (->> (map (partial compare-cards (if part2? order-cards-2 order-cards)) l r) (drop-while #(= 0 %)) first)
      comp)))

(defn gen-solve [path part2?]
  (let [hands (u/read-file-list path #(str/split % #" "))]
    (->> hands
         (sort-by first (partial compare-hands part2?))
         (map-indexed (fn [idx [_ bid]] (* (inc idx) (parse-long bid))))
         (apply +))))

(comment
  (gen-solve "2023/7.in" false) ; ~68 ms
  (gen-solve "2023/7.in" true) ; ~68 ms 
  )