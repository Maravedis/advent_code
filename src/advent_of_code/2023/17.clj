(ns advent-of-code.2023.17
  (:require [advent-of-code.utils :as u]
            [clojure.string :refer [split-lines]]
            [clojure.java.io :as io]
            [clojure.data.priority-map :refer [priority-map]]))

(defn read-input [resource]
  (let [lines (-> (io/resource resource)
                  slurp
                  split-lines)]
    (reduce-kv (fn [acc row line]
                 (reduce-kv (fn [acc1 col heat]
                              (assoc acc1 [row col] (u/char->digit heat)))
                            acc
                            (vec line)))
               {}
               lines)))

(defn mk-neighbors-fn [nodes]
  (let [rows (apply max (map first nodes))
        cols (apply max (map second nodes))]
    (fn [[row col d c]]
      (let [longest (= c 2)]
        (cond-> []
          (and (< col cols) (not= d 2) (not (and (= d 0) longest))) (conj [row (inc col) 0 (if (= 0 d) (inc c) 0)])
          (and (< row rows) (not= d 3) (not (and (= d 1) longest))) (conj [(inc row) col 1 (if (= 1 d) (inc c) 0)])
          (and (< 0 col)    (not= d 0) (not (and (= d 2) longest))) (conj [row (dec col) 2 (if (= 2 d) (inc c) 0)])
          (and (< 0 row)    (not= d 1) (not (and (= d 3) longest))) (conj [(dec row) col 3 (if (= 3 d) (inc c) 0)]))))))

(defn mk-neighbors-fn-2 [nodes]
  (let [rows (apply max (map first nodes))
        cols (apply max (map second nodes))]
    (fn [[row col d c]]
      (let [shortest (< c 3)
            longest  (= c 9)]
        (cond-> []
          (and (< col cols) 
               (not= d 2)
               (not (and (= d 0) longest))
               (not (and (#{1 3} d) shortest))) (conj [row (inc col) 0 (if (= 0 d) (inc c) 0)])
          (and (< row rows) 
               (not= d 3) 
               (not (and (= d 1) longest))
               (not (and (#{0 2} d) shortest))) (conj [(inc row) col 1 (if (= 1 d) (inc c) 0)])
          (and (< 0 col)
               (not= d 0)
               (not (and (= d 2) longest))
               (not (and (#{1 3} d) shortest))) (conj [row (dec col) 2 (if (= 2 d) (inc c) 0)])
          (and (< 0 row)
               (not= d 1)
               (not (and (= d 3) longest))
               (not (and (#{0 2} d) shortest))) (conj [(dec row) col 3 (if (= 3 d) (inc c) 0)]))))))


(defn ppop [v] (pop (pop v)))

(defn a*-manhattan
  ([input part2?] (let [nodes (keys input)
                              i     (apply max (map first nodes))
                              j     (apply max (map second nodes))]
                          (a*-manhattan input (keys input) [0 0] [i j] part2?)))
  ([input nodes start end part2?]
   (let [neighbors-fn (if part2? (mk-neighbors-fn-2 nodes) (mk-neighbors-fn nodes))
         start-weight (if part2? -1 1)]
     (loop [open-set  (priority-map (conj start 0 start-weight) 0 (conj start 1 start-weight) 0)
            costs     (transient {(conj start 0 start-weight) 0
                                  (conj start 1 start-weight) 0})
            came-from (transient {})]
       (let [[r c _ co :as curr] (first (peek open-set))]
         (if  (and (= [r c] end) (or (not part2?) (>= co 3))) 
           (costs curr)
           (let [neighbors     (->> (neighbors-fn curr)
                                    (filter #(< (+ (get costs curr 10000000) (input (ppop %))) (get costs % 10000000))))
                 next-costs    (reduce #(assoc! %1 %2 (+ (get costs curr 10000000) (input (ppop %2)))) costs neighbors)
                 next-open-set (reduce #(conj %1 %2) (pop open-set) (map #(vector % (+ (get costs % 10000000) (u/manhattan (ppop %) end))) neighbors))]
             (recur next-open-set next-costs (reduce #(conj! %1 %2) came-from (map #(vector % curr) neighbors))))))))))

(defn part1 [path]
  (let [input (read-input path)]
    (a*-manhattan input false)))

(defn part2 [path]
  (let [input (read-input path)]
    (a*-manhattan input true)))

(comment 
  (def path (u/get-input 2023 17))
  (def tpath "2023/17_test.in")
  (part1 path)
  (part2 path)
  )