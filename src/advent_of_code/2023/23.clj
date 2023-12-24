(ns advent-of-code.2023.23
  (:require [advent-of-code.utils :as u]
            [clojure.core.reducers :as r]))

(def path (u/get-input 2023 23))
(def tpath "2023/23_test.in")

(defn mk-neighbors-fn [input part2?]
  (let [rows (dec (count input))
        cols (dec (count (first input)))]
    (fn [[row col :as curr] prevs] 
      (->> 
       (if part2?
         [[row (inc col)] [row (dec col)] [(inc row) col] [(dec row) col]]
         (case (get-in input curr)
           \> [[row (inc col)]]
           \< [[row (dec col)]]
           \v [[(inc row) col]]
           \^ [[(dec row) col]]
           [[row (inc col)] [row (dec col)] [(inc row) col] [(dec row) col]]))
       (filter (fn [[x y :as e]] (and (<= 0 x rows) (<= 0 y cols) (not= \# (get-in input e)))))
       (remove #(prevs %))))))

(defn get-nodes [input part2?]
  (let [start [0 1]
        end   [(dec (count input)) (- (count (first input)) 2)]
        neighbors-fn (mk-neighbors-fn input part2?) ]
    (-> (for [i     (range 1 (count (first input)))
              j     (range 1 (count input))
              :let  [ns (r/foldcat (neighbors-fn [i j] #{}))]
              :when (and (not= \# (get-in input [i j])) (>= (count ns) 3))]
          [i j])
        (conj start end)
        set)))

(defn mk-graph [input part2? points]
  (let [n-fn  (mk-neighbors-fn input part2?)]
    (->> (for [pt points
               :let [check (disj points pt)]]
           (loop [open-set [[pt 0]]
                  seen     #{}
                  result   {}]
             (if (empty? open-set)
               result
               (let [[curr n] (peek open-set)]
                 (if (check curr)
                   (recur (pop open-set) seen (update result pt (fnil conj {}) [curr n]))
                   (recur (reduce conj (pop open-set) (map #(vector % (inc n)) (n-fn curr seen))) (conj seen curr) result))))))
         (apply merge-with merge))))

(defn gen-solve [path part2?]
  (let [input        (vec (u/read-file-list path vec))
        start        [0 1]
        end          [(dec (count input)) (- (count (first input)) 2)]
        graph        (->> (get-nodes input part2?)
                          (mk-graph input part2?))]
    (loop [open-set [[start #{} 0]]
           path     0]
      (if (empty? open-set)
        path
        (let [[curr seen n] (peek open-set)]
          (if
           (= end curr) (recur (pop open-set) (max path n))
           (let [nexts (->> (apply dissoc (graph curr) seen)
                            (map (fn [[k v]] [k (conj seen curr) (+ n v)])))]
             (recur (reduce conj (pop open-set) nexts) path))))))))


(comment

  (gen-solve path false)
  (gen-solve path true)
  )