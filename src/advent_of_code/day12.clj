(ns advent-of-code.day12
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines]]))

(defn parse-height [c]
  (if (Character/isUpperCase c)
    (case c \S 0 \E 25)
    (- (int c) 97)))

(defn read-input [resource]
  (let [lines (-> (io/resource resource)
                  slurp
                  split-lines)]
    (reduce-kv (fn [acc row line]
                 (reduce-kv (fn [acc1 col height]
                              (cond-> (assoc acc1 [row col] (parse-height height))
                                (= height \E) (assoc :end [row col])
                                (= height \S) (assoc :start [row col])))
                            acc
                            (vec line)))
               {}
               lines)))

(defn mk-neighbors-fn [input nodes]
  (let [rows (apply max (map first nodes))
        cols (apply max (map second nodes))]
    (fn [row col]
      (->> [[(inc row) col] [(dec row) col] [row (inc col)] [row (dec col)]]
           (filter (fn [[r c]]
                     (and (<= 0 r rows)
                          (<= 0 c cols)
                          (<= (get input [r c]) (+ 1 (get input [row col]))))))))))

(defn udpate-costs [costs unvisited curr neighbors-fn]
  (let [curr-cost (get costs curr)
        neighbors (apply neighbors-fn curr)]
    (reduce (fn [acc n]
              (if (unvisited n)
                (update acc n #(min % (inc curr-cost)))
                acc)) 
            costs neighbors)))

(defn djikstra
  ([input nodes start] (djikstra input nodes start nil))
  ([input nodes start end] 
   (let [neighbors-fn (mk-neighbors-fn input nodes)]
     (loop [costs     (assoc (zipmap nodes (repeat Long/MAX_VALUE)) start 0)
            curr      start
            unvisited (disj (set nodes) start)]
       (cond
         (and (some? end) (= curr end)) (get costs curr)
         (or (empty? unvisited) (= Long/MAX_VALUE (get costs curr))) costs
         :else (let [next-costs (udpate-costs costs unvisited curr neighbors-fn)
                     next-node  (apply min-key next-costs unvisited)]
                 (recur next-costs next-node (disj unvisited next-node))))))))

(defn path [resource all-a?]
  (let [{:keys [start end]
         :as   input} (read-input resource)
        pure (dissoc input :start :end)
        nodes (keys pure)]
    (if all-a?
      (let [reverse-map (->> (map (fn [[k v]] [k (- 25 v)]) pure)
                             (into {}))
            costs-map   (djikstra reverse-map nodes end)] 
        (->> (for [r (range (inc (apply max (map first nodes))))
                   c (range (inc (apply max (map second nodes))))]
               (when (= 0 (get pure [r c]))
                 (get costs-map [r c])))
             (remove nil?)
             (apply min)))
      (djikstra pure nodes start end))))

(comment 
  (time (path "day12" false))
  (time (path "day12" true)))
  