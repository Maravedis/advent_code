(ns advent-of-code.2023.14
  (:require [advent-of-code.utils :as u]
            [clojure.core.reducers :as r]))

(defn parse [path]
  (let [lines            (vec (u/read-file-list path vec))
        h                (count lines)
        w                (count (first lines))
        [rounds squares] (reduce-kv (fn [r i l]
                                      (reduce-kv (fn [[r1 r2] j x]
                                                   (cond (= x \O) [(conj! r1 [i j]) r2]
                                                         (= x \#) [r1 (conj! r2 [i j])]
                                                         :else [r1 r2]))
                                                 r l))
                                    [(transient #{}) (transient #{})] lines)
        squares          (reduce conj! squares (for [i (range h)] [i w]))
        squares          (reduce conj! squares (for [j (range w)] [h j]))]
    [h (persistent! rounds) (persistent! squares)]))

(defn roll-north [rounds squares [i j]]
  (loop [row (dec i)]
    (cond
      (= -1 row)        [0 j]
      (squares [row j]) [(inc row) j]
      (rounds [row j])  (update (roll-north rounds squares [row j]) 0 inc)
      :else             (recur (dec row)))))

(defn roll-west [rounds squares [i j]]
  (loop [col (dec j)]
    (cond
      (= -1 col)        [i 0]
      (squares [i col]) [i (inc col)]
      (rounds [i col])  (update (roll-west rounds squares [i col]) 1 inc)
      :else             (recur (dec col)))))

(defn roll-south [rounds squares [i j]]
  (loop [row (inc i)]
    (cond
      (squares [row j]) [(dec row) j]
      (rounds [row j])  (update (roll-south rounds squares [row j]) 0 dec)
      :else             (recur (inc row)))))

(defn roll-east [rounds squares [i j]]
  (loop [col (inc j)]
    (cond
      (squares [i col]) [i (dec col)]
      (rounds [i col])  (update (roll-east rounds squares [i col]) 1 dec)
      :else             (recur (inc col)))))

(defn rotate [rounds squares r-fn ch]
  (->> (r/map #(r-fn rounds squares %) (vec rounds))
       (r/fold ch r/cat r/append!)
       set))

(defn north-load [rounds h]
  (r/reduce (fn [res [l]] (+ res (- h l))) 0 rounds))

(defn cycles [rounds squares n]
  (let [ch            (int (/ (count rounds) 8))
        full-rotation (comp #(rotate % squares roll-east ch)
                            #(rotate % squares roll-south ch)
                            #(rotate % squares roll-west ch)
                            #(rotate % squares roll-north ch))]
    (loop [[state & states] (iterate full-rotation rounds)
           k                0
           hashes           (transient {})]
      (cond (= k n)        state
            (hashes state) (cycles state squares (mod (- n (hashes state)) (- k (hashes state))))
            :else          (recur states (inc k) (conj! hashes [state k]))))))

(defn part1 [path]
  (let [[h rounds squares] (parse path)]
    (north-load (rotate rounds squares roll-north (int (/ (count rounds) 8))) h)))

(defn part2 [path n]
  (let [[h rounds squares] (parse path)
        cycled (cycles rounds squares n)] 
   (north-load cycled h)))

(comment

  (def path (u/get-input 2023 14))
  (def tpath "2023/14_test.in")

  (time (part1 path))
  (time (part2 path 1000000000))
  )