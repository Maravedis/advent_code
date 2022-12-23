(ns advent-of-code.day23
  (:require [clojure.java.io :as io]
            [clojure.set :refer [union]]
            [clojure.string :as str]))

(defn read-grove [path]
  (->> (slurp (io/resource path))
       str/split-lines
       (reduce-kv (fn [acc row line]
                    (union acc (set (keep-indexed (fn [col chr]
                                                (when (= chr \#) [row col]))
                                              line)))) #{})))

(def checks {[-1 0] [[-1 1] [-1 0] [-1 -1]]
             [1 0]  [[1 1] [1 0] [1 -1]]
             [0 -1] [[-1 -1] [0 -1] [1 -1]]
             [0 1]  [[-1 1] [0 1] [1 1]]})

(defn adj [[x y]]
  (for [i [-1 0 1]
        j [-1 0 1]
        :when (not= 0 i j)]
    [(+ x i) (+ y j)]))

(defn make-proposition [elf grove orientation]
  (if (not-any? #(contains? grove %) (adj elf))
    nil
    (loop [[row col] elf
           [h & t]   orientation]
      (if  (nil? h)  
        nil
        (let [to-check (map (fn [[rc cc]] [(+ row rc) (+ col cc)]) (checks h))]
          (if (some #(contains? grove %) to-check)
            (recur elf t)
            [(+ row (first h)) (+ col (second h))]))))))

(defn calc-rectangle [grove]
  (let [[minr maxr] (apply (juxt min max) (map first grove))
        [minc maxc] (apply (juxt min max) (map second grove))]
    (- (* (abs (- (inc maxr) minr)) (abs (- (inc maxc) minc))) (count grove))))

(defn run [path part1?]
  (let [grove (read-grove path)]
    (loop [grove grove
           o     (cycle [[-1 0] [1 0] [0 -1] [0 1]])
           i 0]
      (if (and part1? (>= i 10)) 
        (calc-rectangle grove)
        (let [props      (->> (map (fn [elf] [elf (make-proposition elf grove (take 4 o))]) grove)
                              (remove (fn [[_ v]] (nil? v))))
              duplicates (->> (group-by second props)
                              (remove (fn [[_ v]] (= (count v) 1)))
                              vals
                              (mapcat (fn [props] (map first props)))
                              set)
              props (remove (fn [[k _]] (contains? duplicates k)) props)
              to-remove (map first props)
              to-add (map second props)]
          (if (empty? to-remove)
            (inc i)
            (recur (apply conj (apply disj grove to-remove) to-add) (rest o) (inc i))))))))

(comment 
  (time (run "day23" true))
  (time (run "day23" false))
  (#{1 2} 3)
  )