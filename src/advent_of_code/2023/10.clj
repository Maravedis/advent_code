(ns advent-of-code.2023.10
  (:require [advent-of-code.utils :as u]
            [clojure.set :refer [difference select union]]))

;; 0 r 
;; 1 d 
;; 2 l 
;; 3 u

(def prox-map {[1 \|] [1 [-1 0]]
               [3 \|] [3 [1 0]]
               [0 \-] [0 [0 -1]]
               [2 \-] [2 [0 1]]
               [2 \J] [1 [-1 0]]
               [3 \J] [0 [0 -1]]
               [0 \L] [1 [-1 0]]
               [3 \L] [2 [0 1]]
               [1 \F] [2 [0 1]]
               [0 \F] [3 [1 0]]
               [1 \7] [0 [0 -1]]
               [2 \7] [3 [1 0]]})

(defn find-start [floor]
  (let [w (count (first floor))]
    (loop [row 0
           col 0]
      (cond (= \S (get-in floor [row col])) [row col]
            (= (mod (inc col) w) 0)         (recur (inc row) 0)
            :else                           (recur row (inc col))))))

(defn find-start-neighbors [floor [row col]]
  (let [above (get-in floor [(dec row) col])
        below (get-in floor [(inc row) col])
        right (get-in floor [row (inc col)])
        left  (get-in floor [row (dec col)])]
    (cond-> []
      (or (= \| above)
          (= \F above)
          (= \7 above)) (conj [1 [(dec row) col]])
      (or (= \| below)
          (= \J below)
          (= \L below)) (conj [3 [(inc row) col]])
      (or (= \- right)
          (= \7 right)
          (= \J right)) (conj [2 [row (inc col)]])
      (or (= \- left)
          (= \L left)
          (= \F left)) (conj [0 [row (dec col)]]))))

(defn next-node [floor [d [r c]]]
  (let [[nd [r1 c1]] (prox-map [d (get-in floor [r c])])]
    [nd [(+ r1 r) (+ c1 c)]]))

(defn find-loop [floor]
  (let [start   (find-start floor)
        [fi se] (find-start-neighbors floor start)]
    (loop [curr1   fi
           curr2   se
           visited (transient [start])]
      (if (= (second curr1) (second curr2))
        (persistent! (conj! visited (second curr1)))
        (recur (next-node floor curr1) (next-node floor curr2) (conj! (conj! visited (second curr1)) (second curr2)))))))

(defn part1 [path]
  (let [floor   (vec (u/read-file-list path vec))]
    (/ (count (find-loop floor)) 2)))

(defn next-not-in [h w fl i o n]
  (let [visited (union fl i o n)]
    (first (drop-while #(visited %) (for [r (range 0 h) c (range 0 w)] [r c])))))

(def close [[0 1] [1 0] [0 -1] [-1 0]])

(defn neighbors [nodes h w fl]
  (let [nexts (set (mapcat (fn [[r c]] (map (fn [[x y]] [(+ r x) (+ c y)]) close)) nodes))] 
    (->> (difference nexts nodes fl)
         (select (fn [[r c]] (and (<= 0 r h) (<= 0 c w)))))))

(defn flood [h w fl curr]
  (loop [result #{curr}]
    (if-let [nexts (not-empty (neighbors result h w fl))]
      (recur (union result nexts))
      result)))

; Not adapted to all inputs, problem being S
(defn is-outside [floor fl nodes]
  (loop [[r c :as curr] (first nodes)
         start          nil
         i              0]
    (if (< r 0)
      (even? i)
      (if-let [node (get-in floor (if-let [x (fl curr)] x [-1 -1]))] 
        (case node
          \- (recur [(dec r) c] nil (inc i))
          \F (recur [(dec r) c] nil (if (= start \L) i (inc i)))
          \7 (recur [(dec r) c] nil (if (or (= start \J) (= start \S)) i (inc i)))
          \| (recur [(dec r) c] start i)
          (recur [(dec r) c] node i))
        (recur [(dec r) c] nil i)))))

(defn part2 [path]
  (let [floor     (vec (u/read-file-list path vec))
        h         (count floor)
        w         (count (first floor))
        full-loop (set (find-loop floor))]
    (loop [curr    [0 0]
           inside  #{}
           outside #{}]
      (if (nil? curr)
        (count inside)
        (let [nodes (flood h w full-loop curr)]
          (if (is-outside floor full-loop nodes)
            (recur (next-not-in h w full-loop inside outside nodes) inside (apply conj outside nodes))
            (recur (next-not-in h w full-loop inside outside nodes) (apply conj inside nodes) outside)))))))

(comment
  (def path (u/get-input 2023 10))
  
  (part1 path)
  (part2 path)
  )