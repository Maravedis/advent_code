(ns advent-of-code.2023.10
  (:require [advent-of-code.utils :as u]))

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

; Shoelace algorithm + Pick theorem
(defn part2 [path]
  (let [floor     (vec (u/read-file-list path vec))
        full-loop (find-loop floor)
        [fi se]   (reduce (fn [[x y] [a b]] [(conj x a) (conj y b)]) [[] []] (partition 2 full-loop))
        lo        (concat fi (reverse (cons (first fi) se)))]
    (- (inc (/ (reduce (fn [res [[a b] [c d]]] (+ res (- (* a d) (* b c)))) 0 (partition 2 1 lo)) 2)) (/ (count full-loop) 2))))

(comment
  (def path (u/get-input 2023 10))
  (def tpath "2023/10_test.in")
  
  (part1 path)
  (part2 path)
  )