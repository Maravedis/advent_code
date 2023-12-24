(ns advent-of-code.2023.24
  (:require [advent-of-code.utils :as u]
            [clojure.core.matrix :as m]
            [clojure.math.combinatorics :refer [combinations]])
  (:import [com.microsoft.z3 Status Context]))

(def path (u/get-input 2023 24))
(def tpath "2023/24_test.in")

(defn cross [[x y] [a b]]
  (- (* x b) (* y a)))

(defn intersect2d [[x1 y1 _ dx1 dy1] [x2 y2 _ dx2 dy2]]
  (let [p [x1 y1]
        r [dx1 dy1]
        q [x2 y2]
        s [dx2 dy2]
        t (/ (cross (m/sub q p) s) (double (cross r s)))
        u (/ (cross (m/sub q p) r) (double (cross r s)))]
    (when (and (not= 0 (cross r s))
               (not (neg? t))
               (not (neg? u)))
      [(+ x1 (* t dx1)) (+ y1 (* t dy1))])))

(defn part1 [path lower upper]
  (let [input  (vec (u/read-file-list path u/nums))
        combo  (combinations input 2)]
    (->> (keep (fn [[x y]] (when-let [[a b] (and (not= x y) (intersect2d x y))] 
                             (when (and (<= lower a upper) (<= lower b upper)) 
                               [a b]))) combo)
         count)))

(defn part2 [path]
  (let [input  (vec (u/read-file-list path u/nums))
        ctx    (Context. {"model" "true"})
        vars   "(declare-const xr Int) 
                (declare-const yr Int)
                (declare-const zr Int)
                (declare-const vxr Int)
                (declare-const vyr Int)
                (declare-const vzr Int)"
        s      (.mkSolver ctx)
        script (reduce (fn [script [x y z vx vy vz]]
                         (str script
                              (format "\n(assert (= (* (- xr %d ) (- %d vyr)) (* (- %d vxr) (- yr %d))))" x vy vx y)
                              (format "\n(assert (= (* (- %d vzr) (- yr %d)) (* (- zr %d) (- %d vyr))))" vz y z vy))) vars (take 5 input))]
    (.add s (.parseSMTLIB2String ctx script nil nil nil nil))
    (if (= (.check s) Status/SATISFIABLE)
      (let [model (.getModel s)]
        (reduce (fn [res sym]
                  (+ res
                     (.getBigInteger (.evaluate model (.mkIntConst ctx sym) false)))) 0 ["xr" "yr" "zr"])) 
      "no")))

(comment
  (time (part1 tpath 7 27))
  (time (part1 path 200000000000000 400000000000000))

  (time (part2 path))
  )