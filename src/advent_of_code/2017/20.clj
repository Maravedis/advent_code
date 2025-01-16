(ns advent-of-code.2017.20
  (:require [advent-of-code.math :refer [quadratic-roots sum-to]]
            [advent-of-code.utils :as u]))

(def path (u/get-input 2017 20))
(def tpath (u/test-path 2017 20))

;; This gives back 2 values:
;; 158: p=<696,2161,-2404>, v=<-35,-108,100>, a=<0,0,2>
;; 243: p=<447,865,1300>, v=<-20,-39,-36>, a=<0,0,-2>
;; I _could_ code something to detect which one is closer to 0 at the start
;; Or I could just use my eyes to see it's 243
(defn part1 [path]
  (->> (u/read-file-list path u/nums)
       (mapv #(partition 3 %))
       (reduce-kv (fn [[idxes curr-min] idx [_ _ [ax ay az]]]
                    (let [a (+ (abs ax) (abs ay) (abs az))]
                      (cond (< a curr-min) [[idx] a]
                            (= a curr-min) [(conj idxes idx) a]
                            :else [idxes curr-min]))) [[] Integer/MAX_VALUE])
       first))

(defn calc-t [p1 v1 a1 p2 v2 a2]
  (let [a (/ (- a1 a2) 2)
        b (+ v1 (* -1 v2) (/ (- a1 a2) 2))
        c (- p1 p2)]
    (cond (not= 0 a)
          (some->> (quadratic-roots (/ (- a1 a2) 2) (+ v1 (* -1 v2) (/ (- a1 a2) 2)) (- p1 p2))
                   (some #(and (zero? (mod % 1)) (pos? %) %)))
          (not= 0 b)
          (let [res (* -1 (/ c b))]
            (when (and (zero? (mod res 1)) (pos? res)) res))
          :else (when (= 0 c) c))))

(defn pn [[[px py pz] [vx vy vz] [ax ay az]] t]
  (let [t (int t)]
    [(+ px (* t vx) (* (sum-to t) ax))
     (+ py (* t vy) (* (sum-to t) ay))
     (+ pz (* t vz) (* (sum-to t) az))]))

(defn collide? [[[pfx pfy pfz] [vfx vfy vfz] [afx afy afz] :as f] [[pgx pgy pgz] [vgx vgy vgz] [agx agy agz] :as g]]
  (let [tx (calc-t pfx vfx afx pgx vgx agx)
        ty (calc-t pfy vfy afy pgy vgy agy)
        tz (calc-t pfz vfz afz pgz vgz agz)
        ts [tx ty tz]]
    (some #(= (pn f %) (pn g %)) (distinct (remove nil? ts)))))

(defn part2 [path]
  (let [particles (->> (u/read-file-list path u/nums)
                       (mapv #(partition 3 %))
                       vec)]
    (loop [i         0
           particles particles]
      (if (>= i (dec (count particles)))
        (count particles)
        (let [f        (particles i)
              new-tail (reduce (fn [acc g] (cond-> acc (not (collide? f g)) (conj! g))) (transient []) (subvec particles (inc i)))]
          (if (= (count new-tail) (count (subvec particles (inc i))))
            (recur (inc i) particles)
            (recur i (into (subvec particles 0 i) (persistent! new-tail)))))))))

(comment

  (part1 path)
  (part2 tpath)
  (part2 path))
