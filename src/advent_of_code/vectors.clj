(ns advent-of-code.vectors)

(defn evict-at [coll idx]
  (into (subvec coll 0 idx) (subvec coll (inc idx))))

(defn insert-at [coll idx v]
  (into (conj (subvec coll 0 idx) v) (subvec coll idx)))

(defn rotate-left [coll n]
  (into (subvec coll (mod n (count coll))) (subvec coll 0 (mod n (count coll)))))

(defn rotate-right [coll n]
  (let [idx (- (count coll) (mod n (count coll)))]
    (into (subvec coll idx) (subvec coll 0 idx))))

(defn swap-at [coll x y]
  (let [a (get coll x)
        b (get coll y)]
    (assoc coll x b y a)))

(defn index-of [coll v]
  (let [n (count coll)]
    (loop [i 0]
      (cond (>= i n) -1
            (= v (coll i)) i
            :else (recur (inc i))))))

(comment

  (rotate-left [1 2 3 4 5 6] 14)
  (index-of [1 2 3 4 5 6] 14))