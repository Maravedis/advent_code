(ns advent-of-code.2022.day20
  (:require [advent-of-code.utils :as u]
            [clojure.core.rrb-vector :as fv]))

(defn vec-remove
  [coll pos]
  (fv/catvec (fv/subvec coll 0 pos) (fv/subvec coll (inc pos))))

(defn vec-insert [coll pos e]
  (fv/catvec (conj (fv/subvec coll 0 pos) e) (fv/subvec coll pos)))

(defn extract [source l]
  ((juxt #(nth % 1000)
         #(nth % 2000)
         #(nth % 3000))
   (->> l
        (map #(get source %))
        cycle
        (drop-while #(not= 0 %)))))

(def decrytion-key 811589153)

(defn part1 [path]
  (let [l (vec (u/read-file-list path))
        c (dec (count l))]
    (->> (reduce-kv (fn [acc n-iter i]
                      (if (= i 0) acc
                          (let [idx     (first (keep-indexed #(when (= %2 n-iter) %1) acc))
                                new-idx (mod (+ i idx) c)]
                            (-> (vec-remove acc idx)
                                (vec-insert new-idx n-iter))))) (fv/vec (range (count l))) l)
         (extract l)
         u/sum)))

(defn part2 [path]
  (let [l (mapv #(* decrytion-key %) (u/read-file-list path))
        c (count l)]
    (->> (reduce-kv (fn [acc n-iter i]
                      (if (= i 0) acc
                          (let [idx     (first (keep-indexed #(when (= %2 (mod n-iter c)) %1) acc))
                                new-idx (mod (+ i idx) (dec c))]
                            (-> (vec-remove acc idx)
                                (vec-insert new-idx (mod n-iter c)))))) (fv/vec (range c)) (vec (apply concat (repeat 10 l))))
         (extract l)
         u/sum)))



(comment
  (time (part1 "2022/day20"))
  (time (part2 "2022/day20"))
  )

