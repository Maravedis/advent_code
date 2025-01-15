(ns advent-of-code.2017.10
  (:require [advent-of-code.utils :as u]
            [advent-of-code.vectors :refer [catvec]]
            [clojure.string :refer [join]]))

(def path (u/get-input 2017 10))
(def tpath (u/test-path 2017 10))

(defn knot-hash
  ([stream list-size rounds]
   (loop [[h & t] stream
          round   0
          curr    0
          skip    0
          list    (vec (range list-size))]
     (cond  (>= round rounds) list
            (nil? h) (recur stream (inc round) curr skip list)
            :else
            (recur t round (mod (+ curr h skip) list-size) (inc skip)
                   (reduce (fn [acc i] (let [idx     (mod (+ curr i) list-size)
                                             rev-idx (mod (- (mod (+ curr h -1) list-size) i) list-size)]
                                         (-> (assoc acc idx (get acc rev-idx))
                                             (assoc rev-idx (get acc idx))))) list (range (/ h 2))))))))

(defn dense-knot-hash [to-hash]
  (->> (knot-hash (catvec (map byte to-hash) [17 31 73 47 23]) 256 64)
       (partition 16)
       (map #(reduce bit-xor %))))

(defn part1 [path size]
  (let [[a b] (knot-hash (u/read-file-line path u/nums) size 1)]
    (* a b)))

(defn part2 [path]
  (->> (dense-knot-hash (u/read-file-line path identity))
       (map #(format "%02x" %))
       join))

(comment

  (part1 tpath 5)
  (part1 path 256)

  (part2 path))