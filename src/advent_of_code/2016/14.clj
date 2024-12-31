(ns advent-of-code.2016.14
  (:require [advent-of-code.utils :as u]
            [clj-commons.digest :refer [md5]]))

(def path (u/get-input 2016 14))
(def tpath (u/test-path 2016 14))

(def kchars "abcdefghijklmnopqrstuvwxyz01234567890")
(def seed (-> (reduce (fn [acc k] (assoc acc k [])) {} kchars)
              (assoc :chunks {})))

(defn calc-next-fn [salt hashing-fn]
  (fn [[i acc]]
    (let [chunks   (partition-by identity (hashing-fn (str salt i)))
          triplet  (first (filter #(>= (count %) 3) chunks))
          quintets (filter #(>= (count %) 5) chunks)]
      (->> (reduce (fn [acc [h]] (update acc h conj i)) (cond-> acc triplet (assoc-in [:chunks i] triplet)) quintets)
           (vector (inc i))))))

(defn aggregate [salt hashing-fn] (iterate (calc-next-fn salt hashing-fn) [0 seed]))
(defn md5-2016 [s] (nth (iterate md5 s) 2017))

(defn solve [path hashing-fn]
  (let [salt (u/read-file-line path identity)]
    (loop [[[_ infos] & t] (drop 1001 (aggregate salt hashing-fn))
           i               0
           nb-keys         0]
      (if (= nb-keys 64)
        (dec i)
        (if-let [potentials (get-in infos [:chunks i])]
          (if (some (fn [c] (some #(< i % (+ i 1001)) (infos c))) potentials)
            (recur t (inc i) (inc nb-keys))
            (recur t (inc i) nb-keys))
          (recur t (inc i) nb-keys))))))

(comment

  (time (solve path md5))
  (time (solve tpath md5))
  (time (solve tpath md5-2016))
  (time (solve path md5-2016))) ; ~1min. Could parallelize the computation of hashes, but I'm lazy.
