(ns advent-of-code.2017.06
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2017 6))
(def tpath (u/test-path 2017 6))

(defn index-max [vector]
  (second (reduce-kv (fn [[m :as acc] idx v] (if (> v m) [v idx] acc)) [Integer/MIN_VALUE -1] vector)))

(defn run [path part1?]
  (let [input (u/read-file-line path u/nums)
        n     (count input)]
    (loop [curr   input
           seen   (transient #{})
           cycles 0
           on?    false]
      (cond (and (or on? part1?) (seen curr)) cycles
            (seen curr) (recur curr (transient #{}) 0 true)
            :else (let [idx    (index-max curr)
                        v      (curr idx)
                        global (quot v n)
                        rem    (mod v n)
                        new-c  (->> (assoc curr idx 0)
                                    (mapv #(+ global %)))]
                    (recur (reduce (fn [acc i] (update acc (mod (+ idx i 1) n) inc)) new-c (range rem))
                           (conj! seen curr)
                           (inc cycles)
                           on?))))))

(comment

  (run tpath true)
  (run path true)
  (run tpath false)
  (run path false)

  ;
  )
