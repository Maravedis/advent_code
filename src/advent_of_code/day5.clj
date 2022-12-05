(ns advent-of-code.day5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-stack-line [acc line]
  (->> (partition-all 4 line)
       vec
       (reduce-kv (fn [acc1 idx crate]
                    (if (not (every? #(= % \space) crate))
                      (update acc1 (+ 1 idx) concat [(second crate)])
                      acc1)) acc)))

(defn read-stacks [lines]
  (->> lines
       drop-last
       (reduce read-stack-line {})))

(defn read-instruction-line [line]
  (->> (str/split line #" ")
       (drop 1)
       (take-nth 2)
       (map read-string)))

(defn parse-input [resource]
  (let [[stacks instructions] (-> (slurp (io/resource resource))
                                  (str/split #"\n\n"))]
    {:stacks (read-stacks (str/split-lines stacks))
     :ins    (->> (str/split-lines instructions)
                  (map read-instruction-line))}))

(defn move [better-move? stacks [n source destination]]
  (let [xs       (get stacks source)
        reverse? (not better-move?)]
    (-> stacks
        (assoc source (drop n xs))
        (update destination #(concat (cond-> (take n xs) reverse? reverse) %)))))

(defn execute [resource better-move?]
  (let [{:keys [stacks ins]} (parse-input resource)]
    (->> (reduce (partial move better-move?) stacks ins)
         (map (fn [[k [h]]] [k h]))
         (sort-by first)
         (map second)
         (apply str))))
(comment
  (execute "day5" false)
  (execute "day5" true))