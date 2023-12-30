(ns advent-of-code.2023.25
  (:require [advent-of-code.utils :as u]
            [clojure.data.priority-map :refer [priority-map-by]]
            [clojure.set :refer [union]]))

(def path (u/get-input 2023 25))
(def tpath "2023/25_test.in")

(defn parse-line [line]
  (let [[orig & dests] (re-seq #"\w+" line)]
    [(keyword orig) (map keyword dests)]))

(defn min-cut-phase [graph]
  (let [glen  (count graph)
        gkey  (keys graph)
        start (rand-nth gkey)]
    (loop [A     #{start}
           prev  nil
           nexts (get graph start)]
      (if (= (count A) (- glen 1))
        [A (first (vals nexts)) prev (ffirst nexts)]
        (let [new-prev (ffirst nexts)
              new-A    (conj A start)]
          (recur new-A new-prev (apply dissoc (merge-with + nexts (get graph new-prev)) new-A)))))))

(defn contract [graph s t]
  (let [new-v (vec (flatten [s t]))
        ns    (get graph s)
        nt    (get graph t)]
    (as-> graph $
      (dissoc $ s t)
      (assoc $ new-v (dissoc (merge-with + ns nt) s t))
      (reduce (fn [g v] (update g v #(let [w (+ (get % s 0) (get % t 0))] (assoc (dissoc % s t) new-v w))))
              $
              (disj (union (set (keys ns)) (set (keys nt))) s t)))))

(defn stoer-wagner [path]
  (let [input (into {} (u/read-file-list path parse-line))
        graph (->> (reduce (fn [res [orig dests]]
                             (reduce (fn [r dest] (update r dest conj orig)) res dests)) input input)
                   (map (fn [[k v]] [k (apply priority-map-by > (conj (vec (interpose 1 v)) 1))]))
                   (into {}))]
    (loop [g graph]
      (let [[cut cut-phase s t] (min-cut-phase g)]
        (if (= cut-phase 3)
          (let [c (count (flatten (seq cut)))]
            (* c (- (count graph) c)))
          (recur (contract g s t)))))))

(comment
  (time (stoer-wagner path))
  ((transient #{1 2 3}) 1)
  (let [input (into {} (u/read-file-list tpath parse-line))
        graph (->> (reduce (fn [res [orig dests]]
                             (reduce (fn [r dest] (update r dest conj orig)) res dests)) input input)
                   (map (fn [[k v]] [k (apply priority-map-by > (conj (vec (interpose 1 v)) 1))]))
                   (into {}))]
    (min-cut-phase graph))
  )