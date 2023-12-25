(ns advent-of-code.2023.25
  (:require [advent-of-code.utils :as u])
  (:import [org.jgrapht.alg StoerWagnerMinimumCut]
           [org.jgrapht.graph DefaultEdge SimpleGraph]))

(def path (u/get-input 2023 25))
(def tpath "2023/25_test.in")

(defn parse-line [line]
  (let [[orig & dests] (re-seq #"\w+" line)]
    [(keyword orig) (set (map keyword dests))]))

(defn mk-graph [connected]
  (let [graph (SimpleGraph. DefaultEdge)]
    (doseq [v (keys connected)]
      (.addVertex graph v))
    (doseq [[v dests] connected
            dest      dests]
      (.addEdge graph v dest))
    graph))

(defn part1 [path]
  (let [input     (into {} (u/read-file-list path parse-line))
        connected (reduce (fn [res [orig dests]]
                            (reduce (fn [r dest] (update r dest (fnil conj #{}) orig)) res dests)) input input)
        graph     (mk-graph connected)
        min-cut   (.minCut (StoerWagnerMinimumCut. graph))]
    (* (count min-cut) (- (count (keys connected)) (count min-cut)))))

(comment
  (part1 path)
  )