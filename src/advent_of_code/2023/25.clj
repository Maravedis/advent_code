(ns advent-of-code.2023.25
  (:require [advent-of-code.utils :as u]
            [clojure.set :refer [union difference]]
            [clojure.data.priority-map :refer [priority-map-by]])
  (:import [org.jgrapht.alg StoerWagnerMinimumCut]
           [org.jgrapht.graph DefaultEdge SimpleGraph]))

(def path (u/get-input 2023 25))
(def tpath "2023/25_test.in")

(defn parse-line [line]
  (let [[orig & dests] (re-seq #"\w+" line)]
    [(keyword orig) (map keyword dests)]))

(defn mk-graph [connected]
  (let [graph (SimpleGraph. DefaultEdge)]
    (doseq [v (keys connected)]
      (.addVertex graph v))
    (doseq [[v dests] connected
            dest      dests]
      (.addEdge graph v dest))
    graph))

; Just use an external library 4head
(defn part1-for-real [path]
  (let [input     (into {} (u/read-file-list path parse-line))
        connected (reduce (fn [res [orig dests]]
                            (reduce (fn [r dest] (update r dest (fnil conj #{}) orig)) res dests)) input input)
        graph     (mk-graph connected)
        min-cut   (.minCut (StoerWagnerMinimumCut. graph))]
    (* (count min-cut) (- (count (keys connected)) (count min-cut)))))

(defn contract [graph v1 v2 e1 e2]
  (let [new-v (vec (flatten [v1 v2]))]
    (as-> graph $
      (dissoc $ v1 v2)
      (assoc $ new-v (remove #{v1 v2} (concat e1 e2)))
      (reduce (fn [g v] (if (#{v1 v2} v) g (update g v #(-> (remove #{v1 v2} %) (conj new-v))))) $ e1)
      (reduce (fn [g v] (if (#{v1 v2} v) g (update g v #(-> (remove #{v1 v2} %) (conj new-v))))) $ e2))))


; Works, but it's  a probabilistic approach. Was my first try. And strangely it works less often than it should. Might have blundered.
(defn karger [connected]
  (loop [graph connected]
    (if (= 2 (count graph)) 
      (apply * (map #(if (coll? %) (count %) 1) (keys graph)))
      (let  [[v1 e1] (rand-nth (seq graph))
             v2      (rand-nth e1)
             e2      (get graph v2)]
        (recur (contract graph v1 v2 e1 e2))))))

(defn min-cut-phase [graph]
  (let [glen (count graph)
        gkey (keys graph)]
    (loop [A     #{(rand-nth gkey)}
           nexts (get graph (first A))
           steps 0] 
      (if (= (count A) (- glen 2))
        (difference (set gkey) A)
        (let [new-A (conj A (ffirst nexts))]
          (recur new-A (apply dissoc (merge-with + nexts (get graph (ffirst nexts))) new-A ) (inc steps)))))))


(defn stoer-wagner [path]
  (let [input     (into {} (u/read-file-list path parse-line))
        graph (u/tee "out.edn" (->> (reduce (fn [res [orig dests]]
                                              (reduce (fn [r dest] (update r dest conj orig)) res dests)) input input)
                                    (map (fn [[k v]] [k (apply priority-map-by > (conj (vec (interpose 1 v)) 1))]))
                                    (into {})))]
    (time (min-cut-phase graph))))

(defn part1 [path]
  (let [input (into {} (u/read-file-list path parse-line))
        connected
        (reduce (fn [res [orig dests]]
                  (reduce (fn [r dest] (update r dest conj  orig)) res dests)) input input)]
    (karger connected)))


(comment
  (stoer-wagner tpath)

  (let [input (u/read-file-list tpath parse-line)]
    (doseq [[orig dests] input
            dest         dests]
      (println (name orig) " -> " (name dest) ";")))
  (interpose 1 [:a :b :c :d])

  (part1 path)
  (part1-for-real path)
  )