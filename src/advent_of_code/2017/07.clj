(ns advent-of-code.2017.07
  (:require [advent-of-code.utils :as u]
            [clojure.string :refer [split]]))

(def path (u/get-input 2017 7))
(def tpath (u/test-path 2017 7))

(def reg #"(\w+) \((\d+)\)(?: -> (.*))?")
(defn parse-line [s]
  (let [m                   (re-matcher reg s)
        [_ name v children] (re-find m)]
    [name [(parse-long v) (some-> children (split #", "))]]))

(defn extract-root [lines]
  (->> (map first lines)
       (remove (set (mapcat #(get-in % [1 1]) lines)))
       first))

(defn part1 [path]
  (extract-root (u/read-file-list path parse-line)))

(defn make-tree-fn [lines]
  (fn internal [root]
    (let [[value children] (lines root)]
      (if (nil? children)
        [root value []]
        (let [cs (mapv internal children)]
          [root (reduce #(+ %1 (second %2)) value cs) cs])))))

(defn find-different [coll]
  (->> (reduce-kv (fn [acc idx v] (update acc v conj idx)) {} coll)
       (filter #(= 1 (count (second %))))
       first second first))

(defn bfs-unbalanced [[name _ children]]
  (let [values (mapv second children)]
    (if (apply = values)
      name
      (bfs-unbalanced (get children (find-different values))))))

(defn part2 [path]
  (let [lines           (->> (u/read-file-list path parse-line)
                             (into {}))
        root            (extract-root lines)
        tree            ((make-tree-fn lines) root)
        unbalanced-node (bfs-unbalanced tree)
        diff            (find-different (mapv second (get tree 2)))
        comp            (if (= diff 0) 1 0)
        x               (- (get-in tree [2 diff 1]) (get-in tree [2 comp 1]))]
    (- (first (lines unbalanced-node)) x)))

(comment

  (part1 tpath)
  (part1 path)
  (part2 tpath)
  (part2 path)

  ;
  )
