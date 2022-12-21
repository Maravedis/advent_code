(ns advent-of-code.day21
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(defn read-monkey [line]
  (let [arr (str/split line #"[:]? ")]
    (if-let [op (get arr 2)]
      [(keyword (get arr 0)) {:left  (keyword (get arr 1))
                              :op    op
                              :right (keyword (get arr 3))}]
      [(keyword (get arr 0)) (read-string (get arr 1))])))

(defn part1 [path]
  (let [tree  (->> (u/read-file-list path read-monkey)
                   (into {}))
        dfs   (fn [dfs curr]
                (let [node (tree curr)]
                  (if (int? node)
                    node
                    ((resolve (symbol (:op node))) (dfs (:left node)) (dfs (:right node))))))
        dfs-m (u/fix (memoize dfs))]
    (dfs-m :root)))

(defn nest [graph curr]
  (cond
    (= curr :humn) (symbol "x")
    (int? (graph curr)) (graph curr)
    :else (let [{:keys [op left right]} (graph curr)]
            {:op    op
             :left  (nest graph left)
             :right (nest graph right)})))

(defn ->infix-string [g]
  (if (map? g)
    (str "(" (->infix-string (:left g)) " " (:op g) " " (->infix-string (:right g)) ")")
    (str g)))


(comment
  (time (part1 "day21")) 
  (->  (into {} (u/read-file-list "day21" read-monkey))
       (nest :root)
       (assoc :op "=")
       ->infix-string) ; paste this in a fuckin online solver and do not bother inversing a BET.
  )

