(ns advent-of-code.2022.day21
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]
            [clojure.math :refer [signum]]))

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

; I wrote this after looking at solutions. Really didn't think of bruteforce, tried to mvoe the tree around to solve it equation like. DIdn't go over well.
(defn part2 [path]
  (let [tree  (-> (into {} (u/read-file-list path read-monkey))
                  (assoc-in [:root :op] "-"))
        dfs   (fn [dfs tree curr]
                (let [node (tree curr)]
                  (if (int? node)
                    node
                    ((resolve (symbol (:op node))) (dfs tree (:left node)) (dfs tree (:right node))))))
        dfs-m (u/fix (memoize dfs))
        init-sign (signum (dfs-m tree :root))]
    (loop [lo 0
           hi 99999999999999]
      (let [mid      (quot (+ lo hi) 2)
            monkeys  (assoc tree :humn mid)
            root-val (dfs-m monkeys :root)]
        (cond 
          (zero? root-val) mid
          (= (signum root-val) init-sign) (recur (inc mid) hi)
          :else (recur lo mid))))))

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
  (time (part1 "2022/day21")) 
  (time (part2 "2022/day21"))
  (->  (into {} (u/read-file-list "2022/day21" read-monkey))
       (nest :root)
       (assoc :op "=")
       ->infix-string) ; paste this in a fuckin online solver and do not bother inversing a BET.
  )

