(ns advent-of-code.2023.22
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2023 22))

(defn parse-brick [line]
  (let [[[x1 y1 z1] [x2 y2 z2]] (split-at 3 (u/nums line))
        [x1 x2] ((juxt min max) x1 x2)
        [y1 y2] ((juxt min max) y1 y2)
        [z1 z2] ((juxt min max) z1 z2)]
    (apply sorted-set (for [x (range x1 (inc x2))
                             y (range y1 (inc y2))
                             z (range z1 (inc z2))]
                         [x y z]))))

(defn fall [stack idx brick]
  (loop [brick brick]
    (let [[[_ _ z] :as b] (map #(update % 2 dec) brick)]
         (if-let [prevs (or (= z 0) (not-empty (filter (fn [[_ v]] (some (:brick v) (set b))) stack)))]
           (assoc stack idx {:prevs  (if (boolean? prevs) nil (set (map first prevs)))
                             :brick (apply sorted-set brick)})
           (recur b)))))

; Caching this because it takes forever to parse the graph (well, it takes 2 minutes. But that's too long).
; I wish we had easily usable mutable structures in clojure :(
(def cached-tree
  (delay
    (let [in       (->> (vec (u/read-file-list path parse-brick))
                        (sort-by #(-> % first (get 2)))
                        vec)
          stack    (->> in
                        (reduce-kv fall {})
                        (into (sorted-map)))]
      (into (sorted-map)
            (persistent! (reduce-kv (fn [res idx e]
                                      (conj! res [idx
                                                  (assoc (dissoc e :brick)
                                                         :nexts
                                                         (set (keep (fn [[k {:keys [prevs]}]]
                                                                      (when (and prevs (prevs idx)) k)) stack)))]))
                                    (transient {})
                                    stack))))))

(defn part1 []
  (->> (set (keys @cached-tree))
       (remove (fn [brick] (some (fn [s] (and (= 1 (count s)) (s brick))) (map :prevs (vals @cached-tree)))))
       (count)))

(defn count-falling [tree idx]
  (loop [currs #{idx}
         tree  tree
         nb 0]
    (if (empty? currs)
      nb
      (let [next-tree  (reduce (fn [t p]
                                 (reduce (fn [t1 n] (update-in t1 [n :prevs] disj p)) t (get-in tree [p :nexts])))
                               tree currs)
            next-currs (->> currs
                            (mapcat #(get-in next-tree [% :nexts]))
                            (filter #(empty? (get-in next-tree [% :prevs])))
                            set)]
        (recur next-currs
               next-tree
               (+ nb (count next-currs)))))))

(defn part2 []
  (reduce (fn [r t] (+ r (count-falling @cached-tree t))) 0 (keys @cached-tree)))

(comment
  (time (part1))
  (time (part2))
  )