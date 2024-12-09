(ns advent-of-code.2024.09
  (:require
   [advent-of-code.utils :as u]))

(defn ->memory [compressed]
  (loop [file    true
         id      0
         memory  (transient [])
         [h & t] compressed]
    (if (nil? h)
      (persistent! memory)
      (if file
        (recur false (inc id) (reduce conj! memory (repeat h id)) t)
        (recur true id (reduce conj! memory (repeat h nil)) t)))))

(defn defragment [filesystem]
  (loop [i   (dec (count filesystem))
         j   0
         res (transient [])]
    (if (< i j)
      (persistent! res)
      (if-let [v (get filesystem j)]
        (recur i (inc j) (conj! res v))
        (recur (loop [i (dec i)] (if (get filesystem i) i (recur (dec i))))
               (inc j) (conj! res (get filesystem i)))))))

(defn next-free-fragment [coll idx size]
  (first (into [] (comp (take-while (fn [[i]] (< i idx)))
                        (filter (fn [[_ bs]] (>= bs size)))
                        (take 1))
               coll)))

(defn consolidate [empty-memory idx size]
  (cond-> (assoc empty-memory idx size)
    (get empty-memory (+ idx size)) (-> (dissoc empty-memory (+ idx size))
                                        (update idx + (get empty-memory (+ idx size))))))

(defn ->maps [frag]
  (reduce (fn [[empty blocks i] h]
            (let [v    (first h)
                  size (count h)]
              (if v
                [empty (assoc blocks i [v size]) (+ i size)]
                [(assoc empty i size) blocks (+ i size)]))) [(sorted-map) (sorted-map) 0] frag))

(defn defragment2 [filesystem]
  (let [frag           (vec (partition-by identity filesystem))
        [empty blocks] (->maps frag)]
    (loop [[[idx [id size]] & t] (reverse blocks)
           empty                 empty
           blocks                blocks]
      (if (nil? idx)
        blocks
        (if-let [[free-idx free-size] (next-free-fragment empty idx size)]
          (recur t
                 (cond-> (dissoc empty free-idx)
                   (not= free-size size)  (consolidate (+ free-idx size) (- free-size size))
                   :always (assoc idx size))
                 (-> (dissoc blocks idx)
                     (assoc free-idx [id size])))
          (recur t empty blocks))))))

(defn part1 [path]
  (let [input  (->> (u/read-file-list path identity)
                    first
                    (re-seq #"\d")
                    (map parse-long))
        memory (->memory input)]
    (->> (defragment memory)
         (reduce-kv #(+ %1 (* %2 %3)) 0))))

(defn part2 [path]
  (let [input  (->> (u/read-file-list path identity)
                    first
                    (re-seq #"\d")
                    (map parse-long))
        memory (->memory input)]
    (->> (defragment2 memory)
         (reduce-kv (fn [acc i [v s]] (reduce (fn [acc idx] (+ acc (* idx v))) acc (range i (+ i s)))) 0))))

(comment
  (def path (u/get-input 2024 9))
  (def tpath "2024/9_test.in")

  (time (part1 path))
  (time (part2 path)))