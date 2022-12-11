(ns advent-of-code.day11
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split split-lines trim]]))

(defn parse-op [[l op r]]
  (fn [x]  ((resolve (symbol (str op "'")))
            (if (= l "old") x (read-string l))
            (if (= r "old") x (read-string r)))))


(defn parse-spec [monkey [op desc]]
  (case op
    "Starting items" (assoc monkey :items (->> (split desc #", ") (mapv read-string)))
    "Operation" (assoc monkey :inspect (parse-op (->> (split desc #" ") (drop 2)))) 
    "Test" (assoc monkey :divisible-by (->> (split desc #" ") peek read-string))
    "If true" (assoc monkey :if-true (->> (split desc #" ") peek read-string))
    "If false" (assoc monkey :if-false (->> (split desc #" ") peek read-string))))

(defn read-monkey [bloc]
  (assoc (->> (map trim bloc)
              (map #(split % #": "))
              (reduce parse-spec {}))
         :inspected 0))

(defn read-starting-position [resource div3?]
  (let [blocks (-> (io/resource resource)
                    slurp
                    (split #"\n\n"))
        monkeys (reduce-kv (fn [acc i monkey]
                             (let [block (drop 1 (split-lines monkey))]
                               (assoc acc i (read-monkey block))))
                           {}
                           blocks)
        supermod (apply *' (map :divisible-by (vals monkeys)))]
    (->> monkeys
         (map (fn [[k v]] [k (cond-> v
                               div3? (update :inspect (fn [f] (comp #(long (/ % 3)) f))) 
                               (not div3?) (update :inspect (fn [f] (comp #(mod % supermod) f)))
                               true (assoc :test (fn [x] (= 0 (mod x (:divisible-by v))))))]))
         (into {}))
    
    ))

(defn monkey-plays [monkeys n]
  (let [{:keys [items inspect test if-true if-false]
         :as   monkey} (get monkeys n)]
    (-> (reduce (fn [result item]
                  (update-in result [(if (test item) if-true if-false) :items] conj item))
                monkeys
                (map inspect items))
        (assoc-in [n :items] [])
        (update-in [n :inspected] #(+ % (count items))))))

(defn play [resource rounds div3?]
  (let [monkeys    (read-starting-position resource div3?)
        r          (range (count monkeys))]
    (->> (iterate #(reduce monkey-plays % r) monkeys)
         (drop rounds)
         first)))

(defn count-inspected [monkeys]
  (->> (map :inspected (vals monkeys))
       (sort >)
       (take 2)
       (apply *)))

(comment
  (count-inspected (play "day11" 20 true))
  (count-inspected (play "day11" 10000 false)))