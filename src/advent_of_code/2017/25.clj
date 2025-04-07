(ns advent-of-code.2017.25
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2017 25))
(def tpath (u/test-path 2017 25))

(defn parse-header [[start-line diag]]
  [(-> (re-seq #"[A-Z]" (subs start-line 1)) first keyword)
   (parse-long (first (re-seq #"\d+" diag)))])

(defn parse-state [[a _ b c d _ e f g]]
  (let [key    (keyword (str (.charAt a 9)))
        write0 (parse-long (str (.charAt b 22)))
        move0  (keyword (subs c 27 28))
        state0 (keyword (str (.charAt d 26)))
        write1 (parse-long (str (.charAt e 22)))
        move1  (keyword (subs f 27 28))
        state1 (keyword (str (.charAt g 26)))]
    [key {0 [write0 move0 state0]
          1 [write1 move1 state1]}]))

(defn part1 [path]
  (let [[header & states] (->> (u/read-file-segmented-list path identity))
        [start end]       (u/tee (parse-header header))
        states            (u/tee (->> (map parse-state states)
                                      (into {})))]
    (loop [state  start
           steps  0
           i      0
           memory (transient {})]
      (if (>= steps end)
        (reduce (fn [acc [_ v]] (+ acc v)) 0 (persistent! memory))
        (let [[w m s] (get-in states [state (get memory i 0)])]
          (recur s
                 (inc steps)
                 (if (= m :r) (inc i) (dec i))
                 (assoc! memory i w)))))))
(comment

  (part1 tpath)
  (part1 path)
  ;
  )
