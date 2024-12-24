(ns advent-of-code.2024.24
  (:require [advent-of-code.utils :as u]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(def path (u/get-input 2024 24))
(def tpath (u/test-path 2024 24))

(defn ->wire [pre i] (format "%s%02d" pre i))

(defn parse-wire [lines]
  (->> (map #(str/split % #": ") lines)
       (map (fn [[name value]] [name (parse-long value)]))
       (into {})))

(defn parse-instruction [instruction]
  (-> (str/split instruction #" (-> )?")
      (match [a "AND" b out] [0 a b out]
             [a "XOR" b out] [1 a b out]
             [a "OR" b out]  [2 a b out])))

(defn ->binary-number [label states]
  (-> (for [i (range 50 -1 -1)]
        (states (->wire label i)))
      (str/join)
      (Long/parseLong 2)))

(defn run [instructions wires]
  (loop [[[op a b out] & t] instructions
         state              wires]
    (if (nil? op)
      state
      (case op
        0 (recur t (cond-> state (and (wires a) (wires b)) (assoc out (bit-and (wires a) (wires b)))))
        1 (recur t (cond-> state (and (wires a) (wires b)) (assoc out (bit-xor (wires a) (wires b)))))
        2 (recur t (cond-> state (and (wires a) (wires b)) (assoc out (bit-or  (wires a) (wires b)))))))))

(defn ->output [wires instructions]
  (let [n   (inc (/ (count wires) 2))
        zns (for [i (range n)] (->wire "z" i))]
    (->> (drop-while #(not-every? % zns) (iterate #(run instructions %) wires))
         (first)
         (->binary-number "z"))))

(defn part1 [path]
  (let [[wires ins]  (u/read-file-segmented-list path identity)
        wires        (parse-wire wires)
        instructions (map parse-instruction ins)]
    (->output wires instructions)))

;; Reverse engineering the output, we see only two kinds of errors.
;; For each bit N, we have either :
;; 1. We don't output in the correct gate (i.e. zN is swapped with something else)
;; 2. The AND-gate and the XOR-gate are swapped on the inputs.
;; This might be particular to my input, but I suspect not,
;; because I don't think the whole thing could keep its cascading structure with other swaps.
(defn detect-swap [instructions i]
  (let [[xn yn zn]      (for [pre ["x" "y" "z"]] (->wire pre i))
        and-ins         (some (fn [[op a b out]] (and (= op 0) (#{xn yn} a) (#{xn yn} b) out)) instructions)
        xor-ins         (some (fn [[op a b out]] (and (= op 1) (#{xn yn} a) (#{xn yn} b) out)) instructions)
        sum             (some (fn [[op a b out]] (and (= op 1) (or (= a xor-ins) (= b xor-ins)) out)) instructions)
        and-xor-ins-cin (some (fn [[op a b out]] (and (= op 0) (or (= a xor-ins) (= b xor-ins)) out)) instructions)
        cout            (some (fn [[op a b out]] (and (= op 2) (every? #{and-ins and-xor-ins-cin} [a b]) out)) instructions)]
    (cond (and sum (not= sum zn)) [sum zn]
          (not (or sum and-xor-ins-cin cout)) [and-ins xor-ins])))

(defn swap [x y instruction]
  (map (fn [[op a b out]]
         (cond (= x out) [op a b y]
               (= y out) [op a b x]
               :else [op a b out])) instruction))

(defn part2 [path]
  (let [[wires ins]  (u/read-file-segmented-list path identity)
        wires        (parse-wire wires)
        instructions (map parse-instruction ins)
        swaps        (keep #(detect-swap instructions %) (range 1 (/ (count wires) 2))) ; pray the error is not on bit 0 or n.
        ins-swapped  (reduce (fn [acc [x y]] (swap x y acc)) instructions swaps)]
    (assert (= (+ (->binary-number "x" wires) (->binary-number "y" wires)) (->output wires ins-swapped)))
    (->> (flatten swaps)
         sort
         (str/join ","))))

(comment
  (time (part1 path))
  (time (part2 path)))
