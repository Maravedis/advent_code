(ns advent-of-code.2024.17
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(def >> bit-shift-right)
(def << bit-shift-left) ; (<< 1 (* 3 x)) <=> (8 to the power of x)

(defn run-program [A B C program]
  (loop [A                A
         B                B
         C                C
         [ins lit & rest] program
         output           []]
    (if (nil? ins)
      output
      (let [combo (case lit 4 A 5 B 6 C lit)]
        (case ins
          0 (recur (>> A combo) B C rest output)
          1 (recur A (bit-xor B lit) C rest output)
          2 (recur A (bit-and combo 0x7) C rest output)
          3 (if (not= A 0)
              (recur A B C (drop lit program) output)
              (recur A B C rest output))
          4 (recur A (bit-xor B C) C rest output)
          5 (recur A B C rest (conj output (bit-and combo 0x7)))
          6 (recur A (>> A combo) C rest output)
          7 (recur A B (>> A combo) rest output))))))

(defn part1 [path]
  (let [[[[A] [B] [C]] [program]] (->> (u/read-file-segmented-list path u/nums))]
    (->> (run-program A B C program)
         (str/join ","))))

;; Analysing the input, we can see that it's a simple loop:
;; bst A {store A mod 8 in B}
;; bxl 1 {store B xor 1 in B}
;; cdv B {store A >> B in C}
;; bxl 5 {store B xor 5 in B}
;; bxc   {store B xor C in B}
;; out B {print B mod 8}
;; adv 3 {store A >> 3 in A}
;; jnz 0 {GOTO start}

;; It means the output is constructed by doing operations on (A mod 8), then looping with (A / 8).
;; B and C are overriden each loop.
;; The output is 16 long, so the max A can be is `(8^16) - 1`
;; We iterate on the 8 possible power 8^15, keeping the ones matching the last digit of the program.
;; Then the same on the 8 possible 8^14, keeping the ones matching the last 2 digits of the program.
;; So on and so forth until 8^0. We then keep the minimum of the possible numbers.

;; TLDR: It's a BFS on the decreasing powers of 8 to build the reverse of the output
;; Note: This code will only work on inputs iterating on power of 8s.
(defn part2 [path]
  (let [[[_ [B] [C]] [program]] (->> (u/read-file-segmented-list path u/nums))
        rprogram                (reverse program)]
    (loop [power (- (count program) 1)
           open  #{(- (<< 1 (* 3 (count program))) 1)}]
      (if (< power 0)
        (apply min open)
        (recur (dec power)
               (mapcat #(for [i     (take 8 (iterate (fn [x] (- x (<< 1 (* 3 power)))) %))
                              :when (= (take (- (count program) power) (reverse (run-program i B C program)))
                                       (take (- (count program) power) rprogram))]
                          i) open))))))

(comment
  (def path (u/get-input 2024 17 true))
  (def tpath (u/test-path 2024 17))

  (time (part1 path))
  (time (part2 path)))