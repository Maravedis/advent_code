(ns advent-of-code.day10
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as io]))

(defn read-instruction [line]
  (let [[h t] (str/split line #" ")]
    (if t
      [h (read-string t)]
      [h])))


(defn calc-signal [resource] 
  (loop [[[code i] & r :as ins]  (u/read-file-list resource read-instruction)
         x                       1
         cycle                   1
         addx?                   false
         signal                  0
         [check & rc :as checks] [20 60 100 140 180 220]]
    (if (or (nil? check) (nil? code))
      signal
      (let [s (cond-> signal (= 0 (mod cycle check)) (+ (* x cycle)))
            c (if (= 0 (mod cycle check)) rc checks)]
        (match [code addx?]
          ["addx" true] (recur r (+ x i) (inc cycle) false s c)
          ["addx" false] (recur ins x (inc cycle) true s c)
          ["noop" false] (recur r x (inc cycle) false s c)
          :else (throw (ex-info "Shouldn't have noop with true" {})))))))

(defn draw-signal [resource]
  (with-open [o     (io/writer (str resource "_out"))]
    (loop [[[code i] & r :as ins] (u/read-file-list resource read-instruction)
           x                      1
           cycle                  1
           addx?                  false]
      (if (< 240 cycle)
        :ok
        (do
          (if (<= (- x 1)  (mod (- cycle 1) 40) (+ x 1))
            (.write o "#")
            (.write o "."))
          (when (= 0 (mod cycle 40))
            (.write o "\n"))
          (match [code addx?]
            ["addx" true] (recur r (+ x i) (inc cycle) false)
            ["addx" false] (recur ins x (inc cycle) true)
            ["noop" false] (recur r x (inc cycle) false)
            :else (throw (ex-info "Shouldn't have noop with true" {}))))))))

  (comment
    (calc-signal "day10")
    (draw-signal "day10_test")
    (draw-signal "day10") ; => PGHFGLUG
)
