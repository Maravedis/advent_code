(ns advent-of-code.2017.05
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2017 5))

(defn run [path part2?]
  (let [input (u/read-file-list path parse-long)
        n     (count input)]
    (loop [program (vec input)
           ip      0
           steps   0]
      (if (< -1 ip n)
        (recur (update program ip (if (and part2? (>= (program ip) 3)) dec inc)) (+ ip (program ip)) (inc steps))
        steps))))

(comment

  (run path false)
  (run path true)
  ;
  )
