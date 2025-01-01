(ns advent-of-code.2016.15
  (:require [advent-of-code.utils :as u]
            [advent-of-code.math :refer [chinese-remainders]]))

(def path (u/get-input 2016 15))
(def tpath (u/test-path 2016 15))

(defn solve [path part2?]
  (let [congruences (->> (u/read-file-list path u/nums)
                         (map (fn [[i modulo _ start-position]]
                                [(mod (* -1 (+ i start-position)) modulo) modulo])))]
    (cond-> congruences
      part2? (concat [[-7 11]])
      :always (chinese-remainders))))

(comment (solve path false)
         (solve path true))

