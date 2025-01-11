(ns advent-of-code.2017.04
  (:require [advent-of-code.utils :as u]
            [clojure.string :refer [split]]))

(def path (u/get-input 2017 4))

(defn part1 [path]
  (->> (u/read-file-list path #(split % #" "))
       (u/count-when (fn [passphrase] (->> (frequencies passphrase)
                                           (every? (fn [[_ x]] (= x 1))))))))

(defn part2 [path]
  (->> (u/read-file-list path #(split % #" "))
       (u/count-when #(reduce (fn [seen word]
                                (let [word-set (set word)]
                                  (if (seen word-set) (reduced false) (conj! seen word-set))))
                              (transient #{}) %))))

(comment
  (part1 path)
  (part2 path))

  ;

