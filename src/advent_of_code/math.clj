(ns advent-of-code.math
  (:require [clojure.math :as m]))

(defn strlen [n] (if (= n 0) 1 (inc (int (m/log10 n)))))
