(ns advent-of-code.2016.09
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(def path (u/get-input 2016 9))
(def tpath (u/test-path 2016 9))

(defn decompress-length [file v2?]
  (if (empty? file)
    0
    (if-let [[marker nexts times] (re-find #"^\((\d+)x(\d+)\)" file)]
      (let [nexts     (parse-long nexts)
            times     (parse-long times)
            to-repeat (subs file (count marker) (+ (count marker) nexts))]
        (+ (if (and v2? (str/includes? to-repeat "x"))
             (* times (decompress-length to-repeat v2?))
             (* times nexts))
           (decompress-length (subs file (+ (count marker) nexts)) v2?)))
      (+ 1 (decompress-length (subs file 1) v2?)))))

(defn solve [path v2?]
  (-> (u/read-file-line path)
      (decompress-length v2?)))

(comment
  (solve path false)
  (solve path true))
