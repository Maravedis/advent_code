(ns advent-of-code.math
  (:require [clojure.math :as m]))

(defn strlen [n] (if (= n 0) 1 (inc (int (m/log10 n)))))

(defn extended-euclidian
  "Calculate the greatest common divisor of `a` and `b`, as well as their Bezout's coefficients.\n
   Bezout identity: `xa + yb = gcd(a, b)`.\n
   Result is in the form `[gcd x y]`."
  [a b]
  (loop [s   0
         s-1 1
         r   b
         r-1 a]
    (if (= r 0)
      [r-1 s-1 (if (= b 0) 0 (quot (- r-1 (* s-1 a)) b))]
      (let [quotient (quot r-1 r)]
        (recur (- s-1 (* quotient s)) s (- r-1 (* quotient r)) r)))))

(defn ^:private solve-2-crt [[a1 m1] [a2 m2]]
  (let [[_ n1 n2] (extended-euclidian m1 m2)]
    [(mod (+ (* a1 n2 m2) (* a2 n1 m1)) (* m1 m2)) (* m1 m2)]))

(defn chinese-remainders
  "For a list of congruences `[a m]`, `x = a (mod m)`, returns a solution x if all the `m` are pairwise coprimes."
  [congruences]
  (->> (reduce solve-2-crt congruences)
       first))