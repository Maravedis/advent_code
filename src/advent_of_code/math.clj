(ns advent-of-code.math
  (:require [clojure.math :as m]))

(defn strlen
  "The length of the string representation of n in base 10."
  [n] (if (= n 0) 1 (inc (int (m/log10 n)))))

(defn sum-to
  "Sum of the integer from 0 to n."
  [x] (/ (* (+ 1 x) x) 2))

(defn quadratic-roots
  "For a quadratic equation of the form `ax^2 + bx + c = 0`, gives back, if they exist, the possible value of x.
   Only accounts for real numbers."
  [a b c]
  (let [det (- (* b b) (* 4 a c))]
    (when (and (>= det 0) (not= 0 a))
      [(/ (+ (* -1 b) (m/sqrt det)) (* 2 a))
       (/ (- (* -1 b) (m/sqrt det)) (* 2 a))])))

(defn extended-euclidian
  "Calculate the greatest common divisor of `a` and `b`, as well as their Bezout's coefficients.

   Bezout identity: `xa + yb = gcd(a, b)`.

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