(ns advent-of-code.hexgrid)

; Reference : http://devmag.org.za/2013/08/31/geometry-with-hex-coordinates/


;; Points are [x, y, z]
;; x is horizontal axis (y = 0)
;; y is vertical skew right axis (x = 0)
;; z is vertical skew left axis (z = 0)

;; For any coordindate, z = -(x + y), meaning we can write any point as just [x y] if needed.

;; This is for an hexgrid oriented in such a way that there are two direction vertically and one horizontally

(def E [1 0 -1])
(def NE [0 1 -1])
(def NW [-1 1 0])
(def W [-1 0 1])
(def SW [0 -1 1])
(def SE [1 -1 0])
(def origin [0 0 0])

(def neighbors [E NE NW W SW SE])

(defn move
  "Given a starting point and a direction, move in that direction on an hexgrid. If n is supplied, repeat n times."
  ([[x y z :as cur] [a b c :as dir]] [(+ x a) (+ y b) (+ z c)])
  ([[x y z :as cur] [a b c :as dir] n] [(+ x (* n a)) (+ y (* n b)) (+ z (* n c))]))

(defn magnitude
  "Hex-distance to the origin of a point"
  [[x y z]]
  (/ (+ (abs x) (abs y) (abs z)) 2))

(defn hex-distance
  "distance between two points, defined as the smallest number of steps in-between them."
  [v1 v2]
  (abs (- (magnitude v1) (magnitude v2))))