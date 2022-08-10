(ns dactyl-keyboard.utils
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

(def is-preview false)

(defn rx [radians shape] (rotate radians [1 0 0] shape))
(defn ry [radians shape] (rotate radians [0 1 0] shape))
(defn rz [radians shape] (rotate radians [0 0 1] shape))


(defn rdx [degrees shape] (rx (deg2rad degrees) shape))
(defn rdy [degrees shape] (ry (deg2rad degrees) shape))
(defn rdz [degrees shape] (rz (deg2rad degrees) shape))


(defn rd [x y z shape] (->> shape
                            (rdx x)
                            (rdy y)
                            (rdz z)))

(defn rcylinder 
  ([radius height] (rcylinder radius height 20))
  ([radius height fn-val]
  (if is-preview
    (cylinder radius height)
    (->>
     (hull
      (translate [0 0 (- (/ height 2) (/ radius 2))] (sphere (/ radius 2)))
      (translate [0 0 (+ (/ height -2) (/ radius 2))] (sphere (/ radius 2))))
     (with-fn fn-val)))))

(defn add-vec  [& args]
  "Add two or more vectors together"
  (when  (seq args)
    (apply mapv + args)))

(defn sub-vec  [& args]
  "Subtract two or more vectors together"
  (when  (seq args)
    (apply mapv - args)))

(defn div-vec  [& args]
  "Divide two or more vectors together"
  (when  (seq args)
    (apply mapv / args)))

(defn fillet-circle [radius]
  (let [diameter (* radius 2)]
    (binding [*fn* 36] (extrude-rotate {:convexity 10} (->>
                                                        (difference
                                                         (square diameter diameter :center true)
                                                         (translate [radius radius 0] (binding [*fn* 36] (circle radius))))
                                                        (intersection (square diameter diameter :center false))
                                                        (translate [radius 0 0]))))))

(defn standoff [inner-radius outer-radius height]
  (difference
   (union
    (fillet-circle outer-radius)
    (binding [*fn* 36] (cylinder outer-radius height :center false)))
   (->>
    (binding [*fn* 36] (cylinder inner-radius (+ 0.2 height) :center false))
    (translate [0 0 -0.1]))))

(def quarterrounnd
  (difference
   (translate [-1 -1 0] (square 4 4 :center false))
   (translate [-1 -1 0] (binding [*fn* 16] (circle 3)))))


(defn offset-delta
  "A broad implementation of OpenSCAD’s offset(), supporting more parameters."
  [{:keys [r delta chamfer]} & block]
  `(:offset-delta {:r ~r :delta ~delta :chamfer ~chamfer} ~@block))

(defmethod write-expr :offset-delta
  [depth [form {:keys [r delta chamfer] :or {chamfer false}} & block]]
  (concat
   (list (indent depth) "offset (")
   (if r
     (list "r = " r)
     (list "delta = " delta))
   (when chamfer (list ", chamfer=true"))
   (list ") {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defn multmatrix-x-rot [theta]
  [[1, 0,                0,                   0]
   [0, (Math/cos theta), (- (Math/sin theta)), 0]
   [0, (Math/sin theta), (Math/cos theta),     0]
   [0, 0,                0,                   1]])

(defn multmatrix-y-rot [theta]
  [[(Math/cos theta),     0, (Math/sin theta), 0]
   [0,                   1, 0,                0]
   [(- (Math/sin theta)), 0, (Math/cos theta), 0]
   [0,                    0, 0,                1]])


(defn multmatrix-z-rot [theta]
  [[(Math/cos theta), (- (Math/sin theta)), 0, 0]
   [(Math/sin theta),  (Math/cos theta),    0, 0]
   [0,                0,                  1, 0]
   [0,                0,                  0, 1]])

(defn multmatrix-translate [[x y z]]
  [[1, 0, 0, x]
   [0, 1, 0, y]
   [0, 0, 1, z]
   [0, 0, 0, 1]])
(defn dot
  "Vector dot product"
  [x y]
  (reduce + (map * x y)))

(defn transpose
  "Transposes matrix"
  [a]
  (apply map vector a))

(defn mat-mult
  "Matrix-Matrix product"
  [a b]
  {:pre [(= (count (nth a 0)) (count b))]}
  (vec
   (->> (for [x a
              y (transpose b)]
          (dot x y))
        (partition (count (transpose b)))
        (map vec))))

(defn bezier-fn [a b curve-fs]

  (Math/round
   (/
    (Math/sqrt (+ (Math/pow (- (first a) (first b)) 2)  (Math/pow (- (second a) (second b)) 2)))
    curve-fs)))

(defn bezier-points [points n idx]
  (let [len (count points)] (cond
                              (> len 2) (map + (map #(* % (* n idx))  (bezier-points
                                                                       (for [i (range 0 (- len 1))] (nth points i)),n, idx))
                                             (map #(* % (- 1 (* n idx)))  (bezier-points
                                                                           (for [i (range 1  len)] (nth points i)),n, idx)))
                              :else

                              (map +  (map #(* % (* n idx)) (first points)) (map #(* % (- 1 (* n idx))) (second points))))))

(defn bezier-curve [points, curve-fs]
  (let [curve-fn (bezier-fn (first points) (nth points (- (count points) 1)) curve-fs)]
    (for [i (range 0 (+ curve-fn 1))]
      (concat (bezier-points points (/ 1 curve-fn) i)))))


(defn bezier-polyhedron-curved-faces [listLength1, listLength2]
  (let [last1 (- listLength1 2) last2 (+ last1 listLength2)]
    (for [a (range 0 (- listLength1 2))]
      (cond (<= (- (+ last2 1) a) (listLength1))
            (conj [(- last1 a) (- (+ last1 1) a) listLength1])
            :else
            (conj [(- last1 - a) (+ (- last1 a) 1) ((+ (- last1 a) 1))])))))
(defn bezier-polyhedron-faces [listLength1, listLength2]
  (let [curved-faces (for [a (range 0 (- listLength1 2))]
                       ())]))


(defn add-third-dimension [point]
  (into [] (concat point [0])))

(defn vec-to-matrix [point]
  (into [] (concat point [1])))

(defn apply-matrix [matrix points]
  (let [tdim (- (count (nth matrix 0)) 1)
        data-dim (count (nth points 0))
        scale (cond (count matrix) 1 :else (nth (nth matrix 0) 0))]))


(defn join-bezier-sides [points]
  (for [i (range 0 (- (count points) 3))]
    (hull
     (last points)
     (nth points i)
     (nth points (+ i 1)))))

(defn join-beziers [bezier-1 bezier-2]
  (let [bezier-larger (cond (>= (count bezier-1) (count bezier-2)) bezier-1 :else bezier-2)
        bezier-smaller (cond (identical? bezier-1 bezier-larger) bezier-2 :else bezier-1)
        penultimate-l (- (count bezier-larger) 2) penultimate-s (- (count bezier-smaller) 2)]
    (union
     (hull
      (first bezier-larger)
      (last bezier-larger)
      (first bezier-smaller)
      (last bezier-smaller))
     (for [i (range 0 penultimate-l)]
       (cond  (< penultimate-s i)

              (hull
               (first bezier-smaller)
               (nth bezier-larger (- penultimate-l i))
               (nth bezier-larger (+ (- penultimate-l i) 1)))

              :else (hull
                     (nth bezier-larger (- penultimate-l i))
                     (nth bezier-larger (+ (- penultimate-l i) 1))
                     (nth bezier-smaller (- penultimate-s i))
                     (nth bezier-smaller (+ (- penultimate-s i) 1)))))
     (join-bezier-sides bezier-larger)
     (join-bezier-sides bezier-smaller))))


(defn bezier-cubic-point [c0 c1 c2 c3 t]
  (let [b0 (Math/pow (- 1 t) 3)
        b1 (* (Math/pow (- 1 t) 2) 3 t)
        b2 (* 3 (- 1 t) (Math/pow t 2))
        b3 (Math/pow t 3)]
    (+ (* b0 c0) (* b1 c1) (* b2 c2) (* b3 c3))))

(defn bezier-cubic [p0 p1 p2 p3 steps]
  (for [t (range 0 steps)]
    (into [] (concat [(bezier-cubic-point (first p0) (first p1) (first p2) (first p3) (/ t steps))
                      (bezier-cubic-point (second p0) (second p1) (second p2) (second p3) (/ t steps))
                      (bezier-cubic-point (nth p0 2) (nth p1 2) (nth p2 2) (nth p3 2) (/ t steps))]))))

(defn bezier-quadratic-point [c0 c1 c2 t]
  (let [b0 (Math/pow (- 1 t) 2)
        b1 (* 2 t (- 1 t))
        b2 (Math/pow t 2)]
     (+ (* b0 c0) (* b1 c1) (* b2 c2))
    )
  )

(defn bezier-quadratic [p0 p1 p2 steps]
  (for [t (range 0 steps)]
    (into [] (concat [(bezier-quadratic-point (first p0) (first p1) (first p2)  (/ t steps))
                      (bezier-quadratic-point (second p0) (second p1) (second p2)  (/ t steps))
                      (bezier-quadratic-point (nth p0 2) (nth p1 2) (nth p2 2)  (/ t steps))]))))

(defn bezier-quadratic-translate [p0 p1 p2 steps shape]
  (for [t (range 0 steps)]
    (into [] (concat (translate [(bezier-quadratic-point (first p0) (first p1) (first p2)  (/ t steps))
                      (bezier-quadratic-point (second p0) (second p1) (second p2)  (/ t steps))
                      (bezier-quadratic-point (nth p0 2) (nth p1 2) (nth p2 2)  (/ t steps))]
                                shape)))))


(defn plot-list-of-shapes [list-of-shapes]
  (for [i (range 0 (- (count list-of-shapes) 2))]
    (concat (hull
     (nth list-of-shapes i)
     (nth list-of-shapes (+ i 1))
     ))
    )
  )

(defn plot-list-of-shapes-and-join-first-and-last [list-of-shapes]
  (concat (plot-list-of-shapes 
           (hull (first list-of-shapes) (nth list-of-shapes (- (count list-of-shapes) 1)))))
  )