(ns dactyl-keyboard.utils
 (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [ mmul set-current-implementation mul mget]]
            [clojure.math :refer [ceil]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            ;[chisel.curves :as chisel-curves :refer [direct-nurbs-evaluation  b-spline clamped-b-spline]]
            ;[chisel.protocols :as chisel-protocols :refer [PParametricCurve]]
            ;[sicmutils.env :as e :refer :all] 
            ;[uncomplicate.neanderthal.linalg :refer [det]] 
            ))

;(set-current-implementation :vectorz)

(def π Math/PI)

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

(defn rad2deg [radians]
  (/ (* radians 180) pi))

(def is-preview false)

(def sphere-preview-fn-value 8)
(def sphere-render-fn-value 36)
(def sphere-fn-value (if is-preview sphere-preview-fn-value sphere-render-fn-value))

(def cylinder-preview-fn-value 8)
(def cylinder-render-fn-value 36)
(def cylinder-fn-value (if is-preview cylinder-preview-fn-value cylinder-render-fn-value))

(def circle-bezier-approx-a 1.00005519)
(def circle-bezier-approx-b 0.55342686)
(def circle-bezier-approx-c 0.99873585)

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

;(defn abs [n] (max n (- n)))

(defn vec-drop-by-index [vector index]
  (concat (subvec vector 0 index)
          (subvec vector (inc index))) 
  )

(defn round-to-precision
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn factorial
  ([n]                    ; when only one argument is passed in
   (factorial n 1))
  ([n acc]                ; when two arguments are passed in
   (if  (= n 0)  acc
        (recur (dec n) (* acc n)))))

(defn transpose
  "Transposes matrix"
  [a]
  (apply map vector a))

;from https://github.com/pcasaretto/clj-ray-tracer/blob/main/src/clj_ray_tracer/matrix.clj

(defn element-apply [m f]
  (mapv #(mapv f %) m))

(defn matrix-width [m]
  (count (get m 0)))

(defn matrix-height [m]
  (count m))

(defn transpose-external [m]
  (vec
   (for [i (range 0 (matrix-width m))]
     (vec
      (for [j (range 0 (matrix-height m))]
        (get-in m [j i]))))))

(defn submatrix [m exclude-row exclude-column]
  (vec
   (keep-indexed
    (fn [current-row row]
      (if (not= current-row exclude-row)
        (vec
         (keep-indexed (fn [current-column column] (if (not= current-column exclude-column) column)) row))))
    m)))

(declare determinant)
(defn minor [m exclude-row exclude-column]
  (-> m
      (submatrix exclude-row exclude-column)
      (determinant)))

(defn- diagonal?
  [row column]
  (even? (+ row column)))

(defn cofactor
  [m row column]
  (let
   [minor (minor m row column)]
    (if (diagonal? row column) minor (- minor))))

(defn determinant2 [m]
  (-
   (* (get-in m [0 0]) (get-in m [1 1]))
   (* (get-in m [1 0]) (get-in m [0 1]))))

(defn determinant3 [m]
  (->>
   (range (matrix-height m))
   (map #(clojure.core/* (get-in m [0 %]) (cofactor m 0 %)))
   (reduce +)))

(defn determinant [m]
  (cond
    (= (matrix-width m) 2) (determinant2 m)
    (>= (matrix-width m) 3) (determinant3 m)))


(defn cofactor-matrix [m]
  (let [size (matrix-height m)]
    (vec
     (for [i (range 0 size)]
       (vec
        (for [j (range 0 size)]
          (cofactor m i j)))))))

(defn matrix-inverse [m]
  (let
   [matrix-bigdec (element-apply m bigdec)
    det (determinant matrix-bigdec)]
    (-> matrix-bigdec
        cofactor-matrix
        transpose-external
        (element-apply #(with-precision 8 :rounding	DOWN (/ % det))))))

;end of code from https://github.com/pcasaretto/clj-ray-tracer/blob/main/src/clj_ray_tracer/matrix.clj

(defn project-coordinate [coordinate]
  (mul (/ 1 (mget coordinate 3)) coordinate)
  )

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

(defn multiple-standoffs [inner-radius outer-radius height positions]
  (let [original-standoff (standoff inner-radius outer-radius height)]
    (for [position positions]
      (translate position original-standoff))))

(defn vec-if-not [collection]
  (if (vector? collection) collection (vec collection)))


(defn drop-nth [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))
(defn drop-by-indexes [indexes coll]
  (keep-indexed #(if (false? (some (fn [index] %1 ) indexes)) %2) coll))

(def quarterrounnd
  (difference
   (translate [-1 -1 0] (square 4 4 :center false))
   (translate [-1 -1 0] (binding [*fn* 16] (circle 3)))))


;from https://github.com/dereknheiley/compactyl/blob/master/src/dactyl_keyboard/dactyl.clj
(def WHI [255/255 255/255 255/255 1])
(def RED [255/255 0/255 0/255 1])
(def ORA [220/255 128/255 0/255 1])
(def YEL [220/255 255/255 0/255 1])
(def GRE [0/255 255/255 0/255 1])
(def DGR [21/255 71/255 52/255 1])
(def CYA [0/255 255/255 255/255 1])
(def BLU [0/255 128/255 255/255 1])
(def NBL [0/255 0/255 255/255 1])
(def PUR [127/255 0/255 255/255 1])
(def PIN [255/255 0/255 255/255 1])
(def MAG [255/255 0/255 127/255 1])
(def BRO [102/255 51/255 0/255 1])
(def BLA [0/255 0/255 0/255 1])
(def GRY [128/255 128/255 128/255 1])
(def SLT [112/255 128/255 144/255 1])

(def D_BLU [0/255 128/255 255/255 0.5])
(def D_RED [255/255 0/255 0/255 0.5])
(def D_PUR [127/255 0/255 255/255 0.75])
(def D_GRE [4/255 106/255 56/255 0.5])
(def D_BLA [112/255 128/255 144/255 0.85])


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

(defn plot-bezier-points [bezier-points shape]
  (mapv (fn [point]
          (translate point shape))
        bezier-points))

(defn vector-distance [v1 v2]
  (Math/sqrt (+ (Math/pow (- (nth v1 0) (nth v2 0)) 2) (Math/pow (- (nth v1 1) (nth v2 1)) 2) (Math/pow (- (nth v1 2) (nth v2 2)) 2)))
  )

(defn lerp-old [a b t]
  (* (+ a (- b a)) t))

(defn lerp [a b t]
  (+ a (*  (- b a) t)))

(defn lerpPoints [a b t]
  (mapv * (mapv + a (mapv - b a) t)))

(defn bezier-sextic-point [c0 c1 c2 c3 c4 c5 c6 t]
  (let [a (- 1 t)
        b t
        b0 (Math/pow a 6)
        b1 (* 6 (Math/pow a 5) b)
        b2 (* 15 (Math/pow a 4) (Math/pow b 2))
        b3 (* 10 (Math/pow a 3) (Math/pow b 3))
        b4 (* 15 (Math/pow a 2) (Math/pow b 4))
        b5 (* 6 a (Math/pow b 5))
        b6 (Math/pow b 6)]
    (+ (* b0 c0) (* b1 c1) (* b2 c2) (* b3 c3) (* b4 c4) (* b5 c5) (* b6 c6))))

(defn bezier-sextic [p0 p1 p2 p3 p4 p5 p6 steps]
  (for [t (range 0 (inc steps))]
    (into [] (concat [(bezier-sextic-point (first p0) (first p1) (first p2) (first p3) (first p4) (first p5) (first p6) (/ t steps))
                      (bezier-sextic-point (second p0) (second p1) (second p2) (second p3) (second p4) (second p5) (second p6) (/ t steps))
                      (bezier-sextic-point (nth p0 2) (nth p1 2) (nth p2 2) (nth p3 2) (nth p4 2) (nth p5 2) (nth p6 2) (/ t steps))]))))

(defn bezier-quintic-point [c0 c1 c2 c3 c4 c5 t]
  (let [a (- 1 t)
        b t
        b0 (Math/pow a 5)
        b1 (* 5 (Math/pow a 4) b)
        b2 (* 10 (Math/pow a 3) (Math/pow b 2))
        b3 (* 10 (Math/pow a 2) (Math/pow b 3))
        b4 (* 5 a (Math/pow b 4))
        b5 (Math/pow b 5)]
    (+ (* b0 c0) (* b1 c1) (* b2 c2) (* b3 c3) (* b4 c4) (* b5 c5))))

(defn bezier-quintic [p0 p1 p2 p3 p4 p5 steps]
  (for [t (range 0 (inc steps))]
    (into [] (concat [(bezier-quintic-point (first p0) (first p1) (first p2) (first p3) (first p4) (first p5) (/ t steps))
                      (bezier-quintic-point (second p0) (second p1) (second p2) (second p3) (second p4) (second p5) (/ t steps))
                      (bezier-quintic-point (nth p0 2) (nth p1 2) (nth p2 2) (nth p3 2) (nth p4 2) (nth p5 2) (/ t steps))]))))

(defn bezier-quartic-point [c0 c1 c2 c3 c4 t]
  (let [a (- 1 t)
        b t
        b0 (Math/pow a 4)
        b1 (* 4 (Math/pow a 3)  b)
        b2 (* 6 (Math/pow a 2) (Math/pow b 2))
        b3 (* 4 a (Math/pow b 3))
        b4 (Math/pow b 4)]
    (+ (* b0 c0) (* b1 c1) (* b2 c2) (* b3 c3) (* b4 c4))))

(defn bezier-cubic-point-with-tension [c0 c1 c2 c3 tension t]
  (let [
        t-squared (Math/pow t 2)
        t-cubed (Math/pow t 3)
        f1 (+ (* (- 2 tension) t-cubed) (* (- (* 2 tension) 3) t-squared) 1) 
        f2 (+ (* -2 t-cubed) (* 3 t-squared)) 
        f3 (+ t-cubed (* -2 t-squared) t)
        f4 (- t-squared t-cubed) 
        ] 
    (+ )
    ))



(defn bezier-quartic [p0 p1 p2 p3 p4 steps]
  (for [t (range 0 (inc steps))]
    (into [] (concat [(bezier-quartic-point (first p0) (first p1) (first p2) (first p3) (first p4) (/ t steps))
                      (bezier-quartic-point (second p0) (second p1) (second p2) (second p3) (second p4) (/ t steps))
                      (bezier-quartic-point (nth p0 2) (nth p1 2) (nth p2 2) (nth p3 2) (nth p4 2) (/ t steps))]))))

(defn bezier-cubic-point [c0 c1 c2 c3 t]
  (let [b0 (Math/pow (- 1 t) 3)
        b1 (* (Math/pow (- 1 t) 2) 3 t)
        b2 (* 3 (- 1 t) (Math/pow t 2))
        b3 (Math/pow t 3)]
    (+ (* b0 c0) (* b1 c1) (* b2 c2) (* b3 c3))))

(defn bezier-cubic [p0 p1 p2 p3 steps]
  (for [t (range 0 (inc steps))]
    (into [] (concat [(bezier-cubic-point (first p0) (first p1) (first p2) (first p3) (/ t steps))
                      (bezier-cubic-point (second p0) (second p1) (second p2) (second p3) (/ t steps))
                      (bezier-cubic-point (nth p0 2) (nth p1 2) (nth p2 2) (nth p3 2) (/ t steps))]))))

(defn cubic-hermite-spline-point [p1 p2 p1t p2t t]
  (let [t-squared (Math/pow t 2)
        t-cubed (Math/pow t 3)
        f1 (+ (* 2 t-cubed) (* -3 t-squared) 1)
        f2 (+ (* -2 t-cubed) (* 3 t-squared))
        f3 (+ t-cubed (* -2 t-squared) t)
        f4 (- t-cubed t-squared)]
    (+ (* f1 p1) (* f2 p2) (* f3 p1t) (* f4 p2t))))

(def hermite-polymomial-matrix
  [[2 -2 1 1]
   [-3 3 -2 -1]
   [0 0 1 0]
   [1 0 0 0]])
(defn hermite-spline-point-matrix [hermite-curve-parameters t]
  (let
   [interpolation-matrix [(Math/pow t 2)
                          (Math/pow t 3)
                          t
                          1]
    result (mmul hermite-curve-parameters (mmul interpolation-matrix hermite-polymomial-matrix))]
    result))

(defn hermite-spline-curve-matrix [p1t p1 p2 p2t steps]

  (for [index (range 0 (inc steps))]

    (into [] (flatten [(hermite-spline-point-matrix [(nth p1 0) (nth p2 0) (nth p1t 0) (nth p2t 0)] (/ index steps))
                       (hermite-spline-point-matrix [(nth p1 1) (nth p2 1) (nth p1t 1) (nth p2t 1)] (/ index steps))
                       (hermite-spline-point-matrix [(nth p1 2) (nth p2 2) (nth p1t 2) (nth p2t 2)] (/ index steps))]))))

(defn cubic-hermite-spline-curve-segment [p1 p2 p1t p2t steps]
  (for [t (range 0 (inc steps))]
    (into []
          (concat [(cubic-hermite-spline-point  (nth p1 0) (nth p2 0) (nth p1t 0) (nth p2t 0) (/ t steps))
                   (cubic-hermite-spline-point  (nth p1 1) (nth p2 1) (nth p1t 1) (nth p2t 1) (/ t steps))
                   (cubic-hermite-spline-point  (nth p1 2) (nth p2 2) (nth p1t 2) (nth p2t 2) (/ t steps))]))))

(defn hermite-tension-spline-point [p1 p2 p1t p2t t tension]
  (let [t-squared (Math/pow t 2)
        t-cubed (Math/pow t 3)
        tension-squared (Math/pow tension 2)
        tension-cubed (Math/pow tension 3)
        f1 (+ (* (/ 2 tension-cubed) t-cubed) (* (/ -3 tension-squared) t-squared) 1)
        f2 (+ (* (/ -2 tension-cubed) t-cubed) (* (/ 3 tension-squared) t-squared))
        f3 (+ (/ t-cubed tension-squared) (* (/ -2 tension) t-squared) t)
        f4 (- (/ t-cubed tension-squared) (/ t-squared tension))]
    (+ (* f1 p1) (* f2 p2) (* f3 p1t) (* f4 p2t))))

(defn cubic-hermite-straight-segment-points [p1 p2-minus-p1 alpha alpha-plus-beta-minus-two two-apha-plus-beta-minus-three t]
  (+ p1 (* (+ (- (* alpha-plus-beta-minus-two (Math/pow t 3)) (* two-apha-plus-beta-minus-three (Math/pow t 2))) (* alpha 4)) p2-minus-p1)))

(defn hermite-straight-segment [p1 p2 pt1 pt2 steps]
  (let [p2-minus-p1 (mapv - p2 p1)
        alpha (mapv / pt1 p2-minus-p1)
        beta (mapv / pt2 p2-minus-p1)
        alpha-plus-beta-minus-two (mapv #(- % 2) (mapv + alpha beta))
        two-apha-plus-beta-minus-three (mapv #(- % 3) (mapv + (mapv (partial * 2) alpha) beta))]
    (for [t (range 0 (inc steps))]
      (into []
            (concat [(cubic-hermite-straight-segment-points (nth p1 0) (nth p2-minus-p1 0) (nth alpha 0) (nth alpha-plus-beta-minus-two 0) (nth two-apha-plus-beta-minus-three 0) (/ t steps))
                     (cubic-hermite-straight-segment-points (nth p1 1) (nth p2-minus-p1 1) (nth alpha 1) (nth alpha-plus-beta-minus-two 1) (nth two-apha-plus-beta-minus-three 1) (/ t steps))
                     (cubic-hermite-straight-segment-points (nth p1 2) (nth p2-minus-p1 2) (nth alpha 2) (nth alpha-plus-beta-minus-two 2) (nth two-apha-plus-beta-minus-three 2) (/ t steps))])))))



(defn cubic-hermite-tension-spline-curve [p1 p2 p1t p2t tension steps]
  (cubic-hermite-spline-curve-segment p1 p2 (mapv (partial * tension)  p1t) (mapv (partial * tension) p2t) steps))

(defn kochanek-bartels-spline-segment [pk-minus-one pk pk-plus-one pk-plus-two tension-k continuinity-k bias-k tension-k-plus-one continuinity-k-plus-one bias-k-plus-one steps]
  (let [pk-departing-tangent-scalar-1 (* (- 1 tension-k) (+ 1 bias-k) (- 1 continuinity-k) (/ 1 2))
        pk-departing-tangent-scalar-2 (* (- 1 tension-k) (- 1 bias-k) (+ 1 continuinity-k) (/ 1 2)) 
        pk-departing-tangent (mapv +  (mapv #(* pk-departing-tangent-scalar-1 %) (mapv - pk pk-minus-one))
                                   (mapv #(* pk-departing-tangent-scalar-2  %) (mapv - pk-plus-one pk)))
        
        pk-arriving-tangent-scalar-1  (* (- 1 tension-k-plus-one) (+ 1 bias-k-plus-one) (+ 1 continuinity-k-plus-one) (/ 1 2))
        pk-arriving-tangent-scalar-2 (* (- 1 tension-k-plus-one) (- 1 bias-k-plus-one) (- 1 continuinity-k-plus-one) (/ 1 2))
        pk-arriving-tangent  (mapv + (mapv #(*  pk-arriving-tangent-scalar-1 %)  (mapv - pk-plus-one pk))
                                   (mapv #(* pk-arriving-tangent-scalar-2 %) (mapv - pk-plus-two pk-plus-one)))
        ]
    (cubic-hermite-spline-curve-segment pk pk-plus-one pk-departing-tangent pk-arriving-tangent  steps)
    ) 
  )

(defn get-drop-last-point-if-not-last-segment [number-of-segments drop-last-point-of-segment] (fn [i  segment-points]
                                                                                                (if (and (not= i number-of-segments) drop-last-point-of-segment) (drop-last segment-points) segment-points)))

(defn kochanek-bartels-spline-curve
  "An extension of a Cardinal Spline with continuity and bias paramenters alongside the tension parameter
   
   Arguments:
   - points is a vector featuring n number of 3d points with an extra p0 point at the start of the vector and pn+1 at the end
   - drop-last-point-of-segment is a boolean that sets when generating multiple segments whether the last value of all but the last segment should be dropped so there are no repeated points
   - tension-values is a n-size vector containing scalar tension values for each n point
   - continuity-values is a n-size vector containing scalar continuity values for each n point
   - bias-values is a n-size vector containing scalar bias values for each n point
   - steps is the totatal number of steps taken from p1 to pn
   Returns:
   - A set of steps+1 points along the Hermite Cubic spline curve from p1 to pn with their arriving and departing tangents affected by the tangent, contiuitnity and bias values  
   "
  [points steps & {:keys [drop-last-point-of-segment tension-values continuinity-values bias-values] 
                                                      :or {drop-last-point-of-segment true tension-values [0 0 0] continuinity-values [0 0 0] bias-values [0 0 0]}}]
  (let [n (- (count points) 2)
        number-of-segments (dec n)
        segment-steps (/ steps number-of-segments)
        drop-last-point-if-not-last-segment (get-drop-last-point-if-not-last-segment number-of-segments drop-last-point-of-segment)
        ]
    (into [](apply concat(for [index (range 0 number-of-segments)
          :let [k (inc index)
                pk-minus-one (nth points (dec k))
                pk (nth points k)
                pk-plus-one  (nth points (inc k))
                pk-plus-two (nth points (+ k 2))
                tension-k (nth tension-values index)  
                continuinity-k (nth bias-values index) 
                bias-k (nth continuinity-values index)
                tension-k-plus-one (nth tension-values (inc index)) 
                continuinity-k-plus-one (nth bias-values (inc index)) 
                bias-k-plus-one (nth continuinity-values (inc index)) 
                ]
          ]
      (drop-last-point-if-not-last-segment 
       k 
       (kochanek-bartels-spline-segment pk-minus-one pk pk-plus-one pk-plus-two  
                                        tension-k continuinity-k bias-k  
                                        tension-k-plus-one continuinity-k-plus-one bias-k-plus-one  
                                        segment-steps))
      )))
      )
    ) 

(def tst
  
  )

(defn reparametrized-cubic-bezier-point [t 
                                         A-Matrix
                                         M-matrix
                                         point-column-matrix
                                      
                                         ;a b b-minus-a b-minus-a-squared b-minus-a-cubed a-squared a-cubed 
                                         ;A-Matrix-dot-M-matrix-point-column-matrix
                                         ]
  (let [t-squared (Math/pow t 2)
        t-cubed (Math/pow t 3)
        T-Matrix [t-squared t-cubed t 1]
        point-column-matrix-by-axes #(for [point point-column-matrix]
                                       (mapv (fn [point] (nth point %)) point))
 
        ]
    [(reduce + (mmul T-Matrix A-Matrix M-matrix (point-column-matrix-by-axes 0)))
     (reduce + (mmul T-Matrix A-Matrix M-matrix (point-column-matrix-by-axes 1)))
     (reduce + (mmul T-Matrix A-Matrix M-matrix (point-column-matrix-by-axes 2)))]
    ;; [(reduce + (mmul T-Matrix (nth A-Matrix-dot-M-matrix-point-column-matrix 0)))
    ;;  (reduce + (mmul T-Matrix (nth A-Matrix-dot-M-matrix-point-column-matrix 1)))
    ;;  (reduce + (mmul T-Matrix (nth A-Matrix-dot-M-matrix-point-column-matrix 2)))]
    )
  )

(defn point-column-matrix-to-scalar [matrix dimension]
  (for [point matrix]
     (mapv (fn [point] (nth point dimension)) point))
  )

(defn mmul-scalar-to-vector [scalar-matrix vector-matrix]
  (let [vector-matrix-size (count vector-matrix)
        x-matrix (point-column-matrix-to-scalar vector-matrix 0)
        y-matrix (point-column-matrix-to-scalar vector-matrix 1)
        z-matrix (point-column-matrix-to-scalar vector-matrix 2)
        scalar-matrix-dot-x-matrix (mmul scalar-matrix x-matrix)
        scalar-matrix-dot-y-matrix (mmul scalar-matrix y-matrix)
        scalar-matrix-dot-z-matrix (mmul scalar-matrix z-matrix)]
    (into [](for [index (range 0 vector-matrix-size)]
      (into [](concat (nth scalar-matrix-dot-x-matrix index) (nth scalar-matrix-dot-y-matrix index) (nth scalar-matrix-dot-z-matrix index)))
      ))
    )
  )

(defn find-control-points-for-reparametrized-cubic-bezier-curve [p0 p1 p2 p3 a b]
 (let [a-squared (Math/pow a 2)
       a-cubed (Math/pow a 3)
       b-squared (Math/pow b 2)
       b-cubed (Math/pow b 3)
       B-Matrix [[(Math/pow (- 1 a) 3) (* 3 (Math/pow (- a 1) 2) a) (* 3 (- 1 a) a-squared) a-cubed]
                 [(* (Math/pow (- a 1) 2) (- 1 b)) (* (- a 1) (+ (* -2 a) (- b) (* 3 a b))) (* a (+ a (* 2 b) (* -3 a b))) (* a-squared b)]
                 [(* (- 1 a) (Math/pow (+ -1 b) 2)) (* (- b 1) (+ (- a) (* -2 b) (* 3 a b))) (* b (+ (* 2 a) b (* -3 a b))) (* a b-squared)]
                 [(Math/pow (- 1 b) 3) (* 3 (Math/pow (- b 1) 2) b) (* 3 (- 1 b) b-squared) b-cubed]]

       point-column-matrix [[p0] [p1] [p2] [p3]]


       ]
   (mmul-scalar-to-vector B-Matrix point-column-matrix))
  )

(defn reparametrized-cubic-bezier-curve [p0 p1 p2 p3 a b steps] 
 (let [q-points (find-control-points-for-reparametrized-cubic-bezier-curve p0 p1 p2 p3 a b) 
       ]
(bezier-cubic (nth q-points 0) (nth q-points 1) (nth q-points 2) (nth q-points 3) steps)
) 
)



(defn bezier-cubic-with-tension [p0 p1 p2 p3 tension steps]
  
  (let [p1t (mapv (partial * tension) (mapv - p1 p0))
        p2t (mapv (partial * tension) (mapv - p3 p2))]
    (for [t (range 0 (inc steps))](concat [(cubic-hermite-spline-point  (nth p0 0) (nth p3 0) (nth p1t 0) (nth p2t 0) (/ t steps))
           (cubic-hermite-spline-point  (nth p0 1) (nth p3 1) (nth p1t 1) (nth p2t 1) (/ t steps))
           (cubic-hermite-spline-point  (nth p0 2) (nth p3 2) (nth p1t 2) (nth p2t 2) (/ t steps))])))
  )

(defn getT [t alpha p0 p1]
  (let [d (mapv - p0 p1)
        a (dot d d)
        b (Math/pow a (* alpha 0.5))]
    (+ b t)))

(defn getT-new [t alpha p0 p1]
  (let [d (mapv - p1 p0)
        a (dot d d)
        b (Math/pow a (* alpha 0.5))]
    (+ b t)))

(defn catmull-rom-spline  [p0 p1 p2 p3 t alpha]
  (let [
        t0 0.0
        t1 (getT t0 alpha p0 p1)
        t2 (getT t1 alpha p1 p2)
        t3 (getT t2 alpha p2 p3)
        tfinal (lerp-old  t1 t2 t)
        a1 (mapv + (mapv #(* % (/ (- t1 tfinal) (- t1 t0))) p0) (mapv #(* % (/ (- tfinal t0) (- t1 t0))) p1))
        a2 (mapv + (mapv #(* % (/ (- t2 tfinal) (- t2 t1))) p1) (mapv #(* % (/ (- tfinal t1) (- t2 t1))) p2))
        a3 (mapv + (mapv #(* % (/ (- t3 tfinal) (- t3 t2))) p2) (mapv #(* % (/ (- tfinal t2) (- t3 t2))) p3))
        b1 (mapv + (mapv #(* % (/ (- t2 tfinal) (- t2 t0))) a1) (mapv #(* % (/ (- tfinal t0) (- t2 t0))) a2))
        b2 (mapv + (mapv #(* % (/ (- t3 tfinal) (- t3 t1))) a2) (mapv #(* % (/ (- tfinal t1) (- t3 t1))) a3))
        c (mapv + (mapv #(* % (/ (- t2 tfinal) (- t2 t1))) b1) (mapv #(* % (/ (- tfinal t1) (- t2 t1))) b2))]
    c))

(defn catmull-rom-spline-point  [p0 p1 p2 p3 t alpha]
  (let [t0 0.0
        t1 (getT-new t0 alpha p0 p1)
        t2 (getT-new t1 alpha p1 p2)
        t3 (getT-new t2 alpha p2 p3)
        tfinal (lerp t1 t2 t)
        a1 (mapv + (mapv #(* % (/ (- t1 tfinal) (- t1 t0))) p0) (mapv #(* % (/ (- tfinal t0) (- t1 t0))) p1))
        a2 (mapv + (mapv #(* % (/ (- t2 tfinal) (- t2 t1))) p1) (mapv #(* % (/ (- tfinal t1) (- t2 t1))) p2))
        a3 (mapv + (mapv #(* % (/ (- t3 tfinal) (- t3 t2))) p2) (mapv #(* % (/ (- tfinal t2) (- t3 t2))) p3))
        b1 (mapv + (mapv #(* % (/ (- t2 tfinal) (- t2 t0))) a1) (mapv #(* % (/ (- tfinal t0) (- t2 t0))) a2))
        b2 (mapv + (mapv #(* % (/ (- t3 tfinal) (- t3 t1))) a2) (mapv #(* % (/ (- tfinal t1) (- t3 t1))) a3))
        c (mapv + (mapv #(* % (/ (- t2 tfinal) (- t2 t1))) b1) (mapv #(* % (/ (- tfinal t1) (- t2 t1))) b2))]
    c))

(defn catmull-rom-spline-curve [p0 p1 p2 p3 steps & {:keys [alphaType] :or {alphaType :centripetal}}]
  (let [
        alpha (case alphaType
                :uniform 0
                :centripetal 0.5
                :chordal 1.0)] 
  (for [t (range 0 (inc steps))]
     (catmull-rom-spline-point  p0  p1  p2  p3 (/ t steps) alpha) 
    )
)
  )

(defn catmull-rom-spline-curve-old [p0 p1 p2 p3 steps &{:keys [alphaType t1 t2] :or {alphaType :centripetal t1 (/ 1 3) t2 (/ 2 3)}}]
  (let [alpha (case alphaType
                :uniform 0
                :centripetal 0.5
                :chordal 1.0)
        c1 (catmull-rom-spline p0 p1 p2 p3 t1 alpha )
        c2 (catmull-rom-spline p0 p1 p2 p3 t2 alpha)]
    
    (bezier-cubic p1 c1 c2 p2 steps)
    )
  )

(defn catmull-rom-spline-as-bezier-cubic-point [p1 x y p2 t]
  (let 
   [t-squared (Math/pow t 2)
    t-cubed (Math/pow t 3)
    f1 (+ (- t-cubed) (* 3 t-squared) (* -3 t) 1)
    f2 (+ (* 3 t-cubed) (* -6 t-squared) (* 3 t))
    f3 (+ (* -3 t-cubed) (* 3 t-squared) )
    f4 t-cubed
    ]
    (+ (* p1 f1) (* x f2) (* y f3) (* f4 p2))
   )
  )

(defn catmull-rom-spline-as-bezier-cubic [p0 p1 p2 p3 steps]
  (let [x (mapv + p1 (mapv #(/ % 6) (mapv - p2 p0)))
        y (mapv - p2 (mapv #(/ % 6) (mapv - p3 p1))) 
        ]
    (for [t (range 0 (inc steps))]
      (into [] (concat [(catmull-rom-spline-as-bezier-cubic-point (first p1) (first x) (first y) (first p2) (/ t steps))
                        (catmull-rom-spline-as-bezier-cubic-point (second p2) (second x) (second y) (second p2) (/ t steps))
                        (catmull-rom-spline-as-bezier-cubic-point (nth p2 2) (nth x 2) (nth y 2) (nth p2 2) (/ t steps))]))) 
    ))




(defn quadratic-uniform-b-spline-point [pi-minus-one pi pi-plus-one t]
  (let [t-squared (Math/pow t 2)
        f1 (/ (+ t-squared (* -2 t) 1) 2)
        f2 (/ (+ (* -2 t-squared) (* 2 t) 1) 2)
        f3 (/ t-squared 2)
        ] 
    (+ (* f1 pi-minus-one) (* f2 pi) (* f3 pi-plus-one))
    )
  )

(defn quadratic-uniform-b-spline-segment [p-i-minus-one p-i p-i-plus-one steps] 
          (concat
           (for [index (range 0 (inc steps))]
             [(quadratic-uniform-b-spline-point (nth p-i-minus-one 0) (nth p-i 0) (nth p-i-plus-one 0) (/ index steps))
              (quadratic-uniform-b-spline-point (nth p-i-minus-one 1) (nth p-i 1) (nth p-i-plus-one 1) (/ index steps))
              (quadratic-uniform-b-spline-point (nth p-i-minus-one 2) (nth p-i 2) (nth p-i-plus-one 2) (/ index steps))])
           ))

(defn quadratic-uniform-b-spline [points steps &{:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
  (let [number-of-points (count points)
        number-of-segments (- number-of-points 2)
        segment-steps (/ steps number-of-segments)
        drop-last-point-if-not-last-segment (get-drop-last-point-if-not-last-segment number-of-segments drop-last-point-of-segment)]
    (into []( apply concat (for [index (range 0 number-of-segments)
          :let [i (inc index)]]
      (drop-last-point-if-not-last-segment i 
                                           (quadratic-uniform-b-spline-segment (nth points (dec i)) (nth points i) (nth points (inc i)) segment-steps)))))
    )
  )

(defn quadratic-uniform-b-spline-through-terminal-endpoint [points steps & {:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
  (let [n (dec (count points))
        find-endpoint-fn #(mapv (partial * (/ 1 2)) %)
        p-minus-one (nth points 0);(find-endpoint-fn (nth points 0))
        p-n-plus-one (nth points n) ;(find-endpoint-fn (nth points n))
        points-with-new-points(into [p-minus-one] (conj points p-n-plus-one))
        ]
    (quadratic-uniform-b-spline points-with-new-points steps :drop-last-point-of-segment drop-last-point-of-segment)
    ))

(defn loop-index-for-closed-b-spline [number-of-points index](if (>= index number-of-points) (- index number-of-points) index))

(defn quadratic-uniform-b-spline-closed [points steps & {:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
  (let [number-of-points (count points)
        number-of-segments number-of-points
        segment-steps (/ steps number-of-segments)
        drop-last-point-of-segment #(if drop-last-point-of-segment (drop-last %) %)
        loop-index (partial loop-index-for-closed-b-spline number-of-points)
        ]
     (into []( apply concat (for [index (range 0  number-of-segments)
          :let [i (inc index)]] 
        (drop-last-point-of-segment (quadratic-uniform-b-spline-segment 
                                     (nth points (loop-index (dec i)))
                                     (nth points (loop-index i))
                                     (nth points (loop-index (inc i)))
                                     segment-steps)))))
    )
    )
  
(defn cubic-uniform-b-spline-segment-point [p-i-minus-one p-i p-i-plus-one p-i-plus-two t]
  (let [t-squared (Math/pow t 2)
        t-cubed (Math/pow t 3)
        f1 (/ (+ (- t-cubed) (* 3 t-squared) (* -3 t) 1) 6)
        f2 (/ (+ (* 3 t-cubed) (* -6 t-squared) 4) 6)
        f3 (/ (+ (* -3 t-cubed) (* 3 t-squared) (* 3 t) 1) 6)
        f4 (/ t-cubed 6)
        ]
    (+ (* f1 p-i-minus-one) (* f2 p-i) (* f3 p-i-plus-one) (* f4 p-i-plus-two)) 
    )
  )

(defn cubic-uniform-b-spline-segment-point-with-tension [p-i-minus-one p-i p-i-plus-one p-i-plus-two tension t]
  (let [t-squared (Math/pow t 2)
        t-cubed (Math/pow t 3)
        f1 (/ (+ (* (- 2 tension) t-cubed) (* (- (* 2 tension) 3) t-squared) (* (- tension) t) 1) 6)
        f2 (/ (+ (* (- 6 tension) t-cubed) (* (- tension 9) t-squared) 4) 6)
        f3 (/ (+ (* (- tension 6) t-cubed) (* (- 9 (* 2 tension)) t-squared) (* tension t) 1) 6)
        f4 (/ (+ (* (- tension 2) t-cubed) (* (- 3 tension) t-squared)) 6)]
    (+ (* f1 p-i-minus-one) (* f2 p-i) (* f3 p-i-plus-one) (* f4 p-i-plus-two))))


(defn cubic-uniform-b-spline-segment [p-i-minus-one p-i p-i-plus-one p-i-plus-two steps ]
  (concat
   (for [index (range 0 (inc steps))]
     [(cubic-uniform-b-spline-segment-point (nth p-i-minus-one 0) (nth p-i 0) (nth p-i-plus-one 0) (nth p-i-plus-two 0) (/ index steps))
      (cubic-uniform-b-spline-segment-point (nth p-i-minus-one 1) (nth p-i 1) (nth p-i-plus-one 1) (nth p-i-plus-two 1) (/ index steps))
      (cubic-uniform-b-spline-segment-point (nth p-i-minus-one 2) (nth p-i 2) (nth p-i-plus-one 2) (nth p-i-plus-two 2) (/ index steps))])))

(defn cubic-uniform-b-spline-segment-with-tension  [p-i-minus-one p-i p-i-plus-one p-i-plus-two tension steps]
  (concat
   (for [index (range 0 (inc steps))]
     [(cubic-uniform-b-spline-segment-point-with-tension (nth p-i-minus-one 0) (nth p-i 0) (nth p-i-plus-one 0) (nth p-i-plus-two 0) tension (/ index steps))
      (cubic-uniform-b-spline-segment-point-with-tension (nth p-i-minus-one 1) (nth p-i 1) (nth p-i-plus-one 1) (nth p-i-plus-two 1) tension (/ index steps))
      (cubic-uniform-b-spline-segment-point-with-tension (nth p-i-minus-one 2) (nth p-i 2) (nth p-i-plus-one 2) (nth p-i-plus-two 2) tension (/ index steps))]))
  )
(defn cubic-uniform-b-spline [points steps & {:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
(let [number-of-points (count points)
      number-of-segments (- number-of-points 3)
      segment-steps (/ steps number-of-segments)
      drop-last-point-if-not-last-segment (get-drop-last-point-if-not-last-segment number-of-segments drop-last-point-of-segment) 
      ]
  (into [] (apply concat (for [index (range 0 number-of-segments)
                               :let [i (inc index)]]
                           (drop-last-point-if-not-last-segment i
                                                                (cubic-uniform-b-spline-segment (nth points (dec i)) (nth points i) (nth points (inc i)) (nth points (+ i 2)) segment-steps)))))
  )  
)

(defn cubic-uniform-b-spline-through-terminal-endpoints [points steps & {:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
  (let [n (dec (count points))
        p-minus-one (mapv - (mapv (partial * 2) (nth points 0)) (nth points 1))
        p-n-plus-1 (mapv - (mapv (partial * 2) (nth points n))(nth points (dec n)))
        points-with-new-points (into [p-minus-one] (conj points p-n-plus-1))
        ] 
    (cubic-uniform-b-spline points-with-new-points steps :drop-last-point-of-segment drop-last-point-of-segment)
    )
  )

(defn cubic-uniform-b-spline-closed [points steps & {:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
  (let [number-of-points (count points)
        number-of-segments number-of-points
        segment-steps (/ steps number-of-segments)
        drop-last-point-of-segment #(if drop-last-point-of-segment (drop-last %) %)
        loop-index (partial loop-index-for-closed-b-spline number-of-points)] 
    (into [] (apply concat (for [index (range 0 number-of-segments)
                                 :let [i (inc index)]]
                             (drop-last-point-of-segment  
                              (cubic-uniform-b-spline-segment
                               (nth points (loop-index (dec i)))
                               (nth points (loop-index i))
                               (nth points (loop-index (inc i))) 
                               (nth points (loop-index (+ i 2))) 
                               segment-steps)))))
    ) 
  )

(defn cubic-b-spline-with-tension [points tension steps & {:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
  (let [number-of-points (count points)
      number-of-segments (- number-of-points 3)
      segment-steps (/ steps number-of-segments)
      drop-last-point-if-not-last-segment (get-drop-last-point-if-not-last-segment number-of-segments drop-last-point-of-segment) ]
    (into [] (apply concat (for [index (range 0 number-of-segments)
                                 :let [i (inc index)
                                       find-hermite-p1-or-p2-fn #(mapv + (mapv (fn [v] (/ v 6)) %1) (mapv (partial * (/ 4 6)) %2) (mapv (partial * (/ 1 6)) %3))
                                      ;;  p-i-minus-one (nth points (dec i))
                                      ;;  p-i (nth points i)
                                      ;;  p-i-plus-one (nth points (inc i))
                                      ;;  p-i-plus-two (nth points (+ i 2))
                                      ;;  p1 (find-hermite-p1-or-p2-fn p-i-minus-one p-i p-i-plus-one)
                                      ;;  p2 (find-hermite-p1-or-p2-fn  p-i p-i-plus-one p-i-plus-two)
                                      ;;  p1t (mapv (partial * tension) (mapv - p-i-plus-one p-i-minus-one))
                                      ;;  p2t (mapv (partial * tension) (mapv - p-i-plus-two p-i))
                                       ]]
                             (drop-last-point-if-not-last-segment 
                              i  
                              (cubic-uniform-b-spline-segment-with-tension
                               (nth points (dec i)) (nth points i) (nth points (inc i)) (nth points (+ i 2))
                               tension
                               segment-steps))))))
  )

(defn generate-basis-matrix-to-find-cubic-uniform-b-spline-points-from-knots [n]
  (let [end-tangent-triplet [-3 0 3]
        core-triplet [1 4 1]
        matrix-dimension (+ n 3)]
    (into [] (for [row (range 0 matrix-dimension)]
               (into [] (for [column (range 0 matrix-dimension)]
                          (cond
                            (or (and (= row 0) (= column 0)) (and (= row (dec matrix-dimension)) (= column (- matrix-dimension 3)))) -3
                            (or (and (= row 0) (= column 2)) (and (= row (dec matrix-dimension)) (= column (dec matrix-dimension)))) 3
                            (= row column) 4
                            (or (and (= column (inc row)) (not= row 0)) (and (= row (inc column)) (not= row (dec matrix-dimension)))) 1
                            :else 0)))))))

(defn calculate-control-points-for-cubic-uniform-b-spline-points-from-knots [tangent1 knots tangent2]
  (let [n (dec (count knots))
        basis-matrix (generate-basis-matrix-to-find-cubic-uniform-b-spline-points-from-knots n)
        basis-matrix-inverse (matrix-inverse basis-matrix)
        values (mapv #(mapv (partial * 6.0) %) (into [] (concat [tangent1]  knots [tangent2])))
        ]
     (into [] (for [row basis-matrix-inverse]
                (reduce (partial mapv +) (mapv #(mapv (partial * %1) %2) row values))))  
    ))

(defn cubic-uniform-b-spline-through-points [tangent1 knots tangent2 steps]
  (let [control-points (calculate-control-points-for-cubic-uniform-b-spline-points-from-knots tangent1 knots tangent2) 
        ]
    (cubic-uniform-b-spline control-points steps)
    )
  )

;; (defn calculate-basis-function [p m U i u]
;;   (cond (or (and (= i 0) (= u (nth U 0)))
;;             (and (= i (- m p 1)) (+ u (nth U m)))) 1.0
;;         (or (< u (nth U i) ) (>= u (nth U (+ i p 1)))) 0.0
;;         :else (let [N (loop [j 0
;;                      N-temp[]]
;;                 (if (<= j p) (if (and (>= u (nth U (+ i j))) (< u (nth U (+ i j 1)))) (recur (inc j) (assoc N-temp j 1))
;;                        (recur (inc j ) (assoc N-temp j 0)))
                
;;                     N-temp) 
                
;;                 )] 
;;                 (loop [k 1
;;                        saved 0]
                  
;;                   (if (<= k p)(do 
;;                                 (if (= (nth N 0) 0) (recur (inc k) 0)
;;                       (recur (inc k ) (/ (- u (* (nth U i) (nth N 0))) (- (nth U (+ i k)) (nth U i)))))
;;                   (loop [j 0
;;                          Uleft 0
;;                          Uright 0]
                    
;;                     ()
;;                     ))
;;                     (nth N 0))
;;                   )
;;                 (for [k (range 1 (inc p)) :let [] j (range 0 (+ p (- k) 1))
;;                       :let [saved (if (zero? (nth N 0)) 0
;;                                       (/ (- u (* (nth U i) (nth N 0))) (- (nth U (+ i k)) (nth U i))))]]
                  
;;                   )
;;                 )
;;         )
  
;;   )

(defn w [control-points weights]
  (mapv #(conj %1 %2) control-points weights)
  )

(defn calculate-knot-span-index [n p u U]
  "calculates i - the knot span index
   is Algorithm A2.1 FindSpan from p68 of the nurbs book
    n - number of control points - 1
    p - spline degree
    u - parametric point
    U - knot sequence"
  ;-(println "calculate-knot-span-index " "n is " n " p is " p " u is " u " U is " U)
  (let [close (fn [a b] (< (abs (- a b)) 1e-10))]
    (cond (close  u  (double (nth U (inc n))))  n
        (> u (nth U (inc n))) n
        (< u (nth U p)) p
        :else
           (loop [low p
                  high (+ n 1)] (let [mid (int (Math/floor (/ (+ low high) 2.0)))]
                                  (if (or (< u (nth U mid)) (>= u (nth U (inc mid))))
                                    (if (< u (nth U mid))
                                      (recur low mid)
                                      (recur mid high))
                                    mid)))
        ;; (loop [low p
        ;;        high (inc n)
        ;;        mid (Math/floor (/ (+ low high) 2))]
        ;;   ;(if (not= mid 3.0)(println "low is " low " high is " high " mid is " mid))
        ;;   (if (or (< u (nth U mid)) (>= u (nth U (inc mid))))
        ;;     (if (< u (nth U mid)) (recur low mid (Math/floor (/ (+ low mid) 2)))
        ;;         (recur mid high (Math/floor (/ (+ mid high) 2))))
        ;;     mid))
          )))

(defn knot-span-index [knots u degree]
  (let [n (- (count knots) degree 2)
        close (fn [a b] (< (Math/abs (- a b)) 1e-10))]
    (if (or (= u (last knots)) (> u (last knots)))
      n
      (if (< u (first knots))
        degree
        (loop [low degree
               high (+ n 1)
               ](let [mid (int (Math/floor (/ (+ low high) 2.0)))]
          (if (or (< u (nth knots mid)) (>= u (nth knots (inc mid))))
            (if (< u (nth knots mid))
              (recur low mid)
              (recur mid high))
            mid)))))))

(defn nip [i u p U]
  (let [m (dec (count U))]
    (cond (or (and (= i 0) (= u (nth U 0)))
           (and (= i (- m p 1)) (+ u (nth U m)))) 1.0
         (or (< u (nth U i) ) (>= u (nth U (+ i p 1)))) 0.0
         :else
         (let [N (double-array
           (for [j (range (inc p))]
             (if (and (>= u (nth U (+ i j)))
                      (< u (nth U (+ i j 1))))
               1.0 0.0)))]
    (doseq [k (range 1 (inc p))
          :let [saved (if (zero? (aget N 0)) 0.0
                          (/ (* (- u (nth U i)) (aget N 0))  (- (nth U (+ i k)) (nth U i))))
                saved-list (double-array (inc (+ p (- k) 1)) saved)] ]
          
          (doseq [j (range 0 (+ p (- k) 1))
                :let [Uleft (nth U (+ i j 1))
                      Uright (nth U (+ i j k 1))
                      ]]
            (if (zero? (aget N (inc j))) 
              (do (aset N j (aget saved-list j)) (aset saved-list (inc j) 0.0))
                (let [temp (/ (aget N (inc j)) (- Uright Uleft))]  (aset N j (+ (aget saved-list j) (* (- Uright u) temp)))
                     (aset saved-list (inc j) (* (- u Uleft) temp))))
            )
          )
   (get N 0)
  )
   ))
)

;; (defn b-spline-basis [knots u degree i] 
;;   (if (and (<= degree 0) (< u (nth knots (+ i 1))) (>= u (nth knots i)))
;;     1
;;     (if (and (> degree 0) (= (nth knots i) (nth knots (+ i degree)))
;;       (* (b-spline-basis knots u (dec degree) i) (/ (- u (nth knots i)) (- (nth knots (+ i degree)) (nth knots i))))
;;       (+ (* (b-spline-basis knots u (dec degree) i) (/ (- u (nth knots i)) (- (nth knots (+ i degree)) (nth knots i))))
;;          (* (b-spline-basis knots u (dec degree) (inc i)) (/ (- (nth knots (+ i degree)) u) (- (nth knots (+ i degree)) (nth knots i))))
;;          )))



(defn calculate-non-vanishing-basis-functions [i u p U]
  (let [N (double-array (inc p) 0.0)
        m (dec (count U))
        left (double-array  (inc p))
        right (double-array  (inc p))
        ;saved-list (double-array (inc p))
        ]
    (aset N 0 1.0)
    ;(cond (or (and (zero? i) (= u (nth U 0)))
     ;        (and (= i (- m p 1)) (= u (nth U m)))) 1.0
      ;   (or (< u (nth U i) ) (>= u (nth U (+ i p 1)))) 0.0
       ;  :else
          (doseq [j (range 1 (inc p))]
          ;  (println "u is " u)
        (aset left j (double (- u (nth U (- (inc i) j)))))
        (aset right j (double (- (nth U (+ i j) ) u)))
        ;(aset saved-list j 0.0) 
                 
        
        (loop [saved 0.0 r 0 ] 
          ;(doall (dorun (map println N)))
          ;(doall (println ["left is " (aget left j) "right is " (aget right j)]))
          (if (< r j) (let [temp (/ (aget N r) (+ (double (aget right (inc r))) (aget left (- j r))) )] 
          ;(println (/ (aget N r)  (aget right (inc r))))
                        (aset N r (+ saved  (* (aget right (inc r)) temp)))
                        (recur (* (aget left (- j r)) temp) (inc r) )
                        )
              (aset N j saved)  
          ;(println (aget N j))
                  )
              )
          )
      ;; (doseq [r (range 0 j)
      ;;         :let [temp (/ (aget N r) (+ (aget right (inc r)) (aget left (- j r))))]]
      ;;   (aset N r (+ (aget saved-list j) (* (aget right (inc r)) temp)))
      ;;   (aset saved-list j (* (aget left (- j r)) temp))
      ;;   )
      ;; (aset N j (aget saved-list j))
        ;)
    N
    )
  )

(defn calculate-nurbs-curve-point [n p U Pw u]
  (let[span (calculate-knot-span-index n p u U)
        N (calculate-non-vanishing-basis-functions span u p U)
        
        ]
   (println span)
    (println N)
    (loop [Cw [0.0 0.0 0.0 0.0] j 0 ]
      (println " nth Pw" (+ span (- p) j))
      (if (<= j p) (recur (mapv + Cw  (mapv (partial * (aget N j)) (nth Pw (+ span (- p) j)))) (inc j) )
          (->(project-coordinate Cw)
           (subvec 0 3)))
      )
    )  )

(defn nurbs-segment [n p U Pw steps &{:keys [u-start u-end] :or {u-start 0 u-end (inc u-start)}}]
  (let [increment (/ (- u-end u-start) steps)]
   (for [u (range u-start (+ u-end increment) increment)]
    
      
        (do
        (println "u-start is " u-start  "segment-steps is " steps)
        (println "u is " u)
       (calculate-nurbs-curve-point n p U Pw u))
    ))
  )

(defn nurbs-with-homogenous-coordinates [Pw p U steps &{:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment false}}]
  (let [number-of-points (count Pw)
        number-of-points-per-segment (inc p)
        number-of-segments (- number-of-points p) 
        steps-total (* steps number-of-segments)
       ; segment-steps  (/ steps number-of-segments)
        segment-steps-normalized (/ steps steps-total)
        drop-last-point-if-not-last-segment (get-drop-last-point-if-not-last-segment number-of-segments drop-last-point-of-segment)
        n (- (count U) p 2);(dec number-of-points)
        ]
    (println  " number-of-segments is " number-of-segments " segment-steps-normalized " segment-steps-normalized)
    
    (into [] (apply concat (for [index (range 0 number-of-segments)
                                 :let [Pw-i (subvec Pw index (+ index number-of-points-per-segment))]
                                 ]
                              (drop-last-point-if-not-last-segment index (do (println "index is " index) 
                               (nurbs-segment n p U Pw steps :u-start index :u-end (inc index))))
                             )))
    ) 
  )

;; (defn b-spline-point [points degree knot-vector t]
;;   (loop [i 0 x 0 y 0 z 0]
;;     (let [temp (Nip i degree knot-vector t)]
;;       (if (< i (count points))(recur (inc i) (+ x (* (nth points 0) temp)) (* (nth points 1) temp) (* (nth points 2) temp))
;;         [x y z]) 
;;       ) 
;;     )
;;   )

(defn calculate-knot-vector [degree control-point-count is-uniform]
  (let [n control-point-count
        m (+ n degree 1)
        divisor (- m 1 (* 2 degree))]
    (if is-uniform (for [i (range 1 m)]
                     (if (>= i (dec m)) 1
                         (/ i (dec m)))
                     )
        (for  [i (range 1 m)] 
          (cond (<= i degree) 0
                (>= i (- m degree 1)) 1
                :else (/ (- i degree) divisor))
          )
        ))
  )
(defn non-uniform-rational-b-spline-point [p0 p1 p2 w t]
(let [one-minus-t (- 1 t)
      one-minus-t-squared (Math/pow one-minus-t 2)
      t-squared (Math/pow t 2)
      two-times-w-times-t-times-one-minus-t (* 2 w t one-minus-t)
      ]
  (/ (+ (* one-minus-t-squared p0) (* two-times-w-times-t-times-one-minus-t p1) (* t-squared p2))
     (+ one-minus-t-squared two-times-w-times-t-times-one-minus-t t-squared)) 
  )
                                  )


(defn non-uniform-rational-b-spline-segment [p0 p1 p2 w steps]
  (concat
   (for [index (range 0 (inc steps))]
     [(non-uniform-rational-b-spline-point (nth p0 0) (nth p1 0) (nth p2 0) w (/ index steps))
      (non-uniform-rational-b-spline-point (nth p0 1) (nth p1 1) (nth p2 1) w (/ index steps))
      (non-uniform-rational-b-spline-point (nth p0 2) (nth p1 2) (nth p2 2) w (/ index steps))]))
  )

(defn non-uniform-rational-b-spline [p0 p1 p2 w steps]
  
  )

(defn linear-non-uniform-b-spline-segment-point [p0 p1 t]
  (let [f1 (+ (- t) 1)
        f2 t]
    (mapv + (mapv #(* f1 %) p0) (mapv #(* f2 %) p1))
    ) 
  )
(defn linear-non-uniform-b-spline-segment [p0 p1 steps]
  (into [] (concat (for [index (range 0 (inc steps))
        :let [t (/ index steps)]]
    (linear-non-uniform-b-spline-segment-point p0 p1 t)
    )))
  
  )

(defn quadratic-non-uniform-b-spline-segment [p0 p1 p2 steps]
  
  )
(def quartic-uniform-b-spline-basis-matrix 
 (mapv #(mapv (partial * (/ 1 (factorial 4))) %)
       [[1 -4 6 -4 1]
  [-4 12 -12 4 0.0]
  [6 -6 -6 6 0.0]
  [-4 -12 12 4 0.0]
  [1 11 11 1 0.0]])
  )

(defn quartic-uniform-b-spline-segment-point [p-i-minus-one p-i p-i-plus-one p-i-plus-two p-i-plus-three t]
  (let[t-to-the-power-of-four (Math/pow t 4)
       t-cubed (Math/pow t 3)
       t-squared (Math/pow t 2) 
       four-factorial (factorial 4)
       f-fn #(/ (+ (* %1 t-to-the-power-of-four) (* %2 t-cubed) (* %3 t-squared) (* %4 t) %5) four-factorial)
      f1 (f-fn 1 -4 6 -4 1)
       f2 (f-fn -4 12 -6 -12 11)
       f3 (f-fn 6 -12 -6 12 11)
       f4 (f-fn -4 4 6 4 1)
       f5 (f-fn 1 0 0 0 0)
       ]
   (+ (* f1 p-i-minus-one ) (* f2 p-i) (* f3 p-i-plus-one) (* f4 p-i-plus-two) (* f5 p-i-plus-three))
   ;(mmul [p-i-minus-one p-i p-i-plus-one p-i-plus-two p-i-plus-three](mmul t-vector quartic-uniform-b-spline-basis-matrix))
   )
  )



(defn quartic-uniform-b-spline-segment[p-i-minus-one p-i p-i-plus-one p-i-plus-two p-i-plus-three steps]
  (concat
   (for [index (range 0 (inc steps))]
     [
      (quartic-uniform-b-spline-segment-point (nth p-i-minus-one 0) (nth p-i 0) (nth p-i-plus-one 0) (nth p-i-plus-two 0) (nth p-i-plus-three 0)  (/ index steps))
      (quartic-uniform-b-spline-segment-point (nth p-i-minus-one 1) (nth p-i 1) (nth p-i-plus-one 1) (nth p-i-plus-two 1) (nth p-i-plus-three 1)  (/ index steps))
      (quartic-uniform-b-spline-segment-point (nth p-i-minus-one 2) (nth p-i 2) (nth p-i-plus-one 2) (nth p-i-plus-two 2) (nth p-i-plus-three 2)  (/ index steps))]))
  )

(defn bezier-basis-fn-times-point [n i point t]
  (let [n-factorial (factorial n)
        i-factorial (factorial i)
        n-minus-i (- n i)
        factorial-n-minus-i (factorial n-minus-i)
        n-factorial-over-i-factorial-factorial-n-minus-i-times-point (* (/ n-factorial (* i-factorial factorial-n-minus-i)) )
        ]
    ;n-factorial-over-i-factorial-factorial-n-minus-i-times-point
    (* n-factorial-over-i-factorial-factorial-n-minus-i-times-point (Math/pow t i) (Math/pow (- 1 t) n-minus-i))
    ;(* (/ (factorial n) (* (factorial i) (factorial (- n i)))) (Math/pow t i) (Math/pow (- 1 t) (- n i)))
    ) 
  )
(defn bezier-basis-fn [n i t]
  (let [n-factorial (factorial n)
        i-factorial (factorial i)
        n-minus-i (- n i)
        factorial-n-minus-i (factorial n-minus-i)
        n-factorial-over-i-factorial-factorial-n-minus-i-times-point (* (/ n-factorial (* i-factorial factorial-n-minus-i)))]
    ;n-factorial-over-i-factorial-factorial-n-minus-i-times-point
    (* n-factorial-over-i-factorial-factorial-n-minus-i-times-point (Math/pow t i) (Math/pow (- 1 t) n-minus-i))
    ;(* (/ (factorial n) (* (factorial i) (factorial (- n i)))) (Math/pow t i) (Math/pow (- 1 t) (- n i)))
    ))

;; (defn b-spline-wrapper [control-points order knot-vector reversed-evaluation? steps]
;;   (chisel-protocols/polyline (chisel-curves/b-spline {:control-points control-points :order order :knot-vector knot-vector :reversed-evaluation? reversed-evaluation?}) steps)
;;   )

;; (defn clamped-b-spline-wrapper [control-points order knot-vector reversed-evaluation? steps]
;;   (chisel-protocols/polyline (chisel-curves/clamped-b-spline {:control-points control-points :order order :knot-vector knot-vector :reversed-evaluation? reversed-evaluation?}) steps))


(defn n-degree-bezier-curve [points steps]
  (let [n (dec (count points)) 
        t-values (for [index (range 0 (inc steps))] (/ index steps))
        basis-functions (for [i (range 0 (inc n))] (fn [t](bezier-basis-fn n i t)))
        curve-points (for [index (range 0 (inc steps))  
                           :let [t (/ index steps)]] 
                          (apply mapv + (for [i (range 0 (inc n))]
                        (mapv #(* ((nth basis-functions i) t) %) (nth points i)))))
        ;; bezier-basis-fn-times-point-map (fn [i point t] (mapv #(bezier-basis-fn-times-point n i % t) point))
        ;; ft (fn [i point t]  (bezier-basis-fn-times-point n i point t) )
        ;; ll (for [index (range 0 (inc steps)) i (range 0 (inc n))
        ;;          :let [t (/ index steps)]]
        ;;      (add-vec (for [point points]
        ;;                 (bezier-basis-fn-times-point-map i point t)
        ;;                 ))
        ;;      ) 
        ;; basis-basis-functions-times-points 
        ;; (for [i (range 0 (inc n))
        ;;       :let [point (nth points i)]]
        ;;   #(println (bezier-basis-fn-times-point-map i point %))
        ;;   )
        ]
 curve-points
    ;; (for [index (range 0 (inc steps))]
    ;;   (map #(% (/ index steps)) basis-basis-functions-times-points)
    ;;   ;;  (for [func basis-basis-functions-times-points
    ;;   ;;       t (/ index steps)]
    ;;   ;;   (println (func t))
    ;;   ;;    )
    ;;   ;(println (basis-basis-functions-times-points (/ index steps)))
    ;;   )
    )
  
  )



;; (defn basis-functions [i u p U]
;;   (for [j (range 1 (inc p))
;;         :let [left (- u (nth U (+ i 1 (- j))))
;;               right (- (nth U (+ i j) u))]] 
;;     )
;;   )


(defn get-spline-coordinate [p0 p1 p2 p3 t1 t2]
  (let [tt1 (- 1 t1)
        tt2 (- 1 t2)
        a11 (* 3 tt1 tt1 t1)
        a12 (* 3 tt1 t1 t1)
        a21 (* 3 tt2 tt2 t2)
        a22 (* 3 tt2 t2 t2)
        determinant (- (* a11 a22) (* a12 a21))

        b1 (- p1 (* p0 tt1 tt1 tt1) (* p3 t1 t1 t1))
        b2 (- p2 (* p0 tt2 tt2 tt2) (* p3 t2 t2 t2))
        ct1 (/ (- (* b1 a22) (* b2 a12)) determinant)
        ct2 (/ (+ (* (- b1) a21) (* b2 a11)) determinant)]
    [ct1 ct2]))

(defn bezier-cubic-through-points [p0 p1 p2 p3 steps & {:keys [t1 t2] :or {t1 (/ 1 3) t2 (/ 2 3)}}]
  (let [
        p0-to-p1 (vector-distance p0 p1)
        p1-to-p2 (vector-distance p1 p2)
        p2-to-p3 (vector-distance p2 p3)
        total-distance (+ p0-to-p1 p1-to-p2 p2-to-p3);(vector-distance p0 p3)
        ;t1 (/ p0-to-p1 total-distance)
        ;t2 (/ p2-to-p3 total-distance)
        x (get-spline-coordinate (nth p0 0) (nth p1 0) (nth p2 0) (nth p3 0) t1 t2)
        y (get-spline-coordinate (nth p0 1) (nth p1 1) (nth p2 1) (nth p3 1) t1 t2)
        z (get-spline-coordinate (nth p0 2) (nth p1 2) (nth p2 2) (nth p3  2) t1 t2)
        control-point1 [(nth x 0) (nth y 0) (nth z 0)]
        control-point2 [(nth x 1) (nth y 1) (nth z 1)]] 
    (bezier-cubic p0 control-point1 control-point2 p3 steps)))

(defn bezier-quadratic-point [c0 c1 c2 t]
  (let [b0 (Math/pow (- 1 t) 2)
        b1 (* 2 t (- 1 t))
        b2 (Math/pow t 2)]
    (+ (* b0 c0) (* b1 c1) (* b2 c2))))

(defn bezier-quadratic [p0 p1 p2 steps]
  (for [t (range 0 (inc steps))]
    (into [] (concat [(bezier-quadratic-point (first p0) (first p1) (first p2)  (/ t steps))
                      (bezier-quadratic-point (second p0) (second p1) (second p2)  (/ t steps))
                      (bezier-quadratic-point (nth p0 2) (nth p1 2) (nth p2 2)  (/ t steps))]))))

(defn bezier-quadratic-translate [p0 p1 p2 steps shape]
  (for [t (range 0 steps)]
    (into [] (concat (translate [(bezier-quadratic-point (first p0) (first p1) (first p2)  (/ t steps))
                                 (bezier-quadratic-point (second p0) (second p1) (second p2)  (/ t steps))
                                 (bezier-quadratic-point (nth p0 2) (nth p1 2) (nth p2 2)  (/ t steps))]
                                shape)))))
(defn bezier-linear-point [c0 c1 t]
  (let [b0 (- 1 t)
        b1 t]
    (+ (* c0 b0) (* c1 b1))))

(defn bezier-linear [p0 p1 steps]
  (for [t (range 0 (inc steps))]
    (into [] (concat [(bezier-linear-point (first p0) (first p1)  (/ t steps))
                      (bezier-linear-point (second p0) (second p1)  (/ t steps))
                      (bezier-linear-point (nth p0 2) (nth p1 2)  (/ t steps))]))))





(defn rearrange-nested-list [steps-outer steps-inner nested-list]
  (let []
    (for [index (range 0 steps-outer)]
      (for [i (range 0 steps-inner)]
        (nth (nth nested-list i) index)))))

(defn plot-list-of-shapes [list-of-shapes]
  (for [i (range 0 (- (count list-of-shapes) 2))]
    (concat (hull
             (nth list-of-shapes i)
             (nth list-of-shapes (+ i 1))))))

(defn plot-list-of-shapes-and-join-first-and-last [list-of-shapes]
  (concat (plot-list-of-shapes
           (hull (first list-of-shapes) (nth list-of-shapes (- (count list-of-shapes) 1))))))

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 10})
       (translate [0 0 (- (/ height 2) 10)])))

(defn translate-points-to-bottom [& points]
 ; (when seq points
  [(apply first points) (apply second points) 0]
        ;( apply mapv (fn [point]
                  ; (println point)


                     ;[(first p) (second p) 0] 

        ;( assoc point 2 0)
  ;                )
      ;     points)
     ;   )
  ;(for [point points](assoc point 2 0))
  )

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))



(defn chained-hull [shapes]
  (for [index (range 0 (- (count shapes) 1))]
    (hull (nth shapes index) (nth shapes (+ index 1)))))

(defn chained-bottom-hull [shapes]

  (for [index (range 0 (- (count shapes) 1))]
    (bottom-hull (nth shapes index) (nth shapes (+ index 1)))))

(defn chained-hull-for-two-lists [shapes1 shapes2 steps]
  (for [index (range 0 (- steps 1))]
    (hull (nth shapes1 index) (nth shapes2 index) (nth shapes1 (+ index 1)) (nth shapes2 (+ index 1)))))

(defn chained-hull-for-three-lists [shapes1 shapes2 shapes3 steps]
  (for [index (range 0 (- steps 1))]
    (hull (nth shapes1 index) (nth shapes2 index) (nth shapes3 index) (nth shapes1 (+ index 1)) (nth shapes2 (+ index 1)) (nth shapes3 (+ index 1)))))
(defn chained-hull-for-four-lists [shapes1 shapes2 shapes3 shapes4 steps]
  (for [index (range 0 (- steps 1))]
    (hull (nth shapes1 index) (nth shapes2 index) (nth shapes3 index) (nth shapes4 index) (nth shapes1 (+ index 1)) (nth shapes2 (+ index 1)) (nth shapes3 (+ index 1)) (nth shapes4 (+ index 1)))))

(defn chained-hull-to-points [upper-list upper-point lower-list lower-point steps]
  (for [index (range 0 (dec steps))]
    (hull (nth upper-list index) upper-point (nth lower-list index) lower-point (nth upper-list (inc index)) (nth lower-list (inc index))))
  )
(defn chained-hull-with-function [function shapes]
  (for [index (range 0 (- (count shapes) 1))]
    (hull (function (nth shapes index)) (function (nth shapes (+ index 1))))))

(defn chained-bottom-hull-for-two-lists [shapes1 shapes2 steps]
  (for [index (range 0 (- steps 1))]
    (bottom-hull (nth shapes1 index) (nth shapes2 index) (nth shapes1 (+ index 1)) (nth shapes2 (+ index 1)))))

;; (defn chained-bottom-hull-translate-for-two-lists [shapes1 shapes2 steps]
;;   (for [index (range 0 (- steps 1))]
;;     (bottom-hull-translate (nth shapes1 index) (nth shapes2 index) (nth shapes1 (+ index 1)) (nth shapes2 (+ index 1)) )
;;     ))

(defn chained-hull-var [steps nested-shapes]

  (let [index-max (- steps 1)]
    (for [index (range 0  index-max)]
      (hull (for [i (range 0 index-max)]
              (union (nth (nth nested-shapes index) i) (nth (nth nested-shapes (+ index 1)) i)))))))

(defmacro fn-name
  [f]
  `(-> ~f var meta :name str))

(defn bezier-polyhedron-generate-front-or-back-faces [top-left-point-index top-right-point-index bottom-left-point-index bottom-right-point-index steps]
  (into [] (concat (for [index (range 0 steps)]
                     [(+ top-left-point-index index) (+ top-left-point-index (inc index)) (+ bottom-left-point-index  index)])
                   (for [index (range 0 steps)]
                     [(+ bottom-left-point-index index) (+ top-left-point-index (inc index)) (+ bottom-left-point-index (inc index))]))))

(defn bezier-polyhedron-generate-side-faces [top-left-point-index top-right-point-index bottom-left-point-index bottom-right-point-index]
  [[top-left-point-index bottom-right-point-index bottom-left-point-index]
   [top-left-point-index top-right-point-index bottom-right-point-index]])

(defn bezier-polyhedron-generate-bottom-faces [outside-lower-start   inside-lower-end steps]
  (into [] (concat (for [index (range 0 steps)]
                     [(+ outside-lower-start index) (+ outside-lower-start  (inc index)) (- inside-lower-end (inc index))])
                   (for [index (range 0 steps)]
                     [(+ outside-lower-start index) (- inside-lower-end (inc index)) (- inside-lower-end index)]))))

(defn bezier-polyhedron-generate-top-faces [outside-upper-start  inside-upper-end steps]
  (into [] (concat (for [index (range 0 steps)]
                     [(- inside-upper-end index) (+ outside-upper-start (inc index)) (+ outside-upper-start index)])
                   (for [index (range 0 steps)]
                     [(- inside-upper-end index) (- inside-upper-end (inc index)) (+ outside-upper-start (inc index))]))))

(defn bezier-polyhedron-generate-faces [outside-upper-start outside-upper-end outside-lower-start outside-lower-end inside-upper-start inside-upper-end inside-lower-start inside-lower-end steps]
  (let [front (bezier-polyhedron-generate-front-or-back-faces outside-upper-start outside-upper-end outside-lower-start outside-lower-end steps)
        back  (bezier-polyhedron-generate-front-or-back-faces inside-upper-start inside-upper-end inside-lower-start inside-lower-end steps)
        left (bezier-polyhedron-generate-side-faces inside-upper-end outside-upper-start inside-lower-end outside-lower-start)
        right (bezier-polyhedron-generate-side-faces outside-upper-end inside-upper-start outside-lower-end inside-lower-start)
        top   (bezier-polyhedron-generate-top-faces outside-upper-start inside-upper-end steps)
        bottom (bezier-polyhedron-generate-bottom-faces outside-lower-start inside-lower-end steps)]
    (into [] (concat front back left right top bottom))))

(defn generate-polyhedron-from-points [outside-upper-points outside-lower-points inside-upper-points inside-lower-points steps]
  (let [get-end-from-start (fn [start-index] (+ start-index  steps))
        outside-upper-start 0
        outside-upper-end (get-end-from-start outside-upper-start)
        outside-lower-start (inc outside-upper-end)
        outside-lower-end (get-end-from-start outside-lower-start)
        inside-upper-start (inc outside-lower-end)
        inside-upper-end (get-end-from-start inside-upper-start)
        inside-lower-start (inc inside-upper-end)
        inside-lower-end (get-end-from-start inside-lower-start)
        faces (bezier-polyhedron-generate-faces outside-upper-start outside-upper-end outside-lower-start outside-lower-end inside-upper-start inside-upper-end inside-lower-start inside-lower-end steps)
        points (into [] (concat outside-upper-points outside-lower-points inside-upper-points inside-lower-points))]
    (polyhedron points faces)))

(defn calculate-point-between-points [point1 point2 movement-vector]
  (mapv + point2 movement-vector (mapv (fn [point] (/ point 2)) (mapv - point1 point2))))

(defn translate-to-floor [point]
  (if (vector? point) (assoc point 2 0) (assoc (vec point) 2 0)))

(defn generate-bezier-quadratic-polyhedron-from-points ([outside-upper-left outside-upper-right outside-lower-left outside-lower-right inside-upper-left inside-upper-right inside-lower-left inside-lower-right steps]

                                                        (generate-bezier-quadratic-polyhedron-from-points outside-upper-left outside-upper-right outside-lower-left outside-lower-right
                                                                                                          inside-upper-left inside-upper-right inside-lower-left inside-lower-right
                                                                                                          (calculate-point-between-points outside-upper-left outside-upper-right [0 -2 0])
                                                                                                          (calculate-point-between-points outside-lower-left outside-lower-right [0 -2 0])
                                                                                                          (calculate-point-between-points inside-upper-left inside-upper-right [0 -2 0])
                                                                                                          (calculate-point-between-points inside-lower-left inside-lower-right [0 -2 0])
                                                                                                          steps))

  ([outside-upper-left outside-upper-right outside-lower-left outside-lower-right
    inside-upper-left inside-upper-right inside-lower-left inside-lower-right
    outside-upper-control-point outside-lower-control-point inside-upper-control-point inside-lower-control-point
    steps]
   (let [outside-upper-points (bezier-quadratic outside-upper-left outside-upper-control-point outside-upper-right steps)
         outside-lower-points (bezier-quadratic outside-lower-left outside-lower-control-point outside-lower-right steps)
         inside-upper-points  (bezier-quadratic inside-upper-right inside-upper-control-point inside-upper-left steps)
         inside-lower-points (bezier-quadratic inside-lower-right inside-lower-control-point inside-lower-left steps)]
     (generate-polyhedron-from-points outside-upper-points outside-lower-points inside-upper-points inside-lower-points steps))))

(defn generate-bezier-quadratic-polyhedron-from-points-and-control-vectors
  [outside-upper-left outside-upper-right outside-lower-left outside-lower-right inside-upper-left inside-upper-right inside-lower-left inside-lower-right
   steps
   {:keys [outside-upper-control-point-vector outside-lower-control-point-vector inside-upper-control-point-vector inside-lower-control-point-vector]}]
  (generate-bezier-quadratic-polyhedron-from-points outside-upper-left outside-upper-right outside-lower-left outside-lower-right inside-upper-left inside-upper-right inside-lower-left inside-lower-right
                                                    (calculate-point-between-points outside-upper-left outside-upper-right outside-upper-control-point-vector)
                                                    (calculate-point-between-points outside-lower-left outside-lower-right outside-lower-control-point-vector)
                                                    (calculate-point-between-points inside-upper-left inside-upper-right inside-upper-control-point-vector)
                                                    (calculate-point-between-points inside-lower-left inside-lower-right inside-lower-control-point-vector)
                                                    steps))

(defn bezier-along-bezier-polyhedron-generate-front-or-back-faces ([count-inner count-outer steps] (bezier-along-bezier-polyhedron-generate-front-or-back-faces count-inner count-outer steps 0))

  ([count-inner count-outer steps start-point]  (into [] (concat
                                                          (for [index-outer (range 0 (dec count-outer)) index-inner (range 0   (dec count-inner))]
                                                            [(+ (* index-outer count-inner) index-inner start-point) (+ (* (inc index-outer) count-inner) (inc index-inner) start-point) (+ (* index-outer count-inner) (inc index-inner) start-point)])
                                                          (for [index-outer (range 0  (dec count-outer)) index-inner (range 0   (dec count-inner))]
                                                            [(+ (* index-outer count-inner) index-inner start-point) (+ (* (inc index-outer) count-inner) index-inner start-point) (+ (* (inc index-outer) count-inner) (inc index-inner) start-point)])))))

(defn bezier-along-bezier-polyhedron-generate-side [index-start1 index-start2 steps]
  (apply concat (for [index (range 0  steps)]
                  [[(+ index-start1 index) (+ index-start2 index) (+ index-start2 (inc index))]
                   [(+ index-start1 index) (+ index-start2 (inc index)) (+ index-start1 (inc index))]])))

(defn bezier-along-bezier-polyhedron-generate-side-reverse [index-start1 index-start2 steps]
  (concat (apply concat (for [index (range 1   steps)]
                          [[(- index-start2 index) (+ index-start1 (inc index)) (+ index-start1  index)]
                           [(- index-start2 index) (- index-start2 (inc index))  (+ index-start1 (inc index))]]))
          [[(inc index-start1) index-start1   (dec index-start2)]]
          [[(+ index-start1  steps) (- index-start2 steps) (dec (- index-start2 steps))]]))

(defn bezier-along-bezier-polyhedron-generate-side-reverse-2 [index-start1 index-start2 steps]
  (apply concat (for [index (range 0   steps)]
                  [[(- index-start1 (inc index))  (- index-start1 index) (+ index-start2  index)]
                   [(- index-start1 (inc index)) (+ index-start2  index) (+ index-start2 (inc index))]])))

(defn bezier-along-bezier-polyhedron-generate-side-reverse-3 [index-start1 index-start2 steps size]
  (apply concat (for [index (range 0    steps)]
                  [[(+ index-start1 (* index size)) (+ index-start2  (* (inc index)  size)) (+ index-start1 (* (inc index) size))]
                   [(+ index-start1 (* index size)) (+ index-start2 (* index size)) (+ index-start2 (* (inc index) size))]])))

(defn bezier-along-bezier-polyhedron-generate-top [front-start front-end back-start back-end size]
  (let [front-end-extra (inc front-end)]
    (concat (for [index (range 0  (dec size))]
              [(- front-end-extra (* size index)) (+ back-start (* size (inc index))) (+ back-start (* size  index))])
            (for [index (range 0 (dec size))]
              [(- front-end-extra (* size index)) (- front-end-extra (* size (inc index))) (+ back-start (* size (inc index)))])
            [[front-start (- back-end  size) (+ front-start size)]])))

(defn bezier-along-bezier-polyhedron-generate-bottom [front-start back-end size]
  (apply concat (for [index (range 0 (dec size))
                      :let [front-start-less (dec front-start)
                            back-end-extra (dec back-end)]]
                  [[(+ front-start-less (* (inc index) size)) (- back-end-extra (* (inc index) size)) (- back-end-extra (*  index size))]
                   [(+ front-start-less (* (inc index) size)) (+ front-start-less (* (+ index 2) size)) (- back-end-extra (*  (inc index) size))]])))

(defn generate-bezier-along-bezier-polyhedron-faces [front-points back-points steps]
  (let [front-points-count (count front-points)
        back-points-count (count back-points)
        front-points-start 0
        front-points-end (dec front-points-count)
        back-points-start (inc front-points-end)
        back-points-end (+ back-points-start back-points-count)]
    (concat (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps)
            (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps back-points-start)
            (bezier-along-bezier-polyhedron-generate-side (- back-points-end (inc steps)) front-points-start   steps)
            (bezier-along-bezier-polyhedron-generate-side  (inc (- front-points-end  (inc steps))) back-points-start steps)
            (bezier-along-bezier-polyhedron-generate-top front-points-start front-points-end back-points-start back-points-end (inc steps))
            (bezier-along-bezier-polyhedron-generate-bottom front-points-start back-points-end   (inc steps)))))

(defn generate-bezier-along-bezier-polyhedron [outer-points inner-points steps]
  (polyhedron (into [] (concat  outer-points inner-points))
              (generate-bezier-along-bezier-polyhedron-faces outer-points inner-points steps)))



(defn generate-bezier-along-bezier-new-face [start-indexes new-indexes-start end-indexes steps]
  (concat
   (apply concat
          (for [index (range 0 steps)
                :let [steps-to-jump (dec steps )]] 
            [[(nth start-indexes index) (+ new-indexes-start (* index steps-to-jump)) (+  new-indexes-start (* (inc index) steps-to-jump))]
             [(nth start-indexes index) (+  new-indexes-start (* (inc index) steps-to-jump)) (nth start-indexes (inc index)) ]]
            )) 
   (apply concat
          (for [index-outer (range 0 (- steps 2)) index-inner (range 0 steps) 
                :let [steps-to-jump (dec steps)]]
            [[(+ new-indexes-start (* index-inner steps-to-jump) index-outer) (+ (inc new-indexes-start) (* index-inner steps-to-jump) index-outer) (+ (inc new-indexes-start) (* (inc index-inner) steps-to-jump) index-outer)]
             [(+ new-indexes-start (* index-inner steps-to-jump) index-outer) (+ (inc new-indexes-start) (* (inc index-inner) steps-to-jump) index-outer) (+ new-indexes-start (* (inc index-inner) steps-to-jump) index-outer) ]]))
   
   (apply concat
          (for [index (range 0 steps)
                :let [start (+ new-indexes-start (- steps 2))
                      steps-to-jump (dec steps)]]
            [[(+ start (* index steps-to-jump)) (nth end-indexes index) (nth end-indexes (inc index))]
             [(+ start (* index steps-to-jump)) (nth end-indexes (inc index)) (+ start (* (inc index) steps-to-jump))]])
          )
  )
  )
(defn generate-bezier-along-bezier-polyhedron-all-sides [outer-points inner-points steps]
    (let [points (into [] (concat  outer-points inner-points))
          front-points-count (count outer-points)
          back-points-count (count inner-points)
          front-points-start 0
          front-points-end (dec front-points-count)
          back-points-start (inc front-points-end) 
          back-points-end-extra (+ back-points-start back-points-count)
          back-points-end (dec back-points-end-extra)
          get-side-points-indexes #(for [index (range 0 (inc steps))
                                         :let [start %]]
                                     (+ start index))
          get-top-and-bottom-points-indexes #(for [index (range 0 (inc steps))
                                                  :let [steps-to-jump (inc steps)]]
                                              (+ % (* index steps-to-jump)))
          generate-bezier-along-bezier-new-face-points  (fn [start-points end-points](->>
                                                         (into []
                                                               (apply concat
                                                                      (for [index (range 0 (inc steps))]
                                                                        (bezier-linear
                                                                         (nth start-points index)
                                                                         (nth end-points index)
                                                                         steps))))
                                                         (remove #(some (fn [value] (=  % value)) (concat start-points end-points)))))
          front-start-side-points-indexes (get-side-points-indexes front-points-start)
          front-end-side-points-indexes (get-side-points-indexes  (inc (- front-points-end  (inc steps))))
          back-start-side-points-indexes (get-side-points-indexes back-points-start)
          back-end-side-points-indexes (get-side-points-indexes (- back-points-end-extra (inc steps)))
          front-bottom-points-indexes (get-top-and-bottom-points-indexes (+ front-points-start steps))
          front-top-points-indexes (get-top-and-bottom-points-indexes front-points-start)
          back-top-points-indexes (get-top-and-bottom-points-indexes back-points-start)
          back-bottom-points-indexes (get-top-and-bottom-points-indexes (+ back-points-start steps))
          get-points-from-indexes (fn [indexes] (map #(nth points %) indexes))
          front-start-side-points (get-points-from-indexes front-start-side-points-indexes)
          front-end-side-points  (get-points-from-indexes front-end-side-points-indexes)
          back-end-side-points (get-points-from-indexes back-end-side-points-indexes)
          back-start-side-points (get-points-from-indexes back-start-side-points-indexes)
          front-top-points (get-points-from-indexes front-top-points-indexes)
          front-bottom-points (get-points-from-indexes front-bottom-points-indexes)
          back-top-points (get-points-from-indexes back-top-points-indexes)
          back-bottom-points (get-points-from-indexes back-bottom-points-indexes)
          side-front-to-back-new-points (generate-bezier-along-bezier-new-face-points front-end-side-points back-start-side-points)
          side-back-to-front-new-points (generate-bezier-along-bezier-new-face-points back-end-side-points front-start-side-points)
          top-new-points (generate-bezier-along-bezier-new-face-points front-top-points (reverse back-top-points))
          bottom-new-points (generate-bezier-along-bezier-new-face-points (reverse front-bottom-points) back-bottom-points )
          side-front-to-back-new-points-start (inc back-points-end)
          side-front-to-back-new-points-end (dec (+ side-front-to-back-new-points-start (count side-front-to-back-new-points)))
          side-back-to-front-new-points-start (inc side-front-to-back-new-points-end)
          side-back-to-front-new-points-end (dec (+ side-back-to-front-new-points-start (count side-back-to-front-new-points)))
          top-new-points-start (inc side-back-to-front-new-points-end)
          top-new-points-end (dec (+ top-new-points-start (count top-new-points)))
          bottom-new-points-start (inc top-new-points-end)
          front (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps)
          back (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps back-points-start)
          side-back-to-front (generate-bezier-along-bezier-new-face back-end-side-points-indexes side-back-to-front-new-points-start front-start-side-points-indexes steps) 
          ;(bezier-along-bezier-polyhedron-generate-side (- back-points-end-extra (inc steps)) front-points-start   steps)
          side-front-to-back (generate-bezier-along-bezier-new-face front-end-side-points-indexes side-front-to-back-new-points-start back-start-side-points-indexes steps) 
          ;(bezier-along-bezier-polyhedron-generate-side  (inc (- front-points-end  (inc steps))) back-points-start steps)
          top (generate-bezier-along-bezier-new-face front-top-points-indexes top-new-points-start (reverse back-top-points-indexes) steps) 
          ;(bezier-along-bezier-polyhedron-generate-top front-points-start front-points-end back-points-start back-points-end-extra (inc steps))
          bottom (generate-bezier-along-bezier-new-face    (reverse front-bottom-points-indexes) bottom-new-points-start  back-bottom-points-indexes steps)
          ;(bezier-along-bezier-polyhedron-generate-bottom front-points-start back-points-end-extra   (inc steps))
          points-extended (concat points side-front-to-back-new-points side-back-to-front-new-points top-new-points bottom-new-points)] 
      (union 
       (polyhedron points-extended
      (concat front
              back 
              side-back-to-front
              side-front-to-back
              top
              bottom))
      ;;  (plot-bezier-points  back-end-side-points (sphere 0.1))
      ;;  ;(plot-bezier-points front-end-side-points (sphere 0.1))
      ;;  ;(plot-bezier-points back-end-side-points (sphere 0.1))
      ;;  (plot-bezier-points front-bottom-points (sphere 0.1))
      ;;  (plot-bezier-points back-top-points (sphere 0.1))
      ;;  (plot-bezier-points back-bottom-points (sphere 0.1))
      ;;  (plot-bezier-points side-front-to-back-new-points (sphere 0.1))
      ;;  (color [1 0 0 1](translate (nth points-extended side-front-to-back-new-points-start) (sphere 0.5)))
       )
      
      )
  )


(defn generate-bezier-along-bezier-polyhedron-from-points-list-linear [outer-upper-points outer-lower-points
                                                                       inner-upper-points inner-lower-points
                                                                       steps]
  (let [points-fn (fn [upper lower]
                    (into [] (apply concat
                                    (for [index (range 0 (inc steps))]
                                      (bezier-linear
                                       (nth upper index)
                                       (nth lower index)
                                       steps)))))
        outer-points (points-fn outer-upper-points outer-lower-points)
        lower-points (points-fn inner-upper-points inner-lower-points)]
    (generate-bezier-along-bezier-polyhedron outer-points lower-points steps)))

(defn generate-bezier-along-bezier-polyhedron-from-points-list-linear-with-higher-resolution-top-and-bottom [outer-upper-points outer-lower-points
                                                                                                             inner-upper-points inner-lower-points
                                                                                                             steps]
  (let [points-fn (fn [upper lower]
                    (into [] (apply concat
                                    (for [index (range 0 (inc steps))]
                                      (bezier-linear
                                       (nth upper index)
                                       (nth lower index)
                                       steps)))))
        outer-points (points-fn outer-upper-points outer-lower-points)
        inner-points (points-fn inner-upper-points inner-lower-points)
        upper-points (points-fn  (reverse outer-upper-points) inner-upper-points)
        lower-points (points-fn outer-lower-points (reverse inner-lower-points))
        outer-side-edge-1 (bezier-linear (nth outer-upper-points steps) (nth outer-lower-points steps) steps)
        inner-side-edge-1 (bezier-linear (nth inner-upper-points 0) (nth inner-lower-points 0) steps)
        outer-side-edge-2 (bezier-linear (nth outer-upper-points 0) (nth outer-lower-points 0) steps)
        inner-side-edge-2 (bezier-linear (nth inner-upper-points steps) (nth inner-lower-points steps) steps)
        side-1-points (points-fn  inner-side-edge-1 outer-side-edge-1)
        side-2-points (points-fn outer-side-edge-2 inner-side-edge-2)
        outer-points-count (count outer-points)
        inner-points-count (count inner-points)
        upper-points-count (count upper-points)
        lower-points-count (count lower-points)
        side-1-points-count (count side-1-points)
        side-2-points-count (count side-2-points)
        outer-points-start 0
        outer-points-end (dec outer-points-count)
        inner-points-start (inc outer-points-end)
        inner-points-end (+ inner-points-start (dec inner-points-count))
        upper-points-start (inc inner-points-end)
        upper-points-end (+ upper-points-start (dec outer-points-count))
        lower-points-start (inc upper-points-end)
        lower-points-end (+ lower-points-start (dec lower-points-count))
        side-1-points-start (inc lower-points-end)
        side-1-points-end (+ side-1-points-start (dec side-1-points-count))
        side-2-points-start (inc side-1-points-end)
        side-2-points-end (+ side-2-points-start (dec side-2-points-count))
        faces (concat
               (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps)
               (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps inner-points-start)
               (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps upper-points-start)
               (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps lower-points-start)
               (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps side-1-points-start)
               (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps side-2-points-start))
        points (into [] (concat  outer-points inner-points upper-points lower-points side-1-points side-2-points))]
    (polyhedron points faces)))

(defn generate-bezier-along-bezier-polyhedron-from-points-linear
  [outside-upper-left outside-upper-right
   outside-lower-left outside-lower-right
   inside-upper-left inside-upper-right
   inside-lower-left inside-lower-right
   steps
   & {:keys [outside-upper-control-point-vector outside-lower-control-point-vector inside-upper-control-point-vector inside-lower-control-point-vector]
      :or {outside-upper-control-point-vector [0 0 0] outside-lower-control-point-vector [0 0 0] inside-upper-control-point-vector [0 0 0] inside-lower-control-point-vector [0 0 0]}}]
  (let [outside-upper-control-point (calculate-point-between-points outside-upper-left outside-upper-right outside-upper-control-point-vector)
        outside-lower-control-point-vector (calculate-point-between-points outside-lower-left outside-lower-right outside-lower-control-point-vector)
        inside-upper-control-point-vector (calculate-point-between-points inside-upper-left inside-upper-right inside-upper-control-point-vector)
        inside-lower-control-point-vector (calculate-point-between-points inside-lower-left inside-lower-right inside-lower-control-point-vector)
        outer-upper-points (bezier-quadratic outside-upper-left outside-upper-control-point outside-upper-right steps)
        outer-lower-points (bezier-quadratic outside-lower-left outside-lower-control-point-vector  outside-lower-right steps)
        inner-upper-points (bezier-quadratic  inside-upper-left inside-upper-control-point-vector  inside-upper-right steps)
        inner-lower-points (bezier-quadratic inside-lower-left inside-lower-control-point-vector inside-lower-right steps)]
    (generate-bezier-along-bezier-polyhedron-from-points-list-linear outer-upper-points outer-lower-points
                                                                     inner-upper-points inner-lower-points
                                                                     steps)))

(defn generate-bezier-along-bezier-polyhedron-from-points-list-quadratic [outer-upper-ponts outer-control-ponts outer-lower-ponts
                                                                          inner-upper-points inner-control-points inner-lower-points
                                                                          steps]
  (let [points-fn (fn [upper control lower]
                    (into [] (apply concat
                                    (for [index (range 0 (inc steps))]
                                      (bezier-quadratic
                                       (nth upper index)
                                       (nth control index)
                                       (nth lower index)
                                       steps)))))
        outer-points (points-fn outer-upper-ponts outer-control-ponts outer-lower-ponts)
        lower-points (points-fn inner-upper-points inner-control-points inner-lower-points)]
    (generate-bezier-along-bezier-polyhedron outer-points lower-points steps)))

;; (defn generate-bezier-along-bezier-polyhedron-from-points-quadratic

;;   [outside-upper-left outside-upper-right outside-lower-left outside-lower-right outside-control-ponint
;;    inside-upper-left inside-upper-right inside-lower-left inside-lower-right steps
;;    {:keys [outside-upper-control-point-vector outside-lower-control-point-vector inside-upper-control-point-vector inside-lower-control-point-vector
;;            outside-control-point outside-right-control-point-vector inside-left-control-point-vector inside-right-control-point-vector]
;;     :or {outside-upper-control-point-vector [0 0 0] outside-lower-control-point-vector [0 0 0] inside-upper-control-point-vector [0 0 0] inside-lower-control-point-vector [0 0 0]
;;          outside-left-control-point-vector [0 0 0] outside-right-control-point-vector [0 0 0] inside-left-control-point-vector [0 0 0] inside-right-control-point-vector [0 0 0]}}]
;;   (let [outside-upper-control-point (calculate-point-between-points outside-upper-left outside-upper-right outside-upper-control-point-vector)
;;         outside-lower-control-point (calculate-point-between-points outside-lower-left outside-lower-right outside-lower-control-point-vector)
;;         inside-upper-control-point  (calculate-point-between-points inside-upper-left inside-upper-right inside-upper-control-point-vector)
;;         inside-lower-control-point (calculate-point-between-points inside-lower-left inside-lower-right inside-lower-control-point-vector)
;;         outside-left-control-point (calculate-point-between-points outside-upper-left outside-lower-left outside-left-control-point-vector)
;;         outside-right-control-point (calculate-point-between-points outside-upper-right outside-lower-left outside-right-control-point-vector)
;;         outside-left-to-right-control-point (calculate-point-between-points outside-upper-right outside-lower-left outside-right-control-point-vector)
;;         inside-left-control-point (calculate-point-between-points inside-upper-left inside-lower-left inside-left-control-point-vector)
;;         inside-right-control-point (calculate-point-between-points inside-upper-right inside-lower-right inside-right-control-point-vector)

;;         outside-upper (bezier-quadratic outside-upper-left outside-upper-control-point outside-upper-right steps)
;;         outside-lower (bezier-quadratic outside-lower-left outside-lower-control-point-vector outside-lower-right steps)
;;         inside-upper  (bezier-quadratic inside-upper-left inside-upper-control-point inside-upper-right steps)
;;         inside-lower (bezier-quadratic inside-lower-left inside-lower-control-point inside-lower-right steps)
;; ;; outside-left (bezier-quadratic outside-upper-left outside-left-control-point outside-lower-left steps)
;; ;; outside-right (bezier-quadratic outside-upper-right  outside-right-control-point outside-lower-left steps)
;; ;; inside-left (bezier-quadratic inside-upper-left inside-left-control-point inside-lower-left steps)
;; ;; inside-right (bezier-quadratic inside-upper-right inside-right-control-point inside-lower-right steps)
;;         ]
;;     (generate-bezier-along-bezier-polyhedron outside-upper outside-lower inside-upper inside-lower)))

(defn generate-bezier-to-point-polyhedron-top-face [bezier-start-index bezier-end-index point-index]
  (into [] (concat (for [index (range bezier-start-index (inc bezier-end-index))]
                     [index point-index (inc index)]))))

(defn generate-bezier-to-point-polyhedron-bottom-face [bezier-start-index bezier-end-index point-index]
  (into [] (concat (for [index (range bezier-start-index (inc bezier-end-index))]
                     [(inc index) point-index index]))))

(defn generate-bezier-to-point-polyhedron-curved-face [bezier-upper-start bezier-upper-end bezier-lower-start]
  (into [] (apply concat (for [index (range bezier-upper-start bezier-upper-end)]
                           [[index (inc (+ bezier-lower-start index)) (+ bezier-lower-start index)]
                            [index (inc index) (inc (+ bezier-lower-start index))]]))))

(defn generate-bezier-to-point-polyhedron-left-flat-face [point-upper-index bezier-upper-start point-lower-index bezier-lower-start]
  [[point-upper-index bezier-lower-start point-lower-index] [point-upper-index bezier-upper-start bezier-lower-start]])

(defn generate-bezier-to-point-polyhedron-right-flat-face [bezier-upper-end point-upper-index  bezier-lower-end point-lower-index]
  [[bezier-upper-end point-upper-index point-lower-index] [bezier-upper-end point-lower-index  bezier-lower-end]])
(defn generate-bezier-to-point-faces [bezier-upper bezier-lower]
  (let [bezier-upper-start 0
        bezier-upper-size (count bezier-upper)
        bezier-upper-end (dec bezier-upper-size)
        point-upper-index (inc bezier-upper-end)
        bezier-lower-start (inc point-upper-index)
        bezier-lower-size (count bezier-lower)
        bezier-lower-end (dec (+ bezier-lower-start bezier-lower-size))
        point-lower-index (inc bezier-lower-end)]
    (concat (generate-bezier-to-point-polyhedron-top-face bezier-upper-start bezier-upper-end point-upper-index)
            (generate-bezier-to-point-polyhedron-bottom-face bezier-lower-start bezier-lower-end point-lower-index)
            (generate-bezier-to-point-polyhedron-curved-face bezier-upper-start bezier-upper-end bezier-lower-start)
            (generate-bezier-to-point-polyhedron-left-flat-face point-upper-index bezier-upper-start point-lower-index bezier-lower-start)
            (generate-bezier-to-point-polyhedron-right-flat-face bezier-upper-end point-upper-index  bezier-lower-end point-lower-index))))

(defn generate-bezier-to-point-polyhedron [bezier-upper point-upper bezier-lower point-lower]
  (let [bezier-to-point-polyhedron-points (apply conj (conj (vec-if-not bezier-upper) point-upper) (conj (vec-if-not bezier-lower) point-lower))
        bezier-to-point-polyhedron-faces (generate-bezier-to-point-faces bezier-upper bezier-lower)]
    (polyhedron bezier-to-point-polyhedron-points bezier-to-point-polyhedron-faces)))
(defn make-point-z-value-not-below-zero [point]
  (if (< (nth point 2) 0) (assoc (vec point) 2 0) point))

(defn equation-on-line-for-x [x-zero a t]
  (let [x (+ x-zero (* a t))]
    x))

(defn equation-on-line-for-y [y-zero b t]
  (let [y (+ y-zero (* b t))]
    y))

(defn equation-on-line-for-z [z-zero c t]
  (let [z (+ z-zero (* c t))]
    z))

(defn find-t-using-x [x x-zero a]
  (/ (- x x-zero) a))

(defn find-t-using-y [y y-zero b]
  (/ (- y y-zero) b))

(defn find-t-using-z [z z-zero c]
  (/ (- z z-zero) c))

(defn find-point-on-line-using-x [point1 point2 x]
  (let [v (mapv - point1 point2)
        [a b c] v
        [x-zero y-zero z-zero] point2
        t (find-t-using-x x x-zero a)
        point [x (equation-on-line-for-y y-zero b t) (equation-on-line-for-z z-zero c t)]]
    point))

(defn find-point-on-line-using-y [point1 point2 y]
  (let [v (mapv - point1 point2)
        [a b c] v
        [x-zero y-zero z-zero] point2
        t (find-t-using-y y y-zero b)
        point [(equation-on-line-for-x x-zero a t) y (equation-on-line-for-z z-zero c t)]]
    point))

(defn find-point-on-line-using-z [point1 point2 z]
  (let [v (mapv - point1 point2)
        [a b c] v
        [x-zero y-zero z-zero] point2
        t (find-t-using-z z z-zero c)
        point [(equation-on-line-for-x x-zero a t) (equation-on-line-for-y y-zero b t) z]]
    point))

(defn find-same-x-and-y [line-1-point-1 line-1-point-2 line-2-point-1 line-2-point-2 ]
  (let [[p0x p0y p0z] line-1-point-1
        [p1x p1y p1z] line-1-point-2
        [p2x p2y p2z] line-2-point-1
        [p3x p3y p3z] line-2-point-2
        s1x (- p1x p0x)
        s1y (- p1y p0y)
        s2x (- p3x p2x)
        s2y (- p3y p2y)
        s (/ (+ (* (- s1y) (- p0x p2x)) (* s1x (- p0y p2y))) (+ (* (- s2x) s1y) (* s1x s2y)))
        t (/ (*(- (* s2x (- p0y p2y)) s2y) (- p0x p2x)) (+ (* (- s2x) s1y) (* s1x s2y))) 
        ]
    [(+ p0x (* t s1x) ) (+ p0y (* t s1y))]
    )
  )

(defn radius-of-chord [chord-length angle-in-radians]
  (/ (/ chord-length 2) (Math/sin (/ angle-in-radians 2))))