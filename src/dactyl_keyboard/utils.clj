(ns dactyl-keyboard.utils
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]))

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

(defn abs [n] (max n (- n)))


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

(defn plot-bezier-points [bezier-points shape]
  (mapv (fn [point]
          (translate point shape))
        bezier-points))

(defn vector-distance [v1 v2]
  (Math/sqrt (+ (Math/pow (- (nth v1 0) (nth v2 0)) 2) (Math/pow (- (nth v1 1) (nth v2 1)) 2) (Math/pow (- (nth v1 2) (nth v2 2)) 2)))
  )

(defn lerp [a b t]
  (* (+ a (- b a)) t))

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


(defn getT [t alpha p0 p1]
  (let [d (mapv - p0 p1)
        a (dot d d)
        b (Math/pow a (* alpha 0.5))]
    (+ b t)))

(defn catmull-rom-spline  [p0 p1 p2 p3 t alpha]
  (let [
        t0 0.0
        t1 (getT t0 alpha p0 p1)
        t2 (getT t1 alpha p1 p2)
        t3 (getT t2 alpha p2 p3)
        tfinal (lerp t1 t2 t)
        a1 (mapv + (mapv #(* % (/ (- t1 tfinal) (- t1 t0))) p0) (mapv #(* % (/ (- tfinal t0) (- t1 t0))) p1))
        a2 (mapv + (mapv #(* % (/ (- t2 tfinal) (- t2 t1))) p1) (mapv #(* % (/ (- tfinal t1) (- t2 t1))) p2))
        a3 (mapv + (mapv #(* % (/ (- t3 tfinal) (- t3 t2))) p2) (mapv #(* % (/ (- tfinal t2) (- t3 t2))) p3))
        b1 (mapv + (mapv #(* % (/ (- t2 tfinal) (- t2 t0))) a1) (mapv #(* % (/ (- tfinal t0) (- t2 t0))) a2))
        b2 (mapv + (mapv #(* % (/ (- t3 tfinal) (- t3 t1))) a2) (mapv #(* % (/ (- tfinal t1) (- t3 t1))) a3))
        c (mapv + (mapv #(* % (/ (- t2 tfinal) (- t2 t1))) b1) (mapv #(* % (/ (- tfinal t1) (- t2 t1))) b2))]
    c))

(defn catmull-rom-spline-curve [p0 p1 p2 p3 steps &{:keys [alphaType t1 t2] :or {alphaType :centripetal t1 (/ 1 3) t2 (/ 2 3)}}]
  (let [alpha (case alphaType
                :uniform 0
                :centripetal 0.5
                :chordal 1.0)
        c1 (catmull-rom-spline p0 p1 p2 p3 t1 alpha )
        c2 (catmull-rom-spline p0 p1 p2 p3 t2 alpha)]
    
    (bezier-cubic p1 c1 c2 p2 steps)
    )
  )


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