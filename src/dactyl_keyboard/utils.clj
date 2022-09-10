(ns dactyl-keyboard.utils
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

(def is-preview false)

(def sphere-preview-fn-value 8)
(def sphere-render-fn-value 36)
(def sphere-fn-value (if is-preview sphere-preview-fn-value sphere-render-fn-value))

(def cylinder-preview-fn-value 8)
(def cylinder-render-fn-value 36)
(def cylinder-fn-value (if is-preview cylinder-preview-fn-value cylinder-render-fn-value))

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

(defn bezier-quartic-point [c0 c1 c2 c3 c4 t]
  (let [a (- 1 t)
        b t
        b0 (Math/pow a 4)
        b1 (* 4 (Math/pow a 3)  b )
        b2 (* 6 (Math/pow a 2) (Math/pow b 2) )
        b3 (* 4 a (Math/pow b 3) )
        b4 (Math/pow b 4)
        ]
    (+ (* b0 c0) (* b1 c1) (* b2 c2) (* b3 c3) (* b4 c4))
    )
  )

(defn bezier-quartic [p0 p1 p2 p3 p4 steps]
  (for [t (range 0 (inc steps))]
    (into [] (concat [(bezier-quartic-point (first p0) (first p1) (first p2) (first p3) (first p4) (/ t steps))
                      (bezier-quartic-point (second p0) (second p1) (second p2) (second p3) (second p4)(/ t steps))
                      (bezier-quartic-point (nth p0 2) (nth p1 2) (nth p2 2) (nth p3 2) (nth p4 2)(/ t steps))]))
    )
  )

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
        b1 t 
  ]
    (+ (* c0 b0) (* c1 b1))
    ))

(defn bezier-linear [p0 p1 steps]
    (for [t (range 0 (inc steps))]
      (into [] (concat [(bezier-linear-point (first p0) (first p1)  (/ t steps))
                        (bezier-linear-point (second p0) (second p1)  (/ t steps))
                        (bezier-linear-point (nth p0 2) (nth p1 2)  (/ t steps))])))
  )

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



(defn chained-hull [shapes ]
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
              (union (nth (nth nested-shapes index) i) (nth (nth nested-shapes (+ index 1)) i)) 
              )))))

(defmacro fn-name
  [f]
  `(-> ~f var meta :name str))

(defn bezier-polyhedron-generate-front-or-back-faces [top-left-point-index top-right-point-index bottom-left-point-index bottom-right-point-index steps]
  (into [] (concat (for [index (range 0 steps)]
    [(+ top-left-point-index index) (+ top-left-point-index (inc index)) (+ bottom-left-point-index  index)])
(for [index (range 0 steps)]
  [(+ bottom-left-point-index index) (+ top-left-point-index (inc index)) (+ bottom-left-point-index (inc index))])))
  )

(defn bezier-polyhedron-generate-side-faces [top-left-point-index top-right-point-index bottom-left-point-index bottom-right-point-index]
  [[top-left-point-index bottom-right-point-index bottom-left-point-index]
   [top-left-point-index top-right-point-index bottom-right-point-index]
   ] 
  )

(defn bezier-polyhedron-generate-bottom-faces [outside-lower-start   inside-lower-end steps]
  (into [](concat (for [index (range 0 steps)]
    [(+ outside-lower-start index) (+ outside-lower-start  (inc index)) (- inside-lower-end (inc index))]
    )
  (for [index (range 0 steps)]
    [(+ outside-lower-start index) (- inside-lower-end (inc index)) (- inside-lower-end index)]
    )))
  )

(defn bezier-polyhedron-generate-top-faces [outside-upper-start  inside-upper-end steps]
  (into [](concat (for [index (range 0 steps)]
            [(+ outside-upper-start index) (- inside-upper-end index) (+ outside-upper-start (inc index))])
          (for [index (range 0 steps)]
            [(- inside-upper-end index) (- inside-upper-end (inc index)) (+ outside-upper-start (inc index))]))))

(defn bezier-polyhedron-generate-faces [outside-upper-start outside-upper-end outside-lower-start outside-lower-end inside-upper-start inside-upper-end inside-lower-start inside-lower-end steps]
  (let [
        front (bezier-polyhedron-generate-front-or-back-faces outside-upper-start outside-upper-end outside-lower-start outside-lower-end steps)
        back  (bezier-polyhedron-generate-front-or-back-faces inside-upper-start inside-upper-end inside-lower-start inside-lower-end steps)
        left (bezier-polyhedron-generate-side-faces inside-upper-end outside-upper-start inside-lower-end outside-lower-start)
        right (bezier-polyhedron-generate-side-faces outside-upper-end inside-upper-start outside-lower-end inside-lower-start)
        top   (bezier-polyhedron-generate-top-faces outside-upper-start inside-upper-end steps)
        bottom (bezier-polyhedron-generate-bottom-faces outside-lower-start inside-lower-end steps)
  ]
    (into [](concat front back left right top bottom
                    )) 
    )
  )

  (defn generate-bezier-polyhedron [outside-upper-points outside-lower-points inside-upper-points inside-lower-points steps]
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
    (mapv + point2 movement-vector (mapv (fn [point] (/ point 2)) (mapv - point1 point2)))
    )
  
  (defn translate-to-floor [point]
    (if (vector? point) (assoc point 2 0) (assoc (vec point) 2 0))
    )

  (defn generate-bezier-quadratic-polyhedron-from-points ([outside-upper-left outside-upper-right outside-lower-left outside-lower-right inside-upper-left inside-upper-right inside-lower-left inside-lower-right steps]

                                                          (generate-bezier-quadratic-polyhedron-from-points outside-upper-left outside-upper-right outside-lower-left outside-lower-right
                                                                                                            inside-upper-left inside-upper-right inside-lower-left inside-lower-right
                                                                                                            (calculate-point-between-points outside-upper-left outside-upper-right [0 -4 0])
                                                                                                            (calculate-point-between-points outside-lower-left outside-lower-right [0 -4 0])
                                                                                                            (calculate-point-between-points inside-upper-left inside-upper-right [0 -4 0])
                                                                                                            (calculate-point-between-points inside-lower-left inside-lower-right [0 -4 0])
                                                                                                            steps))
    
    ([outside-upper-left outside-upper-right outside-lower-left outside-lower-right
      inside-upper-left inside-upper-right inside-lower-left inside-lower-right
      outside-upper-control-point outside-lower-control-point inside-upper-control-point inside-lower-control-point
      steps]
     (let [outside-upper-points (bezier-quadratic outside-upper-left outside-upper-control-point outside-upper-right steps)
           outside-lower-points (bezier-quadratic outside-lower-left outside-lower-control-point outside-lower-right steps)
           inside-upper-points  (bezier-quadratic inside-upper-right inside-upper-control-point inside-upper-left steps)
           inside-lower-points (bezier-quadratic inside-lower-right inside-lower-control-point inside-lower-left steps)]
       (generate-bezier-polyhedron outside-upper-points outside-lower-points inside-upper-points inside-lower-points steps))))

(defn generate-bezier-quadratic-polyhedron-from-points-and-control-vectors 
  [outside-upper-left outside-upper-right outside-lower-left outside-lower-right inside-upper-left inside-upper-right inside-lower-left inside-lower-right
     steps
   {:keys [outside-upper-control-point-vector outside-lower-control-point-vector inside-upper-control-point-vector inside-lower-control-point-vector]}]
   (generate-bezier-quadratic-polyhedron-from-points outside-upper-left outside-upper-right outside-lower-left outside-lower-right inside-upper-left inside-upper-right inside-lower-left inside-lower-right
                                                     (calculate-point-between-points outside-upper-left outside-upper-right outside-upper-control-point-vector)
                                                     (calculate-point-between-points outside-lower-left outside-lower-right outside-lower-control-point-vector)
                                                     (calculate-point-between-points inside-upper-left inside-upper-right inside-upper-control-point-vector)
                                                     (calculate-point-between-points inside-lower-left inside-lower-right inside-lower-control-point-vector)
                                                     steps)
  )