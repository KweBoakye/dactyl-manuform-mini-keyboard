(ns dactyl-keyboard.utils
 (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [ mmul set-current-implementation mul mget matrix? array shape column-matrix dot length
                                         length-squared cross]]
            [clojure.core.matrix.linear :refer [lu solve ]] 
            [clojure.math :refer [pow sqrt floor]]
            [clojure.string :refer [join]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            ))

;(set-current-implementation :vectorz)

(def π Math/PI)


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



;(defn abs [n] (max n (- n)))



(defn round-to-precision
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))


(defn select-values [map keys &{:keys [throw-error-on-nil remove-nil] :or {throw-error-on-nil true remove-nil false}}]
  (let [selected-values (reduce #(conj %1 (%2 map )) [] keys)
        non-existent-keys (keep #(if (false? (contains? map %)) %) keys)]
    (cond  (and throw-error-on-nil (some nil? selected-values))
           (throw (AssertionError. (join ["The key(s)" (join ", " non-existent-keys) " are not in this map"]))) 
           )
    
    (if remove-nil (vec (remove nil? selected-values)) selected-values)
    ) 
  )


(comment (select-values {:a 1 :b 2 :c 3} [:a :c :q :r] :remove-nil true :throw-error-on-nil false) )



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
(defn dot-product
  "Vector dot product"
  [x y]
  (reduce + (map * x y)))



;; (defn mat-mult
;;   "Matrix-Matrix product"
;;   [a b]
;;   {:pre [(= (count (nth a 0)) (count b))]}
;;   (vec
;;    (->> (for [x a
;;               y (transpose b)]
;;           (dot-product x y))
;;         (partition (count (transpose b)))
;;         (map vec))))

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
            (conj [(- last1 a) (+ (- last1 a) 1) ((+ (- last1 a) 1))])))))
(defn bezier-polyhedron-faces [listLength1, listLength2]
  (let [curved-faces (for [a (range 0 (- listLength1 2))]
                       ())]))




(defn apply-matrix [matrix points]
  (let [tdim (- (count (nth matrix 0)) 1)
        data-dim (count (nth points 0))
        scale (cond (count matrix) 1 :else (nth (nth matrix 0) 0))]))




(defn plot-bezier-points [bezier-points shape]
  (mapv (fn [point]
          (translate point shape))
        bezier-points))



(defn lerp-old [a b t]
  (* (+ a (- b a)) t))



(defn lerpPoints [a b t]
  (mapv * (mapv + a (mapv - b a) t)))



 

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










;; (defn solve-tridiagonal [n Q U]
;;   (let [R (for [i (range 3 n)]
;;             (nth Q (dec i))
;;             )
;;         abc (calculate-non-vanishing-basis-functions 4 (nth U 4) 3 U)
;;         den (nth abc 1) 
;;         dd (double-array (inc n) 0.0)
;;         P (double-array (inc n) 0.0) 
;;         ] 
;;     (aset P 2 (mapv - (nth Q 1) (mapv * (nth abc 0) (mapv / (nth P 1) den))))
;;     (doseq [i (range 3 n)
;;             ]
;;         (aset dd i (/ (nth abc 2) den-loop))   
;;            (let [abc-inner (calculate-non-vanishing-basis-functions (+ i 2) (nth U (+ i 2)) 3 U)
;;                  den])
;;            )
    
;;     (loop [i 3 den-loop den abc-loop abc]
;;       (if (< i n) 
;;         (aset dd i (/ (nth abc 2) den-loop))
;;         (let [abc-inner (calculate-non-vanishing-basis-functions (+ i 2) (nth U (+ i 2)) 3 U) 
;;               den-inner (- (nth abc-inner ))])
;;         (recur (inc i) (mapv - (nth abc 1) = (mapv * (nth abc 0) (aget dd i))))
;;         )
      
;;       )
;;     (doseq [i (range 3 n)]
;;       (aset dd i (mapv / (nth abc 2) den))
;;       ()
;;       )
;;     )
;;   )


;; (defn nurbs-from-knots [Q p weights &{:keys [point-paramater-calculation-method] :or {point-paramater-calculation-method :centripetal}} ]
;;   (let [{m :m
;;          U :U
;;          P :P} (global-curve-interop )])
;;   )



;; (defn bezier-basis-fn-times-point [n i point t]
;;   (let [n-factorial (factorial n)
;;         i-factorial (factorial i)
;;         n-minus-i (- n i)
;;         factorial-n-minus-i (factorial n-minus-i)
;;         n-factorial-over-i-factorial-factorial-n-minus-i-times-point (* (/ n-factorial (* i-factorial factorial-n-minus-i)) )
;;         ]
;;     ;n-factorial-over-i-factorial-factorial-n-minus-i-times-point
;;     (* n-factorial-over-i-factorial-factorial-n-minus-i-times-point (Math/pow t i) (Math/pow (- 1 t) n-minus-i))
;;     ;(* (/ (factorial n) (* (factorial i) (factorial (- n i)))) (Math/pow t i) (Math/pow (- 1 t) (- n i)))
;;     ) 
;;   )






;; (defn basis-functions [i u p U]
;;   (for [j (range 1 (inc p))
;;         :let [left (- u (nth U (+ i 1 (- j))))
;;               right (- (nth U (+ i j) u))]] 
;;     )
;;   )









(defn rearrange-nested-list [steps-outer steps-inner nested-list]
  (let []
    (for [index (range 0 steps-outer)]
      (for [i (range 0 steps-inner)]
        (nth (nth nested-list i) index)))))



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





(defmacro fn-name
  [f]
  `(-> ~f var meta :name str))


(defn translate-to-floor [point]
  (if (vector? point) (assoc point 2 0) (assoc (vec point) 2 0)))



(defn make-point-z-value-not-below-zero [point]
  (if (< (nth point 2) 0) (assoc (vec point) 2 0) point))



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



(defn svg-polygon-to-coordinates [svg-polygon-points]
  
  
  )




