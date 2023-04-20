(ns dactyl-keyboard.lib.curvesandsplines.splines
  (:require [clojure.core.matrix :refer [dot magnitude-squared mmul mul]]
            [clojure.math :refer [acos asin floor pow]]
            [dactyl-keyboard.lib.curvesandsplines.curve-utils :refer [get-drop-last-point-if-not-last-segment]]
            [dactyl-keyboard.lib.matrices :refer [coordinate-matrix-to-point-matrix
                                                  split-matrix-into-coordinate-matrices]])
  )

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
  (for [index (range 0 (inc steps))
        :let [t (/ index steps)]]
    (into []
          (concat [(cubic-hermite-spline-point  (nth p1 0) (nth p2 0) (nth p1t 0) (nth p2t 0) t)
                   (cubic-hermite-spline-point  (nth p1 1) (nth p2 1) (nth p1t 1) (nth p2t 1) t)
                   (cubic-hermite-spline-point  (nth p1 2) (nth p2 2) (nth p1t 2) (nth p2t 2) t)]))))


(defn cubic-hermite-spline [points steps]
  
  )
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
                                   (mapv #(* pk-arriving-tangent-scalar-2 %) (mapv - pk-plus-two pk-plus-one)))]
    (cubic-hermite-spline-curve-segment pk pk-plus-one pk-departing-tangent pk-arriving-tangent  steps)))



(defn bezier-cubic-with-tension [p0 p1 p2 p3 tension steps]

  (let [p1t (mapv (partial * tension) (mapv - p1 p0))
        p2t (mapv (partial * tension) (mapv - p3 p2))]
    (for [t (range 0 (inc steps))] (concat [(cubic-hermite-spline-point  (nth p0 0) (nth p3 0) (nth p1t 0) (nth p2t 0) (/ t steps))
                                            (cubic-hermite-spline-point  (nth p0 1) (nth p3 1) (nth p1t 1) (nth p2t 1) (/ t steps))
                                            (cubic-hermite-spline-point  (nth p0 2) (nth p3 2) (nth p1t 2) (nth p2t 2) (/ t steps))]))))

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
                   :or {drop-last-point-of-segment true
                        tension-values (vec (repeat (count points) 0))
                        continuinity-values (vec (repeat (count points) 0))
                        bias-values (vec (repeat (count points) 0))}}]
  (let [n (- (count points) 2)
        number-of-segments (dec n)
        segment-steps (/ steps number-of-segments)
        drop-last-point-if-not-last-segment (get-drop-last-point-if-not-last-segment number-of-segments drop-last-point-of-segment)]
    (into [] (apply concat (for [index (range 0 number-of-segments)
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
                                       bias-k-plus-one (nth continuinity-values (inc index))]]
                             (drop-last-point-if-not-last-segment
                              k
                              (kochanek-bartels-spline-segment pk-minus-one pk pk-plus-one pk-plus-two
                                                               tension-k continuinity-k bias-k
                                                               tension-k-plus-one continuinity-k-plus-one bias-k-plus-one
                                                               segment-steps)))))))

(defn getT [t alpha p0 p1]
  (let [d (mapv - p1 p0)
        a (dot d d)
        b (pow a (* alpha 0.5))]
    (+ b t)))

(comment (getT 1 0.5 [1 1 1] [1.1 1.1 1.1]))

(defn lerp-unclamped [a b t]
   (+ a (* (- b a) t))
  )

(defn getT-2 [t alpha p0 p1] 
  (+ (pow (magnitude-squared (mapv - p0 p1)) (* 0.5 alpha)) t)
  )

  (comment (pow 8 (/ 1 3)))

(defn catmull-rom-spline-point  [p0 p1 p2 p3 t alpha]
  (let [t0 0.0
        t1 (getT t0 alpha p0 p1)
        t2 (getT t1 alpha p1 p2)
        t3 (getT t2 alpha p2 p3)
        tfinal (lerp-unclamped t1 t2 t)
        a1 (mapv + (mapv #(* % (/ (- t1 tfinal) (- t1 t0))) p0) (mapv #(* % (/ (- tfinal t0) (- t1 t0))1) p1))
        a2 (mapv + (mapv #(* % (/ (- t2 tfinal) (- t2 t1))) p1) (mapv #(* % (/ (- tfinal t1) (- t2 t1))) p2))
        a3 (mapv + (mapv #(* % (/ (- t3 tfinal) (- t3 t2))) p2) (mapv #(* % (/ (- tfinal t2) (- t3 t2))) p3))
        b1 (mapv + (mapv #(* % (/ (- t2 tfinal) (- t2 t0))) a1) (mapv #(* % (/ (- tfinal t0) (- t2 t0))) a2))
        b2 (mapv + (mapv #(* % (/ (- t3 tfinal) (- t3 t1))) a2) (mapv #(* % (/ (- tfinal t1) (- t3 t1))) a3))
        c (mapv + (mapv #(* % (/ (- t2 tfinal) (- t2 t1))) b1) (mapv #(* % (/ (- tfinal t1) (- t2 t1))) b2))]
    c))
(defn catmull-rom-spline-deriv  [p0 p1 p2 p3 t alpha]
  (let [t0 0.0
        t1 (getT t0 alpha p0 p1)
        t2 (getT t1 alpha p1 p2)
        t3 (getT t2 alpha p2 p3)
        tfinal (lerp-unclamped t1 t2 t)
        a1 (mapv + (mapv #(* % (/ (- t1 tfinal) (- t1 t0))) p0) (mapv #(* % (/ (- tfinal t0) (- t1 t0)) 1) p1))
        a2 (mapv + (mapv #(* % (/ (- t2 tfinal) (- t2 t1))) p1) (mapv #(* % (/ (- tfinal t1) (- t2 t1))) p2))
        a3 (mapv + (mapv #(* % (/ (- t3 tfinal) (- t3 t2))) p2) (mapv #(* % (/ (- tfinal t2) (- t3 t2))) p3))
        b1 (mapv + (mapv #(* % (/ (- t2 tfinal) (- t2 t0))) a1) (mapv #(* % (/ (- tfinal t0) (- t2 t0))) a2))
        b2 (mapv + (mapv #(* % (/ (- t3 tfinal) (- t3 t1))) a2) (mapv #(* % (/ (- tfinal t1) (- t3 t1))) a3))
        a1-deriv (mul (/ 1 (- t1 t0)) (mapv - p1 p0))
        a2-deriv (mul (/ 1 (- t2 t1)) (mapv - p2 p1))
        a3-deriv (mul (/ 1 (- t3 t2)) (mapv - p3 p2))
        b1-deriv (mapv + (mul (/ 1 (- t2 t0)) (mapv - a2 a1))
                       (mul (/ (- t2 tfinal) (- t2 t0)) a1-deriv)
                       (mul (/ (- tfinal t0) (- t2 t0)) a2-deriv))
        b2-deriv (mapv + (mul (/ 1 (- t3 t1)) (mapv - a3 a2))
                       (mul (/ (- t3 tfinal) (- t3 t1)) a2-deriv)
                       (mul (/ (- tfinal t1 ) (- t3 t1)) a3-deriv))
        c-deriv (mapv + (mul (/ 1 (- t2 t1) ) (mapv - b2 b1))
                      (mul (/ (- t2 tfinal) (- t2 t1)) b1-deriv )
                      (mul (/ (- tfinal t1) (- t2 t1)) b2-deriv))]
                      c-deriv)
  )

(defn catmull-rom-matrix-point [p0 p1 p2 p3 t alpha]
  (let [tension (pow 0.5 (+ alpha 0.5))
        t-matrix [(pow t 3) (pow t 2) t 1]
        basis-matrix [[(- tension) (- 2 tension) (- tension 2) tension]
                      [(* 2 tension) (- tension 3) (- 3 (* 2 tension) ) (- tension)]
                      [(- tension ) 0  tension 0]
                      [0 1 0 0]]
        p-column-matrix [p0
                         p1
                         p2
                         p3]
        ]
    (mmul t-matrix basis-matrix p-column-matrix)
    )
  )

(defn catmull-rom-matrix-deriv [p0 p1 p2 p3 t alpha]
  (let [t-matrix [(* 3(pow t 2)) (* 2 t) 1 ]
        basis-matrix [[(- alpha) (- 2 alpha) (- alpha 2) alpha]
                      [(* 2 alpha) (- alpha 3) (- 3 (* 2 alpha)) (- alpha)]
                      [(- alpha) 0  alpha 0]
                      ;[0 1 0 0]
                      ]
        p-column-matrix [p0
                         p1
                         p2
                         p3]]
    (mmul t-matrix basis-matrix p-column-matrix)))

(comment (catmull-rom-matrix-deriv  [1 1 1] [2 2 2] [3 3 0] [4 4 0] 0.0 0.5))

  
  (defn catmull [p0 p1 p2 p3 t alpha]
    (let [t-cubed (pow t 3)
          t-squared (pow t 2)
          f-n (fn [a b c d] (+ (* a t-cubed) (* b t-squared) (* c t) d))
          f-n-applied (fn [points] (apply f-n (mapv (partial * alpha) points))) 
          f0 (f-n-applied [-1.0 2.0 -1.0 0.0])
          f1 (f-n-applied [3.0 -5.0 -0.0 2.0])
          f2 (f-n-applied [-3.0 4.0 1.0 0.0])
          f3 (f-n-applied [1.0 -1.0 0.0 0.0])
    ]
      (mapv + (mapv (partial * f0) p0) (mapv (partial * f1) p1) (mapv (partial * f2) p2) (mapv (partial * f3) p3))
      )
    )

(defn catmull-rom-spline-segment [p0 p1 p2 p3 steps & {:keys [alphaType] :or {alphaType :centripetal}}]
  (let [alpha (case alphaType
                :uniform 0
                :centripetal 0.5
                :chordal 1.0)]
    (for [t (range 0 (inc steps))]
      (catmull-rom-spline-point  p0  p1  p2  p3 (/ t steps) alpha))))

(let [steps (long 27.0)
      number-of-segments 1
      increment (/ number-of-segments steps)]
  (for [index (range 0 (+ increment number-of-segments) increment)
        :let [i (if (< index number-of-segments) (inc (floor index)) number-of-segments)
              t (if (< index number-of-segments) (- index (floor index)) 1.0)]]
    (double (* index steps))
    ))

(defn catmull-rom-spline-curve [points steps & {:keys [alphaType split-steps drop-last-point-if-not-last-segment]
                                                :or {alphaType :centripetal split-steps true drop-last-point-if-not-last-segment true}}] 
  (let [number-of-segments (- (count points) 3)
        steps-per-segment (if split-steps (floor (/ steps number-of-segments)) steps)
        increment (/ number-of-segments (long steps))
        drop-last-point-if-not-last-segment (get-drop-last-point-if-not-last-segment number-of-segments drop-last-point-if-not-last-segment)
        alpha (if (keyword? alphaType )(case alphaType
                :uniform 0
                :centripetal 0.5 
                :chordal 1.0)
                alphaType)
        ]
    (into []  (for [index (range 0 (+ increment number-of-segments) increment)
                                 :let [i (if (< index number-of-segments) (inc (floor index)) number-of-segments)
                                       t (if (< index number-of-segments) (- index (floor index)) 1.0)]]
                             
                             (catmull-rom-spline-point (nth points (dec i)) (nth points i) (nth points (inc i)) (nth points (+ i 2))  t alpha)
                             ;;  (drop-last-point-if-not-last-segment
                            ;;   i
                            ;;   (catmull-rom-spline-segment (nth points (dec i)) (nth points i) (nth points (inc i)) (nth points (+ i 2)) steps-per-segment :alphaType alphaType))
                             ))))


(defn catmull-rom-matrix-curve [points steps & {:keys [alphaType]
                                                :or {alphaType :centripetal }}]
  
  (let [number-of-segments (- (count points) 3) 
        increment (/ number-of-segments (long steps)) 
        alpha (if (keyword? alphaType) (case alphaType
                                         :uniform 0
                                         :centripetal 0.5
                                         :chordal 1.0)
                  alphaType)]
    (into []  (for [index (range 0 (+ increment number-of-segments) increment)
                    :let [i (if (< index number-of-segments) (inc (floor index)) number-of-segments)
                          t (if (< index number-of-segments) (- index (floor index)) 1.0)]]

                (catmull-rom-matrix-point (nth points (dec i)) (nth points i) (nth points (inc i)) (nth points (+ i 2))  t alpha) 
                ))
    
    ))

(defn catmull-rom-deriv-curve [points steps & {:keys [alphaType]
                                                :or {alphaType :centripetal}}]

  (let [number-of-segments (- (count points) 3)
        increment (/ number-of-segments (long steps))
        alpha (if (keyword? alphaType) (case alphaType
                                         :uniform 0
                                         :centripetal 0.5
                                         :chordal 1.0)
                  alphaType)]
    (into []  (for [index (range 0 (+ increment number-of-segments) increment)
                    :let [i (if (< index number-of-segments) (inc (floor index)) number-of-segments)
                          t (if (< index number-of-segments) (- index (floor index)) 1.0)]]

                (catmull-rom-spline-point (nth points (dec i)) (nth points i) (nth points (inc i)) (nth points (+ i 2))  t alpha)))))
(defn catmull-rom-spline-derivative-curve [points steps &{:keys [alphaType ]
                                                :or {alphaType :centripetal  }}]
  (let [number-of-segments (- (count points) 3) 
        increment (/ number-of-segments (long steps)) 
        alpha (if (keyword? alphaType) (case alphaType
                                         :uniform 0
                                         :centripetal 0.5
                                         :chordal 1.0)
                  alphaType)]
    (into []  (for [index (range 0 (+ increment number-of-segments) increment)
                    :let [i (if (< index number-of-segments) (inc (floor index)) number-of-segments)
                          t (if (< index number-of-segments) (- index (floor index)) 1.0)]]

                (catmull-rom-spline-deriv (nth points (dec i)) (nth points i) (nth points (inc i)) (nth points (+ i 2))  t alpha)
                             ;;  (drop-last-point-if-not-last-segment
                            ;;   i
                            ;;   (catmull-rom-spline-segment (nth points (dec i)) (nth points i) (nth points (inc i)) (nth points (+ i 2)) steps-per-segment :alphaType alphaType))
                ))))

(defn catmull-rom-spline-as-bezier-cubic-point [p1 x y p2 t]
  (let
   [t-squared (Math/pow t 2)
    t-cubed (Math/pow t 3)
    f1 (+ (- t-cubed) (* 3 t-squared) (* -3 t) 1)
    f2 (+ (* 3 t-cubed) (* -6 t-squared) (* 3 t))
    f3 (+ (* -3 t-cubed) (* 3 t-squared))
    f4 t-cubed]
    (+ (* p1 f1) (* x f2) (* y f3) (* f4 p2))))

(defn catmull-rom-spline-as-bezier-cubic [points steps]
  (let [number-of-segments (- (count points) 3)
increment (/ number-of-segments steps)]
    (println "catmull-rom-spline-as-bezier-cubic number-of-segments "number-of-segments " increment " increment " points " (count points))
    (vec (for [index (range 0 (+ increment number-of-segments) increment)
          :let [i (if (< index number-of-segments) (inc (floor index)) number-of-segments) 
                t (if (< index number-of-segments) (- index (floor index)) 1.0)
                p0 (nth points (dec i))
                p1 (nth points i)
                p2 (nth points (inc i))
                p3 (nth points (+ i 2))
                x (mapv + p1 (mapv #(/ % 6) (mapv - p2 p0))) 
                y (mapv - p2 (mapv #(/ % 6) (mapv - p3 p1)))]] 
     (do
       (println "c b " "index " index " i " i " t " t)
       (into [] (concat [(catmull-rom-spline-as-bezier-cubic-point (first p1) (first x) (first y) (first p2) (/ t steps))
                        (catmull-rom-spline-as-bezier-cubic-point (second p1) (second x) (second y) (second p2) (/ t steps))
                        (catmull-rom-spline-as-bezier-cubic-point (nth p1 2) (nth x 2) (nth y 2) (nth p2 2) (/ t steps))])))
      ))
    )

  (defn catmull-rom-experiment-code-point [p0 p1 p2 p3 t]
    (let [t-squared (pow t 2)
          t-cubed (pow t 3)
          basis-fn (fn [a b c d] (/ (+ (* a t-cubed) (* b t-squared) (* c t) d) 2))
          f-zero (basis-fn -1 2 -1 0)
          f-one (basis-fn 3 -5 0 2)
          f-two (basis-fn -3 4 1 0)
          f-three (basis-fn 1 -2 0 1)
          ]
      
      )
    )


  ;; (let [x (mapv + p1 (mapv #(/ % 6) (mapv - p2 p0)))
  ;;       y (mapv - p2 (mapv #(/ % 6) (mapv - p3 p1)))]
  ;;   (for [t (range 0 (inc steps))]
  ;;     (into [] (concat [(catmull-rom-spline-as-bezier-cubic-point (first p1) (first x) (first y) (first p2) (/ t steps))
  ;;                       (catmull-rom-spline-as-bezier-cubic-point (second p1) (second x) (second y) (second p2) (/ t steps))
  ;;                       (catmull-rom-spline-as-bezier-cubic-point (nth p1 2) (nth x 2) (nth y 2) (nth p2 2) (/ t steps))]))))
  )

(comment
  (reverse 
   [[0 1 2 3 ]
   [4 5 6 7 ]]))
(defn catmull-rom-as-bezier-cubic-segment [p0 p1 p2 p3 steps]
  (let [x (mapv + p1 (mapv #(/ % 6) (mapv - p2 p0)))
        y (mapv - p2 (mapv #(/ % 6) (mapv - p3 p1)))]
    (for [t (range 0 (inc steps))]
      (into [] (concat [(catmull-rom-spline-as-bezier-cubic-point (first p1) (first x) (first y) (first p2) (/ t steps))
                        (catmull-rom-spline-as-bezier-cubic-point (second p1) (second x) (second y) (second p2) (/ t steps))
                        (catmull-rom-spline-as-bezier-cubic-point (nth p1 2) (nth x 2) (nth y 2) (nth p2 2) (/ t steps))])))))


(defn hermite-arc [P-one P-two]
  (let [theta-one (acos (dot P-one [1 0 0]))
        theta-two (asin (dot P-two [1 0 0]))
        two-theta (- theta-one theta-two) 
        
        ])
  )