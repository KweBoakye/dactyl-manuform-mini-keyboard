(ns dactyl-keyboard.lib.curvesandsplines.spline-surface 
  (:require [clojure.core.matrix :refer [mmul mul transpose inverse]]
            [clojure.math :refer [pow floor]]))



(def blending-matrix [[-1.0 3.0 -3.0 1.0]
                      [2.0 -5.0 4.0 -1.0]
                      [-1.0 0.0 1.0 0.0]
                      [0.0 2.0 0.0 0.0]])

(def blending-matrix-transposed (transpose blending-matrix))

(defn calculate-alpha-matrix [alpha]
  (let [alpha-squared (pow alpha 2)
        alpha-cubed (pow alpha 3)
        two-times-alpha-squared (* 2 alpha-squared)
        two-times-alpha-cubed (* 2 alpha-cubed)
        f-matrix [[0.0 0.5 0.0 -0.5] 
                  [0.0 0.0 1.0 0.0]
                  [0.0 1.0 0.0 0.0]
                  [0.5 0.0 -0.5 0.0]]
        a-matrix [[0.0 0.0 1.0 0.0]
                  [0.0 0.0 0.0 1.0] 
                  [alpha-cubed alpha-squared alpha 1] 
                  [(* 3 alpha-squared) (* 2 alpha) 1 0]]
        a-matrix-inverse (inverse a-matrix)
        ]
    (mmul  f-matrix a-matrix-inverse)
    ;;  [[0.0 0.0 1.0  0.0]
    ;;  [ 0.0  0.5  0.0 -0.5]
    ;;  [ (/ -1 (* 2 alpha))  (/ (- 3 alpha) alpha-squared)  (/ (- alpha 6) two-times-alpha-squared)  (/ 1 alpha)]
    ;;  [(/ 1 two-times-alpha-squared)  (/ (- alpha 4) two-times-alpha-cubed) (/ (- 4 alpha) two-times-alpha-cubed)  (/ -1 two-times-alpha-squared) ]])
  )
)

(defn catmull-rom-surface-point [point-matrix u w]
  (let [u-row-vector [(pow u 3) (pow u 2) u 1]
        w-column-matrix [[(pow w 3)] [(pow w 2)] [w] [1]] 
        ]
    (mmul u-row-vector blending-matrix point-matrix blending-matrix-transposed w-column-matrix)
    )
  )

(defn catmull-rom-surface-point [points steps])

(defn catmull-rom-spline-curve-matrix-point [point-matrix t alpha-matrix]
  (let [t-vector [(pow t 3) (pow t 2) t 1]
        m [[ ]]]
    
      (mmul alpha-matrix t-vector  point-matrix) 
    )
  )

(defn catmull-rom-spline-curve-matrix [points steps &{:keys [alphaType split-steps drop-last-point-if-not-last-segment]
                                                :or {alphaType :centripetal split-steps true drop-last-point-if-not-last-segment true}}]
  (let [number-of-segments (- (count points) 3)
        steps-per-segment (if split-steps (floor (/ steps number-of-segments)) steps)
        increment (/ number-of-segments steps) 
        alpha (case alphaType
                :uniform 0
                :centripetal 0.5
                :chordal 1.0)
        alpha-matrix (calculate-alpha-matrix alpha)
        alpha-blending-matrix (mapv #(mapv (partial * alpha) %) blending-matrix)]
    (into []  (apply concat (for [index  (range 0 (+ increment number-of-segments) increment)
                    :let [i (if (< index number-of-segments) (inc (floor index)) number-of-segments)
                          t (if (< index number-of-segments) (- index (floor index)) 1.0)]]
                (do (println " matrix ""index " index " i " i " t " t)
                    (catmull-rom-spline-curve-matrix-point [[(nth points (dec i))] [(nth points i)] [(nth points (inc i))] [(nth points (+ i 2))]]  t alpha-matrix))
                             ;;  (drop-last-point-if-not-last-segment
                            ;;   i
                            ;;   (catmull-rom-spline-segment (nth points (dec i)) (nth points i) (nth points (inc i)) (nth points (+ i 2)) steps-per-segment :alphaType alphaType))
                )))))


