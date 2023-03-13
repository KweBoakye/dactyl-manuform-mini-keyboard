(ns dactyl-keyboard.lib.curvesandsplines.polynomial-surfaces 
  (:require [clojure.core.matrix :refer [array matrix mmul transpose]]
            [clojure.math :refer [pow]]
            [dactyl-keyboard.lib.matrices :refer [coordinates-to-points
                                                  split-matrix-into-coordinate-matrices]]))


(defn nine-point-biquadratic-surface-patch 
  ([P-zero-zero P-zero-one P-zero-two P-one-zero P-one-one P-one-two P-two-zero P-two-one P-two-two steps] 
   (nine-point-biquadratic-surface-patch P-zero-zero P-zero-one P-zero-two P-one-zero P-one-one P-one-two P-two-zero P-two-one P-two-two steps steps)
   ) 
  ( [P-zero-zero P-zero-one P-zero-two P-one-zero P-one-one P-one-two P-two-zero P-two-one P-two-two u-steps w-steps]
   (let [point-matrix    [[P-two-two P-two-one P-two-zero ] 
                              [P-one-two P-one-one P-one-zero] 
                              [P-zero-two P-zero-one P-zero-zero]]
        coordinate-matrices (mapv (partial matrix :vectorz)
                                           (split-matrix-into-coordinate-matrices point-matrix))
        coefficient-matrix (matrix :vectorz
                                   [[2 -4 2] 
                                    [-3 4 -1] 
                                    [1 0 0]])
        coefficient-matrix-transposed (transpose coefficient-matrix) 
         result (mapv (fn [coordinate-matrix]
                        (vec (for [u-index (range (inc u-steps))
                                   :let [u (/ u-index u-steps)
                                         u-squared (pow u 2)
                                         u-row-matrix (array :vectorz [u-squared u 1])]]
                               (vec (flatten (for [w-index (range (inc w-steps))
                                                   :let [w (/ w-index w-steps)
                                                         w-squared (pow w 2)
                                                         w-column-matrix (array :vectorz [[w-squared] [w] [1]])]]
                                               (mmul
                                                (vec (mmul u-row-matrix coefficient-matrix coordinate-matrix))
                                                (mapv vec (mmul coefficient-matrix-transposed w-column-matrix)))))))))
                      coordinate-matrices)
        [result-x result-y result-z] result
        
        ] 
          ;; (vec (for [row-index (range (count (nth result 0)))]
          ;;   (vec (for [column-index (range (count (get-in result [0 0])))]
          ;;     (vec (for [coordinate-index (range (count coordinate-matrices))]
          ;;       (get-in (nth result coordinate-index) [row-index column-index])
          ;;       )) ))))
          
          (mapv (partial mapv vector) result-x result-y result-z)
          )
    )
  )


(comment 
  (transpose (matrix :vectorz [[2 -4 2]
                    [-3 4 -1]
                    [1 0 0]]))
  )

(comment 
  (nine-point-biquadratic-surface-patch 
   [0 0 0] [1 0 0] [2 0 0] [0 1 0] [1 1 1] [0 0 -0.5] [0 2 0] [1 2 0] [2 2 0]
   2))

(defmacro en [collection]
  (list 'for ['coll collection]
          '['coll])
  )

(comment 
  (partition 2 (interleave [1 2 3 4] [5 6 7 8])))

(defn sixteen-point-bicubic-surface-patch 
  ([points steps] (sixteen-point-bicubic-surface-patch points steps steps))
  ([points u-steps w-steps]
   (let [point-matrix (mapv vec (partition 4 points))
        N (matrix :vectorz[[-4.5 13.5 -13.5 4.5]
           [9.0 -22.5 18 -4.5]
           [-5.5 9.0 -4.5 1.0]
           [1.0 0.0 0.0 0.0]])
        N-transpose (transpose N)
        coordinate-matrices (mapv (partial matrix :vectorz)
                                  (split-matrix-into-coordinate-matrices point-matrix))
        
        [x-result y-result z-result] (mapv (fn [coordinate-matrix]
                                             (vec (for [u-index (range (inc u-steps))
                                                        :let [u (/ u-index u-steps)
                                                              u-squared (pow u 2)
                                                              u-cubed (pow u 3)
                                                              u-row-matrix (array :vectorz [u-cubed u-squared u 1])]]
                                                    (vec (flatten (for [w-index (range (inc w-steps))
                                                                        :let [w (/ w-index w-steps)
                                                                              w-squared (pow w 2)
                                                                              w-cubed (pow w 3)
                                                                              w-column-matrix (array :vectorz [[w-cubed] [w-squared] [w] [1]])]]
                                                                    (vec (mmul u-row-matrix N coordinate-matrix N-transpose w-column-matrix))))))))
                                           coordinate-matrices) 
        ]
    (mapv (partial mapv vector ) x-result y-result z-result)
    ))
  )


(comment 
  (sixteen-point-bicubic-surface-patch [[0 0 0] [1 0 0] [2 0 0] [3 0 0]
                                        [0 1 0] [1 1 1] [2 1 0.5] [3 1 0]
                                         [0 2 -0.5] [1 2 0] [2 2 0.5] [3 2 0]
                                        [0 3 0] [1 3 0] [2 3 0] [3 3 0]] 10))
(comment 
  (mapv (partial mapv vector) [[1 2 3]
                               [7 8 9]] [[4 5 6]
                                         [10 11 12]]))

(defn coons-surface ([P-zero-zero P-zero-one P-one-zero P-one-one steps]
                     (coons-surface P-zero-zero P-zero-one P-one-zero P-one-one steps steps))
  ([P-zero-zero P-zero-one P-one-zero P-one-one u-steps w-steps]
   (let [point-matrix [[P-zero-zero P-zero-one]
                       [P-one-zero P-one-one]]]
     (for [u-index (range (inc u-steps))
           :let [u (/ u-index u-steps)
                 one-minus-u (- 1 u)
                 u-vector [one-minus-u u]
                 negative-u-vector (mapv (partial * -1) u-vector)]]
       (for [w-index (range (inc w-steps))
             :let [w (/ w-index)
                   one-minus-w (- 1 w)
                   w-vector [[one-minus-w] [w]]]] 
         (mmul negative-u-vector point-matrix w-vector)
         ))
     )
   ))

(comment 
  (coons-surface )
  )