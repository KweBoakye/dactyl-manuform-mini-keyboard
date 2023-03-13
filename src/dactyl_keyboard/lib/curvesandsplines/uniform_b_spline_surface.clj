(ns dactyl-keyboard.lib.curvesandsplines.uniform-b-spline-surface 
  (:require [clojure.core.matrix :refer [matrix transpose array mmul]]
            [dactyl-keyboard.lib.matrices :refer [split-matrix-into-coordinate-matrices]]
            [clojure.math :refer [pow]]))



(defn biquadratic-uniform-b-spline-surface ([points steps] (biquadratic-uniform-b-spline-surface points steps steps))
  ([points u-steps w-steps]
   (let [point-matrix (mapv vec (partition 3 points))
        coefficient-matrix (matrix :vectorz
                                   [[1 -2 1] 
                                    [-2 2 0] 
                                    [1 1 0]])
        transposed-coefficient-matrix (transpose coefficient-matrix)
        coordinate-matrices (mapv (partial matrix :vectorz)
                                  (split-matrix-into-coordinate-matrices point-matrix)) 
        [x-result y-result z-result] (mapv (fn [coordinate-matrix]
                                             (vec (for [u-index (range (inc u-steps))
                                                        :let [u (/ u-index u-steps)
                                                              u-squared (pow u 2) 
                                                              u-row-matrix (array :vectorz (mapv (partial * 0.25) [u-squared u 1]))]]
                                                    (vec (flatten (for [w-index (range (inc w-steps))
                                                                        :let [w (/ w-index w-steps)
                                                                              w-squared (pow w 2) 
                                                                              w-column-matrix (array :vectorz [[w-squared] [w] [1]])]]
                                                                    (vec (mmul u-row-matrix coefficient-matrix coordinate-matrix transposed-coefficient-matrix w-column-matrix))))))))
                                           coordinate-matrices)
        ]
    (mapv (partial mapv vector) x-result y-result z-result)
    ))
  )

(comment
  (biquadratic-uniform-b-spline-surface [[0 0 0] [0 1 0] [0 2 0] [1 0 0] [1 1 1] [1 2 0] [2 0 0] [2 1 0] [2 2 0]] 10))

(defn bicubic-uniform-b-spline-surface ([points steps] (bicubic-uniform-b-spline-surface points steps steps))
  ([points u-steps w-steps]
   (let [point-matrix (mapv vec (partition 4 points))
         coefficient-matrix (matrix :vectorz
                                    [[-1 3 -3 1]
                                     [3 -6 3 0]
                                     [-3 0 3 0]
                                     [1 4 1 0]])
         transposed-coefficient-matrix (transpose coefficient-matrix)
         coordinate-matrices (mapv (partial matrix :vectorz)
                                   (split-matrix-into-coordinate-matrices point-matrix))
         [x-result y-result z-result] (mapv (fn [coordinate-matrix]
                                              (vec (for [u-index (range (inc u-steps))
                                                         :let [u (/ u-index u-steps)
                                                               u-squared (pow u 2)
                                                               u-cubed (pow u 3)
                                                               u-row-matrix (array :vectorz (mapv (partial * (pow (/ 1 6) 2)) [u-cubed u-squared u 1]))]]
                                                     (vec (flatten (for [w-index (range (inc w-steps))
                                                                         :let [w (/ w-index w-steps)
                                                                               w-squared (pow w 2)
                                                                               w-cubed (pow w 3)
                                                                               w-column-matrix (array :vectorz [[w-cubed ] [w-squared] [w] [1]])]]
                                                                     (vec (mmul u-row-matrix coefficient-matrix coordinate-matrix transposed-coefficient-matrix w-column-matrix))))))))
                                            coordinate-matrices)]
     (mapv (partial mapv vector) x-result y-result z-result))))

(comment
  (bicubic-uniform-b-spline-surface [[0 0 0] [0 1 0] [0 2 0] [0 3 0] 
                                     [1 0 0] [1 1 1] [1 2 0] [1 3 0]
                                     [2 0 0] [2 1 0] [2 2 0] [2 3 0]
                                     [3 0 0] [3 1 0] [3 2 0] [3 3 0]] 10))