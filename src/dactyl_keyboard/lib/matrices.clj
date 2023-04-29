(ns dactyl-keyboard.lib.matrices
  (:refer-clojure :exclude [use import])
(:require [clojure.core.matrix :refer [rotate]])
  )


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


(defn coordinate-matrix [point-matrix coordinate-index]
  (let [row-number (count point-matrix)
        colum-number (count (nth point-matrix 0))] 
   (mapv (partial mapv #(nth % coordinate-index)) point-matrix)
    )
  )

(defn split-matrix-into-coordinate-matrices [point-matrix]
  (let [point-dimensions (count (get-in point-matrix [0 0]))]
    (vec (for [coordinate-index (range point-dimensions)]
      (coordinate-matrix point-matrix coordinate-index)
      )
         )
    ) 
  )
[[[98.36991764648326 60.014180555250014 -38.72677267113651] [106.44557428759029 61.77207981460153 -0.0] [0.33056773230237513 0.07195758779811326 -4.481254072577317] [-0.0 3.649299418129634E-15 -5.058603717873] [-98.36991764648326 -60.01418055525002 38.72677267113651]]
 [[68.53664170239762 69.08128384554875 -21.58615223961187] [75.03242497722218 76.04540211183708 -0.0] [[68.53664170239762 69.08128384554875 -21.58615223961187] [2.469938959420329 2.695999428236181 -0.30976021486116245]] [[76.12041411804375 80.0488010623366 24.40700886388353] [3.1255331568734586 13.667463390870537 93.52590528380398]] [-68.53664170239762 -69.08128384554875 21.58615223961187]]
 [[[98.36991764648326 60.014180555250014 -38.72677267113651] [-0.0 -0.0 -0.0]] [[68.53664170239759 69.08128384554874 -21.586152239611863] [-9.88189054513665 -8.169234693647686 1.749540780249935]] [-0.0 -0.0 -0.0] [-0.0 -0.0 -0.0] [-0.853577689919615 8.363561868886844 -3.3534136404036254]]
 [[-31.413149310368112 14.273322297235545 -0.0] [-0.0 0.0 -0.0] [-0.0 -0.0 -0.0] [-0.0 -0.0 -0.0] [[-68.53664170239762 -69.08128384554875 21.58615223961187] [-2.469938959420329 -2.695999428236181 0.30976021486116245]]]
 [[-98.36991764648326 -60.014180555250014 38.72677267113651] [-106.44557428759029 -61.77207981460153 0.0] [-146.8454825925594 -89.58847904807351 57.81088119421321] [31.413149310368112 -14.273322297235545 0.0] [0 0 0]]
 ]
[[[98.36991764648326 60.014180555250014 -38.72677267113651] [106.44557428759029 61.77207981460153 -0.0] [0.33056773230237513 0.07195758779811326 -4.481254072577317] [-0.0 3.649299418129634E-15 -5.058603717873] [-98.36991764648326 -60.01418055525002 38.72677267113651]]
 [[68.53664170239762 69.08128384554875 -21.58615223961187] [75.03242497722218 76.04540211183708 -0.0] [[68.53664170239762 69.08128384554875 -21.58615223961187] [2.469938959420329 2.695999428236181 -0.30976021486116245]] [[76.12041411804375 80.0488010623366 24.40700886388353] [3.1255331568734586 13.667463390870537 93.52590528380398]] [-68.53664170239762 -69.08128384554875 21.58615223961187]]
 [[[98.36991764648326 60.014180555250014 -38.72677267113651] [-0.0 -0.0 -0.0]] [[68.53664170239759 69.08128384554874 -21.586152239611863] [-9.88189054513665 -8.169234693647686 1.749540780249935]] [-0.0 -0.0 -0.0] [-0.0 -0.0 -0.0] [-0.853577689919615 8.363561868886844 -3.3534136404036254]]
 [[-31.413149310368112 14.273322297235545 -0.0] [-0.0 0.0 -0.0] [-0.0 -0.0 -0.0] [-0.0 -0.0 -0.0] [[-68.53664170239762 -69.08128384554875 21.58615223961187] [-2.469938959420329 -2.695999428236181 0.30976021486116245]]]
 [[-98.36991764648326 -60.014180555250014 38.72677267113651] [-106.44557428759029 -61.77207981460153 0.0] [-146.8454825925594 -89.58847904807351 57.81088119421321] [31.413149310368112 -14.273322297235545 0.0] [0 0 0]]]

(defn coordinates-to-points [coordinate-coll]
  (let [dimensions (count coordinate-coll)
        number-of-points (count (nth coordinate-coll 0))
        
        ]
    (vec (for [index (range number-of-points) ]
       (vec (flatten (for [coordinate (range dimensions)]
         (get-in coordinate-coll [coordinate index ])
        )))
      ))
    )
  )

(defn coordinate-matrix-to-point-matrix [coordinate-matrix]
  (let [dimensions (count coordinate-matrix)
        matrix-rows (count (nth coordinate-matrix 0))
        matrix-columns (count (nth (nth coordinate-matrix 0) 0))]
    (vec (for [row (range matrix-rows)]
      (vec (for [column (range matrix-columns)]
        (vec (for [dimension (range dimensions)]
          (get-in (nth coordinate-matrix dimension) [row column])
          ))))))))

(comment (rotate [[1 2 3 4]
                  [5 6 7 8]
                  [9 10 11 12]] 3 0))
(comment (peek [[1 2 3 4]
                [5 6 7 8]
                [9 10 11 12]]))
(defn rotate-matrix [matrix &{:keys [reverse-new-row reverse-new-column] :or {reverse-new-row false reverse-new-column false}}]
  (let [row-count (count matrix)
        column-count (count (peek matrix))
        column-reverse (fn [matrix](mapv #(vec (reverse %)) matrix))]
    (vec (cond-> (vec (for [col (range column-count)]
                   (vec (for [row (range row-count)]
                          (get-in matrix [row col])))))
      reverse-new-row (reverse)
      reverse-new-column (column-reverse)))
    ))

(comment  (rotate-matrix [[1 2 3 4]
                          [5 6 7 8]
                          [9 10 11 12]] :reverse-new-row true :reverse-new-column true))


(comment 
  (coordinate-matrix [[[0 0 0] [3 4 0] [3 7 0]]
                            [[0 0 0] [3 4 0] [3 7 0]]] 1))
(comment
  (coordinate-matrix-to-point-matrix (split-matrix-into-coordinate-matrices [[[0 0 0] [3 4 0] [3 7 0]]
                      [[0 1 2] [3 4 3] [1 8  4]]])))