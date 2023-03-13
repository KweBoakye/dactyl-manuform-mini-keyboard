(ns dactyl-keyboard.lib.matrices)


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
(comment 
  (coordinate-matrix [[[0 0 0] [3 4 0] [3 7 0]]
                            [[0 0 0] [3 4 0] [3 7 0]]] 1))
(comment
  (split-matrix-into-coordinate-matrices [[[0 0 0] [3 4 0] [3 7 0]]
                      [[0 0 0] [3 4 0] [3 7 0]]]))