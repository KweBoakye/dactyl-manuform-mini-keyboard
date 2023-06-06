(ns dactyl-keyboard.lib.curvesandsplines.coons-surface 
  (:require [clojure.core.matrix :refer [mmul negate]]
            [clojure.math :refer [pow]]
            [dactyl-keyboard.lib.matrices :refer [coordinates-to-points
                                                  split-matrix-into-coordinate-matrices]]))

(comment (mmul [1 0] [[0 0 0] [0 1 0]
                      [1 0 0] [1 0 0]]))

(comment (negate [4 -10 1]))

(defn cubic-hermite-f-one-t [t] 
  (+ (+ 1 (* 2 (pow t 3)) (* -3 (pow t 2)))))

(defn cubic-hermite-f-2-t [t]
  (- (* 3 (pow t 3)) (* 2 (pow t 3))))

(defn degree-5-hermite-f-one-t [t]
  (+ 1 (* -10 (pow t 3)) (* 15 (pow t 4)) (* -6 (pow t 5))))

(defn degree-5-hermite-f-2-t [t]
  (+ (* 10 (pow t 3)) (* -15 (pow t 4)) (* 6 (pow t 5))))

;H-five-zero-t (fn [t] (+ 1 (* -10 (pow t 3)) (* 15 (pow t 4)) (* -6 (pow t 5))))
;H-five-five-t (fn [t] (+ (* 10 (pow t 3)) (* -15 (pow t 4)) (* 6 (pow t 5))))

(defn linear-coons-surface-point [P-zero-zero P-zero-one P-one-zero P-one-one P-zero-w P-one-w P-u-zero P-u-one u w]
  (let [
        u-row-vector [(- 1 u) u 1]
        P-matrix [[(negate P-zero-zero) (negate P-one-zero) (P-zero-w w)]
                  [(negate P-zero-one) (negate P-one-one) (P-one-w w)]
                  [(P-u-zero u ) (P-u-one u) [0 0 0]]]
        w-column-vector [[(- 1 w)]
                         [w]
                         [1]]
        [P-x-matrix P-y-matrix P-z-matrix] (split-matrix-into-coordinate-matrices P-matrix)
        result-fn (fn [coordinate-matrix] (mmul  u-row-vector coordinate-matrix w-column-vector ))
        x-result (result-fn P-x-matrix)
        y-result (result-fn P-y-matrix)
        z-result (result-fn P-z-matrix)
        result-matrix (do (println "result-matrix" (vec (flatten [x-result y-result z-result]))) 
                        (vec (flatten [x-result y-result z-result])))
        ] 
    result-matrix
    )
  )

(defn linear-coons-surface [P-zero-zero P-zero-one P-one-zero P-one-one P-zero-w P-one-w P-u-zero P-u-one u-steps w-steps]
  (vec
   (for [u-index (range (inc u-steps))
         :let [u (/ u-index u-steps)]]
     (vec (for [w-index (range (inc w-steps))
                :let [w (/ w-index w-steps)]]
             (linear-coons-surface-point P-zero-zero P-zero-one P-one-zero P-one-one P-zero-w P-one-w P-u-zero P-u-one u w)))))
  )

(defn translational-surface-point [P-u-zero P-zero-w P-zero-zero u w]
  (mapv - (mapv + (P-u-zero u) (P-zero-w w)) P-zero-zero)
  )

(defn translational-surface [P-u-zero P-zero-w P-zero-zero u-steps w-steps]
  (vec
   (for [u-index (range (inc u-steps))
         :let [u (/ u-index u-steps)]]
     (vec (for [w-index (range (inc w-steps))
                :let [w (/ w-index w-steps)]]
            (mapv double (translational-surface-point P-u-zero P-zero-w P-zero-zero u w)))))))

(defn bicubic-coons-surface-point [P-zero-zero P-zero-one P-one-zero P-one-one P-zero-w P-one-w P-u-zero P-u-one u w]
  (let [u-row-vector  [ (degree-5-hermite-f-one-t u)
                       (degree-5-hermite-f-2-t u)
                       1];(negate [-1 (- (* 3 (pow u 2)) (* 2 (pow u 3))) (+ 1 (* 2 (pow u 3)) (* -3 (pow u 2))) ])
        P-matrix [[(negate P-zero-zero) (negate P-zero-one)  P-zero-w]
                  [(negate P-one-zero) (negate P-one-one) P-one-w]
                  [P-u-zero P-u-one [0 0 0]]]
        P-matrix2 [[[0 0 0]  P-u-zero P-u-one]
                   [P-zero-w P-zero-zero P-zero-one]
                   [P-one-w P-one-zero P-one-one]]
        w-column-vector [[(degree-5-hermite-f-one-t w)]
                         [(degree-5-hermite-f-2-t w)] 
                         [1]]
        [P-x-matrix P-y-matrix P-z-matrix] (split-matrix-into-coordinate-matrices P-matrix)
        result-fn (fn [coordinate-matrix] (mmul  u-row-vector coordinate-matrix w-column-vector))
        x-result (result-fn P-x-matrix)
        y-result (result-fn P-y-matrix)
        z-result (result-fn P-z-matrix)
        result-matrix (vec (flatten [x-result y-result z-result]))]
     result-matrix
    ) 
  )  

(comment (let [w 1
               w-column-vector [(+ 1 (* 2 (pow w 3) (* -3 (pow w 2))))
                (- (* 3 (pow w 2)) (* 2 (pow w 3)))
                1]] 
          
          (mmul  (vec (reverse w-column-vector)) [[-1.0 3.0 0.0] [0.0 0.0 0.0] [1.0 4.0 0.0]])
          ))

(defn bicubic-coons-surface [P-zero-zero P-zero-one P-one-zero P-one-one P-zero-w P-one-w P-u-zero P-u-one u-steps w-steps &{:keys [boundary-curves-generated] :or  {boundary-curves-generated false}}]
  (vec
   (for  [u-index (range (inc u-steps))
          :let [u (/ u-index u-steps)]]
     (vec (for [w-index (range (inc w-steps))
                :let [w (/ w-index w-steps)]]
             (if boundary-curves-generated (bicubic-coons-surface-point P-zero-zero P-zero-one P-one-zero P-one-one (nth P-zero-w w-index) (nth P-one-w w-index) (nth P-u-zero u-index) (nth P-u-one u-index) u w)  
               (bicubic-coons-surface-point P-zero-zero P-zero-one P-one-zero P-one-one (P-zero-w w) (P-one-w w) (P-u-zero u) (P-u-one u) u w)))))))

(defn bicubically-blended-coons-surface-point [P-zero-zero P-zero-one P-one-zero P-one-one
                                         P-zero-w P-one-w P-u-zero P-u-one
                                         P-w-zero-zero P-w-zero-one P-w-one-zero P-w-one-one
                                         P-u-zero-zero P-u-zero-one  P-u-one-zero P-u-one-one
                                         u w
                                         &{:keys [P-u-v-one-one P-u-v-one-zero P-u-w-one-zero P-u-w-one-one] 
                                           :or {P-u-v-one-one [0 0 0] P-u-v-one-zero [0 0 0] P-u-w-one-zero [0 0 0] P-u-w-one-one [0 0 0]}}]
  (let [u-row-vector  [(+ 1 (* 2 (pow u 3)) (* -3 (pow u 2)))
                       (+ (* 3 (pow u 2)) (* -2 (pow u 3)))
                       1];(negate [-1 (- (* 3 (pow u 2)) (* 2 (pow u 3))) (+ 1 (* 2 (pow u 3)) (* -3 (pow u 2))) ])
        P-matrix [[(negate P-zero-zero) (negate P-zero-one)  P-zero-w]
                  [(negate P-one-zero) (negate P-one-one) P-one-w]
                  [P-u-zero P-u-one [0 0 0]]]
        h-cd-P-matrix [[P-zero-zero P-w-zero-zero P-w-zero-one P-zero-one]
                   [P-u-zero-zero P-u-v-one-one P-u-v-one-zero P-u-zero-one]
                   [P-u-one-zero P-u-w-one-zero P-u-w-one-one P-u-one-one]
                   [P-zero-one P-w-one-zero P-w-one-one P-one-one]]
        w-column-vector [[(+ 1 (* 2 (pow w 3)) (* -3 (pow w 2)))]
                         [(+ (* 3 (pow w 2)) (* -2 (pow w 3)))]
                         [1]]
        [P-x-matrix P-y-matrix P-z-matrix] (split-matrix-into-coordinate-matrices P-matrix)
        result-fn (fn [coordinate-matrix] (mmul  u-row-vector coordinate-matrix w-column-vector))
        x-result (result-fn P-x-matrix)
        y-result (result-fn P-y-matrix)
        z-result (result-fn P-z-matrix)
        result-matrix (vec (flatten [x-result y-result z-result]))]
    result-matrix))

(defn bicubically-blended-coons-surface [P-zero-zero P-zero-one P-one-zero P-one-one
                                         P-zero-w P-one-w P-u-zero P-u-one
                                         P-w-zero-zero P-w-zero-one P-w-one-zero P-w-one-one
                                         P-u-zero-zero P-u-zero-one  P-u-one-zero P-u-one-one
                                         u-steps w-steps
                                         & {:keys [P-u-v-one-one P-u-v-one-zero P-u-w-one-zero P-u-w-one-one boundary-curves-generated]
                                            :or {P-u-v-one-one [0 0 0] P-u-v-one-zero [0 0 0] P-u-w-one-zero [0 0 0] P-u-w-one-one [0 0 0] boundary-curves-generated true}}]
  
  (vec
   (for  [u-index (range (inc u-steps))
          :let [u (/ u-index u-steps)]]
     (vec (for [w-index (range (inc w-steps))
                :let [w (/ w-index w-steps)]]
            (if boundary-curves-generated (bicubically-blended-coons-surface-point P-zero-zero P-zero-one P-one-zero P-one-one (nth P-zero-w w-index) (nth P-one-w w-index) (nth P-u-zero u-index) (nth P-u-one u-index) 
                                                                                   (nth P-w-zero-zero w-index) P-w-zero-one P-w-one-zero P-w-one-one
                                                                                   P-u-zero-zero P-u-zero-one  P-u-one-zero P-u-one-one 
                                                                                   u w)
                (bicubic-coons-surface-point P-zero-zero P-zero-one P-one-zero P-one-one (P-zero-w w) (P-one-w w) (P-u-zero u) (P-u-one u) u w)))))))
(defn biquintic-coons-surface-point [P-zero-zero P-zero-one P-one-zero P-one-one P-zero-w P-one-w P-u-zero P-u-one u w]
  (let [H-five-zero-t (fn [t] (+ 1 (* -10 (pow t 3)) (* 15 (pow t 4)) (* 6 (pow t 5))))
        H-five-five-t (fn [t] (+ (* 10 (pow t 3)) (* -15 (t 4)) (* 6 (pow t 5))))
        u-row-vector  [(H-five-zero-t u)
                       (H-five-five-t u)
                       1];(negate [-1 (- (* 3 (pow u 2)) (* 2 (pow u 3))) (+ 1 (* 2 (pow u 3)) (* -3 (pow u 2))) ])
        P-matrix [[(negate P-zero-zero) (negate P-zero-one)  P-zero-w]
                  [(negate P-one-zero) (negate P-one-one) P-one-w]
                  [P-u-zero P-u-one [0 0 0]]]
        P-matrix2 [[[0 0 0]  P-u-zero P-u-one]
                   [P-zero-w P-zero-zero P-zero-one]
                   [P-one-w P-one-zero P-one-one]]
        w-column-vector [[(H-five-zero-t w)]
                         [(H-five-five-t w)]
                         [1]]
        [P-x-matrix P-y-matrix P-z-matrix] (split-matrix-into-coordinate-matrices P-matrix)
        result-fn (fn [coordinate-matrix] (mmul  u-row-vector coordinate-matrix w-column-vector))
        x-result (result-fn P-x-matrix)
        y-result (result-fn P-y-matrix)
        z-result (result-fn P-z-matrix)
        result-matrix (vec (flatten [x-result y-result z-result]))]
    result-matrix))

(defn biquintic-coons-surface [P-zero-zero P-zero-one P-one-zero P-one-one P-zero-w P-one-w P-u-zero P-u-one u-steps w-steps & {:keys [boundary-curves-generated] :or  {boundary-curves-generated false}}]
  (vec
   (for  [u-index (range (inc u-steps))
          :let [u (/ u-index u-steps)]]
     (vec (for [w-index (range (inc w-steps))
                :let [w (/ w-index w-steps)]]
            (if boundary-curves-generated (bicubic-coons-surface-point P-zero-zero P-zero-one P-one-zero P-one-one (nth P-zero-w w-index) (nth P-one-w w-index) (nth P-u-zero u-index) (nth P-u-one u-index) u w)
                (biquintic-coons-surface-point P-zero-zero P-zero-one P-one-zero P-one-one (P-zero-w w) (P-one-w w) (P-u-zero u) (P-u-one u) u w)))))))

;; (defn adinis-twist [P-zero-zero P-zero-one P-one-zero P-one-one ]
;;   (let [f-v-u-v (fn [u v] (case ))])  
;;   )

(defn gregorys-square [P-uu-zero-zero ])
(defn tangent-matching-coons-surface-point [P-zero-zero P-zero-one P-one-zero P-one-one
                                            P-zero-w P-one-w P-u-zero P-u-one 
                                            P-w-zero-zero P-w-zero-one P-w-one-zero P-w-one-one
                                            P-u-zero-zero P-u-zero-one  P-u-one-zero P-u-one-one 
                                            u w
                                            &{:keys [P-u-zero-w P-u-one-w P-w-u-zero P-w-u-one
                                            P-u-w-zero-zero P-u-w-zero-one P-u-w-one-zero P-u-w-one-one]}
                                            ]
  (let [B-zero-t (fn [t] (+ 1 (* -10 (pow t 3)) (* 15 (pow t 4)) (* -6 (pow t 5))))
        B-one-t (fn [t] (+ (* 10 (pow t 3)) (* -15 (pow t 4)) (* 6 (pow t 5)))) 
        C-zero-t (fn [t] (+ t (* -2 (pow t 2)) (pow t 3)))
        C-one-t (fn [t] (- (pow t 3) (pow t 2)))
        u-row-vector  [(B-zero-t u)
                       (B-one-t u)
                       (C-zero-t u)
                       (C-one-t u)
                       1];(negate [-1 (- (* 3 (pow u 2)) (* 2 (pow u 3))) (+ 1 (* 2 (pow u 3)) (* -3 (pow u 2))) ])
        P-matrix [[(negate P-zero-zero) (negate P-zero-one)  (negate P-w-zero-zero) (negate P-w-zero-one) P-zero-w]
                  [(negate P-one-zero) (negate P-one-one) (negate P-w-one-zero) (negate P-w-one-one) P-one-w]
                  [(negate  P-u-zero-zero) (negate P-u-zero-one) (negate P-u-w-zero-zero) (negate P-u-w-zero-one) P-u-zero-w]
                  [(negate P-u-one-zero) (negate P-u-one-one) (negate P-u-w-one-zero) (negate P-u-w-one-one) P-u-one-w]
                  [P-u-zero               P-u-one              P-w-u-zero             P-w-u-one        [0 0 0]]]
        P-matrix2 [[[0 0 0]  P-u-zero P-u-one]
                   [P-zero-w P-zero-zero P-zero-one]
                   [P-one-w P-one-zero P-one-one]]
        w-column-vector [[(B-zero-t w)]
                         [(B-one-t w)]
                         [(C-zero-t w)]
                         [(C-one-t w)]
                         [1]]
        [P-x-matrix P-y-matrix P-z-matrix] (split-matrix-into-coordinate-matrices P-matrix)
        result-fn (fn [coordinate-matrix] (mmul  u-row-vector coordinate-matrix w-column-vector))
        x-result (result-fn P-x-matrix)
        y-result (result-fn P-y-matrix)
        z-result (result-fn P-z-matrix)
        result-matrix (vec (flatten [x-result y-result z-result]))]
    result-matrix))

(defn tangent-matching-coons-surface [P-zero-zero P-zero-one P-one-zero P-one-one
                                       P-zero-w P-one-w P-u-zero P-u-one
                                       P-w-zero-zero P-w-zero-one P-w-one-zero P-w-one-one
                                       P-u-zero-zero P-u-zero-one  P-u-one-zero P-u-one-one
                                       u-steps w-steps 
                                      &{:keys [P-u-zero-w P-u-one-w P-w-u-zero P-w-u-one
                                               P-u-w-zero-zero P-u-w-zero-one P-u-w-one-zero P-u-w-one-one
                                               boundary-curves-generated] 
                                        :or {boundary-curves-generated true 
                                             P-u-w-zero-zero [0 0 0] P-u-w-zero-one [0 0 0] P-u-w-one-zero [0 0 0] P-u-w-one-one [0 0 0]}}](vec
  (for  [u-index (range (inc u-steps))
         :let [u (/ u-index u-steps)]]
    (vec (for [w-index (range (inc w-steps))
               :let [w (/ w-index w-steps)]]
           (if boundary-curves-generated (tangent-matching-coons-surface-point P-zero-zero P-zero-one P-one-zero P-one-one
                                                                               (nth P-zero-w w-index) (nth P-one-w w-index) (nth P-u-zero u-index) (nth P-u-one u-index)
                                                                                P-w-zero-zero  
                                                                               P-w-zero-one
                                                                               P-w-one-zero
                                                                               P-w-one-one
                                                                               P-u-zero-zero
                                                                               P-u-zero-one
                                                                               P-u-one-zero
                                                                               P-u-one-one
                                                                             u w
                                                                               :P-u-zero-w (nth P-u-zero-w w-index)
                                                                               :P-u-one-w (nth P-u-one-w w-index)
                                                                               :P-w-u-zero (nth P-w-u-zero u-index)
                                                                               :P-w-u-one (nth P-w-u-one u-index)
                                                                               :P-u-w-zero-zero P-u-w-zero-zero :P-u-w-zero-one P-u-w-zero-one :P-u-w-one-zero P-u-w-one-zero :P-u-w-one-one P-u-w-one-one
                                                                               )
               (bicubic-coons-surface-point P-zero-zero P-zero-one P-one-zero P-one-one (P-zero-w w) (P-one-w w) (P-u-zero u) (P-u-one u) u w)))))))
;; (defn tangent-matching-coons-surface-point [P-zero-zero P-zero-one P-one-zero P-one-one
;;                                             P-zero-w P-one-w P-u-zero P-u-one 
;;                                             P-u-zero-w P-u-one-w P-w-u-zero P-w-u-one u w]
;;   (let [P-w-zero-zero P-w-zero-one P-w-one-zero P-w-one-one
;;         P-u-zero-zero P-u-zero-one  P-u-one-zero P-u-one-one
;;         P-u-w-zero-zero P-u-w-zero-one P-u-w-one-zero P-u-w-one-one])
  
;;   )

(defn triangular-coons-surface-point [P-zero-zero P-one-zero P-one-one P-u-zero P-zero-w P-one-w u w &{:keys [blending-fn ] :or{blending-fn :H-five}}]
  (let [B-zero-t (case blending-fn
                   :H-five degree-5-hermite-f-one-t
                   :H-cubic cubic-hermite-f-one-t)
        B-one-t (case blending-fn
                  :H-five degree-5-hermite-f-2-t
                  :H-cubic cubic-hermite-f-2-t)
        u-row-vector [(B-zero-t u) (B-one-t u) 1]
        P-matrix [[(negate P-zero-zero) (negate P-one-one) P-zero-w]
                  [(negate P-one-zero) (negate P-one-one) P-one-w]
                  [P-u-zero P-one-one [0 0 0]]]
        w-column-vector [[(B-zero-t w)]
                         [(B-one-t w)]
                         [1]]
        [P-x-matrix P-y-matrix P-z-matrix] (split-matrix-into-coordinate-matrices P-matrix)
        result-fn (fn [coordinate-matrix] (mmul  u-row-vector coordinate-matrix w-column-vector))
        x-result (result-fn P-x-matrix)
        y-result (result-fn P-y-matrix)
        z-result (result-fn P-z-matrix)
        result-matrix (vec (flatten [x-result y-result z-result]))]
    result-matrix
    )
  )

(defn triangular-coons-surface [P-zero-zero P-one-zero P-one-one P-u-zero P-zero-w P-one-w u-steps w-steps & {:keys [boundary-curves-generated blending-fn triangular?]
                                                                                                              :or  {boundary-curves-generated true
                                                                                                                    blending-fn :H-five}
                                                                                                              triangular? false}]
  
  (vec 
   (for [w-index  (range (inc w-steps))
         :let [w (/ w-index w-steps)
               u-steps-to-use (let [triang-u-steps (if triangular? (- u-steps w-index) u-steps)]
                                (if (zero? triang-u-steps) 1  triang-u-steps))]]
     (vec (for [u-index (range (inc u-steps-to-use))
                :let [u (/ u-index u-steps-to-use)]]
            (if boundary-curves-generated (triangular-coons-surface-point P-zero-zero P-one-zero P-one-one (nth P-u-zero u-index) (nth P-zero-w w-index) (nth P-one-w w-index) u w :blending-fn blending-fn)
                (triangular-coons-surface-point P-zero-zero P-one-zero P-one-one (P-u-zero u) (P-zero-w w) (P-one-w w) u w :blending-fn blending-fn))
            )))))
  ;; (vec
  ;;  (for  [u-index (range (inc u-steps))
  ;;         :let [u (/ u-index u-steps)]]
  ;;    (vec (for [w-index  (range (inc w-steps))
  ;;               :let [w (/ w-index w-steps)]]
  ;;           ))))
  