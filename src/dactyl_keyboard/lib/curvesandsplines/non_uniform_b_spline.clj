(ns dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline
  (:require [clojure.core.matrix :refer [cross div dot log magnitude mmul mul]]
            [clojure.math :refer [asin floor pow sqrt]]
            [dactyl-keyboard.lib.curvesandsplines.beziers :refer [n-degree-bezier-point]]
            [dactyl-keyboard.lib.curvesandsplines.curve-utils :refer [homogenize-cooridinates
                                                                      homogenize-single-point
                                                                      project-coordinate
                                                                      project-coordinate-and-drop-weight
                                                                      project-coordinates-and-split-weights]]
            [dactyl-keyboard.lib.general-maths :refer [all-binomials-for-n
                                                       binomial-coefficient-2
                                                       factorial]])
  )

(defn bernstein-polynomial
  ""
  [i n u ]
  (let [temp (double-array (inc n) 0.0)
        u1 (- 1.0 u)]
    (aset temp (- n i) 1.0)
    (doseq [k (range 1 (inc n))]
      (loop [j n]
        (if (< j k)
          (do 
            (aset temp j (+ (* u1 (aget temp j)) (* u (aget temp (dec j)))))
            (recur (dec j))))
        ))
    (aget temp n)
    )
  )


(defn calculate-knot-span-index [n p u U]
  "calculates i - the knot span index
   is Algorithm A2.1 FindSpan from p68 of the nurbs book
    n - number of control points - 1
    p - spline degree
    u - parametric point
    U - knot sequence"
  (let [close (fn [a b] (< (abs (- a b)) 1e-10))]
    (cond (close  u  (double (nth U (inc n))))  n
          (> u (nth U (inc n))) n
          (< u (nth U p)) p
          :else
          (loop [low p
                 high (+ n 1)] (let [mid (int (Math/floor (/ (+ low high) 2.0)))]
                                 (if (or (< u (nth U mid)) (>= u (nth U (inc mid))))
                                   (if (< u (nth U mid))
                                     (recur low mid)
                                     (recur mid high))
                                   mid))) 
          )))


(defn ders-basis-funs [i u p n U]
  (let [ndu (into-array (map double-array (repeat (inc p) (repeat (inc p) 0.0))))
        left (double-array (inc p) 0.0)
        right (double-array (inc p) 0.0) 
        ]
    (aset ndu 0 0 1.0)
    (doseq [j (range 1  (inc p))]
      (aset left j (double (- u (nth U (- (inc i) j))))) 
       (aset right j (double (- (nth U (+ i j)) u)))  
      (let [saved-outer (loop [ saved 0.0 r 0]
        (if (< r j)
          (do (aset ndu j r (+ (aget right (inc r)) (aget left (- j r))))
          (let [temp (/ (aget ndu r (dec j)) (aget ndu j r))]
            (aset ndu r j (+ saved (* (aget right (inc r)) temp)))
            (recur (* (aget left (- j r)) temp) (inc r))
            )) 
          saved
          ) 
        )] 
        (aset ndu j j saved-outer))
           )
    (let [ders (into-array (map double-array (repeat (inc n) (repeat (inc p) 0.0))))
          a (into-array (map double-array (repeat 2 (repeat (inc p) 0.0))))] 
      (doseq [j (range (inc p))]
             (aset ders 0 j (aget ndu j p))
             )
      (doseq [r (range (inc p))
              :let [s1 0 s2 1 ]]
             (aset a 0 0 1.0) 
             (loop [ s1-inner s1 s2-inner s2 k 1] 
               (let [d (double-array 1 0.0)]
                 (aset d 0 0.0)
                    (if (<= k n)
                      (let 
                       [rk (- r k) pk (- p k)]
                        (if (>= r k) 
                          (do (aset a s2-inner 0 (/ (aget a s1-inner 0) (aget ndu (inc pk) rk)))
                              (aset d 0 (* (aget a s2-inner 0) (aget ndu rk pk)))
                              ))
                        (let [j1 (if (>= rk -1) 1 (- rk))
                              j2 (if (<= (dec r) pk) (dec k) (- p r))]
                          (doseq [j (range j1 (inc j2))]
                            (aset a s2-inner j (/ (- (aget a s1-inner j) (aget a s1-inner (dec j))) (aget ndu (inc pk) (+ rk j))))
                            (aset d 0 (+ (aget d 0) (* (aget a s2-inner j) (aget ndu (+ rk j) pk))))
                            )
                          (if (<= r pk)
                            (do (aset a s2-inner k (double (/ (- (aget a s1-inner (dec k))) (aget ndu (inc pk) r))))
                                (aset d 0 (+ (aget d 0) (double (* (aget a s2-inner k) (aget ndu r pk))))))
                            )
                          
                          )
                        (aset ders k r (aget d 0))
                        (aset d 0 0.0)
                        (let [j s1-inner]
                          (recur s2-inner j (inc k)))
                        )))
                    ) 
             )  
      (loop [r p k 1]
        (if (<= k n)
          (do
            (doseq [j (range (inc p))
                    :let [ders-k-j (aget ders k j)]]
              (aset ders k j (* ders-k-j r)))
            (recur (* (- p k) r) (inc k)))
          (mapv vec ders)))
      )
    )
  )


(defn curve-derivs-alg1 [n p U P u d]
  (let [du (min d p) 
        dimensions (count (nth P 0))
        zero-vec (vec (repeat  dimensions 0.0))
        CK (into-array (repeat (inc d) zero-vec))
        span (calculate-knot-span-index n p u U)
        nders (ders-basis-funs span u p du U)

        ] 
    (doseq [k (range (inc du))]
      (aset CK k zero-vec)
      (doseq [j (range (inc p))] 
             (aset CK k (mapv + (aget CK k) 
                           (mapv (partial * (get-in nders [k j])) (nth P (+ (- span p) j))))))
      )
    (vec CK)
    )
  )








(comment 
  (let [u 2.5 p 2 U [0 0 0 1 2 3 4 4 5 5 5]
        m (dec (count U))
        n (dec (- m p))
        i (calculate-knot-span-index n p u U)
        N (calculate-non-vanishing-basis-functions i u p U)
        ]
    (println "i " i )
    (println "N" N)
    (println "n" n)
   (ders-basis-funs i u p n U)
    )
  )
(comment 
  (to-array-2d (repeat (inc 3) (repeat (inc 3) 0.0)))
      
  )
(defn nip [i u p U]
  (let [m (dec (count U))]
    (cond (or (and (= i 0) (= u (nth U 0)))
              (and (= i (- m p 1)) (+ u (nth U m)))) 1.0
          (or (< u (nth U i)) (>= u (nth U (+ i p 1)))) 0.0
          :else
          (let [N (double-array
                   (for [j (range (inc p))]
                     (if (and (>= u (nth U (+ i j)))
                              (< u (nth U (+ i j 1))))
                       1.0 0.0)))]
            (doseq [k (range 1 (inc p))
                    :let [saved (if (zero? (aget N 0)) 0.0
                                    (/ (* (- u (nth U i)) (aget N 0))  (- (nth U (+ i k)) (nth U i))))
                          saved-list (double-array (inc (+ p (- k) 1)) saved)]]

              (doseq [j (range 0 (+ p (- k) 1))
                      :let [Uleft (nth U (+ i j 1))
                            Uright (nth U (+ i j k 1))]]
                (if (zero? (aget N (inc j)))
                  (do (aset N j (aget saved-list j)) (aset saved-list (inc j) 0.0))
                  (let [temp (/ (aget N (inc j)) (- Uright Uleft))]  (aset N j (+ (aget saved-list j) (* (- Uright u) temp)))
                       (aset saved-list (inc j) (* (- u Uleft) temp))))))
            (get N 0)))))

;; (defn b-spline-basis [knots u degree i] 
;;   (if (and (<= degree 0) (< u (nth knots (+ i 1))) (>= u (nth knots i)))
;;     1
;;     (if (and (> degree 0) (= (nth knots i) (nth knots (+ i degree)))
;;       (* (b-spline-basis knots u (dec degree) i) (/ (- u (nth knots i)) (- (nth knots (+ i degree)) (nth knots i))))
;;       (+ (* (b-spline-basis knots u (dec degree) i) (/ (- u (nth knots i)) (- (nth knots (+ i degree)) (nth knots i))))
;;          (* (b-spline-basis knots u (dec degree) (inc i)) (/ (- (nth knots (+ i degree)) u) (- (nth knots (+ i degree)) (nth knots i))))
;;          )))



(defn calculate-non-vanishing-basis-functions 
  "Algorithm A2.2 from pg.70 of the Nurbs Book
   `i` is knot span index. See [[calculate-knot-span-index]]
   `u` is the parameter
   `p` is the degree
   `U` is the knot vector
   this function returns the non vanishing basis functions
   in contained in a vector"
  [i u p U]
  (let [N (double-array (inc p) 0.0)
        m (dec (count U))
        left (double-array  (inc p))
        right (double-array  (inc p))
        ;saved-list (double-array (inc p))
        ]
    (aset N 0 1.0)
    ;(cond (or (and (zero? i) (= u (nth U 0)))
     ;        (and (= i (- m p 1)) (= u (nth U m)))) 1.0
      ;   (or (< u (nth U i) ) (>= u (nth U (+ i p 1)))) 0.0
       ;  :else
    (doseq [j (range 1 (inc p))]
          ;  (println "u is " u)
      (aset left j (double (- u (nth U (- (inc i) j)))))
      (aset right j (double (- (nth U (+ i j)) u)))
        ;(aset saved-list j 0.0) 


      (loop [saved 0.0 r 0]
          ;(doall (dorun (map println N)))
          ;(doall (println ["left is " (aget left j) "right is " (aget right j)]))
        (if (< r j) (let [temp (/ (aget N r) (+ (double (aget right (inc r))) (aget left (- j r))))]
          ;(println (/ (aget N r)  (aget right (inc r))))
                      (aset N r (+ saved  (* (aget right (inc r)) temp)))
                      (recur (* (aget left (- j r)) temp) (inc r)))
            (aset N j saved)
          ;(println (aget N j))
            )))
      ;; (doseq [r (range 0 j)
      ;;         :let [temp (/ (aget N r) (+ (aget right (inc r)) (aget left (- j r))))]]
      ;;   (aset N r (+ (aget saved-list j) (* (aget right (inc r)) temp)))
      ;;   (aset saved-list j (* (aget left (- j r)) temp))
      ;;   )
      ;; (aset N j (aget saved-list j))
        ;)
    (vec N)))

(defn all-basis-funs [span u p U]
  (let [Nji (to-array-2d (repeat (inc span) (repeat (inc p) 0.0)))] 
    (doseq [i (range (inc p))
            :let [Nb (calculate-non-vanishing-basis-functions span u p U)]] 
      (doseq [j (range (inc p))]
             
        (aset Nji j i (nth Nb j))
             
             )
           ) 
    (mapv vec Nji)
    ) 
  )




(comment (let [points [[1 0 0] [1 1 0] [0 1 0]]
               n (dec (count points))
               p 2
               u 0
               weights [2 1 2]
               U [0 0 0 1 1 1]
               span (calculate-knot-span-index n p u U)
               basis-fun (calculate-non-vanishing-basis-functions span u p U)
               all-basis (all-basis-funs span u p U)] 
           (println "basis-fun" basis-fun) 
           (mapv vec all-basis)))

(defn calculate-non-uniform-b-spline-point
  "from p82 of the nurbs book
  `n` is the number of control points - 1
   `p` is the degree of the curve
   `U` is the knot vector
   'P' are the control points each a vector of numerical coordinates 
   and all contained in a vector
   'u' is the parameter of the point
  this function is the same as using [[calculate-nurbs-curve-point]] with all
   weights set to one"
  [n p U P u]

  (let [span (calculate-knot-span-index n p u U)
        N (calculate-non-vanishing-basis-functions span u p U)
        p-double (double p)]
    (loop [C [0.0 0.0 0.0] i 0]
      (if (<= i p) (recur (mapv + C (mapv (partial * (nth N i)) (nth P (+ (- span p-double) i)))) (inc i))
          C))))

(defn non-uniform-b-spline-segment [n p U P steps & {:keys [u-start u-end] :or {u-start 0 u-end (inc u-start)}}]
  (let [increment (/ (- u-end u-start) steps)] 
    (for [u (range u-start (+ u-end increment) increment)]
      (calculate-non-uniform-b-spline-point n p U P u))))

(defn non-uniform-b-spline [P p U steps & {:keys [drop-last-point-of-segment u-start u-end] :or {drop-last-point-of-segment true u-start 0 u-end nil}}]
  (let [number-of-points (count P) 
        number-of-segments (- number-of-points p) 
        increment (/ number-of-segments steps)
        range-end (if (nil? u-end) (+ number-of-segments increment) (+ u-end increment))
        n (- (count U) p 2);(dec number-of-points)
        ] 
    (vec (for [u (range u-start range-end increment)]
           (calculate-non-uniform-b-spline-point n p U P u)))
    ;; (into [] (apply concat (for [index (range 0 number-of-segments)]
    ;;                          (drop-last-point-if-not-last-segment (inc index)
    ;;                                                               (non-uniform-b-spline-segment n p U P steps :u-start index :u-end (inc index))))))
    ))



(defn calculate-nurbs-curve-point [n p U Pw u]
  (let [span (calculate-knot-span-index n p u U)
        N (calculate-non-vanishing-basis-functions span u p U)]

    (loop [Cw [0.0 0.0 0.0 0.0] j 0]

      (if (<= j p) (recur (mapv + Cw  (mapv (partial * (nth N j)) (nth Pw (+ span (- p) j)))) (inc j))
          (-> (project-coordinate Cw)
              (subvec 0 3))))))

(defn nurbs-segment [n p U Pw steps &{:keys [u-start u-end] :or {u-start 0 u-end (inc u-start)}}]
  (let [increment (/ (- u-end u-start) steps)]
    (for [u (range u-start (+ u-end increment) increment)]


      (do
        (calculate-nurbs-curve-point n p U Pw u)))))

(defn nurbs-with-homogenous-coordinates [Pw p U steps ]
  (let [number-of-points (count Pw) 
        number-of-segments (- number-of-points p) 
        n (- (count U) p 2)
        increment (/ number-of-segments steps)
        ]
    (into []  (for [u (range 0 (+ number-of-segments increment) increment)]
                (calculate-nurbs-curve-point n p U Pw u)))))

(defn nurbs [points p U weights steps ]
  (let [weighted-points (homogenize-cooridinates points weights)]
    (nurbs-with-homogenous-coordinates weighted-points p U steps)))

(defn curve-deriv-control-points [n p U P d & {:keys [r1 r2] :or {r1 0 r2 n}}]
  (let [r (- r2 r1)
        PK (into-array (repeat (inc d) (into-array (repeat (inc r) (vec (repeat (count (nth P 0)) 0.0))))))] 
    (doseq [i (range (inc r))]
      (aset PK 0 i (nth P (+ r1 i))))

    (doseq [k (range 1 (inc d))
            :let [tmp (inc (- p k))]]
      (doseq [i (range (inc (- r k)))]
        (aset PK k i (mapv (partial * tmp)  (mapv - (aget PK (dec k) (inc i)) (mapv #(/ % (- (nth U (inc (+ r1 i p))) (nth U (+ r1 i k))))
                                                                                    (aget PK (dec k) i))))))
           
           )(mapv vec  PK)))

(defn b-spline-deriv-curve [p U P d steps 
                            &{:keys [u-start u-end] :or {u-start 0 u-end nil}}]
  (let [n (dec (count P))
        control-points (nth (curve-deriv-control-points n p U P d) d)
        knot-vector (subvec U 1 (dec (count U))) 
        ] 
    (non-uniform-b-spline control-points (dec p) knot-vector steps :u-start u-start :u-end u-end)
    )
  )

(comment (subvec [0 1 2 3 4 5 6 7 8] 1 (dec (count [0 1 2 3 4 5 6 7 8]))))


(defn nurbs-first-derivative-point [n p U Pw u d]
  (let [w-list (mapv #(vector (nth % 3)) Pw)
        P (mapv #(subvec % 0 3) Pw)
        Au-deriv (curve-derivs-alg1 n p U Pw u d)
        w-u (calculate-non-uniform-b-spline-point n p U w-list u)
        w-u-deriv  (vec (flatten (curve-derivs-alg1 n p U w-list u d)))
        C-u (calculate-non-uniform-b-spline-point n p U P u)]
    
      (mapv - Au-deriv 
           (mapv (partial *  (nth w-u-deriv 0)) C-u)
           )
    ))

(comment (let [U [0 0 0 1 1 1]
               points [[1 0 0] [1 1 0] [0 1 0]]
               weights [1 1 2]
               Pw (homogenize-cooridinates points weights)]
           (nurbs-first-derivative-point 2 2 U Pw 0 1) ))


(defn curve-derivs-alg2 [n p U P u d]
  (let [du (min d p)
        span (calculate-knot-span-index n p u U)
        N (all-basis-funs span u p U)
        zero-vec (vec (repeat (count (nth P 0)) 0.0))
        PK (curve-deriv-control-points n p U P du :r1 (- span p) :r2 span)
        CK (into-array (repeat (inc d) zero-vec) )]
    (doseq [k (range (inc du))]
      (aset CK k zero-vec)
      (doseq [j (range (inc (- p k)))] 
        (aset CK k (mapv + (aget CK k) (mapv (partial * (get-in N [j (- p k)])) (get-in PK [k j]))))))
    (vec CK)))

(defn nurbs-deriv-deboor [n p U P u weights]
  (let [span (calculate-knot-span-index n p u U)
       zero-vec (vec (repeat (inc (count (nth P 0))) 0.0))
        d (into-array (repeat (inc p) zero-vec))
        q (into-array (repeat (inc p) zero-vec))] 
    (doseq [i (range (inc p))
            :let [Pw-i-plus-k-minus-p (homogenize-single-point (nth P  (- (+ i span) p)) (nth weights (- (+ i span) p)))]]
           (aset d i Pw-i-plus-k-minus-p)
           (if (< i p)
             (do (aset q i (mul (mapv - (homogenize-single-point (nth P (inc (- (+ i span) p))) (nth weights (inc (- (+ i span) p))))
                          Pw-i-plus-k-minus-p) p))
                 (aset q i (mapv #(/ % (- (nth U (inc (+ i span))) (nth U (inc (- (+ i span) p))))) (aget q i )))
                 ))
           )
    (doseq [r (range 1 (inc p))]
           (doseq [j (range p  (dec r) -1)
                   :let [alpha (- u (/ (nth U (- (+ j span) p)) 
                                       (- (nth U (- (inc (+ j span)) r)) (nth U (- (+ j span) p)))))]]
                  (aset d j (mapv + (mul (aget d (dec j)) (- 1 alpha)) (mul (aget d j) alpha)))
                  (if (and (< r p) (< j p))
                    (let [alpha-conditional (- u (/ (nth U (- (+ j span) (inc p))) 
                                                    (- (nth U (- (inc (+ j span)) r)) (nth U (- (+ j span) (inc p))))))]
                      (aset q j (mapv + (mul (aget q (dec j)) (- 1 alpha-conditional)) (mul (aget q j) alpha-conditional))))
                    )
                  )
           )
    (let [point-4-dim (aget d p)
          point-weight (last point-4-dim)
          point-3-dim (mapv #(/ % point-weight) (subvec point-4-dim 0 3)) 
          ]
      (mapv #(mapv double %)[point-3-dim 
       (mapv #(/ % (last (aget d p))) (mapv - (subvec (aget q (dec p)) 0 3 ) (mul point-3-dim (last (aget q (dec p))))))]))
    )
  )



(comment (for [j (range 4 2 -1)]
           j))

(defn rational-curve-derivs [a-derivs wderivs d]
  (let [zero-vec (vec (repeat (count (nth a-derivs 0)) 0.0))
        CK (into-array (repeat (inc d) zero-vec))
        all-binomials (vec (for [index (range (inc d))]
                        (all-binomials-for-n index (inc d))))]
    (doseq [k (range (inc d))
            :let [v (nth a-derivs k)
                  ;binomials (all-binomials-for-n k (inc k))
                  v-result (loop [v-inner v i 1]
                             (if (<= i k) (let [multiplier (* (binomial-coefficient-2 k i) (nth wderivs i))](recur
                                           (mapv - v-inner (mapv #(* multiplier  %) (aget CK (- k i))))
                                           (inc i)))
                                 v-inner))]] 
           (aset CK k (div v-result (nth wderivs 0)))
           )
    (vec CK)
    )
  )

(defn nurbs-derivs-from-homogenous-control-points [n p U Pw  u d ]
  (let [Cu-derivs (curve-derivs-alg1 n p U Pw u d)
        a-derivs (mapv #(subvec % 0 3) Cu-derivs)
        w-derivs (mapv last Cu-derivs)]
    (rational-curve-derivs a-derivs w-derivs d)))

(defn nurbs-derivs [n p U P weights u d]
  (let [Pw (homogenize-cooridinates P weights)]
    (nurbs-derivs-from-homogenous-control-points n p U Pw u d))
  )

(defn nurbs-first-derivative-curve [P p U weights steps]

  (let [number-of-points (count P)
        number-of-segments (- number-of-points p)
        ;steps-total (* steps number-of-segments) 
        n (- (count U) p 2);(dec number-of-points)
        increment (/ number-of-segments steps)
        d 1]
    (into []  (for [u (range 0 (+ number-of-segments increment) increment)]
                (nth (nurbs-derivs n p U P weights u d) d)))))
(defn c-deriv-zero [])

(comment 
  (let [points [[1 0 0] [1 1 0] [0 1 0]]
        n (dec (count points)) 
        p 2
        u 0
        weights [1 1 2]
        U [0 0 0 1 1 1]
        cu-derivs (curve-derivs-alg1 n p U (homogenize-cooridinates points weights) u 2) 
        cu-derivs-2 (curve-derivs-alg2 n p U (homogenize-cooridinates points weights) u 1)
        ;(curve-deriv-control-points 3 3 U (homogenize-cooridinates points weights) 1)
        
        a-derivs (mapv #(subvec % 0 3) cu-derivs)
        w-derivs (mapv last cu-derivs) 
        deriv (nurbs-deriv-deboor n p U points u weights)
        rat-deriv (rational-curve-derivs a-derivs w-derivs 2)
        nurbs-derivssts (nurbs-derivs n p U  points weights u 2) 
        ]
    (println "a-derivs" a-derivs "w-derivs" w-derivs)
    (println "deriv" deriv)
    (println "cu-derivs-2 " cu-derivs-2)
    (println "rat-deriv" rat-deriv)
    (println "nurbs-derivssts" nurbs-derivssts)
    cu-derivs
    ))

(comment (ders-basis-funs 2 (/ 5 2) 2 7 [0 0 0 1 2 3 4 4 5 5 5]))


(comment (curve-deriv-control-points 2 2 [0 0 0 1 1 1] [[1 0 0 1] [1 1 0 1] [0 2 0 2]] 1))

;; (defn b-spline-point [points degree knot-vector t]
;;   (loop [i 0 x 0 y 0 z 0]
;;     (let [temp (Nip i degree knot-vector t)]
;;       (if (< i (count points))(recur (inc i) (+ x (* (nth points 0) temp)) (* (nth points 1) temp) (* (nth points 2) temp))
;;         [x y z]) 
;;       ) 
;;     )
;;   )

(defn exponential-parameterisation 
  "if alpha = 0 is  uniform parameterisation
   0.5 is centripetal parameterisation
   1 is chord length parameterisation"
  [n Q alpha]
  (let [qk-values (vec (for [k (range 1 (inc n))
                             :let [Qk (nth Q k)
                                   Qk-minus-one (nth Q (dec k))]]
                         (mapv - Qk Qk-minus-one)))
        ck-values (mapv magnitude qk-values)
        sum-chord-length-fn (fn [max-i] (reduce + (for [i (range 1 (inc max-i))
                                                        :let [ck (nth ck-values (dec i))]]
                                                    (pow ck alpha))))
        total-chord-length (sum-chord-length-fn n)]
    (vec (for [k (range (inc n))]
           (cond (zero? k) 0.0
                 (= n k) 1.0
                 :else (* (/ 1 total-chord-length)
                          (sum-chord-length-fn k))))))
  )

(defn calculate-uk-values [n numerator-list d]
  (let [uk-list (double-array  n 0.0)]
    (doseq [k (range 1 n)]
      (aset uk-list k (+ (aget uk-list (dec k)) (/ (nth numerator-list (dec k)) d))))
    (conj (vec uk-list) 1.0)))

(defn u-k-chordal [n Q]
  (let [magnitudes-of-qk-qk-minus-one (for [k (range 1 (inc n))]
                                        (sqrt (reduce + (mapv #(pow % 2) (mapv - (nth Q k) (nth Q (dec k)))))))
        d (reduce + magnitudes-of-qk-qk-minus-one)] 
    (calculate-uk-values n magnitudes-of-qk-qk-minus-one d)))

(defn u-k-centripetal [n Q]
  (let [sqrt-of-magnitudes-of-qk-qk-minus-one (for [k (range 1 (inc n))
                                                    :let [calvec (mapv #(pow % 2) (mapv - (nth Q k) (nth Q (dec k))))
                                                          reduced-calvec (reduce + calvec)
                                                          res (Math/sqrt (abs reduced-calvec))
                                                          res-sqrt (Math/sqrt res)]]
                                                res-sqrt)
        d (reduce + sqrt-of-magnitudes-of-qk-qk-minus-one)
        uk-list (double-array (inc n) 0.0)]

    (doseq [k (range 1 n)
            :let [u-k (+ (aget uk-list (dec k)) (/ (nth sqrt-of-magnitudes-of-qk-qk-minus-one (dec k)) d))]]
      (aset uk-list k u-k)) 
    (aset uk-list n 1.0) 
    (vec uk-list)))

(defn dynamic-centripetal-parameterisation 
  "n: number or data points -1
   Q: points that interpolated spline will pass through
   similar to centripetal paremeterisation except with dynamic exponents
   taken from C. Balta, S. Öztürk, M. Kuncan and I. Kandilli, \"Dynamic Centripetal Parameterization Method for B-Spline Curve Interpolation,\"
   in IEEE Access, vol. 8, pp. 589-598, 2020, doi: 10.1109/ACCESS.2019.2961412.
   "
  [n Q]
  (let [qk-values (vec (for [k (range 1 (inc n))
                      :let [Qk (nth Q k)
                            Qk-minus-one (nth Q (dec k))]]
                  (mapv - Qk Qk-minus-one)))
        ck-values (mapv magnitude qk-values) 
        chord-max (apply max ck-values)
        chord-min (apply min ck-values)
        e-max 0.35
        e-min 0.65 
        e-k-fn (fn [chord-k](+ (* (/ (log (/ chord-max chord-k))
                               (log (/ chord-max chord-min)))
                               (- e-max e-min)) e-min))
        e-k-values (vec (for [i (range 1 (inc n))
                              :let [ck (nth ck-values (dec i))]]
                          (e-k-fn ck))) 
        sum-chord-length-fn (fn [max-i] (reduce + (for [i (range 1 (inc max-i))
                                                        :let [ck (nth ck-values (dec i))
                                                              e-k (nth e-k-values (dec i))]]
                                                    (pow ck e-k)))) 
        total-chord-length (sum-chord-length-fn n) 
        
        ] 
    (vec (for [k (range (inc n))]
           (cond (zero? k) 0.0
                 (= n k) 1.0
                 :else (* (/ 1 total-chord-length) 
                          (sum-chord-length-fn k)))
           )))
  )

(comment (let [Q [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]]
               n (dec (count Q))
               u-k-c (u-k-centripetal n Q)
               u-k-dc (dynamic-centripetal-parameterisation n Q)]
           (println "u-k-c" u-k-c)
           u-k-dc
           ))

(defn u-k-average [Q p {:keys [n] :or {n (dec (count Q))}}]
  (let [m (+ n p 2)]
    (for [index (range (inc (+ m p 1)))]
      (cond 
        (<= index p) 0.0
        (and (> index p) (<= index (- m p))) 
        (let [i (- index p)]))
      )
    )
  )

(defn florez-parameterisation [n Q]
  (let [
        u-k-values (double-array (inc n) 0.0)
        ]
    (doseq [index (range (dec n))]
      (if (zero? index) (aset u-k-values index 0.0)
          (let [i (dec index)
                u-i (aget u-k-values i)
                Q-k (nth Q index)
                Q-k-minus-one (nth Q (dec index))
                Q-k-plus-one (nth Q (inc index))
                Q-k-plus-two (nth Q (+ index 2))]
            (aset u-k-values index 
                  (+ u-i (magnitude (mapv - Q-k Q-k-minus-one))
                     (magnitude (mapv - Q-k-plus-one Q-k))
                     (magnitude (mapv - Q-k-plus-two Q-k-plus-one)))))))
    (vec u-k-values)))

(defn calculate-average-radii [Q &{:keys [n] :or {n (dec (count Q))}}]
  (vec (for [k (range (dec n))
             :let [Qk (nth Q k)
                   Qk-plus-one (nth Q (inc k))
                   Qk-plus-two (nth Q (+ k 2))
                   Qk-plus-one-minus-Qk (mapv - Qk-plus-one Qk)
                   Qk-plus-two-minus-Qk (mapv - Qk-plus-two Qk)
                   is-unstable (< (asin (/ (magnitude (cross Qk-plus-one-minus-Qk Qk-plus-two-minus-Qk))
                                           (* (magnitude Qk-plus-one-minus-Qk) (magnitude Qk-plus-two-minus-Qk))))
                                  0.0002)]]
         
         (if is-unstable  :unstable
           (/ (* (magnitude Qk-plus-one-minus-Qk) (magnitude Qk-plus-two-minus-Qk) (magnitude (mapv - Qk-plus-two Qk-plus-one)))
            (* 2 (magnitude (cross Qk-plus-one-minus-Qk Qk-plus-two-minus-Qk))))))))

(defn calculate-circular-arc-span-length [Q qk-values &{:keys [n rk-values] :or {n (dec (count Q)) rk-values (calculate-average-radii Q n)} }] 
  (let [instability-detected (some #(= % :unstable) rk-values)
        l-k-values (vec (for [k (range n)]
         (let [Qk (nth Q k)
               Qk-plus-one (nth Q (inc k))
               Qk-plus-one-minus-Qk (mapv - Qk-plus-one Qk)]
           (cond instability-detected (magnitude Qk-plus-one-minus-Qk) 
           (= k (dec n)) (let [two-r-n-minus-2 (* 2 (last rk-values ))]
                               (* two-r-n-minus-2 (asin (/ (magnitude (mapv - (nth Q n) (nth Q (dec n))))
                                                           two-r-n-minus-2)))) 
               :else (let [
                        ;Qk-plus-two (nth Q (+ k 2))
                          
                           ;Qk-plus-two-minus-Qk (mapv - Qk-plus-two Qk)
                           ]
                       (cond 
                             (zero? k) (let [two-r-zero (* 2 (nth rk-values 0))
                                             q-mag (magnitude (mapv - (nth Q 1) (nth Q 0)))
                                             q-mag-over-two-r-zero (/ q-mag two-r-zero)
                                             asin-q-mag-over-two-r-zero (asin q-mag-over-two-r-zero)
                                             asin-q-mag-over-two-r-zero-times-two-r-zero (* asin-q-mag-over-two-r-zero two-r-zero)] 
                                         asin-q-mag-over-two-r-zero-times-two-r-zero)
                             :else (let [rk (nth rk-values k)
                                         rk-minus-one (nth rk-values (dec k))
                                         rk-plus-rk-minus-one (+ rk rk-minus-one)]
                                     (* rk-plus-rk-minus-one (asin (/ (magnitude (mapv - (nth Q (inc k)) (nth Q k)))
                                                                      rk-plus-rk-minus-one))))
                         )) 
               ))))]
               l-k-values))

(defn circular-arc-parameterisation [n Q]
  (let [qk-values (vec (for [k (range n)] (mapv - (nth Q (inc k)) (nth Q k))))
        rk-values (calculate-average-radii Q n)
        l-k-values (calculate-circular-arc-span-length Q qk-values :rk-values rk-values)
        u-k-values (double-array (+ n 1) 0.0)
        sum-lc (reduce + (for [c (range 1 n)] (nth l-k-values c)))] 
    (doseq [k (range 1 (inc n))]
          (aset u-k-values k (+ (aget u-k-values (dec k)) 
             (/ (nth l-k-values (dec k)) sum-lc)))) 
    (let [u-k-values-vec (vec u-k-values) 
          u-k-n (peek u-k-values-vec)]
      (mapv #(/ % u-k-n) u-k-values-vec))
    ) 
  )



(defn equally-spaced-parameterisation [n]
  (vec (for [k (range (inc n))]
         (/ k n)))
  )

(defn average-harmonic-parameterisation [n Q]
  (let [equally-spaced-u-k (equally-spaced-parameterisation n)
        chordal-u-k (u-k-chordal n Q)
        centripetal-u-k (u-k-centripetal n Q)]
    (vec (for [index (range (inc n))
               :let [equally-spaced-u (nth equally-spaced-u-k index)
                     chordal-u (nth chordal-u-k index)
                     centripetal-u (nth centripetal-u-k index)]]
           (if (zero? index) 0.0
               (/ 3
                  (+ (/ 1 equally-spaced-u) (/ 1 chordal-u) (/ 1 centripetal-u)))))) 
    ) 
  )
(defn average-geometric-parameterisation [n Q]
  (let [equally-spaced-u-k (equally-spaced-parameterisation n)
        chordal-u-k (u-k-chordal n Q)
        centripetal-u-k (u-k-centripetal n Q)] 
    (mapv #(pow (* %1 %2 %3) (/ 1 3)) equally-spaced-u-k chordal-u-k centripetal-u-k)
    ))

(defn farin-simple-parameterisation [n Q]
  (let [ck (for [k (range n)
                 :let [Qk (nth Q k)
                       Qk-minus-one (nth Q (inc k))]] 
             (magnitude (map - Qk-minus-one Qk)))
        uk-plus-one-fn (fn [ak uk uk-minus-one bk-minus-one]
                         (+ (/ (* ak (- uk uk-minus-one))
                            bk-minus-one) uk))
        a-k-values (mapv #(* 1.2 %) ck)
        u-k-values (double-array (inc n) 0.0) 
        ]
    (doseq [index (range (inc n))]
      (cond (zero? index ) (aset u-k-values 0 0.0)
            (= 1 index) (aset u-k-values 1 1.0)
            :else (let [k (dec index)
                        ak (nth a-k-values k)
                        bk-minus-one (nth a-k-values (dec k))
                        uk (aget u-k-values k)
                        uk-minus-one  (aget u-k-values (dec k))
                        uk-plus-one (uk-plus-one-fn ak uk uk-minus-one bk-minus-one)]
                    (aset u-k-values index uk-plus-one)))
      )
    (let [vec-u-k-values (vec u-k-values)
          last-u-k (peek vec-u-k-values)
          normalised-u-k-values (mapv #(/ % last-u-k) vec-u-k-values)]
       normalised-u-k-values)
)
  )



(comment (let [Q [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]]
               n 4
               p 3]
           (println (florez-parameterisation n Q))
           (println  "farin-simple-parameterisation" (farin-simple-parameterisation n Q))
           (u-k-centripetal n Q)))



(defn find-u [p U i n]
  (let [tolerance 1e-6  
        max-iter 1000
        u-start (nth U i)
        u-end (nth U (inc (+ i p)))]
    (loop [u u-start
           iter 0]
      (let [f (nth (nth (ders-basis-funs i u p n U) 1) p)
            abs-f (abs f)]
        (cond 
          (<= abs-f tolerance) u
          (>= iter max-iter) nil
          :else (let [ders (ders-basis-funs i u (dec p) n U)
                      ders-1 (nth ders 1)
                      ders-1-p (nth ders-1 p)] 
                  (recur (- u ders-1-p)
                       (inc iter)))))))
  )

(comment(let [Q [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]]
              n 4
              p 3
              u-k-values (u-k-chordal n Q)
              U (calculate-averaged-knot-vector-from-u-k u-k-values n p)
              span (calculate-knot-span-index n p 0.9 (div U (peek U)))] 
          (println "U" U)
          (println "span" span)
          (find-u p (div U (peek U)) span n)))



(comment (pow 27 (/ 1 3)))

(comment (asin 0.1) )

(defn calculate-equally-spaced-knot-vector-from-u-k [uk-values n p & {:keys [constrained] :or {constrained false}}]
  (let [num-segments (if constrained (+ (- n p) 3) (inc (- n p)))
        u-internal (vec (for [i (range 1 (if constrained (inc (- (+ n 2) p)) (inc (- n p))))]
                          (/ i (inc (- n p)))
                          )) 
        knot-vector (mapv double (mul num-segments
                                      (vec (concat (vec (repeat (inc p) 0.0))
                                                   u-internal (repeat (inc p) 1.0)))))
        m (if constrained (+ n p 3) (inc (+ n p))) 
        knot-vector (double-array (inc m) 0.0)
        ]
    (doseq [i (range (inc m))]
      (cond (<= i p) (aset knot-vector i 0.0)
            (>= i (- m p)) (aset knot-vector i (* 1.0 num-segments))
            :else (aset knot-vector i (/ i (inc (- n p)))))) 
    knot-vector
    ) 
  )

(comment (some #(= % 1) [0 2 3]))
(defn calculate-averaged-knot-vector-from-u-k [U-k n p] 
  (let [m (+ n p 1)
        knot-vector (double-array (inc m) 0.0)
        j-min 1
        j-max (- n p)
        num-segments (inc (- n p))]
    (doseq [index (range 0 (inc m))]
      (cond (< index (+ j-min p)) (aset knot-vector index 0.0)
            (and (>=  index (+ j-min p)) (<= index (+ j-max p))) (aset knot-vector index  (* (/ (reduce + (for [j (range 0 index)]
                                                                                                            (nth U-k j))) p) num-segments))
            (> index j-max) (aset knot-vector index (* 1.0 num-segments))))
    (vec knot-vector)))

(defn improved-hybrid-parameterisation [n Q p]
  (let [alpha 0.8
        u-k-initial (exponential-parameterisation n Q alpha)
        U-average (calculate-averaged-knot-vector-from-u-k u-k-initial n p)
        U-average-normalised (div U-average (peek U-average))
        theta-i-vals (vec (for [i (range 1 n)
                                :let [u (nth u-k-initial i)
                                      span (calculate-knot-span-index n p u U-average)
                                      basis-functions (calculate-non-vanishing-basis-functions span u p U-average)
                                      B-i (apply max basis-functions)
                                      degree-of-B-i (.indexOf basis-functions B-i)
                                      theta-i (find-u degree-of-B-i U-average span n)]]
                            theta-i))
        u-k-new (double-array (inc n) 0.0)
        i-max (if (zero? (mod n 2)) (/ n 2) (/ (dec n) 2))]

    (doseq [i (range 1 i-max)
            :let [theta-two-i (nth theta-i-vals  (* 2 i))
                  theta-two-i-minus-one (nth theta-i-vals (dec (* 2 i)))
                  u-two-i (nth u-k-initial  (* 2 i))
                  u-two-i-minus-one (nth u-k-initial (dec (* 2 i)))
                  m-theta-i (/ (+ theta-two-i-minus-one theta-two-i) 2)
                  m-u-i (/ (+ u-two-i-minus-one u-two-i) 2)
                  s-i (- m-theta-i m-u-i)]]
      (aset u-k-new (dec (* 2 i)) (+ u-two-i-minus-one s-i))
      (aset u-k-new (* 2 i) (+ u-two-i s-i)))
    (aset u-k-new 1 0.0)
    (aset u-k-new n 1.0)))

(comment (improved-hybrid-parameterisation 4 [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]] 3))
(defn calculate-natural-knot-vector-from-u-k [uk-values n p &{:keys [constrained] :or {constrained false}}]
  (let [num-segments (if constrained (+ (- n p) 3) (inc (- n p)))
        m (if constrained (+ n p 3) (inc (+ n p)))
        ;; U-internal (vec (for [i (range 1 (- n 2))]
        ;;              (if constrained (* (nth uk-values i) num-segments)
        ;;                  (* (nth uk-values (inc i)) num-segments))))
        knot-vector (double-array (inc m) 0.0)
        
        ]
    (doseq [i (range (inc m))]
      (cond (<= i p) (aset knot-vector i 0.0)
            (>= i (- m p)) (aset knot-vector i (* 1.0 num-segments))
            :else (aset knot-vector i (if constrained (* (nth uk-values (- i p)) num-segments)
                                          (* (nth uk-values (- (inc i) p)) num-segments)))))
    ;(vec ( concat (repeat (inc p ) 0.0 ) U-internal (repeat (inc p) (* 1.0 num-segments)) ))
    (vec knot-vector)
    )
  )

(comment (calculate-natural-knot-vector-from-u-k [0 0.1 0.3 0.7 0.3 1.0] 4 2 :constrained true) )
(comment (calculate-averaged-knot-vector-from-u-k [0 0.1 0.3 0.7 0.9 1.0] 4 2))

(defn calculate-averaged-knot-vector-from-u-k-with-end-derivs [U-k n p]
  (let [m (+ n p 3)
        knot-vector (double-array (inc m) 0.0)
        j-min 0
        j-max (inc (- n p))
        num-segments (+ (- n p) 3)]
    (doseq [index (range 0 (inc m))]
      (cond (<= index p)
            (aset knot-vector index 0.0)
            (and (> index p) (< index (- m p)))   (aset-double knot-vector index  (* (/ (reduce + (for [j (range (dec (- index p)) (inc (- index 2)))]
                                                                                                    (nth U-k j))) p) num-segments))
            (>= index (- m p))
            (aset knot-vector index (* 1.0 num-segments))))
    (vec knot-vector)))

(defn blah [m knot-vector p knot-fn num-segments]
  (doseq [index (range 0 (inc m))]
    (cond (<= index p)
          (aset knot-vector index 0.0)
          (and (> index p) (< index (- m p)))
          (let [j (- index p)]
            (println j)
            (aset-double knot-vector index (*  (knot-fn j) num-segments)))
          (>= index (- m p))
          (aset knot-vector index (* 1.0 num-segments)))))

(defn calculate-en [u-k n p &{:keys [constrained] :or {constrained false}}]
  (let [m (if constrained (+ n p 3) (inc (+ n p)))
        num-of-data-points (+ (inc n) 2)
        num-segments (+ (- n p) 3)
        knot-vector (double-array (inc m) 0.0)
        c (/ (+ num-of-data-points 2) (inc (- n p)))
        l-fn (fn [j] (floor (* j c)))
        alpha-fn (fn [j] (- (* j c) (l-fn j))) 
        knot-fn (fn [j] (let [alpha (alpha-fn j)
                              l (l-fn j)]
                          (println "l" l )
                          (+ (* (- 1 alpha) (nth u-k j))
                             (* alpha (nth u-k (inc j))))))]
    (blah m knot-vector p knot-fn num-segments)
    
    (vec knot-vector)
    )
  )

(comment [])

(defn piegl-and-tiller [u-k n p &{:keys [constrained] :or {constrained false}}]
  (let [m (inc (+ n p))
        num-segments (+ (- n p) 1)]
     (->>(for [i (range (inc m))]
       (cond (<= i p) 0.0
             (>= i (- m p)) 1.0
             :else (/ (reduce + (subvec u-k 0 (+ (- i p) 2))) p))
       )
      (vec)
      (mapv #(* num-segments %))
      )
    )
  (let [m (+ (+ n p) 3)
        num-segments (+ (- n p) 3)])
 
  )

(defn modified-knot-placement-technique [u-k n p & {:keys [constrained m] :or {constrained false m (if constrained (+ n p 3) (+ n p 1))}}]
  (println "m" m)
  (if constrained
    (let [;m (+ n p 3)
          num-segments (- m p)
          c (/ (+ m 2) (inc (- n p)))
          l-fn (fn [j] (floor (* j c)))
          alpha-fn (fn [j] (- (* j c) (l-fn j)))
          ]
      (println c)
      (vec (for [i (range (inc (+ m p 1)))]
        (cond (<= i p) 0.0
              (>= i (inc (- m p))) (* 1.0 )
              :else (let [j (- i p)
                          alpha (alpha-fn j)
                          l (l-fn j)
                          u-l (nth u-k j)
                          u-l-minus-one (nth u-k  (dec j))]
                      (* (+ (* (- 1 alpha) u-l-minus-one) (* alpha u-l)) 1.0)))))
      )
   (let [;m (inc (+ n p))
        num-segments (- m p)]
     (vec (for [i (range (inc m))]
           (cond (<= i p) 0.0
                 (>= i (- m p)) 1.0
                 :else (let [j (- i p)
                             ](* (/ (reduce + (subvec u-k j (+ j p))) p) 1.0))))
)))
  )

(defn unified-average-technique [u-k n p]
  (let [m (+ n p 1) 
        num-segments( + (- n p) 1)
        ]
    (vec (for [index (range 0 (inc m))]
      (cond (<= index p ) 0.0
            (>=  index (inc n)) num-segments
            :else (let [j (- index p)
                        multiplier (/ 1 (+ (- m n) p))
                        sum-end (+ m (- n) p -1 j)
                        u-sum (reduce + (subvec u-k j (inc sum-end)))]
                    (* multiplier u-sum num-segments)))))
    )
  )

(comment (let [u-k [0 0.1 0.3 0.7 0.9 1.0]
               av (calculate-averaged-knot-vector-from-u-k u-k 4 2)
               p (piegl-and-tiller u-k 4 2)]
           (println "p" p)
           av))

(comment (let [u-k [0.0 0.15 0.35 0.5 0.65 0.85 1.00]
               av (calculate-averaged-knot-vector-from-u-k-with-end-derivs u-k 6 3)
               mkpt (modified-knot-placement-technique u-k 6 3 :constrained true :m 6)
               ]
           (println av)
           mkpt
           ))

(comment (let [u-k [0 0.1 0.3 0.7 0.9 1.0]
               av (calculate-averaged-knot-vector-from-u-k-with-end-derivs u-k 4 2)
               en (calculate-en u-k 4 2 :constrained true)
               mkpt (modified-knot-placement-technique u-k 4 2 :constrained true)
               ;uavg (unified-average-technique u-k 4 2)
               ]
           
           (println "en" en)
           (println "mkpt" mkpt)
           ;(println "uavg" uavg)
           av
           ))
(comment (let [Q [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]]
               n 4
               p 3
               uk (u-k-chordal n Q)
               U [0 0 0 0 (/ 28 51) 1 1 1 1]
               span (calculate-knot-span-index n p (nth uk 0) U)]
           (println span)
           (println "uk" (count uk) uk)
           (println "natural" (calculate-natural-knot-vector-from-u-k uk n p))
           (println "average"(calculate-averaged-knot-vector-from-u-k uk n p)))
  )

(comment
  (let [Q [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]]
        n 4
        p 3
        uk (u-k-chordal n Q)
        U [0 0 0 0 (/ 28 51) 1 1 1 1]
        span (calculate-knot-span-index n p (nth uk 0) U)]
    (println span)
    (calculate-non-vanishing-basis-functions 4 1 p U)))

(defn riesenfield-even-knot-vector [L l-values p m n num-segments]
  (vec (for [index (range (inc m))] 
    (cond (<= index p) 0.0
          (>= index (- m p) ) (double num-segments )
          (= index (dec (- m p))) (* (/ (+ (reduce + (subvec l-values 1  (- n (/ p 2)))) (/ (nth l-values (- n (/ p 2))) 2)) L) num-segments)
          :else (*  (/ (+ (reduce + (subvec l-values 1 (inc (/ p 2))) ) (/ (nth l-values (+ (/ p 2) (- index p))) 2)) L) num-segments))
    )))

(defn riesenfield-odd-knot-vector [L l-values p m n num-segments]
  (vec (for [index (range (inc m))]
         (cond (<= index p) 0.0
               (>= index (- m p)) (double num-segments)
               (= index (dec (- m p))) (* (/(reduce + (subvec l-values 1  (inc (- n (/ (inc p) 2)))))  L) num-segments)
               :else (*  (/ (reduce + (subvec l-values 1 (inc (+ (/ (inc p) 2) (dec (- index p)))))) L) num-segments)
               ))))
(defn riesenfield-knot-vector
  "seems intended for chordal parameterisatin"
  [n p Q & {:keys [u-k constrained] :or {constrained false}}]
  (let [num-segments (if constrained (+ (- n p) 3) (inc (- n p)))
        m (if constrained (+ n p 3) (inc (+ n p)))
        qk-values (vec (for [k (range 1 (inc n))] (mapv - (nth Q k) (nth Q (dec k)))))
        l-values (if u-k u-k (mapv magnitude qk-values))
        L (reduce + l-values)]
    (if (even? p) (vec (riesenfield-even-knot-vector L l-values p m n num-segments))
        (vec (riesenfield-odd-knot-vector L l-values p m n num-segments)))))



(defn hartley-judd-method [n p Q &{:keys [u-k constrained] :or {constrained false}}] 
  (let [num-segments (if constrained (+ (- n p) 3) (inc (- n p)))
        m (if constrained (+ n p 3) (inc (+ n p)))
        qk-values (vec (for [k (range 1 (inc n))] (mapv - (nth Q k) (nth Q (dec k)))))
        l-values (if u-k u-k (mapv magnitude qk-values))
        ;L (reduce + l-values)
        ;; domain-length-fn (fn [i] (/ (reduce + (for [j (range (- i p) i) ]
        ;;                                         (nth l-values j)))
        ;;                             (reduce + (for [s (range  (inc p) (+ n 1)) 
        ;;                                   :let [sum-fn (fn [s] (reduce + (for [j (range (- s p) (dec s))] (nth l-values j))))]] 
        ;;                               (sum-fn s)))))
        ;; domain-lengths (vec (for [i (range (inc p ) (+ n 1)) ] (domain-length-fn i)))
        knot-fn (fn [i] (/ (reduce + (for [s (range (inc p) (inc i))]
                                       (reduce + (for [j (range (- s p) s)] (nth l-values (dec j))))))
                           (reduce + (for [s (range (inc p) (+ n 2))]
                                       (reduce + (for [j (range (- s p) s)] 
                                                   (nth l-values (dec j))
                                                   
                                                   ))
                                       ))
                           ))
        ]
    
    (vec (for [index (range (inc m))]
      (cond (<= index p) 0.0
            (>= index (- m p) ) (double num-segments)
            :else (* (knot-fn index) num-segments))
      ))
    )
  )

(defn rogers-method [n p Q & {:keys [u-k constrained] :or {constrained false}}]
  (let [num-segments (if constrained (+ (- n p) 3) (inc (- n p)))
        m (if constrained (+ n p 3) (inc (+ n p)))
        qk-values (vec (for [k (range 1 (inc n))] (mapv - (nth Q k) (nth Q (dec k)))))
        l-values (if u-k u-k (mapv magnitude qk-values)) 
        ]
    (vec (for [index (range (inc m))]
           (cond (<= index p ) 0.0
                 (>= index (- m p)) (double num-segments)
                 :else (* (/ (+ (* (/ (- index p) (inc (- n p))) (nth l-values  (- index p)))
                                (reduce + (for [j (range 1 (inc (- index p)))] (nth l-values (dec j))))) 
                             (reduce + (for [j (range 1 (inc n))] (nth l-values (dec j))))) num-segments))
           ))
    ))

(defn weighted-average-algorithm [n p Q & {:keys [u-k constrained] :or {constrained false}}]
  (let [num-segments (if constrained (+ (- n p) 3) (inc (- n p)))
        m (if constrained (+ n p 3) (inc (+ n p)))
        qk-values (vec (for [k (range 1 (inc n))] (mapv - (nth Q k) (nth Q (dec k)))))
        l-values (if u-k u-k (mapv magnitude qk-values))
        L (reduce + l-values)
        a (/ 1 L)
        k (/ 1 (reduce + (for [s (range (inc p) (+ n 2))]
                              (reduce + (for [j (range (- s p) s)] (nth l-values (dec j)))))))
        b-j-fn (fn [j i] (cond 
                           (or (= j 1) (= j (- i 1))) 1
                           (or (= j 2) (= j (- i 2))) 2
                           (or (= j 3) (and (>= j 4) (<= j (- 3)))) 3))
        u-i-fn (fn [i]
                 (let [row-one (vec (for [j (range 1 i)]
                                      (if (= j (dec i) ) 0.0 a)))
                       row-two (vec (for [j (range 1 i)]
                                      (cond (= j (- i 2)) (* (/ (- i 3) (- n 2)) a)
                                            (= j (dec i)) 0.0
                                            :else a)))
                       row-three (vec (for [j  (range 1 i)]
                                        (if (>= j (- i 3)) (* (b-j-fn j i) k) (b-j-fn j i))))
                       l-column-vector (vec (for [j (range 1 i)] (nth l-values (dec j))))]
                   (println row-one)
                   (reduce + (div (mmul [row-one
                          row-two
                          row-three] l-column-vector) 3)))
                 )
        ]
        (u-i-fn 7)))

(comment (let [Q [[0 0 0] [3 4 0] [5 2 1] [8 4 2] [7 2 1] [9 1 1]]
               n (dec (count Q))
               p 3
               uk (u-k-chordal n Q)]
         (weighted-average-algorithm n p Q)  )
         )
(comment (let [Q [[0 0 0] [3 4 0] [5 2 1] [8 4 2] [7 2 1] [9 1 1]]
               n (dec (count Q))
               p 3
               uk (u-k-chordal n Q)]
           (println (riesenfield-knot-vector n p  Q :u-k uk))
           (println (riesenfield-knot-vector n p  Q :u-k (u-k-centripetal n Q)))
           (println (riesenfield-knot-vector n p  Q :u-k (circular-arc-parameterisation n Q)))
           (println (riesenfield-knot-vector n p  Q))
           (println (hartley-judd-method n p Q))
           (rogers-method n p Q)) 
         )

(comment (let [Q [[0 0 0] [1 10 0] [10 11 0] [11 0 0]
                  [21 1 0] [22 12 0] [32 10 0] [35 0 0]]
               n (dec (count Q))
               p 3]
           (println (div (hartley-judd-method n p Q) 5.0))
           (println (div (riesenfield-knot-vector n p  Q ) 5.0)) 
           (div (rogers-method n p Q) 5.0)))
(defn de-boors-algorithm-knot-placement [u-k n p]
  (let [])
  )

(defn calculate-knot-vector [degree control-point-count is-uniform]
  (let [n control-point-count
        m (+ n degree 1)
        divisor (- m 1 (* 2 degree))]
    (if is-uniform (for [i (range 1 m)]
                     (if (>= i (dec m)) 1
                         (/ i (dec m))))
        (for  [i (range 1 m)]
          (cond (<= i degree) 0
                (>= i (- m degree 1)) 1
                :else (/ (- i degree) divisor))))))

(defn get-function-for-u-k-values [point-paramater-calculation-method]
  (case point-paramater-calculation-method
    :chordal u-k-chordal
    :equal (fn [n Q] (equally-spaced-parameterisation n))
    :centripetal u-k-centripetal
    :dynamic-centripetal dynamic-centripetal-parameterisation
    :circular circular-arc-parameterisation
    :average-geometric average-geometric-parameterisation
    :average-harmonic average-harmonic-parameterisation
    :farin-simple farin-simple-parameterisation
    :orthgonal-construction nil))

(defn get-knot-vector-generation-fn [knot-vector-generation-method-keyword & {:keys [constrained] :or {constrained false}}]
  (case knot-vector-generation-method-keyword
    :natural (fn [uk-values n p Q] (calculate-natural-knot-vector-from-u-k uk-values n p :constrained constrained))
    :average (if constrained (fn [uk-values n p Q] (calculate-averaged-knot-vector-from-u-k-with-end-derivs uk-values n p))
                 (fn [uk-values n p Q] (calculate-averaged-knot-vector-from-u-k uk-values n p)))
    :equal (fn [uk-values n p Q] calculate-equally-spaced-knot-vector-from-u-k  uk-values n p)
    :riesenfield (fn [uk-values n p Q ] (riesenfield-knot-vector n p Q :constrained constrained))
    :hartley-judd (fn [uk-values n p Q] (hartley-judd-method n p Q :constrained constrained))
    :rogers (fn [uk-values n p Q] (rogers-method n p Q :constrained constrained))
    ))

(defn non-uniform-rational-b-spline-point [p0 p1 p2 w t]
  (let [one-minus-t (- 1 t)
        one-minus-t-squared (Math/pow one-minus-t 2)
        t-squared (Math/pow t 2)
        two-times-w-times-t-times-one-minus-t (* 2 w t one-minus-t)]
    (/ (+ (* one-minus-t-squared p0) (* two-times-w-times-t-times-one-minus-t p1) (* t-squared p2))
       (+ one-minus-t-squared two-times-w-times-t-times-one-minus-t t-squared))))

(defn generate-clamped-knot-vector [degree n]
  (let [m (+ n degree 1)
        number-of-knots (inc m)]))


(defn non-uniform-rational-b-spline-segment [p0 p1 p2 w steps]
  (concat
   (for [index (range 0 (inc steps))]
     [(non-uniform-rational-b-spline-point (nth p0 0) (nth p1 0) (nth p2 0) w (/ index steps))
      (non-uniform-rational-b-spline-point (nth p0 1) (nth p1 1) (nth p2 1) w (/ index steps))
      (non-uniform-rational-b-spline-point (nth p0 2) (nth p1 2) (nth p2 2) w (/ index steps))])))

(defn non-uniform-rational-b-spline [p0 p1 p2 w steps])

(defn linear-non-uniform-b-spline-segment-point [p0 p1 t]
  (let [f1 (+ (- t) 1)
        f2 t]
    (mapv + (mapv #(* f1 %) p0) (mapv #(* f2 %) p1))))
(defn linear-non-uniform-b-spline-segment [p0 p1 steps]
  (into [] (concat (for [index (range 0 (inc steps))
                         :let [t (/ index steps)]]
                     (linear-non-uniform-b-spline-segment-point p0 p1 t)))))

(defn quadratic-non-uniform-b-spline-segment [p0 p1 p2 steps])
(def quartic-uniform-b-spline-basis-matrix
  (mapv #(mapv (partial * (/ 1 (factorial 4))) %)
        [[1 -4 6 -4 1]
         [-4 12 -12 4 0.0]
         [6 -6 -6 6 0.0]
         [-4 -12 12 4 0.0]
         [1 11 11 1 0.0]]))

(defn quartic-uniform-b-spline-segment-point [p-i-minus-one p-i p-i-plus-one p-i-plus-two p-i-plus-three t]
  (let [t-to-the-power-of-four (Math/pow t 4)
        t-cubed (Math/pow t 3)
        t-squared (Math/pow t 2)
        four-factorial (factorial 4)
        f-fn #(/ (+ (* %1 t-to-the-power-of-four) (* %2 t-cubed) (* %3 t-squared) (* %4 t) %5) four-factorial)
        f1 (f-fn 1 -4 6 -4 1)
        f2 (f-fn -4 12 -6 -12 11)
        f3 (f-fn 6 -12 -6 12 11)
        f4 (f-fn -4 4 6 4 1)
        f5 (f-fn 1 0 0 0 0)]
    (+ (* f1 p-i-minus-one) (* f2 p-i) (* f3 p-i-plus-one) (* f4 p-i-plus-two) (* f5 p-i-plus-three))
   ;(mmul [p-i-minus-one p-i p-i-plus-one p-i-plus-two p-i-plus-three](mmul t-vector quartic-uniform-b-spline-basis-matrix))
    ))



(defn quartic-uniform-b-spline-segment [p-i-minus-one p-i p-i-plus-one p-i-plus-two p-i-plus-three steps]
  (concat
   (for [index (range 0 (inc steps))]
     [(quartic-uniform-b-spline-segment-point (nth p-i-minus-one 0) (nth p-i 0) (nth p-i-plus-one 0) (nth p-i-plus-two 0) (nth p-i-plus-three 0)  (/ index steps))
      (quartic-uniform-b-spline-segment-point (nth p-i-minus-one 1) (nth p-i 1) (nth p-i-plus-one 1) (nth p-i-plus-two 1) (nth p-i-plus-three 1)  (/ index steps))
      (quartic-uniform-b-spline-segment-point (nth p-i-minus-one 2) (nth p-i 2) (nth p-i-plus-one 2) (nth p-i-plus-two 2) (nth p-i-plus-three 2)  (/ index steps))])))


(defn nurbs-with-calculated-knot-vector [points p weights steps & {:keys [point-paramater-calculation-method 
                                                                          knot-vector-generation-method]
                                                                   :or {point-paramater-calculation-method :chordal
                                                                        knot-vector-generation-method :average}}]
  (let [number-of-points (count points)
        n (dec number-of-points)
        u-k-fn (get-function-for-u-k-values point-paramater-calculation-method)
        u-k (u-k-fn n points)
        knot-vector-generation-fn (get-knot-vector-generation-fn knot-vector-generation-method :constrained false)
        knot-vector (knot-vector-generation-fn u-k n p points)] 
    (nurbs points p knot-vector weights steps)))

(defn make-one-arc [P-zero T-zero P-two T-two P P1 wa])

(defn get-cubic-b-spline-params-from-catmull-rom-params [points & {:keys [alphaType] :or {alphaType :centripetal}}]
  (let [alpha (if (keyword? alphaType) (case alphaType
                                         :uniform 0
                                         :centripetal 0.5
                                         :chordal 1.0)
                  alphaType)
        n (dec (count points))
        p  3
        bezier-curves (inc (- n p)) 
        segments (inc (* (- n 3) 3)) 
        b-spline-control-points (vec (apply concat (for [index (range bezier-curves)
                                      :let [p0 (nth points index)
                                            p1 (nth points (inc index))
                                            p2 (nth points (+ index 2))
                                            p3 (nth points (+ index 3))
                                            d1 (magnitude (mapv - p1 p0))
                                            d2 (magnitude (mapv - p2 p1))
                                            d3 (magnitude (mapv - p3 p2))
                                            d1-pow-alpha (pow d1 alpha)
                                            d2-pow-alpha (pow d2 alpha)
                                            d3-pow-alpha (pow d3 alpha)
                                            d1-pow-2-alpha (pow d1 (* 2 alpha))
                                            d2-pow-2-alpha (pow d2 (* 2 alpha))
                                            d3-pow-2-alpha (pow d3 (* 2 alpha))
                                            inner-1 (mapv + p1 (div (mapv + (mul d1-pow-2-alpha p2) (mul (- d2-pow-2-alpha) p0) (mul (- d2-pow-2-alpha d1-pow-alpha) p1))
                                                                    (* 3 d1-pow-alpha (+ d3-pow-alpha d2-pow-alpha))))
                                            inner-2 (mapv + p2 (div (mapv + (mul d3-pow-2-alpha p1) (mul (- d2-pow-2-alpha) p3) (mul (- d2-pow-2-alpha d3-pow-2-alpha) p2))
                                                                    (* 3 d3-pow-alpha (+ d3-pow-alpha d2-pow-alpha))))]]
                                                     
                                (do
                                (println "p0" p0 "p1" p1 "p2" p2 "p3" p3 "alpha" alpha)
                                  (cond (and (zero? index) (< index 1)) [p1 inner-1 inner-2 p2]
                                        (= index (dec bezier-curves)) [inner-1 inner-2 p2]
                                        :else [p1 inner-1 inner-2])))))
        n-new (dec (count b-spline-control-points))
        knot-vector (mul segments (vec (flatten (for [index (range (inc bezier-curves))]
                      (cond (zero? index) (repeat (inc p) 0.0)
                            (= bezier-curves index) (repeat (inc p) (double 1.0))
                            :else (repeat p (double (/ (* 3 index) n-new))))))))
        ]
     {:P b-spline-control-points :U knot-vector}
    )
  )

(defn catmull-rom-to-cubic-non-unifrom-cubic-b-spline-curve [points steps & {:keys [alphaType] :or {alphaType :centripetal}}]
  (let [p 3
        {P :P
         U :U} (get-cubic-b-spline-params-from-catmull-rom-params points :alphaType alphaType)] 
    (non-uniform-b-spline P p U steps)
    )
  )
(defn catmull-rom-segment-to-cubic-nurbs [p0 p1 p2 p3 steps & {:keys [alphaType] :or {alphaType :centripetal}}]
  (let [alpha (if (keyword? alphaType) (case alphaType
                                         :uniform 0
                                         :centripetal 0.5
                                         :chordal 1.0)
                  alphaType) 
        d1 (magnitude (mapv - p1 p0))
        d2 (magnitude (mapv - p2 p1))
        d3 (magnitude (mapv - p3 p2))
        d1-pow-alpha (pow d1 alpha)
        d2-pow-alpha (pow d2 alpha)
        d3-pow-alpha (pow d3 alpha)
        d1-pow-2-alpha (pow d1 (* 2 alpha))
        d2-pow-2-alpha (pow d2 (* 2 alpha)) 
        d3-pow-2-alpha (pow d3 (* 2 alpha)) 
        t0 0.0
        t1 0
        inner-1 (mapv + p1 (div (mapv + (mul d1-pow-2-alpha p2) (mul (- d2-pow-2-alpha) p0) (mul (- d2-pow-2-alpha d1-pow-alpha) p1))
                                (* 3 d1-pow-alpha (+ d3-pow-alpha d2-pow-alpha))))
        inner-2 (mapv + p2 (div (mapv + (mul d3-pow-2-alpha p1) (mul (- d2-pow-2-alpha) p3) (mul (- d2-pow-2-alpha d3-pow-2-alpha) p2))
                                (* 3 d3-pow-alpha (+ d3-pow-alpha d2-pow-alpha)))) 
        ]
    (println "segment" [p1 inner-1 inner-2 p2])
    (println "segment" "p0" p0 "p1" p1 "p2" p2 "p3" p3 "alpha" alpha)
    (non-uniform-b-spline [p1 inner-1 inner-2 p2] 3 [0 0 0 0 1 1 1 1] steps) 
    ) 
  )
(comment (pow 0.5 0))


(defn total-chord-length-derivative-magnitude-estimation [Q]
  (let [n (dec (count Q))
        qk-values (vec (for [k (range 1 (inc n))] (mapv - (nth Q k) (nth Q (dec k)))))
        qk-magnitudes (mapv magnitude qk-values)]
    (reduce + qk-magnitudes)) 
  )

;; (defn farins-simple-derivative-magnitude-estimation [Q]
;;   (let [n (dec (count Q))
;;         qk (vec (for [k (range n)]
;;                   (mapv - (nth Q (inc k)) (nth Q k))))
;;         ck (mapv magnitude qk)
;;         Sk (vec (for [k (range n)]
;;                   (div (nth qk k) (nth ck k))))
;;         tau-zero (1.2 )
;;         ]))

(defn farins-simple-derivative-magnitude-estimation [Q]
  (let [n (dec (count Q))
        uk-values (farin-simple-parameterisation n Q)
        c-zero (magnitude (mapv - (nth Q 1) (nth Q 0)))
        c-n (magnitude (mapv - (nth Q n) (nth Q (dec n)) ))
        ;qk-values (vec (for [k (range n)] (mapv - (nth Q (inc k)) (nth Q k))))
        tau-zero (/ (* 1.2 c-zero)
                    (- (nth uk-values 1) (nth uk-values 0)))
        tau-n (/ (* 1.2 c-n)
                 (- (nth uk-values n) (nth uk-values (dec n))))
        ]
    (println :tau-zero tau-zero :tau-n tau-n)
    {:tau-zero tau-zero :tau-n tau-n}
    )
  )

(defn farins-sophisticated-derivative-magnitude-estimation [Q t-zero t-n]
  (let [n (dec (count Q))
        uk-values (farin-simple-parameterisation n Q)
        q-zero (mapv - (nth Q 1) (nth Q 0))
        q-n (mapv - (nth Q n) (nth Q (dec n)))
        c-zero (magnitude q-zero)
        c-n (magnitude q-n)
        S-zero (div q-zero c-zero)
        S-n (div q-n c-n)
        tau-zero (/ c-zero
                    (* (- (nth uk-values 1) (nth uk-values 0))
                       (max (dot S-zero t-zero) 0.5)))
        tau-n (/ c-zero 
                 (* (- (nth uk-values n) (nth uk-values (dec n)))
                    (max (dot S-n t-n) 0.5)))]
           {:tau-zero tau-zero :tau-n tau-n}
    ;; (println "S-zero" S-zero)
    ;; (println "S-n" S-n)
    ;; (println "(dot S-zero t-zero)" (dot S-zero t-zero))
    ;(println "(dot S-n t-n)" (dot S-n t-n))
    )
  )
(defn total-arc-length-derivative-magnitude-estimation [Q] 
  (let [n (dec (count Q))
        qk-values (vec (for [k (range n)] (mapv - (nth Q (inc k)) (nth Q k))))
        l-k-values (calculate-circular-arc-span-length Q qk-values :n (- n 2))
        ]
    (reduce + l-k-values)))



(comment (total-arc-length-derivative-magnitude-estimation [[0 0 0] [1 1 1] [3 9 8] [6 8 9]]))
(comment (total-chord-length-derivative-magnitude-estimation [[0 0 0] [1 1 1] [3 9 8] [6 8 9]]))
(comment (farins-simple-derivative-magnitude-estimation [[0 0 0] [1 1 1] [3 9 8] [6 8 9]]))
(comment (farins-sophisticated-derivative-magnitude-estimation [[0 0 0] [1 1 1] [3 9 8] [6 8 9]] [1 0 0] [1 0 0]))

(defn clamped-end-condition [Q knot-vector D-zero D-n &{:keys [n] :or {n (dec (count Q))}}]
  (let [P-zero (nth Q 0)
        P-one (mapv + (mul (/ (nth knot-vector 4) 3) D-zero) P-zero)
        P-n-plus-two (peek Q)
        P-n-plus-one (mapv - P-n-plus-two (mul (/ (- 1 (nth knot-vector (+ n 2))) 3) D-n))]
    {:P-zero P-zero :P-one P-one :P-n-plus-one P-n-plus-one :P-n-plus-two P-n-plus-two}
    ) 
  )

(defn bessel-end-condition [Q u-k-values]
  (let [n (dec (count Q))
        u-k-size (count u-k-values)
        u-k-zero (nth u-k-values 0)
        u-k-one (nth u-k-values 1)
        u-k-two (nth u-k-values 2)
        alpha-zero (/ (- u-k-two u-k-one) (- u-k-two u-k-zero))
        beta-zero (- 1 alpha-zero)
        P-zero (nth Q 0)
        P-n-plus-two (nth Q n)
        a (mul (/ 1 (* 2 alpha-zero beta-zero)) 
               (mapv - (nth Q 1) (mul (pow alpha-zero 2) (nth Q 0)) (mul (pow beta-zero 2) (nth Q 2))))
        P-one (mapv + (mul (/ 2 3) (mapv + (mul alpha-zero (nth Q 0)) (mul beta-zero a))) (mul (/ 1 8) (nth Q 0)))
        u-k-n-minus-two (nth u-k-values (- n  2))
        u-k-n-minus-one (nth u-k-values (dec n))
        u-k-n (nth u-k-values n)
        alpha-n (/ (- u-k-n u-k-n-minus-one) (- u-k-n u-k-n-minus-two))
        beta-n (- 1 alpha-n)
        a-n (mul (/ 1 (* 2 alpha-n beta-n))
                 (mapv - (nth Q (dec n)) (mul (pow alpha-n 2) (nth Q (- n 2))) (mul (pow beta-n 2) (nth Q n))))
        P-n-plus-one (mapv + (mul (/ 2 3) (mapv + (mul alpha-zero (nth Q n)) (mul beta-n a-n))) (mul (/ 1 8) (nth Q n)))
        ]
     {:P-zero P-zero :P-one P-one :P-n-plus-one P-n-plus-one :P-n-plus-two P-n-plus-two}) 
  )

;; (defn removef-curve-knot [n p U Pw u r s num]
;;   (let [m (inc (+ n p))
;;         ord (inc p)
;;         fout (/ (- (* r 2) s p) 2)
;;         last (- r s)
;;         first (- r p)]
;;     (doseq )
;;     )
  
;;   )

(defn decompose-curve [n p U Pw]
  (let [m (+ n p 1)
        a (double-array 1 p)
        b (double-array 1 (double (inc p)))
        nb (double-array 1 0.0)
        zero-vec (vec (repeat (count (nth Pw 0)) 0.0))
        Qw (to-array-2d (repeat (peek U) (vec (repeat (inc p) zero-vec))))]
    (doseq [i (range (inc p) )] (aset Qw (aget nb 0) i (nth Pw i)))
    (while (< (aget b 0) m)
      (let [i (aget b 0)]
        (while (and (< (aget b 0) m) (= (nth U (aget b 0)) (nth U (inc (aget b 0)))))
          (aset b 0 (inc (aget b 0)))
          )
        (let [mult (inc (- (aget b 0) i))]
          (if (< mult p)
            (let [numer (- (nth U (aget b 0)) (nth U (aget a 0)))
                  alphas (double-array (- p mult) 0.0)]
              (doseq [j (range p mult -1)]
                (aset alphas (- j mult 1) (double (/ numer
                                                     (- (nth U (+ (aget a 0) j)) (nth U (aget a 0)))))))
              (let [r (- p mult)]
                (doseq [j (range 1 (inc r))
                        :let [save (- r j)
                              s (+ mult j)]]
                  (doseq [k (range p (dec s) -1)
                          :let [alpha (aget alphas (- k s))]]
                    (aset Qw (aget nb 0) k (mapv + (mul alpha (aget Qw (aget nb 0) k))
                                                 (mul (- 1.0 alpha) (aget Qw (aget nb 0) (dec k))))))
                  (if (< (aget b 0) m) 
                    (aset Qw (inc (aget nb 0)) save (aget Qw (aget nb 0) p)))))))
          (aset nb 0 (inc (aget nb 0))) 
          (if (< (aget b 0) m)
            (doseq [i (range (- p mult) (inc p))] 
              (aset Qw (aget nb 0) i (nth Pw (+ (- (aget b 0) p) i))))) 
          (aset a 0 (aget b 0))
          (aset b 0 (inc (aget b 0))))
        ))
    
    {:nb (aget nb 0) :Qw (mapv vec Qw)}
    )
  )

(defn decompose-non-homogoneus-nurbs-curve [p U P W]
  (let [n (dec (count P))
        Pw (homogenize-cooridinates P W)
        {nb :nb
         Qw :Qw} (decompose-curve n p U Pw)] 
    
    {:nb nb :Q Qw})
  )

(defn decompose-b-spline-curve [p U P]
  (let [n (dec (count P))
        Pw (homogenize-cooridinates P (vec (repeat (inc n) 1.0)))
        {nb :nb
         Qw :Qw} (decompose-curve n p U Pw)
        Q (mapv #(mapv project-coordinate-and-drop-weight %) Qw)]
    ;(println "Qw" Qw)
    {:nb nb :Q Q}
    )
  )
(comment (vec [1 0 0]))

(comment (let [p 3]
           (vec (concat (repeat (inc p) 0) (repeat (inc p) 1)))))
(defn decompose-non-homogoneus-nurbs-to-nurbs-params [p U P W ]
  (let [{Qw :Q  
         nb :nb    }(decompose-non-homogoneus-nurbs-curve p U P W)]
    
    (vec (for [index (range (count Qw))
               :when (not (zero? (last (first (nth Qw index)))))
               :let [{points :points
                      weights :weights} (project-coordinates-and-split-weights (nth Qw index)) 
                     knot-vector (vec (concat (repeat (inc p) 0) (repeat (inc p) 1))) 
                     n (dec (count points))]
               ]
           (do 
             (println "points" points)
           {:n n :p p :knot-vector knot-vector :points points :weights weights})
           )))
  )

(defn decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves [p U P W start-index end-index-inclusive steps & {:keys [reverse-curve] :or {reverse-curve false}}] 
  (let [params (decompose-non-homogoneus-nurbs-curve p U P W)
        reverse-fn (fn [element] (vec (reverse (mapv #(vec (reverse %)) element))))
        param-points  (cond->> (subvec (:Q params) start-index (inc end-index-inclusive))
                        reverse-curve reverse-fn)
        number-of-segments (do
                             (count param-points))
        increment (/ number-of-segments (long steps))] 
    (vec (for [index (range 0 (+ (* 2 increment) number-of-segments) increment)
               :let [i (dec (if (< index number-of-segments) (inc (floor index)) number-of-segments))
                     t (if (< index number-of-segments) (- index (floor index)) 1.0)
                     points (nth param-points i)
                     n (dec (count points))
                     knot-vector (vec (concat (repeat (inc p) 0) (repeat (inc p) 1)))]]


           (calculate-nurbs-curve-point n p knot-vector points t ;:basis-functions (nth basis-functions-coll i)
                                  ))))
  )

(defn decompose-b-spline-curve-and-return-bezier-composite-bezier-curve-fn [p U P start-index end-index-inclusive & {:keys [reverse-curve extra-fn] :or {reverse-curve false}}]
  (let [params (decompose-b-spline-curve p U P)
        reverse-fn (fn [element] (vec (reverse (mapv #(vec (reverse %)) element))))
        param-points  (cond->> (subvec (:Q params) start-index (inc end-index-inclusive))
                        reverse-curve reverse-fn)
        number-of-segments (do
                             (count param-points))]
    (fn [step]
      (let [index (* step number-of-segments)
            i (dec (if (< index number-of-segments) (inc (floor index)) number-of-segments))
            t (if (< index number-of-segments) (- index (floor index)) 1.0)
            point (n-degree-bezier-point (nth param-points i) t ;:basis-functions (nth basis-functions-coll i)
                                         )]
        (if extra-fn (extra-fn point) point)
        )
      )
    ))

(defn decompose-b-spline-curve-and-calculate-bezier-curves [p U P start-index end-index-inclusive steps &{:keys [reverse-curve] :or {reverse-curve false}}] 
  (let [params (decompose-b-spline-curve p U P)
        reverse-fn (fn [element](vec (reverse (mapv #(vec (reverse %)) element))))
        param-points  (cond->> (subvec (:Q params) start-index (inc end-index-inclusive))
                       reverse-curve reverse-fn)
        number-of-segments (do 
                            (count param-points))
        increment (/ number-of-segments (long steps)) 
        ]
    (vec (for [index (range 0 (+ increment number-of-segments) increment)
               :let [i (dec (if (< index number-of-segments) (inc (floor index)) number-of-segments))
                     t (if (< index number-of-segments) (- index (floor index)) 1.0)]]

           
           (n-degree-bezier-point (nth param-points i) t ;:basis-functions (nth basis-functions-coll i)
                                  ) 
           )))
  )



(comment (decompose-curve 5 3 [0 0 0 0 1 2 3 3 3 3] [[0 0 0 1] [1 1 0 1] [3 2 1 1] 
                                                     [8 2 2 1] [10 3 1 1] [13 2 1 1]]))


(comment (decompose-b-spline-curve 3 [0 0 0 0 1 2 3 3 3 3] [[0 0 0] [1 1 0] [3 2 1]
                                                     [8 2 2] [10 3 1] [13 2 1]]))

(comment (decompose-b-spline-curve 3 [0 0 0 0 1 2 3 3 3 3] [[0 0 0] [1 1 0] [3 2 1]
                                                            [8 2 2] [10 3 1] [13 2 1]]))
(comment (decompose-non-homogoneus-nurbs-curve 3 [0 0 0 0 1 2 3 3 3 3] [[0 0 0] [1 1 0] [3 2 1]
                                                            [8 2 2] [10 3 1] [13 2 1]]
                                               [1 1 1 0.8 1 1]))

(comment (decompose-non-homogoneus-nurbs-curve 2 [0 0 0 1  2 3 4 4 4] [[0 0 0] [1 1 0] [3 2 1]
                                                                        [8 2 2] [10 3 1] [13 2 1]]
                                               [1 1 1 0.8 1 1]))

(comment (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves 3 [0 0 0 0 1 2 3 3 3 3] [[0 0 0] [1 1 0] [3 2 1]
                                                                                                    [8 2 2] [10 3 1] [13 2 1]]
[1 1 1 1 1 1] 0 2 30))

(comment (decompose-b-spline-curve-and-calculate-bezier-curves 3 [0 0 0 0 1 2 3 3 3 3] [[0 0 0] [1 1 0] [3 2 1]
                                                                                        [8 2 2] [10 3 1] [13 2 1]]
                                                               0 1 30))

(comment (let [params (decompose-b-spline-curve-and-return-bezier-composite-bezier-curve-fn 3 [0 0 0 0 1 2 3 3 3 3] [[0 0 0] [1 1 0] [3 2 1]
                                                                                                     [8 2 2] [10 3 1] [13 2 1]]
                                                                            0 1)
               steps 30
               increment (/ 1 steps)
               ] 
           (vec (for [index (range 0 (+ 1 increment) increment)]
                  (params index)))
           ))
(defn natural-end-conditions [Q u-k-values]
  
  )

(comment (let [points  [[0 0 0] [1 1 1] [3 5 8] [6 10 11]]
               u-k-values (u-k-chordal (dec (count points)) points)
               ]
           (bessel-end-condition points u-k-values)))

(comment (let [points  [[0 0 0] [1 1 1] [3 5 8] [6 10 11]]
          u-k-values (u-k-chordal (dec (count points)) points)]
           (clamped-end-condition points u-k-values)
           ))
(comment (double (/ 3 5)))

(comment (/ 1 0.1675 ))

(comment (pow 1 (* 0.5 0.5)))