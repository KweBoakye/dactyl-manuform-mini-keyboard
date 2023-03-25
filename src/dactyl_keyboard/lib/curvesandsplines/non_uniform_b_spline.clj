(ns dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline
  (:require [clojure.core.matrix :refer [magnitude mul]]
            [clojure.math :refer [floor pow sqrt]]
            [dactyl-keyboard.lib.curvesandsplines.curve-utils :refer [get-drop-last-point-if-not-last-segment homogenize-cooridinates homogenize-single-point
                                                                      project-coordinate]]
            [dactyl-keyboard.lib.curvesandsplines.splines :refer [getT
                                                                  lerp-unclamped]]
            [dactyl-keyboard.lib.general-maths :refer [all-binomials-for-n
                                                       binomial-coefficient-2
                                                       factorial]])
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
              :let [s1 0 s2 1 d (double-array 1 0.0)]]
             (aset a 0 0 1.0) 
             (loop [ s1-inner s1 s2-inner s2 k 1] 
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
                        (let [j s1-inner]
                          (recur s2-inner j (inc k)))
                        ))
                    )
             (loop [r p k 1]
               (if (<= k n)
                 (do
                   (doseq [j (range (inc p))]
                     (aset ders k j (* (aget ders k j) r)))
                 (recur (* (- p k) r) (inc k)))
                 )
               )
             
             )
      (println "ders" (mapv vec ders))
      (mapv vec ders) 
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
    (println "nders" nders)
    (println "P " P "span" span "p " p)
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
    (println "n" n)
   (ders-basis-funs i u p n U))
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



(defn calculate-non-vanishing-basis-functions [i u p U]
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
  (let [Nji (into-array (repeat (inc span) (into-array (repeat (inc p) 0.0))))]
    (println "Nji " Nji)
    (doseq [i (range (inc p))
            :let [Nb (calculate-non-vanishing-basis-functions span u p U)]]
           (doseq [j (range (inc p))]
                  (aset Nji j i (nth Nb j))
                  ))
    Nji
    ) 
  )

(defn calculate-non-uniform-b-spline-point
  "from p82 of the nurbs book"
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

(defn non-uniform-b-spline [P p U steps & {:keys [drop-last-point-of-segment ] :or {drop-last-point-of-segment true}}]
  (let [number-of-points (count P)
        number-of-points-per-segment (inc p)
        number-of-segments (- number-of-points p) 
        steps-total (* steps number-of-segments)
        segment-steps  (floor (/ steps number-of-segments))
        segment-steps-normalized (/ steps steps-total)
        drop-last-point-if-not-last-segment (get-drop-last-point-if-not-last-segment number-of-segments drop-last-point-of-segment)
        increment (/ number-of-segments steps)
        n (- (count U) p 2);(dec number-of-points)
        ] 
    (vec (for [u (range 0 (+ number-of-segments increment) increment)]
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

(defn nurbs-with-homogenous-coordinates [Pw p U steps & {:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
  (let [number-of-points (count Pw)
        number-of-points-per-segment (inc p)
        number-of-segments (- number-of-points p)
        steps-total (* steps number-of-segments)
       segment-steps  (/ steps number-of-segments)
        segment-steps-normalized (/ steps steps-total)
        drop-last-point-if-not-last-segment (get-drop-last-point-if-not-last-segment number-of-segments drop-last-point-of-segment)
        n (- (count U) p 2);(dec number-of-points)
        increment (/ number-of-segments steps)
        ]

    (into []  (for [u (range 0 (+ number-of-segments increment) increment)]
                             (calculate-nurbs-curve-point n p U Pw u)
                            ;;  (drop-last-point-if-not-last-segment (inc index)
                            ;;                                       (nurbs-segment n p U Pw segment-steps :u-start index :u-end (inc index)))
                             ))))

(defn nurbs [points p U weights steps & {:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
  (let [weighted-points (homogenize-cooridinates points weights)]
    (nurbs-with-homogenous-coordinates weighted-points p U steps :drop-last-point-of-segment drop-last-point-of-segment)))

(defn curve-deriv-control-points [n p U P d & {:keys [r1 r2] :or {r1 0 r2 n}}]
  (let [r (- r2 r1)
        PK (into-array (repeat (inc d) (into-array (repeat (inc r) (vec (repeat (count (nth P 0)) 0.0))))))]
    (println "PK " PK)
    (doseq [i (range (inc r))]
      (aset PK 0 i (nth P (+ r1 i))))

    (doseq [k (range 1 (inc d))
            :let [tmp (inc (- p k))]]
      (doseq [i (range (inc (- r k)))]
        (aset PK k i (mapv (partial * tmp)  (mapv - (aget PK (dec k) (inc i)) (mapv #(/ % (- (nth U (inc (+ r1 i p))) (nth U (+ r1 i k))))
                                                                                    (aget PK (dec k) i))))))
           
           )(mapv vec  PK)))



(defn nurbs-first-derivative-point [n p U Pw u d]
  (let [w-list (mapv #(vector (nth % 3)) Pw)
        P (mapv #(subvec % 0 3) Pw)
        Au-deriv (curve-derivs-alg1 n p U Pw u d)
        w-u (calculate-non-uniform-b-spline-point n p U w-list u)
        w-u-deriv  (curve-derivs-alg1 n p U w-list u d)
        C-u (calculate-non-uniform-b-spline-point n p U P u)]
    (println "Au-deriv " Au-deriv " w-u-deriv" w-u-deriv "w-u " w-u " C-u" C-u)
      (mapv - Au-deriv 
           (mapv (partial *  (nth w-u-deriv 0)) C-u)
           )
    ))


(defn curve-derivs-alg2 [n p U P u d]
  (let [du (min d p)
        span (calculate-knot-span-index n p u U)
        N (all-basis-funs span u p U)
        zero-vec (vec (repeat (count (nth P 0)) 0.0))
        PK (curve-deriv-control-points n p U P du :r1 (- span p) :r2 span)
        CK (into-array (repeat (inc d) zero-vec) )]
    (doseq [k (range (inc du))]
      (aset CK k zero-vec)
      (doseq [j (range (- p k))]
             (println "(get-in N [j (- p k)]) " (get-in N [j (- p k)]))
             (println "(get-in PK [k j]) " (get-in PK [k j]))
             (println "(aget CK k) " (aget CK k)) 
        (aset CK k (mapv + (aget CK k) (mapv (partial * (get-in N [j (- p k)])) (get-in PK [k j]))))))
    (vec CK)))

(defn nurbs-deriv-deboor [n p U P u weights]
  (let [span (calculate-knot-span-index n p u U)
       zero-vec (vec (repeat (inc (count (nth P 0))) 0.0))
        d (into-array (repeat (inc p) zero-vec))
        q (into-array (repeat (inc p) zero-vec))] 
    (println "d " (repeat (inc p) zero-vec))
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
      [point-3-dim 
       (mapv #(/ % (last (aget d p))) (mapv - (subvec (aget q (dec p)) 0 3 ) (mul point-3-dim (last (aget q (dec p))))))])
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
           (println "(nth wderivs ) " (nth wderivs 0))
           (println "v-result" v-result)
           (aset CK k (mapv #(/ % (nth wderivs 0) ) v-result))
           )
    (vec CK)
    )
  )

(defn c-deriv-zero [])

(comment 
  (let [points [[1 0 0] [1 1 0] [0 1 0]]
        weights [1 1 2]
        U [0 0 0 1 1 1]
        cu-derivs (curve-deriv-control-points 2 2 U points 1) 
        a-derivs (mapv #(subvec % 0 3) cu-derivs)
        w-derivs (mapv last cu-derivs) 
        deriv (nurbs-deriv-deboor 2 2 U points 1 weights)
        ]
    (println "deriv" deriv)
    (println "cu-derivs " cu-derivs)
    (println "a-derivs"  a-derivs)
    (println "w-derivs"  w-derivs)
    ;(rational-curve-derivs a-derivs w-derivs 1)
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

(defn calculate-uk-values [n numerator-list d]
  (let [uk-list (double-array  n 0.0)]
    (doseq [k (range 1 n)]
      (aset uk-list k (+ (aget uk-list (dec k)) (/ (nth numerator-list (dec k)) d))))
    (conj (vec uk-list) 1.0)))

(defn u-k-chordal [n Q]
  (let [magnitudes-of-qk-qk-minus-one (for [k (range 1 (inc n))]
                                        (sqrt (reduce + (mapv #(pow % 2) (mapv - (nth Q k) (nth Q (dec k)))))))
        d (reduce + magnitudes-of-qk-qk-minus-one)]
    (println "d is " d)
    (println "magnitudes-of-qk-qk-minus-one is " magnitudes-of-qk-qk-minus-one)
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

(defn calculate-natural-knot-vector-from-u-k [])



(comment
  (let [Q [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]]
        n 4
        p 3
        uk (u-k-chordal n Q)
        U [0 0 0 0 (/ 28 51) 1 1 1 1]
        span (calculate-knot-span-index n p (nth uk 0) U)]
    (println span)
    (calculate-non-vanishing-basis-functions 4 1 p U)))

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


(defn nurbs-with-calculated-knot-vector [points p weights steps & {:keys [drop-last-point-of-segment clamped style]
                                                                   :or {drop-last-point-of-segment true
                                                                        clamped :start-and-end
                                                                        style :chordal}}]
  (let [number-of-points (count points)
        n (dec number-of-points)
        u-k-fn (cond (= style :centripetal) u-k-centripetal
                     (= style :chordal) u-k-chordal)
        u-k (u-k-fn n points)
        knot-vector (calculate-averaged-knot-vector-from-u-k u-k n p)] 
    (println "knot-vector" knot-vector)
    (println "n" n)
    (nurbs points p knot-vector weights steps :drop-last-point-of-segment drop-last-point-of-segment)))

(defn make-one-arc [P-zero T-zero P-two T-two P P1 wa])

(defn catmull-rom-segment-to-cubic-nurbs [p0 p1 p2 p3 steps & {:keys [alphaType] :or {alphaType :centripetal}}]
  (let [alpha (if (keyword? alphaType) (case alphaType
                                         :uniform 0
                                         :centripetal 0.5
                                         :chordal 1.0)
                  alphaType)
        p0-to-p2  (mapv #(pow % (- 2 (* 2 alpha))) (mapv - p2 p0))
        p1-to-p3 (mapv #(pow % (- 2 (* 2 alpha))) (mapv - p3 p1))
        t0 0.0
        t1 (getT t0 alpha p0 p1)
        t2 (getT t1 alpha p1 p2)
        t3 (getT t2 alpha p2 p3)
        tfinal (lerp-unclamped t1 t2 t3)
        ;p0-to-p1 (mapv - p1 p0)
        ;p2-to-p3 (mapv - p3 p2)
        q0 (mapv - p1    (mapv #(/ %  (magnitude p0-to-p2)) p0-to-p2))
        q1  (mapv + p1    (mapv #(/ %  (magnitude p0-to-p2) ) p0-to-p2))
        q4   (mapv - p2  (mapv #(/ %  (magnitude p1-to-p3) ) p1-to-p3))
        q5  (mapv + p2  (mapv #(/ %  (magnitude p1-to-p3)) p1-to-p3))
        q1-to-q4 (mapv - q4 q1)
        q2 (mapv + q1 (mapv (partial * (/ 1 3)) q1-to-q4))
        q3 (mapv - q4 (mapv (partial * (/ 1 3)) q1-to-q4))
        ]
    (println "q1 " q1 " q2 " q2 " q3 " q3 " q4 " q4)
    (nurbs [p1 q1 p2] 2 [0 0 0 ] [1 1 1 1] steps) 
    ) 
  )

(comment (double (/ 3 5)))

(comment (/ 1 0.1675 ))

(comment (pow 1 (* 0.5 0.5)))