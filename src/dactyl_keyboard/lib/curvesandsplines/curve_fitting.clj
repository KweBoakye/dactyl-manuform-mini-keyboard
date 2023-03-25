(ns dactyl-keyboard.lib.curvesandsplines.curve-fitting
  (:require [clojure.core.matrix :refer [array cross dot length magnitude mul
                                         normalise]]
            [clojure.core.matrix.linear :refer [lu solve]]
            [clojure.math :refer [pow]]
            [dactyl-keyboard.lib.algebra :refer [quadratic-roots]]
            [dactyl-keyboard.lib.collections :refer [remove-by-index]]
            [dactyl-keyboard.lib.constants :refer [epsilon]]
            [dactyl-keyboard.lib.curvesandsplines.curve-utils :refer [homogenize-cooridinates]]
            [dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline 
    :refer [calculate-averaged-knot-vector-from-u-k calculate-knot-span-index
            calculate-non-vanishing-basis-functions non-uniform-b-spline
            nurbs-deriv-deboor u-k-centripetal u-k-chordal]]) 
  )


(defn calculate-knot-vector-from-u-k-with-end-derivs [U-k n p]
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




(defn forward-substitution [lower-matrix b-matrix]
  (let [q (count b-matrix)
        y-matrix (double-array q 0.0) 
        ] 
    (aset y-matrix 0 (/ (nth b-matrix 0) (get-in lower-matrix [0 0])))
    (doseq [i (range 1 q)
            :let [numerator (- (nth b-matrix i) (reduce + (for [j (range 0 i)] (* (get-in lower-matrix [i j]) (aget y-matrix j)))))
                  divisor (get-in lower-matrix [i i])
                  y-matrix-val  (/ numerator divisor) 
                  ]]
      (aset y-matrix i y-matrix-val))
    (vec y-matrix)))

(defn backward-substitution [upper-matrix y-matrix]
  (let [q (count y-matrix)
        x-matrix (double-array q 0.0)
        x-matrix-first-value (let [numerator (nth y-matrix (dec q))
                                   divisor (get-in upper-matrix [(dec q) (dec q)])]
                               (if (zero? divisor) 0.0 (/ numerator divisor)))] 
    (aset x-matrix (dec q) x-matrix-first-value)
    (doseq [i (range (- q 2) -1 -1)
            :let [upper-matrix-i-i (get-in upper-matrix [i i])
                  x-matrix-val  (/ (- (nth y-matrix i) (reduce + (for [j (range i q)] (* (get-in upper-matrix [i j]) (nth x-matrix j)))))
                                   upper-matrix-i-i)]]
      (aset x-matrix i x-matrix-val))
    (vec x-matrix)))

(comment
  (let [Q [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]]
        n 4
        p 3
        u-k-values (u-k-chordal n Q)
        U (calculate-averaged-knot-vector-from-u-k u-k-values n p)
        q (+ n 1)
        dim (count (nth Q 0))
;; uk-i (nth u-k-values 0)
   ;;    span (calculate-knot-span-index n p uk-i U)
;;                start-index (- span p)
;;               basis-funs (calculate-non-vanishing-basis-functions span uk-i p U)
;;                basis-funs-size (count basis-funs)

        A (for [i (range 0 (inc n))
                :let [uk-i (nth u-k-values i) span (calculate-knot-span-index n p uk-i U)
                      start-index (- span p)
                      basis-funs (calculate-non-vanishing-basis-functions span uk-i p U)
                      basis-funs-size (count basis-funs)]]
            basis-funs ;(into [] (concat (repeat start-index 0.0) basis-funs (repeat (- q (+ basis-funs-size start-index) ) 0.0)))
            )
        A-new (array :vectorz A)
        {:keys [L U]} (lu A-new {:return [:L :U]})]
    A))

(defn solve-with-vector [matrix vector]
  (let [vector-size (count vector)
        coordinate-vector-fn  (fn [coordinate] (vec (flatten (for [index (range vector-size)]
                                                               (get-in vector [index coordinate])))))
        x-vector (coordinate-vector-fn 0)
        y-vector (coordinate-vector-fn 1)
        z-vector  (coordinate-vector-fn 2)
        x-solved (vec (solve matrix x-vector))
        y-solved (vec (solve matrix y-vector))
        z-solved (vec (solve matrix z-vector))] 
    (for [index (range vector-size)]
      [(nth x-solved index) (nth y-solved index) (nth z-solved index)])))

(comment (solve (array :vectorz [[1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
                 [-1.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
                 [0.0,0.9996352054012266,3.647831574156622E-4,1.1441353990480655E-8,3.4665889473727118E-15,0.0,0.0,0.0,0.0],
                 [0.0,0.9994798950242914,5.20080439118758E-4,2.4536542517889334E-8,4.71728892210986E-14,0.0,0.0,0.0,0.0],
                 [0.0,0.0,0.0,0.2001854337169558,0.6001058076881196,0.19970875859492473,7.766239056253257E-24,0.0,0.0],
                 [0.0,0.0,0.0,0.0,9.205269329277368E-9,6.177380234646243E-5,0.02542887568641895,0.9745093413059651,0.0],
                 [0.0,0.0,0.0,0.0,1.5330402737134804E-41,8.834844872008977E-6,0.010234837373210423,0.9897563277819176,0.0],
                 [0.0,0.0,0.0,0.0,0.0,0.0,0.0,-1.0,1.0],
                 [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0]])
                (array :vectorz [-100.46506732026194 -4.2051783974774206E-5 -100.46506732026195 -100.46506731026194 -99.29418920589302 -98.12341109152413 -98.12331109152413 -1.2104385536119593 -98.12331109152413])))

(defn global-curve-interp [Q p &{:keys [n point-paramater-calculation-method] :or {n (dec (count Q)) point-paramater-calculation-method :centripetal}}]
  "global interpolation through n+1 points
   
   -n number of points to be calculated - 1
   -Q pass-through points
   -r number of pass through points
   -p curve degree
   return m U P
   
   U knot vector
   
   "
  (let [m (+ n p 1)
        function-for-u-k-values (cond (= point-paramater-calculation-method :chordal) u-k-chordal
                                      (= point-paramater-calculation-method :centripetal) u-k-centripetal)
        u-k-values (function-for-u-k-values n Q)
        U-knot-vector (mapv #(/ % (- (inc n) p)) (calculate-averaged-knot-vector-from-u-k u-k-values n p))
        q (+ n 1)
        dim (count (nth Q 0))
        A (for [i (range 0 q)
                :let [uk-i (nth u-k-values i)
                      span (calculate-knot-span-index n p uk-i U-knot-vector)
                      start-index (- span p)
                      basis-funs (calculate-non-vanishing-basis-functions span uk-i p U-knot-vector)
                      basis-funs-size (count basis-funs)]]
            ;basis-funs
            (into [] (concat (repeat start-index 0.0) basis-funs (repeat (- q (+ basis-funs-size start-index)) 0.0))))
        A-new (array :vectorz A)
        r (count Q)
        ;; {:keys [L U]} (lu A-new {:return [:L :U]})
        ;; L-vec (mapv vec L)
        ;; U-vec (mapv vec U)
        P (solve-with-vector A-new Q);(to-array-2d (repeat q (repeat dim 0.0)))
        ] 
    ;; (doseq [i (range dim)
    ;;         :let [rhs (do
    ;;                     (println "i " i)
    ;;                     (println "Q " Q)
    ;;                     (for [j (range (inc n))] (get-in Q [j i])))
    ;;               y-matrix (forward-substitution L-vec rhs)
    ;;               xt-matrix (backward-substitution U-vec y-matrix)]]
    ;;   (doseq [j (range 0 (inc n))] (aset P j i (nth xt-matrix j)))) 
  
    {:m m :U (mapv #(* % (- (inc n) p)) U-knot-vector) :P (mapv (partial vec) (vec P))}))

(comment
  (to-array-2d (repeat 5 (repeat 3 0.0))))

(comment
  (global-curve-interp [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]] 3 :point-paramater-calculation-method :chordal))

(defn global-curve-interp-curve [Q p steps &{:keys [n point-paramater-calculation-method] :or {n (dec (count Q)) point-paramater-calculation-method :centripetal}}]
  (let [global-curve-interp-data (global-curve-interp Q p :n n :point-paramater-calculation-method point-paramater-calculation-method)]
    (non-uniform-b-spline (:P global-curve-interp-data ) p (:U global-curve-interp-data) steps)
    )
  )

(defn global-curve-interp-with-end-derivatives [Q p D-zero D-n & {:keys [n point-paramater-calculation-method] :or {n (dec (count Q)) point-paramater-calculation-method :centripetal}}]
  (let [m (+ n p 3)
        function-for-u-k-values (cond (= point-paramater-calculation-method :chordal) u-k-chordal
                                      (= point-paramater-calculation-method :centripetal) u-k-centripetal)


        u-k-values (function-for-u-k-values n Q)
        U-knot-vector (calculate-knot-vector-from-u-k-with-end-derivs u-k-values n p)
        U-knot-vector-normalized (mapv #(/ % (- (+ n 3) p)) U-knot-vector)
        num-of-points (count Q)
        Q-with (vec (concat [(nth Q 0) (mapv (partial * (/ (nth U-knot-vector (inc p)) p)) D-zero)]
                            (subvec Q 1 (dec (count Q))) [(mapv (partial * (/ (- 1 (nth U-knot-vector (- m p 1))) p)) D-n) (last Q)]))
        Q-with-tangents 
                          (into [] (for [index (range (+ n 3))]
                                       (cond
                                         (zero? index) (nth Q 0)
                                         (= index 1) D-zero
                                         (and (<= index 2) (>= index n)) (nth Q (dec index))
                                         (= index (inc n)) D-n
                                         :else (last Q))))
        q (+ n 3)
        dim (count (nth Q 0))
        A-row-one (vec (concat [-1.0 1.0] (repeat (inc n) 0.0)))
        A-row-n-plus-one (vec (concat (repeat (inc n) 0.0) [-1.0 1.0]))
        A-initial (vec (for [i (range 0 (+ n 1))
                             :let [uk-i (nth u-k-values i)
                                   span (calculate-knot-span-index (+ n 2) p uk-i U-knot-vector-normalized)
                                   start-index (- span p)
                                   basis-funs (calculate-non-vanishing-basis-functions span uk-i p U-knot-vector-normalized)
                                   basis-funs-size (count basis-funs)]] 
                           (into [] (concat (repeat start-index 0.0) basis-funs (repeat (- (+ n 3) (+ basis-funs-size start-index)) 0.0)))))
        A (vec  (concat [(nth A-initial 0) A-row-one] (subvec A-initial 1 n) [A-row-n-plus-one (last A-initial)])) 
        A-new (array :vectorz A)
        P (solve-with-vector A-new Q-with)
        ] 
    {:m m :U
     U-knot-vector
     :P (mapv (partial vec) P)}))

(defn global-curve-interp-with-end-unit-derivatives [Q p tangent-endpoint-zero tangent-endpoint-n & {:keys [n point-paramater-calculation-method] :or {n (dec (count Q)) point-paramater-calculation-method :centripetal}}] 
  (let [tangent-endpoint-zero-to-Q-zero (mapv - (nth Q 0) tangent-endpoint-zero)
        Q-n-to-tangent-endpoint-n (mapv - tangent-endpoint-n (last Q))
        D-zero (mapv #(/ % (magnitude tangent-endpoint-zero-to-Q-zero)) tangent-endpoint-zero-to-Q-zero)
        D-n (mapv #(/ % (magnitude Q-n-to-tangent-endpoint-n)) Q-n-to-tangent-endpoint-n) ]
    (global-curve-interp-with-end-derivatives Q p D-zero D-n :n n :point-paramater-calculation-method point-paramater-calculation-method)))

(defn global-curve-interp-with-end-unit-derivatives-curve [Q p tangent-endpoint-zero tangent-endpoint-n steps & {:keys [n point-paramater-calculation-method] :or {n (dec (count Q)) point-paramater-calculation-method :centripetal}}]
  (let [curve-interp-data (global-curve-interp-with-end-unit-derivatives Q p tangent-endpoint-zero tangent-endpoint-n :n n :point-paramater-calculation-method point-paramater-calculation-method)
        ]
    (non-uniform-b-spline (:P curve-interp-data) p (:U curve-interp-data) steps)
    )
  )
(comment
  (global-curve-interp-with-end-derivatives [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]]  3 [-1 -1 0] [0 -2 0] :point-paramater-calculation-method :chordal))


;; (defn local [Qk & {:keys [point-paramater-calculation-method] :or {point-paramater-calculation-method :centripetal}}]
;;   (let [n (dec (count Qk))
;;         function-for-u-k-values (cond (= point-paramater-calculation-method :chordal) u-k-chordal
;;                                       (= point-paramater-calculation-method :centripetal) u-k-centripetal)
;;         u-k-values (function-for-u-k-values n Qk)
;;         delta-u-k-values (vec (for [index (range 1 (inc n))]
;;                                 (- (nth u-k-values index) (nth u-k-values (dec index)))))
;;         qk-values (vec (for [index (range 1 (inc n))]
;;                          (mapv - (nth Qk index) (nth Qk (dec index)))))
;;         dk-values (mapv (fn [qk delta-u-k] (mapv #(/ % delta-u-k) qk)) qk-values delta-u-k-values)
;;         alpha-k-values
;;         ;;(vec (for [k (range 1 (- n 2))
;;         ;;                           :let [qk-minus-one (nth qk-values (dec k))
;;         ;;                                 qk (nth qk-values k)
;;         ;;                                 qk-plus-one (nth qk-values (inc k))
;;         ;;                                 qk-plus-two (nth qk-values (+ k 2))
;;         ;;                                 magnitude-of-qk-minus-one-times-qk (vector-magnitude (mapv * qk-minus-one qk))
;;         ;;                                 magnitude-of-qk-plus-one-times-qk-plus-two (vector-magnitude (mapv * qk-plus-one qk-plus-two))
;;         ;;                                 ]]
;;         ;;                       (do (println "qk " qk)
;;         ;;                        (/ magnitude-of-qk-minus-one-times-qk
;;         ;;                          (+ magnitude-of-qk-minus-one-times-qk magnitude-of-qk-plus-one-times-qk-plus-two)))
;;         ;;                       ) 
;;         ;;                     ) 
;;         (vec (for [k (range 1 n)
;;                    :let [index (dec k)
;;                          delta-u-k (nth delta-u-k-values index)
;;                          delta-u-k-plus-one (nth delta-u-k-values (inc index))]]=
;;                (/ delta-u-k (+ delta-u-k delta-u-k-plus-one))))
;;         Dk-values-initial (vec (for [k (range 1 n)
;;                                      :let [index (dec k)
;;                                            alpha-k (nth alpha-k-values index)
;;                                            dk (nth dk-values index)
;;                                            dk-plus-one (nth dk-values (inc index))]]
;;                                  (mapv + (mapv (partial * (- 1 alpha-k)) dk) (mapv (partial * alpha-k) dk-plus-one))))
;;         D-zero (mapv - (mapv (partial * 2) (nth dk-values 1)) (nth Dk-values-initial 0))
;;         D-n (mapv - (mapv (partial * 2) (nth dk-values (dec n))) (nth Dk-values-initial (- n 2)))
;;         Dk-values (into [D-zero] (conj Dk-values-initial D-n))
;;         Vk-values (vec (for [k (range 0 (dec n))
;;                              :let [alpha-k (nth alpha-k-values k)
;;                                    qk (nth qk-values k)
;;                                    qk-plus-one (nth qk-values (inc k))]]
;;                          (mapv + (mapv (partial * (- 1 alpha-k)) qk) (mapv (partial * alpha-k) qk-plus-one))))
;;         Tk-values (vec (mapv (fn [Vk] (mapv #(/ % (vector-magnitude Vk)) Vk)) Vk-values))]
;;     Tk-values))

(defn calculate-tangents-for-local-cubic-curve-interpolation [Qk & {:keys [point-paramater-calculation-method corner-perservation] :or {point-paramater-calculation-method :centripetal corner-perservation :smooth}}]
  (let [n (dec (count Qk))
        qk-values-initial (vec (for [index (range 1 (inc n))]
                                 (mapv - (nth Qk index) (nth Qk (dec index)))))
        q-one (nth qk-values-initial 0)
        q-two (nth qk-values-initial 1)
        q-n (nth qk-values-initial (dec n))
        q-n-minus-one (nth qk-values-initial (- n 2))
        q-zero (mapv - (mapv (partial * 2) q-one) q-two)
        q-minus-one (mapv - (mapv (partial * 2) q-zero) q-one)
        q-n-plus-one (mapv - (mapv (partial * 2) q-n) q-n-minus-one)
        q-n-plus-two (mapv - (mapv (partial * 2) q-n-plus-one) q-n)
        qk-values (into [q-minus-one q-zero] (conj qk-values-initial q-n-plus-one q-n-plus-two))
        alpha-k-fallback-value (cond (= corner-perservation :smooth) 0.5
                                     (= corner-perservation :preserve) 1)
        alpha-k-values (vec (for [k (range 2 (+ n 3))
                                  :let [index (dec k)
                                        qk-minus-one (nth qk-values (dec index))
                                        qk (nth qk-values index)
                                        qk-plus-one (nth qk-values (inc index))
                                        qk-plus-two (nth qk-values (+ index 2))
                                        magnitude-of-qk-minus-one-times-qk (length (cross qk-minus-one qk))
                                        magnitude-of-qk-plus-one-times-qk-plus-two (length (cross qk-plus-one qk-plus-two))
                                        denominator (+ magnitude-of-qk-minus-one-times-qk magnitude-of-qk-plus-one-times-qk-plus-two)]]
                              (if (zero? denominator) alpha-k-fallback-value
                                  (/ magnitude-of-qk-minus-one-times-qk
                                     denominator))))
        Vk-values (vec (for [k (range 0 (count alpha-k-values))
                             :let [alpha-k (nth alpha-k-values k)
                                   qk (nth qk-values k)
                                   qk-plus-one (nth qk-values (inc k))]]
                         (mapv + (mapv (partial * (- 1 alpha-k)) qk) (mapv (partial * alpha-k) qk-plus-one))))
        Tk-values (vec (mapv (fn [Vk] (mapv #(/ % (length Vk)) Vk)) Vk-values))]
    Tk-values))

(comment
  (calculate-tangents-for-local-cubic-curve-interpolation [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]]))


(defn calculate-u-k-local-cubic-curve-interpolation [n points]
  (let [u-k-values (double-array  (inc n) 0.0)] 
    (loop [uk 0.0
           k 0] 
      (if (< k n)
        (let [index (inc k)
              point-index (* k 3)
              u-k-plus-one (+ uk (* 3 (length (mapv - (nth points (inc point-index)) (nth points point-index)))))] 
          (aset u-k-values index u-k-plus-one)
          (recur u-k-plus-one (inc k)))
        (vec u-k-values)))))

(defn local-cubic-curve-interpolation [points tangents]
  (let [n (dec (count points))
        a-values (vec (for [index (range 0 n)]
                        (- 16.0 (pow (length (mapv + (nth tangents index) (nth tangents (inc index)))) 2))))
        b-values (vec (for [index (range 0 n)]
                        (* 12.0 (dot  (mapv - (nth points (inc index)) (nth points index))
                                      (mapv + (nth tangents index) (nth tangents (inc index)))))))
        c-values (vec (for [index (range 0 n)]
                        (* -36.0 (pow (length (mapv - (nth points (inc index)) (nth points index))) 2))))
        alpha-values (mapv (fn [a b c] (let [[root-1 root-2] (quadratic-roots a b c)]
                                         (if (pos? root-1) root-1 root-2))) a-values b-values c-values)
        control-points (vec (apply concat (for [k (range 0 n)
                                                :let [pk-zero (nth points k)
                                                      pk-three (nth points (inc k))
                                                      alpha-k (nth alpha-values k)
                                                      alpha-times-T-k-zero (mapv (partial * alpha-k) (nth tangents k))
                                                      alpha-times-T-k-three (mapv (partial * alpha-k) (nth tangents (inc k)))
                                                      pk-one (mapv + pk-zero (mapv (partial *  (/ 1 3)) alpha-times-T-k-zero))
                                                      pk-two (mapv - pk-three (mapv (partial *  (/ 1 3)) alpha-times-T-k-three))]]
                                            
                                            (if (= k (- n 1)) [pk-zero  pk-one pk-two pk-three] [pk-zero  pk-one pk-two]))))
        control-points-without-inner-Qs (remove-by-index control-points (mapv (partial * 3) (range 1 n)))
        u-k-values (calculate-u-k-local-cubic-curve-interpolation n control-points)
        u-n (peek u-k-values)
        U (into [0.0 0.0 0.0 0.0] (conj (vec (apply concat (for [k (range 1 n)
                                                                 :let [u-k (nth u-k-values k)
                                                                       uk-over-u-n (/ u-k u-n)]]
                                                             [uk-over-u-n uk-over-u-n]))) 1.0 1.0 1.0 1.0))]
    {:U  (mapv (partial * (-  (count control-points-without-inner-Qs) 3)) U) :P control-points-without-inner-Qs}))

(defn local-cubic-curve-interpolation-with-calculated-tangents [points & {:keys [corner-perservation] :or {corner-perservation :smooth}}]
  (let [tangents (calculate-tangents-for-local-cubic-curve-interpolation points :corner-perservation corner-perservation)]
    (local-cubic-curve-interpolation points tangents)))

(defn local-cubic-curve-interpolation-with-calculated-tangents-curve [points steps &{:keys [corner-perservation] :or {corner-perservation :smooth}}]
  (let [curve-parameters (local-cubic-curve-interpolation-with-calculated-tangents points :corner-perservation corner-perservation)]
    (non-uniform-b-spline (:P curve-parameters) 3 (:U curve-parameters) steps)
    )
  )

(comment
  (local-cubic-curve-interpolation-with-calculated-tangents [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]]))


(defn bisector [v1 v2]
  (mapv + (mapv / v1 (normalise v1)) (mapv / v2 (normalise v2)))
  )

(comment  (normalise [-5 -5 0]))
(comment (mapv / [10 8 6] [5 2 2]))
(comment (bisector (mapv - [-5 -5 1] [0.0 0.0 0.0]) (mapv - [5 5 1] [0.0 0.0 0.0])))

(defn calculate-Rk-weight [Qk-minus-one Rk Qk]
  (let [Qk-minus-one-to-Rk (mapv - Rk Qk-minus-one)
        Rk-to-Qk (mapv - Qk Rk)
        points-are-collinear 
        (< (abs (reduce + (cross Qk-minus-one-to-Rk Rk-to-Qk))) epsilon)
        isosceles (< (abs (- (magnitude Qk-minus-one-to-Rk) (magnitude Rk-to-Qk))) epsilon)]
  (cond points-are-collinear 0
        isosceles 1
        :else (let [M (mapv (partial * 0.5) (mapv + Qk-minus-one Qk))
                    MR (mapv - M Rk)
                    bisector-Rk-Qk-minus-one-Qk (bisector (mapv - Rk Qk-minus-one) (mapv - Qk Qk-minus-one))
                    bisector-Qk-minus-one-Qk-Rk (bisector (mapv - Qk-minus-one Qk) (mapv - Rk Qk))
                    ]))
  
    )
  )

(comment (calculate-Rk-weight [0 0 0] [0 5 0] [0 10 0]))

(defn delta-k [sk sk-minus-one]
  (- sk-minus-one sk))

(defn a-k [delta-k C-u-sk]
   (* (magnitude C-u-sk) delta-k)
  )
(defn b-k [delta-k C-u-sk-plus-one]
  (* (magnitude C-u-sk-plus-one) delta-k))

(defn sk-plus-one [ak sk sk-minus-one bk-minus-one]
  (+ (/ (* ak (- sk sk-minus-one) bk-minus-one)) sk)
  )

;unit-direciton-of-chord
(defn sk [qk qk-plus-one ck]
  (mapv / (mapv - qk-plus-one qk) ck)
  )

;; (defn orthoganal-parameterisation [Qk]
;;   (let [
;;         tk []
;;         m "?"
;;         k-max (- m 2)
;;         tau-zero (/ a-zero delta-zero)
;;         tau-m-minus-two (/ b-m-minus-three delta-m-minus-three)

;;         ])
;;   )

(comment (dot  [1 2 3]  (mul [4 5 6] (dot [7 8 9] [4 5 6]))) )
(defn w-equals-zero-orthoganal-construction [Q p U weights]
  (let [n (dec (count Q))
        Pw (homogenize-cooridinates Q weights)
        chord-lengths (vec (for [k (range n)
                                 :let [Qk (nth Q k)
                                       Qk-plus-one (nth Q (inc k))]]
                             (mapv - Qk-plus-one Qk)
                             ))
        
        ck-values (mapv magnitude chord-lengths)
        Sk-values (vec (for [k (range n)
                             :let [Qk-plus-one-minus-Qk (nth chord-lengths k)
                                   ck (nth ck-values k)]]
                         (mapv #(/ % ck) Qk-plus-one-minus-Qk)
                         ))
        a-k-fn (fn [ck Sk tk tk-plus-one ] (* (* 3 ck) 
                                              (/ (- (dot (mul 2 Sk) tk) (dot Sk (mul tk-plus-one (dot tk tk-plus-one))))
                                                 (- 4 (pow (dot tk tk-plus-one) 2)))))
        bk-fn (fn [ck Sk tk tk-plus-one] (* (* 3 ck)
                                            (/ (- (dot (mul 2 Sk) tk-plus-one) (dot Sk (mul tk (dot tk tk-plus-one))))
                                               (- 4 (pow (dot tk tk-plus-one) 2)))))
        tk-fn (fn [sk] (let [c-u-sk (nth (nurbs-deriv-deboor n p U Q sk weights) 1)]
                         (mapv #(/ % (magnitude c-u-sk)) c-u-sk)))
        sk-plus-one-fn (fn [ak sk sk-minus-one bk-minus-one]
                         (+ (/ (* ak (- sk sk-minus-one))
                               bk-minus-one
                               )sk)
                         )
        s-zero 0
        s-one 1
        t-zero (tk-fn s-zero)
        t-one (tk-fn s-one)
        a-zero (a-k-fn (nth ck-values 0) (nth Sk-values 0) t-zero t-one)
        b-zero (bk-fn  (nth ck-values 0) (nth Sk-values 0) t-zero t-one)
        a-one b-zero
        s-two (sk-plus-one-fn a-one s-one s-zero b-zero)
        t-two (tk-fn s-two)
        b-one (bk-fn (nth ck-values 1) (nth Sk-values 1) t-one t-two)
        a-two b-one
        s-three (sk-plus-one-fn a-two s-two s-one b-one)
        t-three (tk-fn s-three)
        a-two-calc (a-k-fn (nth ck-values 2) (nth Sk-values 2) t-two t-three)
        sk-list (double-array (dec n))
        tk-list (into-array (repeat (dec n) (vec (repeat 4 0.0)))) 
        ak-list (double-array (dec n))
        bk-list (double-array (dec n))
        ]
        ;; (aset sk-list 0 s-zero)
        ;; (aset sk-list 1 s-one)
        ;; (aset tk-list 0 t-zero)
        ;; (aset tk-list 1 t-one)
        ;; (aset ak-list 0 a-zero)
        ;; (aset bk-list 0 b-zero)
        ;; (doseq [k (range 1 (dec n))]
        ;;        (a-k-fn ))
        (- a-two a-two-calc)
        )
  )

(comment (w-equals-zero-orthoganal-construction [[0 0 0] [2 3 0] [5 1 3] [4 3 5]] 3 [0 0 0 0 1 1 1 1] [1 1 1 1]))
(defn local-raional-quadratic-curve-interpolation [Q R])