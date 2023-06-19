(ns dactyl-keyboard.lib.curvesandsplines.curve-fitting
  (:require [clojure.core.matrix :refer [array cross div dot length magnitude
                                         mul normalise]]
            [clojure.core.matrix.linear :refer [lu solve]]
            [clojure.math :refer [cos pow]]
            [dactyl-keyboard.lib.algebra :refer [quadratic-roots]]
            [dactyl-keyboard.lib.collections :refer [remove-by-index]]
            [dactyl-keyboard.lib.constants :refer [epsilon]]
            [dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline 
    :refer [calculate-averaged-knot-vector-from-u-k
            calculate-averaged-knot-vector-from-u-k-with-end-derivs calculate-knot-span-index
            calculate-natural-knot-vector-from-u-k calculate-non-vanishing-basis-functions
            circular-arc-parameterisation curve-derivs-alg1 ders-basis-funs
            farins-simple-derivative-magnitude-estimation farins-sophisticated-derivative-magnitude-estimation
            get-function-for-u-k-values get-knot-vector-generation-fn non-uniform-b-spline
            total-arc-length-derivative-magnitude-estimation
            total-chord-length-derivative-magnitude-estimation u-k-centripetal u-k-chordal]]
            [dactyl-keyboard.lib.vectors :refer [angle-between-vectors
                                                 three-d-intersection
                                                 two-d-colinearity]]) 
  )







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



(defn global-curve-interp [Q p &{:keys [n point-paramater-calculation-method knot-vector-generation-method] :or {n (dec (count Q)) point-paramater-calculation-method :centripetal knot-vector-generation-method :average}}]
  "global interpolation through n+1 points
   
   -n number of points to be calculated - 1
   -Q pass-through points
   -r number of pass through points
   -p curve degree
   return m U P
   
   U knot vector
   
   "
  (let [m (+ n p 1)
        function-for-u-k-values (get-function-for-u-k-values point-paramater-calculation-method)
        knot-vector-generation-fn (get-knot-vector-generation-fn knot-vector-generation-method :constrained false)
        u-k-values (function-for-u-k-values n Q)
        U-knot-vector (mapv #(/ % (- (inc n) p)) (knot-vector-generation-fn u-k-values n p Q))
        q (+ n 1)
        dim  (count (nth Q 0))
        A (for [i (range 0 q)
                :let [uk-i (nth u-k-values i)
                      span (calculate-knot-span-index n p uk-i U-knot-vector)
                      start-index (- span p)
                      basis-funs (calculate-non-vanishing-basis-functions span uk-i p U-knot-vector)
                      basis-funs-size (count basis-funs)]]
            ;basis-funs
            (into [] (concat (repeat start-index 0.0) basis-funs (repeat (- q (+ basis-funs-size start-index)) 0.0))))
        A-new (do (println "A" A "Q" Q)(array :vectorz A))
        r (count Q) 

        P (solve-with-vector A-new Q);(to-array-2d (repeat q (repeat dim 0.0)))
        ] 
  
    {:m m :U (mapv #(* % (- (inc n) p)) U-knot-vector) :P (mapv (partial vec) (vec P))}))

(comment
  (to-array-2d (repeat 5 (repeat 3 0.0))))



(comment
  (global-curve-interp [[0 0 0] [4 4 0] [8 4 0] [-4 0 0] [-4 -3 0]] 3 :point-paramater-calculation-method :centripetal))

(defn global-curve-interp-curve [Q p steps &{:keys [n point-paramater-calculation-method knot-vector-generation-method] :or {n (dec (count Q)) point-paramater-calculation-method :centripetal knot-vector-generation-method :average}}]
  (let [global-curve-interp-data (global-curve-interp Q p :n n :point-paramater-calculation-method point-paramater-calculation-method :knot-vector-generation-method knot-vector-generation-method)] 
    (non-uniform-b-spline (:P global-curve-interp-data ) p (:U global-curve-interp-data) steps)
    )
  )

(defn  global-curve-interp-with-end-derivatives-and-provided-parameters-and-knot-vector [Q p D-zero D-n u-k-values U-knot-vector & {:keys [n] :or {n (dec (count Q))}}]
  (let [m (+ n p 3)
        U-knot-vector-normalized (mapv #(/ % (- (+ n 3) p)) U-knot-vector)

        Q-with (vec (concat [(nth Q 0) (mapv (partial * (/ (nth U-knot-vector (inc p)) p)) D-zero)]
                            (subvec Q 1 (dec (count Q))) [(mapv (partial * (/ (- 1 (nth U-knot-vector (- m p 1))) p)) D-n) (last Q)]))
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
        P (solve-with-vector A-new Q-with)]
    {:m m :U
     U-knot-vector
     :P (mapv (partial vec) P)}
    )
  )

(defn global-curve-interp-with-end-derivatives [Q p D-zero D-n & {:keys [n point-paramater-calculation-method knot-vector-generation-method orthoganal-uk-values] 
                                                                  :or {n (dec (count Q)) point-paramater-calculation-method :centripetal knot-vector-generation-method :average}}]
  (let [;m (+ n p 3)

        function-for-u-k-values (get-function-for-u-k-values point-paramater-calculation-method)
        knot-vector-generation-fn (get-knot-vector-generation-fn knot-vector-generation-method :constrained true)
        u-k-values (if (= point-paramater-calculation-method :orthgonal-construction)
                     orthoganal-uk-values
                     (function-for-u-k-values n Q))
        U-knot-vector (knot-vector-generation-fn u-k-values n p Q)
        ] 
    (global-curve-interp-with-end-derivatives-and-provided-parameters-and-knot-vector Q p D-zero D-n u-k-values U-knot-vector )
    ))

(defn end-unit-derivatives [tangent-endpoint-zero-to-Q-zero Q-n-to-tangent-endpoint-n]
  (let [D-zero (mapv #(/ % (magnitude tangent-endpoint-zero-to-Q-zero)) tangent-endpoint-zero-to-Q-zero)
        D-n (mapv #(/ % (magnitude Q-n-to-tangent-endpoint-n)) Q-n-to-tangent-endpoint-n)]
    {:D-zero D-zero :D-n D-n}) 
  )

(defn arc-end-derivatives-for-global [Q tangent-endpoint-zero-to-Q-zero Q-n-to-tangent-endpoint-n] 
  (let [magnitude-estimation (total-arc-length-derivative-magnitude-estimation Q)]
    ;{:D-zero D-zero :D-n D-n}
    {:D-zero (mul magnitude-estimation (normalise tangent-endpoint-zero-to-Q-zero))
     :D-n (mul magnitude-estimation (normalise Q-n-to-tangent-endpoint-n))}))

(defn chord-end-derivatives-for-global [Q tangent-endpoint-zero-to-Q-zero Q-n-to-tangent-endpoint-n]
  (let [magnitude-estimation-zero (total-chord-length-derivative-magnitude-estimation [(nth Q 0) (mapv + (nth Q 0) tangent-endpoint-zero-to-Q-zero)])
        magnitude-estimation-n (total-chord-length-derivative-magnitude-estimation [(peek Q) (mapv + (peek Q) Q-n-to-tangent-endpoint-n)])
        D-zero (mapv #(* % magnitude-estimation-zero) (normalise (mapv - (mapv + (nth Q 0) tangent-endpoint-zero-to-Q-zero) (nth Q 0))))
        D-n (mapv #(* % magnitude-estimation-n)  (normalise (mapv - (mapv + (peek Q) Q-n-to-tangent-endpoint-n) (peek Q))))
        magnitude-estimation (total-chord-length-derivative-magnitude-estimation Q)]
    ;{:D-zero D-zero :D-n D-n}
    {:D-zero (mul magnitude-estimation (normalise tangent-endpoint-zero-to-Q-zero)) 
     :D-n (mul magnitude-estimation (normalise Q-n-to-tangent-endpoint-n))}))

(defn farins-simple-for-global [Q tangent-endpoint-zero-to-Q-zero Q-n-to-tangent-endpoint-n]
  (let [{tau-zero :tau-zero
         tau-n :tau-n} (farins-simple-derivative-magnitude-estimation Q)]
    {:D-zero (mul tau-zero (normalise tangent-endpoint-zero-to-Q-zero))
     :D-n (mul tau-n (normalise Q-n-to-tangent-endpoint-n))})
  )

(defn farins-sophisticated-for-global [Q tangent-endpoint-zero-to-Q-zero Q-n-to-tangent-endpoint-n]
  (let [{tau-zero :tau-zero
         tau-n :tau-n} (farins-sophisticated-derivative-magnitude-estimation Q tangent-endpoint-zero-to-Q-zero Q-n-to-tangent-endpoint-n)]
    {:D-zero (mul tau-zero (normalise tangent-endpoint-zero-to-Q-zero))
     :D-n (mul tau-n (normalise Q-n-to-tangent-endpoint-n))}))

(defn interactive-control-end-derivs [tangent-endpoint-zero-to-Q-zero Q-n-to-tangent-endpoint-n magnitude-estimation-method]
  {:D-zero (mul magnitude-estimation-method (normalise tangent-endpoint-zero-to-Q-zero))
   :D-n (mul magnitude-estimation-method (normalise Q-n-to-tangent-endpoint-n))}
  )


(defn end-direction-cosines [tangent-endpoint-zero Q tangent-endpoint-n]
  
  )


(defn global-curve-interp-with-end-derivatives-calculated [Q p  tangent-endpoint-zero tangent-endpoint-n & {:keys [n point-paramater-calculation-method knot-vector-generation-method
                                                                                                                  magnitude-estimation-method tangent-endpoints t-zero-is-deriv
                                                                                                                   t-n-is-deriv] 
                                                                                                           :or {n (dec (count Q)) point-paramater-calculation-method :centripetal
                                                                                                                knot-vector-generation-method :average
                                                                                                                magnitude-estimation-method :arc tangent-endpoints true
                                                                                                                t-zero-is-deriv false t-n-is-deriv false}}] 
  (let [tangent-endpoint-zero-to-Q-zero (mapv - (nth Q 0) tangent-endpoint-zero)
        Q-n-to-tangent-endpoint-n (mapv - tangent-endpoint-n (last Q))
        t-zero-to-pass (if tangent-endpoints tangent-endpoint-zero-to-Q-zero tangent-endpoint-zero) 
        t-n-to-pass (if tangent-endpoints Q-n-to-tangent-endpoint-n tangent-endpoint-n)
        end-derivs (if (number? magnitude-estimation-method) 
                     (interactive-control-end-derivs t-zero-to-pass t-n-to-pass magnitude-estimation-method)
                     (case magnitude-estimation-method
                     :unit (end-unit-derivatives t-zero-to-pass t-n-to-pass)
                     :arc (arc-end-derivatives-for-global Q t-zero-to-pass t-n-to-pass)
                     :chord (chord-end-derivatives-for-global Q t-zero-to-pass t-n-to-pass)
                     :farin-simple (farins-simple-for-global Q t-zero-to-pass t-n-to-pass)
                     :farins-sophisticated (farins-sophisticated-for-global Q t-zero-to-pass t-n-to-pass)
                     :exact {:D-zero t-zero-to-pass 
                             :D-n t-n-to-pass}
                     )) 
        {D-zero :D-zero
         D-n :D-n} end-derivs
        final-deriv-zero (if t-zero-is-deriv tangent-endpoint-zero D-zero )
        final-deriv-n (if t-n-is-deriv tangent-endpoint-n D-n )]
    (global-curve-interp-with-end-derivatives Q p final-deriv-zero final-deriv-n :n n :point-paramater-calculation-method point-paramater-calculation-method :knot-vector-generation-method knot-vector-generation-method)))

(defn global-curve-interp-with-end-unit-derivatives-curve [Q p tangent-endpoint-zero tangent-endpoint-n steps & {:keys [n point-paramater-calculation-method knot-vector-generation-method
                                                                                                                        magnitude-estimation-method tangent-endpoints
                                                                                                                         t-zero-is-deriv t-n-is-deriv ] 
                                                                                                                 :or {n (dec (count Q)) point-paramater-calculation-method :centripetal
                                                                                                                      knot-vector-generation-method :average
                                                                                                                      magnitude-estimation-method :unit tangent-endpoints true
                                                                                                                       t-zero-is-deriv false t-n-is-deriv false}}]
  (let [
        curve-interp-data (global-curve-interp-with-end-derivatives-calculated Q p tangent-endpoint-zero tangent-endpoint-n :n n :point-paramater-calculation-method point-paramater-calculation-method
                                                                               :knot-vector-generation-method knot-vector-generation-method
                                                                               :magnitude-estimation-method magnitude-estimation-method
                                                                               :tangent-endpoints tangent-endpoints
                                                                               :t-zero-is-deriv t-zero-is-deriv :t-n-is-deriv t-n-is-deriv)
        ] 
    (non-uniform-b-spline (:P curve-interp-data) p (:U curve-interp-data) steps)
    )
  )


(comment
  (let [a (global-curve-interp-with-end-derivatives [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]]  2 [-1 -1 0] [0 -2 0] :point-paramater-calculation-method :chordal)
        p (global-curve-interp-with-end-derivatives [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]]  2 [-1 -1 0] [0 -2 0] :point-paramater-calculation-method :centripetal :knot-vector-calculation-method :natural)]
    (println "a" (:U a))
    (println "p" (:U p))))

(defn global-curve-interp-with-first-derivatives-quadratic-knot-vector [u-k-values] 
  (let [n (dec (count u-k-values))
        knot-vector (vec (concat [0.0 0.0 0.0](apply concat (for [k (range 1  n)
        :let [uk (nth u-k-values k)
              uk-minus-one (nth u-k-values (dec k))]]
    ;(if (= k (- (count u-k-values) 2))
      ;[(/ (+ (nth u-k-values (dec k)) 1) 2)]
      [(/ (+ uk-minus-one uk) 2)
     uk]
      ;)
    ))[(/ (+ (nth u-k-values (dec n)) 1 ) 2) 1.0 1.0 1.0]
               
               ))] 
    knot-vector
    )
  )

(defn quad-to-av [u-k-values]
  (let [n (dec (count u-k-values))]
    (vec (concat [0.0] (apply concat (for [k (range 1  n)
                      :let [uk (nth u-k-values k)
                            uk-minus-one (nth u-k-values (dec k))]]
    ;(if (= k (- (count u-k-values) 2))
      ;[(/ (+ (nth u-k-values (dec k)) 1) 2)]
                  [(/ (+ uk-minus-one uk) 2)
                   uk]
      ;)
                  ))[(/ (+ (nth u-k-values (dec n)) 1) 2) 1.0]))))
(comment (let [u-k-values [0.0 0.3157486525980598 0.6314973051961192 0.7806428519951074 0.8903214259975537 1.0]]
           (nth u-k-values (- (count u-k-values) 2)))
         )



(comment (let [u-k-values [0.0 0.3157486525980598 0.6314973051961192 0.7806428519951074 0.8903214259975537 1.0]
               new (vec (concat [0.0] (apply concat (mapv #(vector % %) (subvec u-k-values 1 (dec (count u-k-values))))) [1.0]))
               av (calculate-averaged-knot-vector-from-u-k-with-end-derivs new (dec (count new)) 3)]))
(comment (let [u-k-values [0.0 0.3157486525980598 0.6314973051961192 0.7806428519951074 0.8903214259975537 1.0]
               new-uk (quad-to-av u-k-values)
               n (dec (count new-uk))
               new-av (calculate-averaged-knot-vector-from-u-k-with-end-derivs (drop 0 new-uk) (dec n) 2)
               ] 
           (println "new-av" (div new-av (peek new-av)))
          (global-curve-interp-with-first-derivatives-quadratic-knot-vector u-k-values) 
           ))
(comment (global-curve-interp-with-first-derivatives-quadratic-knot-vector [0.0 0.3157486525980598 0.6314973051961192 0.7806428519951074 0.8903214259975537 1.0]))
(comment (let [p 2
               av (calculate-averaged-knot-vector-from-u-k-with-end-derivs [0.0 0.3157486525980598 0.6314973051961192 0.7806428519951074 0.8903214259975537 1.0] 4 2)
               av-norm (div av (peek av)) 
               ]
           (global-curve-interp-with-first-derivatives-quadratic-knot-vector (vec (drop-last p (drop p av-norm))))
           ;(div av (peek av))
           ))
(defn global-curve-interp-with-first-derivatives-cubic-knot-vector [u-k-values] 
  (let [n (dec (count u-k-values))
        knot-vector (vec (concat  [0.0 0.0 0.0 0.0 (/ (nth u-k-values 1) 2)]
                                  (apply concat
                                         (for [k (range 1 (dec n))
                                               :let [uk (nth u-k-values k)
                                                     uk-plus-one (nth u-k-values (inc k))]]
                                           [(/ (+ (* 2 uk) uk-plus-one) 3)
                                            (/ (+ uk (* 2 uk-plus-one)) 3)]))
                                  [(/ (+ (nth u-k-values (dec n)) 1) 2) 1.0 1.0 1.0 1.0]))]
    
    knot-vector
    )
  )

(comment (let [u-k-values [0.0 0.3157486525980598 0.6314973051961192 0.7806428519951074 0.8903214259975537 1.0]
               new (vec (concat [0.0] (apply concat (mapv #(vector % %) (subvec u-k-values 1 (dec (count u-k-values))))) [1.0]))
               av (calculate-averaged-knot-vector-from-u-k-with-end-derivs new (dec (count new)) 3)]
           (println (global-curve-interp-with-first-derivatives-cubic-knot-vector u-k-values))
           (println (/ (reduce + (subvec new 1 3)) 3))
           (div av (peek av))))

(comment (global-curve-interp-with-first-derivatives-cubic-knot-vector [0.0 0.3709837081833865 0.6941014688567921 1.0])
  )

(defn global-curve-interp-with-first-derivatives 
  "From pg 373 of the Nurbs Book"
  [Q D p &{:keys [point-paramater-calculation-method calculated-u-k-values n] 
                                                          :or {point-paramater-calculation-method :chordal n (dec (count Q))
                                                               }}] 
  (let [function-for-u-k-values (get-function-for-u-k-values point-paramater-calculation-method)
        u-k-values (if (nil? calculated-u-k-values)(function-for-u-k-values n Q)
                     calculated-u-k-values)
        num-of-equations (* 2 (inc n))
        knot-vector (case p
                      2 (global-curve-interp-with-first-derivatives-quadratic-knot-vector u-k-values)
                      3 (global-curve-interp-with-first-derivatives-cubic-knot-vector u-k-values)) 
        m-original   (+ n p 1)
        m (count knot-vector) 
        Q-matrix (vec (for [k (range (inc n))
                     :let [uk (nth u-k-values k)
                           span (calculate-knot-span-index (inc (* 2 n)) p uk knot-vector)
                           start-index (- span p)
                           basis-funs (calculate-non-vanishing-basis-functions span uk p knot-vector)
                           basis-funs-size (count basis-funs)
                           ;extra-padding (+ n )
                           ]]
                  (vec (concat (repeat start-index 0.0) basis-funs (repeat (-  (* 2 (inc n)) (+ basis-funs-size start-index)) 0.0))) 
                 ))
        D-matrix (vec (for [k (range (inc n))
                            :let [uk (nth u-k-values k)
                                  span (calculate-knot-span-index (inc (* 2 n)) p uk knot-vector)
                                  start-index (- span p)
                                  basis-funs (nth (ders-basis-funs span uk p n knot-vector) 1)
                                  basis-funs-size (count basis-funs)
                           ;extra-padding (+ n )
                                  ]]
                         (vec (concat (repeat start-index 0.0) basis-funs (repeat (-  (* 2 (inc n)) (+ basis-funs-size start-index)) 0.0))) 
                        ))
        A (vec (apply concat
                      (for [k (range (inc n))]
                 (if (< k n)[(nth Q-matrix k)
                  (nth D-matrix k)]
                   [(nth D-matrix k)
                    (nth Q-matrix k)]) 
                 ) )
               )
         R (vec (apply concat
                       (for [k (range (inc n))
                             :let [D-multiplier (cond (zero? k) (/ (nth knot-vector 4) 3)
                                            (= n k) (/ (- 1 (nth knot-vector (dec (- m-original p)))) 3) 
                                            :else 1)
                                   D-val (mapv (partial * D-multiplier) (nth D k))]] 
                           (if (< k n)

                             [(nth Q k)
                              D-val]
                             [D-val (nth Q k)])
                         )))
         P (solve-with-vector (array :vectorz A) R)
        ] 
    {:P (vec P) :U (mapv #(* (+ (- (* 2 n) p) 2) %)knot-vector)}
    
    )
  )

(comment (div [10 10 10] 2.0))
(comment (normalise [0.0 0.0 0.0]))

(comment (some zero? [1 3 1]))
(defn global-curve-interp-with-calculated-first-derivatives [Q  tangent-vectors p & {:keys [point-paramater-calculation-method n magnitude-estimation-method]
                                                                    :or {point-paramater-calculation-method :chordal n (dec (count Q)) magnitude-estimation-method :arc}}]
  (let [
        tangent-directions (mapv normalise tangent-vectors)
        magnitude-estimation-fn (if (number? magnitude-estimation-method ) (fn [Q] magnitude-estimation-method) (case magnitude-estimation-method
                                 :chord total-chord-length-derivative-magnitude-estimation 
                                 :arc total-arc-length-derivative-magnitude-estimation 
                                  :farin-simple (fn [Q] (:tau-zero (farins-simple-derivative-magnitude-estimation Q)))))
        D (magnitude-estimation-fn Q)] 
    (global-curve-interp-with-first-derivatives Q (mul D tangent-directions) p :point-paramater-calculation-method point-paramater-calculation-method :n n))
  )

(defn global-curve-interp-with-calculated-first-derivatives-curve [Q tangent-vectors p steps & {:keys [point-paramater-calculation-method n magnitude-estimation-method]
                                                                    :or {point-paramater-calculation-method :chordal n (dec (count Q)) magnitude-estimation-method :arc} }]
  (let [params (global-curve-interp-with-calculated-first-derivatives Q tangent-vectors p :point-paramater-calculation-method point-paramater-calculation-method
                                                                      :n n :magnitude-estimation-method magnitude-estimation-method)] 
    (non-uniform-b-spline (:P params) p (:U params) steps))
  )

(comment (let [n 1]
          (repeat (inc (* 2 (- n 2))) 0.0)))

(comment (global-curve-interp-with-first-derivatives [[0 0 0] [4 3 9] [10 8 9] [3 8 9] [3 9 0] [9 0 4]] [[1 (/ 1 3) 3] [2 2 2] [0 0 0] [2 2 2] [3 2 2] [1 1 2]] 3))

(defn solve-tridiagonal-system-for-global-c2-cubic-spline-curve-interpolation [n Q knot-vector P-zero P-one P-n-plus-one P-n-plus-two]
  (println "P-n-plus-one" P-n-plus-one "P-n-plus-two" P-n-plus-two)
  (let [p 3
        overwrite-vec-with-array-fn (fn [array vec] (let [vec-size (count vec)]
                                                      (for [index (range vec-size)]
                                                        (aset array index (nth vec index)))))
        zero-vec (vec (repeat (count P-zero) 0.0))
        P (into-array (repeat (+ n 3) zero-vec))
        R (into-array (repeat (inc n) zero-vec))
        abc (to-array (calculate-non-vanishing-basis-functions 4 (nth knot-vector 4) p knot-vector))
        den (double-array 1 (nth abc 1))
        dd (double-array (inc n) 0.0)
        P-two (div (mapv - (nth Q 1) (mul (nth abc 0) P-one)) (aget den 0))]
  (doseq [i (range 3 n)] (aset R i (nth Q (dec i))))
(aset P 0 P-zero)
(aset P 1 P-one)
(aset P 2 P-two)
(aset P (inc n) P-n-plus-one)
(aset P (+ n 2) P-n-plus-two)
(doseq [i (range 3 n)]
  (aset dd i (/ (aget abc 2) (aget den 0)))
  (overwrite-vec-with-array-fn abc (calculate-non-vanishing-basis-functions (+ i 2) (nth knot-vector (+ i 2))
                                                                            3 knot-vector))
  (aset den 0 (- (aget abc 1) (* (aget abc 0) (aget dd i))))
  (aset P i (div (mapv - (aget R i) (mul (aget abc 0) (aget P (dec i)))) (aget den 0)))
  )
(aset dd n (/ (aget abc 2) (aget den 0)))
(overwrite-vec-with-array-fn abc (calculate-non-vanishing-basis-functions
                                  (+ n 2) (nth knot-vector (+ n 2))
                                  3 knot-vector))
(aset den 0 (- (aget abc 1) (* (aget abc 0) (aget dd n))))
(aset P n (div (mapv - (nth Q (dec n)) (mul (aget abc 2) (aget P (inc n)))
                     (mul (aget abc 0) (aget P (dec n))))
               (aget den 0)))
(doseq [i (range (dec n) 1 -1)]
  (aset P i (mapv - (aget P i) (mul (aget dd (inc i)) (aget P (inc i))))))

;{:U knot-vector :P (vec P)}
(println "c2" :U knot-vector "n" n :P (count (vec P)) (vec P))
{:U knot-vector :P (vec P)})
  )

(comment (range (dec 4) 1 -1))

(defn global-c2-cubic-spline-curve-interpolation-knot-vector [u-k-values n p]
  (let [knot-vector-size (+ n 7)
        knot-vector (double-array knot-vector-size 0.0)
        num-segments n
        ]
    (doseq [i (range knot-vector-size)]
     (cond (<= i 3) (aset knot-vector i 0.0)
           (>= i (+ n 3)) (aset knot-vector i (* num-segments 1.0))
           :else (aset knot-vector i (* num-segments (nth u-k-values (- i 3))))) 
    )
    (println "c2 knot-vector in fn"(vec knot-vector))
    (vec knot-vector)
    )
  )


(defn global-c2-cubic-spline-curve-interpolation-with-u-k-values
  "the knot vector described in the nurbs book appears to be a (constrained) natural knot vector"
  [Q D-zero D-n &{:keys [u-k-values n knot-vector-generation-method] :or { n (dec (count Q)) 
                                                                                                                                               knot-vector-generation-method :average}}]
  (let [
        ;; overwrite-vec-with-array-fn (fn [array vec] (let [vec-size (count vec)]
        ;;                                               (for [index (range vec-size)]
        ;;                                                 (aset array index (nth vec index))
        ;;                                                 )))
        p 3 
        knot-vector-generation-fn (get-knot-vector-generation-fn knot-vector-generation-method :constrained true)
        knot-vector (knot-vector-generation-fn u-k-values n p Q)
        P-zero (nth Q 0)
        P-one (mapv + (mul (/ (nth knot-vector 4) 3) D-zero) P-zero)
        P-n-plus-two (peek Q)
        P-n-plus-one (mapv - P-n-plus-two (mul (/ (- 1 (nth knot-vector (+ n 2))) 3) D-n))
        ;zero-vec (vec (repeat (count P-zero) 0.0))
        ;P (into-array (repeat (+ n 3) zero-vec)) 
        ;R (into-array (repeat (inc n) zero-vec))
        ;; (let [R-initial (into-array (repeat (inc n) zero-vec))] (doseq [i (range 3 n)] (aset R-initial i (nth Q (dec i))))
        ;;        (vec R-initial))
        ;abc (to-array (calculate-non-vanishing-basis-functions 4 (nth knot-vector 4) p knot-vector))
        ;den (double-array 1 (nth abc 1))
        ;dd (double-array (inc n) 0.0)
        ;P-two (div (mapv - (nth Q 1) (mul (nth abc 0) P-one)) (aget den 0))
        ]
    (solve-tridiagonal-system-for-global-c2-cubic-spline-curve-interpolation n Q knot-vector P-zero P-one P-n-plus-one P-n-plus-two)
    ;; (println "c2 knot vector" knot-vector)
    ;; (doseq [i (range 3 n)] (aset R i (into [](nth Q (dec i)))))
    ;; (println "P" (mapv vec P))
    ;; (println  P-zero "P-zero")
    ;; (println "P-zero type" (type (vec P-zero)) "Q type"(type (nth Q 2)))
    ;; (println "P-one type" (type P-one) "P-n-plus-two" (type P-n-plus-two))
    ;; (aset P 0  (into [] P-zero))
    ;; (aset P 1  (into [] P-one))
    ;; (aset P 2 P-two)
    ;; (aset P (inc n) P-n-plus-one)
    ;; (aset P (+ n 2) P-n-plus-two)
    ;; (doseq [i (range 3 n)]
    ;;   (aset dd i (/ (aget abc 2) (aget den 0)))
    ;;   (overwrite-vec-with-array-fn abc (calculate-non-vanishing-basis-functions (+ i 2) (nth knot-vector (+ i 2))
    ;;                                                                             3 knot-vector))
    ;;   (aset den 0 (- (aget abc 1) (* (aget abc 0) (aget dd i))))
    ;;   (aset P i (div (mapv - (aget R i) (mul (aget abc 0) (aget P (dec i)))) (aget den 0)))
    ;;   )
    ;; (aset dd n (/ (aget abc 2) (aget den 0)))
    ;; (overwrite-vec-with-array-fn abc (calculate-non-vanishing-basis-functions 
    ;;                                   (+ n 2) (nth knot-vector (+ n 2))
    ;;                                   3 knot-vector))
    ;; (aset den 0 (- (aget abc 1) (* (aget abc 0) (aget dd n))))
    ;; (aset P n (div (mapv - (nth Q (dec n)) (mul (aget abc 2) (aget P (inc n)))
    ;;                                        (mul (aget abc 0) (aget P (dec n))))
    ;;                (aget den 0)))
    ;; (doseq [i (range (dec n) 1 -1)]
    ;;   (aset P i (mapv - (aget P i) (mul (aget dd (inc i)) (aget P (inc i))))))

    ;; {:U knot-vector :P (vec P)}
    )
  )
(defn global-c2-cubic-spline-curve-interpolation [Q D-zero D-n & {:keys [point-paramater-calculation-method n knot-vector-generation-method] :or {point-paramater-calculation-method :centripetal n (dec (count Q))
                                                                                                                                                  knot-vector-generation-method :average}}]
  (let [function-for-u-k-values (get-function-for-u-k-values point-paramater-calculation-method)
        u-k-values (function-for-u-k-values n Q)]
    (global-c2-cubic-spline-curve-interpolation-with-u-k-values Q D-zero D-n :u-k-values u-k-values :n n :knot-vector-generation-method knot-vector-generation-method)))

(comment
  (global-c2-cubic-spline-curve-interpolation [[0 0 0] [4 3 9] [10 8 9] [3 8 9] [3 9 0] [10 0 4]] [1 1 1] [1 1 1])
  )
(defn global-c2-cubic-spline-curve-interpolation-with-tangent-vectors [Q t-zero t-n &{:keys [point-paramater-calculation-method n knot-vector-generation-method magnitude-estimation-method]
                                                                                               :or {point-paramater-calculation-method :centripetal n (dec (count Q))
                                                                                                    knot-vector-generation-method :average
                                                                                                     magnitude-estimation-method :chord}}]
  (let [tangent-direction-zero (normalise t-zero )
        tangent-direction-n (normalise t-n )
        magnitude-estimation-fn (case magnitude-estimation-method
                                  :chord total-chord-length-derivative-magnitude-estimation
                                  :arc total-arc-length-derivative-magnitude-estimation)
        D (magnitude-estimation-fn Q)
        D-zero  (mul D tangent-direction-zero )
        D-n (mul D tangent-direction-n )]
    (global-c2-cubic-spline-curve-interpolation (vec Q) D-zero D-n :point-paramater-calculation-method point-paramater-calculation-method
                                                :knot-vector-generation-method knot-vector-generation-method :n n))
  )

(defn  global-c2-cubic-spline-curve-interpolation-with-tangent-vectors-curve [Q t-zero t-n steps &{:keys [point-paramater-calculation-method n knot-vector-generation-method magnitude-estimation-method]
                                                                                            :or {point-paramater-calculation-method :chordal n (dec (count Q))
                                                                                                 knot-vector-generation-method :average
                                                                                                 magnitude-estimation-method :chord}}]
  (let [params (global-c2-cubic-spline-curve-interpolation-with-tangent-vectors
                 Q t-zero t-n
                 :point-paramater-calculation-method point-paramater-calculation-method
                 :knot-vector-generation-method knot-vector-generation-method
                 :magnitude-estimation-method magnitude-estimation-method :n n)]
    (non-uniform-b-spline (:P params) 3 (:U params) steps)
    )
  )

(comment (range (dec 6) 1 -1))


(defn calculate-tangents-for-local-cubic-curve-interpolation-from-derivatives [Q &{:keys [point-paramater-calculation-method magnitude-estimation-method] :or {point-paramater-calculation-method :chordal
                                                              magnitude-estimation-method :arc }}]
  (let [n (dec (count Q))
        magnitude-estimation-fn (case magnitude-estimation-method
                                  :chord total-chord-length-derivative-magnitude-estimation
                                  :arc total-arc-length-derivative-magnitude-estimation)
        estimated-magnitude (magnitude-estimation-fn Q)
        function-for-u-k-values (get-function-for-u-k-values point-paramater-calculation-method)
        u-k-values (function-for-u-k-values n Q)
        delta-uk-values (let [delta-uk-array (double-array n 0.0)]
                          (doseq [k (range 1 (inc n))
                                  :let [uk (nth u-k-values k)
                                        uk-minus-one (nth u-k-values (dec k))]]
                            (aset delta-uk-array (dec k) (- uk uk-minus-one)))
                          (vec delta-uk-array))
        alpha-k-values (vec (for [k (range 1 n) 
                                  :let [delta-uk (nth delta-uk-values (dec k))
                                        delta-uk-plus-one (nth delta-uk-values k)]]
                              (/ delta-uk (+ delta-uk delta-uk-plus-one))
                              ))
        qk-values (vec (for [k (range 1 (inc n))
                             :let [Qk (nth Q k)
                                   Qk-minus-one (nth Q (dec k))]]
                         (mapv - Qk Qk-minus-one)))
        dk-values (vec (for [k (range 1 (inc n))
                             :let [qk (nth qk-values (dec k))
                                   delta-uk (nth delta-uk-values (dec k))]]
                         (mapv #(/ % delta-uk) qk)))
        Dk-values-initial (vec (for [k (range 1 n)
                                     :let [alpha-k (nth alpha-k-values (dec k))
                                           qk (nth dk-values (dec k))
                                           qk-plus-one (nth dk-values k)]]
                                 (mapv + (mul (- 1 alpha-k) qk) (mul alpha-k qk-plus-one))))
        D-zero (mapv - (mul 2 (nth dk-values 0)) (nth Dk-values-initial 0) )
        D-n  (mapv - (mul 2 (peek dk-values)) (peek Dk-values-initial))
        Dk-values (into [D-zero] (conj Dk-values-initial D-n)) 
        tangents (mapv (fn [Dk] (mapv #(/ % estimated-magnitude) Dk) )Dk-values)
        ]
    (println "u-k-values " u-k-values)
    (println (count u-k-values) (count delta-uk-values))
    (println "qk-values" qk-values)
    (println "dk-values" dk-values)
    (println "Dk-values-initial" Dk-values-initial)
    tangents
    )
  )

(comment (calculate-tangents-for-local-cubic-curve-interpolation-from-derivatives   [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]])
  )

(defn calculate-tangents-for-local-cubic-curve-interpolation-from-tangent [Qk & {:keys [point-paramater-calculation-method corner-perservation use-cross] :or {point-paramater-calculation-method :centripetal corner-perservation :smooth use-cross true}}]
  (let [n (dec (count Qk))
        mul-fn (cond (or (= (count (nth Qk 0)) 2) (false? use-cross)) (fn [a b] (mapv * a))
                     (= (count (nth Qk 0)) 3) cross)
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
                                        magnitude-of-qk-minus-one-times-qk (length (mul-fn qk-minus-one qk))
                                        magnitude-of-qk-plus-one-times-qk-plus-two (length (mul-fn qk-plus-one qk-plus-two))
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
  (calculate-tangents-for-local-cubic-curve-interpolation-from-tangent [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]]))


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

(defn default-local-cubic-interpolotion-knot-vector-generation-method [u-k-values n]
  (let [u-n (peek u-k-values)] 
   (into [0.0 0.0 0.0 0.0] (conj (vec (apply concat (for [k (range 1 n)
                                                         :let [u-k (nth u-k-values k)
                                                               uk-over-u-n (/ u-k u-n)]]
                                                     [uk-over-u-n uk-over-u-n]))) 1.0 1.0 1.0 1.0))))
(comment (let [orig [0.0 0.3395151447377525 0.7431892338227474 1.0727832464109195 1.0]
               inner (subvec orig 1 4)
               inner-new (vec (apply concat (mapv #(vector % %) inner)))
               fin (into [(nth orig 0)] (conj inner-new (peek orig)))]
           fin))


(defn local-cubic-curve-interpolation [points tangents &{:keys [point-paramater-calculation-method knot-vector-generation-method-keyword] 
                                                         :or {point-paramater-calculation-method :default
                                                              knot-vector-generation-method-keyword :default}}]
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
        point-paramater-calculation-fn (if (= point-paramater-calculation-method :default) calculate-u-k-local-cubic-curve-interpolation
                                           (get-function-for-u-k-values point-paramater-calculation-method)) 
        u-k-values (point-paramater-calculation-fn n control-points)
        u-n (peek u-k-values)
        U (if (= knot-vector-generation-method-keyword :default) (default-local-cubic-interpolotion-knot-vector-generation-method u-k-values n)
              (let [inner-knots (subvec u-k-values 1 (dec (count u-k-values)))
                    new-inner-knots (vec (apply concat (mapv #(vector % %) inner-knots)))
                    new-u-k-values (into [(nth u-k-values 0)] (conj new-inner-knots (peek u-k-values)))
                                                  ;; knot-vector (case knot-vector-generation-method-keyword
                                                  ;;               :natural (calculate-natural-knot-vector-from-u-k (mapv #(/ % u-n) new-u-k-values) (+ n 5) 3 :constrained true)
                                                  ;;                   :average (calculate-averaged-knot-vector-from-u-k-with-end-derivs (mapv #(/ % u-n) new-u-k-values) (+ n 3) 3))
                    knot-vector (case knot-vector-generation-method-keyword
                                  :natural (calculate-natural-knot-vector-from-u-k (mapv #(/ % u-n) u-k-values) (+ n 2) 3 :constrained true)
                                  :average (calculate-averaged-knot-vector-from-u-k-with-end-derivs (mapv #(/ % u-n) u-k-values) n 3))
                    knot-vector-size (count knot-vector)
                    normalised-knot-vector (case knot-vector-generation-method-keyword 
                                             :natural (assoc knot-vector (dec knot-vector-size) 1.0 (- knot-vector-size 2) 1.0 
                                                             (- knot-vector-size 3) 1.0 (- knot-vector-size 4) 1.0)
                                             :average (mapv #(/ % (peek knot-vector)) knot-vector)) 
                    U-inner (subvec normalised-knot-vector 4 (- (count  normalised-knot-vector) 4))
                    new-U-inner (vec (apply concat (mapv #(vector % %) U-inner)))
                    final-U (vec (flatten (for [index (range (count normalised-knot-vector))
                                                :let [knot (nth normalised-knot-vector index)]]
                                            (if (and (> index 3) (< index (- (count normalised-knot-vector) 4)))
                                              [knot knot]
                                              knot)
                                            )))
                    ]
                final-U))
        ;U (knot-vector-generation-method-fn u-k-values n)
        ;; (into [0.0 0.0 0.0 0.0] (conj (vec (apply concat (for [k (range 1 n)
        ;;                                                          :let [u-k (nth u-k-values k)
        ;;                                                                uk-over-u-n (/ u-k u-n)]]
        ;;                                                      [uk-over-u-n uk-over-u-n]))) 1.0 1.0 1.0 1.0))
        ]
;    (println "natural" (calculate-natural-knot-vector-from-u-k (mapv #(/ % u-n) u-k-values) (+ n 2) 3 :constrained true))
 ;   (println "default" (default-local-cubic-interpolotion-knot-vector-generation-method u-k-values n))
  ;  (println "u-k-values" u-k-values knot-vector-generation-method-keyword)
   ; (println "U" U knot-vector-generation-method-keyword)
    ;(println "U" (mapv (partial * (-  (count control-points-without-inner-Qs) 3)) U) knot-vector-generation-method-keyword)
    ;(println (count U) knot-vector-generation-method-keyword)
    ;(println "local U" (mapv (partial * (-  (count control-points-without-inner-Qs) 3)) U) )
    {:U  (mapv (partial * (-  (count control-points-without-inner-Qs) 3)) U) :P control-points-without-inner-Qs}))


(defn local-cubic-curve-interpolation-with-calculated-tangents [points & {:keys [corner-perservation point-paramater-calculation-method
                                                                                 knot-vector-generation-method-keyword
                                                                                 magnitude-estimation-method use-cross] 
                                                                          :or {corner-perservation :smooth
                                                                               point-paramater-calculation-method :default
                                                                               knot-vector-generation-method-keyword :default
                                                                               magnitude-estimation-method :default use-cross true}}]
  (let [tangent-calculation-method (case magnitude-estimation-method 
                                     :default (fn [Qk] (calculate-tangents-for-local-cubic-curve-interpolation-from-tangent Qk :corner-perservation corner-perservation :use-cross use-cross))
                                     (fn [Qk] (calculate-tangents-for-local-cubic-curve-interpolation-from-derivatives Qk :magnitude-estimation-method magnitude-estimation-method )))
        tangents (tangent-calculation-method points  )] 
    (local-cubic-curve-interpolation points tangents :point-paramater-calculation-method point-paramater-calculation-method
                                     :knot-vector-generation-method-keyword knot-vector-generation-method-keyword)))

(defn get-U-for-local-cubic-curve-interpolation-segment [segment knot-vector] 
  (nth (subvec knot-vector 2 (- (count knot-vector) 2)) (* segment 2)))

(defn local-cubic-curve-interpolation-with-calculated-tangents-curve [points steps &{:keys [corner-perservation point-paramater-calculation-method
                                                                                            knot-vector-generation-method-keyword
                                                                                            magnitude-estimation-method
                                                                                            start-segment end-segment
                                                                                            use-cross] 
                                                                                     :or {corner-perservation :smooth
                                                                                          point-paramater-calculation-method :default
                                                                                          knot-vector-generation-method-keyword :default
                                                                                          magnitude-estimation-method :default
                                                                                          start-segment 0 end-segment nil
                                                                                          use-cross true}}]
  (let [curve-parameters (local-cubic-curve-interpolation-with-calculated-tangents points :corner-perservation corner-perservation 
                                                                                   :point-paramater-calculation-method point-paramater-calculation-method
                                                                                   :knot-vector-generation-method-keyword knot-vector-generation-method-keyword
                                                                                   :magnitude-estimation-method magnitude-estimation-method
                                                                                   :use-cross use-cross)
        knot-vector (:U curve-parameters)
        get-U (fn [segment] (get-U-for-local-cubic-curve-interpolation-segment segment knot-vector))
        u-start (get-U start-segment)
        u-end (if end-segment (get-U end-segment) end-segment)]
    ;(println "u-start" u-start "u-end" u-end)
    (non-uniform-b-spline (:P curve-parameters) 3 knot-vector steps :u-start u-start :u-end u-end)
    )
  )

(defn local-cubic-curve-interpolation-with-tangents [points tangents & {:keys [point-paramater-calculation-method
                                                                                                       knot-vector-generation-method-keyword]
                                                                                                :or {point-paramater-calculation-method :default
                                                                                                     knot-vector-generation-method-keyword :default}}]
  (let [tangent-directions (mapv normalise tangents)]
    (local-cubic-curve-interpolation points tangent-directions :point-paramater-calculation-method point-paramater-calculation-method
                                     :knot-vector-generation-method-keyword knot-vector-generation-method-keyword))
)

(defn local-cubic-curve-interpolation-with-tangents-curve [points tangents steps & {:keys [point-paramater-calculation-method
                                                                                     knot-vector-generation-method-keyword]
                                                                              :or {point-paramater-calculation-method :default
                                                                                   knot-vector-generation-method-keyword :default}}]
  
    (let [params (local-cubic-curve-interpolation-with-tangents points tangents :point-paramater-calculation-method point-paramater-calculation-method
                                                                :knot-vector-generation-method-keyword knot-vector-generation-method-keyword)]
      (non-uniform-b-spline (:P params) 3 (:U params) steps)
      ))


(comment
  (local-cubic-curve-interpolation-with-calculated-tangents [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]]))


(defn bisector [v1 v2]
  (mapv + (normalise v1) (normalise v2))
  )

(comment  (normalise [-5 -5 0]))
(comment (mapv / [10 8 6] [5 2 2]))
(comment (bisector (mapv - [0 5 0] [0.0 0.0 1]) (mapv - [5 0 0] [0.0 0.0 0.0])))

(defn calculate-Rk-weight [Qk-minus-one Rk Qk]
  (let [Qk-minus-one-to-Rk (mapv - Rk Qk-minus-one)
        Rk-to-Qk (mapv - Qk Rk)
        points-are-collinear 
        (< (abs (two-d-colinearity Qk-minus-one-to-Rk Rk-to-Qk)) epsilon)
        isosceles (< (abs (- (magnitude Qk-minus-one-to-Rk) (magnitude Rk-to-Qk))) epsilon)]
  (cond points-are-collinear 0
        isosceles 1
        :else (let [M (mapv (partial * 0.5) (mapv + Qk-minus-one Qk))
                    MR (mapv - M Rk)
                    bisector-Rk-Qk-minus-one-Qk (bisector (mapv - Qk-minus-one Rk ) (mapv - Qk Qk-minus-one))
                    bisector-Qk-minus-one-Qk-Rk (bisector (mapv -  Qk Qk-minus-one) (mapv - Rk Qk))
                    S-one (three-d-intersection Rk M  Qk-minus-one (mapv + Qk-minus-one bisector-Rk-Qk-minus-one-Qk))
                    S-two (three-d-intersection Rk M  Qk (mapv + Qk bisector-Qk-minus-one-Qk-Rk))
                    S (do (div (mapv + S-one S-two) 2))
                    s (mapv / (mapv - S M) (mapv - Rk M))
                    w1 (do (println "s" s) (mapv / s (mapv #(- 1 %) s)))]
                w1))
  
    )
  )

(cross [1 1 0] [1 1 0])

(defn calculate-R-k-values-for-local-parabolic-or-rational-parabolic-interpolation [Q T-k-values &{:keys [alpha] :or {alpha (/ 2 3)}}]
  (let [n (dec (count Q)) 
        zero-vec (vec (repeat (count (nth Q 0)) 0.0))
        upsilon-k-values-array (double-array (* 2 (inc n)) 0.0)
        upsilon-check (fn [Rk Qk Tk index] (let [u-vector (div (mapv - Rk Qk) Tk)
                                           upsilon (peek u-vector)]
                                       (if (apply = u-vector)
                                         (do (aset upsilon-k-values-array index upsilon) 
                                             (< upsilon 0))
                                           false)))
        upsilon-k-minus-check (fn [Rk Qk-minus-one Tk-minus-one index]
                                (let [u-k-minus-one-vector (div (mapv - Rk Qk-minus-one) Tk-minus-one)
                                      upsilon-minus-one (peek u-k-minus-one-vector)]
                                  (if (apply = u-k-minus-one-vector) (do (aset upsilon-k-values-array (inc index) upsilon-minus-one)
                                                                         (< upsilon-minus-one 0)) false)))
        Rk-array (into-array (vec (repeat (* 2 (inc n)) zero-vec)))
        Qk-array (into-array (vec (repeat (* 2 (inc n)) zero-vec)))
        R-dash-k-fn (fn [Qk-minus-one upsilon-k Tk-minus-one] (mapv + Qk-minus-one (mul upsilon-k Tk-minus-one)))
        R-dash-k-plus-one-fn (fn [Qk upsilon-k-plus-one Tk] (mapv - Qk (mul  upsilon-k-plus-one Tk)))
        Q-dash-k-fn (fn [upsilon-k R-dash-k-plus-one upsilon-k-plus-one R-dash-k]
                      (div (mapv + (mul upsilon-k R-dash-k-plus-one) (mul upsilon-k-plus-one R-dash-k))
                           (+ upsilon-k upsilon-k-plus-one)))]
    (aset Qk-array 0 (nth Q 0)) 
    (let [final-index (loop [index 1 k 1]
      (if (<= k n) 
        (let [Qk (nth Q k)
              Qk-minus-one (nth Q (dec k))
              Tk (nth T-k-values k)
              Tk-minus-one (nth T-k-values (dec k))
              Rk (three-d-intersection Qk-minus-one Tk-minus-one Qk Tk)
              ;upsilon-k-vector (div (mapv - Rk Qk) Tk)
              ;upsilon-k-minus-one-vector (div (mapv - Rk Qk-minus-one) Tk-minus-one)
              ]
          ;(println "upsilon-k" upsilon-k-vector "upsilon-k-minus-one" upsilon-k-minus-one-vector)


          (if Rk
            (if (and (upsilon-check Rk Qk Tk index) (upsilon-k-minus-check Rk Qk-minus-one Tk-minus-one index))
              (do (aset Rk-array index Rk)
                  (aset Qk-array index Qk)
                  ;(aset upsilon-k-values-array index upsilon-k-vector)
                  ;(cond (= index 1)(aset upsilon-k-values-array (dec index) upsilon-k-minus-one-vector))
                  (recur (inc index) (inc k)))
              (let [theta-k (angle-between-vectors (mapv - Qk Qk-minus-one) Tk)
                    theta-k-minus-one (angle-between-vectors (mapv - Qk Qk-minus-one) Tk-minus-one)
                    upsilon-k-new (mul 0.5 (div (magnitude (mapv - Qk-minus-one Qk ))
                                                (+ 1 (* alpha (cos theta-k)) (* (- 1 alpha) (cos theta-k-minus-one)))))
                    upsilon-k-plus-one (mul 0.5 (div (magnitude (mapv - Qk-minus-one Qk ))
                                                     (+ 1 (* alpha (cos theta-k-minus-one)) (* (- 1 alpha) (cos theta-k)))))
                    R-dash-k (do (println "upsilon-k-new" upsilon-k-new "upsilon-k-plus-one" upsilon-k-plus-one)(R-dash-k-fn Qk upsilon-k-new Tk-minus-one))
                    R-dash-k-plus-one (R-dash-k-plus-one-fn Qk upsilon-k-plus-one Tk)
                    Q-dash-k (Q-dash-k-fn upsilon-k-new R-dash-k-plus-one upsilon-k-plus-one R-dash-k)]
                (aset Rk-array index R-dash-k)
                (aset Rk-array (inc index) R-dash-k-plus-one)
                (aset Qk-array index Q-dash-k)
                (aset Qk-array (inc index) Qk) 
                (recur (+ index 2) (inc k))))
            (cond (and (zero? (dot Tk (mapv - Qk Qk-minus-one))) ((zero? (dot Tk-minus-one (mapv - Qk Qk-minus-one)))))
                  (let [Rk (mul 0.5 (mapv + Qk-minus-one Qk)) 
                        ]
                    (do (aset Rk-array index Rk)
                        (aset Qk-array index Qk)
                        (upsilon-check Rk Qk Tk index)
                        
                        (aset upsilon-k-values-array (inc index) (aget upsilon-k-values-array index))
                        ;(cond (= index 1) (aset upsilon-k-values-array (dec index) upsilon-k-minus-one-vector))
                        (recur (inc index) (inc k))))
                  :else (let [upsilon-k (mul 0.5 (magnitude (mapv - Qk Qk-minus-one)))
                              upsilon-k-plus-one upsilon-k
                              R-dash-k (R-dash-k-fn Qk upsilon-k Tk-minus-one)
                              R-dash-k-plus-one (R-dash-k-plus-one-fn Qk upsilon-k-plus-one Tk)
                              Q-dash-k (Q-dash-k-fn upsilon-k R-dash-k-plus-one upsilon-k-plus-one R-dash-k)]
                          (aset Rk-array index R-dash-k)
                          (aset Rk-array (inc index) R-dash-k-plus-one)
                          (aset Qk-array index Q-dash-k)
                          (aset Qk-array (inc index) Qk)
                          (aset Qk-array (inc index) Qk)
                          (aset upsilon-k-values-array index upsilon-k)
                          (aset upsilon-k-values-array (inc index) upsilon-k-plus-one)
                          (recur (+ index 2) (inc k))) 
                  )
            ))
        index
        )
      )
      Rk (subvec (vec Rk-array) 0 (inc final-index))
      Qk (subvec (vec Qk-array) 0 (inc final-index))
      ]
      {:Rk Rk :Qk Qk})
    )
  )

(comment (dot (mapv - [5 0 0] [0 0 0]) (mapv - [5 1 0] [0 1 0])))
(comment (* (magnitude (mapv - [5 0 0] [0 0 0])) (magnitude (mapv - [5 1 0] [0 1 0]))))

(comment (let [Qk [2 3 1]
               Rk [5 2 8]
               Tk [2 1 2]
               upsilon  (mapv / (normalise (mapv - Rk Qk) )Tk)]
           (println "(normalise (mapv - Rk Qk))" (normalise (mapv - Rk Qk)))
           (println "div" (mapv / (mapv - Rk Qk) (normalise (mapv - Rk Qk))))
           (mapv + Qk (mul upsilon Tk))))
(comment (mapv #(vec (drop-last %)) [[0 0 0] [2 3 0] [4 0 0] [8 9 0]]))


(comment (let [M [1 2] 
               P-one [2 5]
               s 0.8
               S (mapv + (mul (- 1 s) M) (mul s P-one))
               s2 (mapv / (mapv - S M) (mapv - P-one M))]
           s2))
(comment [2 3 4] [5 6 7])

(comment (= 1 1 1))

(comment (mapv *  [2 2 0] [1 2 1]))

(comment (cross [2 2 0] [1 2 1]))

(comment (magnitude (div [10 10 10] [2 2 2] )))

(defn calculate-u-k-for-local-parabolic-or-rational-parabolic-interpolation [Q Rk-values]
  (let [n (dec (count Q))
        u0 0
        u1 1
        uk-values (double-array (inc n) 0.0)]
    (aset uk-values 0 u0)
    (aset uk-values 1 u1)
    (doseq [k (range 2 (inc n))
            :let [Rk (nth Rk-values k)
                  Rk-minus-one (nth Rk-values (dec k)) 
                  Qk-minus-one (nth Q (dec k))
                  uk-minus-one (aget uk-values (dec k))
                  uk-minus-two (aget uk-values (- k 2))
                  ]]
           (aset uk-values k (+ uk-minus-one (* (- uk-minus-one uk-minus-two) (/ (magnitude (mapv - Rk Qk-minus-one))
                                                               (magnitude (mapv - Qk-minus-one Rk-minus-one))))))
           )
    (vec uk-values)) 
  )

(comment (let [Q [[0 0 ] [2 3 ] [4 0 ] [8 9 ]]
               T  (calculate-tangents-for-local-cubic-curve-interpolation-from-tangent Q :use-cross false)
               {Rk :Rk
                Qk :Qk} (calculate-R-k-values-for-local-parabolic-or-rational-parabolic-interpolation Q T)
               Rk-weights (vec (for [k (range 1 (count Qk))]
                                 (calculate-Rk-weight (nth Qk (dec k)) (nth Rk k) (nth Qk k))))
               ]
           Rk-weights))


;esitmate tangents
;use tangent intersectons to calculate Rk values
;if Tk-minus-one and Tk are parallel Rk can't be computed by intersection
;then Rk = (mul 0.5 (mapv + Qk-minus-one Qk))
;if Rk can be computed but upsilon-k-minus-one > 0 and upsilon-k < 0 is not true (eq 9.34)
;then use equation 9.41 and 9.42

(defn farin-cubic-b-spline-interpolation [Q D-zero  D-n & {:keys [n point-paramater-calculation-method knot-vector-generation-method ]}]
  (let [point-paramater-calculation-fn (get-function-for-u-k-values point-paramater-calculation-method)
        knot-vector-generation-fn (get-knot-vector-generation-fn knot-vector-generation-method)
        p 3
        u-k-values (point-paramater-calculation-fn n Q)
        knot-vector (knot-vector-generation-fn u-k-values n p Q)
        
        P-zero (nth Q 0)
        P-n (peek Q)
        P-one (mapv + (mapv  #(* (/ (- (nth u-k-values 1)(nth u-k-values 0)) 3) % ) D-zero))
        P-n-minus-one (mapv + (mapv  #(* (/ (- (peek u-k-values) (nth u-k-values (- (count u-k-values) 2)))
                                            3) %) D-n))
        ;; matrix (for [i (range 2 (dec n))
        ;;              :let [uk-i (nth u-k-values i)
        ;;                    span (calculate-knot-span-index n p uk-i knot-vector)
        ;;                    basis-funs (calculate-non-vanishing-basis-functions span uk-i p knot-vector)
        ;;                    ]] 
        ;;          )

        ]))

(comment (let [points [[-2 3 0] [0 0 0] [4 3 0] [2 0 0] [6 0 0] [9 3 0]]
               n (dec (count points))
               p 3
               u-k-values (u-k-centripetal n points)
               knot-vector (calculate-natural-knot-vector-from-u-k u-k-values n p :constrained true)
               i 2
               uk-i (nth u-k-values i)
               span (calculate-knot-span-index n p uk-i knot-vector)
               basis-funs (calculate-non-vanishing-basis-functions span uk-i p knot-vector)]
           basis-funs))

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
(defn w-equals-zero-orthoganal-construction [Q tangent-directions &{:keys [constrained] :or {constrained true}}]
  (let [n (dec (count Q))
        
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
        a-k-fn (fn [ck Sk tk tk-plus-one ] 
                 (println "a (dot Sk tk)" (dot Sk tk) "(dot Sk tk-plus-one)" (dot Sk tk-plus-one))
                 (let [Sk-dot-tk (max (dot Sk tk) 0.5) 
                                                 Sk-dot-tk-plus-one (max (dot Sk tk-plus-one) 0.5)](* (* 3 ck) 
                                              (/ (- (* 2 Sk-dot-tk) (* Sk-dot-tk-plus-one  (dot tk tk-plus-one)))
                                                 (- 4 (pow (dot tk tk-plus-one) 2))))))
        b-k-fn (fn [ck Sk tk tk-plus-one] 
                (println "b (dot Sk tk)" (dot Sk tk) "(dot Sk tk-plus-one)" (dot Sk tk-plus-one))
                (let [Sk-dot-tk (max (dot Sk tk) 0.5)
                                               Sk-dot-tk-plus-one (max (dot Sk tk-plus-one) 0.5)]
                                           (* (* 3 ck)
                                            (/ (- (* 2 Sk-dot-tk-plus-one) (* Sk-dot-tk (dot tk tk-plus-one)))
                                               (- 4 (pow (dot tk tk-plus-one) 2)))))) 
        uk-plus-one-fn (fn [ak uk uk-minus-one bk-minus-one]
                         (+ (/ (* ak (- uk uk-minus-one))
                               bk-minus-one
                               )uk)
                         )
        s-zero 0.0
        s-one 1.0
        a-zero (a-k-fn (nth ck-values 0) (nth Sk-values 0) (nth tangent-directions 0) (nth tangent-directions 1))
        b-zero (b-k-fn  (nth ck-values 0) (nth Sk-values 0) (nth tangent-directions 0) (nth tangent-directions 1))
        a-k-values (vec (for [k (range n)]
                     (if (zero? k)
                       a-zero
                       (a-k-fn (nth ck-values k) (nth Sk-values k) (nth tangent-directions k) (nth tangent-directions (inc k)))
                       )
                     ))
        b-k-values (vec (for [k (range n)]
                          (if (zero? k)
                            b-zero
                            (b-k-fn (nth ck-values k) (nth Sk-values k) (nth tangent-directions k) (nth tangent-directions (inc k))))
                          ))
        u-k-values (double-array (inc n) 0.0)
        ]
         (println "a-k-values" a-k-values)
         (println "b-k-values" b-k-values)
         (aset u-k-values 0 s-zero)
         (aset u-k-values 1 s-one)
         (doseq [k (range 1 n)
                 :let [uk-plus-one (uk-plus-one-fn (nth a-k-values k) (aget u-k-values k) (aget u-k-values (dec k)) (nth b-k-values (dec k)))]] 
                
                (aset u-k-values (inc k) uk-plus-one)
           )
        ;; (aset tk-list 0 t-zero)
        ;; (aset tk-list 1 t-one)
        ;; (aset ak-list 0 a-zero)
        ;; (aset bk-list 0 b-zero)
        ;; (doseq [k (range 1 (dec n))]
        ;;        (a-k-fn ))
         (println "u-k-values" (vec u-k-values))
        (let [u-k-vector (vec u-k-values)
              u-k-values-normalised (mapv #(/ % (peek (vec u-k-values)))(vec u-k-values))
              tau-zero (/ (nth a-k-values 0) (- (nth u-k-vector 1) (nth u-k-vector 0)))
              tau-n (/ (nth b-k-values (- n 2)) (- (nth u-k-vector (dec n)) (nth u-k-vector (- n 2))))] 
          (println "u-k-values" u-k-vector)
          {:u-k-values u-k-values-normalised :tau-zero tau-zero :tau-n tau-n :a-k-values a-k-values :b-k-values b-k-values}
          )
        )
  )
(defn forced-orthogonal-derivative-magnitude-estimation [Q t-zero t-n u-k-values knot-vector]
  (let [n (dec (count Q))
        chord-length (total-chord-length-derivative-magnitude-estimation Q)
        tau-zero-fn (fn [phi] (* phi chord-length))
        tau-n-fn (fn [psi] (* psi chord-length))
        C-fn  (fn [phi psi] (global-curve-interp-with-end-derivatives-and-provided-parameters-and-knot-vector
                             Q  3 (mul (tau-zero-fn phi) t-zero) (mul (tau-n-fn psi) t-n)  u-k-values knot-vector))
        C-second-deriv-fn (fn [phi psi zero-or-n] (let [params (C-fn phi psi)
                                                        P (:P params) 
                                                        U (:U params)
                                                        u (case zero-or-n
                                                            :zero 0.0
                                                            :n (peek U))
                                                        second-deriv (nth (curve-derivs-alg1 (dec (count P)) 3 U P u 2) 2)]
                                                    second-deriv))
        D-zero-second-deriv-fn (fn [phi psi] (dot (C-second-deriv-fn phi psi :zero) t-zero))
        D-n-second-deriv-fn (fn [phi psi] (dot (C-second-deriv-fn phi psi :n) t-n))
        A-zero (D-zero-second-deriv-fn 1 1)
        B-zero (- A-zero (D-zero-second-deriv-fn 0 1))
        C-zero (- A-zero (D-zero-second-deriv-fn 1 0))
        A-n (D-n-second-deriv-fn 1 1)
        B-n (- A-n (D-n-second-deriv-fn 0 1))
        C-n (- A-n (D-n-second-deriv-fn 1 0))
        psi (+ (/ (- (* B-n A-zero) (* B-zero B-n))
                  (- (* B-zero C-n) C-zero)) 1)
        phi (+ (/ (- (- A-zero) (* C-zero (- psi 1)))
               B-zero) 1)
        tau-zero (tau-zero-fn phi)
        tau-n (tau-n-fn psi)]
    (println "tau-zero" tau-zero "tau-n" n )
    {:tau-zero tau-zero :tau-n tau-n}))

(defn global-orthogonal-construction-cubic-interpolation [Q tangent-vectors &{:keys [knot-vector-generation-method constrained magnitude-estimation-method] 
                                                                                 :or {knot-vector-generation-method :natural constrained true
                                                                                      magnitude-estimation-method :orthogonal}} ] 
  (let [n (dec (count Q))
        p 3
        tangent-directions (mapv normalise tangent-vectors)
        {u-k-values :u-k-values  
         tau-zero :tau-zero  
         tau-n :tau-n
         a-k-values :a-k-values 
         b-k-values :b-k-values } (w-equals-zero-orthoganal-construction Q tangent-directions ) 
        ;D-zero (mul tau-zero (nth tangent-directions 0))
        ;D-n (mul tau-n (peek tangent-directions)) 
        knot-vector-generation-fn (get-knot-vector-generation-fn knot-vector-generation-method :constrained true)
        knot-vector (knot-vector-generation-fn u-k-values n p Q)
        mag-fn (fn [D] {:D-zero (mul D (nth tangent-directions 0)) 
                        :D-n (mul D (peek tangent-directions))})  
         {D-zero :D-zero
          D-n :D-n } (case magnitude-estimation-method
                       :chord (mag-fn (total-chord-length-derivative-magnitude-estimation Q))
                       :arc (mag-fn (total-arc-length-derivative-magnitude-estimation Q))
                       :orthogonal {:D-zero (mul tau-zero (nth tangent-directions 0))
                                    :D-n (mul tau-n (peek tangent-directions))})

        ] 
    (if constrained  
      (global-curve-interp-with-first-derivatives Q  (mapv #(mul %1 %2) (conj a-k-values tau-zero) tangent-directions) 3   :calculated-u-k-values u-k-values)
      ;(global-curve-interp-with-end-derivatives-and-provided-parameters-and-knot-vector Q p D-zero D-n u-k-values knot-vector :n n)
      (let [{tau-zero-c2 :tau-zero
             tau-n-c2 :tau-n} (forced-orthogonal-derivative-magnitude-estimation Q (nth tangent-directions 0) (peek tangent-directions)
                                                                                 u-k-values knot-vector)
            D-zero-c2 (mul tau-zero-c2 (nth tangent-directions 0)) 
            D-n-c2 (mul tau-n-c2 (peek tangent-directions))]
        (global-c2-cubic-spline-curve-interpolation-with-u-k-values Q D-zero-c2 D-n-c2 :u-k-values u-k-values :knot-vector-generation-method knot-vector-generation-method)))
    ;(global-curve-interp-with-end-derivatives Q 3 D-zero D-n :knot-vector-generation-method knot-vector-generation-method :orthoganal-uk-values u-k-values)
    )
  )

(defn global-orthogonal-construction-cubic-interpolation-with-tangents [Q tangents & {:keys [knot-vector-generation-method constrained magnitude-estimation-method]
                                                                                                :or {knot-vector-generation-method :natural constrained true
                                                                                                     magnitude-estimation-method :orthogonal}}]
  (let [tangent-directions (mapv normalise tangents)]
    (global-orthogonal-construction-cubic-interpolation Q tangents :knot-vector-generation-method knot-vector-generation-method
                                                        :constrained constrained
                                                        :magnitude-estimation-method magnitude-estimation-method)))
(defn global-orthogonal-construction-cubic-interpolation-curve [Q tangents steps & {:keys [knot-vector-generation-method constrained magnitude-estimation-method]
                                                                                        :or {knot-vector-generation-method :natural constrained true
                                                                                             magnitude-estimation-method :chord}}]
  (let [params (global-orthogonal-construction-cubic-interpolation-with-tangents Q tangents :knot-vector-generation-method knot-vector-generation-method
:constrained constrained
:magnitude-estimation-method magnitude-estimation-method)]
    (non-uniform-b-spline (:P params) 3 (:U params) steps)
    ))
(defn w-equals-zero-orthoganal-construction-c-two [Q t-zero t-n U]
  (let [chord-length (total-chord-length-derivative-magnitude-estimation Q)
        
        ])
  )



(comment (w-equals-zero-orthoganal-construction [[0 0 0] [2 3 0] [5 1 3] [4 3 5]] (mapv normalise [(mapv - [2 3 0] [0 0 0] ) (mapv - [5 1 3] [2 3 0] ) (mapv - [4 3 5] [5 1 3] )  (mapv - [6 8 10] [4 3 5])])))
(defn local-raional-quadratic-curve-interpolation [Q R])