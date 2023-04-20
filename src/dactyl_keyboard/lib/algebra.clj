(ns dactyl-keyboard.lib.algebra
  
  (:require [clojure.core.matrix :refer [magnitude dot]]
            [clojure.math :refer [pow sqrt]]
            [dactyl-keyboard.lib.curvesandsplines.beziers :refer [bezier-linear]]
            [dactyl-keyboard.lib.geometry :refer [minimum-distance]]
            [numeric.expresso.core :refer [ex simplify solve rearrange ]]))

(defn equation-on-line-for-x [x-zero a t]
  (let [
        x (+ x-zero (* a t))]
    x))

(comment 
  (/ 4 (bigdec 4)))

(defn equation-on-line-for-y [y-zero b t]
  (let [y (+ y-zero (* b t))]
    y))

(defn equation-on-line-for-z [z-zero c t]
  (let [z (+ z-zero (* c t))]
    z))

(defn find-t-using-x [x x-zero a] 
   (/  (- x x-zero) a)  )

(defn find-t-using-y [y y-zero b]
   (/  (- y y-zero)  b))

(defn find-t-using-z [z z-zero c]
   (/ (- z z-zero)  c))

(defn find-point-on-line-using-x [point1 point2 x]
  (let [v (mapv - point1 point2)
        [a b c] v
        [x-zero y-zero z-zero] point2
        t (find-t-using-x x x-zero a)
        point [x (equation-on-line-for-y y-zero b t) (equation-on-line-for-z z-zero c t)]]
    point))

(comment
  (find-point-on-line-using-x  [0 0 0] [10 0 0] 0)
  )

(defn find-point-on-line-using-y [point1 point2 y]
  (let [v (mapv - point1 point2)
        [a b c] v
        [x-zero y-zero z-zero] point2
        t (find-t-using-y y y-zero b)
        point [(equation-on-line-for-x x-zero a t) y (equation-on-line-for-z z-zero c t)]]
    point))

(defn find-point-on-line-using-z [point1 point2 z]
  (let [v (mapv - point1 point2)
        [a b c] v
        [x-zero y-zero z-zero] point2
        t (find-t-using-z z z-zero c)
        point [(equation-on-line-for-x x-zero a t) (equation-on-line-for-y y-zero b t) z]]
    point))

(defn are-points-collinear? [line-point-1 line-point-2 point-to-check]
  (assert (or (not= line-point-1 line-point-2) (not= line-point-1 point-to-check)) "all points are the same")
  (let [point-as-bigdecs (partial mapv bigdec)
        line-point-1-bigdec (point-as-bigdecs line-point-1)
        line-point-2-bigdec (point-as-bigdecs line-point-2)
        point-to-check-bigdec (point-as-bigdecs point-to-check)
        
        find-point-on-line-using-coordinate (cond (not= (nth line-point-1-bigdec 0) (nth line-point-2-bigdec 0)) find-point-on-line-using-x
                                                  (not= (nth line-point-1-bigdec 1) (nth line-point-2-bigdec 1)) find-point-on-line-using-y
                                                  :else find-point-on-line-using-z)
        coordinate (cond (not= (nth line-point-1-bigdec 0) (nth line-point-2-bigdec 0)) 0
                         (not= (nth line-point-1-bigdec 1) (nth line-point-2-bigdec 1)) 1
                         :else 2)
        point-on-line (find-point-on-line-using-coordinate line-point-1-bigdec line-point-2-bigdec (nth point-to-check-bigdec coordinate))
        min-dist (minimum-distance line-point-1-bigdec point-on-line point-to-check-bigdec )
        close (fn [a b] (< (abs (magnitude (mapv - a b))) 1e-10))
        ]
    (println "point-on-line " point-on-line)
    (println " min-dist " min-dist)
    
    (close point-on-line point-to-check-bigdec))
  )

(comment
  (are-points-collinear? [0 0 0] [0 10 0] [0 10 0])
  )

(comment
  (let [endpoint-1 [-100.43 -200.874 -300.789]
        endpoint-2 [30.38 40.39 78.489]
        points (bezier-linear endpoint-1 endpoint-2 100)]
    (doseq [point points]
           (println "point " point " endpoint-1 " endpoint-1 " endpoint-2 " endpoint-2)
           (println "(not= endpoint-1 endpoint-2)" (not= endpoint-1 endpoint-2))
           (println "(not= endpoint-1 point)" (not= endpoint-1 point))
          
           (assert (true? (are-points-collinear? endpoint-1 endpoint-2 point)))
           )
    ))

(comment 
  (and (= [0 0 0] [0 10 0]) (= [0 0 0] [0 4 0])))
(defn quadratic-roots
  "Solve for the 2 roots of a quadratic equation of the form:

       ax^2 + bx + c = 0 
  "
  [a b c]
  (let [discriminant (Math/sqrt (- (* b b) (* 4.0 a c)))
        neg-b        (- b)
        inv-a2       (/ 1.0 (* 2.0 a))
        root-1       (* inv-a2 (+ neg-b discriminant))
        root-2       (* inv-a2 (- neg-b discriminant))]
    [root-1 root-2]))

(comment (simplify (ex (* a 3 4))) )
(comment (let [c 4
               c-squared (pow c 2)
               c-cubed (pow c 3)
               c-to-power-four (pow c 4)
               sk [4 2 0]
               tk [3 8 0]
               tk-plus-one [2 2 0]
               dot-sk-tk (dot sk tk)
               dot-sk-tk-plus-one (dot sk tk-plus-one)
               dot-sk-tk-plus-one-squared dot-sk-tk-plus-one
               A 1
               B 0
               C (* -4 c-squared)
               D (* c-cubed dot-sk-tk dot-sk-tk-plus-one-squared)
               E (- (* 4 c-to-power-four) (* 2 c-to-power-four dot-sk-tk-plus-one-squared))]
           (println "A" A "B" B "C" C "D" D "E" E)
            (solve 'a (ex (= (+ (** a 4) 
                             (* C (** a 2)) 
                 (*  D (** a 2) ) E
                                 ) 0)))
          ; (solve 'a (ex (= 0 (+ (* (** a 2) (+ 21504 -64)) (** a 4) (* 0 a) -5120.0))))
           ))
(comment (solve 'x (ex (= 2 (* 4 x)))))

(comment (sqrt -1))
         
         (defn findRoots [f start stop step eps]
           (filter #(-> (f %) Math/abs (< eps)) (range start stop step)))
         
         (comment (findRoots #(+ (* % % %) (* -3 % %) (* 2 %)) -1.0 3.0 0.0001 0.00000001))

(defn solve-depressed-quartic [e d c])