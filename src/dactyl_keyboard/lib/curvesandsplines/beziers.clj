(ns dactyl-keyboard.lib.curvesandsplines.beziers
  (:require [clojure.math :refer [floor]]
            [clojure.core.matrix :refer [mul]]
            [dactyl-keyboard.lib.curvesandsplines.curve-utils :refer[mmul-scalar-to-vector]]
            [dactyl-keyboard.lib.general-maths :refer [factorial]]
            [dactyl-keyboard.lib.vectors :refer [vector-distance]])
  )

(defn bezier-sextic-point [c0 c1 c2 c3 c4 c5 c6 t]
  (let [a (- 1 t)
        b t
        b0 (Math/pow a 6)
        b1 (* 6 (Math/pow a 5) b)
        b2 (* 15 (Math/pow a 4) (Math/pow b 2))
        b3 (* 20 (Math/pow a 3) (Math/pow b 3))
        b4 (* 15 (Math/pow a 2) (Math/pow b 4))
        b5 (* 6 a (Math/pow b 5))
        b6 (Math/pow b 6)]
    (+ (* b0 c0) (* b1 c1) (* b2 c2) (* b3 c3) (* b4 c4) (* b5 c5) (* b6 c6))))

(defn bezier-sextic [p0 p1 p2 p3 p4 p5 p6 steps]
  (for [t (range 0 (inc steps))]
    (into [] (concat [(bezier-sextic-point (first p0) (first p1) (first p2) (first p3) (first p4) (first p5) (first p6) (/ t steps))
                      (bezier-sextic-point (second p0) (second p1) (second p2) (second p3) (second p4) (second p5) (second p6) (/ t steps))
                      (bezier-sextic-point (nth p0 2) (nth p1 2) (nth p2 2) (nth p3 2) (nth p4 2) (nth p5 2) (nth p6 2) (/ t steps))]))))

(defn bezier-quintic-point [c0 c1 c2 c3 c4 c5 t]
  (let [a (- 1 t)
        b t
        b0 (Math/pow a 5)
        b1 (* 5 (Math/pow a 4) b)
        b2 (* 10 (Math/pow a 3) (Math/pow b 2))
        b3 (* 10 (Math/pow a 2) (Math/pow b 3))
        b4 (* 5 a (Math/pow b 4))
        b5 (Math/pow b 5)]
    (+ (* b0 c0) (* b1 c1) (* b2 c2) (* b3 c3) (* b4 c4) (* b5 c5))))

(defn bezier-quintic [p0 p1 p2 p3 p4 p5 steps]
  (for [t (range 0 (inc steps))]
    (into [] (concat [(bezier-quintic-point (first p0) (first p1) (first p2) (first p3) (first p4) (first p5) (/ t steps))
                      (bezier-quintic-point (second p0) (second p1) (second p2) (second p3) (second p4) (second p5) (/ t steps))
                      (bezier-quintic-point (nth p0 2) (nth p1 2) (nth p2 2) (nth p3 2) (nth p4 2) (nth p5 2) (/ t steps))]))))

(defn bezier-quartic-point [c0 c1 c2 c3 c4 t]
  (let [a (- 1 t)
        b t
        b0 (Math/pow a 4)
        b1 (* 4 (Math/pow a 3)  b)
        b2 (* 6 (Math/pow a 2) (Math/pow b 2))
        b3 (* 4 a (Math/pow b 3))
        b4 (Math/pow b 4)]
    (+ (* b0 c0) (* b1 c1) (* b2 c2) (* b3 c3) (* b4 c4))))

(defn bezier-cubic-point-with-tension [c0 c1 c2 c3 tension t]
  (let [t-squared (Math/pow t 2)
        t-cubed (Math/pow t 3)
        f1 (+ (* (- 2 tension) t-cubed) (* (- (* 2 tension) 3) t-squared) 1)
        f2 (+ (* -2 t-cubed) (* 3 t-squared))
        f3 (+ t-cubed (* -2 t-squared) t)
        f4 (- t-squared t-cubed)]
    (+)))



(defn bezier-quartic [p0 p1 p2 p3 p4 steps]
  (for [t (range 0 (inc steps))]
    (into [] (concat [(bezier-quartic-point (first p0) (first p1) (first p2) (first p3) (first p4) (/ t steps))
                      (bezier-quartic-point (second p0) (second p1) (second p2) (second p3) (second p4) (/ t steps))
                      (bezier-quartic-point (nth p0 2) (nth p1 2) (nth p2 2) (nth p3 2) (nth p4 2) (/ t steps))]))))

(defn bezier-cubic-point [c0 c1 c2 c3 t]
  (let [b0 (Math/pow (- 1 t) 3)
        b1 (* (Math/pow (- 1 t) 2) 3 t)
        b2 (* 3 (- 1 t) (Math/pow t 2))
        b3 (Math/pow t 3)]
    (+ (* b0 c0) (* b1 c1) (* b2 c2) (* b3 c3))))

(defn bezier-cubic [p0 p1 p2 p3 steps]
  (for [t (range 0 (inc steps))]
    (into [] (concat [(bezier-cubic-point (first p0) (first p1) (first p2) (first p3) (/ t steps))
                      (bezier-cubic-point (second p0) (second p1) (second p2) (second p3) (/ t steps))
                      (bezier-cubic-point (nth p0 2) (nth p1 2) (nth p2 2) (nth p3 2) (/ t steps))]))))

(defn find-control-points-for-reparametrized-cubic-bezier-curve [p0 p1 p2 p3 a b]
  (let [a-squared (Math/pow a 2)
        a-cubed (Math/pow a 3)
        b-squared (Math/pow b 2)
        b-cubed (Math/pow b 3)
        B-Matrix [[(Math/pow (- 1 a) 3) (* 3 (Math/pow (- a 1) 2) a) (* 3 (- 1 a) a-squared) a-cubed]
                  [(* (Math/pow (- a 1) 2) (- 1 b)) (* (- a 1) (+ (* -2 a) (- b) (* 3 a b))) (* a (+ a (* 2 b) (* -3 a b))) (* a-squared b)]
                  [(* (- 1 a) (Math/pow (+ -1 b) 2)) (* (- b 1) (+ (- a) (* -2 b) (* 3 a b))) (* b (+ (* 2 a) b (* -3 a b))) (* a b-squared)]
                  [(Math/pow (- 1 b) 3) (* 3 (Math/pow (- b 1) 2) b) (* 3 (- 1 b) b-squared) b-cubed]]

        point-column-matrix [[p0] [p1] [p2] [p3]]]
    (mmul-scalar-to-vector B-Matrix point-column-matrix)))

(defn reparametrized-cubic-bezier-curve [p0 p1 p2 p3 a b steps]
  (let [q-points (find-control-points-for-reparametrized-cubic-bezier-curve p0 p1 p2 p3 a b)]
    (bezier-cubic (nth q-points 0) (nth q-points 1) (nth q-points 2) (nth q-points 3) steps)))

(defn bezier-basis-fn [n i t]
  (let [n-factorial (factorial n)
        i-factorial (factorial i)
        n-minus-i (- n i)
        factorial-n-minus-i (factorial n-minus-i)
        n-factorial-over-i-factorial-factorial-n-minus-i-times-point (* (/ n-factorial (* i-factorial factorial-n-minus-i)))]

    (* n-factorial-over-i-factorial-factorial-n-minus-i-times-point (Math/pow t i) (Math/pow (- 1 t) n-minus-i))))

(defn calculate-bezier-basis-functions [n]
  (for [i (range 0 (inc n))] (fn [t] (bezier-basis-fn n i t))))

(defn n-degree-bezier-point [points t &{:keys [basis-functions] :or {basis-functions (calculate-bezier-basis-functions (dec (count points)))}}] 
  (let [n (dec (count points))]
    (apply mapv + (for [i (range 0 (inc n))]
                  (mapv #(* ((nth basis-functions i) t) %) (nth points i)))))
  )

(defn n-degree-bezier-curve [points steps]
  (let [n (dec (count points)) 
        basis-functions (for [i (range 0 (inc n))] (fn [t] (bezier-basis-fn n i t)))
        curve-points (for [index (range 0 (inc steps))
                           :let [t (/ index steps)]]
                       (apply mapv + (for [i (range 0 (inc n))]
                                       (mapv #(* ((nth basis-functions i) t) %) (nth points i)))))]
    (vec curve-points)))

(defn n-degree-bezier-derivative-point [points u &{:keys [n Q] :or {n (dec (count points)) 
                                                                  Q (vec (for [index (range n)]
                                                                           (mul n (mapv - (nth points (inc index)) (nth points index)))))}}]
  (let [C-deriv (reduce + (for [i (range n)
                                :let [basis-fn (bezier-basis-fn n i u)
                                      Qi (nth Q i)]]
                            (mul basis-fn Qi)))
        ]
    C-deriv)
  )

(defn n-degree-bezier-derivative-curve [points steps]
  (let [n (dec (count points))
        Q (vec (for [index (range n)]
                 (mul n (mapv - (nth points (inc index)) (nth points index)))))]
    (vec (for [index (range 0 (inc steps))
          :let [u (/ index steps)]]
      (n-degree-bezier-derivative-point points u :n n :Q Q)
      ))))




(defn bezier-cubic-through-control-points-point [p0 p1 p2 p3 t1 t2]
  (let [tt1 (- 1 t1)
        tt2 (- 1 t2)
        a11 (* 3 tt1 tt1 t1)
        a12 (* 3 tt1 t1 t1)
        a21 (* 3 tt2 tt2 t2)
        a22 (* 3 tt2 t2 t2)
        determinant (- (* a11 a22) (* a12 a21))

        b1 (- p1 (* p0 tt1 tt1 tt1) (* p3 t1 t1 t1))
        b2 (- p2 (* p0 tt2 tt2 tt2) (* p3 t2 t2 t2))
        ct1 (/ (- (* b1 a22) (* b2 a12)) determinant)
        ct2 (/ (+ (* (- b1) a21) (* b2 a11)) determinant)]
    [ct1 ct2]))




(defn bezier-cubic-through-control-points [p0 p1 p2 p3 steps & {:keys [t1 t2] :or {t1 (/ 1 3) t2 (/ 2 3)}}]
  (let [p0-to-p1 (vector-distance p0 p1)
        p1-to-p2 (vector-distance p1 p2)
        p2-to-p3 (vector-distance p2 p3)
        total-distance (+ p0-to-p1 p1-to-p2 p2-to-p3);(vector-distance p0 p3)
        ;t1 (/ p0-to-p1 total-distance)
        ;t2 (/ p2-to-p3 total-distance)
        x (bezier-cubic-through-control-points-point (nth p0 0) (nth p1 0) (nth p2 0) (nth p3 0) t1 t2)
        y (bezier-cubic-through-control-points-point (nth p0 1) (nth p1 1) (nth p2 1) (nth p3 1) t1 t2)
        z (bezier-cubic-through-control-points-point (nth p0 2) (nth p1 2) (nth p2 2) (nth p3  2) t1 t2)
        control-point1 [(nth x 0) (nth y 0) (nth z 0)]
        control-point2 [(nth x 1) (nth y 1) (nth z 1)]]
    (bezier-cubic p0 control-point1 control-point2 p3 steps)))

(defn bezier-quadratic-point [c0 c1 c2 t]
  (let [b0 (Math/pow (- 1 t) 2)
        b1 (* 2 t (- 1 t))
        b2 (Math/pow t 2)]
    (+ (* b0 c0) (* b1 c1) (* b2 c2))))

(defn bezier-quadratic [p0 p1 p2 steps]
  (for [t (range 0 (inc steps))]
    (into [] (concat [(bezier-quadratic-point (first p0) (first p1) (first p2)  (/ t steps))
                      (bezier-quadratic-point (second p0) (second p1) (second p2)  (/ t steps))
                      (bezier-quadratic-point (nth p0 2) (nth p1 2) (nth p2 2)  (/ t steps))]))))


(defn bezier-linear-point-coordinate [c0 c1 t]
  (let [b0 (- 1 t)
        b1 t]
    (+ (* c0 b0) (* c1 b1))))

(defn bezier-linear-point [c0 c1 t]
  (let [b0 (- 1 t)
        b1 t]
    (mapv + (mapv (partial * b0) c0) (mapv (partial * b1) c1 ))))

(defn bezier-linear [p0 p1 steps] 
  (for [t (range 0 (inc steps))]
    (into [] (concat [(bezier-linear-point-coordinate (first p0) (first p1)  (/ t steps))
                      (bezier-linear-point-coordinate (second p0) (second p1)  (/ t steps))
                      (bezier-linear-point-coordinate (nth p0 2) (nth p1 2)  (/ t steps))]))))

(defn bezier-linear-spline [points steps] 
  (let [number-of-segments (dec (count points))
        increment (/ number-of-segments steps)]
    (vec (for [index (range 0 (+ increment number-of-segments) increment)
               :let [i (dec (if (< index number-of-segments) (inc (floor index)) number-of-segments))
                     t (if (< index number-of-segments) (- index (floor index)) 1.0)]]
           (bezier-linear-point (nth points i) (nth points (inc i)) t)
           ))
    ))