(ns dactyl-keyboard.lib.curvesandsplines.uniform-b-splines
  (:require
   [clojure.math :refer [floor]]
   [clojure.core.matrix :refer [array inverse]]
   [dactyl-keyboard.lib.curvesandsplines.curve-utils :refer [get-drop-last-point-if-not-last-segment]])
  )


(defn quadratic-uniform-b-spline-point [pi-minus-one pi pi-plus-one t]
  (let [t-squared (Math/pow t 2)
        f1 (/ (+ t-squared (* -2 t) 1) 2)
        f2 (/ (+ (* -2 t-squared) (* 2 t) 1) 2)
        f3 (/ t-squared 2)]
    (+ (* f1 pi-minus-one) (* f2 pi) (* f3 pi-plus-one))))

(defn quadratic-uniform-b-spline-segment [p-i-minus-one p-i p-i-plus-one steps]
  (concat
   (for [index (range 0 (inc steps))]
     [(quadratic-uniform-b-spline-point (nth p-i-minus-one 0) (nth p-i 0) (nth p-i-plus-one 0) (/ index steps))
      (quadratic-uniform-b-spline-point (nth p-i-minus-one 1) (nth p-i 1) (nth p-i-plus-one 1) (/ index steps))
      (quadratic-uniform-b-spline-point (nth p-i-minus-one 2) (nth p-i 2) (nth p-i-plus-one 2) (/ index steps))])))

(defn quadratic-uniform-b-spline [points steps & {:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
  (let [number-of-points (count points)
        number-of-segments (- number-of-points 2)
        segment-steps (/ steps number-of-segments)
        drop-last-point-if-not-last-segment (get-drop-last-point-if-not-last-segment number-of-segments drop-last-point-of-segment)]
    (into [] (apply concat (for [index (range 0 number-of-segments)
                                 :let [i (inc index)]]
                             (drop-last-point-if-not-last-segment i 
                               (quadratic-uniform-b-spline-segment (nth points (dec i)) (nth points i) (nth points (inc i)) segment-steps)))))))

(defn quadratic-uniform-b-spline-through-terminal-endpoint [points steps & {:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
  (let [n (dec (count points))
        find-endpoint-fn #(mapv (partial * (/ 1 2)) %)
        p-minus-one (nth points 0);(find-endpoint-fn (nth points 0))
        p-n-plus-one (nth points n) ;(find-endpoint-fn (nth points n))
        points-with-new-points (into [p-minus-one] (conj points p-n-plus-one))]
    (quadratic-uniform-b-spline points-with-new-points steps :drop-last-point-of-segment drop-last-point-of-segment)))

(defn loop-index-for-closed-b-spline [number-of-points index] (if (>= index number-of-points) (- index number-of-points) index))

(defn quadratic-uniform-b-spline-closed [points steps & {:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
  (let [number-of-points (count points)
        number-of-segments number-of-points
        segment-steps (/ steps number-of-segments)
        drop-last-point-of-segment #(if drop-last-point-of-segment (drop-last %) %)
        loop-index (partial loop-index-for-closed-b-spline number-of-points)]
    (into [] (apply concat (for [index (range 0  number-of-segments)
                                 :let [i (inc index)]]
                             (drop-last-point-of-segment (quadratic-uniform-b-spline-segment
                                                          (nth points (loop-index (dec i)))
                                                          (nth points (loop-index i))
                                                          (nth points (loop-index (inc i)))
                                                          segment-steps)))))))

(defn cubic-uniform-b-spline-segment-point [p-i-minus-one p-i p-i-plus-one p-i-plus-two t]
  (let [t-squared (Math/pow t 2)
        t-cubed (Math/pow t 3)
        f1 (/ (+ (- t-cubed) (* 3 t-squared) (* -3 t) 1) 6)
        f2 (/ (+ (* 3 t-cubed) (* -6 t-squared) 4) 6)
        f3 (/ (+ (* -3 t-cubed) (* 3 t-squared) (* 3 t) 1) 6)
        f4 (/ t-cubed 6)]
    (+ (* f1 p-i-minus-one) (* f2 p-i) (* f3 p-i-plus-one) (* f4 p-i-plus-two))))

(defn cubic-uniform-b-spline-segment-point-with-tension [p-i-minus-one p-i p-i-plus-one p-i-plus-two tension t]
  (let [t-squared (Math/pow t 2)
        t-cubed (Math/pow t 3)
        f1 (/ (+ (* (- 2 tension) t-cubed) (* (- (* 2 tension) 3) t-squared) (* (- tension) t) 1) 6)
        f2 (/ (+ (* (- 6 tension) t-cubed) (* (- tension 9) t-squared) 4) 6)
        f3 (/ (+ (* (- tension 6) t-cubed) (* (- 9 (* 2 tension)) t-squared) (* tension t) 1) 6)
        f4 (/ (+ (* (- tension 2) t-cubed) (* (- 3 tension) t-squared)) 6)]
    (+ (* f1 p-i-minus-one) (* f2 p-i) (* f3 p-i-plus-one) (* f4 p-i-plus-two))))


(defn cubic-uniform-b-spline-segment [p-i-minus-one p-i p-i-plus-one p-i-plus-two steps]
  (concat
   (for [index (range 0 (inc steps))]
     [(cubic-uniform-b-spline-segment-point (nth p-i-minus-one 0) (nth p-i 0) (nth p-i-plus-one 0) (nth p-i-plus-two 0) (/ index steps))
      (cubic-uniform-b-spline-segment-point (nth p-i-minus-one 1) (nth p-i 1) (nth p-i-plus-one 1) (nth p-i-plus-two 1) (/ index steps))
      (cubic-uniform-b-spline-segment-point (nth p-i-minus-one 2) (nth p-i 2) (nth p-i-plus-one 2) (nth p-i-plus-two 2) (/ index steps))])))

(defn cubic-uniform-b-spline-segment-with-tension  [p-i-minus-one p-i p-i-plus-one p-i-plus-two tension steps]
  (concat
   (for [index (range 0 (inc steps))]
     [(cubic-uniform-b-spline-segment-point-with-tension (nth p-i-minus-one 0) (nth p-i 0) (nth p-i-plus-one 0) (nth p-i-plus-two 0) tension (/ index steps))
      (cubic-uniform-b-spline-segment-point-with-tension (nth p-i-minus-one 1) (nth p-i 1) (nth p-i-plus-one 1) (nth p-i-plus-two 1) tension (/ index steps))
      (cubic-uniform-b-spline-segment-point-with-tension (nth p-i-minus-one 2) (nth p-i 2) (nth p-i-plus-one 2) (nth p-i-plus-two 2) tension (/ index steps))])))
(defn cubic-uniform-b-spline [points steps & {:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
  (let [number-of-points (count points)
        number-of-segments (- number-of-points 3)
        segment-steps (floor (/ steps number-of-segments))
        drop-last-point-if-not-last-segment (get-drop-last-point-if-not-last-segment number-of-segments drop-last-point-of-segment)
        curve (into [] (apply concat (for [index (range 0 number-of-segments)
                                           :let [i (inc index)]]
                                       (drop-last-point-if-not-last-segment i
                                                                            (cubic-uniform-b-spline-segment (nth points (dec i)) (nth points i) (nth points (inc i)) (nth points (+ i 2)) segment-steps)))))]
  ;(println "steps is " steps "points " number-of-points "  segments is " number-of-segments "segment-steps is "segment-steps "num points" (count curve))
    curve))

(defn cubic-uniform-b-spline-through-terminal-endpoints [points steps & {:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
  (let [n (dec (count points))
        p-minus-one (mapv - (mapv (partial * 2) (nth points 0)) (nth points 1))
        p-n-plus-1 (mapv - (mapv (partial * 2) (nth points n)) (nth points (dec n)))
        points-with-new-points (into [p-minus-one] (conj points p-n-plus-1))]
    (cubic-uniform-b-spline points-with-new-points steps :drop-last-point-of-segment drop-last-point-of-segment)))

(defn cubic-uniform-b-spline-closed [points steps & {:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
  (let [number-of-points (count points)
        number-of-segments number-of-points
        segment-steps (floor (/ steps number-of-segments))
        drop-last-point-of-segment #(if drop-last-point-of-segment (drop-last %) %)
        loop-index (partial loop-index-for-closed-b-spline number-of-points)]
    (into [] (apply concat (for [index (range 0 number-of-segments)
                                 :let [i (inc index)]]
                             (drop-last-point-of-segment
                              (cubic-uniform-b-spline-segment
                               (nth points (loop-index (dec i)))
                               (nth points (loop-index i))
                               (nth points (loop-index (inc i)))
                               (nth points (loop-index (+ i 2)))
                               segment-steps)))))))

(defn cubic-uniform-b-spline-with-tension [points tension steps & {:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
  (let [number-of-points (count points)
        number-of-segments (- number-of-points 3)
        segment-steps (floor (/ steps number-of-segments))
        drop-last-point-if-not-last-segment (get-drop-last-point-if-not-last-segment number-of-segments drop-last-point-of-segment)]
    (into [] (apply concat (for [index (range 0 number-of-segments)
                                 :let [i (inc index)
                                       find-hermite-p1-or-p2-fn #(mapv + (mapv (fn [v] (/ v 6)) %1) (mapv (partial * (/ 4 6)) %2) (mapv (partial * (/ 1 6)) %3))
                                      ;;  p-i-minus-one (nth points (dec i))
                                      ;;  p-i (nth points i)
                                      ;;  p-i-plus-one (nth points (inc i))
                                      ;;  p-i-plus-two (nth points (+ i 2))
                                      ;;  p1 (find-hermite-p1-or-p2-fn p-i-minus-one p-i p-i-plus-one)
                                      ;;  p2 (find-hermite-p1-or-p2-fn  p-i p-i-plus-one p-i-plus-two)
                                      ;;  p1t (mapv (partial * tension) (mapv - p-i-plus-one p-i-minus-one))
                                      ;;  p2t (mapv (partial * tension) (mapv - p-i-plus-two p-i))
                                       ]]
                             (drop-last-point-if-not-last-segment
                              i
                              (cubic-uniform-b-spline-segment-with-tension
                               (nth points (dec i)) (nth points i) (nth points (inc i)) (nth points (+ i 2))
                               tension
                               segment-steps)))))))

(defn generate-basis-matrix-to-find-cubic-uniform-b-spline-points-from-knots [n]
  (let [end-tangent-triplet [-3 0 3]
        core-triplet [1 4 1]
        matrix-dimension (+ n 3)]
    (into [] (for [row (range 0 matrix-dimension)]
               (into [] (for [column (range 0 matrix-dimension)]
                          (cond
                            (or (and (= row 0) (= column 0)) (and (= row (dec matrix-dimension)) (= column (- matrix-dimension 3)))) -3
                            (or (and (= row 0) (= column 2)) (and (= row (dec matrix-dimension)) (= column (dec matrix-dimension)))) 3
                            (= row column) 4
                            (or (and (= column (inc row)) (not= row 0)) (and (= row (inc column)) (not= row (dec matrix-dimension)))) 1
                            :else 0)))))))

(defn calculate-control-points-for-cubic-uniform-b-spline-points-from-knots [tangent1 knots tangent2]
  (let [n (dec (count knots))
        basis-matrix (generate-basis-matrix-to-find-cubic-uniform-b-spline-points-from-knots n)
        basis-matrix-inverse (mapv vec (
                              ;matrix-inverse
                              inverse
                              (array :vectorz basis-matrix)))
        values (mapv #(mapv (partial * 6.0) %) (into [] (concat [tangent1]  knots [tangent2])))]
    (into [] (for [row basis-matrix-inverse]
               (reduce (partial mapv +) (mapv #(mapv (partial * %1) %2) row values))))))

(defn cubic-uniform-b-spline-through-points [tangent1 knots tangent2 steps]
  (let [control-points (calculate-control-points-for-cubic-uniform-b-spline-points-from-knots tangent1 knots tangent2)]
    (cubic-uniform-b-spline control-points steps)))
