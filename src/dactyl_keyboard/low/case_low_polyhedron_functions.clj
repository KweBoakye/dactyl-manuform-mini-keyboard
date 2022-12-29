(ns dactyl-keyboard.low.case-low-polyhedron-functions
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.sa-keycaps :refer :all]
            [dactyl-keyboard.low.placement-functions-low :refer :all]
            [dactyl-keyboard.low.web-connecters-low :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.low.thumbs-low :refer :all]
            [dactyl-keyboard.low.oled-low-placements :refer :all]
            [dactyl-keyboard.low.case-low-functions :refer :all]
            [dactyl-keyboard.oled :refer :all]
            [dactyl-keyboard.tps-65 :refer :all]
            [dactyl-keyboard.EVQWGD001 :refer :all]))

(defn wall-locate1-circular ([dx dy] (wall-locate1-circular dx dy wall-xy-offset)) 
  ([dx dy xy ][(* dx wall-thickness  xy circle-bezier-approx-b) (* dy wall-thickness xy circle-bezier-approx-b) (* -1 circle-bezier-approx-c)]))

(defn wall-locate3-for-polyhedron-point-circular ([dx dy] (wall-locate3-for-polyhedron-point-circular dx dy wall-xy-offset))
  ([dx dy xy]
   [(* 0.98 (* dx circle-bezier-approx-c (+ xy wall-thickness (+ (/ oled-post-size 2)))))
    (* 0.98 (* dy circle-bezier-approx-c (+ xy wall-thickness (+ (/ oled-post-size 2)))))
     (* (+ 0.5 (- wall-z-offset (/ oled-holder-thickness  1))) circle-bezier-approx-b)]))

(defn wall-locate3-for-polyhedron-point-circular-2 ([dx dy] (wall-locate3-for-polyhedron-point-circular-2 dx dy wall-xy-offset))
  ([dx dy xy]
   [(* 0.98 (* dx circle-bezier-approx-a (+ xy wall-thickness (+ (/ oled-post-size 2)))))
    (* 0.98 (* dy circle-bezier-approx-a (+ xy wall-thickness (+ (/ oled-post-size 2)))))
    (* (+ 0.5 (- wall-z-offset (/ oled-holder-thickness  1))) circle-bezier-approx-b)]))

(defn wall-locate-1-to-3-curve-for-polyhedron-control-point [dx dy]
  [(* dx wall-thickness) (* dy wall-thickness) 0])
(defn wall-locate2-for-polyhedron-point ([dx dy] (wall-locate2-for-polyhedron-point dx dy [0 0 0]))
  ([dx dy orig-point] (wall-locate2-for-polyhedron-point dx dy orig-point wall-xy-offset))
  ([dx dy orig-point xy] (mapv + orig-point [(* dx xy) (* dy xy) wall-z-offset])))

(defn wall-locate-1-to-3-curve-for-polyhedron-second-control-point
  ([dx dy] (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx dy [0 0 0]))
  ([dx dy orig-point] (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx dy orig-point wall-xy-offset))
  ([dx dy orig-point xy] (mapv + [(* dx (+ xy wall-thickness (+ (/ oled-post-size 2))))
                                  (* dy (+ xy wall-thickness (+ (/ oled-post-size 2))))
                                  (- wall-z-offset 1)] orig-point)))
(defn wall-locate3-for-polyhedron-point ([dx dy] (wall-locate3-for-polyhedron-point dx dy wall-xy-offset))
  ([dx dy xy]
   [(* 0.98 (* dx (+ xy wall-thickness (+ (/ oled-post-size 2)))))
    (* 0.98 (* dy (+ xy wall-thickness (+ (/ oled-post-size 2)))))
    (+ 0.5 (- wall-z-offset (/ oled-holder-thickness  1)))]))

(defn wall-locate3-xy-for-polyhedron-point [dx dy xy]
  [(* 0.98 (* dx (+ xy wall-thickness (+ (/ oled-post-size 2)))))
   (* 0.98 (* dy (+ xy wall-thickness (+ (/ oled-post-size 2)))))
   (+ 0.5 (- wall-z-offset (/ oled-holder-thickness  1)))])

(defn wall-brace-polyhedron-outer-floor-point ([place dx dy post-position rad-or-deg] (wall-brace-polyhedron-outer-floor-point place dx dy post-position rad-or-deg wall-xy-offset))
  ([place dx dy post-position rad-or-deg xy]
   (let [transform (get-transform-fn rad-or-deg place)
         curve-corner-translation-vector (get-curve-corner-translation-vector post-position)
         curve-post-position-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector) (get-curve-post-outer-x-and-y-vector dx dy))
         wall-locate3-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate3-for-polyhedron-point dx dy xy) curve-post-position-bottom (get-oled-post-outer-x-and-y-vector dx dy))))
         wall-locate3-point-floor (assoc (vec wall-locate3-point) 2 0)
         ]
     wall-locate3-point-floor
     ) 
   ))

(defn wall-brace-polyhedron-points ([place dx dy post-position rad-or-deg] (wall-brace-polyhedron-points place dx dy post-position rad-or-deg wall-xy-offset))
  ([place dx dy post-position rad-or-deg xy]
   (let [transform (get-transform-fn rad-or-deg place)
         web-corner-translation-vector (get-single-plate-corner-position-vector post-position)
         web-post-position-top (transform (web-post-position-top web-corner-translation-vector))
         web-post-position-bottom (transform (web-post-position-bottom web-corner-translation-vector))
         oled-corner-translation-vector (get-oled-corner-translation-vector post-position)
         oled-post-position-top (oled-post-position-top oled-corner-translation-vector)
         oled-post-position-bottom (oled-post-position-bottom oled-corner-translation-vector)
         curve-corner-translation-vector (get-curve-corner-translation-vector post-position)
         curve-post-position-top (mapv + (curve-post-position-top curve-corner-translation-vector) (get-curve-post-outer-x-and-y-vector dx dy))
         curve-post-position-middle (mapv + (curve-post-position-middle curve-corner-translation-vector) (get-curve-post-outer-x-and-y-vector dx dy))
         curve-post-position-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector) (get-curve-post-outer-x-and-y-vector dx dy))
         wall-locate1-point (transform (mapv + (wall-locate1 dx dy) curve-post-position-top))
         wall-locate-1-to-3-curve-for-polyhedron-control-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx dy) curve-post-position-middle (get-oled-post-outer-x-and-y-vector dx dy))))
         wall-locate-1-to-3-curve-for-polyhedron-second-control-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx dy) curve-post-position-middle (get-oled-post-outer-x-and-y-vector dx dy))))
         wall-locate3-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate3-for-polyhedron-point dx dy xy) curve-post-position-bottom (get-oled-post-outer-x-and-y-vector dx dy))))
         wall-locate3-point-floor (assoc (vec wall-locate3-point) 2 0)
         wall-locate-2-top (make-point-z-value-not-below-zero (transform (wall-locate2-for-polyhedron-point dx dy (mapv + oled-post-position-top (get-oled-post-inner-x-and-y-vector dx dy)) xy)))
         wall-locate-2-bottom (make-point-z-value-not-below-zero (transform (wall-locate2-for-polyhedron-point dx dy (mapv + oled-post-position-bottom (get-oled-post-inner-x-and-y-vector dx dy)) xy)))
         wall-locate-2-bottom-floor (assoc (vec wall-locate-2-bottom) 2 0)]

     {:web-post-position-top web-post-position-top
      :web-post-position-bottom web-post-position-bottom
      :oled-post-position-top oled-post-position-top
      :oled-post-position-bottom oled-post-position-bottom
      :wall-locate1-point wall-locate1-point
      :wall-locate-1-to-3-curve-for-polyhedron-control-point wall-locate-1-to-3-curve-for-polyhedron-control-point
      :wall-locate-1-to-3-curve-for-polyhedron-second-control-point wall-locate-1-to-3-curve-for-polyhedron-second-control-point
      :wall-locate3-point wall-locate3-point
      :wall-locate3-point-floor wall-locate3-point-floor
      :wall-locate-2-top wall-locate-2-top
      :wall-locate-2-bottom wall-locate-2-bottom
      :wall-locate-2-bottom-floor wall-locate-2-bottom-floor})))

(defn wall-brace-polyhedron-curve-points ([place dx dy post-position rad-or-deg steps] (wall-brace-polyhedron-curve-points place dx dy post-position rad-or-deg wall-xy-offset steps))
  ([place dx dy post-position rad-or-deg xy steps]
   (let [wall-brace-polyhedron-points-map (wall-brace-polyhedron-points place dx dy post-position rad-or-deg xy)
         outer-points (bezier-quintic
                       (wall-brace-polyhedron-points-map :web-post-position-top)
                       (wall-brace-polyhedron-points-map :wall-locate1-point)
                       (wall-brace-polyhedron-points-map :wall-locate-1-to-3-curve-for-polyhedron-control-point)
                       (wall-brace-polyhedron-points-map :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)
                       (wall-brace-polyhedron-points-map :wall-locate3-point)
                       (wall-brace-polyhedron-points-map :wall-locate3-point-floor)
                       steps)
         inner-points (bezier-cubic
                       (wall-brace-polyhedron-points-map :web-post-position-bottom)
                       (wall-brace-polyhedron-points-map :wall-locate-2-top)
                       (wall-brace-polyhedron-points-map :wall-locate-2-bottom)
                       (wall-brace-polyhedron-points-map :wall-locate-2-bottom-floor)
                       steps)]
     {:outer-points outer-points
      :inner-points inner-points})))

(defn wall-brace-polyhedron-curves ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
                                    (wall-brace-polyhedron-curves place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps]
   (let [wall-brace-polyedron-curve-points1 (wall-brace-polyhedron-points place1 dx1 dy1 post-position-1 rad-or-deg1 xy1)
         wall-brace-polyedron-curve-points2 (wall-brace-polyhedron-points place2 dx2 dy2 post-position-2 rad-or-deg2 xy2)

         web-post-top-curve (bezier-linear  (wall-brace-polyedron-curve-points1 :web-post-position-top)  (wall-brace-polyedron-curve-points2 :web-post-position-top) steps)
         wall-locate1-curve (bezier-linear  (wall-brace-polyedron-curve-points1 :wall-locate1-point)  (wall-brace-polyedron-curve-points2 :wall-locate1-point)  steps)
         wall-locate-1-to-3-curve-for-polyhedron-second-control-curve (bezier-linear  (wall-brace-polyedron-curve-points1 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)  (wall-brace-polyedron-curve-points2 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)  steps)
         wall-locate-1-to-3-curve-for-polyhedron-control-curve (bezier-linear  (wall-brace-polyedron-curve-points1 :wall-locate-1-to-3-curve-for-polyhedron-control-point)  (wall-brace-polyedron-curve-points2 :wall-locate-1-to-3-curve-for-polyhedron-control-point)  steps)
         wall-locate3-curve (bezier-linear  (wall-brace-polyedron-curve-points1 :wall-locate3-point)   (wall-brace-polyedron-curve-points2 :wall-locate3-point) steps)
         wall-locate3-floor-curve (bezier-linear (wall-brace-polyedron-curve-points1 :wall-locate3-point-floor)  (wall-brace-polyedron-curve-points2 :wall-locate3-point-floor)  steps)
         wall-locate2-bottom-floor-curve (bezier-linear  (wall-brace-polyedron-curve-points2 :wall-locate-2-bottom-floor)  (wall-brace-polyedron-curve-points1 :wall-locate-2-bottom-floor) steps)
         wall-locate2-bottom-curve (bezier-linear  (wall-brace-polyedron-curve-points2 :wall-locate-2-bottom)   (wall-brace-polyedron-curve-points1 :wall-locate-2-bottom) steps)
         wall-locate2-top-curve (bezier-linear   (wall-brace-polyedron-curve-points2 :wall-locate-2-top)  (wall-brace-polyedron-curve-points1 :wall-locate-2-top)   steps)
         web-post-bottom-curve (bezier-linear   (wall-brace-polyedron-curve-points2 :web-post-position-bottom)  (wall-brace-polyedron-curve-points1 :web-post-position-bottom) steps)]

     {:web-post-top-curve web-post-top-curve
      :wall-locate1-curve wall-locate1-curve
      :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve wall-locate-1-to-3-curve-for-polyhedron-second-control-curve
      :wall-locate-1-to-3-curve-for-polyhedron-control-curve wall-locate-1-to-3-curve-for-polyhedron-control-curve
      :wall-locate3-curve wall-locate3-curve
      :wall-locate3-floor-curve wall-locate3-floor-curve
      :wall-locate2-bottom-floor-curve wall-locate2-bottom-floor-curve
      :wall-locate2-bottom-curve wall-locate2-bottom-curve
      :wall-locate2-top-curve wall-locate2-top-curve
      :web-post-bottom-curve web-post-bottom-curve})))

(defn wall-brace-polyhedron-outer-floor-linear  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
   (wall-brace-polyhedron-outer-floor-linear place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps]
   (let [wall-brace-polyhedron-outer-floor-point1 (wall-brace-polyhedron-outer-floor-point place1 dx1 dy1 post-position-1 rad-or-deg1 xy1) 
         wall-brace-polyhedron-outer-floor-point2 (wall-brace-polyhedron-outer-floor-point place2 dx2 dy2 post-position-2 rad-or-deg2 xy2 )]
     (bezier-linear wall-brace-polyhedron-outer-floor-point1 wall-brace-polyhedron-outer-floor-point2 steps)
     )
   
   )
)

(defn wall-brace-polyhedron
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
   (wall-brace-polyhedron place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps]
   (let [wall-brace-polyhedron-curves-map (wall-brace-polyhedron-curves place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps)




         outer-points (into [] (apply concat
                                      (for [index (range 0 (inc steps))]
                                        (bezier-quintic
                                         (nth (wall-brace-polyhedron-curves-map :web-post-top-curve) index)
                                         (nth (wall-brace-polyhedron-curves-map :wall-locate1-curve) index)
                                         (nth (wall-brace-polyhedron-curves-map :wall-locate-1-to-3-curve-for-polyhedron-control-curve) index)
                                         (nth (wall-brace-polyhedron-curves-map :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve) index)
                                         (nth (wall-brace-polyhedron-curves-map :wall-locate3-curve) index)
                                         (nth (wall-brace-polyhedron-curves-map :wall-locate3-floor-curve) index)
                                         steps))))

         inner-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                               (bezier-cubic
                                                (nth (wall-brace-polyhedron-curves-map :web-post-bottom-curve) index)
                                                (nth (wall-brace-polyhedron-curves-map :wall-locate2-top-curve) index)
                                                (nth (wall-brace-polyhedron-curves-map :wall-locate2-bottom-curve) index)
                                                (nth (wall-brace-polyhedron-curves-map :wall-locate2-bottom-floor-curve) index)
                                                steps))))
         smoother-wall-polyhedron (polyhedron (concat outer-points inner-points)
                                              (generate-bezier-along-bezier-polyhedron-faces
                                               outer-points inner-points
                                               steps))]
         ;wall-polyhedron 
     smoother-wall-polyhedron)))

;; (defn wall-brace-polyhedron 
;;   ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
;;    (wall-brace-polyhedron place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset steps))
;;   ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps]
;;    (let [transform-1 (get-transform-fn rad-or-deg1 place1)
;;         transform-2 (get-transform-fn rad-or-deg2 place2)
;;         oled-post-outer-x-1 (if (pos? dx1) (/ oled-post-size 2) (/ oled-post-size -2))
;;         oled-post-outer-x-2 (if (pos? dx2) (/ oled-post-size 2) (/ oled-post-size -2))
;;         oled-post-outer-y-1 (if (pos? dy1) (/ oled-post-size 2) (/ oled-post-size -2))
;;         oled-post-outer-y-2 (if (pos? dy2) (/ oled-post-size 2) (/ oled-post-size -2))
;;         oled-post-inner-x-1 (if (pos? dx1) (/ oled-post-size -2) (/ oled-post-size 2))
;;         oled-post-inner-x-2 (if (pos? dx2) (/ oled-post-size -2) (/ oled-post-size 2))
;;         oled-post-inner-y-1 (if (pos? dy1) (/ oled-post-size -2) (/ oled-post-size 2))
;;         oled-post-inner-y-2 (if (pos? dy2) (/ oled-post-size -2) (/ oled-post-size 2))
;;         curve-post-x-1 (if (pos? dx1) (/ curve-post-size 2) (/ curve-post-size -2))
;;         curve-post-y-1 (if (pos? dy1) (/ curve-post-size 2) (/ curve-post-size -2))
;;         curve-post-x-2 (if (pos? dx2) (/ curve-post-size 2) (/ curve-post-size -2))
;;         curve-post-y-2 (if (pos? dy2) (/ curve-post-size 2) (/ curve-post-size -2))






;;         web-corner-translation-vector1 (get-single-plate-corner-position-vector post-position-1)
;;         web-corner-translation-vector2 (get-single-plate-corner-position-vector post-position-2)
;;         web-post-position-1-top (transform-1 (web-post-position-top web-corner-translation-vector1))
;;         web-post-position-1-bottom (transform-1 (web-post-position-bottom web-corner-translation-vector1))
;;         web-post-position-2-top (transform-2 (web-post-position-top web-corner-translation-vector2))
;;         web-post-position-2-bottom (transform-2 (web-post-position-bottom web-corner-translation-vector2))
;;         oled-corner-translation-vector1 (get-oled-corner-translation-vector post-position-1)
;;         oled-corner-translation-vector2  (get-oled-corner-translation-vector post-position-2)
;;         web-corner-translation-vector1 (get-single-plate-corner-position-vector post-position-1)
;;         wneb-corner-translation-vector2 (get-single-plate-corner-position-vector post-position-2)
;;         oled-post-position-1-top (oled-post-position-top oled-corner-translation-vector1)
;;         oled-post-position-1-bottom (oled-post-position-bottom oled-corner-translation-vector1)
;;         oled-post-position-2-top (oled-post-position-top oled-corner-translation-vector2)
;;         oled-post-position-2-bottom  (oled-post-position-bottom oled-corner-translation-vector2)
;;         curve-corner-translation-vector1 (get-curve-corner-translation-vector post-position-1)
;;         curve-corner-translation-vector2 (get-curve-corner-translation-vector post-position-2)

;;         curve-post-position-1-top (mapv + (curve-post-position-top curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))
;;         curve-post-position-1-middle (mapv + (curve-post-position-middle curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))
;;         curve-post-position-1-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))
;;         curve-post-position-2-top (mapv + (curve-post-position-top curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))
;;         curve-post-position-2-middle (mapv + (curve-post-position-middle curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))
;;         curve-post-position-2-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))
;;         wall-locate1-point1 (transform-1 (mapv + (wall-locate1 dx1 dy1) curve-post-position-1-top))
;;         wall-locate1-point2 (transform-2 (mapv + (wall-locate1 dx2 dy2) curve-post-position-2-top))

;;         wall-locate-1-to-3-curve-for-polyhedron-control-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx1 dy1) curve-post-position-1-middle (get-oled-post-outer-x-and-y-vector dx1 dy1))))
;;          wall-locate-1-to-3-curve-for-polyhedron-control-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx2 dy2) curve-post-position-2-middle (get-oled-post-outer-x-and-y-vector dx2 dy2))))

;;         wall-locate-1-to-3-curve-for-polyhedron-second-control-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx1 dy1) curve-post-position-1-middle (get-oled-post-outer-x-and-y-vector dx1 dy1))))
;; wall-locate-1-to-3-curve-for-polyhedron-second-control-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx2 dy2) curve-post-position-2-middle (get-oled-post-outer-x-and-y-vector dx2 dy2))))

;;         wall-locate3-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate3-for-polyhedron-point dx1 dy1 xy1) curve-post-position-1-bottom (get-oled-post-outer-x-and-y-vector dx1 dy1))))
;;         wall-locate3-point1-floor (assoc (vec wall-locate3-point1) 2 0)
;;         wall-locate3-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate3-for-polyhedron-point dx2 dy2 xy2) curve-post-position-2-bottom (get-oled-post-outer-x-and-y-vector dx2 dy2))))

;;         wall-locate3-point2-floor (assoc (vec wall-locate3-point2) 2 0)


;;         wall-locate-2-top1 (transform-1 (wall-locate2-for-polyhedron-point dx1 dy1 (mapv + oled-post-position-1-top (get-oled-post-inner-x-and-y-vector dx1 dy1)) xy1))
;;         wall-locate-2-top2 (transform-2 (wall-locate2-for-polyhedron-point dx1 dy1 (mapv + oled-post-position-2-top (get-oled-post-inner-x-and-y-vector dx2 dy2)) xy2))
;;         wall-locate-2-bottom1 (make-point-z-value-not-below-zero (transform-1 (wall-locate2-for-polyhedron-point dx1 dy1 (mapv + oled-post-position-1-bottom (get-oled-post-inner-x-and-y-vector dx1 dy1)) xy1)))
;;         wall-locate-2-bottom1-floor (assoc (vec wall-locate-2-bottom1) 2 0)
;;         wall-locate-2-bottom2 (make-point-z-value-not-below-zero (transform-2 (wall-locate2-for-polyhedron-point dx2 dy2 (mapv + oled-post-position-2-bottom (get-oled-post-inner-x-and-y-vector dx2 dy2)) xy2)))
;;         wall-locate-2-bottom2-floor (assoc (vec wall-locate-2-bottom2) 2 0)

;;         web-post-top-curve (bezier-linear  web-post-position-1-top  web-post-position-2-top  steps)
;;         wall-locate1-curve (bezier-linear  wall-locate1-point1  wall-locate1-point2  steps)
;;          wall-locate-1-to-3-curve-for-polyhedron-second-control-curve (bezier-linear  wall-locate-1-to-3-curve-for-polyhedron-second-control-point1  wall-locate-1-to-3-curve-for-polyhedron-second-control-point2  steps)
;;         wall-locate-1-to-3-curve-for-polyhedron-control-curve (bezier-linear  wall-locate-1-to-3-curve-for-polyhedron-control-point1  wall-locate-1-to-3-curve-for-polyhedron-control-point2  steps)
;;         wall-locate3-curve (bezier-linear  wall-locate3-point1   wall-locate3-point2 steps)
;;         wall-locate3-floor-curve (bezier-linear wall-locate3-point1-floor  wall-locate3-point2-floor  steps)

;;         wall-locate2-bottom-floor-curve (bezier-linear  wall-locate-2-bottom2-floor  wall-locate-2-bottom1-floor steps)
;;         wall-locate2-bottom-curve (bezier-linear  wall-locate-2-bottom2   wall-locate-2-bottom1 steps)
;;         wall-locate2-top-curve (bezier-linear   wall-locate-2-top2  wall-locate-2-top1   steps)
;;         web-post-bottom-curve (bezier-linear   web-post-position-2-bottom  web-post-position-1-bottom steps)

;;         outer-curve1 (bezier-cubic web-post-position-1-top  wall-locate1-point1 wall-locate3-point1 wall-locate3-point1-floor    steps)
;;         inner-curve1 (bezier-cubic   wall-locate-2-bottom1-floor wall-locate-2-bottom1 wall-locate-2-top1 web-post-position-1-bottom steps)
;;         outer-curve2 (bezier-cubic web-post-position-2-top  wall-locate1-point2  wall-locate3-point2 wall-locate3-point2-floor   steps)
;;         inner-curve2 (bezier-cubic   wall-locate-2-bottom2-floor wall-locate-2-bottom2 wall-locate-2-top2 web-post-position-2-bottom steps)
;;         wall-polyhedron (generate-polyhedron-from-points  outer-curve2 outer-curve1  inner-curve2 inner-curve1   steps)

;;         outer-points (into [] (apply concat
;;                                      (for [index (range 0 (inc steps))]
;;                                        (bezier-quintic
;;                                         (nth web-post-top-curve index)
;;                                         (nth wall-locate1-curve index)
;;                                         (nth wall-locate-1-to-3-curve-for-polyhedron-control-curve index)
;;                                         (nth wall-locate-1-to-3-curve-for-polyhedron-second-control-curve index) 
;;                                         (nth wall-locate3-curve index)
;;                                         (nth wall-locate3-floor-curve index)
;;                                         steps))))

;;         inner-points (into [] (apply concat (for [index (range 0 (inc steps))]
;;                                               (bezier-cubic
;;                                                (nth web-post-bottom-curve index)
;;                                                (nth wall-locate2-top-curve index)
;;                                                (nth wall-locate2-bottom-curve index)
;;                                                (nth wall-locate2-bottom-floor-curve index)
;;                                                steps))))
;;         smoother-wall-polyhedron (polyhedron (concat outer-points inner-points)
;;                                              (generate-bezier-along-bezier-polyhedron-faces
;;                                               outer-points inner-points
;;                                               steps))]
;;          ;wall-polyhedron 
;;     smoother-wall-polyhedron)))

(defn wall-brace-polyhedron-circular-points ([place dx dy post-position rad-or-deg] (wall-brace-polyhedron-circular-points place dx dy post-position rad-or-deg wall-xy-offset))
  ([place dx dy post-position rad-or-deg xy]
   (let [transform (get-transform-fn rad-or-deg place)
         web-corner-translation-vector (get-single-plate-corner-position-vector post-position)
         web-post-position-top (transform (web-post-position-top web-corner-translation-vector))
         web-post-position-bottom (transform (web-post-position-bottom web-corner-translation-vector))
         oled-corner-translation-vector (get-oled-corner-translation-vector post-position)
         oled-post-position-top (oled-post-position-top oled-corner-translation-vector)
         oled-post-position-bottom (oled-post-position-bottom oled-corner-translation-vector)
         curve-corner-translation-vector (get-curve-corner-translation-vector post-position)
         curve-post-position-top (mapv + (curve-post-position-top curve-corner-translation-vector) (get-curve-post-outer-x-and-y-vector dx dy))
         curve-post-position-middle (mapv + (curve-post-position-middle curve-corner-translation-vector) (get-curve-post-outer-x-and-y-vector dx dy))
         curve-post-position-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector) (get-curve-post-outer-x-and-y-vector dx dy))
         circular-z-factor (/ (nth web-post-position-top 2) circle-bezier-approx-a)
         circular-point-1 (transform (mapv + (wall-locate1-circular dx dy) curve-post-position-top))
         circular-point-2 (make-point-z-value-not-below-zero (transform (mapv + (wall-locate3-for-polyhedron-point-circular dx dy xy) curve-post-position-middle)))
         circular-point-3 (make-point-z-value-not-below-zero (transform (mapv + (wall-locate3-for-polyhedron-point-circular-2 dx dy xy) curve-post-position-middle)))
         circular-point-floor (assoc (vec (make-point-z-value-not-below-zero (transform (mapv + (wall-locate3-for-polyhedron-point-circular-2 dx dy xy) curve-post-position-bottom)))) 2 0)
         wall-locate1-point (transform (mapv + (wall-locate1 dx dy) curve-post-position-top))
         wall-locate-1-to-3-curve-for-polyhedron-control-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx dy) curve-post-position-middle (get-oled-post-outer-x-and-y-vector dx dy))))
         wall-locate-1-to-3-curve-for-polyhedron-second-control-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx dy) curve-post-position-middle (get-oled-post-outer-x-and-y-vector dx dy))))
         wall-locate3-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate3-for-polyhedron-point dx dy xy) curve-post-position-bottom (get-oled-post-outer-x-and-y-vector dx dy))))
         wall-locate3-point-floor (assoc (vec wall-locate3-point) 2 0)
         wall-locate-2-top (make-point-z-value-not-below-zero (transform (wall-locate2-for-polyhedron-point dx dy (mapv + oled-post-position-top (get-oled-post-inner-x-and-y-vector dx dy)) xy)))
         wall-locate-2-bottom (make-point-z-value-not-below-zero (transform (wall-locate2-for-polyhedron-point dx dy (mapv + oled-post-position-bottom (get-oled-post-inner-x-and-y-vector dx dy)) xy)))
         wall-locate-2-bottom-floor (assoc (vec wall-locate-2-bottom) 2 0)]

     {:web-post-position-top web-post-position-top 
      :circular-point-1 circular-point-1
      :circular-point-2 circular-point-2 
      :circular-point-3 circular-point-3
      :circular-point-floor circular-point-floor 
      :web-post-position-bottom web-post-position-bottom
      :wall-locate-2-top wall-locate-2-top
      :wall-locate-2-bottom wall-locate-2-bottom
      :wall-locate-2-bottom-floor wall-locate-2-bottom-floor})))

(defn wall-brace-polyhedron-circular-outer-floor-point ([place dx dy post-position rad-or-deg] (wall-brace-polyhedron-circular-outer-floor-point place dx dy post-position rad-or-deg wall-xy-offset))
  ([place dx dy post-position rad-or-deg xy]
   (let [transform (get-transform-fn rad-or-deg place)
         curve-corner-translation-vector (get-curve-corner-translation-vector post-position)
         curve-post-position-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector) (get-curve-post-outer-x-and-y-vector dx dy))
          circular-point-3 (make-point-z-value-not-below-zero (transform (mapv + (wall-locate3-for-polyhedron-point-circular-2 dx dy xy) curve-post-position-bottom)))
circular-point-floor (assoc (vec circular-point-3) 2 0)]
     circular-point-floor)))

(defn wall-brace-polyhedron-circular-curve-points ([place dx dy post-position rad-or-deg steps] (wall-brace-polyhedron-circular-curve-points place dx dy post-position rad-or-deg wall-xy-offset steps))
  ([place dx dy post-position rad-or-deg xy steps]
   (let [wall-brace-polyhedron-points-map (wall-brace-polyhedron-circular-points place dx dy post-position rad-or-deg xy)
         outer-points (bezier-quartic
                       (wall-brace-polyhedron-points-map :web-post-position-top) 
                       (wall-brace-polyhedron-points-map :circular-point-1)
                       (wall-brace-polyhedron-points-map :circular-point-2)
                       (wall-brace-polyhedron-points-map :circular-point-3)
                       (wall-brace-polyhedron-points-map :circular-point-floor) 
                       steps)
         inner-points (bezier-cubic
                       (wall-brace-polyhedron-points-map :web-post-position-bottom)
                       (wall-brace-polyhedron-points-map :wall-locate-2-top)
                       (wall-brace-polyhedron-points-map :wall-locate-2-bottom)
                       (wall-brace-polyhedron-points-map :wall-locate-2-bottom-floor)
                       steps)]
     {:outer-points outer-points
      :inner-points inner-points})))

(defn wall-brace-polyhedron-circular-curves ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
                                    (wall-brace-polyhedron-circular-curves place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps]
   (let [wall-brace-polyhedron-circular-points1 (wall-brace-polyhedron-circular-points place1 dx1 dy1 post-position-1 rad-or-deg1 xy1)
         wall-brace-polyhedron-circular-points2 (wall-brace-polyhedron-circular-points place2 dx2 dy2 post-position-2 rad-or-deg2 xy2)

         web-post-top-curve (bezier-linear  (wall-brace-polyhedron-circular-points1 :web-post-position-top)  (wall-brace-polyhedron-circular-points2 :web-post-position-top) steps)
         circular-point-1-curve (bezier-linear  (wall-brace-polyhedron-circular-points1 :circular-point-1)  (wall-brace-polyhedron-circular-points2 :circular-point-1)  steps)
         circular-point-2-curve (bezier-linear  (wall-brace-polyhedron-circular-points1 :circular-point-2)  (wall-brace-polyhedron-circular-points2 :circular-point-2)  steps)
         circular-point-3-curve (bezier-linear  (wall-brace-polyhedron-circular-points1 :circular-point-3)  (wall-brace-polyhedron-circular-points2 :circular-point-3)  steps)
         circular-point-floor (bezier-linear  (wall-brace-polyhedron-circular-points1 :circular-point-floor)   (wall-brace-polyhedron-circular-points2 :circular-point-floor) steps) 
         wall-locate2-bottom-floor-curve (bezier-linear  (wall-brace-polyhedron-circular-points2 :wall-locate-2-bottom-floor)  (wall-brace-polyhedron-circular-points1 :wall-locate-2-bottom-floor) steps)
         wall-locate2-bottom-curve (bezier-linear  (wall-brace-polyhedron-circular-points2 :wall-locate-2-bottom)   (wall-brace-polyhedron-circular-points1 :wall-locate-2-bottom) steps)
         wall-locate2-top-curve (bezier-linear   (wall-brace-polyhedron-circular-points2 :wall-locate-2-top)  (wall-brace-polyhedron-circular-points1 :wall-locate-2-top)   steps)
         web-post-bottom-curve (bezier-linear   (wall-brace-polyhedron-circular-points2 :web-post-position-bottom)  (wall-brace-polyhedron-circular-points1 :web-post-position-bottom) steps)]

     {:web-post-top-curve web-post-top-curve
      :circular-point-1-curve circular-point-1-curve
      :circular-point-2-curve circular-point-2-curve
      :circular-point-3-curve circular-point-3-curve
      :circular-point-floor circular-point-floor
      :wall-locate2-bottom-floor-curve wall-locate2-bottom-floor-curve
      :wall-locate2-bottom-curve wall-locate2-bottom-curve
      :wall-locate2-top-curve wall-locate2-top-curve
      :web-post-bottom-curve web-post-bottom-curve})))

(defn wall-brace-polyhedron-circular-outer-floor-linear  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
                                                 (wall-brace-polyhedron-circular-outer-floor-linear place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps]
   (let [wall-brace-polyhedron-circular-outer-floor-point1 (wall-brace-polyhedron-circular-outer-floor-point place1 dx1 dy1 post-position-1 rad-or-deg1 xy1)
         wall-brace-polyhedron-circular-outer-floor-point2 (wall-brace-polyhedron-circular-outer-floor-point place2 dx2 dy2 post-position-2 rad-or-deg2 xy2)]
     (bezier-linear wall-brace-polyhedron-circular-outer-floor-point1 wall-brace-polyhedron-circular-outer-floor-point2 steps))))

(defn wall-brace-polyhedron-circular
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
   (wall-brace-polyhedron-circular place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps]
   (let [wall-brace-polyhedron-circular-map (wall-brace-polyhedron-circular-curves place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps)




         outer-points (into [] (apply concat
                                      (for [index (range 0 (inc steps))]
                                        (bezier-quartic
                                         (nth (wall-brace-polyhedron-circular-map :web-post-top-curve) index)
                                         (nth (wall-brace-polyhedron-circular-map :circular-point-1-curve) index)
                                         (nth (wall-brace-polyhedron-circular-map :circular-point-2-curve) index)
                                         (nth (wall-brace-polyhedron-circular-map :circular-point-3-curve) index)
                                         (nth (wall-brace-polyhedron-circular-map :circular-point-floor) index) 
                                         steps))))

         inner-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                               (bezier-cubic
                                                (nth (wall-brace-polyhedron-circular-map :web-post-bottom-curve) index)
                                                (nth (wall-brace-polyhedron-circular-map :wall-locate2-top-curve) index)
                                                (nth (wall-brace-polyhedron-circular-map :wall-locate2-bottom-curve) index)
                                                (nth (wall-brace-polyhedron-circular-map :wall-locate2-bottom-floor-curve) index)
                                                steps))))
         smoother-wall-polyhedron (polyhedron (concat outer-points inner-points)
                                              (generate-bezier-along-bezier-polyhedron-faces
                                               outer-points inner-points
                                               steps))]
         ;wall-polyhedron 
     smoother-wall-polyhedron)))

(defn wall-brace-quadratic-polyhedron-circular-curves ([place1 dx1 dy1  post-position-1 rad-or-deg1
                                        place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                        place2 dx2 dy2  post-position-2 rad-or-deg2 steps]
                                       (wall-brace-quadratic-polyhedron-circular-curves
                                        place1 dx1 dy1  post-position-1 rad-or-deg1
                                        place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                        place2 dx2 dy2  post-position-2 rad-or-deg2
                                        wall-xy-offset wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1  post-position-1 rad-or-deg1
    place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
    place2 dx2 dy2  post-position-2 rad-or-deg2
    xy1 xy-mid1 xy2 steps]
   (let [wall-brace-polyhedron-circular-curve-points1 (wall-brace-polyhedron-circular-points place1 dx1 dy1  post-position-1 rad-or-deg1 xy1)
         wall-brace-polyhedron-circular-curve-points-mid1 (wall-brace-polyhedron-circular-points place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1 xy-mid1 )
         wall-brace-polyhedron-circular-curve-points2 (wall-brace-polyhedron-circular-points place2 dx2 dy2  post-position-2 rad-or-deg2 xy2)
     
     web-post-top-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points1 :web-post-position-top)
                                              (wall-brace-polyhedron-circular-curve-points-mid1 :web-post-position-top)
                                              (wall-brace-polyhedron-circular-curve-points2 :web-post-position-top)
                                              steps)


circular-point-1-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points1 :circular-point-1)
                                              (wall-brace-polyhedron-circular-curve-points-mid1 :circular-point-1)
                                              (wall-brace-polyhedron-circular-curve-points2 :circular-point-1)
                                              steps)
circular-point-2-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points1 :circular-point-2)
                                    (wall-brace-polyhedron-circular-curve-points-mid1 :circular-point-2)
                                    (wall-brace-polyhedron-circular-curve-points2 :circular-point-2)
                                    steps)
circular-point-3-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points1 :circular-point-3)
                                    (wall-brace-polyhedron-circular-curve-points-mid1 :circular-point-3)
                                    (wall-brace-polyhedron-circular-curve-points2 :circular-point-3)
                                    steps)
circular-point-floor-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points1 :circular-point-floor)
                                        (wall-brace-polyhedron-circular-curve-points-mid1 :circular-point-floor)
                                        (wall-brace-polyhedron-circular-curve-points2 :circular-point-floor)
                                        steps)

wall-locate-2-bottom-floor-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points2 :wall-locate-2-bottom-floor)
                                              (wall-brace-polyhedron-circular-curve-points-mid1 :wall-locate-2-bottom-floor)
                                              (wall-brace-polyhedron-circular-curve-points1 :wall-locate-2-bottom-floor)
                                              steps)
     wall-locate-2-bottom-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points2 :wall-locate-2-bottom)
                                             (wall-brace-polyhedron-circular-curve-points-mid1 :wall-locate-2-bottom)
                                             (wall-brace-polyhedron-circular-curve-points1 :wall-locate-2-bottom)
                                             steps)
wall-locate-2-top-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points2 :wall-locate-2-top)
                                     (wall-brace-polyhedron-circular-curve-points-mid1 :wall-locate-2-top)
                                     (wall-brace-polyhedron-circular-curve-points1 :wall-locate-2-top)
                                     steps)

web-post-position-bottom-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points2 :web-post-position-bottom)
                                            (wall-brace-polyhedron-circular-curve-points-mid1 :web-post-position-bottom)
                                            (wall-brace-polyhedron-circular-curve-points1 :web-post-position-bottom)
                                            steps)
]
     {:web-post-top-curve web-post-top-curve
      :circular-point-1-curve circular-point-1-curve
      :circular-point-2-curve circular-point-2-curve
      :circular-point-3-curve circular-point-3-curve
      :circular-point-floor-curve circular-point-floor-curve
      :wall-locate-2-bottom-floor-curve wall-locate-2-bottom-floor-curve
      :wall-locate-2-bottom-curve wall-locate-2-bottom-curve
      :wall-locate-2-top-curve wall-locate-2-top-curve
      :web-post-position-bottom-curve web-post-position-bottom-curve}
     )
   ))
(defn wall-brace-quadratic-polyhedron-circular-floor-outer ([place1 dx1 dy1  post-position-1 rad-or-deg1
                                                             place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                                             place2 dx2 dy2  post-position-2 rad-or-deg2 steps]
                                                            (wall-brace-quadratic-polyhedron-circular-floor-outer
                                                             place1 dx1 dy1  post-position-1 rad-or-deg1
                                                             place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                                             place2 dx2 dy2  post-position-2 rad-or-deg2
                                                             wall-xy-offset wall-xy-offset wall-xy-offset steps) 
                                                            )
  ([place1 dx1 dy1  post-position-1 rad-or-deg1
   place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
   place2 dx2 dy2  post-position-2 rad-or-deg2
   xy1 xy-mid1 xy2 steps]
   (let [wall-brace-polyhedron-circular-curve-points1 (wall-brace-polyhedron-circular-points place1 dx1 dy1  post-position-1 rad-or-deg1 xy1)
         wall-brace-polyhedron-circular-curve-points-mid1 (wall-brace-polyhedron-circular-points place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1 xy-mid1)
         wall-brace-polyhedron-circular-curve-points2 (wall-brace-polyhedron-circular-points place2 dx2 dy2  post-position-2 rad-or-deg2 xy2)]
     (bezier-quadratic (wall-brace-polyhedron-circular-curve-points1 :circular-point-floor)
                      ( wall-brace-polyhedron-circular-curve-points-mid1 :circular-point-floor) 
                       (wall-brace-polyhedron-circular-curve-points2 :circular-point-floor) steps)
     
     
     )
   )
)

(defn wall-brace-quadratic-polyhedron-circular ([place1 dx1 dy1  post-position-1 rad-or-deg1
                                                 place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                                 place2 dx2 dy2  post-position-2 rad-or-deg2 steps]
                                                (wall-brace-quadratic-polyhedron-circular
                                                 place1 dx1 dy1  post-position-1 rad-or-deg1
                                                 place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                                 place2 dx2 dy2  post-position-2 rad-or-deg2
                                                 wall-xy-offset wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1  post-position-1 rad-or-deg1
    place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
    place2 dx2 dy2  post-position-2 rad-or-deg2
    xy1 xy-mid1 xy2 steps]
   (let [wall-brace-quadratic-polyhedron-circular-curves-map
         (wall-brace-quadratic-polyhedron-circular-curves place1 dx1 dy1  post-position-1 rad-or-deg1
    place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
    place2 dx2 dy2  post-position-2 rad-or-deg2
    xy1 xy-mid1 xy2 steps)
         
         outer-points (into [] (apply concat
                                      (for [index (range 0 (inc steps))]
                                        (bezier-quartic
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :web-post-top-curve ) index)
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :circular-point-1-curve) index)
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :circular-point-2-curve) index)
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :circular-point-3-curve) index)
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :circular-point-floor-curve) index)
                                     steps
                                        ))))
         inner-points (into [] (apply concat 
                                      (for [index (range 0 (inc steps))]
                                        (bezier-cubic
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :web-post-position-bottom-curve) index)
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :wall-locate-2-top-curve) index)
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :wall-locate-2-bottom-curve) index)
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :wall-locate-2-bottom-floor-curve) index) 
                                         steps
                                         ) 
                                        )))
         smoother-wall-polyhedron (polyhedron (concat outer-points inner-points)
                                              (generate-bezier-along-bezier-polyhedron-faces
                                               outer-points inner-points
                                               steps))
   ]
   smoother-wall-polyhedron
   )
                            
  ))

(defn wall-brace-catmull-rom-spline-horizontal-curves
  [outer-control-data-1 point-data-1 point-data-2 outer-control-data-2 steps & {:keys [alpha-type t1 t2 web-post-top-style] :or {alpha-type :centripetal t1 (/ 1 3) t2 (/ 2 3) web-post-top-style :linear}}]
  
  (let [data-to-wall-brace-polyhedron-points #(wall-brace-polyhedron-points (% :place) (% :dx) (% :dy) (% :post-position) (% :rad-or-deg) (% :xy))
        outer-control-points-1 (data-to-wall-brace-polyhedron-points outer-control-data-1)
        points-1 (data-to-wall-brace-polyhedron-points point-data-1)
        points-2 (data-to-wall-brace-polyhedron-points point-data-2)
        outer-control-points-2 (data-to-wall-brace-polyhedron-points outer-control-data-2)
        web-post-top-curve (case web-post-top-style 
                             :linear (bezier-linear (points-1 :web-post-position-top) (points-2 :web-post-position-top) steps)
                             :curved (catmull-rom-spline-curve (outer-control-points-1 :web-post-position-top) (points-1 :web-post-position-top) (points-2 :web-post-position-top) (outer-control-points-2 :web-post-position-top) steps :alphaType alpha-type :t1 t1 :t2 t2)) 
        wall-locate1-curve (catmull-rom-spline-curve (outer-control-points-1 :wall-locate1-point) (points-1 :wall-locate1-point) (points-2 :wall-locate1-point) (outer-control-points-2 :wall-locate1-point) steps :alphaType alpha-type :t1 t1 :t2 t2)
        wall-locate-1-to-3-curve-for-polyhedron-control-curve (catmull-rom-spline-curve (outer-control-points-1 :wall-locate-1-to-3-curve-for-polyhedron-control-point) (points-1 :wall-locate-1-to-3-curve-for-polyhedron-control-point) (points-2 :wall-locate-1-to-3-curve-for-polyhedron-control-point) (outer-control-points-2 :wall-locate-1-to-3-curve-for-polyhedron-control-point) steps :alphaType alpha-type :t1 t1 :t2 t2)
        wall-locate-1-to-3-curve-for-polyhedron-second-control-curve (catmull-rom-spline-curve (outer-control-points-1 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (points-1 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (points-2 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (outer-control-points-2 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) steps :alphaType alpha-type :t1 t1 :t2 t2)
        wall-locate3-curve  (catmull-rom-spline-curve (outer-control-points-1 :wall-locate3-point) (points-1 :wall-locate3-point) (points-2 :wall-locate3-point) (outer-control-points-2 :wall-locate3-point) steps :alphaType alpha-type :t1 t1 :t2 t2)
        wall-locate3-floor-curve (catmull-rom-spline-curve (outer-control-points-1 :wall-locate3-point-floor) (points-1 :wall-locate3-point-floor) (points-2 :wall-locate3-point-floor) (outer-control-points-2 :wall-locate3-point-floor) steps :alphaType alpha-type :t1 t1 :t2 t2) 
        wall-locate2-bottom-curve (catmull-rom-spline-curve (outer-control-points-1 :wall-locate-2-bottom) (points-1 :wall-locate-2-bottom) (points-2 :wall-locate-2-bottom) (outer-control-points-2 :wall-locate-2-bottom) steps :alphaType alpha-type :t1 t1 :t2 t2)
        wall-locate2-bottom-floor-curve ;(map translate-to-floor wall-locate2-bottom-curve) 
        (catmull-rom-spline-curve (outer-control-points-1 :wall-locate-2-bottom-floor) (points-1 :wall-locate-2-bottom-floor) (points-2 :wall-locate-2-bottom-floor) (outer-control-points-2 :wall-locate-2-bottom-floor) steps :alphaType alpha-type :t1 t1 :t2 t2)
        wall-locate2-top-curve (catmull-rom-spline-curve (outer-control-points-1 :wall-locate-2-top) (points-1 :wall-locate-2-top) (points-2 :wall-locate-2-top) (outer-control-points-2 :wall-locate-2-top) steps :alphaType alpha-type :t1 t1 :t2 t2)
        web-post-bottom-curve (bezier-linear  (points-2 :web-post-position-bottom) (points-1 :web-post-position-bottom) steps)

        ]
        {:web-post-top-curve web-post-top-curve
         :wall-locate1-curve wall-locate1-curve
         :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve wall-locate-1-to-3-curve-for-polyhedron-second-control-curve
         :wall-locate-1-to-3-curve-for-polyhedron-control-curve wall-locate-1-to-3-curve-for-polyhedron-control-curve
         :wall-locate3-curve wall-locate3-curve
         :wall-locate3-floor-curve wall-locate3-floor-curve
         :wall-locate2-bottom-floor-curve (reverse wall-locate2-bottom-floor-curve)
         :wall-locate2-bottom-curve (reverse wall-locate2-bottom-curve)
         :wall-locate2-top-curve (reverse wall-locate2-top-curve)
         :web-post-bottom-curve web-post-bottom-curve}
    )
  )


(defn wall-brace-catmull-rom-spline-points
  [outer-control-data-1 point-data-1 point-data-2 outer-control-data-2 steps & {:keys [alpha-type t1 t2 web-post-top-style] :or {alpha-type :centripetal t1 (/ 1 3) t2 (/ 2 3) web-post-top-style :linear}}]

  (let [wall-brace-catmull-rom-spline-horizontal-curves-map (wall-brace-catmull-rom-spline-horizontal-curves outer-control-data-1 point-data-1 point-data-2 outer-control-data-2 steps :alpha-type alpha-type :t1 t1 :t2 t2 :web-post-top-style web-post-top-style)
        outer-points (into [] (apply concat
                                     (for [index (range 0 (inc steps))]
                                       (bezier-quintic
                                        (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :web-post-top-curve) index)
                                        (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :wall-locate1-curve) index)
                                        (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :wall-locate-1-to-3-curve-for-polyhedron-control-curve) index)
                                        (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve) index)
                                        (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :wall-locate3-curve) index)
                                        (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :wall-locate3-floor-curve) index)
                                        steps))))

        inner-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                              (bezier-cubic
                                               (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :web-post-bottom-curve) index)
                                               (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :wall-locate2-top-curve) index)
                                               (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :wall-locate2-bottom-curve) index)
                                               (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :wall-locate2-bottom-floor-curve) index)
                                               steps))))]
    {:outer-points outer-points
     :inner-points inner-points}                                           
    )
  )
(defn wall-brace-catmull-rom-spline 
  [outer-control-data-1 point-data-1 point-data-2 outer-control-data-2 steps & {:keys [alpha-type t1 t2 web-post-top-style] :or {alpha-type :centripetal t1 (/ 1 3) t2 (/ 2 3) web-post-top-style :linear} }]
  (let [points (wall-brace-catmull-rom-spline-points outer-control-data-1 point-data-1 point-data-2 outer-control-data-2 steps :alpha-type alpha-type :t1 t1 :t2 t2 :web-post-top-style web-post-top-style)]
    (polyhedron (concat (points :outer-points) (points :inner-points))
              (generate-bezier-along-bezier-polyhedron-faces
               (points :outer-points) (points :inner-points)
               steps))) 
  )

(defn wall-brace-catmull-rom-spline-floor 
  [outer-control-data-1 point-data-1 point-data-2 outer-control-data-2 steps & {:keys [alpha-type t1 t2 web-post-top-style] :or {alpha-type :centripetal t1 (/ 1 3) t2 (/ 2 3) web-post-top-style :linear}}]
  (let [points (wall-brace-catmull-rom-spline-horizontal-curves outer-control-data-1 point-data-1 point-data-2 outer-control-data-2 steps :alpha-type alpha-type :t1 t1 :t2 t2 :web-post-top-style web-post-top-style) 
        ]
    (points :wall-locate3-floor-curve)
    )
  )

(defn wall-brace-bezier-cubic-through-points-horizontal-curves [point-1-data point-2-data point-3-data point-4-data steps & {:keys [  t1 t2] :or { t1 (/ 1 3) t2 (/ 2 3)}}]
  (let [data-to-wall-brace-polyhedron-points #(wall-brace-polyhedron-points (% :place) (% :dx) (% :dy) (% :post-position) (% :rad-or-deg) (% :xy))
        points-1 (data-to-wall-brace-polyhedron-points point-1-data)
        points-2 (data-to-wall-brace-polyhedron-points point-2-data)
        points-3 (data-to-wall-brace-polyhedron-points point-3-data)
        points-4 (data-to-wall-brace-polyhedron-points point-4-data)
        web-post-top-curve (bezier-cubic (points-1 :web-post-position-top) (points-2 :web-post-position-top) (points-3 :web-post-position-top) (points-4 :web-post-position-top) steps )
        ;;(case web-post-top-style
        ;;                      :linear (bezier-linear (points-1 :web-post-position-top) (points-2 :web-post-position-top) steps)
        ;;                      :curved )
        wall-locate1-curve (bezier-cubic-through-points (points-1 :wall-locate1-point) (points-2 :wall-locate1-point) (points-3 :wall-locate1-point) (points-4 :wall-locate1-point) steps  :t1 t1 :t2 t2)
        wall-locate-1-to-3-curve-for-polyhedron-control-curve (bezier-cubic-through-points (points-1 :wall-locate-1-to-3-curve-for-polyhedron-control-point) (points-2 :wall-locate-1-to-3-curve-for-polyhedron-control-point) (points-3 :wall-locate-1-to-3-curve-for-polyhedron-control-point) (points-4 :wall-locate-1-to-3-curve-for-polyhedron-control-point) steps  :t1 t1 :t2 t2)
        wall-locate-1-to-3-curve-for-polyhedron-second-control-curve (bezier-cubic-through-points (points-1 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (points-2 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (points-3 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (points-4 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) steps  :t1 t1 :t2 t2)
        wall-locate3-curve  (bezier-cubic-through-points (points-1 :wall-locate3-point) (points-2 :wall-locate3-point) (points-3 :wall-locate3-point) (points-4 :wall-locate3-point) steps  :t1 t1 :t2 t2)
        wall-locate3-floor-curve (bezier-cubic-through-points (points-1 :wall-locate3-point-floor) (points-2 :wall-locate3-point-floor) (points-3 :wall-locate3-point-floor) (points-4 :wall-locate3-point-floor) steps  :t1 t1 :t2 t2)
        wall-locate2-bottom-curve (bezier-cubic-through-points (points-1 :wall-locate-2-bottom) (points-2 :wall-locate-2-bottom) (points-3 :wall-locate-2-bottom) (points-4 :wall-locate-2-bottom) steps  :t1 t1 :t2 t2)
        wall-locate2-bottom-floor-curve ;(map translate-to-floor wall-locate2-bottom-curve) 
        (bezier-cubic-through-points (points-1 :wall-locate-2-bottom-floor) (points-2 :wall-locate-2-bottom-floor) (points-3 :wall-locate-2-bottom-floor) (points-4 :wall-locate-2-bottom-floor) steps  :t1 t1 :t2 t2)
        wall-locate2-top-curve (bezier-cubic-through-points (points-1 :wall-locate-2-top) (points-2 :wall-locate-2-top) (points-3 :wall-locate-2-top) (points-4 :wall-locate-2-top) steps  :t1 t1 :t2 t2)
        web-post-bottom-curve (bezier-cubic   (points-1 :web-post-position-bottom) (points-2 :web-post-position-bottom) (points-3 :web-post-position-bottom)  (points-4 :web-post-position-bottom) steps)]
    {:web-post-top-curve web-post-top-curve
     :wall-locate1-curve wall-locate1-curve
     :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve wall-locate-1-to-3-curve-for-polyhedron-second-control-curve
     :wall-locate-1-to-3-curve-for-polyhedron-control-curve wall-locate-1-to-3-curve-for-polyhedron-control-curve
     :wall-locate3-curve wall-locate3-curve
     :wall-locate3-floor-curve wall-locate3-floor-curve
     :wall-locate2-bottom-floor-curve (reverse wall-locate2-bottom-floor-curve)
     :wall-locate2-bottom-curve (reverse wall-locate2-bottom-curve)
     :wall-locate2-top-curve (reverse wall-locate2-top-curve)
     :web-post-bottom-curve (reverse web-post-bottom-curve)}))

(defn wall-brace-bezier-cubic-through-points-points
  [point-1-data point-2-data point-3-data point-4-data steps & {:keys [ t1 t2] :or {t1 (/ 1 3) t2 (/ 2 3)}}]

  (let [wall-brace-bezier-cubic-through-points-horizontal-curves-map (wall-brace-bezier-cubic-through-points-horizontal-curves point-1-data point-2-data point-3-data point-4-data steps :t1 t1 :t2 t2)
        outer-points (into [] (apply concat
                                     (for [index (range 0 (inc steps))]
                                       (bezier-quintic
                                        (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :web-post-top-curve) index)
                                        (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :wall-locate1-curve) index)
                                        (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :wall-locate-1-to-3-curve-for-polyhedron-control-curve) index)
                                        (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve) index)
                                        (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :wall-locate3-curve) index)
                                        (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :wall-locate3-floor-curve) index)
                                        steps))))

        inner-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                              (bezier-cubic
                                               (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :web-post-bottom-curve) index)
                                               (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :wall-locate2-top-curve) index)
                                               (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :wall-locate2-bottom-curve) index)
                                               (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :wall-locate2-bottom-floor-curve) index)
                                               steps))))]
    {:outer-points outer-points
     :inner-points inner-points}))

(defn wall-brace-bezier-cubic-through-points-polyhedron
  [point-1-data point-2-data point-3-data point-4-data steps & {:keys [ t1 t2] :or { t1 (/ 1 3) t2 (/ 2 3)}}]
  (let [points (wall-brace-bezier-cubic-through-points-points point-1-data point-2-data point-3-data point-4-data steps  :t1 t1 :t2 t2)]
    (polyhedron (concat (points :outer-points) (points :inner-points))
                (generate-bezier-along-bezier-polyhedron-faces
                 (points :outer-points) (points :inner-points)
                 steps))))

(defn wall-brace-bezier-cubic-through-points-floor
  [point-1-data point-2-data point-3-data point-4-data steps & {:keys [t1 t2] :or {t1 (/ 1 3) t2 (/ 2 3)}}]
  (let [points (wall-brace-bezier-cubic-through-points-horizontal-curves point-1-data point-2-data point-3-data point-4-data steps :t1 t1 :t2 t2)]
    (points :wall-locate3-floor-curve)))

(defn wall-brace-with-circular ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
   (wall-brace-with-circular place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset :circular :circular steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 curve-type1 curve-type2 steps]
   (let [curve1-fn (case curve-type1
                     :circular wall-brace-polyhedron-circular-curve-points
                     :standard wall-brace-polyhedron-curve-points)
         curve2-fn (case curve-type2
                     :circular wall-brace-polyhedron-circular-curve-points
                     :standard wall-brace-polyhedron-curve-points)
         curve-points1 (curve1-fn place1 dx1 dy1 post-position-1 rad-or-deg1 xy1 steps)
         curve-points2 (curve2-fn place2 dx2 dy2 post-position-2 rad-or-deg2 xy2 steps)]
     (generate-bezier-along-bezier-polyhedron-from-points-list-linear
      (curve-points2 :outer-points) (curve-points1 :outer-points)
     (reverse (curve-points2 :inner-points)) (reverse (curve-points1 :inner-points))
     steps
      )
     ) 
   )
  )

(defn wall-brace-with-circular-outer-floor-linear ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
                                (wall-brace-with-circular-outer-floor-linear place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset :circular :circular steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 curve-type1 curve-type2 steps]
   (let [curve1-fn (case curve-type1
                     :circular wall-brace-polyhedron-circular-points
                     :standard wall-brace-polyhedron-points)
         curve2-fn (case curve-type2
                     :circular wall-brace-polyhedron-circular-points
                     :standard wall-brace-polyhedron-points)
         floor-point-key1 (case curve-type1
                           :circular :circular-point-floor
                            :standard :wall-locate3-point-floor)
         floor-point-key2 (case curve-type2
                            :circular :circular-point-floor
                            :standard :wall-locate3-point-floor)                   
         curve-points1 (curve1-fn place1 dx1 dy1 post-position-1 rad-or-deg1 xy1 )
         curve-points2 (curve2-fn place2 dx2 dy2 post-position-2 rad-or-deg2 xy2 )] 
     (bezier-linear (curve-points1 floor-point-key1) (curve-points2 floor-point-key2) steps))))

(defn key-wall-brace-polyhedron [x1 y1 dx1 dy1 post-position1 x2 y2 dx2 dy2 post-position2 & 
                                 {:keys [steps xy1 xy2] :or {steps 36 xy1 wall-xy-offset xy2 wall-xy-offset}}] 

   (wall-brace-polyhedron (partial key-place x1 y1) dx1 dy1 post-position1 :radians
                         (partial key-place x2 y2) dx2 dy2 post-position2 :radians
                          xy1 xy2 steps))

(defn key-wall-brace-polyhedron-cicular [x1 y1 dx1 dy1 post-position1 x2 y2 dx2 dy2 post-position2 &
                                 {:keys [steps xy1 xy2] :or {steps 36 xy1 wall-xy-offset xy2 wall-xy-offset}}]

  (wall-brace-polyhedron-circular (partial key-place x1 y1) dx1 dy1 post-position1 :radians
                         (partial key-place x2 y2) dx2 dy2 post-position2 :radians
                         xy1 xy2 steps))

(defn key-wall-brace-polyhedron-with-circular [x1 y1 dx1 dy1 post-position1 x2 y2 dx2 dy2 post-position2 &
                                 {:keys [steps xy1 xy2 curve-type1 curve-type2] :or {steps 36 xy1 wall-xy-offset xy2 wall-xy-offset curve-type1 :circular curve-type2 :circular}}]

  (wall-brace-with-circular (partial key-place x1 y1) dx1 dy1 post-position1 :radians
                         (partial key-place x2 y2) dx2 dy2 post-position2 :radians
                         xy1 xy2 curve-type1 curve-type2 steps))

(defn key-wall-brace-polyhedron-outer-floor-linear [x1 y1 dx1 dy1 post-position1 x2 y2 dx2 dy2 post-position2 &
                                 {:keys [steps xy1 xy2] :or {steps 36 xy1 wall-xy-offset xy2 wall-xy-offset}}]

  (wall-brace-polyhedron-outer-floor-linear (partial key-place x1 y1) dx1 dy1 post-position1 :radians
                         (partial key-place x2 y2) dx2 dy2 post-position2 :radians
                         xy1 xy2 steps))

(defn key-wall-brace-polyhedron-with-circular-outer-floor-linear [x1 y1 dx1 dy1 post-position1 x2 y2 dx2 dy2 post-position2 &
                                               {:keys [steps xy1 xy2 curve-type1 curve-type2] :or {steps 36 xy1 wall-xy-offset xy2 wall-xy-offset curve-type1 :circular curve-type2 :circular}}]

  (wall-brace-with-circular-outer-floor-linear (partial key-place x1 y1) dx1 dy1 post-position1 :radians
                            (partial key-place x2 y2) dx2 dy2 post-position2 :radians
                            xy1 xy2 curve-type1 curve-type2 steps))

(defn key-wall-brace-polyhedron-circular-outer-floor-linear [x1 y1 dx1 dy1 post-position1 x2 y2 dx2 dy2 post-position2 &
                                                    {:keys [steps xy1 xy2] :or {steps 36 xy1 wall-xy-offset xy2 wall-xy-offset}}]

  (wall-brace-polyhedron-circular-outer-floor-linear (partial key-place x1 y1) dx1 dy1 post-position1 :radians
                                            (partial key-place x2 y2) dx2 dy2 post-position2 :radians
                                            xy1 xy2 steps))


(defn thumb-wall-brace-polyhedron 
  [place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2 & {:keys [xy1 xy2 steps] :or {xy1 wall-xy-offset xy2 wall-xy-offset steps 36}}] 
  
   (wall-brace-polyhedron place1 dx1 dy1 post-position1 :degrees
                          place2 dx2 dy2 post-position2 :degrees
                          xy1 xy2
                          steps))

(defn thumb-wall-brace-polyhedron-circular
  [place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2 & {:keys [xy1 xy2 steps] :or {xy1 wall-xy-offset xy2 wall-xy-offset steps 36}}]

  (wall-brace-polyhedron-circular place1 dx1 dy1 post-position1 :degrees
                         place2 dx2 dy2 post-position2 :degrees
                         xy1 xy2
                         steps))

(defn thumb-wall-brace-polyhedron-outer-floor-linear
  [place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2 & {:keys [xy1 xy2 steps] :or {xy1 wall-xy-offset xy2 wall-xy-offset steps 36}}]

  (wall-brace-polyhedron-outer-floor-linear place1 dx1 dy1 post-position1 :degrees
                         place2 dx2 dy2 post-position2 :degrees
                         xy1 xy2
                         steps))

(defn thumb-wall-brace-polyhedron-circular-outer-floor-linear
  [place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2 & {:keys [xy1 xy2 steps] :or {xy1 wall-xy-offset xy2 wall-xy-offset steps 36}}]

  (wall-brace-polyhedron-circular-outer-floor-linear place1 dx1 dy1 post-position1 :degrees
                                            place2 dx2 dy2 post-position2 :degrees
                                            xy1 xy2
                                            steps))

(defn thumb-wall-brace-polyhedron-with-circular
  [place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2 & 
   {:keys [steps xy1 xy2 curve-type1 curve-type2] :or {steps 36 xy1 wall-xy-offset xy2 wall-xy-offset curve-type1 :circular curve-type2 :circular}}]

  (wall-brace-with-circular place1 dx1 dy1 post-position1 :degrees
                         place2 dx2 dy2 post-position2 :degrees
                         xy1 xy2
                            curve-type1 curve-type2
                         steps))

(defn thumb-wall-brace-polyhedron-with-circular-outer-floor-linear
  [place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2 &
   {:keys [steps xy1 xy2 curve-type1 curve-type2] :or {steps 36 xy1 wall-xy-offset xy2 wall-xy-offset curve-type1 :circular curve-type2 :circular}}]

  (wall-brace-with-circular-outer-floor-linear place1 dx1 dy1 post-position1 :degrees
                            place2 dx2 dy2 post-position2 :degrees
                            xy1 xy2
                            curve-type1 curve-type2
                            steps))

(defn web-post-point-top [place post-position rad-or-deg]
  (let [transform (get-transform-fn rad-or-deg place)
        web-corner-translation-vector (get-single-plate-corner-position-vector post-position)
web-post-point-top-coordinates (transform (web-post-position-top web-corner-translation-vector))
        ] 
    web-post-point-top-coordinates
    )
  )

(defn web-post-point-bottom [place post-position rad-or-deg]
  (let [transform (get-transform-fn rad-or-deg place)
        web-corner-translation-vector (get-single-plate-corner-position-vector post-position) 
        web-post-position-1-bottom (transform (web-post-position-bottom (mapv +  web-corner-translation-vector)))]
    web-post-position-1-bottom
    )
  )

(defn web-post-point [place post-position rad-or-deg] 
    {:top (web-post-point-top place post-position rad-or-deg) 
     :bottom (web-post-point-bottom place post-position rad-or-deg)}
    
  )

(defn web-post-linear [place1 post-position-1 rad-or-deg1
                       place2 post-position-2 rad-or-deg2 steps
                       & {:keys [offset1 offset2] :or {offset1 [0 0 0] offset2 [0 0 0]}}]
  (let [transform-1 (get-transform-fn rad-or-deg1 place1)
        transform-2 (get-transform-fn rad-or-deg2 place2)
        web-corner-translation-vector1 (get-single-plate-corner-position-vector post-position-1)
        web-corner-translation-vector2 (get-single-plate-corner-position-vector post-position-2)
        web-post-position-1-top (transform-1 (web-post-position-top (mapv +  web-corner-translation-vector1 offset1)))
        web-post-position-1-bottom (transform-1 (web-post-position-bottom (mapv +  web-corner-translation-vector1 offset1)))


        web-post-position-2-top (transform-2 (web-post-position-top (mapv +  web-corner-translation-vector2 offset2)))
        web-post-position-2-bottom (transform-2 (web-post-position-bottom (mapv +  web-corner-translation-vector2 offset2)))
        web-post-top-curve (bezier-linear  web-post-position-1-top  web-post-position-2-top  steps)
        web-post-bottom-curve (bezier-linear   web-post-position-2-bottom  web-post-position-1-bottom steps)
        ;; web-post-curve (into [] (apply concat
        ;;                                (for [index (range 0 (inc steps))]
        ;;                                  (bezier-linear
        ;;                                   (nth web-post-top-curve index)
        ;;                                   (nth web-post-bottom-curve index)
        ;;                                   steps))))
        web-post-curve {:top web-post-top-curve :bottom web-post-bottom-curve}]
    web-post-curve))

(defn web-post-quadratic-curve [place1 post-position-1 rad-or-deg1
                                place-mid1 post-position-mid1 rad-or-degmid1
                                place2 post-position-2 rad-or-deg2 steps
                                & {:keys [offset1 offset-mid1 offset2] :or {offset1 [0 0 0] offset-mid1 [0 0 0] offset2 [0 0 0]}}]
  (let [transform-1 (get-transform-fn rad-or-deg1 place1)
        transform-mid1 (get-transform-fn rad-or-degmid1 place-mid1)
        transform-2 (get-transform-fn rad-or-deg2 place2)
        web-corner-translation-vector1 (get-single-plate-corner-position-vector post-position-1)
        web-corner-translation-vectormid1 (get-single-plate-corner-position-vector post-position-mid1)
        web-corner-translation-vector2 (get-single-plate-corner-position-vector post-position-2)
        web-post-position-1-top (transform-1 (web-post-position-top (mapv +  web-corner-translation-vector1 offset1)))
        web-post-position-1-bottom (transform-1 (web-post-position-bottom (mapv +  web-corner-translation-vector1 offset1)))

        web-post-position-mid1-top (transform-mid1 (web-post-position-top (mapv +  web-corner-translation-vectormid1 offset-mid1)))
        web-post-position-mid1-bottom (transform-mid1 (web-post-position-bottom (mapv +  web-corner-translation-vectormid1 offset-mid1)))

        web-post-position-2-top (transform-2 (web-post-position-top (mapv +  web-corner-translation-vector2 offset2)))
        web-post-position-2-bottom (transform-2 (web-post-position-bottom (mapv +  web-corner-translation-vector2 offset2)))
        web-post-top-curve (bezier-quadratic  web-post-position-1-top web-post-position-mid1-top web-post-position-2-top  steps)
        web-post-bottom-curve (bezier-quadratic   web-post-position-2-bottom web-post-position-mid1-bottom web-post-position-1-bottom steps)
        ;; web-post-curve (into [](apply concat
        ;;                         (for [index (range 0 (inc steps))]
        ;;                           (bezier-linear
        ;;                            (nth web-post-top-curve index)
        ;;                            (nth web-post-bottom-curve index)
        ;;                            steps
        ;;                            )

        ;;                           )
        ;;                         )) 
        web-post-curve {:top web-post-top-curve :bottom web-post-bottom-curve}]
    web-post-curve))
(defn wall-brace-quadratic-polyhedron-bottom ([place1 dx1 dy1  post-position-1 rad-or-deg1
                                        place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                        place2 dx2 dy2  post-position-2 rad-or-deg2 steps]
                                       (wall-brace-quadratic-polyhedron-bottom
                                        place1 dx1 dy1  post-position-1 rad-or-deg1
                                        place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                        place2 dx2 dy2  post-position-2 rad-or-deg2
                                        wall-xy-offset wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1  post-position-1 rad-or-deg1
    place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
    place2 dx2 dy2  post-position-2 rad-or-deg2 xy1 xy-mid1 xy2 steps]
   (let[ point1 (wall-brace-polyhedron-outer-floor-point place1 dx1 dy1  post-position-1 rad-or-deg1 xy1)
        point-mid1 (wall-brace-polyhedron-outer-floor-point place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1 xy-mid1)
        point2 (wall-brace-polyhedron-outer-floor-point  place2 dx2 dy2  post-position-2 rad-or-deg2 xy2)
        ]
    (bezier-quadratic point1 point-mid1 point2 steps)
    )
   
   )
  )
(defn wall-brace-quadratic-polyhedron ([place1 dx1 dy1  post-position-1 rad-or-deg1
                                        place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                        place2 dx2 dy2  post-position-2 rad-or-deg2 steps]
                                       (wall-brace-quadratic-polyhedron
                                        place1 dx1 dy1  post-position-1 rad-or-deg1
                                        place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                        place2 dx2 dy2  post-position-2 rad-or-deg2
                                        wall-xy-offset wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1  post-position-1 rad-or-deg1
    place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
    place2 dx2 dy2  post-position-2 rad-or-deg2 xy1 xy-mid1 xy2 steps]
   (let [transform-1 (get-transform-fn rad-or-deg1 place1)
         transform-mid1 (get-transform-fn rad-or-degmid1 place-mid1)
         transform-2 (get-transform-fn rad-or-deg2 place2)

         web-corner-translation-vector1 (get-single-plate-corner-position-vector post-position-1)
         web-corner-translation-vectormid1 (get-single-plate-corner-position-vector post-position-mid1)
         web-corner-translation-vector2 (get-single-plate-corner-position-vector post-position-2)
         web-post-position-1-top (transform-1 (web-post-position-top (mapv +  web-corner-translation-vector1)))
         web-post-position-1-bottom (transform-1 (web-post-position-bottom (mapv +  web-corner-translation-vector1)))

         web-post-position-mid1-top (transform-mid1 (web-post-position-top (mapv +  web-corner-translation-vectormid1)))
         web-post-position-mid1-bottom (transform-mid1 (web-post-position-bottom (mapv +  web-corner-translation-vectormid1)))

         web-post-position-2-top (transform-2 (web-post-position-top (mapv +  web-corner-translation-vector2)))
         web-post-position-2-bottom (transform-2 (web-post-position-bottom (mapv +  web-corner-translation-vector2)))

         oled-corner-translation-vector1 (get-oled-corner-translation-vector post-position-1)
         oled-corner-translation-vectormid1 (get-oled-corner-translation-vector post-position-mid1)
         oled-corner-translation-vector2  (get-oled-corner-translation-vector post-position-2)

         oled-post-position-1-top (oled-post-position-top oled-corner-translation-vector1)
         oled-post-position-1-bottom (oled-post-position-bottom oled-corner-translation-vector1)

         oled-post-position-mid1-top (oled-post-position-top oled-corner-translation-vectormid1)
         oled-post-position-mid1-bottom (oled-post-position-bottom oled-corner-translation-vectormid1)

         oled-post-position-2-top (oled-post-position-top oled-corner-translation-vector2)
         oled-post-position-2-bottom  (oled-post-position-bottom oled-corner-translation-vector2)

         curve-corner-translation-vector1 (get-curve-corner-translation-vector post-position-1)
         curve-corner-translation-vectormid1 (get-curve-corner-translation-vector post-position-mid1)
         curve-corner-translation-vector2 (get-curve-corner-translation-vector post-position-2)

         curve-post-position-1-top (mapv + (curve-post-position-top curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))
         curve-post-position-1-middle (mapv + (curve-post-position-middle curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))
         curve-post-position-1-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))

         curve-post-position-mid1-top (mapv + (curve-post-position-top curve-corner-translation-vectormid1) (get-curve-post-outer-x-and-y-vector dxmid1 dymid1))
         curve-post-position-mid1-middle (mapv + (curve-post-position-middle curve-corner-translation-vectormid1) (get-curve-post-outer-x-and-y-vector dxmid1 dymid1))
         curve-post-position-mid1-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vectormid1) (get-curve-post-outer-x-and-y-vector dxmid1 dymid1))

         curve-post-position-2-top (mapv + (curve-post-position-top curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))
         curve-post-position-2-middle (mapv + (curve-post-position-middle curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))
         curve-post-position-2-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))

         wall-locate1-point1 (transform-1 (mapv + (wall-locate1 dx1 dy1) curve-post-position-1-top))
         wall-locate1-pointmid1 (transform-mid1 (mapv + (wall-locate1 dxmid1 dymid1) curve-post-position-mid1-top))
         wall-locate1-point2 (transform-2 (mapv + (wall-locate1 dx2 dy2) curve-post-position-2-top))

         wall-locate-1-to-3-curve-for-polyhedron-control-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx1 dy1) curve-post-position-1-middle (get-oled-post-outer-x-and-y-vector dx1 dy1))))
         wall-locate-1-to-3-curve-for-polyhedron-control-pointmid1 (make-point-z-value-not-below-zero (transform-mid1 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dxmid1 dymid1) curve-post-position-mid1-middle (get-oled-post-outer-x-and-y-vector dxmid1 dymid1))))
         wall-locate-1-to-3-curve-for-polyhedron-control-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx2 dy2) curve-post-position-2-middle (get-oled-post-outer-x-and-y-vector dx2 dy2))))

         wall-locate-1-to-3-curve-for-polyhedron-second-control-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx1 dy1) curve-post-position-1-middle (get-oled-post-outer-x-and-y-vector dx1 dy1))))
         wall-locate-1-to-3-curve-for-polyhedron-second-control-pointmid1 (make-point-z-value-not-below-zero (transform-mid1 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dxmid1 dymid1) curve-post-position-1-middle (get-oled-post-outer-x-and-y-vector dxmid1 dymid1))))
         wall-locate-1-to-3-curve-for-polyhedron-second-control-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx2 dy2) curve-post-position-2-middle (get-oled-post-outer-x-and-y-vector dx2 dy2))))

         wall-locate3-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate3-for-polyhedron-point dx1 dy1 xy1) curve-post-position-1-bottom (get-oled-post-outer-x-and-y-vector dx1 dy1))))
         wall-locate3-point1-floor (assoc (vec wall-locate3-point1) 2 0)
         wall-locate3-pointmid1 (make-point-z-value-not-below-zero (transform-mid1 (mapv + (wall-locate3-for-polyhedron-point dxmid1 dymid1 xy-mid1) curve-post-position-mid1-bottom (get-oled-post-outer-x-and-y-vector dxmid1 dymid1))))
         wall-locate3-pointmid1-floor (assoc (vec wall-locate3-pointmid1) 2 0)
         wall-locate3-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate3-for-polyhedron-point dx2 dy2 xy2) curve-post-position-2-bottom (get-oled-post-outer-x-and-y-vector dx2 dy2))))
         wall-locate3-point2-floor (assoc (vec wall-locate3-point2) 2 0)


         wall-locate-2-top1 (transform-1 (wall-locate2-for-polyhedron-point dx1 dy1 (mapv + oled-post-position-1-top (get-oled-post-inner-x-and-y-vector dx1 dy1)) xy1))
         wall-locate-2-topmid1 (transform-mid1 (wall-locate2-for-polyhedron-point dxmid1 dymid1 (mapv + oled-post-position-mid1-top (get-oled-post-inner-x-and-y-vector dxmid1 dymid1)) xy-mid1))
         wall-locate-2-top2 (transform-2 (wall-locate2-for-polyhedron-point dx2 dy2 (mapv + oled-post-position-2-top (get-oled-post-inner-x-and-y-vector dx2 dy2)) xy2))

         wall-locate-2-bottom1 (make-point-z-value-not-below-zero (transform-1 (wall-locate2-for-polyhedron-point dx1 dy1 (mapv + oled-post-position-1-bottom (get-oled-post-inner-x-and-y-vector dx1 dy1)) xy1)))
         wall-locate-2-bottom1-floor (assoc (vec wall-locate-2-bottom1) 2 0)
         wall-locate-2-bottommid1 (make-point-z-value-not-below-zero (transform-mid1 (wall-locate2-for-polyhedron-point dxmid1 dymid1 (mapv + oled-post-position-mid1-bottom (get-oled-post-inner-x-and-y-vector dxmid1 dymid1)) xy-mid1)))
         wall-locate-2-bottommid1-floor (assoc (vec wall-locate-2-bottommid1) 2 0)
         wall-locate-2-bottom2 (make-point-z-value-not-below-zero (transform-2 (wall-locate2-for-polyhedron-point dx2 dy2 (mapv + oled-post-position-2-bottom (get-oled-post-inner-x-and-y-vector dx2 dy2)) xy2)))
         wall-locate-2-bottom2-floor (assoc (vec wall-locate-2-bottom2) 2 0)

         web-post-top-curve (bezier-quadratic  web-post-position-1-top web-post-position-mid1-top web-post-position-2-top  steps)
         wall-locate1-curve (bezier-quadratic  wall-locate1-point1 wall-locate1-pointmid1 wall-locate1-point2  steps)
         wall-locate-1-to-3-curve-for-polyhedron-second-control-curve (bezier-quadratic wall-locate-1-to-3-curve-for-polyhedron-control-point1 wall-locate-1-to-3-curve-for-polyhedron-control-pointmid1 wall-locate-1-to-3-curve-for-polyhedron-control-point2 steps)
         wall-locate-1-to-3-curve-for-polyhedron-control-curve (bezier-quadratic wall-locate-1-to-3-curve-for-polyhedron-second-control-point1 wall-locate-1-to-3-curve-for-polyhedron-second-control-pointmid1 wall-locate-1-to-3-curve-for-polyhedron-second-control-point2 steps)
         wall-locate3-curve (bezier-quadratic  wall-locate3-point1 wall-locate3-pointmid1  wall-locate3-point2 steps)
         wall-locate3-floor-curve (bezier-quadratic wall-locate3-point1-floor wall-locate3-pointmid1-floor wall-locate3-point2-floor  steps)

         wall-locate2-bottom-floor-curve (bezier-quadratic  wall-locate-2-bottom2-floor wall-locate-2-bottommid1-floor wall-locate-2-bottom1-floor steps)
         wall-locate2-bottom-curve (bezier-quadratic  wall-locate-2-bottom2 wall-locate-2-bottommid1  wall-locate-2-bottom1 steps)
         wall-locate2-top-curve (bezier-quadratic   wall-locate-2-top2 wall-locate-2-topmid1 wall-locate-2-top1   steps)
         web-post-bottom-curve (bezier-quadratic   web-post-position-2-bottom web-post-position-mid1-bottom web-post-position-1-bottom steps)

         outer-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                               (bezier-quintic
                                                (nth web-post-top-curve index)
                                                (nth wall-locate1-curve index)
                                                (nth wall-locate-1-to-3-curve-for-polyhedron-second-control-curve index)
                                                (nth wall-locate-1-to-3-curve-for-polyhedron-control-curve index)
                                                (nth wall-locate3-curve index)
                                                (nth wall-locate3-floor-curve index)
                                                steps))))

         inner-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                               (bezier-cubic
                                                (nth web-post-bottom-curve index)
                                                (nth wall-locate2-top-curve index)
                                                (nth wall-locate2-bottom-curve index)
                                                (nth wall-locate2-bottom-floor-curve index)
                                                steps))))
         wall-brace-quadratic-polyhedron (polyhedron (concat outer-points inner-points)
                                                     (generate-bezier-along-bezier-polyhedron-faces
                                                      outer-points inner-points
                                                      steps))]
     wall-brace-quadratic-polyhedron)))

(defn wall-brace-cubic-polyhedron-floor-outer [{:keys [place1 dx1 dy1  post-position-1 rad-or-deg1
                                                 place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                                 place-mid2 dxmid2 dymid2  post-position-mid2 rad-or-degmid2
                                                 place2 dx2 dy2  post-position-2 rad-or-deg2 xy1 xymid1 xymid2 xy2
                                                  steps] :or {steps 36 xy1 wall-xy-offset xymid1 wall-xy-offset xymid2 wall-xy-offset xy2 wall-xy-offset}}]
  (let [wall-brace-polyedron-curve-points1 (wall-brace-polyhedron-points place1 dx1 dy1 post-position-1 rad-or-deg1 xy1)
        wall-brace-polyedron-curve-points-mid1 (wall-brace-polyhedron-points place-mid1 dxmid1 dymid1 post-position-mid1 rad-or-degmid1 xymid1) 
        wall-brace-polyedron-curve-points-mid2 (wall-brace-polyhedron-points place-mid2 dxmid2 dymid2 post-position-mid2 rad-or-degmid2 xymid2)
        wall-brace-polyedron-curve-points2 (wall-brace-polyhedron-points place2 dx2 dy2 post-position-2 rad-or-deg2 xy2)]
  (bezier-cubic (wall-brace-polyedron-curve-points1 :wall-locate3-point-floor)
                (wall-brace-polyedron-curve-points-mid1 :wall-locate3-point-floor)
                (wall-brace-polyedron-curve-points-mid2 :wall-locate3-point-floor)
                (wall-brace-polyedron-curve-points2 :wall-locate3-point-floor)  steps)
)
  )

(defn wall-brace-cubic-polyhedron-curves [{:keys [place1 dx1 dy1  post-position-1 rad-or-deg1
                                                 place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                                 place-mid2 dxmid2 dymid2  post-position-mid2 rad-or-degmid2
                                                 place2 dx2 dy2  post-position-2 rad-or-deg2 xy1 xymid1 xymid2 xy2
                                                  steps] :or {steps 36 xy1 wall-xy-offset xymid1 wall-xy-offset xymid2 wall-xy-offset xy2 wall-xy-offset}}]
  (let [wall-brace-polyedron-curve-points1 (wall-brace-polyhedron-points place1 dx1 dy1 post-position-1 rad-or-deg1 xy1)
        wall-brace-polyedron-curve-points-mid1 (wall-brace-polyhedron-points place-mid1 dxmid1 dymid1 post-position-mid1 rad-or-degmid1 xymid1) 
        wall-brace-polyedron-curve-points-mid2 (wall-brace-polyhedron-points place-mid2 dxmid2 dymid2 post-position-mid2 rad-or-degmid2 xymid2)
        wall-brace-polyedron-curve-points2 (wall-brace-polyhedron-points place2 dx2 dy2 post-position-2 rad-or-deg2 xy2)

        web-post-top-curve (bezier-cubic  (wall-brace-polyedron-curve-points1 :web-post-position-top) 
                                           (wall-brace-polyedron-curve-points-mid1 :web-post-position-top)
                                          (wall-brace-polyedron-curve-points-mid2 :web-post-position-top)
                                           (wall-brace-polyedron-curve-points2 :web-post-position-top)
                                           steps)
        
        wall-locate1-curve (bezier-cubic  (wall-brace-polyedron-curve-points1 :wall-locate1-point)
                                            (wall-brace-polyedron-curve-points-mid1 :wall-locate1-point)
                                            (wall-brace-polyedron-curve-points-mid2 :wall-locate1-point)
                                              (wall-brace-polyedron-curve-points2 :wall-locate1-point)
                                              steps)
        
        wall-locate-1-to-3-curve-for-polyhedron-second-control-curve (bezier-cubic  (wall-brace-polyedron-curve-points1 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)
                                                                                          (wall-brace-polyedron-curve-points-mid1 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)
                                                                                          (wall-brace-polyedron-curve-points-mid2 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)
                                                                                        (wall-brace-polyedron-curve-points2 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)
                                                                                        steps)

        wall-locate-1-to-3-curve-for-polyhedron-control-curve (bezier-cubic  (wall-brace-polyedron-curve-points1 :wall-locate-1-to-3-curve-for-polyhedron-control-point)
                                                                               (wall-brace-polyedron-curve-points-mid1 :wall-locate-1-to-3-curve-for-polyhedron-control-point)
                                                                               (wall-brace-polyedron-curve-points-mid2 :wall-locate-1-to-3-curve-for-polyhedron-control-point)  
                                                                                 (wall-brace-polyedron-curve-points2 :wall-locate-1-to-3-curve-for-polyhedron-control-point)  steps)

        wall-locate3-curve (bezier-cubic  (wall-brace-polyedron-curve-points1 :wall-locate3-point)   
                                              (wall-brace-polyedron-curve-points-mid1 :wall-locate3-point)
                                              (wall-brace-polyedron-curve-points-mid2 :wall-locate3-point)
                                              (wall-brace-polyedron-curve-points2 :wall-locate3-point) steps)
        
        wall-locate3-floor-curve (bezier-cubic (wall-brace-polyedron-curve-points1 :wall-locate3-point-floor)
                                             (wall-brace-polyedron-curve-points-mid1 :wall-locate3-point-floor)
                                             (wall-brace-polyedron-curve-points-mid2 :wall-locate3-point-floor)
                                               (wall-brace-polyedron-curve-points2 :wall-locate3-point-floor)  steps)
        
        wall-locate2-bottom-floor-curve (bezier-cubic  (wall-brace-polyedron-curve-points2 :wall-locate-2-bottom-floor)
                                                     (wall-brace-polyedron-curve-points-mid2 :wall-locate-2-bottom-floor)
                                                     (wall-brace-polyedron-curve-points-mid1 :wall-locate-2-bottom-floor)
                                                       (wall-brace-polyedron-curve-points1 :wall-locate-2-bottom-floor) steps)
        
        wall-locate2-bottom-curve (bezier-cubic  (wall-brace-polyedron-curve-points2 :wall-locate-2-bottom)
                                              (wall-brace-polyedron-curve-points-mid2 :wall-locate-2-bottom)
                                              (wall-brace-polyedron-curve-points-mid1 :wall-locate-2-bottom)
                                                 (wall-brace-polyedron-curve-points1 :wall-locate-2-bottom) steps)
        
        wall-locate2-top-curve (bezier-cubic   (wall-brace-polyedron-curve-points2 :wall-locate-2-top)
                                              (wall-brace-polyedron-curve-points-mid2 :wall-locate-2-top)
                                              (wall-brace-polyedron-curve-points-mid1 :wall-locate-2-top)
                                               (wall-brace-polyedron-curve-points1 :wall-locate-2-top)   steps)
        
        web-post-bottom-curve (bezier-cubic   (wall-brace-polyedron-curve-points2 :web-post-position-bottom)
                                              (wall-brace-polyedron-curve-points-mid2 :web-post-position-bottom)
                                              (wall-brace-polyedron-curve-points-mid1 :web-post-position-bottom)
                                              (wall-brace-polyedron-curve-points1 :web-post-position-bottom) steps)
        ]

    {:web-post-top-curve web-post-top-curve
     :wall-locate1-curve wall-locate1-curve
     :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve wall-locate-1-to-3-curve-for-polyhedron-second-control-curve
     :wall-locate-1-to-3-curve-for-polyhedron-control-curve wall-locate-1-to-3-curve-for-polyhedron-control-curve
     :wall-locate3-curve wall-locate3-curve
     :wall-locate3-floor-curve wall-locate3-floor-curve
     :wall-locate2-bottom-floor-curve wall-locate2-bottom-floor-curve
     :wall-locate2-bottom-curve wall-locate2-bottom-curve
     :wall-locate2-top-curve wall-locate2-top-curve
     :web-post-bottom-curve web-post-bottom-curve})
  
  )
(defn wall-brace-cubic-polyhedron
  [{:keys [place1 dx1 dy1  post-position-1 rad-or-deg1
           place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
           place-mid2 dxmid2 dymid2  post-position-mid2 rad-or-degmid2
           place2 dx2 dy2  post-position-2 rad-or-deg2 xy1 xymid1 xymid2 xy2
           steps] :or {steps 36 xy1 wall-xy-offset xymid1 wall-xy-offset xymid2 wall-xy-offset xy2 wall-xy-offset}}] 
   (let [wall-brace-cubic-polyhedron-curves-map 
         (wall-brace-cubic-polyhedron-curves 
          {:place1 place1 :dx1 dx1 :dy1 dy1  :post-position-1 post-position-1 :rad-or-deg1 rad-or-deg1
:place-mid1 place-mid1 :dxmid1 dxmid1 :dymid1 dymid1  :post-position-mid1 post-position-mid1 :rad-or-degmid1 rad-or-degmid1
:place-mid2 place-mid2 :dxmid2 dxmid2 :dymid2 dymid2  :post-position-mid2 post-position-mid2 :rad-or-degmid2 rad-or-degmid2
:place2 place2 :dx2 dx2 :dy2 dy2  :post-position-2 post-position-2 :rad-or-deg2 rad-or-deg2 :xy1 xy1 :xymid1 xymid1 :xymid2 xymid2 :xy2 xy2
           :steps steps
   })
         outer-points (into [] (apply concat
                                      (for [index (range 0 (inc steps))]
                                        (bezier-quintic
                                         (nth (wall-brace-cubic-polyhedron-curves-map :web-post-top-curve) index)
                                         (nth (wall-brace-cubic-polyhedron-curves-map :wall-locate1-curve) index)
                                         (nth (wall-brace-cubic-polyhedron-curves-map :wall-locate-1-to-3-curve-for-polyhedron-control-curve) index)
                                         (nth (wall-brace-cubic-polyhedron-curves-map :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve) index)
                                         (nth (wall-brace-cubic-polyhedron-curves-map :wall-locate3-curve) index)
                                         (nth (wall-brace-cubic-polyhedron-curves-map :wall-locate3-floor-curve) index)
                                         steps))))

         inner-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                               (bezier-cubic
                                                (nth (wall-brace-cubic-polyhedron-curves-map :web-post-bottom-curve) index)
                                                (nth (wall-brace-cubic-polyhedron-curves-map :wall-locate2-top-curve) index)
                                                (nth (wall-brace-cubic-polyhedron-curves-map :wall-locate2-bottom-curve) index)
                                                (nth (wall-brace-cubic-polyhedron-curves-map :wall-locate2-bottom-floor-curve) index)
                                                steps))))
         smoother-wall-polyhedron (polyhedron (concat outer-points inner-points)
                                              (generate-bezier-along-bezier-polyhedron-faces
                                               outer-points inner-points
                                               steps))]
         ;wall-polyhedron 
     smoother-wall-polyhedron))

     (defn generate-polyhedron-web-connecters [top-left-place top-left-post-position top-left-rad-or-deg
     top-right-place top-right-post-position top-right-rad-or-deg
      bottom-left-place bottom-left-post-position bottom-left-rad-or-deg
       bottom-right-place bottom-right-post-position bottom-right-rad-or-deg
        & {:keys [steps] :or {steps 36}}]
        (let [top-left-web-post (web-post-point top-left-place top-left-post-position top-left-rad-or-deg)
        top-right-web-post (web-post-point top-right-place top-right-post-position top-right-rad-or-deg)
      bottom-left-web-post (web-post-point bottom-left-place bottom-left-post-position bottom-left-rad-or-deg)
       bottom-right-web-post (web-post-point bottom-right-place bottom-right-post-position bottom-right-rad-or-deg)
       polyhedron-web-connecters (generate-bezier-along-bezier-polyhedron-from-points-linear 
        (top-left-web-post :top) (top-right-web-post :top)
        (bottom-left-web-post :top) (bottom-right-web-post :top)
        (top-right-web-post :bottom) (top-left-web-post :bottom) 
        (bottom-right-web-post :bottom) (bottom-left-web-post :bottom) 
        steps
        )
        ]
        polyhedron-web-connecters
        )
     )

     (defn generate-polyhedron-key-web-connecters [top-left-place top-left-post-position 
     top-right-place top-right-post-position 
      bottom-left-place bottom-left-post-position 
       bottom-right-place bottom-right-post-position & {:keys [steps] :or {steps 36}}]
      (generate-polyhedron-web-connecters
       top-left-place top-left-post-position :radians 
     top-right-place top-right-post-position :radians
      bottom-left-place bottom-left-post-position :radians
       bottom-right-place bottom-right-post-position :radians :steps steps)
       )

     (defn generate-polyhedron-thumb-web-connecters [top-left-place top-left-post-position 
     top-right-place top-right-post-position 
      bottom-left-place bottom-left-post-position 
       bottom-right-place bottom-right-post-position & {:keys [steps] :or {steps 36}}]
      (generate-polyhedron-web-connecters
       top-left-place top-left-post-position :degrees 
     top-right-place top-right-post-position :degrees
      bottom-left-place bottom-left-post-position :degrees
       bottom-right-place bottom-right-post-position :degrees :steps steps)
       )


(defn get-wall-brace-polyhedron-fn [bottom-plate] (if (true? bottom-plate) wall-brace-polyhedron-outer-floor-linear wall-brace-polyhedron))
(defn get-wall-brace-polyhedron-circular-fn [bottom-plate] (if (true? bottom-plate) wall-brace-polyhedron-circular-outer-floor-linear wall-brace-polyhedron-circular))
(defn get-wall-brace-quadratic-fn [bottom-plate] (if (true? bottom-plate)  wall-brace-quadratic-polyhedron-bottom wall-brace-quadratic-polyhedron))
(defn get-wall-brace-with-circular-fn [bottom-plate] (if (true? bottom-plate)  wall-brace-with-circular-outer-floor-linear wall-brace-with-circular))
(defn get-wall-brace-cubic-fn [bottom-plate] (if (true? bottom-plate) wall-brace-cubic-polyhedron-floor-outer wall-brace-cubic-polyhedron))  
     (defn get-wall-brace-bezier-cubic-through-points-fn [bottom-plate] (if (true? bottom-plate) wall-brace-bezier-cubic-through-points-floor wall-brace-bezier-cubic-through-points-polyhedron))
(defn get-key-wall-brace-polyhedron-fn [bottom-plate] (if (true? bottom-plate) key-wall-brace-polyhedron-outer-floor-linear key-wall-brace-polyhedron))
(defn get-key-wall-brace-polyhedron-circular-fn [bottom-plate] (if (true? bottom-plate) key-wall-brace-polyhedron-circular-outer-floor-linear key-wall-brace-polyhedron-cicular))
(defn get-wall-brace-quadratic-polyhedron-circular-fn [bottom-plate] (if (true? bottom-plate) wall-brace-quadratic-polyhedron-circular-floor-outer wall-brace-quadratic-polyhedron-circular))
(defn get-key-wall-brace-polyhedron-with-circular-fn [bottom-plate] (if (true? bottom-plate) key-wall-brace-polyhedron-with-circular-outer-floor-linear key-wall-brace-polyhedron-with-circular))
(defn get-thumb-wall-brace-polyhedron-fn [bottom-plate] (if (true? bottom-plate) thumb-wall-brace-polyhedron-outer-floor-linear thumb-wall-brace-polyhedron))
(defn get-thumb-wall-brace-polyhedron-circular-fn [bottom-plate] (if (true? bottom-plate) thumb-wall-brace-polyhedron-circular-outer-floor-linear thumb-wall-brace-polyhedron-circular))
(defn get-thumb-wall-brace-polyhedron-with-circular [bottom-plate] (if (true? bottom-plate) thumb-wall-brace-polyhedron-with-circular-outer-floor-linear thumb-wall-brace-polyhedron-with-circular))
(defn get-wall-brace-catmull-rom-spline-fn [bottom-plate] (if (true? bottom-plate) wall-brace-catmull-rom-spline-floor wall-brace-catmull-rom-spline))
(defn get-collect-fn [bottom-plate] (if (true? bottom-plate) concat union))

(defn place-symbol [file {:keys [height place translation orientation-angle offset scale-x scale-y center z-rotation rotation] 
                          :or {offset [0 0 0]}}]
  (let [svg (call :import (format "file = \"%s\"" file) (format "center = %s" center))
        ]
    (->> svg 
         (scale [scale-x scale-y 1]) 
         (extrude-linear {:height height :center false})
         (rdz orientation-angle)
         rotation
         (rdz z-rotation)
         (translate (mapv + place offset translation))
         )
    )
  )

(defn place-2d-shape [shape {:keys [height place translation orientation-angle offset scale-x scale-y center z-rotation rotation]
                          :or {offset [0 0 0]}}]
  (let []
    (->> shape
         (scale [scale-x scale-y 1])
         (extrude-linear {:height height :center false})
         (rdz orientation-angle)
         rotation
         (rdz z-rotation)
         (translate (mapv + place offset translation)))))

(defn place-2d-shape-on-case-wall [shape {:keys [height place orientation-angle z-rotation offset scale-x scale-y center position wall-xy-offset]
                                       :or {height 1 center true  orientation-angle 0 z-rotation 0 wall-xy-offset wall-xy-offset offset [0 0 0]}}]
  (let [web-corner-translation-vector (get-web-corner-translation-vector position)
        rotation (case position
                   "tm" #(rdx -90 %)
                   "bm" #(rdx 90 %)
                   "lm" #(rdy -90 %)
                   "rm" #(rdy 90 %))
        z (/ (nth place 2) -2)
        translation-fn (case position
                         "tm" (fn [vector z-vector] [0 vector z-vector])
                         "bm" (fn [vector z-vector] [0 (- vector) z-vector])
                         "lm" (fn [vector z-vector] [(- vector) 0 z-vector])
                         "rm" (fn [vector z-vector] [vector 0 z-vector]))
        translation (mapv + web-corner-translation-vector (translation-fn (+ wall-thickness wall-xy-offset) z))]
    (place-2d-shape shape  {:height height :place place :translation translation
                        :orientation-angle orientation-angle
                        :scale-x scale-x :scale-y scale-y :center center
                        :z-rotation z-rotation :offset offset :rotation rotation})))

(defn place-symbol-on-case-wall [file {:keys [height place orientation-angle z-rotation offset scale-x scale-y center position wall-xy-offset] 
                                      :or {height 1 center true  orientation-angle 0 z-rotation 0 wall-xy-offset wall-xy-offset offset [0 0 0]}}]
  (let [web-corner-translation-vector (get-web-corner-translation-vector position)
        rotation (case position
                   "tm" #(rdx -90 %)
                   "bm" #(rdx 90 %)
                   "lm" #(rdy -90 %)
                   "rm" #(rdy 90 %)
                   )
        z (/ (nth place 2) -2)
        translation-fn (case position
                         "tm" (fn [vector z-vector] [0 vector z-vector])
                         "bm" (fn [vector z-vector] [0 (- vector) z-vector])
                         "lm" (fn [vector z-vector] [(- vector) 0 z-vector])
                         "rm" (fn [vector z-vector][vector 0 z-vector])
                         )
        translation (mapv + web-corner-translation-vector (translation-fn (+ wall-thickness wall-xy-offset) z))] 
    (place-symbol file {:height height :place place :translation translation 
                        :orientation-angle orientation-angle
                        :scale-x scale-x :scale-y scale-y :center center 
                        :z-rotation z-rotation :offset offset :rotation rotation})
    ) 
  )

 (defn place-symbol-on-thumb-wall [file {:keys [height place orientation-angle z-rotation offset scale-x scale-y center position wall-xy-offset rotation]
                                         :or {height 1 center true  orientation-angle 0 z-rotation 0 wall-xy-offset wall-xy-offset offset [0 0 0] rotation #(rdx 0 %)}}]
   (let [web-corner-translation-vector (get-web-corner-translation-vector position)
         thumb-rotation (case position
                    "tm" #(rdx -90 %)
                    "bm" #(rdx 90 %)
                    "lm" #(rdy -90 %)
                    "rm" #(rdy 90 %))
         z (/ (nth (transform-position place [0 0 0]) 2) -2)
         translation-fn (case position
                          "tm" (fn [vector z-vector] [vector 0 z-vector])
                          "bm" (fn [vector z-vector] [(- vector) 0 z-vector])
                          "lm" (fn [vector z-vector] [0 (- vector) z-vector])
                          "rm" (fn [vector z-vector] [0 vector z-vector]))
         translation (mapv + web-corner-translation-vector (translation-fn (+ wall-thickness wall-xy-offset) z))
         svg (call :import (format "file = \"%s\"" file) (format "center = %s" center))]
     (->> svg
          (scale [scale-x scale-y 1])
          (extrude-linear {:height height :center false})
          (rdz orientation-angle)
          thumb-rotation
          rotation
          (rdz z-rotation)
          (translate (mapv + offset translation))
          (place)
          )
     )
   )
 
  (defn place-2d-shape-on-thumb-wall [shape {:keys [height place orientation-angle z-rotation offset scale-x scale-y center position wall-xy-offset]
                                          :or {height 1 center true  orientation-angle 0 z-rotation 0 wall-xy-offset wall-xy-offset offset [0 0 0]}}]
    (let [web-corner-translation-vector (get-web-corner-translation-vector position)
          rotation (case position
                     "tm" #(rdx -90 %)
                     "bm" #(rdx 90 %)
                     "lm" #(rdy -90 %)
                     "rm" #(rdy 90 %))
          z (/ (nth (transform-position place [0 0 0]) 2) -2)
          translation-fn (case position
                           "tm" (fn [vector z-vector] [vector 0 z-vector])
                           "bm" (fn [vector z-vector] [(- vector) 0 z-vector])
                           "lm" (fn [vector z-vector] [0 (- vector) z-vector])
                           "rm" (fn [vector z-vector] [0 vector z-vector]))
          translation (mapv + web-corner-translation-vector (translation-fn (+ wall-thickness wall-xy-offset) z))
          ]
      (->> shape
           (scale [scale-x scale-y 1])
           (extrude-linear {:height height :center false})
           (rdz orientation-angle)
           rotation
           (rdz z-rotation)
           (translate (mapv + offset translation))
           (place))))

 (defn place-symbol-on-key-wall [file {:keys [height column row orientation-angle z-rotation offset scale-x scale-y center position wall-xy-offset]
                                          :or {height 1 center true column 0 row 0 orientation-angle 0 z-rotation 0 wall-xy-offset wall-xy-offset offset [0 0 0]}}]
   (let [place (key-position column row [0 0 0])]
     
     (place-symbol-on-case-wall file {:height height :place place
                         :orientation-angle orientation-angle
                         :scale-x scale-x :scale-y scale-y :center center
                         :z-rotation z-rotation :offset offset :wall-xy-offset wall-xy-offset
                                      :position position}))
   )
 

