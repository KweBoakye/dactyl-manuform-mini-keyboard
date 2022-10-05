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


(defn key-wall-brace-polyhedron [x1 y1 dx1 dy1 post-position1 x2 y2 dx2 dy2 post-position2 & 
                                 {:keys [steps xy1 xy2] :or {steps 36 xy1 wall-xy-offset xy2 wall-xy-offset}}] 

   (wall-brace-polyhedron (partial key-place x1 y1) dx1 dy1 post-position1 :radians
                         (partial key-place x2 y2) dx2 dy2 post-position2 :radians
                          xy1 xy2 steps))
(defn key-wall-brace-polyhedron-outer-floor-linear [x1 y1 dx1 dy1 post-position1 x2 y2 dx2 dy2 post-position2 &
                                 {:keys [steps xy1 xy2] :or {steps 36 xy1 wall-xy-offset xy2 wall-xy-offset}}]

  (wall-brace-polyhedron-outer-floor-linear (partial key-place x1 y1) dx1 dy1 post-position1 :radians
                         (partial key-place x2 y2) dx2 dy2 post-position2 :radians
                         xy1 xy2 steps))


(defn thumb-wall-brace-polyhedron 
  [place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2 & {:keys [xy1 xy2 steps] :or {xy1 wall-xy-offset xy2 wall-xy-offset steps 36}}] 
  
   (wall-brace-polyhedron place1 dx1 dy1 post-position1 :degrees
                          place2 dx2 dy2 post-position2 :degrees
                          xy1 xy2
                          steps))

(defn thumb-wall-brace-polyhedron-outer-floor-linear
  [place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2 & {:keys [xy1 xy2 steps] :or {xy1 wall-xy-offset xy2 wall-xy-offset steps 36}}]

  (wall-brace-polyhedron-outer-floor-linear place1 dx1 dy1 post-position1 :degrees
                         place2 dx2 dy2 post-position2 :degrees
                         xy1 xy2
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

;; (defn wall-brace-cubic-polyhedron [{:keys [place1 dx1 dy1  post-position-1 rad-or-deg1
;;                                            place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
;;                                            place-mid2 dxmid2 dymid2  post-position-mid2 rad-or-degmid2
;;                                            place2 dx2 dy2  post-position-2 rad-or-deg2 xy1 xymid1 xymid2 xy2
;;                                            steps] :or {steps 36 xy1 wall-xy-offset xymid1 wall-xy-offset xymid2 wall-xy-offset xy2 wall-xy-offset}}]
;;   (let [transform-1 (get-transform-fn rad-or-deg1 place1)
;;         transform-mid1 (get-transform-fn rad-or-degmid1 place-mid1)
;;         transform-mid2 (get-transform-fn rad-or-degmid2 place-mid2)
;;         transform-2 (get-transform-fn rad-or-deg2 place2)

;;         web-corner-translation-vector1 (get-single-plate-corner-position-vector post-position-1)
;;         web-corner-translation-vectormid1 (get-single-plate-corner-position-vector post-position-mid1)
;;         web-corner-translation-vectormid2 (get-single-plate-corner-position-vector post-position-mid2)
;;         web-corner-translation-vector2 (get-single-plate-corner-position-vector post-position-2)

;;         web-post-position-1-top (transform-1 (web-post-position-top (mapv +  web-corner-translation-vector1)))
;;         web-post-position-1-bottom (transform-1 (web-post-position-bottom (mapv +  web-corner-translation-vector1)))

;;         web-post-position-mid1-top (transform-mid1 (web-post-position-top (mapv +  web-corner-translation-vectormid1)))
;;         web-post-position-mid1-bottom (transform-mid1 (web-post-position-bottom (mapv +  web-corner-translation-vectormid1)))

;;         web-post-position-mid2-top (transform-mid2 (web-post-position-top (mapv +  web-corner-translation-vectormid2)))
;;         web-post-position-mid2-bottom (transform-mid2 (web-post-position-bottom (mapv +  web-corner-translation-vectormid2)))

;;         web-post-position-2-top (transform-2 (web-post-position-top (mapv +  web-corner-translation-vector2)))
;;         web-post-position-2-bottom (transform-2 (web-post-position-bottom (mapv +  web-corner-translation-vector2)))

;;         oled-corner-translation-vector1 (get-oled-corner-translation-vector post-position-1)
;;         oled-corner-translation-vectormid1 (get-oled-corner-translation-vector post-position-mid1)
;;         oled-corner-translation-vectormid2 (get-oled-corner-translation-vector post-position-mid2)
;;         oled-corner-translation-vector2  (get-oled-corner-translation-vector post-position-2)

;;         oled-post-position-1-top (oled-post-position-top oled-corner-translation-vector1)
;;         oled-post-position-1-bottom (oled-post-position-bottom oled-corner-translation-vector1)

;;         oled-post-position-mid1-top (oled-post-position-top oled-corner-translation-vectormid1)
;;         oled-post-position-mid1-bottom (oled-post-position-bottom oled-corner-translation-vectormid1)

;;         oled-post-position-mid2-top (oled-post-position-top oled-corner-translation-vectormid2)
;;         oled-post-position-mid2-bottom (oled-post-position-bottom oled-corner-translation-vectormid2)

;;         oled-post-position-2-top (oled-post-position-top oled-corner-translation-vector2)
;;         oled-post-position-2-bottom  (oled-post-position-bottom oled-corner-translation-vector2)

;;         curve-corner-translation-vector1 (get-curve-corner-translation-vector post-position-1)
;;         curve-corner-translation-vectormid1 (get-curve-corner-translation-vector post-position-mid1)
;;         curve-corner-translation-vectormid2 (get-curve-corner-translation-vector post-position-mid2)
;;         curve-corner-translation-vector2 (get-curve-corner-translation-vector post-position-2)

;;         curve-post-position-1-top (mapv + (curve-post-position-top curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))
;;         curve-post-position-1-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))

;;         curve-post-position-mid1-top (mapv + (curve-post-position-top curve-corner-translation-vectormid1) (get-curve-post-outer-x-and-y-vector dxmid1 dymid1))
;;         curve-post-position-mid1-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vectormid1) (get-curve-post-outer-x-and-y-vector dxmid1 dymid1))

;;         curve-post-position-mid2-top (mapv + (curve-post-position-top curve-corner-translation-vectormid2) (get-curve-post-outer-x-and-y-vector dxmid2 dymid2))
;;         curve-post-position-mid2-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vectormid2) (get-curve-post-outer-x-and-y-vector dxmid2 dymid2))

;;         curve-post-position-2-top (mapv + (curve-post-position-top curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))
;;         curve-post-position-2-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))

;;         wall-locate1-point1 (transform-1 (mapv + (wall-locate1 dx1 dy1) curve-post-position-1-top))
;;         wall-locate1-pointmid1 (transform-mid1 (mapv + (wall-locate1 dxmid1 dymid1) curve-post-position-mid1-top))
;;         wall-locate1-pointmid2 (transform-mid2 (mapv + (wall-locate1 dxmid2 dymid2) curve-post-position-mid2-top))
;;         wall-locate1-point2 (transform-2 (mapv + (wall-locate1 dx2 dy2) curve-post-position-2-top))

;;         wall-locate3-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate3-for-polyhedron-point dx1 dy1) curve-post-position-1-bottom (get-oled-post-outer-x-and-y-vector dx1 dy1))))
;;         wall-locate3-point1-floor (assoc (vec wall-locate3-point1) 2 0)

;;         wall-locate3-pointmid1 (make-point-z-value-not-below-zero (transform-mid1 (mapv + (wall-locate3-for-polyhedron-point dxmid1 dymid1) curve-post-position-mid1-bottom (get-oled-post-outer-x-and-y-vector dxmid1 dymid1))))
;;         wall-locate3-pointmid1-floor (assoc (vec wall-locate3-pointmid1) 2 0)

;;         wall-locate3-pointmid2 (make-point-z-value-not-below-zero (transform-mid2 (mapv + (wall-locate3-for-polyhedron-point dxmid2 dymid2) curve-post-position-mid2-bottom (get-oled-post-outer-x-and-y-vector dxmid2 dymid2))))
;;         wall-locate3-pointmid2-floor (assoc (vec wall-locate3-pointmid2) 2 0)

;;         wall-locate3-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate3-for-polyhedron-point dx2 dy2) curve-post-position-2-bottom (get-oled-post-outer-x-and-y-vector dx2 dy2))))
;;         wall-locate3-point2-floor (assoc (vec wall-locate3-point2) 2 0)


;;         wall-locate-2-top1 (transform-1 (wall-locate2 dx1 dy1 (mapv + oled-post-position-1-top (get-oled-post-inner-x-and-y-vector dx1 dy1))))
;;         wall-locate-2-topmid1 (transform-mid1 (wall-locate2 dxmid1 dymid1 (mapv + oled-post-position-mid1-top (get-oled-post-inner-x-and-y-vector dxmid1 dymid1))))
;;         wall-locate-2-topmid2 (transform-mid2 (wall-locate2 dxmid2 dymid2 (mapv + oled-post-position-mid2-top (get-oled-post-inner-x-and-y-vector dxmid2 dymid2))))
;;         wall-locate-2-top2 (transform-2 (wall-locate2 dx2 dy2 (mapv + oled-post-position-2-top (get-oled-post-inner-x-and-y-vector dx2 dy2))))

;;         wall-locate-2-bottom1 (make-point-z-value-not-below-zero (transform-1 (wall-locate2 dx1 dy1 (mapv + oled-post-position-1-bottom (get-oled-post-inner-x-and-y-vector dx1 dy1)))))
;;         wall-locate-2-bottom1-floor (assoc (vec wall-locate-2-bottom1) 2 0)
;;         wall-locate-2-bottommid1 (make-point-z-value-not-below-zero (transform-mid1 (wall-locate2 dxmid1 dymid1 (mapv + oled-post-position-mid1-bottom (get-oled-post-inner-x-and-y-vector dxmid1 dymid1)))))
;;         wall-locate-2-bottommid1-floor (assoc (vec wall-locate-2-bottommid1) 2 0)

;;         wall-locate-2-bottommid2 (make-point-z-value-not-below-zero (transform-mid2 (wall-locate2 dxmid2 dymid2 (mapv + oled-post-position-mid2-bottom (get-oled-post-inner-x-and-y-vector dxmid2 dymid2)))))
;;         wall-locate-2-bottommid2-floor (assoc (vec wall-locate-2-bottommid2) 2 0)

;;         wall-locate-2-bottom2 (make-point-z-value-not-below-zero (transform-2 (wall-locate2 dx2 dy2 (mapv + oled-post-position-2-bottom (get-oled-post-inner-x-and-y-vector dx2 dy2)))))
;;         wall-locate-2-bottom2-floor (assoc (vec wall-locate-2-bottom2) 2 0)

;;         web-post-top-curve (bezier-cubic  web-post-position-1-top web-post-position-mid1-top web-post-position-mid2-top web-post-position-2-top  steps)
;;         wall-locate1-curve (bezier-cubic  wall-locate1-point1 wall-locate1-pointmid1 wall-locate1-pointmid2 wall-locate1-point2  steps)
;;         wall-locate3-curve (bezier-cubic  wall-locate3-point1 wall-locate3-pointmid1 wall-locate3-pointmid2  wall-locate3-point2 steps)
;;         wall-locate3-floor-curve (bezier-cubic wall-locate3-point1-floor wall-locate3-pointmid1-floor wall-locate3-pointmid2-floor wall-locate3-point2-floor  steps)

;;         wall-locate2-bottom-floor-curve (bezier-cubic  wall-locate-2-bottom2-floor wall-locate-2-bottommid2-floor wall-locate-2-bottommid1-floor  wall-locate-2-bottom1-floor steps)
;;         wall-locate2-bottom-curve (bezier-cubic  wall-locate-2-bottom2 wall-locate-2-bottommid2 wall-locate-2-bottommid1  wall-locate-2-bottom1 steps)
;;         wall-locate2-top-curve (bezier-cubic   wall-locate-2-top2 wall-locate-2-topmid2 wall-locate-2-topmid1 wall-locate-2-top1   steps)
;;         web-post-bottom-curve (bezier-cubic   web-post-position-2-bottom web-post-position-mid2-bottom web-post-position-mid1-bottom web-post-position-1-bottom steps)

;;         outer-points (into [] (apply concat (for [index (range 0 (inc steps))]
;;                                               (bezier-cubic
;;                                                (nth web-post-top-curve index)
;;                                                (nth wall-locate1-curve index)
;;                                                (nth wall-locate3-curve index)
;;                                                (nth wall-locate3-floor-curve index)
;;                                                steps))))

;;         inner-points (into [] (apply concat (for [index (range 0 (inc steps))]
;;                                               (bezier-cubic
;;                                                (nth web-post-bottom-curve index)
;;                                                (nth wall-locate2-top-curve index)
;;                                                (nth wall-locate2-bottom-curve index)
;;                                                (nth wall-locate2-bottom-floor-curve index)
;;                                                steps))))
;;         wall-brace-cubic-polyhedron (polyhedron (concat outer-points inner-points)
;;                                                 (generate-bezier-along-bezier-polyhedron-faces
;;                                                  outer-points inner-points
;;                                                  steps))]
;;     wall-brace-cubic-polyhedron))

