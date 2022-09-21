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
          [dactyl-keyboard.EVQWGD001 :refer :all])
  )

(defn wall-locate-1-to-3-curve-for-polyhedron-control-point [dx dy] 
   [(* dx wall-thickness) (* dy wall-thickness) 0]
  )
(defn wall-locate2-for-polyhedron-point ([dx dy] (wall-locate2-for-polyhedron-point dx dy [0 0 0]))
  ([dx dy orig-point] (wall-locate2-for-polyhedron-point dx dy orig-point wall-xy-offset))
  ([dx dy orig-point xy] (mapv + orig-point [(* dx xy) (* dy xy) wall-z-offset])))

(defn wall-locate-1-to-3-curve-for-polyhedron-second-control-point 
  ([dx dy] (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx dy [0 0 0]))
  ([dx dy orig-point](wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx dy orig-point wall-xy-offset))
  ([dx dy orig-point xy](mapv + [(* dx (+ xy wall-thickness (+ (/ oled-post-size 2))))
           (* dy (+ xy wall-thickness (+ (/ oled-post-size 2))))
           (- wall-z-offset 1)] orig-point))
  )
(defn wall-locate3-for-polyhedron-point ([dx dy] (wall-locate3-for-polyhedron-point dx dy wall-xy-offset))
  ([dx dy xy]
   [(* 0.98 (* dx (+ xy wall-thickness (+ (/ oled-post-size 2)))))
   (* 0.98 (* dy (+ xy wall-thickness (+ (/ oled-post-size 2)))))
   (+ 0.5 (- wall-z-offset (/ oled-holder-thickness  1)))]))

(defn wall-locate3-xy-for-polyhedron-point [dx dy xy]
  [(* 0.98 (* dx (+ xy wall-thickness (+ (/ oled-post-size 2)))))
   (* 0.98 (* dy (+ xy wall-thickness (+ (/ oled-post-size 2)))))
   (+ 0.5 (- wall-z-offset (/ oled-holder-thickness  1)))])

(defn wall-brace-polyhedron 
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
   (wall-brace-polyhedron place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps]
   (let [transform-1 (get-transform-fn rad-or-deg1 place1)
        transform-2 (get-transform-fn rad-or-deg2 place2)
        oled-post-outer-x-1 (if (pos? dx1) (/ oled-post-size 2) (/ oled-post-size -2))
        oled-post-outer-x-2 (if (pos? dx2) (/ oled-post-size 2) (/ oled-post-size -2))
        oled-post-outer-y-1 (if (pos? dy1) (/ oled-post-size 2) (/ oled-post-size -2))
        oled-post-outer-y-2 (if (pos? dy2) (/ oled-post-size 2) (/ oled-post-size -2))
        oled-post-inner-x-1 (if (pos? dx1) (/ oled-post-size -2) (/ oled-post-size 2))
        oled-post-inner-x-2 (if (pos? dx2) (/ oled-post-size -2) (/ oled-post-size 2))
        oled-post-inner-y-1 (if (pos? dy1) (/ oled-post-size -2) (/ oled-post-size 2))
        oled-post-inner-y-2 (if (pos? dy2) (/ oled-post-size -2) (/ oled-post-size 2))
        curve-post-x-1 (if (pos? dx1) (/ curve-post-size 2) (/ curve-post-size -2))
        curve-post-y-1 (if (pos? dy1) (/ curve-post-size 2) (/ curve-post-size -2))
        curve-post-x-2 (if (pos? dx2) (/ curve-post-size 2) (/ curve-post-size -2))
        curve-post-y-2 (if (pos? dy2) (/ curve-post-size 2) (/ curve-post-size -2))






        web-corner-translation-vector1 (get-single-plate-corner-position-vector post-position-1)
        web-corner-translation-vector2 (get-single-plate-corner-position-vector post-position-2)
        web-post-position-1-top (transform-1 (web-post-position-top web-corner-translation-vector1))
        web-post-position-1-bottom (transform-1 (web-post-position-bottom web-corner-translation-vector1))
        web-post-position-2-top (transform-2 (web-post-position-top web-corner-translation-vector2))
        web-post-position-2-bottom (transform-2 (web-post-position-bottom web-corner-translation-vector2))
        oled-corner-translation-vector1 (get-oled-corner-translation-vector post-position-1)
        oled-corner-translation-vector2  (get-oled-corner-translation-vector post-position-2)
        web-corner-translation-vector1 (get-single-plate-corner-position-vector post-position-1)
        wneb-corner-translation-vector2 (get-single-plate-corner-position-vector post-position-2)
        oled-post-position-1-top (oled-post-position-top oled-corner-translation-vector1)
        oled-post-position-1-bottom (oled-post-position-bottom oled-corner-translation-vector1)
        oled-post-position-2-top (oled-post-position-top oled-corner-translation-vector2)
        oled-post-position-2-bottom  (oled-post-position-bottom oled-corner-translation-vector2)
        curve-corner-translation-vector1 (get-curve-corner-translation-vector post-position-1)
        curve-corner-translation-vector2 (get-curve-corner-translation-vector post-position-2)

        curve-post-position-1-top (mapv + (curve-post-position-top curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))
        curve-post-position-1-middle (mapv + (curve-post-position-middle curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))
        curve-post-position-1-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))
        curve-post-position-2-top (mapv + (curve-post-position-top curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))
        curve-post-position-2-middle (mapv + (curve-post-position-middle curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))
        curve-post-position-2-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))
        wall-locate1-point1 (transform-1 (mapv + (wall-locate1 dx1 dy1) curve-post-position-1-top))
        wall-locate1-point2 (transform-2 (mapv + (wall-locate1 dx2 dy2) curve-post-position-2-top))
        
        wall-locate-1-to-3-curve-for-polyhedron-control-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx1 dy1) curve-post-position-1-middle (get-oled-post-outer-x-and-y-vector dx1 dy1))))
         wall-locate-1-to-3-curve-for-polyhedron-control-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx2 dy2) curve-post-position-2-middle (get-oled-post-outer-x-and-y-vector dx2 dy2))))
        
        wall-locate-1-to-3-curve-for-polyhedron-second-control-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx1 dy1) curve-post-position-1-middle (get-oled-post-outer-x-and-y-vector dx1 dy1))))
wall-locate-1-to-3-curve-for-polyhedron-second-control-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx2 dy2) curve-post-position-2-middle (get-oled-post-outer-x-and-y-vector dx2 dy2))))
        
        wall-locate3-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate3-for-polyhedron-point dx1 dy1 xy1) curve-post-position-1-bottom (get-oled-post-outer-x-and-y-vector dx1 dy1))))
        wall-locate3-point1-floor (assoc (vec wall-locate3-point1) 2 0)
        wall-locate3-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate3-for-polyhedron-point dx2 dy2 xy2) curve-post-position-2-bottom (get-oled-post-outer-x-and-y-vector dx2 dy2))))
        
        wall-locate3-point2-floor (assoc (vec wall-locate3-point2) 2 0)


        wall-locate-2-top1 (transform-1 (wall-locate2-for-polyhedron-point dx1 dy1 (mapv + oled-post-position-1-top (get-oled-post-inner-x-and-y-vector dx1 dy1)) xy1))
        wall-locate-2-top2 (transform-2 (wall-locate2-for-polyhedron-point dx1 dy1 (mapv + oled-post-position-2-top (get-oled-post-inner-x-and-y-vector dx2 dy2)) xy2))
        wall-locate-2-bottom1 (make-point-z-value-not-below-zero (transform-1 (wall-locate2-for-polyhedron-point dx1 dy1 (mapv + oled-post-position-1-bottom (get-oled-post-inner-x-and-y-vector dx1 dy1)) xy1)))
        wall-locate-2-bottom1-floor (assoc (vec wall-locate-2-bottom1) 2 0)
        wall-locate-2-bottom2 (make-point-z-value-not-below-zero (transform-2 (wall-locate2-for-polyhedron-point dx2 dy2 (mapv + oled-post-position-2-bottom (get-oled-post-inner-x-and-y-vector dx2 dy2)) xy2)))
        wall-locate-2-bottom2-floor (assoc (vec wall-locate-2-bottom2) 2 0)

        web-post-top-curve (bezier-linear  web-post-position-1-top  web-post-position-2-top  steps)
        wall-locate1-curve (bezier-linear  wall-locate1-point1  wall-locate1-point2  steps)
         wall-locate-1-to-3-curve-for-polyhedron-second-control-curve (bezier-linear  wall-locate-1-to-3-curve-for-polyhedron-second-control-point1  wall-locate-1-to-3-curve-for-polyhedron-second-control-point2  steps)
        wall-locate-1-to-3-curve-for-polyhedron-control-curve (bezier-linear  wall-locate-1-to-3-curve-for-polyhedron-control-point1  wall-locate-1-to-3-curve-for-polyhedron-control-point2  steps)
        wall-locate3-curve (bezier-linear  wall-locate3-point1   wall-locate3-point2 steps)
        wall-locate3-floor-curve (bezier-linear wall-locate3-point1-floor  wall-locate3-point2-floor  steps)

        wall-locate2-bottom-floor-curve (bezier-linear  wall-locate-2-bottom2-floor  wall-locate-2-bottom1-floor steps)
        wall-locate2-bottom-curve (bezier-linear  wall-locate-2-bottom2   wall-locate-2-bottom1 steps)
        wall-locate2-top-curve (bezier-linear   wall-locate-2-top2  wall-locate-2-top1   steps)
        web-post-bottom-curve (bezier-linear   web-post-position-2-bottom  web-post-position-1-bottom steps)

        outer-curve1 (bezier-cubic web-post-position-1-top  wall-locate1-point1 wall-locate3-point1 wall-locate3-point1-floor    steps)
        inner-curve1 (bezier-cubic   wall-locate-2-bottom1-floor wall-locate-2-bottom1 wall-locate-2-top1 web-post-position-1-bottom steps)
        outer-curve2 (bezier-cubic web-post-position-2-top  wall-locate1-point2  wall-locate3-point2 wall-locate3-point2-floor   steps)
        inner-curve2 (bezier-cubic   wall-locate-2-bottom2-floor wall-locate-2-bottom2 wall-locate-2-top2 web-post-position-2-bottom steps)
        wall-polyhedron (generate-polyhedron-from-points  outer-curve2 outer-curve1  inner-curve2 inner-curve1   steps)

        outer-points (into [] (apply concat
                                     (for [index (range 0 (inc steps))]
                                       (bezier-quintic
                                        (nth web-post-top-curve index)
                                        (nth wall-locate1-curve index)
                                        (nth wall-locate-1-to-3-curve-for-polyhedron-control-curve index)
                                        (nth wall-locate-1-to-3-curve-for-polyhedron-second-control-curve index) 
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
        smoother-wall-polyhedron (polyhedron (concat outer-points inner-points)
                                             (generate-bezier-along-bezier-polyhedron-faces
                                              outer-points inner-points
                                              steps))]
         ;wall-polyhedron 
    smoother-wall-polyhedron)))


(defn key-wall-brace-polyhedron [x1 y1 dx1 dy1 post-position1 x2 y2 dx2 dy2 post-position2]
  (wall-brace-polyhedron (partial key-place x1 y1) dx1 dy1 post-position1 :radians
                         (partial key-place x2 y2) dx2 dy2 post-position2 :radians
                         36))

(defn thumb-wall-brace-polyhedron ([place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2] (thumb-wall-brace-polyhedron place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2 20))
  ([place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2 steps]
   (wall-brace-polyhedron place1 dx1 dy1 post-position1 :degrees
                          place2 dx2 dy2 post-position2 :degrees
                          steps)))

(defn web-post-linear [place1 post-position-1 rad-or-deg1 
                                place2 post-position-2 rad-or-deg2 steps]
  (let [transform-1 (get-transform-fn rad-or-deg1 place1)
        transform-2 (get-transform-fn rad-or-deg2 place2)
        web-corner-translation-vector1 (get-single-plate-corner-position-vector post-position-1) 
        web-corner-translation-vector2 (get-single-plate-corner-position-vector post-position-2)
        web-post-position-1-top (transform-1 (web-post-position-top (mapv +  web-corner-translation-vector1)))
        web-post-position-1-bottom (transform-1 (web-post-position-bottom (mapv +  web-corner-translation-vector1)))


        web-post-position-2-top (transform-2 (web-post-position-top (mapv +  web-corner-translation-vector2)))
        web-post-position-2-bottom (transform-2 (web-post-position-bottom (mapv +  web-corner-translation-vector2)))
        web-post-top-curve (bezier-linear  web-post-position-1-top  web-post-position-2-top  steps)
        web-post-bottom-curve (bezier-linear   web-post-position-2-bottom  web-post-position-1-bottom steps)
        ;; web-post-curve (into [] (apply concat
        ;;                                (for [index (range 0 (inc steps))]
        ;;                                  (bezier-linear
        ;;                                   (nth web-post-top-curve index)
        ;;                                   (nth web-post-bottom-curve index)
        ;;                                   steps))))
        web-post-curve [web-post-top-curve web-post-bottom-curve]
        ]
    web-post-curve))

(defn web-post-quadratic-curve [place1 post-position-1 rad-or-deg1
place-mid1 post-position-mid1 rad-or-degmid1
place2 post-position-2 rad-or-deg2 steps]
  (let [
        transform-1 (get-transform-fn rad-or-deg1 place1)
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
     web-post-curve [web-post-top-curve web-post-bottom-curve]
        ] 
    web-post-curve
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
        wall-locate-1-to-3-curve-for-polyhedron-control-pointmid1 (make-point-z-value-not-below-zero (transform-mid1 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dxmid1 dymid1) curve-post-position-1-middle (get-oled-post-outer-x-and-y-vector dxmid1 dymid1))))
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

(defn wall-brace-cubic-polyhedron [{:keys [place1 dx1 dy1  post-position-1 rad-or-deg1
                                           place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                           place-mid2 dxmid2 dymid2  post-position-mid2 rad-or-degmid2
                                           place2 dx2 dy2  post-position-2 rad-or-deg2 steps] :or {steps 36}}]
  (let [transform-1 (get-transform-fn rad-or-deg1 place1)
        transform-mid1 (get-transform-fn rad-or-degmid1 place-mid1)
        transform-mid2 (get-transform-fn rad-or-degmid2 place-mid2)
        transform-2 (get-transform-fn rad-or-deg2 place2)

        web-corner-translation-vector1 (get-single-plate-corner-position-vector post-position-1)
        web-corner-translation-vectormid1 (get-single-plate-corner-position-vector post-position-mid1)
        web-corner-translation-vectormid2 (get-single-plate-corner-position-vector post-position-mid2)
        web-corner-translation-vector2 (get-single-plate-corner-position-vector post-position-2)

        web-post-position-1-top (transform-1 (web-post-position-top (mapv +  web-corner-translation-vector1)))
        web-post-position-1-bottom (transform-1 (web-post-position-bottom (mapv +  web-corner-translation-vector1)))

        web-post-position-mid1-top (transform-mid1 (web-post-position-top (mapv +  web-corner-translation-vectormid1)))
        web-post-position-mid1-bottom (transform-mid1 (web-post-position-bottom (mapv +  web-corner-translation-vectormid1)))

        web-post-position-mid2-top (transform-mid2 (web-post-position-top (mapv +  web-corner-translation-vectormid2)))
        web-post-position-mid2-bottom (transform-mid2 (web-post-position-bottom (mapv +  web-corner-translation-vectormid2)))

        web-post-position-2-top (transform-2 (web-post-position-top (mapv +  web-corner-translation-vector2)))
        web-post-position-2-bottom (transform-2 (web-post-position-bottom (mapv +  web-corner-translation-vector2)))

        oled-corner-translation-vector1 (get-oled-corner-translation-vector post-position-1)
        oled-corner-translation-vectormid1 (get-oled-corner-translation-vector post-position-mid1)
        oled-corner-translation-vectormid2 (get-oled-corner-translation-vector post-position-mid2)
        oled-corner-translation-vector2  (get-oled-corner-translation-vector post-position-2)

        oled-post-position-1-top (oled-post-position-top oled-corner-translation-vector1)
        oled-post-position-1-bottom (oled-post-position-bottom oled-corner-translation-vector1)

        oled-post-position-mid1-top (oled-post-position-top oled-corner-translation-vectormid1)
        oled-post-position-mid1-bottom (oled-post-position-bottom oled-corner-translation-vectormid1)

        oled-post-position-mid2-top (oled-post-position-top oled-corner-translation-vectormid2)
        oled-post-position-mid2-bottom (oled-post-position-bottom oled-corner-translation-vectormid2)

        oled-post-position-2-top (oled-post-position-top oled-corner-translation-vector2)
        oled-post-position-2-bottom  (oled-post-position-bottom oled-corner-translation-vector2)

        curve-corner-translation-vector1 (get-curve-corner-translation-vector post-position-1)
        curve-corner-translation-vectormid1 (get-curve-corner-translation-vector post-position-mid1)
        curve-corner-translation-vectormid2 (get-curve-corner-translation-vector post-position-mid2)
        curve-corner-translation-vector2 (get-curve-corner-translation-vector post-position-2)

        curve-post-position-1-top (mapv + (curve-post-position-top curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))
        curve-post-position-1-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))

        curve-post-position-mid1-top (mapv + (curve-post-position-top curve-corner-translation-vectormid1) (get-curve-post-outer-x-and-y-vector dxmid1 dymid1))
        curve-post-position-mid1-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vectormid1) (get-curve-post-outer-x-and-y-vector dxmid1 dymid1))

        curve-post-position-mid2-top (mapv + (curve-post-position-top curve-corner-translation-vectormid2) (get-curve-post-outer-x-and-y-vector dxmid2 dymid2))
        curve-post-position-mid2-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vectormid2) (get-curve-post-outer-x-and-y-vector dxmid2 dymid2))

        curve-post-position-2-top (mapv + (curve-post-position-top curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))
        curve-post-position-2-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))

        wall-locate1-point1 (transform-1 (mapv + (wall-locate1 dx1 dy1) curve-post-position-1-top))
        wall-locate1-pointmid1 (transform-mid1 (mapv + (wall-locate1 dxmid1 dymid1) curve-post-position-mid1-top))
        wall-locate1-pointmid2 (transform-mid2 (mapv + (wall-locate1 dxmid2 dymid2) curve-post-position-mid2-top))
        wall-locate1-point2 (transform-2 (mapv + (wall-locate1 dx2 dy2) curve-post-position-2-top))

        wall-locate3-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate3-for-polyhedron-point dx1 dy1) curve-post-position-1-bottom (get-oled-post-outer-x-and-y-vector dx1 dy1))))
        wall-locate3-point1-floor (assoc (vec wall-locate3-point1) 2 0)

        wall-locate3-pointmid1 (make-point-z-value-not-below-zero (transform-mid1 (mapv + (wall-locate3-for-polyhedron-point dxmid1 dymid1) curve-post-position-mid1-bottom (get-oled-post-outer-x-and-y-vector dxmid1 dymid1))))
        wall-locate3-pointmid1-floor (assoc (vec wall-locate3-pointmid1) 2 0)

        wall-locate3-pointmid2 (make-point-z-value-not-below-zero (transform-mid2 (mapv + (wall-locate3-for-polyhedron-point dxmid2 dymid2) curve-post-position-mid2-bottom (get-oled-post-outer-x-and-y-vector dxmid2 dymid2))))
        wall-locate3-pointmid2-floor (assoc (vec wall-locate3-pointmid2) 2 0)

        wall-locate3-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate3-for-polyhedron-point dx2 dy2) curve-post-position-2-bottom (get-oled-post-outer-x-and-y-vector dx2 dy2))))
        wall-locate3-point2-floor (assoc (vec wall-locate3-point2) 2 0)


        wall-locate-2-top1 (transform-1 (wall-locate2 dx1 dy1 (mapv + oled-post-position-1-top (get-oled-post-inner-x-and-y-vector dx1 dy1))))
        wall-locate-2-topmid1 (transform-mid1 (wall-locate2 dxmid1 dymid1 (mapv + oled-post-position-mid1-top (get-oled-post-inner-x-and-y-vector dxmid1 dymid1))))
        wall-locate-2-topmid2 (transform-mid2 (wall-locate2 dxmid2 dymid2 (mapv + oled-post-position-mid2-top (get-oled-post-inner-x-and-y-vector dxmid2 dymid2))))
        wall-locate-2-top2 (transform-2 (wall-locate2 dx2 dy2 (mapv + oled-post-position-2-top (get-oled-post-inner-x-and-y-vector dx2 dy2))))

        wall-locate-2-bottom1 (make-point-z-value-not-below-zero (transform-1 (wall-locate2 dx1 dy1 (mapv + oled-post-position-1-bottom (get-oled-post-inner-x-and-y-vector dx1 dy1)))))
        wall-locate-2-bottom1-floor (assoc (vec wall-locate-2-bottom1) 2 0)
        wall-locate-2-bottommid1 (make-point-z-value-not-below-zero (transform-mid1 (wall-locate2 dxmid1 dymid1 (mapv + oled-post-position-mid1-bottom (get-oled-post-inner-x-and-y-vector dxmid1 dymid1)))))
        wall-locate-2-bottommid1-floor (assoc (vec wall-locate-2-bottommid1) 2 0)

        wall-locate-2-bottommid2 (make-point-z-value-not-below-zero (transform-mid2 (wall-locate2 dxmid2 dymid2 (mapv + oled-post-position-mid2-bottom (get-oled-post-inner-x-and-y-vector dxmid2 dymid2)))))
        wall-locate-2-bottommid2-floor (assoc (vec wall-locate-2-bottommid2) 2 0)

        wall-locate-2-bottom2 (make-point-z-value-not-below-zero (transform-2 (wall-locate2 dx2 dy2 (mapv + oled-post-position-2-bottom (get-oled-post-inner-x-and-y-vector dx2 dy2)))))
        wall-locate-2-bottom2-floor (assoc (vec wall-locate-2-bottom2) 2 0)

        web-post-top-curve (bezier-cubic  web-post-position-1-top web-post-position-mid1-top web-post-position-mid2-top web-post-position-2-top  steps)
        wall-locate1-curve (bezier-cubic  wall-locate1-point1 wall-locate1-pointmid1 wall-locate1-pointmid2 wall-locate1-point2  steps)
        wall-locate3-curve (bezier-cubic  wall-locate3-point1 wall-locate3-pointmid1 wall-locate3-pointmid2  wall-locate3-point2 steps)
        wall-locate3-floor-curve (bezier-cubic wall-locate3-point1-floor wall-locate3-pointmid1-floor wall-locate3-pointmid2-floor wall-locate3-point2-floor  steps)

        wall-locate2-bottom-floor-curve (bezier-cubic  wall-locate-2-bottom2-floor wall-locate-2-bottommid2-floor wall-locate-2-bottommid1-floor  wall-locate-2-bottom1-floor steps)
        wall-locate2-bottom-curve (bezier-cubic  wall-locate-2-bottom2 wall-locate-2-bottommid2 wall-locate-2-bottommid1  wall-locate-2-bottom1 steps)
        wall-locate2-top-curve (bezier-cubic   wall-locate-2-top2 wall-locate-2-topmid2 wall-locate-2-topmid1 wall-locate-2-top1   steps)
        web-post-bottom-curve (bezier-cubic   web-post-position-2-bottom web-post-position-mid2-bottom web-post-position-mid1-bottom web-post-position-1-bottom steps)

        outer-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                              (bezier-cubic
                                               (nth web-post-top-curve index)
                                               (nth wall-locate1-curve index)
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
        wall-brace-cubic-polyhedron (polyhedron (concat outer-points inner-points)
                                                (generate-bezier-along-bezier-polyhedron-faces
                                                 outer-points inner-points
                                                 steps))]
    wall-brace-cubic-polyhedron))

(defn pinky-to-fourth-br-bl [steps]
  (let [
        pinky-to-fourth-web-post-curve
(web-post-quadratic-curve (partial key-place lastcol cornerrow)  "bl" :radians 
                          (partial key-place 3 cornerrow) "br" :radians 
                          (partial key-place 3 cornerrow) "bl" :radians
                          steps)
fourth-br-top (transform-position-radians (partial key-place 3 cornerrow) (web-post-position-top (mapv +  (get-single-plate-corner-position-vector "br"))))
fourth-br-bottom (transform-position-radians (partial key-place 3 cornerrow) (web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "br"))))
fourth-bl-to-br (web-post-linear
                 (partial key-place lastcol cornerrow) "bl" :radians
                 (partial key-place 3 cornerrow) "br" :radians 
                 steps)
fourth-br-to-pinky-bl (web-post-linear
                       (partial key-place lastcol cornerrow) "bl" :radians
                       (partial key-place 3 cornerrow) "br" :radians
                       18)
        outer-top-start 0
        outer-top-count (count (nth pinky-to-fourth-web-post-curve 0))
        outer-top-end steps
        outer-bottom-start (inc outer-top-end)
        outer-bottom-count (count (nth pinky-to-fourth-web-post-curve 1))
        outer-bottom-end (+ outer-bottom-start steps)
        inner-top-start (inc outer-bottom-end)
inner-top-end (+ inner-top-start steps)
inner-bottom-start (inc inner-top-end)
inner-bottom-end (+ inner-bottom-start steps)
top-layer (into [] (apply concat (for [index (range 0 (inc steps))
                                 :let [pinky-to-fourth-web-post-curve-top 
                                       (nth pinky-to-fourth-web-post-curve 0)
                                       fourth-bl-to-br-top (nth fourth-bl-to-br 0)
                                       ]] 
                             (bezier-linear
                              (nth pinky-to-fourth-web-post-curve-top index)
                              (nth fourth-bl-to-br-top index)
                              steps
                              ) 
                             )))
        bottom-layer (into [] (apply concat (for [index (range 0 (inc steps))
                                            :let [pinky-to-fourth-web-post-curve-bottom
 (nth pinky-to-fourth-web-post-curve 1)
fourth-bl-to-br-bottom (nth fourth-bl-to-br 1)]]
                                        (bezier-linear
                                         (nth pinky-to-fourth-web-post-curve-bottom index)
                                         (nth fourth-bl-to-br-bottom index)
                                         steps
                                         )
                                        )))
  ]
  ;; (generate-polyhedron-from-points 
   
  ;;   (nth pinky-to-fourth-web-post-curve 0) (reverse (nth pinky-to-fourth-web-post-curve 1))
  ;;   (nth fourth-bl-to-br 0) (reverse (nth fourth-bl-to-br 1)) 
  ;;   steps
  ;;  )
  ;; (polyhedron (concat top-layer bottom-layer) 
  ;;             (generate-bezier-along-bezier-polyhedron-faces 
  ;;              top-layer bottom-layer steps))
  (generate-bezier-to-point-polyhedron 
   (reverse (nth pinky-to-fourth-web-post-curve 0))
   fourth-br-top
    (nth pinky-to-fourth-web-post-curve 1)
   fourth-br-bottom
   )
  )
  )

(defn middle-bm-to-fourth-bl-to-middle-br [steps]
  (let [middle-bm-to-fourth-bl 
    (web-post-quadratic-curve (partial key-place 2 cornerrow) "bm" :radians
(partial key-place 2 cornerrow)  "br" :radians
(partial key-place 3 cornerrow)  "bl" :radians
  steps)
        
       middle-br-top (transform-position-radians (partial key-place 2 cornerrow) (web-post-position-top (mapv +  (get-single-plate-corner-position-vector "br"))))
       middle-br-bottom (transform-position-radians (partial key-place 2 cornerrow) (web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "br")))) 
        ]
    (generate-bezier-to-point-polyhedron
     (nth middle-bm-to-fourth-bl 0)
     middle-br-top 
     (reverse (nth middle-bm-to-fourth-bl 1))
     middle-br-bottom
     )
    )
  )

(def right-wall-polyhedron
  (union
   (for [y (range 0 lastrow)] (key-wall-brace-polyhedron lastcol y 1 0 "br" lastcol y 1 0 "tr"))
   (for [y (range 1 lastrow)] (key-wall-brace-polyhedron lastcol y 1 0 "tr" lastcol (dec y) 1 0 "br"))))

(def back-wall-polyhedron
  (union (for [x (range 0 ncols)] (key-wall-brace-polyhedron x 0 0 1 "tr" x 0 0 1 "tl"))
         (for [x (range 1 ncols)] (key-wall-brace-polyhedron x 0 0 1 "tl" (dec x) 0 0 1 "tr"))))

(def front-wall-polyhedron
  (let [pinky-to-fourth-web-post-curve
        (web-post-quadratic-curve (partial key-place 3 cornerrow) "bl" :radians 
                                  (partial key-place 3 cornerrow) "br" :radians
                                  (partial key-place lastcol cornerrow)  "bl" :radians
                                  36)
        fourth-br-top (transform-position-radians (partial key-place 3 cornerrow) (web-post-position-top (mapv +  (get-single-plate-corner-position-vector "br"))))
        fourth-br-bottom (transform-position-radians (partial key-place 3 cornerrow) (web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "br"))))
        fourth-bl-to-br (web-post-linear 
                         (partial key-place 3 cornerrow) "br" :radians
                         (partial key-place 3 cornerrow) "bl" :radians 
                                         36)
        fourth-br-to-pinky-bl (web-post-linear
                               (partial key-place lastcol cornerrow) "bl" :radians
                               (partial key-place 3 cornerrow) "br" :radians
                               18)] 
   (union
    (-# (key-place lastcol cornerrow web-post-br))
    (key-wall-brace-polyhedron lastcol cornerrow 0 -1 "bl" lastcol cornerrow 0 -1 "br")

   (wall-brace-quadratic-polyhedron (partial key-place lastcol cornerrow) -1 0 "bl" :radians
                                    (partial key-place lastcol cornerrow) -1 -1 "bl" :radians
                                    (partial key-place lastcol cornerrow)  0 -1 "bl" :radians
                                    36)
    (pinky-to-fourth-br-bl 72)
   
 
   (wall-brace-quadratic-polyhedron (partial key-place 3 cornerrow) 0 -1 "bl" :radians
                                    (partial key-place 3 cornerrow) 0 -1 "br" :radians
                                    (partial key-place lastcol cornerrow) -1 0 "bl" :radians
                                    36)

   (wall-brace-quadratic-polyhedron (partial key-place 3 cornerrow) -1 0 "bl" :radians
                                    (partial key-place 3 cornerrow) -1 -1 "bl" :radians
                                    (partial key-place 3 cornerrow)  0 -1 "bl" :radians
                                    36)
    (middle-bm-to-fourth-bl-to-middle-br 36)

  ;;  (wall-brace-quadratic-polyhedron (partial key-place 1 cornerrow) 1 0 "br" :radians
  ;;                                   (partial key-place 2 cornerrow) 0 -1 "bl" :radians
  ;;                                   (partial key-place 2 cornerrow) 0 -1 "bm" :radians
  ;;                                   36)
  ;;  (wall-brace-quadratic-polyhedron (partial key-place 2 cornerrow) 0 -1 "bm" :radians
  ;;                                   (partial key-place 2 cornerrow) 0 -1 "br" :radians
  ;;                                   (partial key-place 3 cornerrow) -1 0 "bl" :radians
  ;;                                   36)
  ;;  (wall-brace-quadratic-polyhedron
  ;;   (partial key-place 2 cornerrow) 0 -1 "bl" :radians 
  ;;   (partial key-place 2 cornerrow) 1 -1 "bl" :radians 
  ;;   (partial key-place 2 cornerrow) 1 0 "bl" :radians                                
  ;;   36)
  ;;  (wall-brace-quadratic-polyhedron thumb-tr-place 1 0 "tr" :degrees
  ;;                                   (partial key-place 2 cornerrow) 0 -1 "bl" :radians
  ;;                                   (partial key-place 2 cornerrow) 0 -1 "bm" :radians
  ;;                                   36)
   (wall-brace-quadratic-polyhedron (partial key-place 2 cornerrow) 0 -1 "bm" :radians
                                    (partial key-place 2 cornerrow) -1 -1 "br" :radians
                                    (partial key-place 3 cornerrow) -1 0 "bl" :radians
                                    36)

   (key-wall-brace-polyhedron 1 cornerrow 0 -1 "br" 2 cornerrow 0 -1 "bm")
  ;;  (wall-brace-quadratic-polyhedron
  ;;   (partial key-place 1 cornerrow) 1  0 "br" :radians
  ;;   (partial key-place 2 cornerrow) 1 -1 "bl" :radians
  ;;   (partial key-place 2 cornerrow) 0 -1 "bm" :radians
  ;;   36)
  ;;  (wall-brace-cubic-polyhedron {:place1 (partial key-place 1 cornerrow) :dx1 1 :dy1 0  :post-position-1 "br" :rad-or-deg1 :radians
  ;;                                       :place-mid1 (partial key-place 2 cornerrow) :dxmid1 0 :dymid1 -1 :post-position-mid1 "bl" :rad-or-degmid1 :radians
  ;;                                       :place-mid2 (partial key-place 2 cornerrow) :dxmid2 0 :dymid2 -1  :post-position-mid2 "br" :rad-or-degmid2 :radians
  ;;                                       :place2 (partial key-place 3 cornerrow) :dx2 -1 :dy2 0  :post-position-2 "bl" :rad-or-deg2 :radians 
  ;;                                :steps 36})
   )))

(def thumb-walls-polyhedron
  (union
   (thumb-wall-brace-polyhedron  thumb-tr-place 1 0 "br" thumb-tr-place  1 0 "tr")

   ;;  (wall-brace-quadratic-polyhedron thumb-tr-place 1 0 "br" :degrees
  ;;                                   thumb-tr-place 1 0 "tr" :degrees
  ;;                         (partial key-place 1 cornerrow) 0.5 -1 "br" :radians
  ;;                         36)
   (thumb-wall-brace-polyhedron  thumb-mr-place  0 -1 "bl" thumb-mr-place  0 -1 "br")
   (thumb-wall-brace-polyhedron thumb-br-place  0 -1 "bl" thumb-br-place  0 -1 "br")
;(wall-brace thumb-bl-place  0  1 oled-post-tr thumb-bl-place  0  1 oled-post-tl)
   (thumb-wall-brace-polyhedron thumb-br-place -1  0 "tl" thumb-br-place -1  0 "bl")
   (thumb-wall-brace-polyhedron thumb-bl-place -1  0 "tl" thumb-bl-place -1  0 "bl")))

(def thumb-corners-polyhedron
  (union

   (wall-brace-quadratic-polyhedron (partial thumb-br-place) -1 0 "bl" :degrees
                                    (partial thumb-br-place) -1 -1 "bl" :degrees
                                    (partial thumb-br-place) 0 -1 "bl" :degrees
                                    36)
   (wall-brace-quadratic-polyhedron
    (partial thumb-mr-place) 0 -1 "br" :degrees
    (partial thumb-mr-place) 1 -1 "br" :degrees
    (partial thumb-mr-place) 1 0 "br" :degrees
    36)

   (wall-brace-quadratic-polyhedron
    (partial thumb-mr-place) 1 0 "br" :degrees
    (partial thumb-mr-place) 1 0 "tr" :degrees
    (partial thumb-tr-place) 0 -1 "br" :degrees
    36)

   (wall-brace-quadratic-polyhedron
    (partial thumb-tr-place) 0 -1 "br" :degrees
    (partial thumb-tr-place) 1 -1 "br" :degrees
    (partial thumb-tr-place) 1 0 "br" :degrees
    36)
   
   
   
   ;(thumb-wall-brace-polyhedron thumb-tr-place  0 0 "tr" thumb-tr-place  1 0 "tr")
  
   ;;  (wall-brace-quadratic-polyhedron
  ;;   (partial thumb-tr-place) 0  1 "tr" :degrees
  ;;   (partial thumb-tr-place) 1 1 "tr" :degrees
  ;;   (partial thumb-tr-place) 1 0 "tr" :degrees
  ;;   36)
   ))

(def thumb-tweeners-polyhedron
  (union
   (thumb-wall-brace-polyhedron thumb-br-place  0 -1 "br" thumb-mr-place  0 -1 "bl")
   (thumb-wall-brace-polyhedron thumb-bl-place -1  0 "bl" thumb-br-place -1  0 "tl")))

(defn thumb-connecters-polyhedron [steps]
  (let [thumb-mr-tr-top (transform-position thumb-mr-place (web-post-position-top (mapv +  (get-single-plate-corner-position-vector "tr"))))
        thumb-mr-tr-bottom (transform-position thumb-mr-place (web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "tr")))) 
        thumb-mr-br-to-thumb-tr-br-curve (web-post-quadratic-curve
                                          thumb-mr-place "br" :degrees
                                          thumb-mr-place "tr" :degrees
                                          thumb-tr-place "br" :degrees
                                          steps 
                                          )
  ](union 
   (generate-bezier-to-point-polyhedron
    (nth thumb-mr-br-to-thumb-tr-br-curve 0)
    thumb-mr-tr-top
    (reverse (nth thumb-mr-br-to-thumb-tr-br-curve 1))
    thumb-mr-tr-bottom
    )
   
   ))
  )
(defn right-side-polyhedron [steps]
  (let [top-row-web-post-tl-top (key-position 0 0 (mapv + web-post-translation-vector web-post-tl-translation-vector [0 0 (/ web-thickness 2)]))
        top-row-web-post-bl-top (key-position 0 0 (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness 2)]))

        middle-row-web-post-tl-top (key-position 0 1 (mapv + web-post-translation-vector web-post-tl-translation-vector [0 0 (/ web-thickness 2)]))
        middle-row-web-post-bl-top (key-position 0 1 (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness 2)]))

        third-row-web-post-tl-top (key-position 0 2 (mapv + web-post-translation-vector web-post-tl-translation-vector [0 0 (/ web-thickness 2)]))
        third-row-web-post-bl-top (key-position 0 2 (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness 2)]))

        top-row-web-post-tl-bottom (key-position 0 0 (mapv + web-post-translation-vector web-post-tl-translation-vector [0 0 (/ web-thickness -2)]))
        top-row-web-post-bl-bottom (key-position 0 0 (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness -2)]))

        middle-row-web-post-tl-bottom (key-position 0 1 (mapv + web-post-translation-vector web-post-tl-translation-vector [0 0 (/ web-thickness -2)]))
        middle-row-web-post-bl-bottom (key-position 0 1 (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness -2)]))

        third-row-web-post-tl-bottom (key-position 0 2 (mapv + web-post-translation-vector web-post-tl-translation-vector [0 0 (/ web-thickness -2)]))
        third-row-web-post-bl-bottom (key-position 0 2 (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness -2)]))
        number-of-slices 14
        total-width (+ tps-65-width tps-65-tolerance)
        max-width-from-zero (/ total-width 2)
        tps-65-width-slice  (/ total-width number-of-slices)
        position-adjustment (fn [upper-or-lower] (mapv + oled-translation-vector
                                                       [0 (+ (tps-radius-compensation-adjust (- tps-65-mount-corner-radius)) (/ oled-post-size 2) -0.1) (/ oled-holder-thickness (cond (= upper-or-lower :upper) 2
                                                                                                                                                                                       (= upper-or-lower :lower) -2))]))

        tps-65-point-to-connect-to-top-row-web-post-tl-top (transform-position (partial tps-65-translate-and-place-at-position [max-width-from-zero
                                                                                                                                (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                (- plate-thickness)])
                                                                               (position-adjustment :upper))
        tps-65-point-to-connect-to-top-row-web-post-bl-top (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 4))
                                                                                                                                (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                (- plate-thickness)])
                                                                               (position-adjustment :upper))
        tps-65-point-to-connect-to-middle-row-web-post-tl-top (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 5))
                                                                                                                                   (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                   (- plate-thickness)]) (position-adjustment :upper))
        tps-65-point-to-connect-to-middle-row-web-post-bl-top (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 6))
                                                                                                                                   (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                   (- plate-thickness)]) (position-adjustment :upper))
        tps-65-point-to-connect-to-third-row-web-post-tl-top (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 10))
                                                                                                                                  (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                  (- plate-thickness)]) (position-adjustment :upper))
        tps-65-point-to-connect-to-third-row-web-post-bl-top (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 14))
                                                                                                                                  (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                  (- plate-thickness)]) (position-adjustment :upper))

        tps-65-point-to-connect-to-top-row-web-post-tl-bottom (transform-position (partial tps-65-translate-and-place-at-position [max-width-from-zero
                                                                                                                                   (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                   (- plate-thickness)]) (position-adjustment :lower))
        tps-65-point-to-connect-to-top-row-web-post-bl-bottom (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 4))
                                                                                                                                   (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                   (- plate-thickness)]) (position-adjustment :lower))
        tps-65-point-to-connect-to-middle-row-web-post-tl-bottom (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 5))
                                                                                                                                      (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                      (- plate-thickness)]) (position-adjustment :lower))
        tps-65-point-to-connect-to-middle-row-web-post-bl-bottom (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 6))
                                                                                                                                      (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                      (- plate-thickness)]) (position-adjustment :lower))
        tps-65-point-to-connect-to-third-row-web-post-tl-bottom (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 10))
                                                                                                                                     (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                     (- plate-thickness)]) (position-adjustment :lower))
        tps-65-point-to-connect-to-third-row-web-post-bl-bottom (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 14))
                                                                                                                                     (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                     (- plate-thickness)]) (position-adjustment :lower))

        thumb-tl-tl-web-post-top (transform-position (partial thumb-tl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-tl-tl-web-post-bottom (transform-position (partial thumb-tl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        thumb-tl-tr-web-post-top (transform-position (partial thumb-tl-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-tl-tr-web-post-bottom (transform-position (partial thumb-tl-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))

        tps-65-to-top-row (generate-bezier-quadratic-polyhedron-from-points-and-control-vectors
                           tps-65-point-to-connect-to-top-row-web-post-tl-top top-row-web-post-tl-top
                           tps-65-point-to-connect-to-top-row-web-post-bl-top top-row-web-post-bl-top
                           tps-65-point-to-connect-to-top-row-web-post-tl-bottom top-row-web-post-tl-bottom
                           tps-65-point-to-connect-to-top-row-web-post-bl-bottom top-row-web-post-bl-bottom
                           steps
                           {:outside-upper-control-point-vector [0 0 -1] :outside-lower-control-point-vector [0 0 -1]
                            :inside-upper-control-point-vector [0 0 -1] :inside-lower-control-point-vector [0 0 -1]})
        tps-65-to-top-and-middle-row (generate-bezier-quadratic-polyhedron-from-points-and-control-vectors
                                      tps-65-point-to-connect-to-top-row-web-post-bl-top top-row-web-post-bl-top
                                      tps-65-point-to-connect-to-middle-row-web-post-tl-top middle-row-web-post-tl-top
                                      tps-65-point-to-connect-to-top-row-web-post-bl-bottom top-row-web-post-bl-bottom
                                      tps-65-point-to-connect-to-middle-row-web-post-tl-bottom middle-row-web-post-tl-bottom
                                      steps
                                      {:outside-upper-control-point-vector [0 0 -1] :outside-lower-control-point-vector [0 0 -1]
                                       :inside-upper-control-point-vector [0 0 -1] :inside-lower-control-point-vector [0 0 -1]})

        tps-65-to-middle-row (generate-bezier-quadratic-polyhedron-from-points-and-control-vectors
                              tps-65-point-to-connect-to-middle-row-web-post-tl-top middle-row-web-post-tl-top
                              tps-65-point-to-connect-to-middle-row-web-post-bl-top middle-row-web-post-bl-top
                              tps-65-point-to-connect-to-middle-row-web-post-tl-bottom middle-row-web-post-tl-bottom
                              tps-65-point-to-connect-to-middle-row-web-post-bl-bottom middle-row-web-post-bl-bottom
                              steps
                              {:outside-upper-control-point-vector [0 0 -1] :outside-lower-control-point-vector [0 0 -1]
                               :inside-upper-control-point-vector [0 0 -1] :inside-lower-control-point-vector [0 0 -1]})
        tps-65-to-middle-and-third-row (generate-bezier-quadratic-polyhedron-from-points-and-control-vectors
                                        tps-65-point-to-connect-to-middle-row-web-post-bl-top middle-row-web-post-bl-top
                                        tps-65-point-to-connect-to-third-row-web-post-tl-top third-row-web-post-tl-top
                                        tps-65-point-to-connect-to-middle-row-web-post-bl-bottom middle-row-web-post-bl-bottom
                                        tps-65-point-to-connect-to-third-row-web-post-tl-bottom third-row-web-post-tl-bottom
                                        steps
                                        {:outside-upper-control-point-vector [0 0 -1] :outside-lower-control-point-vector [0 0 -1]
                                         :inside-upper-control-point-vector [0 0 -1] :inside-lower-control-point-vector [0 0 -1]})

        tps-65-to-third-row (generate-bezier-quadratic-polyhedron-from-points-and-control-vectors
                             tps-65-point-to-connect-to-third-row-web-post-tl-top third-row-web-post-tl-top
                             tps-65-point-to-connect-to-third-row-web-post-bl-top third-row-web-post-bl-top
                             tps-65-point-to-connect-to-third-row-web-post-tl-bottom third-row-web-post-tl-bottom
                             tps-65-point-to-connect-to-third-row-web-post-bl-bottom third-row-web-post-bl-bottom
                             steps
                             {:outside-upper-control-point-vector [0 0 -1] :outside-lower-control-point-vector [0 0 -1]
                              :inside-upper-control-point-vector [0 0 -1] :inside-lower-control-point-vector [0 0 -1]})

        tps-65-to-third-row-and-thumb-tl-tl (generate-bezier-quadratic-polyhedron-from-points-and-control-vectors
                                             tps-65-point-to-connect-to-third-row-web-post-bl-top third-row-web-post-bl-top
                                             thumb-tl-tl-web-post-top thumb-tl-tr-web-post-top
                                             tps-65-point-to-connect-to-third-row-web-post-bl-bottom third-row-web-post-bl-bottom
                                             thumb-tl-tl-web-post-bottom thumb-tl-tr-web-post-bottom
                                             steps
                                             {:outside-upper-control-point-vector [0 0 -1] :outside-lower-control-point-vector [0 0 -1]
                                              :inside-upper-control-point-vector [0 0 0] :inside-lower-control-point-vector [0 0 0]})]
    (union
     tps-65-to-top-row
     tps-65-to-top-and-middle-row
     tps-65-to-middle-row
     tps-65-to-middle-and-third-row
     tps-65-to-third-row
     tps-65-to-third-row-and-thumb-tl-tl)))