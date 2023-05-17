(ns dactyl-keyboard.klor.klor-case-walls
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [mul]]
            [clojure.math :refer [sqrt]]
            [dactyl-keyboard.klor.klor-case-functions :refer :all]
            [dactyl-keyboard.klor.klor-config :refer :all]
            [dactyl-keyboard.klor.klor-placement-functions :refer :all]
            [dactyl-keyboard.klor.klor-points :refer :all]
            [dactyl-keyboard.tps-65 :refer [tps-65-mount-new
                                            tps-65
                                            tps-65-mount-cutout
                                            tps-65-mount-main-cutout]]
            [dactyl-keyboard.tps-43 :refer :all]
            [dactyl-keyboard.lib.curvesandsplines.beziers :refer [n-degree-bezier-curve
                                                                  bezier-linear-spline]]
            [dactyl-keyboard.lib.curvesandsplines.curve-fitting :refer [global-curve-interp-with-calculated-first-derivatives-curve
                                                                        global-curve-interp-with-end-unit-derivatives-curve local-cubic-curve-interpolation-with-tangents-curve]]
            [dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline :refer [nurbs nurbs-with-calculated-knot-vector]]
            [dactyl-keyboard.lib.curvesandsplines.splines :refer [catmull-rom-spline-curve]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [include-bosl2]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vnf :refer [vnf-polyhedron
                                                                     vnf-vertex-array]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.shapes-3d :refer [cuboid]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer :all]
            [dactyl-keyboard.lib.transformations :refer [rdx rdy rdz ]]
            [dactyl-keyboard.low.case-low-polyhedron-functions :refer [wall-vnf-array]]
            [dactyl-keyboard.oled :refer [oled-holder-width]]
            [dactyl-keyboard.utils :refer [plot-bezier-points select-values]]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all])
  )

(defn klor-back-wall [wall-cross-section-steps wall-section-steps]
  (let [points (mapv klor-wall-control-points-from-map
                     [pcb-top-right-corner-point-north-east
                      pcb-top-right-corner-point-north
                      middle-top-tr-inset
                      middle-top-tr-north
                      middle-top-tl-north
                      middle-top-tl-inset
                      oled-holder-tr-inset
                      oled-holder-tr-north
                      oled-holder-tl-north])
        n (dec (count points))
        segments (- (count points) 3)
        total-wall-section-steps (* wall-section-steps segments)
        outer-vertical-curves (mapv #(klor-outer-wall-vertical-nurbs % wall-section-steps) points)
        inner-vertical-curves (mapv #(klor-inner-wall-vertical-linear % wall-section-steps) points)
        outer-wall-points (vec (for [index (range (inc wall-cross-section-steps))
                                :let [catmull-points (vec (for [inner-index (range (inc n))] (get-in outer-vertical-curves [inner-index index])))]]
                            (catmull-rom-spline-curve catmull-points total-wall-section-steps)))
        inner-wall-points (mapv reverse (vec (for [index (range (inc wall-cross-section-steps))
                                :let [catmull-points (vec (for [inner-index (range (inc n))] (get-in inner-vertical-curves [inner-index index])))]]
                            (catmull-rom-spline-curve catmull-points total-wall-section-steps))))
        nurbs-points (mapv klor-wall-control-points-from-map
                           [oled-holder-tr-north
                            oled-holder-tm-north
                            oled-holder-tl-north
                            oled-holder-tl-north-west
                            oled-holder-tl-west
                            oled-holder-bl-inset
                            tps-43-tl-north
                            tps-43-tl-north-west
                            tps-43-tl-west]) 
        nurbs-outer-vertical-curves (mapv #(klor-outer-wall-vertical-nurbs % wall-section-steps) nurbs-points)
        nurbs-inner-vertical-curves (mapv #(klor-inner-wall-vertical-linear % wall-section-steps) nurbs-points)
        nurbs-degree 2
        nurbs-n (dec (count nurbs-points))
        nurbs-total-wall-section-steps (* wall-section-steps nurbs-n)
        circ-weight (/ (sqrt 2) 2)
        weights [1 1
                 1 circ-weight 1
                 (sqrt 2)
                 1 circ-weight 1]
        knot-vector (let [denom nurbs-n]
                      (mul (dec denom)
                           [0 0 0 (/ 2 denom) (/ 2 denom)
                            (/ 4 denom) (/ 4 denom)
                            (/ 6 denom) (/ 6 denom)
                        ;    (/ 8 denom) (/ 8 denom)
                        ;    (/ 10 denom) (/ 10 denom)
                        ;    (/ 12 denom) (/ 12 denom)
                        
                         ;   (/ 14 denom) (/ 14 denom)
                        ;    (/ 16 denom) (/ 16 denom)
                            1 1 1]))
nurbs-outer-wall (vec (for [index (range (inc wall-cross-section-steps))
                        :let [points (vec (for [inner-index (range (inc nurbs-n))] (get-in nurbs-outer-vertical-curves [inner-index index])))]]
                    (nurbs points 2 knot-vector weights nurbs-total-wall-section-steps)))
        nurbs-inner-wall (mapv reverse (vec (for [index (range (inc wall-cross-section-steps))
                               :let [points (vec (for [inner-index (range (inc nurbs-n))] (get-in nurbs-inner-vertical-curves [inner-index index])))]]
                           (nurbs points 2 knot-vector weights nurbs-total-wall-section-steps))))
        global-outer-wall (vec (for [index (range (inc wall-cross-section-steps))
                                     :let [points (vec (for [inner-index (range (inc nurbs-n))] (get-in nurbs-outer-vertical-curves [inner-index index])))]]
                                 (nurbs points 2 knot-vector weights nurbs-total-wall-section-steps)))
        ]
    (union (vnf-polyhedron (wall-vnf-array outer-wall-points inner-wall-points))
           (vnf-polyhedron (wall-vnf-array nurbs-outer-wall nurbs-inner-wall )))) 
  )

(defn get-floor-points [points]
  (->>
   (mapv (fn [points-list] (filter #(zero? (nth % 2)) points-list)) points)
   (apply concat)
   (vec)) 
  )

(defn klor-back-wall-2 [wall-cross-section-steps wall-section-steps]
(let [pcb-top-right-corner-point-north-east-control-points (klor-wall-control-points-from-map pcb-top-right-corner-point-north-east)
      pcb-top-right-corner-point-north-control-points (klor-wall-control-points-from-map                       pcb-top-right-corner-point-north)
      middle-top-tr-inset-control-points (klor-wall-control-points-from-map   middle-top-tr-inset)
      middle-top-tr-north-control-points (klor-wall-control-points-from-map   middle-top-tr-north)
      middle-top-tr-north-east-control-points (klor-wall-control-points-from-map   middle-top-tr-north-east)
      middle-top-tl-north-control-points (klor-wall-control-points-from-map   middle-top-tl-north)
      middle-top-tl-inset-control-points (klor-wall-control-points-from-map   middle-top-tl-inset)
      inner-index-top-tr-control-points (klor-wall-control-points-from-map inner-index-top-tr)
      oled-holder-tr-inset-control-points (klor-wall-control-points-from-map   oled-holder-tr-inset)
      oled-holder-tr-north-control-points (klor-wall-control-points-from-map   oled-holder-tr-north)
      oled-holder-tr-north-east-control-points (klor-wall-control-points-from-map   oled-holder-tr-north-east)
      oled-holder-tm-north-control-points (klor-wall-control-points-from-map oled-holder-tm-north)
      oled-holder-tl-north-control-points (klor-wall-control-points-from-map   oled-holder-tl-north)
      oled-holder-tl-north-west-control-points (klor-wall-control-points-from-map   oled-holder-tl-north-west)
      oled-holder-tl-west-control-points (klor-wall-control-points-from-map oled-holder-tl-west)
      oled-holder-bl-inset-control-points (klor-wall-control-points-from-map oled-holder-bl-inset)
      oled-holder-bl-control-points (klor-wall-control-points-from-map oled-holder-bl-west)
      
      tps-43-tl-north-control-points (klor-wall-control-points-from-map tps-43-tl-north)
      ;pcb-top-right-corner-point-north-east-control-points (klor-wall-control-points-from-map pcb-top-right-corner-point-north-east)
      ;pcb-top-right-corner-point-north-control-points (klor-wall-control-points-from-map pcb-top-right-corner-point-north)
      pcb-top-right-corner-point-north-outer-curve (klor-outer-wall-vertical-nurbs pcb-top-right-corner-point-north-control-points wall-cross-section-steps)
      pcb-top-right-corner-point-north-east-outer-curve (klor-outer-wall-vertical-nurbs  pcb-top-right-corner-point-north-east-control-points wall-cross-section-steps)
      middle-top-tr-inset-outer-curve (klor-outer-wall-vertical-nurbs middle-top-tr-inset-control-points wall-cross-section-steps)
      middle-top-tr-north-east-outer-curve (klor-outer-wall-vertical-nurbs middle-top-tr-north-east-control-points wall-cross-section-steps)
      middle-top-tr-north-outer-curve (klor-outer-wall-vertical-nurbs middle-top-tr-north-control-points wall-cross-section-steps)
      middle-top-tl-north-outer-curve (klor-outer-wall-vertical-nurbs middle-top-tl-north-control-points wall-cross-section-steps)
      middle-top-tl-inset-outer-curve (klor-outer-wall-vertical-nurbs middle-top-tl-inset-control-points wall-cross-section-steps)
      
      oled-holder-tr-inset-outer-curve (klor-outer-wall-vertical-nurbs oled-holder-tr-inset-control-points wall-cross-section-steps)
      oled-holder-tr-north-outer-curve (klor-outer-wall-vertical-nurbs oled-holder-tr-north-control-points wall-cross-section-steps)
      oled-holder-tr-north-east-outer-curve (klor-outer-wall-vertical-nurbs oled-holder-tr-north-east-control-points wall-cross-section-steps)
      oled-holder-tm-north-outer-curve (klor-outer-wall-vertical-nurbs oled-holder-tm-north-control-points wall-cross-section-steps)
      oled-holder-tl-north-outer-curve (klor-outer-wall-vertical-nurbs oled-holder-tl-north-control-points wall-cross-section-steps)
      oled-holder-tl-north-west-outer-curve (klor-outer-wall-vertical-nurbs oled-holder-tl-north-west-control-points wall-cross-section-steps)
      oled-holder-tl-west-outer-curve (klor-outer-wall-vertical-nurbs oled-holder-tl-west-control-points wall-cross-section-steps)
      oled-holder-bl-inset-outer-curve (klor-outer-wall-vertical-nurbs oled-holder-bl-inset-control-points wall-cross-section-steps)
      oled-holder-bl-outer-curve (klor-outer-wall-vertical-nurbs oled-holder-bl-control-points wall-cross-section-steps)
      tps-43-tl-north-outer-curve (klor-outer-wall-vertical-nurbs tps-43-tl-north-control-points wall-cross-section-steps)
      
      pcb-top-right-corner-point-north-inner-curve (klor-inner-wall-vertical-linear pcb-top-right-corner-point-north-control-points wall-cross-section-steps)
      pcb-top-right-corner-point-north-east-inner-curve (klor-inner-wall-vertical-linear  pcb-top-right-corner-point-north-east-control-points wall-cross-section-steps)
      middle-top-tr-inset-inner-curve (klor-inner-wall-vertical-linear middle-top-tr-inset-control-points wall-cross-section-steps)
      middle-top-tr-north-inner-curve (klor-inner-wall-vertical-linear middle-top-tr-north-control-points wall-cross-section-steps)
      middle-top-tl-north-inner-curve (klor-inner-wall-vertical-linear middle-top-tl-north-control-points wall-cross-section-steps)
      middle-top-tl-inset-inner-curve (klor-inner-wall-vertical-linear middle-top-tl-inset-control-points wall-cross-section-steps)
      oled-holder-tr-inset-inner-curve (klor-inner-wall-vertical-linear oled-holder-tr-inset-control-points wall-cross-section-steps)
      middle-top-tr-north-east-inner-curve (klor-inner-wall-vertical-linear middle-top-tr-north-east-control-points wall-cross-section-steps)
      oled-holder-tr-north-inner-curve (klor-inner-wall-vertical-linear oled-holder-tr-north-control-points wall-cross-section-steps)
      oled-holder-tr-north-east-inner-curve (klor-inner-wall-vertical-linear oled-holder-tr-north-east-control-points wall-cross-section-steps)
      oled-holder-tm-north-inner-curve (klor-inner-wall-vertical-linear oled-holder-tm-north-control-points wall-cross-section-steps)
      oled-holder-tl-north-inner-curve (klor-inner-wall-vertical-linear oled-holder-tl-north-control-points wall-cross-section-steps)
      oled-holder-tl-north-west-inner-curve (klor-inner-wall-vertical-linear oled-holder-tl-north-west-control-points wall-cross-section-steps) 
      oled-holder-tl-west-inner-curve (klor-inner-wall-vertical-linear oled-holder-tl-west-control-points wall-cross-section-steps)
      oled-holder-bl-inset-inner-curve (klor-inner-wall-vertical-linear oled-holder-bl-inset-control-points wall-cross-section-steps)
      oled-holder-bl-inner-curve (klor-inner-wall-vertical-linear oled-holder-bl-control-points wall-cross-section-steps)
      tps-43-tl-north-inner-curve (klor-inner-wall-vertical-linear tps-43-tl-north-control-points wall-cross-section-steps)
      total-wall-section-steps (* wall-section-steps 15)
      outer-wall-points (vec (for [index (range (inc wall-cross-section-steps))]
                               (global-curve-interp-with-calculated-first-derivatives-curve
                                (mapv #(nth % index) [pcb-top-right-corner-point-north-outer-curve
                                                      middle-top-tr-inset-outer-curve
                                                      middle-top-tr-north-outer-curve
                                                      middle-top-tl-north-outer-curve
                                                      middle-top-tl-inset-outer-curve
                                                      oled-holder-tr-inset-outer-curve
                                                      oled-holder-tr-north-outer-curve
                                                      oled-holder-tm-north-outer-curve
                                                      oled-holder-tl-north-outer-curve
                                                      oled-holder-tl-west-outer-curve
                                                      ;oled-holder-bl-inset-outer-curve
                                                      ])
                                (mapv #(mapv - (nth (nth % 0) index) (nth (nth % 1) index))
                                      [[pcb-top-right-corner-point-north-outer-curve pcb-top-right-corner-point-north-east-outer-curve]
                                       [middle-top-tr-inset-outer-curve pcb-top-right-corner-point-north-outer-curve]
                                       [middle-top-tl-north-outer-curve middle-top-tr-north-east-outer-curve]
                                       [middle-top-tl-north-outer-curve middle-top-tr-north-outer-curve]
                                       [oled-holder-tr-inset-outer-curve middle-top-tl-inset-outer-curve]
                                       [oled-holder-tr-north-outer-curve oled-holder-tr-north-east-outer-curve ]
                                       [oled-holder-tl-north-outer-curve oled-holder-tm-north-outer-curve]
                                       [oled-holder-tl-north-outer-curve oled-holder-tm-north-outer-curve]
                                       [oled-holder-tl-north-west-outer-curve oled-holder-tl-north-outer-curve]
                                       [oled-holder-bl-inset-outer-curve oled-holder-tl-west-outer-curve]
                                       ;[tps-43-tl-north-outer-curve oled-holder-bl-inset-outer-curve]
                                       ])
                                2
                                total-wall-section-steps
                                :magnitude-estimation-method :chord
                                :point-paramater-calculation-method :chordal)))
      inner-wall-points (mapv #(vec (reverse %)) (vec (for [index (range (inc wall-cross-section-steps))]
                              ;;  (global-curve-interp-with-calculated-first-derivatives-curve
                              ;;   (mapv #(nth % index) [;oled-holder-bl-inset-inner-curve
                              ;;                         oled-holder-tl-west-inner-curve
                              ;;                         oled-holder-tl-north-inner-curve
                              ;;                         oled-holder-tr-north-inner-curve
                              ;;                         oled-holder-tr-inset-inner-curve
                              ;;                         middle-top-tl-inset-inner-curve 
                              ;;                         middle-top-tl-north-inner-curve 
                              ;;                         middle-top-tr-north-inner-curve
                              ;;                         middle-top-tr-inset-inner-curve
                              ;;                         pcb-top-right-corner-point-north-inner-curve])
                              ;;   (mapv #(mapv - (nth (nth % 0) index) (nth (nth % 1) index))
                              ;;         [;[oled-holder-bl-inset-inner-curve tps-43-tl-north-inner-curve]
                              ;;          [oled-holder-tl-west-inner-curve oled-holder-bl-inset-inner-curve]
                              ;;          [oled-holder-tl-north-inner-curve oled-holder-tl-north-west-inner-curve]
                              ;;          [oled-holder-tr-north-inner-curve oled-holder-tl-north-inner-curve]
                              ;;          [oled-holder-tr-north-inner-curve oled-holder-tl-north-inner-curve]
                              ;;          [middle-top-tl-inset-inner-curve oled-holder-tr-inset-inner-curve]
                              ;;          [middle-top-tr-north-inner-curve middle-top-tl-north-inner-curve]
                              ;;          [middle-top-tr-north-inner-curve middle-top-tl-north-inner-curve]
                              ;;          [pcb-top-right-corner-point-north-inner-curve middle-top-tr-inset-inner-curve] 
                              ;;          [pcb-top-right-corner-point-north-east-inner-curve pcb-top-right-corner-point-north-inner-curve]])
                              ;;   2
                              ;;   total-wall-section-steps
                              ;;   :magnitude-estimation-method :chord
                              ;;   :point-paramater-calculation-method :chordal)
                               (global-curve-interp-with-calculated-first-derivatives-curve
                                (mapv #(nth % index) [pcb-top-right-corner-point-north-inner-curve
                                                      middle-top-tr-inset-inner-curve
                                                      middle-top-tr-north-inner-curve
                                                      middle-top-tl-north-inner-curve
                                                      middle-top-tl-inset-inner-curve
                                                      oled-holder-tr-inset-inner-curve
                                                      oled-holder-tr-north-inner-curve
                                                      oled-holder-tm-north-inner-curve
                                                      oled-holder-tl-north-inner-curve
                                                      oled-holder-tl-west-inner-curve
                                                      ;oled-holder-bl-inset-inner-curve
                                                      ])
                                (mapv #(mapv - (nth (nth % 0) index) (nth (nth % 1) index))
                                      [[pcb-top-right-corner-point-north-inner-curve pcb-top-right-corner-point-north-east-inner-curve]
                                       [middle-top-tr-inset-inner-curve pcb-top-right-corner-point-north-inner-curve]
                                       [middle-top-tl-north-inner-curve middle-top-tr-north-east-inner-curve]
                                       [middle-top-tl-north-inner-curve middle-top-tr-north-inner-curve]
                                       [oled-holder-tr-inset-inner-curve middle-top-tl-inset-inner-curve]
                                       [oled-holder-tr-north-inner-curve oled-holder-tr-north-east-inner-curve]
                                       [oled-holder-tl-north-inner-curve oled-holder-tm-north-inner-curve]
                                       [oled-holder-tl-north-inner-curve oled-holder-tm-north-inner-curve]
                                       [oled-holder-tl-north-west-inner-curve oled-holder-tl-north-inner-curve]
                                       [oled-holder-bl-inset-inner-curve oled-holder-tl-west-inner-curve]
                                       ;[tps-43-tl-north-inner-curve oled-holder-bl-inset-inner-curve]
                                       ])
                                2
                                total-wall-section-steps
                                :magnitude-estimation-method :chord
                                :point-paramater-calculation-method :chordal)
                                )))] 
                                       {:wall (vnf-polyhedron (wall-vnf-array outer-wall-points  inner-wall-points
                                                                              :args {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :alt}))
                                        :outer-wall-floor-points (get-floor-points outer-wall-points)
                                        :inner-wall-floor-points (get-floor-points inner-wall-points)} 
                                       ) 
                  )

(defn klor-pinky-wall [wall-cross-section-steps wall-section-steps]
  (let [points (mapv klor-wall-control-points-from-map
                     [outer-pinky-bottom-br-south
                      outer-pinky-bottom-br-south-east
                      outer-pinky-bottom-br-east
                      outer-pinky-top-br
                      outer-pinky-top-tr-east
                      outer-pinky-top-tr-north-east
                      outer-pinky-top-tr-north
                      outer-pinky-top-tl
                      pinky-top-tr-east
                      pinky-top-tr-north-east
                      pinky-top-tr-north
                      ;pinky-top-tr-inset
                      pinky-top-tm-north-east
                      pcb-top-right-corner-point-east
                      pcb-top-right-corner-point-north-east
                      pcb-top-right-corner-point-north
                     ; middle-top-tr-inset
                     ; middle-top-tr-east
                      ;middle-top-tr-north-east
                      ;middle-top-tr-north
                      ])
        n (dec (count points))
        nurbs-degree 2
        circ-weight (/ (sqrt 2) 2)
        weights [1 circ-weight 1
                 1
                 1 circ-weight 1
                 (* (sqrt 2) 1.5)
                 1 circ-weight 1
                 circ-weight 1
                 circ-weight 1
                ; circ-weight 1
                 ;circ-weight 1
                 ]
        knot-vector (let [denom n]
                      (mul (dec denom)
                           [0 0 0 (/ 2 denom) (/ 2 denom)
                            (/ 4 denom) (/ 4 denom)
                            (/ 6 denom) (/ 6 denom)
                            (/ 8 denom) (/ 8 denom)
                            (/ 10 denom) (/ 10 denom)
                            (/ 12 denom) (/ 12 denom)
                         ;   (/ 14 denom) (/ 14 denom)
                        ;    (/ 16 denom) (/ 16 denom)
                            1 1 1]))
        total-wall-section-steps (* (- (inc n) (inc nurbs-degree)) wall-section-steps)
        outer-vertical-curves (mapv #(klor-outer-wall-vertical-nurbs % wall-cross-section-steps) points)
        inner-vertical-curves (mapv #(klor-inner-wall-vertical-linear % wall-cross-section-steps) points)
        outer-wall-points (vec (for [index (range (inc wall-cross-section-steps))
                                     :let [points (vec (for [inner-index (range (inc n))] (get-in outer-vertical-curves [inner-index index])))]]
                                 (nurbs points 2 knot-vector weights total-wall-section-steps)))
        inner-wall-points (mapv reverse (vec (for [index (range (inc wall-cross-section-steps))
                                                   :let [points (vec (for [inner-index (range (inc n))] (get-in inner-vertical-curves [inner-index index])))]]
                                               (nurbs points 2 knot-vector weights total-wall-section-steps))))
        outer-floor-points (get-floor-points outer-wall-points)
        inner-floor-points (get-floor-points inner-wall-points)]
    {:wall (vnf-polyhedron (wall-vnf-array outer-wall-points inner-wall-points))
     :outer-wall-floor-points outer-floor-points
     :inner-wall-floor-points inner-floor-points} 
    )
  )

(comment (klor-thumb-position 2 (mapv + (klor-spacing :bl) (get-position-offset :south) [0 -3.25 0])))
(comment (:wall-top-outer  (klor-wall-control-points-from-map mid-left-thumb-bl-south)))
 (defn klor-thumb-wall [wall-cross-section-steps wall-section-steps]
   (let [points (mapv klor-wall-control-points-from-map [outer-thumb-bl-south
                                                         outer-thumb-br-south
                 ;mid-left-thumb-bl-south
                                                         mid-left-thumb-br-south
                 ;mid-right-thumb-bl-south
                                                         mid-right-thumb-br-south
                 ;inner-thumb-bl-south
                                                         inner-thumb-br-south])
         outer-thumb-bl-south-west-control-points (klor-wall-control-points-from-map outer-thumb-bl-south-west)
         outer-thumb-bl-west-outer-curve (klor-outer-wall-vertical-nurbs outer-thumb-bl-south-west-control-points wall-cross-section-steps)
         outer-thumb-bl-west-inner-curve (klor-inner-wall-vertical-linear outer-thumb-bl-south-west-control-points wall-cross-section-steps)
         outer-thumb-bl-west-control-points (klor-wall-control-points-from-map outer-thumb-bl-south-west)
         outer-thumb-bl-south-west-outer-curve (klor-outer-wall-vertical-nurbs outer-thumb-bl-south-west-control-points wall-cross-section-steps)
         outer-thumb-bl-south-west-inner-curve (klor-inner-wall-vertical-linear outer-thumb-bl-south-west-control-points wall-cross-section-steps)
         inner-thumb-br-south-east-control-points (klor-wall-control-points-from-map inner-thumb-br-south-east)
         inner-thumb-br-south-east-outer-curve (klor-outer-wall-vertical-nurbs inner-thumb-br-south-east-control-points wall-cross-section-steps)
         inner-thumb-br-south-east-inner-curve (klor-inner-wall-vertical-linear inner-thumb-br-south-east-control-points wall-cross-section-steps)
         inner-thumb-br-east-control-points (klor-wall-control-points-from-map inner-thumb-br-east)
         inner-thumb-br-east-outer-curve (klor-outer-wall-vertical-nurbs inner-thumb-br-east-control-points wall-cross-section-steps)
         inner-thumb-br-east-inner-curve (klor-inner-wall-vertical-linear inner-thumb-br-east-control-points wall-cross-section-steps)
         degree 3
         n (dec (count points))
         segments (- n (dec degree))
         total-wall-section-steps (* wall-section-steps segments)
         outer-vertical-curves (mapv #(klor-outer-wall-vertical-nurbs % wall-cross-section-steps) points)
         inner-vertical-curves (mapv #(klor-inner-wall-vertical-linear % wall-cross-section-steps) points)
         outer-wall-points (vec (for [index (range (inc wall-cross-section-steps))
                                      :let [current-points (vec (for [inner-index (range (inc n))]
                                                                  (get-in outer-vertical-curves [inner-index index])))]]
                                  (global-curve-interp-with-end-unit-derivatives-curve
                                   current-points
                                   degree
                                   (nth outer-thumb-bl-south-west-outer-curve index)
                                   (nth inner-thumb-br-east-outer-curve index)
                                   total-wall-section-steps
                                   :point-paramater-calculation-method :circular)))
         inner-wall-points (mapv #(vec (reverse %)) (vec (for [index (range (inc wall-cross-section-steps))
                                      :let [current-points (vec (for [inner-index (range (inc n))]
                                                                  (get-in inner-vertical-curves [inner-index index])))]]
                                  (global-curve-interp-with-end-unit-derivatives-curve
                                   current-points
                                   degree
                                   (nth outer-thumb-bl-south-west-inner-curve index)
                                   (nth inner-thumb-br-east-inner-curve index)
                                   total-wall-section-steps
                                   :point-paramater-calculation-method :circular))))
         outer-floor-points (get-floor-points outer-wall-points)
         inner-floor-points (get-floor-points inner-wall-points)] 
         {:wall (vnf-polyhedron (wall-vnf-array outer-wall-points  inner-wall-points))
          :outer-wall-floor-points outer-floor-points
          :inner-wall-floor-points inner-floor-points}
         ))

(defn thumb-to-pinky-wall [wall-cross-section-steps wall-section-steps]
  (let [inner-thumb-br-south-control-points (klor-wall-control-points-from-map inner-thumb-br-south)
        inner-thumb-br-south-east-control-points (klor-wall-control-points-from-map inner-thumb-br-south-east)
        inner-thumb-br-east-control-points (klor-wall-control-points-from-map inner-thumb-br-east)
        inner-thumb-rm-control-points (klor-wall-control-points-from-map inner-thumb-rm)
        fourth-bottom-bl-control-points (klor-wall-control-points-from-map fourth-bottom-bl)
        inner-thumb-br-south-outer-curve (klor-outer-wall-vertical-nurbs inner-thumb-br-south-control-points wall-cross-section-steps)
        inner-thumb-br-south-east-outer-curve (klor-outer-wall-vertical-nurbs inner-thumb-br-south-east-control-points wall-cross-section-steps)
        inner-thumb-br-east-outer-curve (klor-outer-wall-vertical-nurbs inner-thumb-br-east-control-points wall-cross-section-steps)
        inner-thumb-rm-outer-curve (klor-outer-wall-vertical-nurbs inner-thumb-rm-control-points wall-cross-section-steps)
        fourth-bottom-bl-outer-curve (klor-outer-wall-vertical-nurbs fourth-bottom-bl-control-points wall-cross-section-steps)
        fourth-bottom-bm-control-points (klor-wall-control-points-from-map fourth-bottom-bm)
        fourth-bottom-br-control-points (klor-wall-control-points-from-map fourth-bottom-br)
        pinky-bottom-lm-south-west-control-points (klor-wall-control-points-from-map pinky-bottom-lm-south-west)
        pinky-bottom-bl-west-inset-control-points (klor-wall-control-points-from-map pinky-bottom-bl-south-west-inset)
        pinky-bottom-bl-west-control-points (klor-wall-control-points-from-map pinky-bottom-bl-west)
        pinky-bottom-bl-south-west-control-points (klor-wall-control-points-from-map pinky-bottom-bl-south-west)
        pinky-bottom-bl-south-control-points (klor-wall-control-points-from-map pinky-bottom-bl-south)
        pinky-bottom-br-south-control-points (klor-wall-control-points-from-map pinky-bottom-br-south)
        fourth-bottom-bm-outer-curve (klor-outer-wall-vertical-nurbs fourth-bottom-bm-control-points wall-cross-section-steps)
        fourth-bottom-br-outer-curve (klor-outer-wall-vertical-nurbs fourth-bottom-br-control-points wall-cross-section-steps)
        pinky-bottom-lm-south-west-outer-curve (klor-outer-wall-vertical-nurbs pinky-bottom-lm-south-west-control-points wall-cross-section-steps)
        pinky-bottom-bl-west-inset-outer-curve (klor-outer-wall-vertical-nurbs pinky-bottom-bl-west-inset-control-points  wall-cross-section-steps)
        pinky-bottom-bl-west-outer-curve (klor-outer-wall-vertical-nurbs pinky-bottom-bl-west-control-points  wall-cross-section-steps)
        pinky-bottom-bl-south-west-outer-curve (klor-outer-wall-vertical-nurbs pinky-bottom-bl-south-west-control-points wall-cross-section-steps)
        pinky-bottom-bl-south-outer-curve (klor-outer-wall-vertical-nurbs pinky-bottom-bl-south-control-points wall-cross-section-steps)
        pinky-bottom-br-south-outer-curve (klor-outer-wall-vertical-nurbs pinky-bottom-br-south-control-points wall-cross-section-steps)
        fourth-bottom-bl-to-pinky-bottom-bl-south-degree 2
        fourth-bottom-bl-to-pinky-bottom-bl-south-wall-section-steps (* 3 wall-section-steps)
        fourth-bottom-bl-to-pinky-bottom-bl-south-wall (vec (for [index (range (inc wall-cross-section-steps))]
                                                              (global-curve-interp-with-calculated-first-derivatives-curve
                                                               [(nth fourth-bottom-bl-outer-curve index)
                                                                (nth fourth-bottom-bm-outer-curve index)
                                                                (nth fourth-bottom-br-outer-curve index)
                                                      ;(nth pinky-bottom-bl-west-outer-curve index)
                                                      ;(nth pinky-bottom-bl-south-west-outer-curve index)
                                                                (nth pinky-bottom-bl-south-outer-curve index)]
                                                               [(mapv - (nth fourth-bottom-bl-outer-curve index) (nth inner-thumb-rm-outer-curve index))
                                                                (mapv - (nth fourth-bottom-br-outer-curve index) (nth fourth-bottom-bm-outer-curve index))
                                                                (mapv - (nth pinky-bottom-bl-west-outer-curve index) (nth fourth-bottom-bm-outer-curve index))
                                                      ;(mapv - (nth pinky-bottom-bl-south-west-outer-curve index) (nth fourth-bottom-br-outer-curve index))
                                                                (mapv - (nth pinky-bottom-br-south-outer-curve index) (nth pinky-bottom-bl-south-outer-curve index))]
                                                               fourth-bottom-bl-to-pinky-bottom-bl-south-degree
                                                               fourth-bottom-bl-to-pinky-bottom-bl-south-wall-section-steps
                                                               :point-paramater-calculation-method :dynamic-centripetal
                                                               :magnitude-estimation-method :chord)))
        fourth-bottom-bl-to-pinky-bottom-bl-south-wall-2 (vec (for [index (range (inc wall-cross-section-steps))]
                                                                (local-cubic-curve-interpolation-with-tangents-curve
                                                                 [(nth fourth-bottom-bl-outer-curve index)
                                                                  (nth fourth-bottom-bm-outer-curve index)
                                                                  (nth pinky-bottom-bl-west-inset-outer-curve index)
                                                                  (nth pinky-bottom-bl-west-outer-curve index)
                                                                  ;(nth pinky-bottom-bl-south-west-outer-curve index)
                                                                  (nth pinky-bottom-bl-south-outer-curve index)]
                                                                 [(mapv - (nth fourth-bottom-bl-outer-curve index) (nth inner-thumb-rm-outer-curve index))
                                                                  (mapv - (nth fourth-bottom-br-outer-curve index) (nth fourth-bottom-bm-outer-curve index))
                                                                  (mapv - (nth pinky-bottom-bl-west-outer-curve index) (nth fourth-bottom-bm-outer-curve index))
                                                                  (mapv - (nth pinky-bottom-bl-south-west-outer-curve index) (nth fourth-bottom-bm-outer-curve index))
                                                                  (mapv - (nth pinky-bottom-br-south-outer-curve index) (nth pinky-bottom-bl-south-outer-curve index))]
                                                                 fourth-bottom-bl-to-pinky-bottom-bl-south-wall-section-steps
                                                                 ;:use-cross true
                                                                 ;:corner-perservation :preserve
                                                                 )))
        points (mapv klor-wall-control-points-from-map
                     [inner-thumb-br-south
                      inner-thumb-br-south-east
                      inner-thumb-br-east
                      inner-thumb-rm
                      fourth-bottom-bl

                      pinky-bottom-bl-south-west-inset
                     ; pinky-bottom-lm-south-west
                      pinky-bottom-bl-west
                      pinky-bottom-bl-south-west
                      pinky-bottom-bl-south
                      pinky-bottom-bm
                      pinky-bottom-br-south
                      pinky-bottom-br-south-east
                      pinky-bottom-br-east
                      pinky-bottom-br-south-east-inset
                      outer-pinky-bottom-br-south])
        n (dec (count points))
        nurbs-degree 2
        circ-weight (/ (sqrt 2) 2)
        weights [1 circ-weight 1
                 circ-weight 1
                 2
                 1 circ-weight 1
                 1
                 1 circ-weight 1
                 (sqrt 2) 1]
        knot-vector (let [denom n]
                      (mul (dec denom)
                           [0 0 0
                            (/ 2 denom) (/ 2 denom)
                            (/ 4 denom) (/ 4 denom)
                            (/ 6 denom) (/ 6 denom)
                            (/ 8 denom) (/ 8 denom)
                            (/ 10 denom) (/ 10 denom)
                            (/ 12 denom) (/ 12 denom)
                            ;; (/ 14 denom) (/ 14 denom)
                            ;; (/ 16 denom) (/ 16 denom)
                            1 1 1]))
        total-wall-section-steps (* (- (inc n) (inc nurbs-degree)) wall-section-steps)
        outer-vertical-curves (mapv #(klor-outer-wall-vertical-nurbs % wall-section-steps) points)
        inner-vertical-curves (mapv #(klor-inner-wall-vertical-linear % wall-section-steps) points)
        outer-wall-points (vec (for [index (range (inc wall-cross-section-steps))
                                     :let [points (vec (for [inner-index (range (inc n))] (get-in outer-vertical-curves [inner-index index])))]]
                                 (nurbs points 2 knot-vector weights total-wall-section-steps)))
        inner-wall-points (mapv #(reverse %) (vec (for [index (range (inc wall-cross-section-steps))
                                                        :let [points (vec (for [inner-index (range (inc n))] (get-in inner-vertical-curves [inner-index index])))]]
                                                    (nurbs points 2 knot-vector weights total-wall-section-steps))))
        outer-floor-points (get-floor-points outer-wall-points)
        inner-floor-points (get-floor-points inner-wall-points)]
    (union (vnf-polyhedron (wall-vnf-array outer-wall-points inner-wall-points))
           (plot-bezier-points outer-floor-points (sphere 0.5))
           (plot-bezier-points inner-floor-points (sphere 0.5))
           ;(vnf-polyhedron (vnf-vertex-array inner-wall-points :caps false :col-wrap false))
           ;(vnf-polyhedron(vnf-vertex-array fourth-bottom-bl-to-pinky-bottom-bl-south-wall-2 :caps false :col-wrap false))
          ; (color [1 0 0 1](plot-bezier-points pinky-bottom-bl-west-outer-curve (sphere 0.5)))
           ;(color [0 1 0 1] (plot-bezier-points fourth-bottom-br-outer-curve (sphere 0.5)))
           ;(plot-bezier-points pinky-bottom-bl-west-inset-outer-curve (sphere 0.5))
           )
    
    {:wall (vnf-polyhedron (wall-vnf-array outer-wall-points inner-wall-points))
     :outer-wall-floor-points outer-floor-points
     :inner-wall-floor-points inner-floor-points})
  )

(defn trackpad-walls [wall-cross-section-steps wall-section-steps]
  (let [tps-43-tl-north (klor-tps-43-wall-control-points :tl :north)
        tps-43-tl-north-west (klor-tps-43-wall-control-points :tl :north-west)
        tps-43-tl-west (klor-tps-43-wall-control-points :tl :west)
        tps-43-lm-west (klor-tps-43-wall-control-points :lm :west)
        tps-43-bl-west (klor-tps-43-wall-control-points :bl :west)
        tps-43-bl-south-west (klor-tps-43-wall-control-points :bl :south-west)
        tps-43-bl-south (klor-tps-43-wall-control-points :bl :south)
        tm-inset-offset [(/ key-spacing-horizontal 3) 0 0]
        tps-43-tm-inset-west (update-vals (klor-tps-43-wall-control-points :tl :west) #(mapv +  tm-inset-offset %))
        tps-43-tm-inset-north-west (update-vals  (klor-tps-43-wall-control-points :tl :north-west) #(mapv + tm-inset-offset %))
        tps-43-tm-inset-north (update-vals (klor-tps-43-wall-control-points :tl :north) #(mapv + tm-inset-offset  %))

        lm-inset-offset (mapv + key-spacing-east [2 0 0])
        tps-43-lm-inset-west (update-vals (klor-tps-43-wall-control-points :lm :west) #(mapv + lm-inset-offset %))
        bm-inset-offset (mapv + key-spacing-east [1 2 0])
        tps-43-bm-inset-west (update-vals (klor-tps-43-wall-control-points :bl :west) #(mapv + bm-inset-offset %))
        tps-43-bm-inset-south-west (update-vals (klor-tps-43-wall-control-points :bl :south-west) #(mapv + bm-inset-offset %))
        tps-43-bm-inset-south (update-vals (klor-tps-43-wall-control-points :bl :south) #(mapv + bm-inset-offset %))
        oled-holder-bl-inset-control-points (klor-wall-control-points-from-map oled-holder-bl-inset)
        above-trrs-jack-control-points (klor-wall-control-points-from-map above-trrs-jack)
        below-trrs-jack-control-points (klor-wall-control-points-from-map below-trrs-jack)
        outer-thumb-tm-north-inset-control-points (klor-wall-control-points-from-map outer-thumb-tm-north-inset)
        outer-thumb-tm-north-inset-left-control-points (klor-wall-control-points-from-map outer-thumb-tm-north-inset-left)
        outer-thumb-tm-north-control-points (klor-wall-control-points-from-map outer-thumb-tm-north)
        outer-thumb-tl-north-control-points (klor-wall-control-points-from-map outer-thumb-tl-north)
        outer-thumb-tl-north-west-control-points (klor-wall-control-points-from-map outer-thumb-tl-north-west)
        outer-thumb-tl-west-control-points (klor-wall-control-points-from-map outer-thumb-tl-west)
        outer-thumb-bl-west-control-points (klor-wall-control-points-from-map outer-thumb-bl-west)
        outer-thumb-bl-south-west-control-points (klor-wall-control-points-from-map outer-thumb-bl-south-west)
        outer-thumb-bl-south-control-points (klor-wall-control-points-from-map outer-thumb-bl-south)
        outer-thumb-br-south-control-points (klor-wall-control-points-from-map outer-thumb-br-south)
        tps-43-tl-north-outer-curve (klor-outer-wall-vertical-nurbs tps-43-tl-north wall-cross-section-steps)
        oled-holder-tl-north-control-points (klor-wall-control-points-from-map   oled-holder-tl-north)
        oled-holder-tl-north-west-control-points (klor-wall-control-points-from-map   oled-holder-tl-north-west)
        oled-holder-tl-north-outer-curve (klor-outer-wall-vertical-nurbs oled-holder-tl-north-control-points wall-cross-section-steps)
        oled-holder-tl-north-inner-curve (klor-inner-wall-vertical-linear oled-holder-tl-north-control-points wall-cross-section-steps)
        oled-holder-tl-north-west-outer-curve (klor-outer-wall-vertical-nurbs oled-holder-tl-north-west-control-points wall-cross-section-steps)
        oled-holder-tl-west-control-points (klor-wall-control-points-from-map oled-holder-tl-west)
        oled-holder-lm-west-control-points (klor-wall-control-points-from-map oled-holder-lm-west)
        oled-holder-tl-west-outer-curve (klor-outer-wall-vertical-nurbs oled-holder-tl-west-control-points wall-cross-section-steps)
        oled-holder-lm-west-outer-curve (klor-outer-wall-vertical-nurbs oled-holder-lm-west-control-points wall-cross-section-steps)
        oled-holder-bl-inset-outer-curve (klor-outer-wall-vertical-nurbs oled-holder-bl-inset-control-points wall-cross-section-steps)
        oled-holder-tl-west-inner-curve (klor-inner-wall-vertical-linear oled-holder-tl-west-control-points wall-cross-section-steps)
        oled-holder-lm-west-inner-curve (klor-inner-wall-vertical-linear oled-holder-lm-west-control-points wall-cross-section-steps)
        oled-holder-bl-inset-inner-curve (klor-inner-wall-vertical-linear oled-holder-bl-inset-control-points wall-cross-section-steps)
        outer-thumb-tm-north-inset-left-outer-curve (klor-outer-wall-vertical-nurbs outer-thumb-tm-north-inset-left-control-points wall-cross-section-steps)
        outer-thumb-tm-north-inset-left-inner-curve (klor-inner-wall-vertical-linear outer-thumb-tm-north-inset-left-control-points wall-cross-section-steps)
        outer-thumb-tm-north-inset-outer-curve (klor-outer-wall-vertical-nurbs outer-thumb-tm-north-inset-control-points wall-cross-section-steps)
        outer-thumb-tm-north-inset-inner-curve (klor-inner-wall-vertical-linear outer-thumb-tm-north-inset-control-points wall-cross-section-steps)
        outer-thumb-tm-north-outer-curve (klor-outer-wall-vertical-nurbs outer-thumb-tm-north-control-points wall-cross-section-steps)
        outer-thumb-tm-north-inner-curve (klor-inner-wall-vertical-linear outer-thumb-tm-north-control-points wall-cross-section-steps)

        above-trrs-jack-outer (klor-outer-wall-vertical-nurbs above-trrs-jack-control-points wall-cross-section-steps)
        below-trrs-jack-outer-inset (nurbs-with-calculated-knot-vector (update-vals (select-values above-trrs-jack-control-points klor-outer-wall-vertical-nurbs-control-points-keywords)
                                                                                    #(mapv + [0 1 0] %)) 3
                                                                       [1 1 (/ (sqrt 2) 2) 0.6 0.75 1]
                                                                       wall-cross-section-steps :point-paramater-calculation-method :dynamic-centripetal
                                                                       :knot-vector-generation-method :natural)
        below-trrs-jack-outer (nurbs-with-calculated-knot-vector (select-values below-trrs-jack-control-points klor-outer-wall-vertical-nurbs-control-points-keywords) 3
                                                                 [1 1 (/ (sqrt 2) 2) 0.6 0.75 1]
                                                                 wall-cross-section-steps :point-paramater-calculation-method :dynamic-centripetal
                                                                 :knot-vector-generation-method :natural)
        ;; (n-degree-bezier-curve (select-values below-trrs-jack-control-points [:wall-locate-1-to-3-curve-for-polyhedron-control-point-point :wall-locate-2-bottom-floor])
        ;;                                              wall-cross-section-steps)
        above-trrs-jack-inner (klor-inner-wall-vertical-linear above-trrs-jack-control-points wall-cross-section-steps)
        below-trrs-jack-inner-inset (n-degree-bezier-curve (update (update-vals (select-values above-trrs-jack-control-points [:wall-top-inner :wall-locate-2-bottom-floor]) #(mapv + [0 1 0] %)) 1  #(mapv + [1 0 0] %)) wall-cross-section-steps)
        below-trrs-jack-inner (n-degree-bezier-curve (update (select-values below-trrs-jack-control-points [:wall-top-inner :wall-locate-2-bottom-floor]) 1 #(mapv + [1 0 0] %))   wall-cross-section-steps)
        outer-thumb-tl-north-outer (klor-outer-wall-vertical-nurbs outer-thumb-tl-north-control-points wall-cross-section-steps)
        outer-thumb-tl-north-west-outer (klor-outer-wall-vertical-nurbs outer-thumb-tl-north-west-control-points wall-cross-section-steps)
        outer-thumb-tl-west-outer (klor-outer-wall-vertical-nurbs outer-thumb-tl-west-control-points wall-cross-section-steps)
        outer-thumb-bl-west-outer (klor-outer-wall-vertical-nurbs outer-thumb-bl-west-control-points wall-cross-section-steps)
        outer-thumb-bl-south-west-outer (klor-outer-wall-vertical-nurbs outer-thumb-bl-south-west-control-points wall-cross-section-steps)
        outer-thumb-bl-south-outer (klor-outer-wall-vertical-nurbs outer-thumb-bl-south-control-points wall-cross-section-steps)
        outer-thumb-br-south-outer (klor-outer-wall-vertical-nurbs outer-thumb-br-south-control-points wall-cross-section-steps)
        outer-thumb-tl-north-inner (klor-inner-wall-vertical-linear outer-thumb-tl-north-control-points wall-cross-section-steps)
        outer-thumb-tl-north-west-inner (klor-inner-wall-vertical-linear outer-thumb-tl-north-west-control-points wall-cross-section-steps)
        outer-thumb-tl-west-inner (klor-inner-wall-vertical-linear outer-thumb-tl-west-control-points wall-cross-section-steps)
        outer-thumb-bl-west-inner (klor-inner-wall-vertical-linear outer-thumb-bl-west-control-points wall-cross-section-steps)
        outer-thumb-bl-south-west-inner (klor-inner-wall-vertical-linear outer-thumb-bl-south-west-control-points wall-cross-section-steps)
        outer-thumb-bl-south-inner (klor-inner-wall-vertical-linear outer-thumb-bl-south-control-points wall-cross-section-steps)
        outer-thumb-br-south-inner (klor-inner-wall-vertical-linear outer-thumb-br-south-control-points wall-cross-section-steps)
        outer-keywords [:wall-top-outer

                        :wall-locate1-point
                        :wall-locate-2-top]
        inset-outer-keywords [;:wall-locate-1-to-3-curve-for-polyhedron-second-control-point
                              :wall-locate1-point
                              :wall-locate-2-top
                              :wall-locate-2-bottom
                              :wall-locate-2-bottom-floor]
        adjust-inset-fn (fn [control-points] (update (select-values control-points inset-outer-keywords) 1 #(mapv + [0 0 -2] %)))
        tm-inset-selected-control-points (update (select-values tps-43-tm-inset-west inset-outer-keywords) 1 #(mapv + [0 0 -2] %))
        bm-inset-selected-control-points (update (select-values tps-43-bm-inset-west inset-outer-keywords) 1 #(mapv + [0 0 -2] %))
        knot-vector (let [denom 6]
                      (mul (dec denom) [0 0 0 (/  2 denom) (/  3 denom)   (/ 5 denom) (/ 5 denom) 1 1 1]))
        weights [1 (/ (sqrt 2) 2) 1  (/ (sqrt 2) 2) (/ (sqrt 2) 2) 1 1]
        outer-fn (fn [control-points]
                   (nurbs
                    control-points
                    2
                    knot-vector
                    weights
                    wall-cross-section-steps))
        tl-north (outer-fn (vec (concat (select-values tps-43-tl-north outer-keywords)
                                        (adjust-inset-fn tps-43-tm-inset-north))))
        tl-north-west (outer-fn (vec (concat (select-values tps-43-tl-north-west outer-keywords)
                                             (adjust-inset-fn tps-43-tm-inset-north-west))))
        tl-west (outer-fn (vec (concat (select-values tps-43-tl-west outer-keywords)
                                       (adjust-inset-fn tps-43-tm-inset-west))))
        lm-west (outer-fn (vec (concat (select-values tps-43-lm-west outer-keywords)
                                       (adjust-inset-fn tps-43-lm-inset-west))))
        bl-west (outer-fn (vec (concat (select-values tps-43-bl-west outer-keywords)
                                       (adjust-inset-fn tps-43-bm-inset-west))))
        bl-south-west (outer-fn (vec (concat (select-values tps-43-bl-south-west outer-keywords)
                                             (adjust-inset-fn tps-43-bm-inset-south-west))))
        bl-south (outer-fn (vec (concat (select-values tps-43-bl-south outer-keywords)
                                        (adjust-inset-fn tps-43-bm-inset-south))))
        total-wall-section-steps (* wall-section-steps 14)
        outer-wall-points (vec (for [index (range (inc wall-cross-section-steps))]

                          ;; (n-degree-bezier-curve
                          ;;  [(nth tl-west index)
                          ;;   (nth bl-west index)]
                          ;;  wall-cross-section-steps)
                                 (global-curve-interp-with-calculated-first-derivatives-curve
                                  (mapv #(nth % index)
                                        [oled-holder-tl-west-outer-curve
                                         oled-holder-lm-west-outer-curve
                                         oled-holder-bl-inset-outer-curve
                                  ;tl-north
                                 ; tl-north-west
                                  ;tl-west
                                  ;lm-west
                                  ;bl-west
                                 ; bl-south-west
                                 ; bl-south
                                  ;above-trrs-jack-outer
                                         below-trrs-jack-outer-inset
                                         below-trrs-jack-outer
                                         outer-thumb-tm-north-inset-outer-curve
                                         outer-thumb-tm-north-inset-left-outer-curve
                                         outer-thumb-tl-north-outer
                                         outer-thumb-tl-west-outer
                                         outer-thumb-bl-west-outer
                                         outer-thumb-bl-south-outer])
                                  (mapv #(mapv - (nth (nth % 0) index) (nth (nth % 1) index))
                                        [[oled-holder-lm-west-outer-curve oled-holder-tl-west-outer-curve]

                                         [oled-holder-bl-inset-outer-curve oled-holder-lm-west-outer-curve]
                                         [below-trrs-jack-outer-inset oled-holder-bl-inset-outer-curve]
                                  ;[tl-north oled-holder-lm-west-outer-curve]
                                  ;[tl-north oled-holder-bl-inset-outer-curve]
                                 ; [tl-north oled-holder-bl-inset-outer-curve]
                                  ;[tl-north-west tl-north]
                                  ;[lm-west tl-west]
                                  ;[bl-west lm-west]
                                  ;[bl-south-west bl-west]
                                  ;[below-trrs-jack-outer-inset bl-south]
                                         [below-trrs-jack-outer below-trrs-jack-outer-inset]
                                         [below-trrs-jack-outer below-trrs-jack-outer-inset]
                                         [outer-thumb-tm-north-inset-left-outer-curve outer-thumb-tm-north-inset-outer-curve]
                                  ;[outer-thumb-tm-north-inset-left-outer-curve outer-thumb-tm-north-inset-outer-curve]
                                         [outer-thumb-tl-north-outer outer-thumb-tm-north-inset-left-outer-curve]
                                         [outer-thumb-tl-north-outer outer-thumb-tm-north-inset-left-outer-curve];[outer-thumb-tl-north-west-outer outer-thumb-tm-north-inset-left-outer-curve ]
                                         [outer-thumb-bl-west-outer outer-thumb-tl-west-outer]
                                         [outer-thumb-bl-south-west-outer outer-thumb-bl-west-outer]
                                         [outer-thumb-br-south-outer outer-thumb-bl-south-outer; outer-thumb-bl-south-west-outer
                                          ]])
                                  2
                                  total-wall-section-steps
                                  :point-paramater-calculation-method :chordal
                                  :magnitude-estimation-method :arc)))
        inner-wall-points (vec (for [index (range (inc wall-cross-section-steps))]
                                 (global-curve-interp-with-calculated-first-derivatives-curve
                                  (mapv #(nth % index)
                                        [oled-holder-tl-west-inner-curve
                                         oled-holder-lm-west-inner-curve
                                         oled-holder-bl-inset-inner-curve
                                  ;tl-north
                                 ; tl-north-west
                                  ;tl-west
                                  ;lm-west
                                  ;bl-west
                                 ; bl-south-west
                                 ; bl-south
                                  ;above-trrs-jack-inner
                                         below-trrs-jack-inner-inset
                                         below-trrs-jack-inner
                                         outer-thumb-tm-north-inset-inner-curve
                                         outer-thumb-tm-north-inset-left-inner-curve
                                         outer-thumb-tl-north-inner
                                         outer-thumb-tl-west-inner
                                         outer-thumb-bl-west-inner
                                         outer-thumb-bl-south-inner])
                                  (mapv #(mapv - (nth (nth % 0) index) (nth (nth % 1) index))
                                        [[oled-holder-lm-west-inner-curve oled-holder-tl-west-inner-curve]

                                         [oled-holder-bl-inset-inner-curve oled-holder-lm-west-inner-curve]
                                         [below-trrs-jack-inner-inset oled-holder-bl-inset-inner-curve]
                                  ;[tl-north oled-holder-lm-west-inner-curve]
                                  ;[tl-north oled-holder-bl-inset-inner-curve]
                                 ; [tl-north oled-holder-bl-inset-inner-curve]
                                  ;[tl-north-west tl-north]
                                  ;[lm-west tl-west]
                                  ;[bl-west lm-west]
                                  ;[bl-south-west bl-west]
                                  ;[below-trrs-jack-inner-inset bl-south]
                                         [below-trrs-jack-inner below-trrs-jack-inner-inset]
                                         [below-trrs-jack-inner below-trrs-jack-inner-inset]
                                         [outer-thumb-tm-north-inset-left-inner-curve outer-thumb-tm-north-inset-inner-curve]
                                  ;[outer-thumb-tm-north-inset-left-inner-curve outer-thumb-tm-north-inset-inner-curve]
                                         [outer-thumb-tl-north-inner outer-thumb-tm-north-inset-left-inner-curve]
                                         [outer-thumb-tl-north-inner outer-thumb-tm-north-inset-left-inner-curve];[outer-thumb-tl-north-west-inner outer-thumb-tm-north-inset-left-inner-curve ]
                                         [outer-thumb-bl-west-inner outer-thumb-tl-west-inner]
                                         [outer-thumb-bl-south-west-inner outer-thumb-bl-west-inner]
                                         [outer-thumb-br-south-inner outer-thumb-bl-south-inner; outer-thumb-bl-south-west-inner
                                          ]])
                                  2
                                  total-wall-section-steps
                                  :point-paramater-calculation-method :chordal
                                  :magnitude-estimation-method :arc)
                                 ;;  (global-curve-interp-with-calculated-first-derivatives-curve
                                ;;   (mapv #(nth % index)
                                ;;         [outer-thumb-bl-south-inner
                                ;;          outer-thumb-bl-west-inner
                                ;;          outer-thumb-tl-west-inner
                                ;;          outer-thumb-tl-north-inner
                                ;;          outer-thumb-tm-north-inset-left-inner-curve
                                ;;          outer-thumb-tm-north-inset-inner-curve
                                ;;          below-trrs-jack-inner
                                ;;          below-trrs-jack-inner-inset
                                ;;          oled-holder-bl-inset-inner-curve
                                ;;          oled-holder-lm-west-inner-curve
                                ;;          oled-holder-tl-west-inner-curve
                                ;;          ])
                                ;;   (mapv #(mapv - (nth (nth % 0) index) (nth (nth % 1) index))
                                ;;         [
                                ;;          [ outer-thumb-bl-south-inner outer-thumb-br-south-inner]
                                ;;    [outer-thumb-tl-west-inner outer-thumb-bl-south-west-inner]
                                ;;    ;      [outer-thumb-tl-north-inner outer-thumb-bl-south-west-inner]
                                ;;   [outer-thumb-tl-west-inner outer-thumb-bl-south-west-inner]
                                ;;   ;       [outer-thumb-tl-north-inner outer-thumb-tl-west-inner]
                                ;;    [outer-thumb-tm-north-inset-left-inner-curve outer-thumb-tl-north-inner]
                                ;;    [outer-thumb-tm-north-inset-inner-curve outer-thumb-tm-north-inset-left-inner-curve]
                                ;;          [below-trrs-jack-inner outer-thumb-tm-north-inset-inner-curve]
                                ;;    [below-trrs-jack-inner-inset below-trrs-jack-inner]
                                ;;          [below-trrs-jack-inner-inset below-trrs-jack-inner]
                                ;;    [oled-holder-bl-inset-inner-curve below-trrs-jack-inner-inset]
                                ;;    ;[oled-holder-lm-west-inner-curve oled-holder-bl-inset-inner-curve]
                                ;;    [oled-holder-lm-west-inner-curve oled-holder-bl-inset-inner-curve]
                                ;;    [oled-holder-tl-north-inner-curve oled-holder-lm-west-inner-curve]
                                ;;    ])
                                ;;   2
                                ;;   total-wall-section-steps
                                ;;   :point-paramater-calculation-method :chordal
                                ;;   :magnitude-estimation-method :arc
                                ;;   )
                                  ;; (bezier-linear-spline
                                  ;;  (mapv #(nth % index)
                                  ;;        [outer-thumb-bl-south-inner
                                  ;;         ;outer-thumb-bl-west-inner
                                  ;;         ;outer-thumb-tl-west-inner
                                  ;;         outer-thumb-tl-north-inner
                                  ;;         outer-thumb-tm-north-inset-inner-curve
                                  ;;         below-trrs-jack-inner
                                  ;;         below-trrs-jack-inner-inset
                                  ;;         oled-holder-bl-inset-inner-curve
                                  ;;         oled-holder-lm-west-inner-curve
                                  ;;         oled-holder-tl-west-inner-curve])
                                  ;;  total-wall-section-steps
                                  ;;  )
                                 ))

        outer-floor-points (get-floor-points outer-wall-points)
        inner-floor-points (get-floor-points inner-wall-points)
                          ;;  inner-wall-points (vec (for [index (range (inc wall-cross-section-steps))]
                          ;;                           (global-curve-interp-with-calculated-first-derivatives-curve
                          ;;                            []
                          ;;                            )
                          ;;                           ))
        ] 
    {:wall  (vnf-polyhedron (wall-vnf-array outer-wall-points (mapv #(vec (reverse %)) inner-wall-points)
                                            :args {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :alt}))
     :outer-wall-floor-points outer-floor-points
     :inner-wall-floor-points inner-floor-points})
  )

(comment (spit "things-low/tps-43.scad"
      (write-scad
       (include include-bosl2)

       (let [
            ;;  (->> (cube tps-43-width tps-43-length 1.05)
            ;;                (translate [0 0 -0.5]))
             ]
         (union
          ;cutout
           ;                    (-# tps-65-mount-main-cutout)
            ;; (difference  tps-65-mount-new
            ;;                           tps-65
            ;;                           tps-65-mount-cutout
            ;;                           (translate [0 0 (+ 0 1)] tps-65-mount-main-cutout))
          (difference (tps-43-mount-body)
                      tps-43-cutout
                      tps-43-full-cutout))))))

(defn trackpad-attachnent-walls [wall-cross-section-steps wall-section-steps]
  (let [offset-value 0.4
        top-tm (klor-tps-43-position (mapv + [-3 offset-value 0] key-spacing-inner-west (tps-43-mount-spacing :tm)) :height klor-case-walls-height)
        top-tl-north (klor-tps-43-position (mapv + [0 offset-value 0] [2 0 0] (tps-43-mount-spacing :tl)) :height klor-case-walls-height)
        top-tl-north-west (klor-tps-43-position (mapv + [(- offset-value) offset-value 0] (tps-43-mount-spacing :tl)) :height klor-case-walls-height)
        top-tl-west (klor-tps-43-position (mapv + [(- offset-value) -2 0] (tps-43-mount-spacing :tl)) :height klor-case-walls-height)
        top-lm (klor-tps-43-position (mapv + [(- offset-value) 0 0] (tps-43-mount-spacing :lm)) :height klor-case-walls-height)
        top-bl-west  (klor-tps-43-position (mapv + [(- offset-value) 2 0] (tps-43-mount-spacing :bl)) :height klor-case-walls-height)
        top-bl-south-west (klor-tps-43-position (mapv + [(- offset-value) (- offset-value) 0] (tps-43-mount-spacing :bl)) :height klor-case-walls-height)
        top-bl-south (klor-tps-43-position (mapv + [0 (- offset-value) 0] [2 0 0] (tps-43-mount-spacing :bl)) :height klor-case-walls-height)
        top-bm (klor-tps-43-position (mapv + [0 (- offset-value) 0] key-spacing-inner-west (tps-43-mount-spacing :bm)) :height klor-case-walls-height)
        top-points [top-tm
                    top-tl-north
                    top-tl-north-west
                    top-tl-west
                    top-lm
                    top-bl-west
                    top-bl-south-west
                    top-bl-south
                    top-bm]
        mid-points (mapv #(mapv + % [0 0 (- (+ tps-43-depth 1.2))]) top-points)
        inner-offset 0.8
        top-tm-inner (klor-tps-43-position (mapv + [-3 (- inner-offset) 0] key-spacing-inner-west (tps-43-mount-spacing :tm)) :height klor-case-walls-height)
        top-tl-north-inner (klor-tps-43-position (mapv + [2 (- inner-offset) 0] (tps-43-mount-spacing :tl)) :height klor-case-walls-height)
        top-tl-north-west-inner (klor-tps-43-position (mapv + [inner-offset (- inner-offset) 0] (tps-43-mount-spacing :tl)) :height klor-case-walls-height)
        top-tl-west-inner (klor-tps-43-position (mapv + [inner-offset -2 0] (tps-43-mount-spacing :tl)) :height klor-case-walls-height)
        top-lm-inner (klor-tps-43-position (mapv +  [inner-offset 0 0] (tps-43-mount-spacing :lm)) :height klor-case-walls-height)
        top-bl-west-inner  (klor-tps-43-position (mapv + [inner-offset 2 0] (tps-43-mount-spacing :bl)) :height klor-case-walls-height)
        top-bl-south-west-inner (klor-tps-43-position (mapv + [inner-offset inner-offset 0] (tps-43-mount-spacing :bl)) :height klor-case-walls-height)
        top-bl-south-inner (klor-tps-43-position (mapv + [2 inner-offset 0] (tps-43-mount-spacing :bl)) :height klor-case-walls-height)
        top-bm-inner (klor-tps-43-position (mapv + [0 inner-offset 0] key-spacing-inner-west (tps-43-mount-spacing :bm)) :height klor-case-walls-height) 
        
        top-points-inner [top-tm-inner
                          top-tl-north-inner
                          top-tl-north-west-inner
                          top-tl-west-inner
                          top-lm-inner
                          top-bl-west-inner
                          top-bl-south-west-inner
                          top-bl-south-inner
                          top-bm-inner]
        bottom-points-inner (mapv #(mapv + [0 0 (- tps-43-depth)] %) top-points-inner)
        control-2 [(mapv + top-tm [-4 -2 (- (+ tps-43-depth 1.2))])
                   (mapv +  top-bm [-2.5 2 (- (+ tps-43-depth 1.2))])]
        
        last-points [(mapv + top-tm [-4 -2 -6])
                     (mapv +  top-bm [-2.5 2 -6])]
        
        oled-holder-bl-inset-control-points (klor-wall-control-points-from-map oled-holder-bl-inset)
        oled-holder-bl-inset-outer-curve (klor-outer-wall-vertical-nurbs oled-holder-bl-inset-control-points wall-cross-section-steps)
        above-trrs-jack-control-points (klor-wall-control-points-from-map above-trrs-jack)
        below-trrs-jack-control-points (klor-wall-control-points-from-map below-trrs-jack)
        below-trrs-jack-outer-inset (nurbs-with-calculated-knot-vector (update-vals (select-values above-trrs-jack-control-points klor-outer-wall-vertical-nurbs-control-points-keywords)
                                                                                    #(mapv + [0 1 0] %)) 3
                                                                       [1 1 (/ (sqrt 2) 2) 0.6 0.75 1]
                                                                       wall-cross-section-steps :point-paramater-calculation-method :dynamic-centripetal
                                                                       :knot-vector-generation-method :natural)
        below-trrs-jack-outer (nurbs-with-calculated-knot-vector (select-values below-trrs-jack-control-points klor-outer-wall-vertical-nurbs-control-points-keywords) 3
                                                                 [1 1 (/ (sqrt 2) 2) 0.6 0.75 1]
                                                                 wall-cross-section-steps :point-paramater-calculation-method :dynamic-centripetal
                                                                 :knot-vector-generation-method :natural)
        total-wall-section-steps (* 7 wall-section-steps)
        mid-points-inner (mapv #(mapv + % [0 0 (- (+ tps-43-depth 1.2))]) top-points-inner)
        circ-weight (/ (sqrt 2) 2)
        weights [1 1 circ-weight 1 1 1 circ-weight 1 1]
        horizontal-knot-vector (let [denom 8]
                                 (mul (dec denom) [0 0 0
                                                   (/ 1 denom) (/ 3 denom)
                                                   (/ 4 denom) (/ 4 denom)
                                                   (/ 5 denom) (/ 7 denom)
                                                   1 1 1]))
        degree 2
        
        curve-fn (fn [points] (nurbs points degree horizontal-knot-vector weights total-wall-section-steps))
        top-curve (curve-fn top-points)
        mid-curve (curve-fn mid-points)
        mid-inner-curve (curve-fn mid-points-inner)
        top-inner-curve (curve-fn top-points-inner)
        bottom-points-inner-curve (curve-fn bottom-points-inner) 
        control-2-curve (n-degree-bezier-curve control-2 total-wall-section-steps)
        last-curve (n-degree-bezier-curve last-points total-wall-section-steps) 
        bottom-inner [(mapv + top-tm [-3 -2 -6])
                      (mapv +  top-bm [-1.5 2 -6])]
        control-3 [(mapv + top-tm [-3 -2 (- (+ tps-43-depth 1.2))])
                   (mapv +  top-bm [-1.5 2 (- (+ tps-43-depth 1.2))])]
        bottom-inner-curve (n-degree-bezier-curve bottom-inner total-wall-section-steps)
        control-3-curve (n-degree-bezier-curve control-3 total-wall-section-steps)
        test-points [top-tm top-tl-north top-tl-north-west top-tl-west top-lm
                     top-bl-west top-bl-south-west top-bl-south top-bm]
        test-curve (nurbs test-points
                          degree
                          (let [denom (dec (count test-points))]
                            (mul (dec denom) [0 0 0 (/ 1 denom) (/ 3 denom)
                                              (/ 4 denom)
                                              (/ 4 denom)
                                              (/ 5 denom) (/ 7 denom) 1 1 1]))
                          [1 1 circ-weight 1 1 1 circ-weight 1 1]
                          40)

        outer-wall-control-points [top-inner-curve
                                   top-curve
                                   mid-curve
                                   mid-inner-curve
                                   control-2-curve
                                   last-curve
                                   bottom-inner-curve
                                   control-3-curve 
                                   bottom-points-inner-curve
                                   ]
        outer-wall-knot-vector (let [denom (dec (count outer-wall-control-points))]
                                 (mul (dec denom) [0 0 0 (/ 1 denom) (/ 3 denom) (/ 3 denom) 
                                                   (/ 5 denom ) (/ 6 denom)  (/ 6 denom) 1 1 1])) 
        outer-wall-weights [1 1 circ-weight 1 circ-weight 1 1 circ-weight 1]
        total-wall-cross-section-steps (* wall-cross-section-steps (- (count outer-wall-control-points) 2))
        outer-wall (vec (for [index (range (inc total-wall-section-steps))]
                          (nurbs
                           (mapv #(nth % index) outer-wall-control-points)
                           degree
                           outer-wall-knot-vector
                           outer-wall-weights
                           total-wall-cross-section-steps)))]
    (union
     ;(color [1 0 0 1](plot-bezier-points control-2-curve (sphere 0.5)))
     ;(color [1 0 0 1] (plot-bezier-points control-3-curve (sphere 0.5)))
     ;(color [1 0 0 1] (plot-bezier-points last-curve (sphere 0.5)))
     ;(plot-bezier-points bottom-inner-curve (sphere 0.5))
     ;(plot-bezier-points top-points (sphere 1))
     ;(plot-bezier-points bottom-points-inner (sphere 1))
    ;;  (plot-bezier-points top-curve (sphere 0.5))
    ;;  (plot-bezier-points mid-curve (sphere 0.5))
    ;;  (plot-bezier-points mid-inner-curve (sphere 0.5))
    ;;  (plot-bezier-points top-inner-curve (sphere 0.5))
     ;(plot-bezier-points test-curve (sphere 0.5))
     ;(color [1 0 0 1](plot-bezier-points mid-points (sphere 1)))
     ;(color [0 0 1 1] (plot-bezier-points mid-points-inner (sphere 1)))
     ;(color [0 1 0 1](plot-bezier-points top-points-inner (sphere 1)))
    ;;  (plot-bezier-points (update-vals (select-values above-trrs-jack-control-points klor-outer-wall-vertical-nurbs-control-points-keywords)
    ;;                                   #(mapv + [0 1 0] %)) (sphere 1))
     ;(plot-bezier-points (vals oled-holder-bl-inset-control-points) (sphere 1))
     (translate [0 0 0](vnf-polyhedron (vnf-vertex-array outer-wall :caps true :col-wrap true :reverse true)))
     )
    )
  )
(def klor-switch-plate-polygon 
  
  (let [vert-offset 1.75
        horiz-offset 0.8
        inner-index-tl (klor-key-position 0 2 (mapv + [(- horiz-offset) vert-offset 0] keycap-spacing-north-west))
        inner-index-tr (klor-key-position 0 2 (mapv + [(- (+ horiz-offset 0.2)) vert-offset 0] keycap-spacing-north-east))
        index-tl (klor-key-position 1 2 (mapv + [(- (+ horiz-offset 1.2)) (+ vert-offset 0.3) 0] keycap-spacing-north-west))
        index-tr (klor-key-position 1 2 (mapv + [(- (+ horiz-offset 0.2)) (+ vert-offset 0.3) 0] keycap-spacing-north-east))
        middle-tl (klor-key-position 2 2 (mapv + [(- (+ horiz-offset 1.2)) (+ vert-offset 0.3) 0] keycap-spacing-north-west))
        middle-tr (klor-key-position 2 2 (mapv + [(+ horiz-offset 1.25) (+ vert-offset 0.3) 0] keycap-spacing-north-east))
        fourth-tl (klor-key-position 3 2 (mapv + [(- (+ horiz-offset 1.55)) (+ vert-offset 0.2) 0] keycap-spacing-north-west))
        fourth-tr (klor-key-position 3 2 (mapv + [(+ horiz-offset 1.275) (+ vert-offset 0.2) 0] keycap-spacing-north-east))
        pinky-tl (klor-key-position 4 2 (mapv + [(- (+ horiz-offset 3.35)) (+ vert-offset 0.4) 0] keycap-spacing-north-west))
        pinky-tr (klor-key-position 4 2 (mapv + [(+ horiz-offset 1.0) (+ vert-offset 0.4) 0] keycap-spacing-north-east))
        outer-pinky-tl (klor-key-position 5 1 (mapv + [(+ horiz-offset) (+ vert-offset 0.4) 0] keycap-spacing-north-west))
        outer-pinky-tr (klor-key-position 5 1 (mapv + [(+ horiz-offset 1.0) (+ vert-offset 0.4) 0] keycap-spacing-north-east))
        outer-pinky-bl (klor-key-position 5 0 (mapv + [(+ horiz-offset) (- (+ vert-offset 0.4)) 0] keycap-spacing-south-west))
        outer-pinky-br (klor-key-position 5 0 (mapv + [(+ horiz-offset 1.0) (- (+ vert-offset 0.4)) 0] keycap-spacing-south-east))
        pinky-bl (klor-key-position 4 0 (mapv + [(- (+ horiz-offset 1.4)) (- (+ vert-offset 0.2)) 0] keycap-spacing-south-west))
        pinky-br (klor-key-position 4 0 (mapv + [(+ horiz-offset 1.0) (- (+ vert-offset 0.2)) 0] keycap-spacing-south-east))
        fourth-bl (klor-key-position 3 0 (mapv + [(- (+ horiz-offset 1.0)) (- (+ vert-offset 0.4)) 0] keycap-spacing-south-west))
        fourth-br (klor-key-position 3 0 (mapv + [(- (+ horiz-offset 0.2)) (- (+ vert-offset 0.4)) 0] keycap-spacing-south-east))
        middle-bl (klor-key-position 2 0 (mapv + [(+ horiz-offset 0.15) (- (+ vert-offset 0.3)) 0] keycap-spacing-south-west))
        middle-br (klor-key-position 2 0 (mapv + [(- (+ horiz-offset 0.05)) (- (+ vert-offset 0.3)) 0] keycap-spacing-south-east))
        inner-thumb-tl-north-east-inset (klor-thumb-position 0 (mapv + [-1.4 1.5 0] key-spacing-north-east))
        inner-thumb-tl-north-east (klor-thumb-position 0 (mapv + [1.5 1.5 0] key-spacing-north-east))
        inner-thumb-tl-east (klor-thumb-position 0 (mapv + [1.5 1.4 0] key-spacing-east))
        inner-thumb-tl-east-outset (klor-thumb-position 0 (mapv + [3 1.4 0] key-spacing-east))
        bottom-thumb-points (reverse (mapv #(:wall-bottom-inner (klor-wall-control-points-from-map %)) [outer-thumb-bl-south
                                                                                               outer-thumb-br-south
                 ;mid-left-thumb-bl-south
                                                                                               mid-left-thumb-br-south
                 ;mid-right-thumb-bl-south
                                                                                               mid-right-thumb-br-south
                 ;inner-thumb-bl-south
                                                                                               inner-thumb-br-south]))
        inner-thumb-br (:wall-bottom-inner (klor-wall-control-points-from-map inner-thumb-br-south-east))
        outer-thumb-bl (:wall-bottom-inner (klor-wall-control-points-from-map outer-thumb-bl-south-west))
        outer-thumb-tl (:wall-bottom-inner (klor-wall-control-points-from-map outer-thumb-tl-north-west))
        outer-thumb-tm-inset (klor-thumb-position 3 (mapv + [1.55 1.5 0] key-spacing-north))
        mid-left-thumb-tr (klor-thumb-position 2 (mapv + [-1.5 1.5 0] key-spacing-north-west))
        inner-index-bl (klor-key-position 0 0 (mapv + [(- horiz-offset) (- (+ vert-offset 1.4)) 0] keycap-spacing-south-west))
        points (vec (concat [inner-index-tl
                             inner-index-tr
                             index-tl
                             index-tr
                             middle-tl
                             middle-tr
                             fourth-tl
                             fourth-tr
                             pinky-tl
                             pinky-tr
                             outer-pinky-tl
                             outer-pinky-tr
                             outer-pinky-br
                             outer-pinky-bl
                             pinky-br
                             pinky-bl
                             fourth-br
                             fourth-bl 
                             middle-br
                             middle-bl
                             inner-thumb-tl-north-east-inset
                             inner-thumb-tl-north-east
                             inner-thumb-tl-east
                             inner-thumb-tl-east-outset]
                 bottom-thumb-points
                 [outer-thumb-tl
                  outer-thumb-tm-inset
                  mid-left-thumb-tr
                  inner-index-bl]))] 
        ;(->> (extrude-linear {:height klor-switchplate-thickness  :convexity 10}
                         (polygon (mapv drop-last points))
          ;               )
         ;(translate [0 0 klor-switchplate-z-position]))
        
        )
  )


(spit "things-low/klor.scad" 
 (write-scad 
  (include include-bosl2) 
  
  
  (let [key-shape (extrude-linear {:height klor-switchplate-thickness :center false} (difference (square (dec key-spacing-horizontal) (dec key-spacing-vertical))
                                                                                                 (square 14 14)))
        wall-cross-section-steps 10
        wall-section-steps 10
        cyl (binding [*fn* 36] (cylinder 0.1 14.5 :center false))
        thumb-cyl (binding [*fn* 36] (cylinder 0.5 5.5 :center false))
        tps-43-to-thumb-position (klor-tps-43-position (mapv + [(+ (/ oled-holder-width -2) 5) (- (/ tps-43-mount-length -2) 2) 0] [(- klor-case-offset) 0 0]) :height 0)
        tps-43-to-thumb-position-2 (klor-tps-43-position (mapv + [(+ (/ oled-holder-width -2) 5) (- (/ tps-43-mount-length -2) 19) 0] [(- klor-case-offset) 0 0]) :height 0)
        tps-43-to-thumb-position-3 (klor-tps-43-position (mapv + [(+ (/ oled-holder-width -2) 5) (- (/ tps-43-mount-length -2) 10) 0] [(- klor-case-offset) 0 0]) :height 0)
        {klor-pinky-wall-polyhedron :wall
         klor-pinky-wall-outer-floor :outer-wall-floor-points
         klor-pinky-wall-inner-floor :inner-wall-floor-points} (klor-pinky-wall wall-cross-section-steps wall-section-steps)
        {klor-thumb-to-pinky-wall-polyhedron :wall
         klor-thumb-to-pinky-wall-outer-floor :outer-wall-floor-points
         klor-thumb-to-pinky-wall-inner-floor :inner-wall-floor-points} (thumb-to-pinky-wall wall-cross-section-steps wall-section-steps)
        {klor-thumb-wall-polyhedron :wall
         klor-thumb-wall-outer-floor :outer-wall-floor-points
         klor-thumb-wall-inner-floor :inner-wall-floor-points} (klor-thumb-wall wall-cross-section-steps wall-section-steps)
        {klor-trackpad-wall-polyhedron :wall
         klor-trackpad-wall-outer-floor :outer-wall-floor-points
         klor-trackpad-wall-inner-floor :inner-wall-floor-points} (trackpad-walls 20 wall-section-steps)
        {klor-back-wall-polyhedron :wall
         klor-back-wall-outer-floor :outer-wall-floor-points
         klor-back-wall-inner-floor :inner-wall-floor-points} (klor-back-wall-2 wall-cross-section-steps wall-section-steps)
        inner-points (mapv drop-last(apply concat [klor-pinky-wall-inner-floor
                                    klor-thumb-to-pinky-wall-inner-floor
                                    klor-thumb-wall-inner-floor
                                    klor-trackpad-wall-inner-floor
                                    klor-back-wall-inner-floor]))
        ] 
    
    (union
     
    ;;  (difference
    ;;   (union (difference 
    ;;           (->> 
    ;;   (extrude-linear {:height (- klor-case-walls-height klor-switchplate-z-position):center false}(polygon inner-points))
    ;;   (translate [0 0 klor-switchplate-z-position]))
    ;;           (klor-tps-43-place (tps-43-cutout-with-height (- klor-case-walls-height klor-switchplate-z-position)) :height klor-switchplate-z-position))
              klor-trackpad-wall-polyhedron
    ;; )

    ;;              (->>(extrude-linear {:height (- klor-case-walls-height klor-switchplate-z-position) :center false}klor-switch-plate-polygon)
    ;;               (translate [0 0 (+ klor-switchplate-z-position klor-switchplate-thickness )]))
    ;;    (klor-tps-43-place (translate [0 0 1] tps-43-cutout))
    ;;   (klor-tps-43-place  tps-43-full-cutout)
      
    ;;              )
;;      (mapv #(translate (mapv + (:wall-top-outer  (klor-wall-control-points-from-map %)) [0 0 (- klor-thumb-walls-height)]) thumb-cyl) [outer-thumb-bl-south
;; outer-thumb-br-south 
;; ;mid-left-thumb-bl-south
;; mid-left-thumb-br-south
;; mid-left-thumb-bl-south
;; mid-left-thumb-br-south
;; inner-thumb-bl-south]) 

     ;(plot-bezier-points (vals(klor-tps-43-wall-control-points :bm :west :offset key-spacing-inner-west)) (sphere 1))
     ;(color [1 0 0 1](plot-bezier-points (vals (klor-tps-43-wall-control-points :bm :west :offset (mapv + key-spacing-inner-west [0 -2 0]))) (sphere 1)))
klor-pinky-wall-polyhedron
klor-thumb-to-pinky-wall-polyhedron
klor-thumb-wall-polyhedron 
klor-back-wall-polyhedron
     

     ;(translate (mapv + (:wall-top-outer  (klor-wall-control-points-from-map outer-thumb-bl-south)) [0 0 (- klor-case-walls-height)]) cyl)
     ;(color [0 1 0 1](translate  (:wall-top-outer  (klor-wall-control-points-from-map mid-left-thumb-bl-south))) thumb-cyl)
     ;    (color [0 1 0 1] (translate (mapv + (:wall-top-outer  (klor-wall-control-points-from-map mid-left-thumb-br-south)) [0 0 (- klor-case-walls-height)]) cyl))
     ;switch-plate
    ;;  (->> (import "../parts/klor-ks27-polydactyl-body-right.stl")
    ;;       (rdz anchor-rotation)
    ;;       (translate [715.65 85.25 2.5])
    ;;       (-#))
      ;; (->> (import "../parts/Kailh LP Choc Burnt Orange.stl")
      ;;      (rdx 90)
      ;;      (rdz -90)
      ;;      (klor-key-place 0 0)
      ;;      (translate [0 0 (+ klor-switchplate-z-position (/ klor-switchplate-thickness  2))])) 
      ;; (->> (import "../parts/klor1_3-klor1_3.stl")
      ;;      (rdz 10)
      ;;      (translate [-31 -4.5 0]))
      ;(plot-bezier-points (vals (klor-tps-43-wall-control-points :bl :west :offset [0 1 0]) ) (sphere 1))
      ;(translate (klor-tps-43-position (tps-43-mount-spacing :bl) :height klor-case-walls-height)  (sphere 1))
      ;(translate (klor-tps-43-position (mapv + [0 2 0](tps-43-mount-spacing :bl)) :height klor-case-walls-height)  (sphere 1))
      ;(translate (klor-tps-43-position (mapv + [2 0 0] (tps-43-mount-spacing :bl)) :height klor-case-walls-height)  (sphere 1))
      ;(translate (klor-tps-43-position (mapv + key-spacing-inner-west (tps-43-mount-spacing :bm)) :height klor-case-walls-height)  (sphere 1))
     (apply union
            (for [column columns
                  row rows
                  :when (false? (and (= row 2) (= column 5)))]
              (translate [0 0 (- klor-switchplate-z-position (/ klor-switchplate-thickness 2))]  (klor-key-place column row key-shape))))
     (apply union
            (for [thumb-key thumb-keys]
              (translate [0 0 (- klor-switchplate-z-position (/ klor-switchplate-thickness 2))] (-# (klor-thumb-place thumb-key key-shape)))))
     ;(color [0 1 0 1] (klor-key-place-with-offset 0 2 [-30 7 11] (import "../parts/oled.stl")))
     ;(color [0 1 0 1](klor-oled-place (import "../parts/oled.stl") :height klor-case-walls-height))
     (klor-tps-43-place (translate [0 0 1](tps-43-mount))) 
     
     
     
     
     
     (trackpad-attachnent-walls wall-cross-section-steps wall-section-steps)
     )
    )
  ))

(comment (- klor-thumb-walls-height klor-switchplate-thickness))