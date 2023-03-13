(ns dactyl-keyboard.vybronics-vl91022
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.dovetail :refer :all]
            [dactyl-keyboard.tps-65 :refer :all]
            [dactyl-keyboard.lib.curvesandsplines.beziers :refer [bezier-quadratic]]
            [dactyl-keyboard.lib.openscad.polyhedrons :refer [generate-bezier-to-point-polyhedron]]
            ))


(def vybronics-vl91022-x-axis 23.1)
(def vybronics-vl91022-y-axis 10.1)
(def vybronics-vl91022-z-axis 10.1)
(def tolerance 0.4)
(def vybronics-vl91022-subtraction-x-axis (+ vybronics-vl91022-x-axis tolerance))
(def vybronics-vl91022-subtraction-y-axis (+ vybronics-vl91022-y-axis tolerance))
(def vybronics-vl91022-subtraction-z-axis (+ vybronics-vl91022-z-axis tolerance))
(def vybronics-vl91022-mount-x-axis (+ vybronics-vl91022-x-axis plate-thickness ))
(def vybronics-vl91022-mount-y-axis (+ vybronics-vl91022-y-axis plate-thickness))
(def vybronics-vl91022-mount-z-axis (+ vybronics-vl91022-z-axis (/ plate-thickness 2)))

(def vybronics-vl91022-body 
  (->> 
   (cube vybronics-vl91022-x-axis vybronics-vl91022-y-axis vybronics-vl91022-z-axis)
   (translate [0 0 (/ vybronics-vl91022-z-axis 2)])))

(def vybronics-vl91022-mount-body
  (->> 
   (cube vybronics-vl91022-mount-x-axis vybronics-vl91022-mount-y-axis (/  vybronics-vl91022-z-axis 2))
   (translate [0 0 (/ vybronics-vl91022-z-axis 4)])))

(def vybronics-vl91022-mount-body-subtract
  (->>
   (cube vybronics-vl91022-subtraction-x-axis vybronics-vl91022-subtraction-y-axis vybronics-vl91022-subtraction-z-axis)
   (translate [0 0 (/ vybronics-vl91022-subtraction-z-axis 2)])))

(def vybronics-vl91022-mount-hole-cover 
   (cube vybronics-vl91022-x-axis vybronics-vl91022-y-axis tps-65-depth)
   )

(def top-subtraction
  (->> 
   (cube (+ vybronics-vl91022-mount-x-axis 0.2) (+ vybronics-vl91022-mount-y-axis 0.2) plate-thickness)
   (translate [0 0 (+ (/ plate-thickness 2) (- vybronics-vl91022-mount-z-axis plate-thickness))])
   ))

(def holder
  (let [holder-tolerance 0.4
        steps 12
        inner-z (+ vybronics-vl91022-z-axis holder-tolerance)
        outer-z vybronics-vl91022-mount-z-axis
        outer-x (+ vybronics-vl91022-mount-x-axis holder-tolerance)
        inner-x (- vybronics-vl91022-x-axis 2.0)
        outer-y vybronics-vl91022-mount-y-axis 
        holder-x (- (/ outer-x 2) (/ inner-x 2)) 
        inner-y (+ vybronics-vl91022-y-axis holder-tolerance)
        quad-curve-from-points #(bezier-quadratic (nth % 0) (nth % 1) (nth % 2) steps)
        curve-points-pos-y [[(/ inner-x 2)   (/ outer-y 2) outer-z]
                            [(/ outer-x 2) (/ outer-y 2) outer-z] 
                            [(/ outer-x 2) (/ outer-y 2) inner-z]]
        pos-y-target-point [(/ inner-x 2)   (/ outer-y 2) inner-z]
        curve-points-neg-y [[(/ inner-x 2)   (/ outer-y -2) outer-z]
                            [(/ outer-x 2) (/ outer-y -2) outer-z] 
                            [(/ outer-x 2) (/ outer-y -2) inner-z]] 
        neg-y-target-point [(/ inner-x 2)   (/ outer-y -2) inner-z]
        curve-pos-y (quad-curve-from-points curve-points-pos-y)
        curve-neg-y (quad-curve-from-points curve-points-neg-y)
        curved-poly (generate-bezier-to-point-polyhedron curve-pos-y pos-y-target-point curve-neg-y neg-y-target-point)
        holder-back (translate [(+ (/ inner-x 2) (/ holder-x 2)) 0 (/ inner-z 2)] (cube holder-x outer-y inner-z)) 
        ]
        (union
         curved-poly
         holder-back
         
         (mirror [1 0 0]
                 curved-poly
                 holder-back
                 )
         )
        ) 
  )

(def vybronics-vl91022-holder-leg
  (let [leg (cube plate-thickness plate-thickness vybronics-vl91022-mount-z-axis)] 
  (for [x [-2 2] y [-2 2]]
    (translate (div-vec [vybronics-vl91022-mount-x-axis vybronics-vl91022-mount-y-axis vybronics-vl91022-mount-z-axis] [x y 2]) leg)
    )
    )
)

;vybronics-vl91022-mount-body

(def position [0 0 (/ plate-thickness -4)])
(def dimension [vybronics-vl91022-y-axis (/ vybronics-vl91022-x-axis 2) (/ plate-thickness 2)])
(def tooth-count 2)
(def tooth-height 2)
(def tooth-clearance 0.5)
(def tooth-settings [tooth-count tooth-height tooth-clearance])
(def vybronics-vl91022-support-bar
  (->> 
   (cube tps-65-mount-length vybronics-vl91022-mount-y-axis  tps-65-depth)
   (translate [0 0   (+ (/ tps-65-depth -2)  (+ tps-65-trackpad-only-thickness tps-65-depth-tolerance))])))
(def vybronics-vl91022-mount 
  (let [
        ]
    (difference 
    (union
     ;(-# vybronics-vl91022-body)
     holder 
      vybronics-vl91022-mount-body 
     vybronics-vl91022-support-bar
     (translate [0 0 (- tps-65-depth)]vybronics-vl91022-support-bar)
     )
     (translate [0 0 -4] vybronics-vl91022-mount-body-subtract)
     vybronics-vl91022-mount-body-subtract
     )
;;     (difference
;;    vybronics-vl91022-mount-body
   
;;     (-# (translate [(- (/ (- vybronics-vl91022-x-axis 2) -2) tooth-height) 0 0](rdz -90 (cutter position dimension tooth-settings false false))))
;;      (-# (mirror [1 0 0] (-# (translate [(- (/ (- vybronics-vl91022-x-axis 2) -2) tooth-height) 0 0] (rdz -90 (cutter position dimension tooth-settings false false))))))
    
;;    vybronics-vl91022-body
;;    top-subtraction
;;    (translate [0 0 (/ plate-thickness 2)] vybronics-vl91022-mount-body-subtract)
;;    (translate [0 0 (- (/ plate-thickness 2))] vybronics-vl91022-mount-body-subtract)
;;     (translate [0 (/ vybronics-vl91022-mount-y-axis 2) 0 ] vybronics-vl91022-mount-body-subtract)
;;    (translate [-1 (/ (- vybronics-vl91022-mount-y-axis) 2) 0] vybronics-vl91022-mount-body-subtract)
;;    (translate [1 (/ (- vybronics-vl91022-mount-y-axis) 2) 0] vybronics-vl91022-mount-body-subtract)
;;     (translate [(/ (- vybronics-vl91022-mount-x-axis) 2) 0 0] vybronics-vl91022-mount-body-subtract)
;; (translate [(/ vybronics-vl91022-mount-x-axis 2) 0 0] vybronics-vl91022-mount-body-subtract))
    )
  ) 
