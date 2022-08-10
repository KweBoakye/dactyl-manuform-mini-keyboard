(ns dactyl-keyboard.vybronics-vl91022
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]))


(def vybronics-vl91022-x-axis 22.7)
(def vybronics-vl91022-y-axis 9.1)
(def vybronics-vl91022-z-axis 10.1)

(def vybronics-vl91022-mount-x-axis (+ vybronics-vl91022-x-axis plate-thickness ))
(def vybronics-vl91022-mount-y-axis (+ vybronics-vl91022-y-axis plate-thickness))
(def vybronics-vl91022-mount-z-axis (+ vybronics-vl91022-z-axis plate-thickness))

(def vybronics-vl91022-body 
  (->> 
   (cube vybronics-vl91022-x-axis vybronics-vl91022-y-axis vybronics-vl91022-z-axis)
   (translate [0 0 (/ vybronics-vl91022-z-axis 2)])))

(def vybronics-vl91022-mount-body
  (->> 
   (cube vybronics-vl91022-mount-x-axis vybronics-vl91022-mount-y-axis vybronics-vl91022-mount-z-axis)
   (translate [0 0 (/ vybronics-vl91022-z-axis 2) (/ plate-thickness 2)])))

(def vybronics-vl91022-mount-body-subtract
  (->>
   (cube (- vybronics-vl91022-x-axis 2) vybronics-vl91022-y-axis vybronics-vl91022-z-axis)
   (translate [0 0 (/ vybronics-vl91022-z-axis 2)])))

(def top-subtraction
  (->> 
   (cube (+ vybronics-vl91022-mount-x-axis 0.2) (+ vybronics-vl91022-mount-y-axis 0.2) plate-thickness)
   (translate [0 0 (+ (/ plate-thickness 2) (- vybronics-vl91022-mount-z-axis plate-thickness))])
   ))

;vybronics-vl91022-mount-body

(def vybronics-vl91022-mount 
  (difference
   vybronics-vl91022-mount-body
   vybronics-vl91022-body
   top-subtraction
   (translate [0 0 (/ plate-thickness 2)] vybronics-vl91022-mount-body-subtract)
    (translate [0 (/ vybronics-vl91022-mount-y-axis 2) 0 ] vybronics-vl91022-mount-body-subtract)
   (translate [-1 (/ (- vybronics-vl91022-mount-y-axis) 2) 0] vybronics-vl91022-mount-body-subtract)
   (translate [1 (/ (- vybronics-vl91022-mount-y-axis) 2) 0] vybronics-vl91022-mount-body-subtract)
    (translate [(/ (- vybronics-vl91022-mount-x-axis) 2) 0 0] vybronics-vl91022-mount-body-subtract)
(translate [(/ vybronics-vl91022-mount-x-axis 2) 0 0] vybronics-vl91022-mount-body-subtract)
   )
  ) 
