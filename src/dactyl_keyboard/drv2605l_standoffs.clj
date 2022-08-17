(ns dactyl-keyboard.drv2605l-standoffs
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]))

(def drv2605l-width 17.78)
(def drv2605l-length 16.51)
(def drv2605l-thickness 2)
(def drv2605l-distance-between-centre-of-standoffs 12.7)
(def drv2605l-distance-from-bottom-edge-to-centre-of-standoffs 13.97)
(def drv2605l-distance-from-top-edge-to-centre-of-standoffs (- drv2605l-length drv2605l-distance-from-bottom-edge-to-centre-of-standoffs))
(def drv2605l-hole-diameter 2.5)
(def drv2605l-hole-radius (/ drv2605l-hole-diameter 2))

(def drv2605l-standoff-inner-diameter 3.5)
(def drv2605l-standoff-outer-diameter (+ drv2605l-standoff-inner-diameter 2))
(def drv2605l-standoff-inner-radius (/ drv2605l-standoff-inner-diameter 2))
(def drv2605l-standoff-outer-radius (/ drv2605l-standoff-outer-diameter 2))


(def drv2605l-left-mounting-position [(- (/ drv2605l-distance-between-centre-of-standoffs 2))
                                      (- (/ drv2605l-length 2) drv2605l-distance-from-top-edge-to-centre-of-standoffs)
                                      0])
(def drv2605l-right-mounting-position [(/ drv2605l-distance-between-centre-of-standoffs 2)
                                      (- (/ drv2605l-length 2) drv2605l-distance-from-top-edge-to-centre-of-standoffs)
                                      0])

(def drv2605l-hole (binding [*fn* 36] (cylinder drv2605l-hole-radius (+ drv2605l-thickness 0.4) :center false)))

(def drv2605l-left-hole (translate (add-vec drv2605l-left-mounting-position [0 0 -0.2])
                                   drv2605l-hole))

(def drv2605l-right-hole (translate (add-vec drv2605l-right-mounting-position [0 0 -0.2])
                                    drv2605l-hole))

(def drv2605l-standoff-hole (binding [*fn* 36] (cylinder drv2605l-standoff-inner-radius 4.4 :center false)))
(def drv2605l-standoff-shell (binding [*fn* 36] (cylinder drv2605l-standoff-outer-radius 4 :center false)))

(def drv2605l-standoff-bar
  (->>
   (cube drv2605l-standoff-outer-diameter (- drv2605l-distance-from-bottom-edge-to-centre-of-standoffs 4) 4)
   (translate [0 (- (- (/ drv2605l-length 2) drv2605l-distance-from-top-edge-to-centre-of-standoffs 1)) 2])
   ))


(def drv2605l-standoff
  (difference
   (union
    drv2605l-standoff-shell
    drv2605l-standoff-bar)
   drv2605l-standoff-hole))


;(def drv2605l-standoff-bar-left (translate [(- (/ drv2605l-distance-between-centre-of-standoffs 2)) -1 2] drv2605l-standoff-bar))
;(def drv2605l-standoff-bar-right (translate [ (/ drv2605l-distance-between-centre-of-standoffs 2) -1 2] drv2605l-standoff-bar))


(def drv2605l-left-standoff (translate drv2605l-left-mounting-position drv2605l-standoff))
(def drv2605l-right-standoff (translate drv2605l-right-mounting-position drv2605l-standoff))

(def drv2605l-standoffs
  (union 
   drv2605l-left-standoff
  drv2605l-right-standoff 
   )
  )

(def drv2605l-standoffs-test
  (union 
   drv2605l-standoffs
   (color [1 0 0 1] 
          ( translate [(- (/ drv2605l-width 2)) (- (/ drv2605l-length 2)) 4] (import "2305 DRV2605L.stl")))
   )
  )

(def drv2605l-standoffs-print-test
  (union
   (translate [0 0 drv2605l-thickness] drv2605l-standoffs)
   (difference
    (->>
     (cube (+ drv2605l-width 1) (- drv2605l-length 3.5) drv2605l-thickness)
     (translate [0 2.25 (/ drv2605l-thickness 2)]))
    (->>
     (cube 6 10 drv2605l-thickness)
     (translate [0 2.25 (/ drv2605l-thickness 2)])
     )
    )
   )
  )

(def drv2605l-body 
  (->>
   (cube drv2605l-width drv2605l-length drv2605l-thickness)
   (translate [0 0 (/ drv2605l-thickness 2)])))

(def drv2605l 
  (union
   (translate [(/ drv2605l-width 2) (/ drv2605l-length 2) 0](difference
   drv2605l-body
   drv2605l-left-hole
   drv2605l-right-hole
   ))
   (color [1 0 0 1](import "2305 DRV2605L.stl"))
   ))

(defn drv2605l-place [shape]
  (translate [-90 -40 0] shape)
  )