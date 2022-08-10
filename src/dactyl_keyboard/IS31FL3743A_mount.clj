(ns dactyl-keyboard.IS31FL3743A-mount
  (:refer-clojure :exclude [use import])
   (:require [clojure.core.matrix :refer [array matrix mmul]]
             [scad-clj.scad :refer :all]
             [scad-clj.model :refer :all]
             [dactyl-keyboard.utils :refer :all]
   ))

(def IS31FL3743A-width 21.33)
(def IS31FL3743A-length 46.75)
(def IS31FL3743A-thickness 1)
(def IS31FL3743A-mounting-hole-diameter  3)
(def IS31FL3743A-mounting-hole-radius  (/ IS31FL3743A-mounting-hole-diameter  2))
(def IS31FL3743A-standoff-height 4)
(def IS31FL3743A-standoff-inner-diameter 5)
(def IS31FL3743A-standoff-inner-radius (/ IS31FL3743A-standoff-inner-diameter 2))
(def IS31FL3743A-standoff-outer-diameter (+ IS31FL3743A-standoff-inner-diameter 2))
(def IS31FL3743A-standoff-outer-radius (/ IS31FL3743A-standoff-outer-diameter 2))
(def IS31FL3743A-mounting-hole-vertical-mounting-distance-from-center 19)
(def IS31FL3743A-top-mounting-hole-horizontal-distance-from-center 1)
(def IS31FL3743A-bottom-mounting-hole-horizontal-distance-from-center -2)

(def IS31FL3743A-mounting-hole 
  (->>
   (binding [*fn* 36] (cylinder IS31FL3743A-mounting-hole-radius IS31FL3743A-thickness :center false))
   (translate [0 0 (/ IS31FL3743A-thickness)]) 
   ))

(def IS31FL3743A-standoff-inner-hole (binding [*fn* 36] (cylinder IS31FL3743A-standoff-inner-radius IS31FL3743A-standoff-height :center false)))
(def IS31FL3743A-standoff-outer-shell (binding [*fn* 36] (cylinder IS31FL3743A-standoff-outer-radius IS31FL3743A-standoff-height :center false)))

(def IS31FL3743A-standoff 
  (difference
   IS31FL3743A-standoff-outer-shell
   IS31FL3743A-standoff-inner-hole
   )
  )

(def IS31FL3743A-upper-mounting-hole-position [IS31FL3743A-top-mounting-hole-horizontal-distance-from-center IS31FL3743A-mounting-hole-vertical-mounting-distance-from-center])
(def IS31FL3743A-bottom-mounting-hole-position [IS31FL3743A-bottom-mounting-hole-horizontal-distance-from-center (- IS31FL3743A-mounting-hole-vertical-mounting-distance-from-center)])

(def IS31FL3743A-mounting-top-hole (translate IS31FL3743A-upper-mounting-hole-position IS31FL3743A-mounting-hole))
(def IS31FL3743A-mounting-bottom-hole (translate IS31FL3743A-bottom-mounting-hole-position IS31FL3743A-mounting-hole))

(def IS31FL3743A-standoff-top (translate IS31FL3743A-upper-mounting-hole-position IS31FL3743A-standoff))
(def IS31FL3743A-standoff-bottom (translate IS31FL3743A-bottom-mounting-hole-position IS31FL3743A-standoff))

(def IS31FL3743A-pcb-placeholder-body 
  (->>
   (cube IS31FL3743A-width IS31FL3743A-length IS31FL3743A-thickness)
   (translate [0 0 (/ IS31FL3743A-thickness 2)])
   )
  )


(def IS31FL3743A-pcb-placeholder
 (difference
  IS31FL3743A-pcb-placeholder-body
  IS31FL3743A-mounting-top-hole
  IS31FL3743A-mounting-bottom-hole
  ) 
  )

(def IS31FL3743A-standoffs
  (union
   IS31FL3743A-standoff-top
   IS31FL3743A-standoff-bottom
   )
  )

(defn IS31FL3743A-standoff-place [shape]
  (translate [-45 -20 0] shape)
  )


(def IS31FL3743A-standoff-test
  (union
   (translate [0 0 IS31FL3743A-standoff-height] IS31FL3743A-pcb-placeholder)
   IS31FL3743A-standoffs
   )
  )