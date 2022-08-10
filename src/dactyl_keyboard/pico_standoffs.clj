(ns dactyl-keyboard.pico-standoffs
  (:refer-clojure :exclude [use import])
   (:require [clojure.core.matrix :refer [array matrix mmul]]
             [scad-clj.scad :refer :all]
             [scad-clj.model :refer :all]
             [dactyl-keyboard.utils :refer :all]
   ))


(def pico-pcb-width 21)
(def pico-pcb-length 51)
(def pico-pcb-thickness 1)
(def pico-micro-usb-connecter-width 8)
(def pico-micro-usb-connecter-overhang 1.3)
(def pico-micro-usb-connecter-thickness 2) ;this is an estimation not measured 
(def pico-micro-usb-connecter-distance-from-edge (/ (- pico-pcb-width pico-micro-usb-connecter-width) 2))
(def pico-distance-between-centre-of-mounting-holes 17.78)
(def pico-mounting-hole-diameter 2.1)
(def pico-mounting-hole-radius (/ pico-mounting-hole-diameter 2))
(def pico-horizontal-distance-from-edge-to-centre-of-mounting-hole (+ (/ (- pico-pcb-width pico-distance-between-centre-of-mounting-holes) 2) pico-mounting-hole-radius))
(def pico-vertical-distance-from-edge-to-centre-of-mounting-hole 2)
(def pico-standoff-inner-diameter 3.5)
(def pico-standoff-outer-diameter (+ pico-standoff-inner-diameter 2))
(def pico-standoff-inner-radius (/ pico-standoff-inner-diameter 2))
(def pico-standoff-outer-radius (/ pico-standoff-outer-diameter 2))

(def pico-mounting-hole 
  (->>
   (binding [*fn* 36] (cylinder pico-mounting-hole-radius pico-pcb-thickness :center false))
   (translate [0 0 0])
   ))
(def pico-standoff-hole (binding [*fn* 36] (cylinder pico-standoff-inner-radius 4 :center false)))
(def pico-standoff-shell (binding [*fn* 36] (cylinder pico-standoff-outer-radius 4 :center false)))

(def pico-top-left-mounting-hole-position [(- pico-horizontal-distance-from-edge-to-centre-of-mounting-hole (/ pico-pcb-width 2)) (- (/ pico-pcb-length 2) pico-vertical-distance-from-edge-to-centre-of-mounting-hole) 0])
(def pico-top-right-mounting-hole-position [(-  (/ pico-pcb-width 2) pico-horizontal-distance-from-edge-to-centre-of-mounting-hole) (- (/ pico-pcb-length 2) pico-vertical-distance-from-edge-to-centre-of-mounting-hole) 0])
(def pico-bottom-left-mounting-hole-position [(- pico-horizontal-distance-from-edge-to-centre-of-mounting-hole (/ pico-pcb-width 2)) (- pico-vertical-distance-from-edge-to-centre-of-mounting-hole (/ pico-pcb-length 2) ) 0])
(def pico-bottom-right-mounting-hole-position [(- (/ pico-pcb-width 2) pico-horizontal-distance-from-edge-to-centre-of-mounting-hole) (- pico-vertical-distance-from-edge-to-centre-of-mounting-hole (/ pico-pcb-length 2)) 0])

(def pico-top-left-mounting-hole (translate pico-top-left-mounting-hole-position pico-mounting-hole))
(def pico-top-right-mounting-hole (translate pico-top-right-mounting-hole-position pico-mounting-hole))
(def pico-bottom-left-mounting-hole (translate pico-bottom-left-mounting-hole-position pico-mounting-hole))
(def pico-bottom-right-mounting-hole (translate pico-bottom-right-mounting-hole-position pico-mounting-hole))

(def pico-standoff 
  (difference
   pico-standoff-shell
   pico-standoff-hole))

(def pico-top-left-standoff (translate pico-top-left-mounting-hole-position pico-standoff))
(def pico-top-right-standoff (translate pico-top-right-mounting-hole-position pico-standoff))
(def pico-bottom-left-standoff (translate pico-bottom-left-mounting-hole-position pico-standoff))
(def pico-bottom-right-standoff (translate pico-bottom-right-mounting-hole-position pico-standoff))

(def pico-standoffs
  (union
   pico-top-left-standoff
   pico-top-right-standoff
   pico-bottom-left-standoff
   pico-bottom-right-standoff
   )
  )

(def pico-placeholder-body
  (->> (cube pico-pcb-width pico-pcb-length pico-pcb-thickness)
       (translate [0 0 (/ pico-pcb-thickness 2)])))

(def pico-placeholder-usb-connecter-overhang 
  (->> (cube pico-micro-usb-connecter-width pico-micro-usb-connecter-overhang pico-micro-usb-connecter-thickness)
       (translate [0 (+ (/ pico-micro-usb-connecter-overhang 2) (/ pico-pcb-length 2)) (+ (/ pico-micro-usb-connecter-thickness 2) pico-pcb-thickness)])))

(def pico-placeholder
  (difference
   (union
   pico-placeholder-body
   pico-placeholder-usb-connecter-overhang
   )
   pico-top-left-mounting-hole
   pico-top-right-mounting-hole
   pico-bottom-left-mounting-hole
   pico-bottom-right-mounting-hole 
   )
  )

(def pico-standoff-test
  (union
   (translate [0 0 4] pico-placeholder)
   pico-standoffs
   ))

(defn  pico-standoffs-place [shape]
  (translate [-70 -20 0] shape)
  )