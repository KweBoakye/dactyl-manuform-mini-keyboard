(ns dactyl-keyboard.pimoroni-haptic-mount
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            ))


(def ELV1411A-height 3)
(def width-between-holes 13.5)
(def pimoroni-haptic-mount-mount-length 5)
(def pimoroni-haptic-mount-mount-width 20)
(def pimoroni-haptic-mount-hole-diameter 2.5)
(def pimoroni-haptic-mount-hole-radius (/ pimoroni-haptic-mount-hole-diameter 2))

(def pimoroni-haptic-mount-mount-hole (binding [*fn* 36] (cylinder pimoroni-haptic-mount-hole-radius (+ ELV1411A-height 0.2))))
(def pimoroni-haptic-mount-mount-base (cube pimoroni-haptic-mount-mount-width pimoroni-haptic-mount-mount-length ELV1411A-height))
(def pimoroni-haptic-mount-left-hole (translate [(-(/ width-between-holes 2)) 0 -0.1] pimoroni-haptic-mount-mount-hole))
(def pimoroni-haptic-mount-right-hole (translate [(/ width-between-holes 2) 0 -0.1] pimoroni-haptic-mount-mount-hole))
(def pimoroni-haptic-mount 
  (difference 
   pimoroni-haptic-mount-mount-base
   pimoroni-haptic-mount-left-hole
   pimoroni-haptic-mount-right-hole
   ))
