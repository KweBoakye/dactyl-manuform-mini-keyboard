(ns dactyl-keyboard.3528-led-holder
   (:refer-clojure :exclude [use import])
(:require [clojure.core.matrix :refer [array matrix mmul]]
          [scad-clj.scad :refer :all]
          [scad-clj.model :refer :all]
          ))

(def led-width 3.5)
(def led-length 2.8)
(def led-depth 1.9)
(def tolerance 0.2)
(def led-space-width  (+ led-width  tolerance))
(def led-space-length (+ led-length tolerance))
(def led-space-depth (+ led-depth tolerance 0.1))
(def keyswitch-width 14.15)
(def led-space (cube led-space-width led-space-length led-space-depth :center false))
(def led-holder-bottom-thickness 2.5)
(def led-holder-wall-thickness 2.5)
(def led-holder-width keyswitch-width)
(def led-holder 
  (difference (cube led-holder-width (+ (* led-holder-wall-thickness 2) led-space-length) (+ led-holder-bottom-thickness led-depth) :center false)
              (translate [(/ (- led-holder-width led-space-width) 2) led-holder-wall-thickness led-holder-bottom-thickness] led-space) 
))

;(def 3528-led-holder)

(spit "things/led-holder.scad"
      (write-scad led-holder))

(defn -main [dum] 1)  ; dummy to make it easier to batch