(ns dactyl-keyboard.tps-65-breakout
  (:refer-clojure :exclude [use import])
  (:require
   [scad-clj.scad :refer :all]
   [scad-clj.model :refer :all]
   [dactyl-keyboard.utils :refer :all]
   [dactyl-keyboard.lib.transformations :refer [rdz]]) 
  )


(def tps-65-breakout-mounting-hole-y-coordinate -63.5)
(def tps-65-breakout-mounting-hole-position-left [124.714 tps-65-breakout-mounting-hole-y-coordinate])
(def tps-65-breakout-mounting-hole-position-right [139.192 tps-65-breakout-mounting-hole-y-coordinate])
(def tps-65-breakout-mounting-hole-mid-x-coordinate (/ (+ (nth tps-65-breakout-mounting-hole-position-left 0) (nth tps-65-breakout-mounting-hole-position-right 0)) -2))
(def tps-65-breakout-mounting-hole-position-diameter 2)
(def tps-65-breakout-mounting-standoff-inner-diameter 3.2)
(def tps-65-breakout-mounting-standoff-outer-diameter (+ tps-65-breakout-mounting-standoff-inner-diameter 2))
(def tps-65-breakout-mounting-standoff-inner-radius (/ tps-65-breakout-mounting-standoff-inner-diameter 2))
(def tps-65-breakout-mounting-standoff-outer-radius (/ tps-65-breakout-mounting-standoff-outer-diameter 2))
(def tps-65-breakout-mounting-standoff-height 4)
(def tps-65-breakout (translate [0 0 4] (import "../parts/Tps-65-breakout.stl")))

(def tps-65-breakout-support-bar 
  (->>
   (cube 4 tps-65-breakout-mounting-standoff-outer-diameter tps-65-breakout-mounting-standoff-height)
   (translate [(- tps-65-breakout-mounting-hole-mid-x-coordinate)
               (- tps-65-breakout-mounting-hole-y-coordinate 4) 
               (/ tps-65-breakout-mounting-standoff-height 2)])))
(def tps-65-breakout-mounting-standoffs
  (multiple-standoffs tps-65-breakout-mounting-standoff-inner-radius
                      tps-65-breakout-mounting-standoff-outer-radius
                      tps-65-breakout-mounting-standoff-height
                      [tps-65-breakout-mounting-hole-position-left tps-65-breakout-mounting-hole-position-right]))

(defn tps-65-breakout-place [shape]
  (->> shape
       (translate [tps-65-breakout-mounting-hole-mid-x-coordinate  
              (- (nth tps-65-breakout-mounting-hole-position-left 1))
              0] )
       (rdz -90)
       (translate [-85 -15 0] )))
(spit "things-low/tps-65-breakout.scad"
      (write-scad
       (tps-65-breakout-place (union
                        ;; (let [cyl (binding [*fn* 36] 
                        ;;             (cylinder tps-65-breakout-mounting-standoff-inner-radius tps-65-breakout-mounting-standoff-height :center false))]
                        ;;   (hull
                        ;;    (translate tps-65-breakout-mounting-hole-position-left cyl)
                        ;;    (translate tps-65-breakout-mounting-hole-position-right cyl)))
        ;(translate tps-65-breakout-mounting-hole-position-left (binding [*fn* 36](cylinder 2 8)))
        ;(translate tps-65-breakout-mounting-hole-position-right (binding [*fn* 36] (cylinder 2 8)))
                               (translate [0 0 4] (import "../parts/Tps-65-breakout.stl"))
                               tps-65-breakout-mounting-standoffs
                               tps-65-breakout-support-bar
                               ))
       ))








