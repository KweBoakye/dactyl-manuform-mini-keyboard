(ns dactyl-keyboard.joystick
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            ))

;Not Used(scrapped and used joycon instead)

;;;;;;;;;;;;;;;;
;; Joystick   ;;
;;;;;;;;;;;;;;;;

(def joystick-pcb-length 34)
(def joystick-pcb-width 26)
(def joystick-pcb-thickness 1.5)
(def joystick-pcb-joystick-pot-to-left-side-width 4)
(def joystick-pcb-joystick-pot-width 16)
(def joystick-pcb-box-to-right 6)
(def joystick-pcb-bottom-to-pots 9)
(def joystick-pcb-joystick-pots-length 16)
(def joystick-pcb-pots-to-top 9)
(def joystick-pcb-screw-hole-diameter 3)
(def joystick-pcb-screw-hole-radius (/ joystick-pcb-screw-hole-diameter 2))
(def joystick-pcb-screw-hole-distance-from-pcb-horizontal-edge 1.5)
(def joystick-pcb-screw-hole-distance-from-pcb-veritical-edge-top 1.5)
(def joystick-pcb-screw-hole-distance-from-pcb-veritical-edge-bottom 3)
(def joystick-pcb-screw-hole-bottom-y-position  (+ joystick-pcb-screw-hole-distance-from-pcb-veritical-edge-bottom joystick-pcb-screw-hole-radius))
(def joystick-pcb-screw-hole-top-y-position (- (- joystick-pcb-length joystick-pcb-screw-hole-distance-from-pcb-veritical-edge-top) joystick-pcb-screw-hole-radius))
(def joystick-pcb-screw-hole-left-x-position (+ joystick-pcb-screw-hole-distance-from-pcb-horizontal-edge joystick-pcb-screw-hole-radius))
(def joystick-pcb-screw-hole-right-x-position (- (- joystick-pcb-width joystick-pcb-screw-hole-distance-from-pcb-horizontal-edge) joystick-pcb-screw-hole-radius))

(def joystick-max-diameter 26)
(def joystick-max-radius (/ joystick-max-diameter 2))
(def joystick-module-depth (- 14 joystick-pcb-thickness))
(def joystick-umbrella-cap-height 8.25)
(def joystick-stick-cylinder-height 5)
(def joystick-stick-cylinder-diameter 11)
(def joystick-stick-cylinder-radius (/ joystick-stick-cylinder-diameter 2))
(def joystick-cap-height 6)
(def joystick-cap-diameter 19.5)
(def joystick-cap-radius (/ joystick-cap-diameter 2))
(def joystick-pcb-to-top-of-joystick-umbrella-height 21)
(def joystick-horizontal-centre (+ joystick-pcb-joystick-pot-to-left-side-width (/ joystick-pcb-joystick-pot-width 2)))
(def joystick-vertictal-centre (+ joystick-pcb-bottom-to-pots (/ joystick-pcb-joystick-pots-length 2)))
(def joystick-umbrella-y-position (+ (- joystick-pcb-to-top-of-joystick-umbrella-height  joystick-umbrella-cap-height) joystick-pcb-thickness))
(def joystick-cylinder-y-position (+ joystick-umbrella-y-position joystick-umbrella-cap-height))
(def joystick-cap-y-position (+ joystick-cylinder-y-position joystick-stick-cylinder-height))

(def joystick-pcb-screw-hole
  (->>
   (binding [*fn* 36] (cylinder joystick-pcb-screw-hole-radius (+ joystick-pcb-thickness 0.3) :center false))
   (translate [0 0 -0.1])))

(def bottom-left-joystick-pcb-screw-hole
  (translate [joystick-pcb-screw-hole-left-x-position
              joystick-pcb-screw-hole-bottom-y-position
              0] joystick-pcb-screw-hole))

(def bottom-right-joystick-pcb-screw-hole
  (translate [joystick-pcb-screw-hole-right-x-position
              joystick-pcb-screw-hole-bottom-y-position
              0] joystick-pcb-screw-hole))

(def top-left-joystick-pcb-screw-hole
  (translate [joystick-pcb-screw-hole-left-x-position
              joystick-pcb-screw-hole-top-y-position
              0] joystick-pcb-screw-hole))

(def top-right-joystick-pcb-screw-hole
  (translate [joystick-pcb-screw-hole-right-x-position
              joystick-pcb-screw-hole-top-y-position
              0] joystick-pcb-screw-hole))

(def joystick-pcb-screw-hole-pillar
  (->>
   (binding [*fn* 36] (cylinder (+ joystick-pcb-screw-hole-radius 2) (+  joystick-module-depth 4) :center false))
   (translate [0 0 joystick-pcb-thickness])))

(def bottom-left-joystick-pcb-screw-hole-pillar
  (translate [joystick-pcb-screw-hole-left-x-position
              joystick-pcb-screw-hole-bottom-y-position
              0] joystick-pcb-screw-hole-pillar))

(def bottom-right-joystick-pcb-screw-hole-pillar
  (translate [joystick-pcb-screw-hole-right-x-position
              joystick-pcb-screw-hole-bottom-y-position
              0] joystick-pcb-screw-hole-pillar))

(def top-left-joystick-pcb-screw-hole-pillar
  (translate [joystick-pcb-screw-hole-left-x-position
              joystick-pcb-screw-hole-top-y-position
              0] joystick-pcb-screw-hole-pillar))

(def top-right-joystick-pcb-screw-hole-pillar
  (translate [joystick-pcb-screw-hole-right-x-position
              joystick-pcb-screw-hole-top-y-position
              0] joystick-pcb-screw-hole-pillar))

(def joystick-pot
  (->>
   (cube joystick-pcb-joystick-pot-width joystick-pcb-joystick-pots-length joystick-module-depth :center false)
   (translate [joystick-pcb-joystick-pot-to-left-side-width joystick-pcb-bottom-to-pots joystick-pcb-thickness])))

(def joystick-pcb
  (difference
   (cube joystick-pcb-width joystick-pcb-length joystick-pcb-thickness :center false)
   bottom-left-joystick-pcb-screw-hole
   bottom-right-joystick-pcb-screw-hole
   top-left-joystick-pcb-screw-hole
   top-right-joystick-pcb-screw-hole))


(def joystick-umbrella
  (->>
   (binding [*fn* 36] (cylinder [joystick-max-radius joystick-stick-cylinder-radius] joystick-umbrella-cap-height :center false))
   (translate [joystick-horizontal-centre joystick-vertictal-centre joystick-umbrella-y-position])))

(def joystick-cylinder
  (->>
   (binding [*fn* 36] (cylinder joystick-stick-cylinder-radius joystick-stick-cylinder-height :center false))
   (translate [joystick-horizontal-centre joystick-vertictal-centre joystick-cylinder-y-position])))


(def joystick-cap
  (->>
   (binding [*fn* 36] (cylinder joystick-cap-radius joystick-cap-height :center false))
   (translate [joystick-horizontal-centre joystick-vertictal-centre joystick-cap-y-position])))

(def joystick-module
  (union
   joystick-pcb
   joystick-pot
   joystick-umbrella
   joystick-cylinder
   joystick-cap
   bottom-left-joystick-pcb-screw-hole-pillar
   bottom-right-joystick-pcb-screw-hole-pillar
   top-left-joystick-pcb-screw-hole-pillar
   top-right-joystick-pcb-screw-hole-pillar))