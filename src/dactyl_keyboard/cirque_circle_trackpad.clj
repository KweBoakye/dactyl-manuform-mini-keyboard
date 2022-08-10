(ns dactyl-keyboard.cirque-circle-trackpad
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.sa-keycaps :refer :all]
            ;[dactyl-keyboard.placement-functions :refer :all]
            ;[dactyl-keyboard.web-connecters :refer :all]
           ; [dactyl-keyboard.case :refer :all]
            [unicode-math.core :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cirque Circle Trackpad ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def cirque-circle-trackpad-diameter 23.20)
(def cirque-circle-trackpad-TM040040-diameter 40)
(def cirque-circle-trackpad-TM040040-curved-overlay-diameter 40)
(def cirque-circle-trackpad-TM040040-curved-overlay-gap 2)
(def cirque-circle-trackpad-hole-diameter (+ cirque-circle-trackpad-diameter 0.8))
(def cirque-circle-trackpad-hole-radius (/ cirque-circle-trackpad-hole-diameter 2))
(def cirque-circle-trackpad-TM040040-hole-radius (/ cirque-circle-trackpad-TM040040-diameter 2))
(def cirque-circle-trackpad-first-notch-position-angle (- 30 90))
(def cirque-circle-trackpad-second-notch-position-angle (+ cirque-circle-trackpad-first-notch-position-angle 30))
(def cirque-circle-trackpad-third-notch-position-angle (+ cirque-circle-trackpad-second-notch-position-angle 30 135))
(def cirque-circle-trackpad-notch-diameter 0.6)
(def cirque-circle-trackpad-depth-to-asic 1.15)
(def cirque-circle-trackpad-depth-including-asic 2.2)
(def cirque-circle-trackpad-horizontal-distance-from-top-of-pad-to-bottom-of-asic 18.55)
(def asic-length 10.5)
(def asic-width 10.5)
(def asic-depth (- cirque-circle-trackpad-depth-including-asic cirque-circle-trackpad-depth-to-asic))
(def cirque-circle-trackpad-distance-from-bottom-edge-to-connector 2.0)
(def cirque-TM040040-circle-trackpad-distance-from-bottom-edge-to-connector 5.98)
(def cirque-circle-trackpad-connector-width 9.75)
(def cirque-circle-trackpad-connector-length 3.75)
(def cirque-circle-trackpad-depth-to-asic-plus-tolerance (+ cirque-circle-trackpad-depth-to-asic 0.05))
(def cirque
  (cylinder (/ 23.5 2) cirque-circle-trackpad-depth-to-asic-plus-tolerance :center false))


(def cirque-TM040040
  (binding [*fn* 36] (cylinder (+ cirque-circle-trackpad-TM040040-hole-radius 0.3) cirque-circle-trackpad-depth-to-asic-plus-tolerance :center false)))


(def cirque-clearance
  (binding [*fn* 36] (cylinder cirque-circle-trackpad-hole-radius 5 :center false)))

(def cirque-TM040040-clearance
  (binding [*fn* 36] (cylinder (+ cirque-circle-trackpad-TM040040-hole-radius 0.3) 16 :center false)))

(def cirque-TM040040-under-clearance
  (binding [*fn* 36] (cylinder (- cirque-circle-trackpad-TM040040-hole-radius 0) 14 :center false)))

(def cirque-connector-clearance
  (->>
   (cube cirque-circle-trackpad-connector-width cirque-circle-trackpad-connector-length 7)
   (translate [0
               (+ (- cirque-circle-trackpad-hole-radius) cirque-circle-trackpad-distance-from-bottom-edge-to-connector (/ cirque-circle-trackpad-connector-length 2))
               (+ -3.5 0.1)])))

(def cirque-TM040040-connector-clearance
  (->>
   (cube cirque-circle-trackpad-connector-width cirque-circle-trackpad-connector-length 7)
   (translate [0
               (+ (- cirque-circle-trackpad-TM040040-hole-radius) cirque-TM040040-circle-trackpad-distance-from-bottom-edge-to-connector (- (/ cirque-circle-trackpad-connector-length 2)))
               (+ -3.5 0.1)])))

(def cirque-circle-trackpad-notch-holder
  (->> (binding [*fn* 36] (cylinder (- (/ cirque-circle-trackpad-notch-diameter 2) 0.05) cirque-circle-trackpad-depth-to-asic-plus-tolerance :center false))
       (translate [0 cirque-circle-trackpad-hole-radius 0])))

(def cirque-circle-trackpad-first-notch (rotate (deg2rad cirque-circle-trackpad-first-notch-position-angle) [0 0 1] cirque-circle-trackpad-notch-holder))
(def cirque-circle-trackpad-second-notch (rotate (deg2rad cirque-circle-trackpad-second-notch-position-angle) [0 0 1] cirque-circle-trackpad-notch-holder))
(def cirque-circle-trackpad-third-notch (rotate (deg2rad cirque-circle-trackpad-third-notch-position-angle) [0 0 1] cirque-circle-trackpad-notch-holder))

(def cirque-circle-trackpad-test-cube
  (->> (cube (+ cirque-circle-trackpad-hole-diameter 3) (+ cirque-circle-trackpad-hole-diameter 3) (* cirque-circle-trackpad-depth-to-asic-plus-tolerance 2))
       (translate [0 0 -0.1])))




(def cirque-test
  (union
   (difference
    cirque-circle-trackpad-test-cube
    (translate [0 0 -1] cirque-clearance)
    cirque-connector-clearance)

 ;  cirque-circle-trackpad-first-notch
;cirque-circle-trackpad-second-notch
;cirque-circle-trackpad-third-notch
   ))

(def asic-cutout
  (->>
   (cube asic-width asic-length asic-depth)
   (translate [0
               (+ cirque-circle-trackpad-TM040040-hole-radius (- cirque-circle-trackpad-horizontal-distance-from-top-of-pad-to-bottom-of-asic) (/ asic-length 2))
               (- (+ cirque-circle-trackpad-depth-to-asic-plus-tolerance 0.15)  (/ asic-depth 2))])))

(def cirque-TM040040-mount
  (difference
   (binding [*fn* 36] (cylinder (+ cirque-circle-trackpad-TM040040-hole-radius 2) (+ (* cirque-circle-trackpad-depth-to-asic-plus-tolerance 2) 0.7) :center false))
   asic-cutout
   (translate [0 0 -0.2] asic-cutout)
   (translate [0 0 (+ cirque-circle-trackpad-depth-to-asic-plus-tolerance 2)] cirque-TM040040)
   (translate [0 0 (+ cirque-circle-trackpad-depth-to-asic-plus-tolerance 0.1 asic-depth)] cirque-TM040040)))

(def cirque-TM040040-mount-height 10)





(def cirque-TM040040-mount-walls
  (->>
   (difference
    (binding [*fn* 36] (cylinder (+ cirque-circle-trackpad-TM040040-hole-radius 2) (+ cirque-TM040040-mount-height 2) :center false))
    (translate [0 0 -0.1] (binding [*fn* 36] (cylinder cirque-circle-trackpad-TM040040-hole-radius (+ cirque-TM040040-mount-height 0.2 2) :center false))))
   (translate [0 0 (- (+ cirque-TM040040-mount-height 2))])))




