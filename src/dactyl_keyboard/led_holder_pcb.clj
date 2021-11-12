(ns dactyl-keyboard.led-holder-pcb
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]))

;;;;;;;;;;;;;;;;;;;;;
;;  3D Printed PCB ;;
;;;;;;;;;;;;;;;;;;;;;


(def kailh-switch-length 15.60)
(def kailh-switch-width 13.95)
(def kailh-switch-mount-hole-diameter 3.9)
(def kailh-switch-mount-hole-radius (/ kailh-switch-mount-hole-diameter 2))
(def vertical-distance-from-kailh-switch-mount-hole-center-to-first-pin 2.54)
(def vertical-distance-from-kailh-switch-mount-hole-center-to-second-pin 5.08)
(def horizontal-distance-from-kailh-switch-mount-hole-center-to-first-pin 3.81)
(def horizontal-distance-from-kailh-switch-mount-hole-center-to-second-pin  2.54)
(def vertical-distance-from-kailh-switch-mount-hole-center-to-centre-of-led-slot 4.96)

(def kailh-hotswap-width 10.90)
(def kailh-hotswap-width-including-contacts 14.50)
(def kailh-hotswap-long-right-side-length 5.89)
(def kailh-hotswap-short-left-side-length 4.00)
(def kailh-hotswap-total-depth 3.05)
(def kailh-hotswap-main-body-depth 1.80)
(def kailh-hotswap-non-main-body-depth (- kailh-hotswap-total-depth kailh-hotswap-main-body-depth))
(def kailh-hotswap-mount-diameter 2.90)
(def kailh-hotswap-mount-radius (/ kailh-hotswap-mount-diameter 2))


(def kailh-switch-mount-hole (->>
                              (binding [*fn* 36] (cylinder (+ kailh-switch-mount-hole-radius 0.4) 5 :center false))
                              (translate [0 0 -0.1])))

(def kailh-hotswap-mount-hole (binding [*fn* 36] (cylinder (+ kailh-hotswap-mount-radius 0.4)  5 :center false)))
(def kailh-hotswap-long-right-side-hole (translate [horizontal-distance-from-kailh-switch-mount-hole-center-to-first-pin
                                                    (- vertical-distance-from-kailh-switch-mount-hole-center-to-first-pin)
                                                    -0.1] kailh-hotswap-mount-hole))

(def kailh-hotswap-short-left-side-hole (translate [(- horizontal-distance-from-kailh-switch-mount-hole-center-to-second-pin)
                                                    (- vertical-distance-from-kailh-switch-mount-hole-center-to-second-pin)
                                                    -0.1] kailh-hotswap-mount-hole))

(def led-width 5.0)
(def led-length 5.0)
(def led-depth 1.9)
(def tolerance 0.4)
(def led-space-width  (+ led-width  tolerance))
(def led-space-length (+ led-length tolerance))
(def led-space-depth (- led-depth 0.3 0.1))
(def kailh-keyswitch-width 14.15)
(def led-space (->>
                (cube led-space-width led-space-length led-space-depth)
                (translate [0 vertical-distance-from-kailh-switch-mount-hole-center-to-centre-of-led-slot (+ (/ led-space-depth 2) (- kailh-hotswap-total-depth led-space-depth) 0.1)])))
(def led-wire-channel-length (- led-width 0.4))
(def led-wire-channel-length-offset (/ (- led-space-length led-wire-channel-length) 2))
(def led-wire-channel-width (+ kailh-switch-width 0.2))
(def led-wire-channel (->> (cube led-wire-channel-width led-wire-channel-length  led-space-depth)
                           (translate [0 vertical-distance-from-kailh-switch-mount-hole-center-to-centre-of-led-slot (+ (/ led-space-depth 2) (- kailh-hotswap-total-depth led-space-depth) 0.1)])))

(def led-wire-cutout-length 1.0)
(def led-wire-cutout-width 1.0)
(def led-wire-cutout-width-offset (+ (/ led-width 2) (/ led-wire-cutout-width 2)))

(def led-wire-cutout (->> (cube led-wire-cutout-length led-wire-cutout-width (+ kailh-hotswap-total-depth 0.1))
                          (translate [0 0 (/ kailh-hotswap-total-depth 2)])))

(def top-left-led-wire-cutout (translate [(- led-wire-cutout-width-offset) (+ vertical-distance-from-kailh-switch-mount-hole-center-to-centre-of-led-slot (- (/ led-wire-channel-length 2) (/ led-wire-cutout-length 2))) 0] led-wire-cutout))
(def bottom-left-led-wire-cutout (translate [(- led-wire-cutout-width-offset) (- vertical-distance-from-kailh-switch-mount-hole-center-to-centre-of-led-slot (- (/ led-wire-channel-length 2) (/ led-wire-cutout-length 2))) 0] led-wire-cutout))
(def top-right-led-wire-cutout (translate [led-wire-cutout-width-offset (+ vertical-distance-from-kailh-switch-mount-hole-center-to-centre-of-led-slot (- (/ led-wire-channel-length 2) (/ led-wire-cutout-length 2))) 0] led-wire-cutout))
(def bottom-right-led-wire-cutout (translate [led-wire-cutout-width-offset (- vertical-distance-from-kailh-switch-mount-hole-center-to-centre-of-led-slot (- (/ led-wire-channel-length 2) (/ led-wire-cutout-length 2))) 0] led-wire-cutout))
(def led-holder-bottom-thickness 2.5)
(def led-holder-wall-thickness 2.5)
(def led-holder-width kailh-keyswitch-width)
(def side-cutout (->>
                  (cube led-space-width led-space-length  (+ kailh-hotswap-total-depth 0.2))
                  (translate [0 (+ vertical-distance-from-kailh-switch-mount-hole-center-to-centre-of-led-slot 0.2) (/ kailh-hotswap-total-depth 2)])))
(def left-side-cutout (translate [(- led-space-width) 0 0] side-cutout))
(def right-side-cutout (translate [led-space-width  0 0] side-cutout))
;(def led-holder
;  (difference (cube led-holder-width (+ (* led-holder-wall-thickness 2) led-space-length) (+ led-holder-bottom-thickness led-depth))
;              (translate [(/ (- led-holder-width led-space-width) 2) led-holder-wall-thickness led-holder-bottom-thickness] led-space)))

(def kailh-hotswap-cutout-shape (->>
                                 (cube (+ kailh-switch-width 0.2) (+ (/ kailh-switch-length 2) 0.1) (+ kailh-hotswap-main-body-depth 0.2))
                                 (translate [0 (- (+ (/ kailh-switch-length 4) 0.1)) (- (/ kailh-hotswap-main-body-depth 2) 0.1)])))


(def pcb-main-body
  (->> (cube kailh-switch-width kailh-switch-length kailh-hotswap-total-depth)
       (translate [0 0 (/ kailh-hotswap-total-depth 2)])))

(def three-D-printed-pcb
  (difference
   pcb-main-body
   (union
    kailh-switch-mount-hole
    kailh-hotswap-long-right-side-hole
    kailh-hotswap-short-left-side-hole
    led-space
    kailh-hotswap-cutout-shape
    led-wire-channel
    top-left-led-wire-cutout
    bottom-left-led-wire-cutout
    top-right-led-wire-cutout
    bottom-right-led-wire-cutout
    left-side-cutout
    right-side-cutout)))