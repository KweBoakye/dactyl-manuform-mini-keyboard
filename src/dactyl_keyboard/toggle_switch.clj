(ns dactyl-keyboard.toggle-switch
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.oled :refer :all]
            [dactyl-keyboard.case :refer :all]
            [unicode-math.core :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Toggle Switch        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(def toggle-switch-main-body-x 15.92)
(def toggle-switch-main-body-y 28.10)
(def toggle-switch-main-body-z 19.5)

(def toggle-switch-connector-length 8.94)
(def toggle-switch-connector-width 6.3)
(def toggle-switch-connector-thickness 0.85)

(def toggle-switch-cover-base-x 17.35)
(def toggle-switch-cover-base-y 40.9)
(def toggle-switch-cover-base-thickness 1.5)

(def toggle-switch-cover-length 40.6)
(def toggle-switch-cover-width 14.4)
(def toggle-switch-cover-height 27.8)

(def toggle-switch-panel-mount-screw-diameter 12.2)
(def toggle-switch-panel-mount-screw-radius (/ toggle-switch-panel-mount-screw-diameter 2))
(def toggle-switch-panel-mount-screw-height 12.5)
(def toggle-switch-hole-diameter (+ toggle-switch-panel-mount-screw-diameter 0.2))
(def toggle-switch-hole-radius (/ toggle-switch-hole-diameter 2))


(def toggle-switch-main-body
  (cube toggle-switch-main-body-x
        toggle-switch-main-body-y
        toggle-switch-main-body-z))

(def toggle-switch-ground-connecter
  (->> (cube toggle-switch-connector-width toggle-switch-connector-length toggle-switch-connector-thickness)
       (translate [0 (- (+ (/ toggle-switch-main-body-y 2) (/ toggle-switch-connector-length 2))) (/ toggle-switch-main-body-z 2)])))

(def toggle-switch-positive-connecter
  (->> (cube toggle-switch-connector-width toggle-switch-connector-thickness toggle-switch-connector-length)
       (translate [0 (- (/ toggle-switch-main-body-y 2)) (- (+ (/ toggle-switch-main-body-z 2) (/ toggle-switch-connector-length 2)))])))

(def toggle-switch-led-connecter
  (->> (cube toggle-switch-connector-width toggle-switch-connector-thickness toggle-switch-connector-length)
       (translate [0 (/ toggle-switch-main-body-y 2) (- (+ (/ toggle-switch-main-body-z 2) (/ toggle-switch-connector-length 2)))])))

(def toggle-switch-panel-mount-screw
  (->>
   (binding [*fn* 36] (cylinder toggle-switch-panel-mount-screw-radius toggle-switch-panel-mount-screw-height :center false))
   (translate [0 0 (/ toggle-switch-main-body-z 2)])))

(def toggle-switch
  (union
   toggle-switch-main-body
   toggle-switch-ground-connecter
   toggle-switch-positive-connecter
   toggle-switch-led-connecter
   toggle-switch-panel-mount-screw))

(defn rotate-toggle-around-x [should-rotate-x shape]
  (if should-rotate-x (rotate oled-mount-rotation-x-old [1 0 0] shape) shape))
(defn toggle-switch-place [shape should-rotate-x translate-x translate-z]
  (->> shape
       (rotate (deg2rad -90) [0 1 0])
       (rotate-toggle-around-x should-rotate-x)
       (translate (left-wall-plate-position  -1 -3))
       (rotate oled-mount-rotation-z-old [0 0 1])
       (translate [(+ 10 translate-x) 10 (+ -20 translate-z)])))

(def upper-toggle-switch-z-position 1)
(def lower-toggle-switch-z-position -23)

(def toggle-switch-hole (binding [*fn* 36] (cylinder toggle-switch-hole-radius plate-thickness :center false)))

(def upper-toggle-switch-hole (translate [(- (+ (/ toggle-switch-main-body-z 2))) 0 0] (toggle-switch-place (translate [0 (+ (/ (- toggle-switch-main-body-y) 2) 7.64) 0] toggle-switch-hole) true 0 upper-toggle-switch-z-position)))
(def lower-toggle-switch-hole (translate [(- (+ (/ toggle-switch-main-body-z 2))) 0 0] (toggle-switch-place (translate [0 (+ (/ (- toggle-switch-main-body-y) 2) 7.64) 0] toggle-switch-hole) false 0 lower-toggle-switch-z-position)))

(def toggle-switch-cover-base

  (cube toggle-switch-cover-base-x toggle-switch-cover-base-y toggle-switch-cover-base-thickness))


(def upper-toggle-switch-cover-base-cutout (translate [(- (+ (/ toggle-switch-main-body-z 2) (/ plate-thickness 2) toggle-switch-cover-base-thickness 0.1)) 0 0] (toggle-switch-place toggle-switch-cover-base true 0 upper-toggle-switch-z-position)))
(def lower-toggle-switch-cover-base-cutout (translate [(- (+ (/ toggle-switch-main-body-z 2) (/ plate-thickness 2) toggle-switch-cover-base-thickness 0.1)) 0 0] (toggle-switch-place toggle-switch-cover-base false 0 lower-toggle-switch-z-position)))

(def toggle-switch-cover (cube toggle-switch-cover-width toggle-switch-cover-length toggle-switch-cover-height))
(def upper-toggle-switch-cover (translate [(- toggle-switch-main-body-z) 0 0] (toggle-switch-place toggle-switch-cover true 0 upper-toggle-switch-z-position)))
(def lower-toggle-switch-cover (translate [(- toggle-switch-main-body-z) 0 0] (toggle-switch-place toggle-switch-cover false 0 lower-toggle-switch-z-position)))