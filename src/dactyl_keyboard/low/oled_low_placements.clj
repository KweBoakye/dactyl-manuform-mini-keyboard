(ns dactyl-keyboard.low.oled-low-placements
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.oled :refer :all]
            [dactyl-keyboard.low.web-connecters-low :refer :all]
             [dactyl-keyboard.switch-hole :refer :all]
            ))

(def screen-holder-position [2, -2, 0])

(when (= oled-type "ST7789-240x240-154")
  (def screen-holder-cut (rotate (deg2rad 0) [1 0 0] (rotate (deg2rad 90) [0 0 1] ST7789-240x240-154-holder-cut)))
  (def screen-holder (rotate (deg2rad 0) [1 0 0] (rotate (deg2rad 90) [0 0 1] ST7789-240x240-154-holder)))
  (def screen-holder-width ST7789-240x240-154-holder-width)
  (def screen-holder-height ST7789-240x240-154-holder-height)
  (def screen-holder-depth ST7789-240x240-154-holder-thickness)
  )

(def oled-post-size 1)

(def oled-post (->> (web-post-shape-with-size oled-holder-thickness oled-post-size 36)
                    (translate [0 0 (+ (/ oled-holder-thickness -2) plate-thickness)])))

(def oled-post-adj (/ oled-post-size 2))
(def oled-post-tr (translate [(- (/ mount-width 1.95)  oled-post-adj) (- (/ mount-height 1.95) oled-post-adj) 0] oled-post))
(def oled-post-tl (translate [(+ (/ mount-width -1.95) oled-post-adj) (- (/ mount-height 1.95) oled-post-adj) 0] oled-post))
(def oled-post-bl (translate [(+ (/ mount-width -1.95) oled-post-adj) (+ (/ mount-height -1.95) oled-post-adj) 0] oled-post))
(def oled-post-br (translate [(- (/ mount-width 1.95)  oled-post-adj) (+ (/ mount-height -1.95) oled-post-adj) 0] oled-post))

(def curve-post (->> (binding [*fn* 36](sphere (/ 0.2 2)))
                    (translate [0 0 (+ (/ (- (/ oled-holder-thickness 2) 0.4) -2) plate-thickness)])))

(def curve-post-adj (/ 0.1 2))
(def curve-post-tr (translate [(- (/ mount-width 1.95)  curve-post-adj) (- (/ mount-height 1.95) curve-post-adj) 0] curve-post))
(def curve-post-tl (translate [(+ (/ mount-width -1.95) curve-post-adj) (- (/ mount-height 1.95) curve-post-adj) 0] curve-post))
(def curve-post-bl (translate [(+ (/ mount-width -1.95) curve-post-adj) (+ (/ mount-height -1.95) curve-post-adj) 0] curve-post))
(def curve-post-br (translate [(- (/ mount-width 1.95)  curve-post-adj) (+ (/ mount-height -1.95) curve-post-adj) 0] curve-post))

(def sphere-post (->> (sphere (/ oled-post-size 2))
                     (translate [0 0 (+ (/ oled-holder-thickness -2) plate-thickness)])))

(def sphere-post-adj (/ oled-post-size 2))
(def sphere-post-tr (translate [(- (/ mount-width 1.95)  sphere-post-adj) (- (/ mount-height 1.95) sphere-post-adj) 0] sphere-post))
(def sphere-post-tl (translate [(+ (/ mount-width -1.95) sphere-post-adj) (- (/ mount-height 1.95) sphere-post-adj) 0] sphere-post))
(def sphere-post-bl (translate [(+ (/ mount-width -1.95) sphere-post-adj) (+ (/ mount-height -1.95) sphere-post-adj) 0] sphere-post))
(def sphere-post-br (translate [(- (/ mount-width 1.95)  sphere-post-adj) (+ (/ mount-height -1.95) sphere-post-adj) 0] sphere-post))