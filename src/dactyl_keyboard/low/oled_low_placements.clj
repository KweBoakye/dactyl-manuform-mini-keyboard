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
(def oled-translation-vector [0 0 (+ (/ oled-holder-thickness -2) plate-thickness)])

(def oled-post (->> (web-post-shape-with-size oled-holder-thickness oled-post-size 36)
                    (translate oled-translation-vector )))

(def oled-post-adj (/ oled-post-size 2))
(def oled-post-tr-translation-vector  [(- (/ mount-width 1.95)  oled-post-adj) (- (/ mount-height 1.95) oled-post-adj) 0] )
(def oled-post-tl-translation-vector  [(+ (/ mount-width -1.95) oled-post-adj) (- (/ mount-height 1.95) oled-post-adj) 0] )
(def oled-post-bl-translation-vector  [(+ (/ mount-width -1.95) oled-post-adj) (+ (/ mount-height -1.95) oled-post-adj) 0])
(def oled-post-br-translation-vector  [(- (/ mount-width 1.95)  oled-post-adj) (+ (/ mount-height -1.95) oled-post-adj) 0])
(def oled-post-bm-translation-vector  [0 (+ (/ mount-height -1.95) oled-post-adj) 0])
  
(def oled-post-tr (translate oled-post-tr-translation-vector oled-post))
(def oled-post-tl (translate oled-post-tl-translation-vector oled-post))
(def oled-post-bl (translate oled-post-bl-translation-vector  oled-post))
(def oled-post-br (translate oled-post-br-translation-vector  oled-post))
(def oled-post-bm (translate oled-post-bm-translation-vector  oled-post))

(def curve-post-size (/ oled-post-size 2))
(def curve-post-translation-vector [0 0 (+ (/ oled-post-size -2)  plate-thickness)])
(def curve-post (->> (binding [*fn* sphere-fn-value](sphere curve-post-size))
                    (translate curve-post-translation-vector)))

(def curve-post-adj (/ oled-post-size 2))
(def curve-post-tr-translation-vector  [(- (/ mount-width 1.95)  curve-post-adj) (- (/ mount-height 1.95) curve-post-adj) 0] )
(def curve-post-tl-translation-vector  [(+ (/ mount-width -1.95) curve-post-adj) (- (/ mount-height 1.95) curve-post-adj) 0] )
(def curve-post-bl-translation-vector  [(+ (/ mount-width -1.95) curve-post-adj) (+ (/ mount-height -1.95) curve-post-adj) 0] )
(def curve-post-br-translation-vector  [(- (/ mount-width 1.95)  curve-post-adj) (+ (/ mount-height -1.95) curve-post-adj) 0] )
(def curve-post-bm-translation-vector  [0 (+ (/ mount-height -1.95) curve-post-adj) 0])

(def curve-post-tr (translate curve-post-tr-translation-vector curve-post))
(def curve-post-tl (translate curve-post-tl-translation-vector curve-post))
(def curve-post-bl (translate curve-post-bl-translation-vector  curve-post))
(def curve-post-br (translate curve-post-br-translation-vector  curve-post))
(def curve-post-bm (translate curve-post-bm-translation-vector  curve-post))

(def sphere-post (->> (sphere (/ oled-post-size 2))
                     (translate [0 0 (+ (/ oled-holder-thickness -2) plate-thickness)])))

(def sphere-post-adj (/ oled-post-size 2))
(def sphere-post-tr (translate [(- (/ mount-width 1.95)  sphere-post-adj) (- (/ mount-height 1.95) sphere-post-adj) 0] sphere-post))
(def sphere-post-tl (translate [(+ (/ mount-width -1.95) sphere-post-adj) (- (/ mount-height 1.95) sphere-post-adj) 0] sphere-post))
(def sphere-post-bl (translate [(+ (/ mount-width -1.95) sphere-post-adj) (+ (/ mount-height -1.95) sphere-post-adj) 0] sphere-post))
(def sphere-post-br (translate [(- (/ mount-width 1.95)  sphere-post-adj) (+ (/ mount-height -1.95) sphere-post-adj) 0] sphere-post))

(def round-thumb-post (->> (web-post-shape-with-size web-thickness oled-post-size 36)
                    (translate [0 0 (+ (/ web-thickness -2) plate-thickness)])))
(def round-thumb-post-tr (translate [(- (/ mount-width 1.95)  oled-post-adj) (- (+ (/ mount-height 1.95) (/ oled-post-size 2)) oled-post-adj) 0] round-thumb-post))
(def round-thumb-post-tl (translate [(+ (/ mount-width -1.95) oled-post-adj) (- (+ (/ mount-height 1.95) (/ oled-post-size 2)) oled-post-adj) 0] round-thumb-post))
(def round-thumb-post-bl (translate [(+ (/ mount-width -1.95) oled-post-adj) (+ (/ mount-height -1.95) oled-post-adj) 0] round-thumb-post))
(def round-thumb-post-br (translate [(- (/ mount-width 1.95)  oled-post-adj) (+ (/ mount-height -1.95) oled-post-adj) 0] round-thumb-post))