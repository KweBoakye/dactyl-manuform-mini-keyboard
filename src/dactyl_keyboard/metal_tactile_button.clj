(ns dactyl-keyboard.metal-tactile-button
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]))

(def metal-tactile-button-main-body-width 8)
(def metal-tactile-button-main-body-width-with-legs (+ metal-tactile-button-main-body-width 1.4))
(def metal-tactile-button-main-body-length 7.8)
(def metal-tactile-button-main-body-height 2.5 )
(def metal-tactile-button-ball-diameter  3.8)
(def metal-tactile-button-neck-diameter 5.4)
(def metal-tactile-button-distance-from-top-of-ball-to-top-of-neck 1.8)
(def metal-tactile-button-neck-height 2.15)

(def metal-tactile-button-main-body
  (->>
   (cube   metal-tactile-button-main-body-width metal-tactile-button-main-body-length metal-tactile-button-main-body-height)
   (translate [0 0 metal-tactile-button-main-body-height])
   )
  )

(defn metal-tactile-button-main-body-cutout [height]
  (extrude-linear {:height height :convexity 10 :center true} 
                  (square   metal-tactile-button-main-body-width-with-legs metal-tactile-button-main-body-length)
                  )
  )


