(ns dactyl-keyboard.RP2040-Plus
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.low.case-low :refer :all]
            ))


(def rp2040-plus-width 21.00)
(def rp2040-plus-length 51.00)
(def rp2040-plus-thickness 2)
(def rp2040-plus-pin-below-depth 1.5)
(def rp2040-plus-usb-connecter-width 8.95)
(def rp2040-plus-horizontal-distance-from-right-edge-to-usb-connecter 6.17)
(def rp2040-plus-horizontal-distance-from-left-edge-to-usb-connecter (- rp2040-plus-width rp2040-plus-usb-connecter-width rp2040-plus-horizontal-distance-from-right-edge-to-usb-connecter))
(def rp2040-plus-mount-thickness wall-thickness)
(def rp2040-plus-mount-width (+ rp2040-plus-width rp2040-plus-mount-thickness))
(def rp2040-plus-mount-length (+ rp2040-plus-length rp2040-plus-mount-thickness))
(def rp2040-plus-mount-depth 8)
(def rp2040-plus-cutout-width (+ rp2040-plus-width 0.4))
(def rp2040-plus-cutout-length (+ rp2040-plus-length 0.4))
(def rp2040-plus-cutout-depth (- rp2040-plus-mount-depth rp2040-plus-mount-thickness))

(def rp2040-plus-body
  (->> 
   (cube rp2040-plus-width rp2040-plus-length rp2040-plus-thickness)
   (translate [0 0 (/ rp2040-plus-thickness 2)])
   )
)

(def rp2040-plus-usb-c-connecter
  (->>
   (import "USB_Type_C_Female_Connector.stl")
   (rdz 180)
   (translate [(- (/ rp2040-plus-width 2) (/ rp2040-plus-usb-connecter-width 2) rp2040-plus-horizontal-distance-from-right-edge-to-usb-connecter) (- (/ rp2040-plus-length 2) 4) 0] )
   ))
;(- (/ rp2040-plus-width 2) rp2040-plus-horizontal-distance-from-right-edge-to-usb-connecter)
(def rp2040-plus
  (translate [ 0 0 (+ rp2040-plus-mount-thickness rp2040-plus-pin-below-depth) ]
   
   (union
   rp2040-plus-body
   rp2040-plus-usb-c-connecter
   ;(color [1 0 0 1](translate
   ;                 [(- (/ rp2040-plus-width 2) (/ rp2040-plus-usb-connecter-width 2) rp2040-plus-horizontal-distance-from-right-edge-to-usb-connecter) (- (/ rp2040-plus-length 2) 4) (+ rp2040-plus-thickness 4.5)] (cube rp2040-plus-usb-connecter-width 9 9) ))
   ))
   )

(def rp2040-plus-mount-body
  (->>
   (cube rp2040-plus-mount-width rp2040-plus-mount-length rp2040-plus-mount-depth)
   (translate [0 0 (/ rp2040-plus-mount-depth 2)])))

(def rp2040-plus-connecter-cutout
  (->> 
   (cube rp2040-plus-usb-connecter-width 15 rp2040-plus-cutout-depth)
   (translate [0 0 (/ rp2040-plus-cutout-depth 2)])
   (translate [0 0 rp2040-plus-mount-thickness])
   )
)

(def  rp2040-plus-cutout
  (->> 
   (cube rp2040-plus-cutout-width rp2040-plus-cutout-length rp2040-plus-cutout-depth)
   (translate [0 0 (/ rp2040-plus-cutout-depth 2)])
   (translate [0 0 rp2040-plus-mount-thickness])
   )
)

(def rp2040-plus-mount
  (difference 
   rp2040-plus-mount-body
   rp2040-plus-cutout
   rp2040-plus-connecter-cutout
   )
  )

(defn rp2040-plus-place [shape]
  (->> shape
   (usb-jack-place )
       (translate [0 -27.5 -6.5])
   )
  )