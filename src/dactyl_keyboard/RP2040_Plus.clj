(ns dactyl-keyboard.RP2040-Plus
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.lib.transformations :refer [rdx rdy]]
            [dactyl-keyboard.low.case-low :refer :all]
            [dactyl-keyboard.low.placement-functions-low :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))


(def rp2040-plus-width 21.00)
(def rp2040-plus-length 51.5)
(def rp2040-plus-thickness 1.15)
(def rp2040-plus-pin-below-depth 3)
(def rp2040-plus-usb-connecter-width 9.5)
(def rp2040-plus-usb-connecter-height 4)
(def rp2040-plus-usb-connecter-length 8)
(def rp2040-plus-usb-connecter-vertical-distance-infront-of-board-edge 1)
(def rp2040-plus-horizontal-distance-from-right-edge-to-usb-connecter 6.17)
(def rp2040-plus-horizontal-distance-from-left-edge-to-usb-connecter (- rp2040-plus-width rp2040-plus-usb-connecter-width rp2040-plus-horizontal-distance-from-right-edge-to-usb-connecter))
(def rp2040-plus-mount-thickness wall-thickness)
(def rp2040-plus-mount-width (+ rp2040-plus-width rp2040-plus-mount-thickness))
(def rp2040-plus-mount-length (+ rp2040-plus-length rp2040-plus-mount-thickness))
(def rp2040-plus-mount-depth 8)
(def rp2040-plus-mount-height (+ rp2040-plus-mount-depth rp2040-plus-mount-thickness))
(def rp2040-plus-cutout-width (+ rp2040-plus-width 0.4))
(def rp2040-plus-cutout-length (+ rp2040-plus-length 0.4))
(def rp2040-plus-cutout-depth (- rp2040-plus-mount-depth rp2040-plus-mount-thickness))
(def rp2040-plus-button-cutout-length 11.5)
(def rp2040-plus-button-cutout-width 14)
(def rp2040-plus-button-cutout-height 3)
(def rp2040-plus-button-cutout-vertical-distance-from-top-of-board 16.5)
(def rp2040-plus-button-cutout-horizontal-distance-from-side-of-board (/ (- rp2040-plus-width rp2040-plus-button-cutout-width) 2))
(def rp2040-plus-battery-connecter-height (- 4.7 rp2040-plus-thickness))
(def rp2040-plus-battery-connecter-length 6.4)
(def rp2040-plus-battery-connecter-width 9.00)
(def rp2040-plus-battery-connecter-horizontal-distance-from-side-of-board (/ (- rp2040-plus-width rp2040-plus-battery-connecter-width) 2))
(def rp2040-plus-dupont-pitch 2.54)
(def rp2040-plus-dupont-margin 0.75)
(def rp2040-plus-dupont-count-per-side 20)
(def rp2040-plus-dupont-cutout-height 2.5)
(def rp2040-plus-dupont-cutout-width 3)
(def rp2040-plus-dupont-cutout-length (+ (* rp2040-plus-dupont-pitch rp2040-plus-dupont-count-per-side) (* 2 rp2040-plus-dupont-margin)))

(def rp2040-plus-dupont-cutout 
  (->>
   (cube rp2040-plus-dupont-cutout-width rp2040-plus-dupont-cutout-length rp2040-plus-dupont-cutout-height)
   (translate [0 0 (- (+ rp2040-plus-mount-depth  (/ (- usb-jack-height rp2040-plus-usb-connecter-height) 2)) (/ rp2040-plus-dupont-cutout-height 2))])))



(def rp2040-plus-body
  (->> 
   (cube rp2040-plus-width rp2040-plus-length rp2040-plus-thickness)
   (translate [0 0 (/ rp2040-plus-thickness 2)])
   )
)

(def rp2040-plus-usb-c-connecter
  (->>
  (import "../parts/usb connector type c female 6pin.stl")
   (rdx -90)
   (rdy 180)
   ;(cube rp2040-plus-usb-connecter-width rp2040-plus-usb-connecter-length rp2040-plus-usb-connecter-height)
   (translate [(- (/ rp2040-plus-width 2) (/ rp2040-plus-usb-connecter-width 2) rp2040-plus-horizontal-distance-from-right-edge-to-usb-connecter)
               (+ (/ rp2040-plus-length 2)  rp2040-plus-usb-connecter-vertical-distance-infront-of-board-edge )
               (- (+ (/ (+ rp2040-plus-mount-height  ) 2) 0.4) (/ rp2040-plus-usb-connecter-height 2) (/ rp2040-plus-thickness 2) 
                  )])
  ;;  (translate [(- (/ rp2040-plus-width 2) (/ rp2040-plus-usb-connecter-width 2) rp2040-plus-horizontal-distance-from-right-edge-to-usb-connecter)
  ;;              (+ (/ rp2040-plus-length 2) (/ rp2040-plus-usb-connecter-length -2) rp2040-plus-usb-connecter-vertical-distance-infront-of-board-edge)
  ;;              (- (+ rp2040-plus-usb-connecter-height ;rp2040-plus-thickness
  ;;                    ) 
  ;;                 (/ rp2040-plus-mount-height 2)) ]) 
   ;(translate [(- (/ rp2040-plus-width 2) (/ rp2040-plus-usb-connecter-width 2) rp2040-plus-horizontal-distance-from-right-edge-to-usb-connecter) (- (/ rp2040-plus-length 2) 4) -1] )
   ))
;(- (/ rp2040-plus-width 2) rp2040-plus-horizontal-distance-from-right-edge-to-usb-connecter)

(def rp2040-plus-battery-connecter
  (->>
   (cube rp2040-plus-battery-connecter-width rp2040-plus-battery-connecter-length rp2040-plus-battery-connecter-height)
   (translate [0 (- (/ rp2040-plus-battery-connecter-length 2) (/ rp2040-plus-length 2)) (+ (/ rp2040-plus-battery-connecter-height 2) rp2040-plus-thickness)]) 
   ))

(def rp2040-plus-button-array 
  (->> 
   (cube rp2040-plus-button-cutout-width rp2040-plus-button-cutout-length rp2040-plus-button-cutout-height)
   (translate [0 (- (/ rp2040-plus-length 2) (/ rp2040-plus-button-cutout-length 2) rp2040-plus-button-cutout-vertical-distance-from-top-of-board) (+ (/ rp2040-plus-button-cutout-height 2) rp2040-plus-thickness)])
   )
  )

(def rp2040-plus-centre-cutout
  (->>
   (cube rp2040-plus-button-cutout-width rp2040-plus-length rp2040-plus-button-cutout-height)
   (translate [0 0 (+ (/ rp2040-plus-button-cutout-height -2)  rp2040-plus-mount-depth  (/ (- usb-jack-height rp2040-plus-usb-connecter-height) 2))])))

(def rp2040-plus-position [0 0 (+ rp2040-plus-mount-depth rp2040-plus-thickness (/ (- usb-jack-height rp2040-plus-usb-connecter-height) 2))])
(def rp2040-plus
  (translate [ 0 0 (+ rp2040-plus-mount-depth rp2040-plus-thickness (/ (- usb-jack-height rp2040-plus-usb-connecter-height) 2)) ]
   
   (rdy 180 (union
   rp2040-plus-body
   ;rp2040-plus-usb-c-connecter
    rp2040-plus-battery-connecter
     rp2040-plus-button-array)
   ;(color [1 0 0 1](translate
   ;                 [(- (/ rp2040-plus-width 2) (/ rp2040-plus-usb-connecter-width 2) rp2040-plus-horizontal-distance-from-right-edge-to-usb-connecter) (- (/ rp2040-plus-length 2) 4) (+ rp2040-plus-thickness 4.5)] (cube rp2040-plus-usb-connecter-width 9 9) ))
   ))
   )


(def rp2040-plus-mount-body
  (->>
   (cube rp2040-plus-mount-width rp2040-plus-mount-length rp2040-plus-mount-height)
   (translate [0 0 (/ rp2040-plus-mount-height 2)])))
(def rp2040-plus-mount-body-clearance
  (->>
   (cube 4 (+ rp2040-plus-mount-length 0.5) (+ rp2040-plus-mount-height 0.5))
   (translate [(+  (/ rp2040-plus-mount-width -2) 1.5) 0 (/ (+ rp2040-plus-mount-height 0.5) 2)])))

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
(def rp2040-plus-mount-front-cutout
  (->> 
   (cube rp2040-plus-mount-width (/ rp2040-plus-thickness 2) rp2040-plus-mount-height)
   (translate [0 (- (/ rp2040-plus-mount-length 2) (/ rp2040-plus-thickness 4)) (/ rp2040-plus-mount-height 2)])
   )
  )

(def rp2040-plus-usb-c-connecter-cutout
  (->>
   ;(import "USB_Type_C_Female_Connector.stl")
   ;(rdz 180)
   (cube rp2040-plus-usb-connecter-width rp2040-plus-usb-connecter-length rp2040-plus-usb-connecter-height)
   (translate [(- (/ rp2040-plus-width 2) (/ rp2040-plus-usb-connecter-width 2) rp2040-plus-horizontal-distance-from-right-edge-to-usb-connecter)
               (+ (/ rp2040-plus-length 2) (/ rp2040-plus-usb-connecter-length -2) rp2040-plus-usb-connecter-vertical-distance-infront-of-board-edge)
               (+ (/ rp2040-plus-usb-connecter-height 2)  (/ rp2040-plus-mount-depth 2))]))) 

(def rp2040-plus-usb-c-connecter-wider-cutout
  (->>
   ;(import "USB_Type_C_Female_Connector.stl")
   ;(rdz 180)
   (cube rp2040-plus-width (* rp2040-plus-usb-connecter-length 2) rp2040-plus-thickness)
   (translate [0
               (+ (/ rp2040-plus-length 2) (- rp2040-plus-usb-connecter-length) )
               (- (+ rp2040-plus-mount-depth  (/ (- usb-jack-height rp2040-plus-usb-connecter-height) 2)) (/ rp2040-plus-thickness 2))])
   ;(translate [(- (/ rp2040-plus-width 2) (/ rp2040-plus-usb-connecter-width 2) rp2040-plus-horizontal-distance-from-right-edge-to-usb-connecter) (- (/ rp2040-plus-length 2) 4) -1] )
   ))

(def rp2040-plus-mount
  (difference
    (translate [0 0 0] rp2040-plus-mount-body)
   (translate [(+ (/ rp2040-plus-width -2) (/ rp2040-plus-dupont-cutout-width 2)) 0 0] rp2040-plus-dupont-cutout)
(translate [(- (/ rp2040-plus-width 2) (/ rp2040-plus-dupont-cutout-width 2)) 0 0] rp2040-plus-dupont-cutout)       
   (translate [0 0 (/ (- rp2040-plus-mount-depth rp2040-plus-mount-thickness) 2)] (cube (- rp2040-plus-width (* rp2040-plus-dupont-cutout-width 2.25)) rp2040-plus-length (- rp2040-plus-mount-depth rp2040-plus-mount-thickness)))
   ;rp2040-plus-cutout
    (translate rp2040-plus-position (cube rp2040-plus-width rp2040-plus-mount-length (+ rp2040-plus-mount-thickness (/ rp2040-plus-thickness 2))))
   rp2040-plus
   rp2040-plus-mount-front-cutout
   ;(translate [0 5 0] rp2040-plus-mount-front-cutout)
   rp2040-plus-centre-cutout
    rp2040-plus-usb-c-connecter-wider-cutout
   ;(translate [0 4 0] rp2040-plus-usb-c-connecter-wider-cutout)
    rp2040-plus-connecter-cutout
   rp2040-plus-usb-c-connecter-cutout
   )
  )

(defn rp2040-plus-place [shape &{:keys [place-fn] :or {place-fn usb-jack-place}}]
  (->> shape
       (translate [-0.35 (- (+  (/ rp2040-plus-length 2) rp2040-plus-mount-thickness rp2040-plus-usb-connecter-vertical-distance-infront-of-board-edge ))  (+ -6.5 )])
   (place-fn )
       
   
       ;(translate (rotate-around-z far-index-splay [-0.5 (first far-index-post-splay-translation) 0]))
       )
  )

(comment 
  (spit "things-low/rp2040-plus-mount-test.scad"
        (write-scad
         rp2040-plus-mount
         )))