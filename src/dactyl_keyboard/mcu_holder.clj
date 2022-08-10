(ns dactyl-keyboard.mcu-holder
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.shape-parameters :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.placement-functions :refer :all]
            [dactyl-keyboard.case :refer :all]
            [dactyl-keyboard.web-connecters :refer :all]
            [unicode-math.core :refer :all]))

;(def usb-holder-position (map + [(+ 18.8 holder-offset) 18.7 1.3] [(first usb-holder-ref) (second usb-holder-ref) 2]))
;(def usb-holder-space  (rotate (deg2rad 90) [1 0 0](translate (map + usb-holder-position [-1.5 (* -1 wall-thickness) 2.9]) (cube 28.666 30 12.4))))
;(def usb-holder-notch  (translate (map + usb-holder-position [-1.5 (+ 4.4 notch-offset) 2.9]) (cube 31.366 1.3 12.4)))
;(def trrs-notch        (translate (map + usb-holder-position [-10.33 (+ 3.6 notch-offset) 6.6]) (cube 8.4 2.4 19.8)))





;(def usb-holder-size [19 33.65 5.5]);[5.5 33.65 19])	;;5.5 33.34 18.4
;(def usb-hole-size [19 33.65 9.5]);[9.5 33.65 19]) ;;9.5 33.34 18.4
;(def usb-hole-size-left [8.0 35.6 9.5]);[9.5 35.6 8.0]) ;;9.5 35.6 8.0
;(def usb-hole-size-right [10.0 35.6 6]);[6 35.6 10.0]) ;;6 35.6 10.0

(def elite-c-holder-position (key-position 1 1 (map + (wall-locate1 -2 (- 4.9 (* 0.2 nrows))) [0 (/ mount-height 2) 0])))
(def elite-c-holder-size [5.5 33.65 19])  ;;5.5 33.34 18.4
(def elite-c-hole-size [9.5 33.65 19]) ;;9.5 33.34 18.4
(def elite-c-hole-size-left [9.5 35.6 8.0]) ;;9.5 35.6 8.0
(def elite-c-hole-size-right [6 35.6 10.0]) ;;6 35.6 10.0

(def blackpill-holder-position (key-position 1 1 (map + (wall-locate1 0 (- 0 (* 0.2 nrows))) [0 (+ (/ mount-height 2) 1.5) 0 ])))
(def blackpill-holder-size [21.2 53.2 5.5])
(def blackpill-hole-size [21.2 53.2 10.5])
(def blackpill-hole-size-left [13 57 9.5])
(def blackpill-hole-size-right [12.0 57 9])

(when (= controller-type elite-c)
  (def usb-holder-size elite-c-holder-size)
  (def usb-hole-size elite-c-hole-size)
  (def usb-hole-size-left elite-c-hole-size-left)
  (def usb-hole-size-right elite-c-hole-size-right)
  (def usb-holder-position elite-c-holder-position))

(when (= controller-type blackpill)
  (def usb-holder-size blackpill-holder-size)
  (def usb-hole-size blackpill-hole-size)
  (def usb-hole-size-left blackpill-hole-size-left)
  (def usb-hole-size-right blackpill-hole-size-right)
  (def usb-holder-position blackpill-holder-position))

(def usb-holder-thickness 5)

(def usb-holder-main-body
  (->>

   (cube (+ (first usb-holder-size) usb-holder-thickness) (+ (second usb-holder-size) 0) (+ (last usb-holder-size) usb-holder-thickness))


   (translate [(first usb-holder-position) (second usb-holder-position) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])
  )
)

(def dovetail-inner-x-position 3)
(def dovetail-outer-x-position 0)
(def dovetail-inner-top-y-position 3.5)
(def dovetail-inner-bottom-y-position 2.5)
(def dovetail-outer-top-y-position 4.5)
(def dovetail-outer-bottom-y-position 1.5)
(def dovetail-lower-z-position 0)
(def dovetail-upper-z-position 12.5)

(def dovetail-clearance-amount 0.2)
(def dovetail-clearance-inner-x-position (+ dovetail-inner-x-position dovetail-clearance-amount))
(def dovetail-clearance-outer-x-position (- dovetail-outer-x-position dovetail-clearance-amount))
(def dovetail-clearance-inner-top-y-position (+ dovetail-inner-top-y-position dovetail-clearance-amount))
(def dovetail-clearance-inner-bottom-y-position (- dovetail-inner-bottom-y-position dovetail-clearance-amount))
(def dovetail-clearance-outer-top-y-position (+ dovetail-outer-top-y-position dovetail-clearance-amount))
(def dovetail-clearance-outer-bottom-y-position (- dovetail-outer-bottom-y-position dovetail-clearance-amount))
(def dovetail-clearance-lower-z-position dovetail-lower-z-position)
(def dovetail-clearance-upper-z-position (+ dovetail-upper-z-position dovetail-clearance-amount))

(def dovetail-inner-bottom-lower-corner [dovetail-inner-x-position,dovetail-inner-bottom-y-position, dovetail-lower-z-position])
(def dovetail-inner-top-lower-corner  [dovetail-inner-x-position, dovetail-inner-top-y-position, dovetail-lower-z-position])
(def dovetail-outer-bottom-lower-corner [dovetail-outer-x-position,dovetail-outer-bottom-y-position, dovetail-lower-z-position])
(def dovetail-outer-top-lower-corner  [dovetail-outer-x-position,dovetail-outer-top-y-position, dovetail-lower-z-position])
(def dovetail-inner-bottom-upper-corner [dovetail-inner-x-position,dovetail-inner-bottom-y-position, dovetail-upper-z-position])
(def dovetail-inner-top-upper-corner [dovetail-inner-x-position, dovetail-inner-top-y-position, dovetail-upper-z-position])
(def dovetail-outer-bottom-upper-corner [dovetail-outer-x-position,dovetail-outer-bottom-y-position, dovetail-upper-z-position])
(def dovetail-outer-top-upper-corner [dovetail-outer-x-position,dovetail-outer-top-y-position, dovetail-upper-z-position])

(def dovetail-clearance-inner-bottom-lower-corner [dovetail-clearance-inner-x-position,dovetail-clearance-inner-bottom-y-position, dovetail-clearance-lower-z-position])
(def dovetail-clearance-inner-top-lower-corner  [dovetail-clearance-inner-x-position, dovetail-clearance-inner-top-y-position, dovetail-clearance-lower-z-position])
(def dovetail-clearance-outer-bottom-lower-corner [dovetail-clearance-outer-x-position,dovetail-clearance-outer-bottom-y-position, dovetail-clearance-lower-z-position])
(def dovetail-clearance-outer-top-lower-corner  [dovetail-clearance-outer-x-position,dovetail-clearance-outer-top-y-position, dovetail-clearance-lower-z-position])
(def dovetail-clearance-inner-bottom-upper-corner [dovetail-clearance-inner-x-position,dovetail-clearance-inner-bottom-y-position, dovetail-clearance-upper-z-position])
(def dovetail-clearance-inner-top-upper-corner [dovetail-clearance-inner-x-position, dovetail-clearance-inner-top-y-position, dovetail-clearance-upper-z-position])
(def dovetail-clearance-outer-bottom-upper-corner [dovetail-clearance-outer-x-position,dovetail-clearance-outer-bottom-y-position, dovetail-clearance-upper-z-position])
(def dovetail-clearance-outer-top-upper-corner [dovetail-clearance-outer-x-position,dovetail-clearance-outer-top-y-position, dovetail-clearance-upper-z-position])

(def dovetail-shape
  (polyhedron [dovetail-outer-bottom-lower-corner
               dovetail-inner-bottom-lower-corner
               dovetail-inner-top-lower-corner
               dovetail-outer-top-lower-corner
               dovetail-outer-bottom-upper-corner
               dovetail-inner-bottom-upper-corner
               dovetail-inner-top-upper-corner
               dovetail-outer-top-upper-corner], [[0,1,2,3], [4,5,1,0],[7,6,5,4],  [5,6,2,1], [6,7,3,2], [7,4,0,3]]))

(def dovetail-clearance-shape
  (polyhedron [dovetail-clearance-outer-bottom-lower-corner
               dovetail-clearance-inner-bottom-lower-corner
               dovetail-clearance-inner-top-lower-corner
               dovetail-clearance-outer-top-lower-corner
               dovetail-clearance-outer-bottom-upper-corner
               dovetail-clearance-inner-bottom-upper-corner
               dovetail-clearance-inner-top-upper-corner
               dovetail-clearance-outer-top-upper-corner], [[0,1,2,3], [4,5,1,0],[7,6,5,4],  [5,6,2,1], [6,7,3,2], [7,4,0,3]]))

(def dovetail-left
  (translate [(- (first usb-holder-position) (/ (first usb-holder-size) 2) usb-holder-thickness) (+ (second usb-holder-size) 1.5) 0] dovetail-shape))

(def dovetail-right
  (translate [(+ (first usb-holder-position) (/ (first usb-holder-size) 2) usb-holder-thickness) (+ (second usb-holder-size) 1.5) 0] (mirror [-1 0 0] dovetail-shape)))

(def dovetail-clearance-left
  (translate [(- (first usb-holder-position) (/ (first usb-holder-size) 2) usb-holder-thickness) (+ (second usb-holder-size) 1.5) 0] dovetail-clearance-shape))

(def dovetail-clearance-right
  (translate [(+ (first usb-holder-position) (/ (first usb-holder-size) 2) usb-holder-thickness) (+ (second usb-holder-size) 1.5) 0] (mirror [-1 0 0] dovetail-clearance-shape)))

(def usb-holder
  (
   union 
   (difference
   (union
   usb-holder-main-body
   (for [x (range 0 2)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
   (for [x (range 1 3)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr)) 
    
   )
    (translate [(first usb-holder-position) (second usb-holder-size) 42.5] (cube 40 15 60))
   (translate [(first usb-holder-position) (+ (second usb-holder-size) 3) -10] (cube 40 15 20))
   (translate [(+ (first usb-holder-position) (/ (first usb-holder-size) 2) 5.5) (+ (second usb-holder-size) 4) 6.25] (cube 6 8 12.7))
(translate [(+ (first usb-holder-position) (- (/ (first usb-holder-size) 2)) -5.5) (+ (second usb-holder-size) 4) 6.25] (cube 6 8 12.7))
   )
   dovetail-left
dovetail-right))

    ; (rotate -0.6 [0 0 1])
    ;( translate [(- (first usb-holder-position) 10) (+ (second usb-holder-position) 4) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])

(def usb-holder-to-glue-to-left 
  (let [usb-holder-y (+ (last usb-holder-size) usb-holder-thickness)]
   (->>
   (cube (first usb-holder-size) (+ usb-holder-thickness 4) usb-holder-y)
   (translate [(first usb-holder-position) (+ (second usb-holder-size) usb-holder-thickness -2) (+ (/ usb-holder-y 2) 2)])
  ))
)

(def usb-holder-extra-clearance-top (->> 
        (cube (+ (first usb-holder-size) 5.8) 6 0.2 )
        (translate [(first usb-holder-position) (+( second usb-holder-size) 4) (+ (last usb-holder-size) 7.2)])
            ))

(def usb-holder-extra-clearance-side 
                                     (cube 0.4 6 12.5))

(def usb-holder-extra-clearance-left 
 (translate [(+ (first usb-holder-position) (- (/ (first usb-holder-size) 2)) -2.7) (+ (second usb-holder-size) 4) 6.25] usb-holder-extra-clearance-side)
  )

(def usb-holder-extra-clearance-right
  (translate [(+ (first usb-holder-position) (+ (/ (first usb-holder-size) 2)) 2.7) (+ (second usb-holder-size) 4) 6.25] usb-holder-extra-clearance-side))

(def usb-holder-extra-clearance
  (union
   usb-holder-extra-clearance-top
   usb-holder-extra-clearance-left
   usb-holder-extra-clearance-right
   ))

(def usb-holder-hole
  (->>
   (union
    (->> ( cube (first usb-hole-size) (-(second usb-hole-size ) usb-holder-thickness ) (last usb-hole-size))
         (translate [(+ (first usb-holder-position) 0) (second usb-holder-position) (+ (/ (last usb-holder-size) 2) usb-holder-thickness)]))
    (->> (apply cube usb-hole-size-left)
         (translate [(+ (first usb-holder-position) 0) (- (second usb-holder-position) 32 ) (/ (+ (last usb-holder-size) usb-holder-thickness 4) 2)]))
    (->> (apply cube usb-hole-size-right)
         (translate [(+ (first usb-holder-position) 0) (+ (second usb-holder-position) 32 ) (/ (+ (last usb-holder-size) usb-holder-thickness 4) 2)]))
    (->> (apply cube (map + usb-hole-size-right [1 1 2]))
         (translate [(+ (first usb-holder-position) 0) (+ (second usb-holder-position) 32 26 ) (/ (+ (last usb-holder-size) usb-holder-thickness 4) 2)])))))

