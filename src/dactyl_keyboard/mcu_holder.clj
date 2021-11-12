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

(def blackpill-holder-position (key-position 1 1 (map + (wall-locate1 0 (- 0 (* 0.2 nrows))) [0 (/ mount-height 2) 0])))
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


(def usb-holder
  (->>

   (cube (+ (first usb-holder-size) usb-holder-thickness) (+ (second usb-holder-size) usb-holder-thickness) (+ (last usb-holder-size) usb-holder-thickness))





   (translate [(first usb-holder-position) (second usb-holder-position) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])))

    ; (rotate -0.6 [0 0 1])
    ;( translate [(- (first usb-holder-position) 10) (+ (second usb-holder-position) 4) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])




(def usb-holder-hole
  (->>
   (union
    (->> (apply cube usb-hole-size)
         (translate [(+ (first usb-holder-position) 0) (second usb-holder-position) (+ (/ (last usb-holder-size) 2) usb-holder-thickness)]))
    (->> (apply cube usb-hole-size-left)
         (translate [(+ (first usb-holder-position) 0) (- (second usb-holder-position) 32) (/ (+ (last usb-holder-size) usb-holder-thickness 4) 2)]))
    (->> (apply cube usb-hole-size-right)
         (translate [(+ (first usb-holder-position) 0) (+ (second usb-holder-position) 32) (/ (+ (last usb-holder-size) usb-holder-thickness 4) 2)]))
    (->> (apply cube (map + usb-hole-size-right [1 1 2]))
         (translate [(+ (first usb-holder-position) 0) (+ (second usb-holder-position) 32 26) (/ (+ (last usb-holder-size) usb-holder-thickness 4) 2)])))))
