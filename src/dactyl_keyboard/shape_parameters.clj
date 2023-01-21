(ns dactyl-keyboard.shape-parameters
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            ))

;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 5)
(def ncols 7)

(def trackball-enabled true)
(def joystick-enabled false)
(def joycon-joystick-enabled false)
(def printed-hotswap? false) ; Whether you want the 3d printed version of the hotswap or you ordered some from krepublic



;(def α (/ π 12))                        ; curvature of the columns


(def centerrow (- nrows 3))             ; controls front-back tilt
(def centercol 4)                    ; controls left-right tilt / tenting (higher number is more tenting)

;(def column-style
;  (if (> nrows 5) :orthographic :standard))  ; options include :standard, :orthographic, and :fixed
(def column-style :standard)
(def pinky-15u false)
(def first-15u-row 0)                   ; controls which should be the first row to have 1.5u keys on the outer column
(def last-15u-row 3)                    ; controls which should be the last row to have 1.5u keys on the outer column

(def extra-row false)                   ; adds an extra bottom row to the outer columns
(def inner-column true)
(def thumb-style "mini")
(def smooth-front-wall-on true)
(def smooth-back-wall-on true)
(def elite-c "elite-c")
(def blackpill "black-pill-STM32F411")
(def controller-type blackpill) ; elite-c or black-pill-STM32F411

(def index-curvature (deg2rad 19.4))
(def middle-curvature (deg2rad 17.5))
(def ring-curvature (deg2rad 21.8))
(def pinky-curvature (deg2rad 25))

(if (true? inner-column) ; curvature of the columns
  (defn α [column] (cond (<= column 2) index-curvature
                         (= column 3) middle-curvature
                         (= column 4) ring-curvature
                         (>= column 5) pinky-curvature
                         :else (/ π 12)))
  (defn α [column] (cond (<= column 1) index-curvature
                         (= column 2) middle-curvature
                         (= column 3) ring-curvature
                         (>= column 4) pinky-curvature
                         :else (/ π 12))))

(def β (/ π 36))                        ; curvature of the rows

(def tenting-angle (/ π 12))            ; or, change this for more precise tenting control
(def index-stagger [0 -2 0])
(def middle-stagger [0 5.82 -4.5])
(def ring-stagger [0 -6 0])
(def pinky-stagger [0 -15 5.64]) ; original [0 -5.8 5.64]

(defn column-offset [column]
  (if inner-column
    (cond (<= column 2) index-stagger
          (= column 3) middle-stagger
          (= column 4) ring-stagger
          (>= column 5) pinky-stagger
          :else [0 0 0])
    (cond (<= column 1) index-stagger
          (= column 2) middle-stagger
          (= column 3) ring-stagger
          (>= column 4) [0 -12 5.64]    ; original [0 -5.8 5.64]
          :else [0 0 0])))

(def thumb-offsets [6 0 10])

(def keyboard-z-offset 10)               ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

(def extra-width 2.5)                   ; extra space between the base of keys; original= 2
(def extra-height 1.0)                  ; original= 0.5

(def wall-z-offset -8)                 ; original=-15 length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 5)                  ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 3)                  ; wall thickness parameter; originally 5
(def wall-xy-offset-thin 1)

;; Settings for column-style == :fixed
;; The defaults roughly match Maltron settings
;;   http://patentimages.storage.googleapis.com/EP0219944A2/imgf0002.png
;; Fixed-z overrides the z portion of the column ofsets above.
;; NOTE: THIS DOESN'T WORK QUITE LIKE I'D HOPED.
(def fixed-angles [(deg2rad 10) (deg2rad 10) 0 0 0 (deg2rad -15) (deg2rad -15)])
(def fixed-x [-41.5 -22.5 0 20.3 41.4 65.5 89.6])  ; relative to the middle finger
(def fixed-z [12.1    8.3 0  5   10.7 14.5 17.5])
(def fixed-tenting (deg2rad 0))

; If you use Cherry MX or Gateron switches, this can be turned on.
; If you use other switches such as Kailh, you should set this as false

(def round-case true)
;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def lastcol (dec ncols))
(def extra-cornerrow (if extra-row lastrow cornerrow))
(def innercol-offset (if inner-column 1 0))



(def rounding-radius (if round-case 1 0))