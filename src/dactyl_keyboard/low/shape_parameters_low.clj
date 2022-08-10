(ns dactyl-keyboard.low.shape-parameters-low
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.utils :refer :all]
            ))



;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 4)
(def ncols 5)

(def far-index-curvature (deg2rad 20.4))
(def index-curvature (deg2rad 20.4))
(def middle-curvature (deg2rad 19.5))
(def ring-curvature (deg2rad 22.8))
(def pinky-curvature (deg2rad 27.5))

(defn α [column] (cond (= column 0) far-index-curvature
                   (<= column 1) index-curvature
                         (= column 2) middle-curvature
                         (= column 3) ring-curvature
                         (>= column 4) pinky-curvature
                         :else (/ π 12)))

;(def α (/ π 12))                        ; curvature of the columns
(def β (/ π 36))                        ; curvature of the rows
(defn centerrow [column] (cond
                           (= column 0) (- nrows 3)  ;inner index
                           (= column 1) (- nrows 3)  ;index
                           (= column 2) (- nrows 3)  ;middle
                           (= column 3) (- nrows 3)  ;ring
                           (>= column 4) (- nrows 3)  ;pinky
                           :else (- nrows 3)))          ; controls front-back tilt
(def centercol 2)                       ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (/ π 12))            ; or, change this for more precise tenting control
(def column-style
  (if (> nrows 5) :orthographic :standard))  ; options include :standard, :orthographic, and :fixed
; (def column-style :fixed)
(def pinky-15u false)

(def extra-row false)                   ; adds an extra bottom row to the outer columns
(def inner-column false)
(def thumb-style "default")

(def far-index-stagger [0 -6 0])
(def index-stagger [0 -2 0])
(def middle-stagger [0 5.82 -2.5])
(def ring-stagger [0 -3 0])
(def pinky-stagger [0 -18 5.64])

(defn column-offset [column] (cond
                               (= column 0) far-index-stagger
                               (= column 1) index-stagger
                               (= column 2) middle-stagger;[0 2.82 -4.5]
                               (= column 3) ring-stagger
                               (>= column 4) pinky-stagger ;[0 -12 5.64]            ; original [0 -5.8 5.64]
                               :else [0 0 0]))

(def thumb-offsets [6 -3 7])

(def keyboard-z-offset 9)               ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

(def extra-width 2.5)                   ; extra space between the base of keys; original= 2
(def extra-height 1.0)                  ; original= 0.5

(def wall-z-offset -4)                 ; original=-15 length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 5)                  ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 2)                  ; wall thickness parameter; originally 5
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
;(def create-side-nubs? true)

(def round-case true)
(def cirque-TM040040-mount-thumb false)
(def oled-type "ST7789-240x240-154")
(def EVQWGD001-mount true)
;(keyword "screen-holder-mount" "screen-holder-mount-top")
;(keyword "screen-holder-mount" "screen-holder-mount-side" )
(def screen-holder-mount-position "screen-holder-mount-side")
;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def lastcol (dec ncols))
(def extra-cornerrow (if extra-row lastrow cornerrow))
(def innercol-offset (if inner-column 1 0))
(def rounding-radius (if round-case 2 0))

(when (= nrows 5)
  (def left-wall-y-modifier 0)
  )

(when (= nrows 4)
  (def left-wall-y-modifier 0))