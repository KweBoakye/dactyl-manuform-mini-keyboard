(ns dactyl-keyboard.low.shape-parameters-low
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.lib.geometry :refer [deg2rad]]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.utils :refer :all]))



;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 4)
(def ncols 5)

(def far-index-curvature (deg2rad 20.4))
(def index-curvature (deg2rad 20.4))
(def middle-curvature (deg2rad 20.5))
(def ring-curvature (deg2rad 22.8))
(def pinky-curvature (deg2rad 27.5))

(defn α [column] (cond (= column 0) far-index-curvature
                       (<= column 1) index-curvature
                       (= column 2) middle-curvature
                       (= column 3) ring-curvature
                       (>= column 4) pinky-curvature
                       :else (/ π 12)))

;(def α (/ π 12))                        ; curvature of the columns
;https://discord.com/channels/681309835135811648/747850923023532073/1017470634730717254

(def far-index-tilt (/ π 18))
(def index-tilt (/ π 180))
(def middle-tilt (/ π 180))
(def ring-tilt (/ π 180))
(def pinky-tilt (/ π 180))
(defn β [column](cond (= column 0) far-index-tilt
                      (<= column 1) index-tilt
                      (= column 2) middle-tilt
                      (= column 3) ring-tilt
                      (>= column 4) pinky-tilt
                      :else (/ π 180))) ; (/ π 36)                       ; curvature of the rows

(def splay-last-row true)
(def far-index-splay  5)
(def index-splay far-index-splay)
(def middle-splay 0)
(def ring-splay -2.5)
(def pinky-splay -7.5)
(defn γ [column]
  (->>
   (cond (= column 0) far-index-splay
         (<= column 1) index-splay
         (= column 2) middle-splay
         (= column 3) ring-splay
         (>= column 4) pinky-splay
         :else 0)
   (deg2rad)))  

(defn splay-angle-to-translation [splay-angle]
  (let [x-trans (* (- splay-angle) (/ 3 5))
        width (* (cond (neg? x-trans) (- keyswitch-width) :else keyswitch-width) 1.5)]
    (cond (> (abs x-trans) (* keyswitch-width 1.5)) width :else x-trans)))

(def far-index-post-splay-translation [0 0 0])
(def index-post-splay-translation [0 0 0])
(def middle-post-splay-translation [0 0 0])
(def ring-post-splay-translation [0 0 0])
(def pinky-post-splay-translation [0 0 0])

(defn post-splay-translation [column]
  (cond (= column 0) far-index-post-splay-translation
        (<= column 1) index-post-splay-translation
        (= column 2) middle-post-splay-translation
        (= column 3) ring-post-splay-translation
        (>= column 4) pinky-post-splay-translation
        :else 0))

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
(def last-row-style :no-last-row) ; :no-last-row :last-row-middle-and-fourth-keys-only :all-columns 
(def last-row-middle-and-fourth-keys-only false)
(def no-last-row true) ; having only three rows comes out nicer if you have four then remove the last row
(def inner-column false)
(def thumb-style "default")
(keyword "thumb-curvature-standard")
(keyword "thumb-curvature-convex")
(def thumb-curvature-type :thumb-curvature-convex)

(def far-index-stagger [0 -6 -4])
(def index-stagger [0 -2 0])
(def middle-stagger [0 5.82 -2.5])
(def ring-stagger [0 -6 0.5])
(def pinky-stagger [0 -18 5.64])

(defn column-offset [column] (cond
                               (= column 0) far-index-stagger
                               (= column 1) index-stagger
                               (= column 2) middle-stagger;[0 2.82 -4.5]
                               (= column 3) ring-stagger
                               (>= column 4) pinky-stagger ;[0 -12 5.64]            ; original [0 -5.8 5.64]
                               :else [0 0 0]))

(def thumb-offsets [6 -3 7])
;(def thumb-fan-radius 70)
(def thumb-fan-angle 15)
;(def thumb-well-radius 85)
(def thumb-well-angle 10)
(def thumb-alpha 15)
(def thumb-beta 3.75)
(def thumb-rows 1)


(def keyboard-z-offset 10)               ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

(def extra-width 2.5)                   ; extra space between the base of keys; original= 2
(def extra-height 1.0)                  ; original= 0.5

(def wall-z-offset -4)                 ; original=-15 length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 5)                  ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 2)                  ; wall thickness parameter; originally 5
(def wall-xy-offset-thin 1)
(def wall-xy-offset-medium-thin 2)
(def wall-xy-offset-mid 3)


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
  (def left-wall-y-modifier 0))

(when (= nrows 4)
  (def left-wall-y-modifier 0))
(when (= nrows 3)
  (def left-wall-y-modifier 0))

(def left-wall-x-offset 5) ; original 10
(def left-wall-z-offset 4) ; original 3
(def left-wall-x-offset-oled  -10)

