(ns dactyl-keyboard.encoder
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.oled :refer :all]
            [dactyl-keyboard.case :refer :all]
            [unicode-math.core :refer :all]))

(def encoder-pos (add-vec (left-wall-plate-position 0 -1.75) [-6 -7 0]))
(def encoder-rot-x oled-mount-rotation-x-old)
(def encoder-rot-z oled-mount-rotation-z-old)
(def encoder-cutout-shape (binding [*fn* 36] (cylinder (/ 9.4 2) 1000)))
(def encoder-cutout (->> encoder-cutout-shape
                         (rx encoder-rot-x)
                         (rz encoder-rot-z)
                         (translate encoder-pos)))

(def encoder-pos-top-left-below  [-6.75 6.75 0])
(def encoder-pos-top-right-below  [6.75 6.75 0])
(def encoder-pos-bottom-left-below   [-6.75 -6.75 0])
(def encoder-pos-bottom-right-below   [6.75 -6.75 0])
(def encoder-pos-top-left-above  [-6.75 6.75 1])
(def encoder-pos-top-right-above  [6.75 6.75 1])
(def encoder-pos-bottom-left-above  [-6.75 -6.75 1])
(def encoder-pos-bottom-right-above  [6.75 -6.75 1])

(def sphere-to-hull
  (binding [*fn* 100] (sphere 2)))

(def encoder-mount 
  (hull 
   (translate encoder-pos-top-left-below sphere-to-hull)
    (translate encoder-pos-top-right-below sphere-to-hull)
    (translate encoder-pos-bottom-left-below sphere-to-hull)
    (translate encoder-pos-bottom-right-below sphere-to-hull)
    (translate encoder-pos-top-left-above sphere-to-hull)
    (translate encoder-pos-top-right-above sphere-to-hull)
    (translate encoder-pos-bottom-left-above sphere-to-hull)
    (translate encoder-pos-bottom-right-above sphere-to-hull)

   ))

(defn encoder-place [shape]
  (->> shape
       (rx encoder-rot-x)
       (rz encoder-rot-z)
       (translate encoder-pos)))

(def encoder-placeholder
  (->>
   (cube 13.5 13.5 12)
   (translate [0 0 (- plate-thickness)])))