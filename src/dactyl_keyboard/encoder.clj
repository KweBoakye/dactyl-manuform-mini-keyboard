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

(def encoder-pos (add-vec (left-wall-plate-position 0 -1.75) [-6 -6 0]))
(def encoder-rot-x oled-mount-rotation-x-old)
(def encoder-rot-z oled-mount-rotation-z-old)
(def encoder-cutout-shape (binding [*fn* 36] (cylinder (/ 9 2) 1000)))
(def encoder-cutout (->> encoder-cutout-shape
                         (rx encoder-rot-x)
                         (rz encoder-rot-z)
                         (translate encoder-pos)))

(defn encoder-place [shape]
  (->> shape
       (rx encoder-rot-x)
       (rz encoder-rot-z)
       (translate encoder-pos)))

(def encoder-placeholder
  (->>
   (cube 13.5 13.5 12)
   (translate [0 0 (- plate-thickness)])))