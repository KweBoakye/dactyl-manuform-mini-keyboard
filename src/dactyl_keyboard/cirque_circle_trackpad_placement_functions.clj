(ns dactyl-keyboard.cirque-circle-trackpad-placement-functions
    (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.case :refer :all]
            [dactyl-keyboard.cirque-circle-trackpad :refer :all]
            [dactyl-keyboard.lib.geometry :refer [deg2rad]]
            [dactyl-keyboard.placement-functions :refer :all]
            [dactyl-keyboard.sa-keycaps :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.web-connecters :refer :all]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))

(defn cirque-place [shape]
  (->> shape
       (translate [-1 0.5 2.8])
       (key-place 0 2)))

(def cirque-circle-trackpad-notch-holders
  (union
   (cirque-place  cirque-circle-trackpad-first-notch)
   (cirque-place cirque-circle-trackpad-second-notch)
   (cirque-place cirque-circle-trackpad-third-notch)))

(def cirque-TM040040-mount-walls-mask
  (union
   key-holes-inner
   inner-connectors
   (key-place 0 2 keyhole-fill)
   left-section
   inner-column-bottom-section))

(def cirque-TM040040-mount-walls-mask-block
  (union
   (translate [0 0 (- plate-thickness)] cirque-TM040040-mount-walls-mask)
   (translate [0 0 (- (* 2 plate-thickness))] cirque-TM040040-mount-walls-mask)
   (translate [0 0 (- (* 3 plate-thickness))] cirque-TM040040-mount-walls-mask)
   (translate [0 0 (- (* 4 plate-thickness))] cirque-TM040040-mount-walls-mask)))


(defn cirque-TM040040-place [shape]
  (->> shape
       (rotate (deg2rad -10) [0 1 0])
       (rotate (deg2rad 0) [1 0  0])
       (key-place 0 2)
       (translate [-6 -10 cirque-TM040040-mount-height])))

