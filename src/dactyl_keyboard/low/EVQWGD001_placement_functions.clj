(ns dactyl-keyboard.low.EVQWGD001-placement-functions
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.EVQWGD001 :refer [EVQWGD001-height
                                               EVQWGD001-mount-length EVQWGD001-mount-width]]
            [dactyl-keyboard.lib.transformations :refer [rdx rdy rdz]]
            [dactyl-keyboard.low.tps-65-placement-functions :refer [tps-65-translate-and-place-with-radius tps-65-z-rotation]]
            [dactyl-keyboard.tps-65 :refer [tps-65-corner-radius
                                            tps-65-mount-corner-radius-with-offset
                                            tps-65-mount-width]]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))


(defn EVQWGD001-place ([shape] (EVQWGD001-place translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape] (->> shape
    
                                                                 (rotate-z-fn (- tps-65-z-rotation))
                                                                 (rotate-y-fn -20)
                                                                 (rotate-x-fn -20)
                                                                 (tps-65-translate-and-place-with-radius [(- tps-65-corner-radius (/ tps-65-mount-width 2)),
                                                                                                          0,
                                                                                                          0] (- tps-65-mount-corner-radius-with-offset)  0
                                                                                                         translate-fn rotate-x-fn rotate-y-fn rotate-z-fn)
                                                                 (translate-fn [(- (/ EVQWGD001-mount-width 2)) (- (/ EVQWGD001-height 2)) (- (/ EVQWGD001-mount-length 2))]))))

(defn EVQWGD001-translate-and-place ([x y z shape] (EVQWGD001-translate-and-place x y z translate rdx rdy rdz shape))
  ([x y z translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (translate-fn [x y z])
        (EVQWGD001-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))

(defn EVQWGD001-translate-and-place-at-position ([position shape] (EVQWGD001-translate-and-place-at-position position translate rdx rdy rdz shape))
  ([position translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (translate-fn position)
        (EVQWGD001-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))

(defn EVQWGD001-translate-and-place-at-position-with-offset ([position offset shape] (EVQWGD001-translate-and-place-at-position-with-offset position offset translate rdx rdy rdz shape))
  ([position offset translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (translate-fn offset)
        (translate-fn position)
        (EVQWGD001-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))