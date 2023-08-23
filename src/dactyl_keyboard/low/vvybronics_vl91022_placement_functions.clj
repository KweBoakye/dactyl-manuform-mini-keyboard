(ns dactyl-keyboard.low.vvybronics-vl91022-placement-functions 
  (:refer-clojure :exclude [use import])
  (:require 
   [scad-clj.model :refer :all]
   [scad-clj.scad :refer :all]
   [dactyl-keyboard.low.tps-65-placement-functions :refer [tps-65-translate-and-place-at-position]]
   [dactyl-keyboard.vybronics-vl91022 :refer [vybronics-vl91022-y-axis]]
   [dactyl-keyboard.lib.transformations :refer [rdx rdy rdz]]
   [dactyl-keyboard.tps-65 :refer [tps-65-depth
                                            tps-65-depth-tolerance tps-65-overlay-thickness]]))


(defn vybronics-vl91022-place ([shape] (vybronics-vl91022-place translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (rotate-z-fn -90)
        (rotate-y-fn 180)
        (tps-65-translate-and-place-at-position [(/ (+ vybronics-vl91022-y-axis 2) 2)
                                                 0
                                                 (- (+ (* tps-65-depth 2) tps-65-depth-tolerance tps-65-overlay-thickness 1))] translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))