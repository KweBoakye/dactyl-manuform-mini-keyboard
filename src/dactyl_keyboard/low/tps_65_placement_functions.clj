(ns dactyl-keyboard.low.tps-65-placement-functions
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.lib.transformations :refer [rdx rdy rdz]]
            [dactyl-keyboard.low.case-low-functions :refer [left-wall-plate-place
                                                            left-wall-plate-place-rotate]]
            [dactyl-keyboard.low.shape-parameters-low :refer [far-index-splay
                                                              left-wall-y-modifier]]
            [dactyl-keyboard.switch-hole :refer [plate-thickness]]
            [dactyl-keyboard.tps-65 :refer :all] 
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))

(def tps-65-z-position 10)
(def tps-65-z-rotation (+ 90 far-index-splay))
(def tps-65-x-rotation -20)
(def tps-65-y-rotation 0)

(defn tps-65-rotate ([shape] (tps-65-rotate  rdx rdy rdz shape))
  ([rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape (rotate-z-fn tps-65-z-rotation)
        (rotate-x-fn tps-65-x-rotation)
        (rotate-y-fn tps-65-y-rotation)
        (left-wall-plate-place-rotate  rotate-x-fn rotate-z-fn))))

(defn tps-65-place ([shape] (tps-65-place translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (rotate-z-fn tps-65-z-rotation)
        (rotate-x-fn tps-65-x-rotation)

        (rotate-y-fn tps-65-y-rotation)
       ;(rdy -7.5)
        (left-wall-plate-place 0 (+ left-wall-y-modifier -3) translate-fn rotate-x-fn rotate-z-fn)
        (translate-fn [0 3 tps-65-z-position]))))

(defn tps-65-translate-and-place [x y z shape]
  (->> shape
       (translate [x y z])
       (tps-65-place)))

(defn tps-65-translate-and-place-at-position ([position shape] (tps-65-translate-and-place-at-position position translate rdx rdy rdz shape))
  ([position translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape] (->> shape
                                                                          (translate-fn position)
                                                                          (tps-65-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))

(defn tps-65-translate-and-place-at-position-with-offset ([position offset shape] (tps-65-translate-and-place-at-position-with-offset position offset translate rdx rdy rdz shape))
  ([position offset translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape] (->> shape
                                                                                 (translate-fn offset)
                                                                                 (translate-fn position)
                                                                                 (tps-65-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))



(defn tps-65-translate-and-place-with-radius
  ([position radius-compensation-x radius-compensation-y shape]
   (tps-65-translate-and-place-with-radius position radius-compensation-x radius-compensation-y translate rdx rdy rdz shape))
  ([position radius-compensation-x radius-compensation-y translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (translate-fn position)
       ;plate-thickness
        (translate-fn [(tps-radius-compensation-adjust radius-compensation-x) (tps-radius-compensation-adjust radius-compensation-y) (- plate-thickness)])
        (tps-65-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))

(defn tps-65-translate-and-place-with-radius-xyz
  ([x y z radius-compensation-x radius-compensation-y shape]
   (tps-65-translate-and-place-with-radius-xyz x y z radius-compensation-x radius-compensation-y translate rdx rdy rdz shape))
  ([x y z radius-compensation-x radius-compensation-y translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (translate-fn [x y z])
       ;plate-thickness
        (translate-fn [(tps-radius-compensation-adjust radius-compensation-x) (tps-radius-compensation-adjust radius-compensation-y) (- plate-thickness)])
        (tps-65-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))

