(ns dactyl-keyboard.low.screen-holder-placement-functions
  (:refer-clojure :exclude [use import])
  (:require
   [scad-clj.scad :refer :all]
   [scad-clj.model :refer :all]
   [dactyl-keyboard.oled :refer :all]
   [dactyl-keyboard.low.case-low-functions :refer [left-wall-plate-place]]
   [dactyl-keyboard.low.oled-low-placements :refer :all]
   [dactyl-keyboard.low.shape-parameters-low :refer :all]
   [dactyl-keyboard.lib.transformations :refer [rdx rdy rdz]] 
   )
  )

(defn screen-holder-rotate [shape]
  (->> shape
       (rdx -5)
       (rdy 7.5)
       (rdz screen-rotation-angle)))

(defn screen-holder-place [shape]
  (->> shape
       (screen-holder-rotate)
       (left-wall-plate-place 0 (+ left-wall-y-modifier 0.5))
       (translate screen-holder-position)
       (translate [0 0 (cond (= nrows 5) 0 (= nrows 4) -4 (= nrows 3) -4)])))

(defn screen-holder-place-x-y [xdir ydir shape]
  (->> shape
       (translate [(* xdir oled-holder-width 0.5) (* ydir oled-holder-height 0.5) (/ screen-holder-depth 2)])
       (screen-holder-rotate)
       (translate [(- (* xdir oled-holder-width 0.5)) (- (* ydir oled-holder-height 0.5)) 0])
       (left-wall-plate-place xdir ydir)

       (translate screen-holder-position)))

(defn screen-holder-translate-and-place [x y z shape]
  (->> shape
       (translate [x y z])
       (screen-holder-place)))

(def screen-holder-rotate-side-y -70)
(defn screen-holder-rotate-side
  ([shape] (screen-holder-rotate-side rdz rdy rdz shape))
  ([rotate-x-fn rotate-y-fn rotate-z-fn shape] (->> shape
                                                    (rotate-z-fn -90)
                                                    (rotate-y-fn screen-holder-rotate-side-y)
                                                    (rotate-x-fn -18)
                                                    (rotate-z-fn 20))))
(defn screen-holder-place-side ([shape] (screen-holder-place-side translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape] (->> shape
                                                                 (screen-holder-rotate-side rotate-x-fn rotate-y-fn rotate-z-fn)
                                                                 (translate-fn [0 -20 8])
       ;(rdz 5)
                                                                 (left-wall-plate-place -2 -3 translate-fn rotate-x-fn rotate-z-fn)
                                                                 (translate-fn [0 0 (- 2.5 keyboard-z-offset 3)]))))

(defn screen-holder-translate-and-place-side ([x y z shape] (screen-holder-translate-and-place-side x y z translate rdx rdy rdz shape))
  ([x y z translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape] (->> shape
                                                                       (translate-fn [x y z])
                                                                       (screen-holder-place-side translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))
(defn screen-holder-translate-and-place-side-with-offset ([x y z offset shape] (screen-holder-translate-and-place-side-with-offset x y z offset translate rdx rdy rdz shape))
  ([x y z offset translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape] (->> shape
                                                                              (translate-fn offset)
                                                                              (translate-fn [x y z])
                                                                              (screen-holder-place-side translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))