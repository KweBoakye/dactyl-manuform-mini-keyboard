(ns dactyl-keyboard.magsafe-sticker-cutout
  (:refer-clojure :exclude [use import])
  (:require 
    [scad-clj.model :refer :all]
   [scad-clj.scad :refer :all])
  )

(def magsafe-cutout-outer-radius 28)
(def magsafe-cutout-inner-radius 21)
(def magsafe-cutout-thickness 1)
(defn magsafe-cutout [&{:keys [height ] :or {height magsafe-cutout-thickness}}] (difference
                     (binding [*fn* 72] (cylinder magsafe-cutout-outer-radius height :center false))
                     (binding [*fn* 72] (cylinder magsafe-cutout-inner-radius height :center false))))