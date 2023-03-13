(ns dactyl-keyboard.lib.transformations
  (:refer-clojure :exclude [use import])
  (:require
   [scad-clj.scad :refer :all]
   [scad-clj.model :refer :all]
   [dactyl-keyboard.lib.geometry :refer [deg2rad]]) 
  )


(defn rx [radians shape] (rotate radians [1 0 0] shape))
(defn ry [radians shape] (rotate radians [0 1 0] shape))
(defn rz [radians shape] (rotate radians [0 0 1] shape))


(defn rdx [degrees shape] (rx (deg2rad degrees) shape))
(defn rdy [degrees shape] (ry (deg2rad degrees) shape))
(defn rdz [degrees shape] (rz (deg2rad degrees) shape))


(defn rd [x y z shape] (->> shape
                            (rdx x)
                            (rdy y)
                            (rdz z)))