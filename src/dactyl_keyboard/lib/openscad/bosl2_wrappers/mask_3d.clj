(ns dactyl-keyboard.lib.openscad.bosl2-wrappers.mask-3d
(:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [CENTER UP]]
            [dactyl-keyboard.lib.openscad.openscad-formatting :refer [format-arg]]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all])
  )

(defn chamfer-edge-mask [length chamfer 
                         &{:keys [excess anchor spin orient] 
                           :or {excess 0.1 anchor :CENTER spin 0 orient :UP}}] 
  (call-module :chamfer_edge_mask (format "length = "))
  )

(defn teardrop-edge-mask [length radius &{:keys [ angle excess anchor spin orient] 
                                   :or { angle 45.0  excess 0.1 anchor CENTER spin 0.0 orient UP}}]
  (call-module :teardrop_edge_mask (format "length = %f" (double length)) (format "r = %f" (double radius)) (format "angle = %f" (double angle))
               (format "excess = %f" excess) (format-arg "anchor" anchor) (format "spin = %f" spin) (format-arg "orient" orient)))

(comment (name :CENTER))