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

(defn rounding-edge-mask [length &{:keys [r r1 r2 d d1 d2 excess anchor spin orient fn] 
                                     :or {excess 0.1 anchor CENTER spin 0 orient UP}}]
  (let [length-param (cond
                     (double? length) (format "length = %f" length)
                     (number? length) (format "length = %d" length))
        ]
    (apply call-module (concat [:rounding_edge_mask] (filter #(not (nil? %))  [length-param (cond r (format-arg "r" r)) (cond r1 (format-arg "r1" r1))
                 (cond r2 (format-arg "r2" r2)) (cond d(format-arg "d" d)) (cond d1 (format-arg "d1" d1)) (cond d2 (format-arg "d2" d2))
                 (format-arg "excess" excess)
                 (format-arg "anchor" anchor) (format "spin = %d" spin) (format-arg "orient" orient) (cond fn (format "$fn = %d" fn))])))))

(defn teardrop-edge-mask [length radius &{:keys [ angle excess anchor spin orient] 
                                   :or { angle 45.0  excess 0.1 anchor CENTER spin 0.0 orient UP}}]
  (call-module :teardrop_edge_mask (format "length = %f" (double length)) (format "r = %f" (double radius)) (format "angle = %f" (double angle))
               (format "excess = %f" excess) (format-arg "anchor" anchor) (format "spin = %f" spin) (format-arg "orient" orient)))

(comment (name :CENTER))