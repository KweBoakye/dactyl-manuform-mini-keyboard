(ns dactyl-keyboard.lib.openscad.bosl2-wrappers.mask-3d
(:refer-clojure :exclude [use import])
  (:require [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]
            [clojure.pprint :refer [cl-format]]
            )
  )

(defn chamfer-edge-mask [length chamfer 
                         &{:keys [excess anchor spin orient] 
                           :or [excess 0.1 anchor :CENTER spin 0 orient :UP]}] 
  (call-module :chamfer_edge_mask (format "length = "))
  )

(comment (name :CENTER))