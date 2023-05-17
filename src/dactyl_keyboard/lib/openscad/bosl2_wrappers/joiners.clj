(ns dactyl-keyboard.lib.openscad.bosl2-wrappers.joiners
  (:refer-clojure :exclude [use import])
  (:require 
   [scad-clj.model :refer :all]
   [scad-clj.scad :refer :all]
   [dactyl-keyboard.lib.openscad.openscad-formatting :refer :all])
  )

(defn dovetail [gender width height &{:keys [slide thickness slope angle-in-degrees taper-in-degrees back-width chamfer radius round $slop extra]
                                      :or {slope 6 taper-in-degrees 0 chamfer 0 radius 0 round false $slop 0 extra 0.01}}] 
  (call-module :dovetail (format "gender =\"%s\"" gender) (format "width = %f" (double width)) (format "height = %f" (double height)) 
               (if slide (format "slide = %f" (double slide)) (format "thickness = %f" (double thickness)))
               (if angle-in-degrees (format "angle = %f" angle-in-degrees) (format "slope = %f" (double slope))) 
               (if back-width (format "back_width = %f" (double back-width)) (format "taper = %f" (double taper-in-degrees)))
               (format "chamfer = %f" (double chamfer)) (format "radius = %f" (double radius)) (format "round = %b" round) (format "$slop = %f" (double $slop))
               (format "extra = %f" (double extra))) 
  )