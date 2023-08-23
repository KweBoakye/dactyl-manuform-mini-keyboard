(ns dactyl-keyboard.lib.openscad.bosl2-wrappers.shapes-3d
   (:refer-clojure :exclude [use import])
  (:require [clojure.pprint :refer [cl-format]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.attachments :refer [EDGES_ALL EDGES_NONE]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [CENTER UP]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vector-conversion :refer [nested-vec-to-nested-scad-vec vec-to-scad-vec]]
            [dactyl-keyboard.lib.openscad.openscad-formatting :refer [format-arg]]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))

(defn cuboid [size &{:keys[chamfer rounding edges except trim-corners teardrop p1 p2 anchor spin orient fn]
                     :or {chamfer 0 rounding 0 edges EDGES_ALL except EDGES_NONE trim-corners true 
                          teardrop false p1 nil p2 nil  anchor CENTER spin 0 orient UP}}]
  (let [size_param (cond 
                     (double? size) (format "size = %f" size)
                     (number? size) (format "size = %d" size)
                     (vector? size) (cl-format nil "size = ~A" (vec-to-scad-vec size)))
        
        args (cond-> [size_param (format-arg "chamfer" chamfer) (format-arg "rounding" rounding) (cl-format nil "edges = ~A" (nested-vec-to-nested-scad-vec edges))
                      (cl-format nil "except = ~A" (nested-vec-to-nested-scad-vec except)) (format "trimcorners = %b" trim-corners)
                      (format "teardrop = %b" teardrop) (cl-format nil "anchor = ~A" (vec-to-scad-vec anchor))
                      (format "spin = %d" spin) (cl-format nil "orient = ~A" (vec-to-scad-vec orient))]
               p1 (conj (cl-format nil "p1 = ~A" (vec-to-scad-vec p1)))
               p2 (conj (cl-format nil "p2 = ~A" (vec-to-scad-vec p2))) 
                     fn (conj (format "$fn = %d"fn))
                     )]
    (println args)
    (apply (partial call-module :cuboid) args) ))

(spit "things-low/bosl2-cuboid-test.scad"
      (write-scad 
       (include "../BOSL2/std.scad")
       (union (cuboid [20 40 50] :chamfer 5))
       ))
(comment (format "size = %f" 10.0))

(comment (let [ch 2]
           (if ch "hi" "no")))

(defn text-3d [text height size font &{:keys [spacing direction language script atype anchor center spin orient]
                                       :or {spacing 1.0 direction "ltr" language "en" script "latin" 
                                            atype "baseline" anchor [0 0 0] center false spin 0 orient UP}}]
  (call-module :text3d (format "text = \"%s\"" text) (format-arg "height" height) (format-arg "size" size) (format-arg "spacing" spacing)
               (format-arg "direction" direction) (format-arg "language" language) (format-arg "script" script) (format-arg "atype" atype)
               (format-arg "anchor" anchor) (format-arg "center" center) (format-arg "spin " spin) (format-arg orient)))
