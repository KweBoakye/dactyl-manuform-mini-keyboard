(ns dactyl-keyboard.lib.openscad.bosl2-wrappers.joiners
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [include-bosl2
                                                                           include-bosl2-joiners]]
            [dactyl-keyboard.lib.openscad.openscad-formatting :refer :all]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all])
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

(defn snap-pin [size &{:keys [pointed r d l nub-depth snap thickness clearance preload]
                        :or {pointed true preload 0.2}}] 
  (apply call-module (concat [:snap_pin] (filter #(not (nil? %)) [(format "size =\"%s\"" size)(format-arg "pointed" pointed) (cond r (format-arg "r" r)) (cond d (format-arg "d" d)) (cond l (format-arg "l" l))
               (cond nub-depth (format-arg "nub_depth" nub-depth))(cond snap (format-arg "snap" snap))(cond thickness (format-arg "thickness" thickness))
               (cond clearance (format-arg "clearance" clearance)) (format-arg "preload" preload)])))
  
  )

(defn snap_pin_socket [size & {:keys [pointed r d l nub-depth snap fixed fins]
                               :or {pointed true fixed true fins false}}]
  (apply call-module 
         (concat [:snap_pin_socket] (filter #(not (nil? %)) [(format "size =\"%s\"" size) (format-arg "pointed" pointed) (cond r (format-arg "r" r)) (cond d (format-arg "d" d)) (cond l (format-arg "l" l)) (cond nub-depth (format-arg "nub_depth" nub-depth)) (cond snap (format-arg "snap" snap))
                                                      (format-arg "fixed" fixed)  (format-arg "fins" fins)]))))

(spit "things-low/joiner-test.scad"
      (write-scad
       (include include-bosl2)
       (include include-bosl2-joiners )
       (union
        (snap-pin "tiny")
        (snap_pin_socket "tiny"))
       ))

