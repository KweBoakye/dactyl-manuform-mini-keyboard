(ns dactyl-keyboard.lib.openscad.bosl2-wrappers.screws
  (:refer-clojure :exclude [use import])
   (:require [dactyl-keyboard.lib.openscad.openscad-formatting :refer [format-arg]]
             [scad-clj.model :refer :all]
             [scad-clj.scad :refer :all]))


(defn screw [spec &{:keys [length head]}]
  (call-module :screw (format "spec =\"%s\""  spec) (format-arg "length" length)
               (format "head =\"%s\""  head))
  )


(comment 
  (spit "things-low/screw-test.scad"
        (write-scad
         (include include-bosl2)
         (include include-bosl2-screws)
         (screw "M3" :length 10 :head "flat"))))