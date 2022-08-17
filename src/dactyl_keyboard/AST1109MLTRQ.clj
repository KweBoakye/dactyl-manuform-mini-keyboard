(ns dactyl-keyboard.AST1109MLTRQ
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]))

(def AST1109MLTRQ-width 9.4)
(def AST1109MLTRQ-length 11.4)
(def AST1109MLTRQ-height 2)
(def AST1109MLTRQ-solder-tab-length 2)
(def AST1109MLTRQ-solder-tab-width 2)
(def AST1109MLTRQ-holder-thickness 1)

(def AST1109MLTRQ-holder-main-body 
  (->> 
   (cube  (+ AST1109MLTRQ-width AST1109MLTRQ-holder-thickness) (+ AST1109MLTRQ-length AST1109MLTRQ-holder-thickness) (+ AST1109MLTRQ-height AST1109MLTRQ-holder-thickness) ) 
   (translate [0 0 (/ (+ AST1109MLTRQ-height AST1109MLTRQ-holder-thickness) 2)])
   ))

(def AST1109MLTRQ-holder-cutout
  (->> 
   (cube AST1109MLTRQ-width AST1109MLTRQ-length AST1109MLTRQ-height)
  (translate [0 0 (+ (/ AST1109MLTRQ-height 2) AST1109MLTRQ-holder-thickness)]))
  )

(def AST1109MLTRQ-solder-tab-cutout
  (->>
   (cube (+ AST1109MLTRQ-solder-tab-width 0.4) (+ AST1109MLTRQ-solder-tab-length 0.4 AST1109MLTRQ-holder-thickness) AST1109MLTRQ-height)
   (translate [0 0 (+ (/ AST1109MLTRQ-height 2) AST1109MLTRQ-holder-thickness)])
   )
  )

(def AST1109MLTRQ-solder-tab-cutout-top
  (translate [0 (- (/ AST1109MLTRQ-length 2) (/ AST1109MLTRQ-solder-tab-length 2))] AST1109MLTRQ-solder-tab-cutout)
  )

(def AST1109MLTRQ-solder-tab-cutout-bottom
  (translate [0 (- (/ AST1109MLTRQ-solder-tab-length 2) (/ AST1109MLTRQ-length 2))] AST1109MLTRQ-solder-tab-cutout)
  )

(def AST1109MLTRQ-holder 
  (difference
   AST1109MLTRQ-holder-main-body
   (-# AST1109MLTRQ-holder-cutout)
   (-# AST1109MLTRQ-solder-tab-cutout-top)
   AST1109MLTRQ-solder-tab-cutout-bottom
   ) 
  )