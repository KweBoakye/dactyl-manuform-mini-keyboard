(ns dactyl-keyboard.tenting-hardware
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.shape-parameters :refer :all]
            [dactyl-keyboard.plate :refer :all]
            [unicode-math.core :refer :all]))


(def tent-ball-rad 7)
(def tent-stand-rad 5)
(def crank-rad 1.5)
(def crank-len 20)
(def tent-stand-thread-height 25)
(def tent-stand-thread-lead 1.25)
(def tent-thread (call-module "thread" tent-stand-rad tent-stand-thread-height tent-stand-thread-lead))
(def tent-stand (union
                 tent-thread
                 (translate [0 0 (- tent-stand-rad)] (sphere tent-ball-rad))))


(def tent-foot-width 25)
(def tent-foot-height 30)
(def tent-foot-thickness 2)
(def tent-ball-holder-thickness 4)
(def hook-angle 40)

; Some convoluted logic to create a little hook to hold the ball in
(defn ball-hook [with-hook?]
  (let
   [hook-height (if with-hook? tent-ball-rad (/ tent-ball-rad 1.5))]
    (rotate (deg2rad 90) [1 0 0]
            (union
             (translate [0 (/ hook-height 2) 0]
                        (rotate (deg2rad 90) [1 0 0] (cube tent-ball-holder-thickness tent-ball-holder-thickness hook-height)))
             (if with-hook? (translate [(- (+ tent-ball-rad (/ tent-ball-holder-thickness 2))) tent-ball-rad 0]
                                       (extrude-rotate {:angle hook-angle :convexity 10} (translate [(+ tent-ball-rad (/ tent-ball-holder-thickness 2)) 0]
                                                                                                    (square tent-ball-holder-thickness tent-ball-holder-thickness)))) nil)))))

(defn rotated-ball-hook [angle with-hook?]
  (rotate (deg2rad angle) [0 0 1] (translate [(+ tent-ball-rad (/ tent-ball-holder-thickness 2)) 0 (/ tent-foot-thickness 2)] (ball-hook with-hook?))))

(def tent-foot (union
                (cube tent-foot-width tent-foot-height tent-foot-thickness)
                (rotated-ball-hook 0 true)
                (rotated-ball-hook 90 true)
                (rotated-ball-hook 180 true)
                (rotated-ball-hook 270 false)))

(def thumb-tent-origin (map + [-52 -74 -1] (if trackball-enabled [3 -12 0] [0 0 0])))
(def index-tent-origin [-104 27 -1])

(def tent-nut-height 6)
(def tent-thread
  (translate [0 0 tent-nut-height] (rotate (deg2rad 180) [0 1 0]
                                           (call-module "thread" (+ tent-stand-rad 0.5) (+ tent-nut-height bottom-plate-thickness) tent-stand-thread-lead))))
(def tent-nut (difference
               (translate [0 0 (/ tent-nut-height 2)] (cylinder (+ tent-stand-rad 1.5) tent-nut-height))
               tent-thread))