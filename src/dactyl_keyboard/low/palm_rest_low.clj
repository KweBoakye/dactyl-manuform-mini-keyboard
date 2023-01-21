(ns dactyl-keyboard.low.palm-rest-low
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.low.placement-functions-low :refer :all]
            [dactyl-keyboard.hand :refer :all]
            [dactyl-keyboard.low.case-low-functions :refer :all]
            [dactyl-keyboard.tenting-hardware :refer :all]
            ))

;;;;;;;;;;;;;;;
;; Palm Rest ;;
;;;;;;;;;;;;;;;
(def palm-length 80)
(def palm-width 80)
(def palm-cutoff 32)
(def palm-support (translate [0 0 (- palm-cutoff)] (difference
                                                    (resize [palm-width palm-length 80] (sphere 240))
                                                    (translate [0 0 (- (- 100 palm-cutoff))] (cube 400 400 200)))))

(defn palm-rest-hole-rotate [h] (rotate (deg2rad -3) [0 0 1] h))
(def palm-hole-origin (map + (key-position 3  (+ cornerrow 1) (wall-locate3 0 -1)) [-1.5 -10 -11]))

(def triangle-length 7)
(def triangle-width 5)
; Make the buckle holes 2mm longer because the holes to the case aren't perfectly straight, which causes some problems.
(def buckle-width-adjust 0)
(def buckle-width 12)
(def buckle-thickness 3)
(def buckle-length 3.7)
(def buckle-end-length 14)
(def buckle-height 4)
(def buckle-end-width (- buckle-width (* 2 buckle-thickness)))
(def palm-buckle (buckle
                  :include-middle true
                  :triangle-length triangle-length
                  :triangle-width triangle-width
                  :buckle-width-adjust buckle-width-adjust
                  :buckle-width buckle-width
                  :buckle-thickness buckle-thickness
                  :buckle-length buckle-length
                  :buckle-end-length buckle-end-length
                  :buckle-height buckle-height))
(def palm-buckle-holes (buckle-holes
                        :buckle-length buckle-length
                        :buckle-thickness buckle-thickness
                        :buckle-width buckle-width
                        :buckle-width-adjust buckle-width-adjust
                        :triangle-width triangle-width
                        :triangle-length triangle-length
                        :buckle-height buckle-height))
(def support-length 30)
(def palm-screw-height 26.5)
(def positioned-palm-support (->> palm-support
                                  (rotate (deg2rad 20) [0 0 1])
                                  (rotate (deg2rad 17) [1 0 0])
                                  (rotate (+ tenting-angle (deg2rad 11)) [0 1 0])
                                  (translate [2 -34 15])))
(def palm-attach-rod (union
                      (translate [0 (+ (- buckle-length) (/ support-length -2) (/ (+ tent-stand-rad 0.5) 2)) 0]
                                 (cube buckle-end-width (- support-length (+ tent-stand-rad 0.5)) 5))
                      (difference
                       (translate [0 (+ (- buckle-length) (- support-length)) (- (/ palm-screw-height 2) (/ 5 2))]
                                  (cube 13 13 palm-screw-height))
                       (translate [-0.5 (+ (- buckle-length) (- support-length)) (+ (/ 5 -2) palm-screw-height)]
                                  (rotate (deg2rad 180) [1 0 0]
                                          (call-module "thread" (+ tent-stand-rad 0.5) palm-screw-height tent-stand-thread-lead)))
                       ; Rm the top bit sticking out
                       (hull positioned-palm-support
                             (translate [0 (+ (- buckle-length) (- support-length)) (+ palm-screw-height 20)]
                                        (cube 13 13 0.1))))

                      palm-buckle))

(def palm-rest (union
                positioned-palm-support
                ; Subtract out the part of the rod that's sticking up

                palm-attach-rod))