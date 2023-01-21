(ns dactyl-keyboard.IS31FL3731-mount
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            ))

;;;;;;;;;;;;;;;;
;; IS31FL3731 ;; 
;;;;;;;;;;;;;;;;

(def IS31FL3731-length 43.5)
(def IS31FL3731-width 28)

(def IS31FL3731-screwsize 2.5)
(def IS31FL3731-horizontal-distance-between-hole-edge-and-edge 1.5)
(def IS31FL3731-vertical-distance-between-hole-edge-and-edge 1.5)
(def IS31FL3731-length-between-centre-of-screw-holes (- IS31FL3731-length  IS31FL3731-screwsize  (* IS31FL3731-vertical-distance-between-hole-edge-and-edge 2)))
(def IS31FL3731-width-between-centre-of-screw-holes (- IS31FL3731-width  IS31FL3731-screwsize  (* IS31FL3731-horizontal-distance-between-hole-edge-and-edge 2)))

(def IS31FL3731-hole-size 3)
(def IS31FL3731-hole-height 6)
(def IS31FL3731-screw-hole-outer-size (+ IS31FL3731-hole-size 2))
(def IS31FL3731-screw-hole (binding [*fn* 36] (cylinder (/ IS31FL3731-hole-size 2) (+ IS31FL3731-hole-height 0.3))))
(def IS31FL3731-screw-hole-outer (binding [*fn* 36] (cylinder (/ IS31FL3731-screw-hole-outer-size 2) IS31FL3731-hole-height)))

(def IS31FL3731-screw-mount
  (difference IS31FL3731-screw-hole-outer
              (translate [0 0 -0.1] IS31FL3731-screw-hole)))

(def IS31FL3731-top-right-position [IS31FL3731-width-between-centre-of-screw-holes IS31FL3731-length-between-centre-of-screw-holes 0])

(def IS31FL3731-top-left-position [0 IS31FL3731-length-between-centre-of-screw-holes 0])

(def IS31FL3731-bottom-left [0 0 0])

(def IS31FL3731-bottom-right [IS31FL3731-width-between-centre-of-screw-holes 0 0])

(def IS31FL3731-screw-mounts-set
  (union
   (translate IS31FL3731-bottom-left IS31FL3731-screw-mount)
   (translate IS31FL3731-bottom-right IS31FL3731-screw-mount)
   (translate IS31FL3731-top-left-position IS31FL3731-screw-mount)
   (translate IS31FL3731-top-right-position IS31FL3731-screw-mount)))