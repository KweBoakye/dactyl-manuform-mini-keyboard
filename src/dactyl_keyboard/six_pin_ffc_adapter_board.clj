(ns dactyl-keyboard.six-pin-ffc-adapter-board
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]))

(def six-pin-ffc-adapter-board-width 17)
(def six-pin-ffc-adapter-board-height 26)
(def six-pin-ffc-adapter-board-thickness 1)
(def six-pin-ffc-adapter-board-dimensions [six-pin-ffc-adapter-board-width six-pin-ffc-adapter-board-height six-pin-ffc-adapter-board-thickness])
(def six-pin-ffc-adapter-board-distance-from-bottom-to-edge-of-mounting-hole 7)
(def six-pin-ffc-adapter-mounting-hole-diameter 3)
(def six-pin-ffc-adapter-mounting-hole-radius (/ six-pin-ffc-adapter-mounting-hole-diameter 2))
(def six-pin-ffc-adapter-board-distance-from-bottom-to-centre-of-mounting-hole (+ six-pin-ffc-adapter-board-distance-from-bottom-to-edge-of-mounting-hole six-pin-ffc-adapter-mounting-hole-radius))
(def six-pin-ffc-adapter-horizonal-distance-from-edge-to-edge-of-mounting-hole 1)
(def six-pin-ffc-adapter-horizonal-distance-from-edge-to-centre-of-mounting-hole (+ six-pin-ffc-adapter-horizonal-distance-from-edge-to-edge-of-mounting-hole six-pin-ffc-adapter-mounting-hole-radius))
(def six-pin-ffc-adapter-board-standoff-height 4)
(def six-pin-ffc-adapter-board-standoff-inner-diameter 5)
(def six-pin-ffc-adapter-board-standoff-inner-radius (/ six-pin-ffc-adapter-board-standoff-inner-diameter 2))
(def six-pin-ffc-adapter-board-standoff-outer-diameter (+ six-pin-ffc-adapter-board-standoff-inner-diameter 2))
(def six-pin-ffc-adapter-board-standoff-outer-radius (/ six-pin-ffc-adapter-board-standoff-outer-diameter 2))
(def six-pin-ffc-adapter-board-body
  (->>
   (cube six-pin-ffc-adapter-board-width six-pin-ffc-adapter-board-height six-pin-ffc-adapter-board-thickness)
   (translate [0 0 (/ six-pin-ffc-adapter-board-thickness 2)])))

(def six-pin-ffc-adapter-mounting-hole
  (->>
   (cylinder six-pin-ffc-adapter-mounting-hole-radius (+ six-pin-ffc-adapter-board-thickness 0.2) :center false)
   (translate [0 0 -0.1])
   (with-fn 36)))

(def six-pin-ffc-adapter-mounting-hole-position-left
  [(+ (- (/ six-pin-ffc-adapter-board-width 2)) six-pin-ffc-adapter-horizonal-distance-from-edge-to-centre-of-mounting-hole)
   (+ (- (/ six-pin-ffc-adapter-board-height 2)) six-pin-ffc-adapter-board-distance-from-bottom-to-centre-of-mounting-hole)
   0])

(def six-pin-ffc-adapter-mounting-hole-position-right
  [(+ (- (/ six-pin-ffc-adapter-board-width 2) six-pin-ffc-adapter-horizonal-distance-from-edge-to-centre-of-mounting-hole))
   (+ (- (/ six-pin-ffc-adapter-board-height 2)) six-pin-ffc-adapter-board-distance-from-bottom-to-centre-of-mounting-hole)
   0])

(def six-pin-ffc-adapter-mounting-hole-left
  (translate six-pin-ffc-adapter-mounting-hole-position-left six-pin-ffc-adapter-mounting-hole))

(def six-pin-ffc-adapter-mounting-hole-right
  (translate six-pin-ffc-adapter-mounting-hole-position-right six-pin-ffc-adapter-mounting-hole))

(def six-pin-ffc-adapter-board
  (difference
   six-pin-ffc-adapter-board-body
   six-pin-ffc-adapter-mounting-hole-left
   six-pin-ffc-adapter-mounting-hole-right))

(def six-pin-ffc-adapter-board-standoff
  (standoff six-pin-ffc-adapter-board-standoff-inner-radius six-pin-ffc-adapter-board-standoff-outer-radius six-pin-ffc-adapter-board-standoff-height)
  )

(def six-pin-ffc-adapter-board-standoff-left
  (translate six-pin-ffc-adapter-mounting-hole-position-left six-pin-ffc-adapter-board-standoff)
  )

(def six-pin-ffc-adapter-board-standoff-right
  (translate six-pin-ffc-adapter-mounting-hole-position-right six-pin-ffc-adapter-board-standoff))

(def six-pin-ffc-adapter-standoffs
   (multiple-standoffs six-pin-ffc-adapter-board-standoff-inner-radius
                       six-pin-ffc-adapter-board-standoff-outer-radius
                       six-pin-ffc-adapter-board-standoff-height
                       [six-pin-ffc-adapter-mounting-hole-position-left six-pin-ffc-adapter-mounting-hole-position-right])
  )

(def six-pin-ffc-adapter-test
  (union
   (translate [0 0 six-pin-ffc-adapter-board-standoff-height] six-pin-ffc-adapter-board)
  six-pin-ffc-adapter-standoffs
   )
  )

(defn six-pin-ffc-adapter-place [shape]
  (translate [-90 -15 0] shape)
  )