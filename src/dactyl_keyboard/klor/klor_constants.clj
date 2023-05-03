(ns dactyl-keyboard.klor.klor-constants
  (:refer-clojure :exclude [use import])
  )

(def key-hole-height 14)
(def key-hole-width 14)
(def key-corner-distance-horizontal (/ key-hole-width 2))
(def key-corner-distance-vertical (/ key-hole-height 2))
(def key-case-corner-distance-horizontal (+ key-corner-distance-horizontal 5.5))
(def key-case-corner-distance-vertical (+ key-corner-distance-vertical 5.5))

(def key-inside-corner-bl [(/ key-hole-width -2) (/ key-hole-height -2) 0])
(def key-inside-corner-br [(/ key-hole-width 2) (/ key-hole-height -2) 0])
(def key-inside-corner-tl [(/ key-hole-width -2) (/ key-hole-height 2) 0])
(def key-inside-corner-tr [(/ key-hole-width 2) (/ key-hole-height 2) 0])