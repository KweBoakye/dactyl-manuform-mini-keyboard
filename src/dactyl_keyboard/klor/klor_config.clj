(ns dactyl-keyboard.klor.klor-config
  (:refer-clojure :exclude [use import])
  (:require 
   [dactyl-keyboard.tps-43 :refer :all])
  )

(def nrows 3)
(def ncols 6)
(def nthumb-keys 4)

(def key-spacing-horizontal 19.00)
(def key-spacing-vertical 19.00)

(def inner-index-stagger [0 4 0])
(def index-stagger [0 7 0])
(def middle-stagger [0 16 0])
(def ring-stagger [-0.5 9.75 0])
(def pinky-stagger [-0.5 4.75 0])
(def extra-pinky-stagger [-0.5 9.75 0])

(def inner-pre-translation [0 0 0])
(def index-pre-translation [0 0 0])
(def middle-pre-translation [0 0 0])
(def ring-pre-translation  [-12 -10  0]
  )
(def pinky-pre-translation  [0 0  0])
(def extra-pinky-pre-translation [0 0  0])

(def anchor-rotation 10)
(def inner-index-rotation 0)
(def index-rotation 0)
(def middle-rotation 0)
(def ring-rotation -4)
(def pinky-rotation -10)
(def extra-pinky-rotation -10)

(def thumb-anchor [22 -18 0])
(def thumb-splay 15)
(def thumb-spread -21.25)
(def thumb-origin [11 -1 0] )


(def keycap-width 18)
(def keycap-length 18)
(def keycap-height 18)

(def key-spacing-width 19.05)
(def key-spacing-length 19.05)
(def inner-spacing-width 16.5)
(def inner-spacing-length 16.5)

(def klor-dev-board-z-offset 3.5)
(def klor-case-walls-height (+ 14.5 ))

(def kailh-choc-hotswap-thickness 1.9)
(def pcb-thickness 1.6)
(def pcb-to-plate-distance 2.2)

(def klor-switchplate-thickness 1.2)
(def klor-bottom-plate-thickness 1.5)
(def klor-switchplate-z-position (+ 4.4 (/ klor-switchplate-thickness 2) ))
(def klor-thumb-walls-height (+ klor-switchplate-z-position  (/ klor-switchplate-thickness 2)) ;(+ 6.6)
  )
(def klor-wall-thickness 2)
(def klor-wall-z-offset -4)
(def klor-thumbs-wall-z-offset -2)
(def klor-wall-xy-offset 2)
(def klor-case-offset 0.1)
(def klor-pcb-mount-hole-radius 1.75)

(def tps-43-mount-inner-width (+ tps-43-width 1))
(def tps-43-mount-inner-length (+ tps-43-length 1))

(defn column-offset [column]
  (cond
    (= column 0) inner-index-stagger
    (= column 1) index-stagger
    (= column 2) middle-stagger
    (= column 3) ring-stagger
    (= column 4) pinky-stagger
    (= column 5) extra-pinky-stagger
    :else [0 0 0]))

(defn pre-translation [column]
  (cond 
    (= column 0) inner-pre-translation
    (= column 1) index-pre-translation
    (= column 2) middle-pre-translation
    (= column 3) ring-pre-translation
    (= column 4) pinky-pre-translation
    (= column 5) extra-pinky-pre-translation
    :else [0 0 0]))

(defn column-rotation [column] 
   (cond (= column 0) inner-index-rotation
         (= column 1) index-rotation
         (= column 2) middle-rotation
         (= column 3) ring-rotation
         (= column 4) pinky-rotation
         (= column 5) extra-pinky-rotation
         :else 0)
   )

(def trrs-jack-hole-radius 3.14)
(def reset-button-inner-radius 1)
(def reset-button-outer-radius 2.5)

