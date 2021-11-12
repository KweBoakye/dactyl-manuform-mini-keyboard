(ns dactyl-keyboard.oled
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.web-connecters :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [unicode-math.core :refer :all]))

;from https://github.com/oysteinkrog/dactyl-manuform-mini-keyboard
;;;;;;;;;;;;;;;;;;;;;;;;
;; OLED screen holder ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(def oled-pcb-size [27.35 28.3 (- plate-thickness 1)])
(def oled-screen-offset [0 -0.5 0])
(def oled-screen-size [24.65 16.65 (- plate-thickness 1)])
(def oled-viewport-size [24.0 13.0 (+ 0.1 plate-thickness)])
(def oled-viewport-offset [0 1.0 0])
(def oled-mount-size [20.2 23.75 0.5])
(def oled-holder-width (+ 3 (nth oled-pcb-size 0)))
(def oled-holder-height (+ 3 (nth oled-pcb-size 1)))
(def oled-holder-thickness plate-thickness)
(def oled-holder-size [oled-holder-width oled-holder-height oled-holder-thickness])
(def oled-mount-rotation-x-old (deg2rad 20))
(def oled-mount-rotation-z-old (deg2rad -3))

(def oled-post (->> (web-post-shape oled-holder-thickness)
                    (translate [0 0 (+ (/ oled-holder-thickness -2) plate-thickness)])))

(def oled-holder-cut
  (->>
   (union
      ; cut for oled pcb
    (difference
     (translate [0 0 1] (apply cube (add-vec [0.5 0.5 0.1] oled-pcb-size)))
     (for [x [-2 2] y [-2 2]]
       (translate (div-vec oled-mount-size [x y 1])
                  (binding [*fn* 36] (cylinder 2.5 (- oled-holder-thickness 2.5))))))
      ; cut for oled screen
    (translate oled-screen-offset (apply cube oled-screen-size))
      ; cut for oled screen viewport
    (translate oled-viewport-offset (apply cube oled-viewport-size))
      ; cutout for oled cable
    (->> (cube 10 2 10)
         (translate oled-screen-offset)
         (translate [0 (- (+ (/ (nth oled-screen-size 1) 2) 1)) (+ plate-thickness 1.0)]))
    (for [x [-2 2] y [-2 2]]
      (translate (div-vec oled-mount-size [x y 1]) (binding [*fn* 36] (cylinder (/ 2.5 2) 10)))))
   (rdy 180)
   (translate [0 0 (/ oled-holder-thickness 2)])))

(def oled-holder
  (->>
    ; main body
   (apply cube oled-holder-size)
   (rdy 180)
   (translate [0 0 (/ oled-holder-thickness 2)])))