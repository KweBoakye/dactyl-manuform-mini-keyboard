(ns dactyl-keyboard.plate
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.shape-parameters :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.placement-functions :refer :all]
            [dactyl-keyboard.web-connecters :refer :all]
            [dactyl-keyboard.thumbs :refer :all]
             [dactyl-keyboard.hotswap :refer :all]
            [dactyl-keyboard.trackball :refer :all]
            [dactyl-keyboard.case :refer :all]
            [dactyl-keyboard.screw-inserts :refer :all]
            ))

(def bottom-plate-thickness 2.6)

(def right-wall-plate
  (let [tr (if (true? pinky-15u) wide-post-tr web-post-tr)
        br (if (true? pinky-15u) wide-post-br web-post-br)
        hull-with (translate (key-position 0 0 [0 0 0]) (square 1 1))]
    (union (hull (cut (key-wall-brace lastcol 0 0 1 tr lastcol 0 1 0 tr)) hull-with)
           (for [y (range 0 lastrow)] (hull (cut (key-wall-brace lastcol y 1 0 tr lastcol y 1 0 br)) hull-with))
           (for [y (range 1 lastrow)] (hull (cut (key-wall-brace lastcol (dec y) 1 0 br lastcol y 1 0 tr)) hull-with))
           (hull (cut (key-wall-brace lastcol cornerrow 0 -1 br lastcol cornerrow 1 0 br)) hull-with))))


(def plate-attempt (difference
                    (extrude-linear {:height bottom-plate-thickness}
                                    (union
                                     ; pro micro wall
                                     (for [x (range 0 (- ncols 1))] (hull  (cut (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr)) (translate (key-position x (- lastrow innercol-offset) [0 0 0]) (square (+ keyswitch-width 15) keyswitch-height))))
                                     (for [x (range 1 ncols)] (hull (cut (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr)) (translate (key-position x 2 [0 0 0]) (square 1 1))))
                                     (hull (cut right-wall) (translate (key-position lastcol 0 [0 0 0]) (square keyswitch-width keyswitch-height)))
                                     (hull (cut mini-thumb-wall) (translate bl-minithumb-loc (square 1 1)))
                                     right-wall-plate
                                     (hull (cut back-convex-thumb-wall-0) (translate bl-minithumb-loc (square 1 1)))
                                     (hull (cut back-convex-thumb-wall-1) (translate bl-minithumb-loc (square 1 1)))
                                     (hull (cut back-convex-thumb-wall-2) (translate bl-minithumb-loc (square 1 1)))
                                     (hull (cut thumb-corners))
                                     (if (or trackball-enabled joystick-enabled) nil (hull (cut thumb-to-front-wall) (translate (key-position (- lastcol 1) (- lastrow 1) [0 0 0]) (square 1 1))))
                                     (hull (cut non-thumb-walls))))
                    (translate [0 0 -10] screw-insert-screw-holes)))


(def trackball-subtract (union
                         ; Subtract out the actual trackball
                         (translate trackball-origin (dowell-angle raised-trackball))
                         ; Subtract out space for the cup, because sometimes things from the keyboard creep in
                         (translate trackball-origin (sphere (/ trackball-width-plus-bearing 2)))
                         ; Just... double check that we have the full dowell negative
                         (translate trackball-origin rotated-dowells)
                         ;key-trackball-clearance
                         hotswap-clearance))

(def plate-trackball-hole-healer 
  (->>
   (cube 50 40 bottom-plate-thickness)
   (rotate (deg2rad 30 ) [0 0 1])
   (translate [-55 -40 (/ bottom-plate-thickness 2)])))

(def trackball-mount-translated-to-model (difference
                                          (union
                                           (translate trackball-origin trackball-mount)
                                           trackball-walls
                                           trackball-to-case)
                                          trackball-subtract
                                          key-clearance
                                          thumb-key-clearance
                                          (translate trackball-origin trackball-insertion-cyl)))