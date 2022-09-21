(ns dactyl-keyboard.low.case-low
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.sa-keycaps :refer :all]
            [dactyl-keyboard.low.placement-functions-low :refer :all]
            [dactyl-keyboard.low.web-connecters-low :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.low.thumbs-low :refer :all]
            [dactyl-keyboard.low.oled-low-placements :refer :all]
            [dactyl-keyboard.low.case-low-functions :refer :all] 
            [dactyl-keyboard.oled :refer :all]
             [dactyl-keyboard.tps-65 :refer :all]
            [dactyl-keyboard.EVQWGD001 :refer :all]
            ))



;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

 
 


 (def top-right-corner 
   (curved-corner 0 1 1 1 1 0 (partial key-place lastcol 0) oled-post-tr)

   )

 (def bottom-right-corner
   (curved-corner 0 -1 1 -1 1 0 (partial key-place lastcol cornerrow) oled-post-br)
   )

(def right-wall
  (let [tr (if (true? pinky-15u) wide-post-tr oled-post-tr)
        br (if (true? pinky-15u) wide-post-br oled-post-br)]
    (union 
     ;(key-wall-brace lastcol 0 0 1 tr lastcol 0 1 0 tr)
top-right-corner      
     (for [y (range 0 lastrow)] (key-wall-brace lastcol y 1 0 tr lastcol y 1 0 br))
           (for [y (range 1 lastrow)] (key-wall-brace lastcol (dec y) 1 0 br lastcol y 1 0 tr))
            ;(key-wall-brace lastcol cornerrow 0 -1 br lastcol cornerrow 1 0 br)
     bottom-right-corner      
     )))
 
 

(def back-wall 
   (union(for [x (range 0 ncols)] (key-wall-brace x 0 0 1 oled-post-tl x       0 0 1 oled-post-tr))
(for [x (range 1 ncols)] (key-wall-brace x 0 0 1 oled-post-tl (dec x) 0 0 1 oled-post-tr ))
  ))
 
 

(def left-wall-x-furthest -1.5)
(def left-wall-y-furthest 1.75)

(def left-wall-4-rows
  ; left wall
   (union

    (when (= screen-holder-mount-position "screen-holder-mount-top")(wall-brace-xy (partial key-place 0 0) 0 1 oled-post-tl  (partial screen-holder-translate-and-place (/ screen-holder-height 2) (/ screen-holder-width 2)  0) 0 1 oled-post wall-xy-offset wall-xy-offset-thin))
   ; (-# (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial key-place 0 0) 0 1 oled-post-tl (partial key-place 0 0) -1 1 oled-post-tl  wall-xy-offset wall-xy-offset)))
   (curved-corner 0 1 -1 1 -1 0 (partial key-place 0 0) oled-post-tl)
    (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial key-place 0 0) -1 0 oled-post-tl (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset)) 1 1 oled-post  wall-xy-offset wall-xy-offset))
    (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset)) 1 1 oled-post (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 1 0 oled-post wall-xy-offset wall-xy-offset ))
    ;(-# (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 1 0 oled-post (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 0 oled-post wall-xy-offset wall-xy-offset)))

    (curved-corner-xy 1 0 1 1 0 1 (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) oled-post wall-xy-offset)
     ;(when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 1 0 oled-post (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 oled-post wall-xy-offset wall-xy-offset))
     ;(when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 oled-post (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2) 0) 1 -1 oled-post wall-xy-offset wall-xy-offset-thin))
   
    ;(-# (when (= screen-holder-mount-position "screen-holder-mount-side")  (wall-brace-xy (partial screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2) 0) -1 -1 oled-post (partial screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2) 0) 0 -1 oled-post wall-xy-offset wall-xy-offset)))
   ;(when (= screen-holder-mount-position "screen-holder-mount-side")  (wall-brace-xy (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2) 0)  1 -1 oled-post (partial screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2) 0) -1 -1 oled-post wall-xy-offset wall-xy-offset))
    ; (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2) 0) 0 -1 oled-post  (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset) -1 0  oled-post wall-xy-offset wall-xy-offset-thin))
    (when (= screen-holder-mount-position "screen-holder-mount-side") (hull (bottom-hull ( screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2) 0 oled-post))  (wall-brace-xy-half-top (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left) 0 -1 oled-post wall-xy-offset)))
    (when (= screen-holder-mount-position "screen-holder-mount-side") (hull (bottom-hull (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2) 0 oled-post))  (wall-brace-xy-half-bottom (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left) 0 -1 oled-post wall-xy-offset)))
    ;(when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset) -1 0 oled-post   (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left) 0 -1 oled-post wall-xy-offset-thin wall-xy-offset))
    ;(when (= screen-holder-mount-position  "screen-holder-mount-top") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  (- tps-65-mount-corner-radius) ) 0 1 oled-post  (partial screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0) 0 0 oled-post wall-xy-offset-thin wall-xy-offset-thin))
    (when (= screen-holder-mount-position  "screen-holder-mount-top") (wall-brace-xy (partial screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0) 0 1 oled-post  (partial screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0) 0 1 oled-post wall-xy-offset-thin wall-xy-offset-thin))
    ;(wall-brace-xy (partial screen-holder-place-x-y  1.5   (+ left-wall-y-modifier left-wall-y-furthest -0.25)) 0 1 oled-post  (partial screen-holder-place-x-y left-wall-x-furthest    (+ left-wall-y-modifier left-wall-y-furthest -0.5)) 0 1 oled-post wall-xy-offset-thin wall-xy-offset-thin)
    (when (= screen-holder-mount-position  "screen-holder-mount-top") (wall-brace-xy (partial screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0) 0 1 oled-post  (partial screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0) 0 1 oled-post wall-xy-offset-thin wall-xy-offset-thin))
    ;(color [0 1 0 1](wall-brace-xy (partial screen-holder-place-x-y left-wall-x-furthest (+ left-wall-y-modifier left-wall-y-furthest -0.5)) 0 1 oled-post  (partial screen-holder-place-x-y left-wall-x-furthest    (+ left-wall-y-modifier left-wall-y-furthest -0.5)) -1 0 oled-post wall-xy-offset-thin wall-xy-offset-thin))
    ( when (= screen-holder-mount-position "screen-holder-mount-top") (color [0 1 0 1] (wall-brace-xy (partial screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0) 0 1 oled-post  (partial screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2) 0) -1 0 oled-post wall-xy-offset-thin wall-xy-offset-thin)))
    (when (= screen-holder-mount-position "screen-holder-mount-top")(color [1 0 0 1](wall-brace-xy (partial screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0) -1 0 oled-post  (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset   tps-65-mount-corner-radius-with-offset ) 1 -1 oled-post wall-xy-offset-thin wall-xy-offset-thin)))
    (when (= screen-holder-mount-position "screen-holder-mount-top") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset ) 1 -1 oled-post  (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset ) tps-65-mount-corner-radius-with-offset ) 0 0  oled-post wall-xy-offset-thin wall-xy-offset))
    ;(wall-brace (partial  translate (left-wall-plate-position left-wall-x-furthest (+ left-wall-y-modifier  -5.5))) 0 0  oled-post  (partial thumb-bl-place) -1 -1  thumb-post-tl)
    (wall-brace-xy   (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left) 0 -1 oled-post (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right) 0 -1 oled-post wall-xy-offset wall-xy-offset) 
    (wall-brace-xy (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right) 0 -1 oled-post (partial thumb-bl-place) -1 0 oled-post-tl wall-xy-offset wall-xy-offset)
    ;(wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset ) tps-65-mount-corner-radius-with-offset ) 0 0  oled-post  (partial thumb-bl-place) -1 0 oled-post-tl wall-xy-offset-thin wall-xy-offset)
    ;(wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position  0 tps-65-mount-corner-radius-with-offset) 0 -1  oled-post  (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset) tps-65-mount-corner-radius-with-offset) 0 -1  oled-post wall-xy-offset-thin wall-xy-offset-thin)
    ;(when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 oled-post (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2) 0) 1 -1 oled-post wall-xy-offset-thin wall-xy-offset-thin false))
   ;(when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 oled-post (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2) 0) 1 -1 oled-post wall-xy-offset wall-xy-offset false))
  ;;  (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy  (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2) 0) 1 -1 oled-post (partial screen-holder-translate-and-place-side (/  screen-holder-height 2) (/ (+ screen-holder-width) 2) 0) 0 0 oled-post wall-xy-offset-thin wall-xy-offset-thin false))
  ;;   (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy  (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2) 0) 1 -1 oled-post (partial screen-holder-translate-and-place-side (/  screen-holder-height 2) (/ (- screen-holder-width) 2) 0) 0 0 oled-post wall-xy-offset-thin wall-xy-offset-thin false))
  ;;  (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy  (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2) 0) 1 -1 oled-post (partial screen-holder-translate-and-place-side (/  (- screen-holder-height) 2) (/ (- screen-holder-width) 2) 0) 0 0 oled-post wall-xy-offset-thin wall-xy-offset-thin false))
  ; (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 oled-post (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset) -1 0  oled-post wall-xy-offset-thin wall-xy-offset-thin false))
   ; (-# (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset ) tps-65-mount-corner-radius-with-offset ) -1 0  oled-post  (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left) 0 -2 oled-post wall-xy-offset-thin wall-xy-offset))
   ; (wall-brace-xy (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left) 0 -2 oled-post (partial thumb-bl-place) -1 0 oled-post-tl wall-xy-offset wall-xy-offset)
;(wall-brace (partial key-place 0 0) 0 1 oled-post-tl (partial left-key-place 0 1) 0 1 oled-post)
;(wall-brace (partial left-key-place 0 1) 0 1 oled-post (partial left-key-place 0 1) -1 0 oled-post)
    ))

(def left-wall-5-rows
  ; left wall
  (union
;    (for [y (range 0 lastrow)] (union (wall-brace (partial left-key-place y 1)       -1 0 web-post (partial left-key-place y -1) -1 0 web-post)
;                                     (hull (key-place 0 y web-post-tl)
;                                           (key-place 0 y web-post-bl)
;                                           (left-key-place y  1 web-post)
;                                           (left-key-place y -1 web-post))))
;
;
;(for [y (range 1 lastrow)] (union (wall-brace (partial left-key-place (dec y) -1) -1 0 web-post (partial left-key-place y  1) -1 0 web-post)
;                                  (hull (key-place 0 y       web-post-tl)
;                                        (key-place 0 (dec y) web-post-bl)
;                                        (left-key-place y        1 web-post)
;                                        (left-key-place (dec y) -1 web-post))))
   (wall-brace-xy (partial key-place 0 0) 0 1 web-post-tl  (partial left-wall-plate-place 1 2.5) 0 1 oled-post wall-xy-offset wall-xy-offset-thin)
   (wall-brace-xy  (partial left-wall-plate-place 1 1.5) 0 1 oled-post  (partial left-wall-plate-place -2.5 1.5) 0 1 oled-post wall-xy-offset-thin wall-xy-offset-thin)
   (wall-brace-xy  (partial left-wall-plate-place -2.5 1.5) 0 1 oled-post  (partial left-wall-plate-place -2.5 1.5) -1 0 oled-post wall-xy-offset-thin wall-xy-offset-thin)
   (wall-brace-xy  (partial left-wall-plate-place -2.5 1.5) -1 0 oled-post  (partial left-wall-plate-place -2.5 -1) -1 -1 oled-post wall-xy-offset-thin wall-xy-offset-thin)
   (wall-brace-xy (partial left-wall-plate-place -2.5 -1) -1 -1 oled-post  (partial left-wall-plate-place -2.5 -5) 0 0  web-post-br wall-xy-offset-thin wall-xy-offset)
   (wall-brace-xy (partial left-wall-plate-place -2.5 -5) 0 0  web-post-br  (partial thumb-bl-place) -1 0  thumb-post-tl wall-xy-offset-thin wall-xy-offset)
;(wall-brace (partial key-place 0 0) 0 1 web-post-tl (partial left-key-place 0 1) 0 1 web-post)
;(wall-brace (partial left-key-place 0 1) 0 1 web-post (partial left-key-place 0 1) -1 0 web-post)
   ))




  (def left-wall left-wall-4-rows)


(defn apply-key-geometry-rotation-variable [rot1-x rot1-y rot1-z x-rot-slice y-rot-slice z-rot-slice rotate-x-fn rotate-y-fn rotate-z-fn index shape] 
  (->> shape
         (rotate-x-fn (+ rot1-x (* x-rot-slice index)))
       (rotate-z-fn (+ rot1-z (* z-rot-slice index)))  
       (rotate-y-fn (+ rot1-y (* y-rot-slice  index)))
        
       )
  )
  

(defn between-ring-and-pinky-front-wall [{:keys [target1-dx target1-dy target1-place target1-post target1-rad-or-deg
                                                 dx1 dy1 place1 rotate1 post-position-1
                                           dxmid1 dymid1 place-mid1 rotatemid1 post-position-mid1
                                           dxmid2 dymid2 place-mid2 rotatemid2 post-position-mid2
                                           dxmid3 dymid3 place-mid3 rotatemid3 post-position-mid3
                                                 dx2 dy2 place2 rotate2 post-position-2
                                         target2-dx target2-dy target2-place target2-post target2-rad-or-deg
                                                 rotation-values1 rotation-values2 rotate-x-fn rotate-y-fn rotate-z-fn variable-rotation-fn
                                           post steps] :or {steps 20}}]
  (let [
       curved-wall (curved-corner-quartic
         {:target1-dx target1-dx :target1-dy target1-dy :target1-place target1-place :target1-post target1-post :target1-rad-or-deg target1-rad-or-deg
          :dx1 dx1 :dy1 dy1 :place1 place1 :rotate1 rotate1 :post-position-1 post-position-1
          :dxmid1 dxmid1 :dymid1 dymid1 :place-mid1 place-mid1 :rotatemid1 rotatemid1 :post-position-mid1 post-position-mid1
          :dxmid2 dxmid2 :dymid2 dymid2 :place-mid2 place-mid2 :rotatemid2 rotatemid2 :post-position-mid2 post-position-mid2
          :dxmid3 dxmid3 :dymid3 dymid3 :place-mid3 place-mid3 :rotatemid3 rotatemid3 :post-position-mid3 post-position-mid3
          :dx2 dx2 :dy2 dy2 :place2 place2 :rotate2 rotate2 :post-position-2 post-position-2
          :target2-dx target2-dx :target2-dy target2-dy :target2-place target2-place :target2-post target2-post :target2-rad-or-deg target2-rad-or-deg
          :rotation-values1 rotation-values1 :rotation-values2 rotation-values2  :rotate-x-fn rotate-x-fn  :rotate-y-fn rotate-y-fn :rotate-z-fn rotate-z-fn :variable-rotation-fn variable-rotation-fn
          :post post :steps steps})
  ]
    (union
     curved-wall
     )
    
    )

  )

(defn between-middle-and-thumb-front-wall [{:keys [dx1 dy1 place1 rotate1 post-position-1
                                                 dxmid1 dymid1 place-mid1 rotatemid1 post-position-mid1
                                                 dxmid2 dymid2 place-mid2 rotatemid2 post-position-mid2
                                                 dx2 dy2 place2 rotate2 post-position-2 
                                                 rotation-values1 rotation-values2 
                                                 rotate-x-fn rotate-y-fn rotate-z-fn variable-rotation-fn post steps] :or {steps 20}}]
  (let [curved-wall (curved-corner-cubic
                     {:dx1 dx1 :dy1 dy1 :place1 place1 :rotate1 rotate1 :post-position-1 post-position-1
                      :dxmid1 dxmid1 :dymid1 dymid1 :place-mid1 place-mid1 :rotatemid1 rotatemid1 :post-position-mid1 post-position-mid1
                      :dxmid2 dxmid2 :dymid2 dymid2 :place-mid2 place-mid2 :rotatemid2 rotatemid2 :post-position-mid2 post-position-mid2
                      :dx2 dx2 :dy2 dy2 :place2 place2 :rotate2 rotate2 :post-position-2 post-position-2 
                      :rotation-values1 rotation-values1 :rotation-values2 rotation-values2 :rotate-x-fn rotate-x-fn  :rotate-y-fn rotate-y-fn :rotate-z-fn rotate-z-fn :variable-rotation-fn variable-rotation-fn
                      
                      :post post :steps steps})]
    (union
     curved-wall)))


    
(def front-wall
  (union 
    

 
   ;(key-wall-brace )
    
   (cond (> ncols 5)
(union 
 (for [x (range 4 ncols)] (key-wall-brace x cornerrow 0 -1 oled-post-bl x       cornerrow 0 -1 oled-post-br)) ; TODO fix extra wall
(for [x (range 5 ncols)] (key-wall-brace x cornerrow 0 -1 oled-post-bl (dec x) cornerrow 0 -1 oled-post-br))
 (key-wall-brace 3 lastrow 0.5 -1 oled-post-br 4 cornerrow 0.5 -1 oled-post-bl)
 (key-wall-brace 3 lastrow   0 -1 oled-post-bl 3 lastrow 0.5 -1 oled-post-br)
 )
     :else
 (union
  (-# (key-place 3 lastrow oled-post-br))
  (curved-corner 0 -1 1 -1 1 0 (partial key-place 4 cornerrow) oled-post-br)
  ;(-# (translate (key-position  3 lastrow [0 0 0]) (apply-key-geometry-rotation 3 lastrow (translate oled-post-br-translation-vector oled-post))))
  ;(curved-corner -1 0 -1 -1 0 -1 (partial key-place 3 lastrow) oled-post-bl)
 ;(curved-corner 0 -1 1 -1 1 0 (partial key-place 3 lastrow) oled-post-br)
  (key-wall-brace 3 lastrow 0 -1 oled-post-bl 3 lastrow 0 -1 oled-post-br)
 ; (key-wall-brace 4 cornerrow 0 -1 oled-post-br 4 cornerrow 0 -1 oled-post-bm)
  ;(key-wall-brace 4 cornerrow 0.5 -1 oled-post-bl 4 cornerrow 0 -1 oled-post-br )
  (between-ring-and-pinky-front-wall 
   {:target1-dx 0 :target1-dy -1 :target1-place (partial key-place 4 cornerrow) :target1-post oled-post-br :target1-rad-or-deg :radians
    :dx1 0  :dy1 -0.9 :place1 (partial key-position 4 cornerrow) :rotate1 (partial apply-key-geometry-rotation 4 cornerrow)   :post-position-1 "br"
    :dxmid1 0 :dymid1 -0.7 :place-mid1 (partial key-position 4 cornerrow) :rotatemid1 (partial apply-key-geometry-rotation 4 cornerrow) :post-position-mid1 "bm" 
    ; :dxmid1 1  :dymid1 0 :place-mid1  (partial key-position 3 lastrow) :rotatemid1 (partial apply-key-geometry-rotation 3 lastrow) :post-position-mid1 "br"
     :dxmid2 1  :dymid2 -0.7 :place-mid2  (partial key-position 4 cornerrow) :rotatemid2 (partial apply-key-geometry-rotation 4 cornerrow) :post-position-mid2 "bl"
    :dxmid3 0.7  :dymid3 -0.7 :place-mid3  (partial key-position 3 lastrow) :rotatemid3 (partial apply-key-geometry-rotation 3 lastrow) :post-position-mid3 "br"
    :dx2 0.4 :dy2 -0.7 :place2  (partial key-position 3 lastrow) :rotate2 (partial apply-key-geometry-rotation 3 lastrow)   :post-position-2 "br"
    :target2-dx 0 :target2-dy -1 :target2-place (partial key-place 3 lastrow) :target2-post oled-post-br :target2-rad-or-deg :radians
    :rotation-values1  (apply-key-geometry-rotation-values 4 cornerrow) :rotation-values2 (apply-key-geometry-rotation-values 3 lastrow)  
    :rotate-x-fn rx  :rotate-y-fn ry :rotate-z-fn rz :variable-rotation-fn (partial apply-key-geometry-rotation-variable)
    :post oled-post :steps 20})
  
  ;; (curved-corner-quadratic
  ;;  {:dx1 0  :dy1 -1 :place1 (partial key-position 3 lastrow) :rotate1 (partial apply-key-geometry-rotation 3 lastrow)   :post-position-1 "br"
  ;;   :dxmid1 1 :dymid1 -1 :place-mid1 (partial key-position 3 lastrow) :rotatemid1 (partial apply-key-geometry-rotation 3 lastrow) :post-position-mid1 "br"
  ;;   ; :dxmid1 1  :dymid1 0 :place-mid1  (partial key-position 3 lastrow) :rotatemid1 (partial apply-key-geometry-rotation 3 lastrow) :post-position-mid1 "br"
  ;;   ; :dxmid2 1  :dymid2 -1 :place-mid2  (partial key-position 3 lastrow) :rotatemid2 (partial apply-key-geometry-rotation 3 lastrow) :post-position-mid2 "br"
  ;;   :dx2 -1 :dy2 0 :place2  (partial key-position 3 lastrow) :rotate2 (partial apply-key-geometry-rotation 3 lastrow)   :post-position-2 "br"
  ;;   :rotation-values1   (apply-key-geometry-rotation-values 3 lastrow):rotation-values2 (apply-key-geometry-rotation-values 3 lastrow)
  ;;   :rotate-x-fn rx  :rotate-y-fn ry :rotate-z-fn rz :variable-rotation-fn (partial apply-key-geometry-rotation-variable)
  ;;   :post oled-post :steps 20})
  
  (between-middle-and-thumb-front-wall
   {:dx1 0 :dy1 -1 :place1 (partial key-position 2 lastrow) :rotate1 (partial apply-key-geometry-rotation 2 lastrow)   :post-position-1 "bm"
    :dxmid1 -1 :dymid1 -1 :place-mid1 (partial key-position 2 lastrow) :rotatemid1 (partial apply-key-geometry-rotation 2 lastrow) :post-position-mid1  "br"
    :dxmid2 -1  :dymid2 0 :place-mid2  (partial key-position 3 lastrow) :rotatemid2 (partial apply-key-geometry-rotation 3 lastrow) :post-position-mid2 "bl"
    :dx2 0 :dy2 -1.1 :place2  (partial key-position 3 lastrow) :rotate2 (partial apply-key-geometry-rotation 3 lastrow)   :post-position-2 "bl"
    :rotation-values1  (apply-key-geometry-rotation-values 2 lastrow) :rotation-values2 (apply-key-geometry-rotation-values 3 lastrow)
    :rotate-x-fn rx  :rotate-y-fn ry :rotate-z-fn rz :variable-rotation-fn (partial apply-key-geometry-rotation-variable)
    :post oled-post :steps 20})
  
;;  (key-wall-brace 3 lastrow   0 -1 oled-post-bl 3 lastrow 0 -1 oled-post-br)
;;  (key-wall-brace 3 lastrow 0 -1 oled-post-br 4 cornerrow 0 -1 oled-post-br)
;;(hull 
;; (key-place lastcol cornerrow web-post-bl)
;; (key-place lastcol cornerrow web-post-br)
;; (key-place (dec lastcol) lastrow web-post-br)
;; )
  )    
         ))
  )



(defn thumb-place-rotation-variable [rot1-x rot1-y rot1-z x-rot-slice y-rot-slice z-rot-slice rotate-x-fn rotate-y-fn rotate-z-fn index shape]
  (->> shape
       (rotate-x-fn (+ rot1-x (* x-rot-slice (inc index))))
       (rotate-z-fn (+ rot1-z (* z-rot-slice (inc index))))
       (rotate-y-fn (+ rot1-y (* y-rot-slice (inc index))))))

(defn between-thumb-and-front-wall [{:keys [dx1 dy1 place1 rotate1 post-position-1 
                                            dxmid1 dymid1 place-mid1 rotatemid1 post-position-mid1 
                                            dxmid2 dymid2 place-mid2 rotatemid2 post-position-mid2  
                                            dx2 dy2 place2 rotate2 post-position-2
                                           rotation-values1 rotation-values2 rotate-x-fn rotate-y-fn rotate-z-fn variable-rotation-fn
                                            post steps] :or {steps 20}}]
  
   (let [web-post-curved-list (plot-bezier-points-variable-rotation   (wall-corner-cubic dx1 dy1 place1 (get-web-corner-translation-vector post-position-1)
                                                                                         dxmid1 dymid1 place-mid1 (get-web-corner-translation-vector post-position-mid1)
                                                                                         dxmid2 dymid2 place-mid2 (get-web-corner-translation-vector post-position-mid2)
                                                                                         dx2 dy2  place2 (get-web-corner-translation-vector post-position-2)
                                                                                         0 0 steps)
                                                                      web-post rotation-values1 rotation-values2 thumb-place-rotation-variable) 
         curved-wall (curved-corner-cubic
                      {:dx1 dx1 :dy1 dy1 :place1 place1 :rotate1 rotate1 :post-position-1 post-position-1
                       :dxmid1 dxmid1 :dymid1 dymid1 :place-mid1 place-mid1 :rotatemid1 rotatemid1 :post-position-mid1 post-position-mid1
                       :dxmid2 dxmid2 :dymid2 dymid2 :place-mid2 place-mid2 :rotatemid2 rotatemid2 :post-position-mid2 post-position-mid2
                       :dx2 dx2 :dy2 dy2 :place2 place2 :rotate2 rotate2 :post-position-2 post-position-2
                       :rotation-values1 rotation-values1 :rotation-values2 rotation-values2 :rotate-x-fn rotate-x-fn  :rotate-y-fn rotate-y-fn :rotate-z-fn rotate-z-fn :variable-rotation-fn variable-rotation-fn
                       :post post :steps steps})
         web-post-curved-list-last (last web-post-curved-list)
         fill-gap-to-thumb-tr (hull 
                               web-post-curved-list-last
                               (thumb-mr-place web-post-tr)
                               (thumb-tr-place web-post-br)
                               )
         ]
     (union
    curved-wall
    
    (for [index (range 0 (- (count web-post-curved-list) 1))]
           (hull
            (nth web-post-curved-list index)
            (nth web-post-curved-list (inc index))
            (thumb-mr-place web-post-tr) 
            )
          )
      fill-gap-to-thumb-tr
    ))
  )

(def thumb-walls
  (union 
   
   ;(curved-corner 1 1 1 1 1 0 (partial thumb-mr-place) oled-post-br)
   ;(curved-corner 1 0 1 1 1 1 (partial thumb-mr-place) oled-post-br)
   ;(curved-corner 1 1 1 2 2 2  (partial thumb-mr-place ) oled-post-br)
   ;(-# (curved-corner -1 -1 -1 -1 0 -1 (partial thumb-tr-place) oled-post-br))
     ;(translate (rotate-around-z-in-degrees 25 (rotate-around-y-in-degrees -23(rotate-around-x-in-degrees 10 [0 0 0])))
   
                ;; (between-thumb-and-front-wall 
                ;;  {:dx1 1 :dy1 0 :place1 (partial transform-position thumb-mr-place ) :rotate1 (partial rotate-position thumb-mr-rotate) :post-position-1 "br"
                ;;  :dxmid1 1 :dymid1 -1 :place-mid1 (partial transform-position thumb-mr-place) :rotatemid1 (partial rotate-position thumb-mr-rotate) :post-position-mid1 "tr" 
                ;;  :dxmid2 -0.5 :dymid2 -1 :place-mid2 (partial transform-position thumb-tr-place) :rotatemid2 (partial rotate-position thumb-tr-rotate) :post-position-mid2 "br"  
                ;;  :dx2 0 :dy2 -1 :place2 (partial transform-position thumb-tr-place) :rotate2 (partial rotate-position thumb-tr-rotate) :post-position-2 "br" 
                ;;   :rotation-values1 thumb-mr-rotation-values :rotation-values2 thumb-tr-rotation-values :rotate-x-fn  (partial rdx) :rotate-y-fn (partial rdy) :rotate-z-fn  (partial rdz)
                ;;   :variable-rotation-fn (partial thumb-place-rotation-variable)
                ;;   :post oled-post :steps 20} 
                ;;   )
                
                
   ;(curved-corner -1 -1 -1 -2 -1 -2  (partial thumb-tr-place) oled-post-br)
   
   ;(wall-brace thumb-mr-place 1 0 oled-post-br thumb-tr-place -1 -2  oled-post-br)
    
   ;(wall-brace thumb-mr-place  0 -1 oled-post-br thumb-tr-place  0 -1 oled-post-br)
    
   (translate (thumb-tr-position web-post-bl-translation-vector))
(thumb-wall-brace thumb-mr-place  0 -1 oled-post-br thumb-mr-place  0 -1 oled-post-bl thumb-mr-rotate thumb-mr-rotate)
(thumb-wall-brace thumb-br-place  0 -1 oled-post-br thumb-br-place  0 -1 oled-post-bl thumb-br-rotate thumb-br-rotate)
;(wall-brace thumb-bl-place  0  1 oled-post-tr thumb-bl-place  0  1 oled-post-tl)
(thumb-wall-brace thumb-br-place -1  0 oled-post-tl thumb-br-place -1  0 oled-post-bl thumb-br-rotate thumb-br-rotate)
(thumb-wall-brace thumb-bl-place -1  0 oled-post-tl thumb-bl-place -1  0 oled-post-bl thumb-bl-rotate thumb-bl-rotate)
   ))



(def thumb-corners
  (union
  (curved-corner -1 0 -1 -1 0 -1 (partial thumb-br-place) oled-post-bl)
  (curved-corner 0 -1 1 -1 1 0 (partial thumb-mr-place) oled-post-br) 
     ;(-# (wall-brace thumb-br-place -1  0 oled-post-bl thumb-br-place  0 -1 oled-post-bl))
;(wall-brace thumb-bl-place -1  0 oled-post-tl thumb-bl-place  0  1 oled-post-tl)
   ))



(def thumb-tweeners
     (union 
    (thumb-wall-brace thumb-mr-place  0 -1 oled-post-bl thumb-br-place  0 -1 oled-post-br thumb-mr-rotate thumb-br-rotate)
(thumb-wall-brace thumb-bl-place -1  0 oled-post-bl thumb-br-place -1  0 oled-post-tl  thumb-bl-rotate thumb-br-rotate)
;(wall-brace thumb-tr-place  0 -1 oled-post-br (partial key-place 3 lastrow)  0 -1 oled-post-bl (get-thumb-translate-fn thumb-tr-place) (partial key-position  3 lastrow) thumb-tr-rotate (partial apply-key-geometry-rotation 3 lastrow))
     )
  )





(def left-side 
  (triangle-hulls
   
   (triangle-hulls
    (screen-holder-place (translate [(/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0] web-post))
                    ;(left-wall-plate-place left-wall-x-furthest  (+ left-wall-y-modifier  -1) web-post)
    (screen-holder-place (translate [(/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0] web-post))
    (left-wall-plate-place left-wall-x-furthest    (+ left-wall-y-modifier left-wall-y-furthest -0.5) web-post)
                        ; (tps-65-place (translate tps-65-mount-corner-cylinder-top-right-position oled-post))

    (translate [0 5 0]  (tps-65-place (translate tps-65-mount-corner-cylinder-top-right-position tps-65-mount-post)))
    (screen-holder-place (translate [(/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0] web-post))
    (left-wall-plate-place left-wall-x-furthest    (+ left-wall-y-modifier left-wall-y-furthest -0.5) web-post))
   
   (triangle-hulls

    (tps-65-place (translate tps-65-mount-corner-cylinder-top-right-position (rdz -90 tps-65-mount-corner-quarter-cylinder)))
                    ;(translate [0 0 0] (screen-holder-place (translate [(/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2) 0] web-post)))
    (left-wall-plate-place left-wall-x-furthest    (+ left-wall-y-modifier left-wall-y-furthest -0.5) web-post)
    (left-wall-plate-place left-wall-x-furthest  (+ left-wall-y-modifier  -1) web-post)

                  ; (translate [0 1 0] (screen-holder-place (translate [(/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2) screen-holder-depth] web-post)))
                  ; (tps-65-place (translate tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-cylinder))
                  ; (translate [0 0.5 0] (screen-holder-place (translate [(/ (- screen-holder-height) 2) (/ screen-holder-width 2)  (- screen-holder-depth)] web-post)))

   ; (translate [0 2 0] (left-wall-plate-place left-wall-x-furthest    (+ left-wall-y-modifier left-wall-y-furthest -0.5) oled-post))
    )
   
   
      (tps-65-place (translate tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-cylinder))
   (left-wall-plate-place left-wall-x-furthest  (+ left-wall-y-modifier  -1) web-post)
   (tps-65-place (translate tps-65-mount-corner-cylinder-top-left-position tps-65-mount-corner-cylinder))

   ( translate (left-wall-plate-position left-wall-x-furthest (+ left-wall-y-modifier  -5.5)) web-post)
   (left-wall-plate-place left-wall-x-furthest  (+ left-wall-y-modifier  -1) web-post)
   (tps-65-place (translate tps-65-mount-corner-cylinder-top-left-position tps-65-mount-corner-cylinder))
   
   )
  )


(def left-side-new
  (union 
   (triangle-hulls
   (  screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0 oled-post)
   (screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post) 
      (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset   tps-65-mount-corner-radius-with-offset oled-post)
    
          (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset   tps-65-mount-corner-radius-with-offset oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
    (screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)

    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset   tps-65-mount-corner-radius-with-offset oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius)  tps-65-mount-corner-radius oled-post)

    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset   tps-65-mount-corner-radius-with-offset oled-post)
    (color [1 0 0 1](tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset)  tps-65-mount-corner-radius oled-post))
    (color [0 1 0 1](tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset)  tps-65-mount-corner-radius-with-offset oled-post))


    )
   )
)
  

(def thumb-side-normal
  
  (triangle-hulls
    (translate (left-wall-plate-position left-wall-x-furthest (+ left-wall-y-modifier  -5.5)) oled-post)
   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset)  tps-65-mount-corner-radius-with-offset oled-post)
   (thumb-bl-place thumb-post-tl)

   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset)  tps-65-mount-corner-radius-with-offset oled-post)
(thumb-bl-place thumb-post-tl)
   (thumb-bl-place thumb-post-tr)

   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset)  tps-65-mount-corner-radius-with-offset oled-post)
   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset)  (- tps-65-mount-corner-radius-with-offset) oled-post)
   (thumb-bl-place thumb-post-tl)

   (thumb-bl-place thumb-post-tr)
(thumb-tl-place thumb-post-tl)
   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset)  (- tps-65-mount-corner-radius-with-offset) oled-post)

   )
  )

(def thumb-side-EVQWGD001-mount
  (union
  ;(hull 
  ; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
  ; (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left oled-post)
  ; (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/ (- screen-holder-width) 2) 0 oled-post)
  ; )
 (triangle-hulls 
  ;(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
  (screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
(EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left oled-post)
(EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-left oled-post))
   
   (hull
    (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-left oled-post)
    (screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
    )
   
    (hull
     (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left oled-post)
(screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
     (screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
     )
   
;;    (-# (triangle-hulls (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
;; (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left oled-post)
;; (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left oled-post)))
   
   (triangle-hulls (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
(EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post))
   
   
   (triangle-hulls 
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
                   (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-left oled-post)
                   (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post))
   
   (triangle-hulls (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
                       (thumb-bl-place web-post-tr)
                   (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post))
   
   (hull
    ;(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
    
    (thumb-bl-place web-post-tr)
    (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right oled-post)
    (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post)
    )
   
;;    (-# (hull
;;      (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-left oled-post)
;; (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post)
;; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
;;     ))
   (triangle-hulls
     

  
   
  ;(EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right oled-post)
  ; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset)  (- tps-65-mount-corner-radius-with-offset) oled-post)
  ; (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post)

 ; (thumb-bl-place thumb-post-tr)
  (thumb-tl-place web-post-tl)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
    (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post)

    (thumb-bl-place web-post-tr)
   (thumb-tl-place web-post-tl)
   (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post)

   (thumb-bl-place web-post-tr)
   (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post)
   (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right oled-post)

   (thumb-bl-place web-post-tr)
    (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right oled-post)
 ( thumb-bl-place web-post-tl)
    )
   
;;    (-# (triangle-hulls
;;     (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left oled-post)
;; (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right oled-post)
;; (thumb-bl-place web-post-tl)))
   
  (triangle-hulls
     (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
(thumb-bl-place web-post-tr)
(thumb-tl-place web-post-tl)
    )
   )
)
  
  (def thumb-side
    (if EVQWGD001-mount thumb-side-EVQWGD001-mount thumb-side-normal)
    )

(def thumb-side-to-trackpad-mount 
(triangle-hulls
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-mod)  tps-65-mount-corner-radius oled-post)

   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-mod)  tps-65-mount-corner-radius oled-post)
   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius)  (- tps-65-mount-corner-radius) oled-post)
   )
  
  
  )

(def between-thumb-cornerrow-and-trackpad
  (let [thumb-position (thumb-tl-place (partial map +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees web-post-tl-translation-vector)
        trackpad-corner (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) (partial map +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees [0 0 0])
        corner-key (key-position 0 cornerrow web-post-bl-translation-vector)
        trackpad-to-thumb-bezier-modifier-initial-point (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius)  (- tps-65-mount-corner-radius-with-offset-mod-neg) (partial map +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees [0 0 0])
        trackpad-to-thumb-bezier-modifier [(first trackpad-to-thumb-bezier-modifier-initial-point) (second trackpad-to-thumb-bezier-modifier-initial-point) (last thumb-position)]
        trackpad-to-thumb-bezier-points (bezier-quadratic trackpad-corner trackpad-to-thumb-bezier-modifier thumb-position 20)
        corner-key-to-thumb-bezier-modifier [(first corner-key) (second corner-key) (last thumb-position)]
        corner-key-to-thumb-bezier-points (bezier-quadratic corner-key corner-key-to-thumb-bezier-modifier thumb-position 20)
        ]

    (hull
     (plot-bezier-points trackpad-to-thumb-bezier-points curve-post)
     (plot-bezier-points corner-key-to-thumb-bezier-points curve-post)
     (thumb-tl-place web-post-tl)
 (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
 (key-place 0 cornerrow web-post-bl)
     ;(translate thumb-position (->> oled-post
     ;                               (rdx  10)
     ;                               (rdy -23)
     ;                               (rdz  25)))
     ;(translate trackpad-corner oled-post)
     ;(translate corner-key web-post)
    )
    )
  
  )
(def row-at-top-of-trackpad
  (cond
    (= nrows 5) 1
    (= nrows 4) 0
    (= nrows 3) 0
    ))


(def right-side
  (union
   (triangle-hulls

(when (= screen-holder-mount-position "screen-holder-mount-top") 
  (key-place 0 0 web-post-tl)
    (screen-holder-translate-and-place (/  screen-holder-height 2) (/ screen-holder-width 2)  0 oled-post)
    (screen-holder-translate-and-place (/  screen-holder-height 2) (/ (- screen-holder-width) 2)  0 oled-post)
    
    (key-place 0 0 web-post-tl)
    (key-place 0 0 web-post-bl)
    (screen-holder-translate-and-place (/  screen-holder-height 2) (/ screen-holder-width 2)  0 oled-post)

     (key-place 0 0 web-post-bl)
(screen-holder-translate-and-place (/  screen-holder-height 2) (/ screen-holder-width 2)  0 oled-post)
    (screen-holder-translate-and-place (/  screen-holder-height 2) (/ (- screen-holder-width) 2)  0 oled-post)
     
(key-place 0 0 web-post-bl)
    (key-place 0 1 web-post-tl)
    (screen-holder-translate-and-place (/  screen-holder-height 2) (/ (- screen-holder-width) 2)  0 oled-post))

   
    
 ;   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
 ;  (key-place 0 0 web-post-tl)
 ; (translate [0 0 -1] (left-wall-plate-place  1.5   (+ left-wall-y-modifier left-wall-y-furthest -0.25) oled-post))

 ;  (left-wall-plate-place  1.5   (+ left-wall-y-modifier left-wall-y-furthest -0.25) oled-post)
 ;  (translate [0 0 0] (screen-holder-place (translate [(/ screen-holder-height 2) (/  screen-holder-width 2) 0] web-post)))
 ;   (translate [0 0 0] (screen-holder-place (translate [(/ screen-holder-height 2)  (/  (- screen-holder-width) 2) 0] web-post)))
    
;     (left-wall-plate-place  1.5   (+ left-wall-y-modifier left-wall-y-furthest -0.25) oled-post)
;(translate [0 5 0]  (tps-65-place (translate tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-post)))
;(translate [0 0 0] (screen-holder-place (translate [(/ screen-holder-height 2)  (/  (- screen-holder-width) 2) 0] web-post)))
   ;  (translate [0 0 0] (screen-holder-place (translate [(/ screen-holder-height 2) (/  screen-holder-width 2) 0] web-post)))
   ;(translate [0 0 0] (screen-holder-place (translate [(/ screen-holder-height 2)  (/  (- screen-holder-width) 2) 0] web-post)))
   ;  (tps-65-place (translate tps-65-mount-corner-cylinder-bottom-right-position web-post))
    )

   (-# between-thumb-cornerrow-and-trackpad)
;; (-# (hull 
;;  (thumb-tl-place web-post-tl)
;; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
;; (key-place 0 cornerrow web-post-bl)
;;  )  ) 
   (triangle-hulls
    

    (key-place 0 cornerrow web-post-bl)
    (key-place 0 cornerrow web-post-tl)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)

    (key-place 0 (- cornerrow 1) web-post-bl)
    (key-place 0 cornerrow web-post-tl)
    (tps-65-translate-and-place-with-radius-xyz  0 (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)

    (key-place 0 cornerrow web-post-tl)
    (tps-65-translate-and-place-with-radius-xyz  0 (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)

    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius)  (- tps-65-mount-corner-radius) oled-post)
    (tps-65-translate-and-place-with-radius-xyz  0 (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)

    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius)  (- tps-65-mount-corner-radius) oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post))


 ( triangle-hulls
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius (- tps-65-mount-corner-radius) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius)  (- tps-65-mount-corner-radius) oled-post)
 

  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius (- tps-65-mount-corner-radius) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius)  (- tps-65-mount-corner-radius) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)


 
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius (- tps-65-mount-corner-radius) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius)  (- tps-65-mount-corner-radius) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
 
 (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
 (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
  (tps-65-translate-and-place-with-radius-xyz  0 (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)
  )
  
;    (tps-65-translate-and-place-with-radius-xyz  0 (- (- (/ tps-65-length 2) tps-65-corner-radius)) 0 0 (- tps-65-mount-corner-radius) oled-post)
;(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset)  (- tps-65-mount-corner-radius-with-offset) oled-post)
;    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset)  (- tps-65-mount-corner-radius-with-offset) oled-post)

(triangle-hulls
   (key-place 0 (- cornerrow 1) web-post-bl)
   (key-place 0 (- cornerrow 1) web-post-tl)
   (tps-65-translate-and-place-with-radius-xyz  0 (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)

    (key-place 0 (- cornerrow 1) web-post-tl)
    (tps-65-translate-and-place-with-radius-xyz  0 (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)
    (tps-65-translate-and-place-with-radius-xyz  (/ tps-65-width 3) (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)

    (key-place 0 (- cornerrow 1) web-post-tl)
    (key-place 0 row-at-top-of-trackpad web-post-bl)
    (tps-65-translate-and-place-with-radius-xyz  (/ tps-65-width 3) (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)

    (key-place 0 row-at-top-of-trackpad web-post-bl)
    (tps-65-translate-and-place-with-radius-xyz  (/ tps-65-width 3) (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)

    (key-place 0 row-at-top-of-trackpad web-post-bl)
    (key-place 0 row-at-top-of-trackpad web-post-tl)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
    
    
    ;(key-place 0 row-at-top-of-trackpad web-post-tl)
    ;(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius (- tps-65-mount-corner-radius) oled-post)
    ;(screen-holder-translate-and-place (/ screen-holder-height 2) (- (/ screen-holder-width 2)) 0 oled-post)
    )
   
   ))

 (def tps-65-mount-cyl
   (binding [*fn* 100] (cylinder tps-65-mount-corner-radius 2 :center true))
   )
 
 
(def between-screen-and-trackpad-top
  (triangle-hulls
        (screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
(screen-holder-translate-and-place (/ screen-holder-height 2) (/ (- screen-holder-width) 2)  0 oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius (- tps-65-mount-corner-radius) oled-post)


(screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
(screen-holder-translate-and-place (/ screen-holder-height 2) (/ (- screen-holder-width) 2)  0 oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)

   ;(translate [0 0 0] (screen-holder-place (translate [0 (/  (- screen-holder-width) 2) 0] web-post)))
(screen-holder-translate-and-place 0 (/  (- screen-holder-width) 2) 0 oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius (- tps-65-mount-corner-radius) oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)

(screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
(screen-holder-translate-and-place (/ screen-holder-height 2) (/ (- screen-holder-width) 2)  0 oled-post)
(tps-65-translate-and-place-with-radius-xyz (- (/ tps-65-width 2) tps-65-corner-radius) 0 0 tps-65-mount-corner-radius 0 oled-post)
)
  )
 
 (defn bottom-of-screen-bezier [place]
   
   (mapv (fn [point]
          (->>
           (translate point curve-post)
           (rdx 10)
           (rdy  screen-holder-rotate-side-y)
           (place))
   )
           (fillet-about-point-xy-2 0 -1 wall-xy-offset-thin 20)
   )
   
 )
 (def between-screen-and-trackpad-side 
  (union
   (hull
    (wall-bezier-1-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 wall-xy-offset oled-post)
    (wall-bezier-1-xy (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0) 0 1 wall-xy-offset-thin oled-post))
   (hull
    (wall-bezier-1-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 wall-xy-offset oled-post)
    ;(wall-bezier-1-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset) 0 1 wall-xy-offset-thin oled-post)
    ;( screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
   ; (wall-bezier-1-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 wall-xy-offset oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
    )
    (hull
    (screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
    (screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
    )

  ;;  (-# (hull 
  ;;  ; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
  ;;    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
  ;;   (screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
  ;;   (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
  ;;   ))

   (hull
    (bottom-hull 
     (wall-bezier-2-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 wall-xy-offset oled-post)
     (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset (translate (wall-locate3-xy 0 1 wall-xy-offset) oled-post)))
   
    (wall-bezier-1-xy (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0) 0 1 wall-xy-offset-thin oled-post)
    ( screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
    (bottom 0.001 (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post))

    ;(bottom 0.001 (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset (translate (wall-locate3-xy 0 1 wall-xy-offset) oled-post)))
    )

   (hull
    (wall-bezier-1-xy (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0) 0 1 wall-xy-offset-thin oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
    (tps-65-translate-and-place-with-radius [0 (- (/ tps-65-length 2) tps-65-corner-radius) 0] 0  tps-65-mount-corner-radius-with-offset oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
    ;(wall-bezier-1-xy (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0) 0 1 wall-xy-offset-thin oled-post)
    (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
    (screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post))

   (hull
    (wall-bezier-2-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 wall-xy-offset oled-post)
    (wall-bezier-1-xy (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0) 0 1 wall-xy-offset-thin oled-post)
    ( screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
    )

   (bottom-hull 
    
    (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
    (screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
;    ( screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 curve-post)
;( screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  oled-post-size  curve-post)
;    (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 curve-post)
;(screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  oled-post-size  oled-)
    )

;          (triangle-hulls
;    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
;(screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
;(screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/  screen-holder-width 2)  0  oled-post))

;;  (triangle-hulls
  ;; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
;;  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
;;  (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post))

  ;(triangle-hulls
  ; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)  
  ; (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/  screen-holder-width 2)  0 oled-post)
  ; (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post))
  
   ; (-# (triangle-hulls
  ;  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
  ;  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
  ;  (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post)))
;
;  (hull 
;   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
;screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post)
;tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post))
;  
   (triangle-hulls
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)

  ;  (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post)
  ;  (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2)  0 oled-post)
  ;  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)

    

    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius)  tps-65-mount-corner-radius oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post))
   ;(triangle-hulls
    ;(screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post)
    ;(screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2)  0 oled-post)
   ; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post))
   ) 
    


;(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius)  tps-65-mount-corner-radius oled-post)
;(screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post)
;(screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/  screen-holder-width 2)  0 oled-post)
    
  ; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius)  tps-65-mount-corner-radius oled-post) 
  ;  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
  ; (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/  screen-holder-width 2)  0 oled-post)
    
;    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius)  tps-65-mount-corner-radius oled-post)
;    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset)  tps-65-mount-corner-radius-with-offset oled-post)
;(screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post)
    
    )

(def top-side
  (union  
   (triangle-hulls
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius (- tps-65-mount-corner-radius) oled-post) 

     (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
    
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset tps-65-mount-corner-radius-with-offset oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius (- tps-65-mount-corner-radius) oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
    )
   
   (triangle-hulls
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset tps-65-mount-corner-radius-with-offset oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post) 
(key-place 0 0 web-post-tl)
    )
   
   )
  )
(def between-screen-and-trackpad 
    
     (cond
  (= screen-holder-mount-position "screen-holder-mount-top") between-screen-and-trackpad-top
  
 (= screen-holder-mount-position "screen-holder-mount-side") between-screen-and-trackpad-side
   
    
   ))
   
;; (def back-wall-to-screen 
;;   (let)
;;   )

(def left-section
  (union
   (when (= screen-holder-mount-position "screen-holder-mount-top")(screen-holder-place screen-holder))
   (when (= screen-holder-mount-position "screen-holder-mount-side") (screen-holder-place-side screen-holder))
   ;left-side
   (when (= screen-holder-mount-position "screen-holder-mount-top") ( color [0 1 0] left-side-new))
    thumb-side
   thumb-side-to-trackpad-mount
   right-side
    between-screen-and-trackpad
   top-side
   
  

    ))
(def default-thumb-wall 
  (union 
    ; thumb walls
   thumb-walls
   ; thumb corners
thumb-corners
   ; thumb tweeners
 thumb-tweeners
   ))
(def case-walls
  (union
   right-wall
   left-section
   ; back wall
  back-wall

   left-wall
  front-wall
   
  
   
   ))

(def cf-thumb-wall
  (union
   ; thumb walls
   (wall-brace cfthumb-mr-place  0 -1 web-post-br cfthumb-tr-place  0 -1 web-post-br)
   (wall-brace cfthumb-mr-place  0 -1 web-post-br cfthumb-mr-place  0 -1.15 web-post-bl)
   (wall-brace cfthumb-br-place  0 -1 web-post-br cfthumb-br-place  0 -1 web-post-bl)
   (wall-brace cfthumb-bl-place -0.3  1 thumb-post-tr cfthumb-bl-place  0  1 thumb-post-tl)
   (wall-brace cfthumb-br-place -1  0 web-post-tl cfthumb-br-place -1  0 web-post-bl)
   (wall-brace cfthumb-bl-place -1  0 thumb-post-tl cfthumb-bl-place -1  0 web-post-bl)
   ; cfthumb corners
   (wall-brace cfthumb-br-place -1  0 web-post-bl cfthumb-br-place  0 -1 web-post-bl)
   (wall-brace cfthumb-bl-place -1  0 thumb-post-tl cfthumb-bl-place  0  1 thumb-post-tl)
   ; cfthumb tweeners
   (wall-brace cfthumb-mr-place  0 -1.15 web-post-bl cfthumb-br-place  0 -1 web-post-br)
   (wall-brace cfthumb-bl-place -1  0 web-post-bl cfthumb-br-place -1  0 web-post-tl)
   (wall-brace cfthumb-tr-place  0 -1 web-post-br (partial key-place (+ innercol-offset 3) lastrow)  0 -1 web-post-bl)
   ; clunky bit on the top left cfthumb connection  (normal connectors don't work well)
   (bottom-hull
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (cfthumb-bl-place (translate (wall-locate2 -0.3 1) thumb-post-tr))
    (cfthumb-bl-place (translate (wall-locate3 -0.3 1) thumb-post-tr)))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (cfthumb-bl-place (translate (wall-locate2 -0.3 1) thumb-post-tr))
    (cfthumb-bl-place (translate (wall-locate3 -0.3 1) thumb-post-tr))
    (cfthumb-ml-place thumb-post-tl))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 web-post)
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (cfthumb-ml-place thumb-post-tl))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 web-post)
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
    (key-place 0 (- cornerrow innercol-offset) web-post-bl)
    (cfthumb-ml-place thumb-post-tl))
   (hull
    (cfthumb-bl-place thumb-post-tr)
    (cfthumb-bl-place (translate (wall-locate1 -0.3 1) thumb-post-tr))
    (cfthumb-bl-place (translate (wall-locate2 -0.3 1) thumb-post-tr))
    (cfthumb-bl-place (translate (wall-locate3 -0.3 1) thumb-post-tr))
    (cfthumb-ml-place thumb-post-tl))
   ; connectors below the inner column to the thumb & second column
   (if inner-column
     (union
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 0 (dec cornerrow) web-post-br)
       (key-place 0 cornerrow web-post-tr))
      (hull
       (key-place 0 cornerrow web-post-tr)
       (key-place 1 cornerrow web-post-tl)
       (key-place 1 cornerrow web-post-bl))
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 0 cornerrow web-post-tr)
       (key-place 1 cornerrow web-post-bl))
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 1 cornerrow web-post-bl)
       (cfthumb-ml-place thumb-post-tl))))))

(def inner-column-bottom-section
  (color [0.3 0.4 0.8 1] (union
                          (hull
                           (key-place 0 (dec cornerrow) web-post-bl)
                           (key-place 0 (dec cornerrow) web-post-br)
                           (key-place 0 cornerrow web-post-tr))

                          (hull
                           (key-place 0 (dec cornerrow) web-post-bl)
                           (key-place 0 cornerrow (translate (wall-locate1 0 0) web-post-tl))
                           (key-place 0 cornerrow web-post-tr))

                          (hull
                           (key-place 0 cornerrow web-post-tr)
                           (key-place 1 cornerrow web-post-tl)
                           (key-place 1 cornerrow web-post-bl))
                          (hull
                           (key-place 0 cornerrow (translate (wall-locate1 0 0) web-post-tl))
                           (key-place 0 cornerrow web-post-tr)
                           (key-place 0 cornerrow web-post-br))
     ;removed so it doesnt clash with the trackball
                            ; (if (or trackball-enabled joystick-enabled) nil (hull
                            ;                                                  (key-place 1 lastrow web-post-tl)
                            ;                                                  (key-place 1 cornerrow web-post-bl)
                            ;                                                  (minithumb-tl-place minithumb-post-tl)))

                          (triangle-hulls
                           (key-place 0  cornerrow (translate (wall-locate1 0 0) web-post-tl))
                           (key-place 0 cornerrow web-post-br)
                           (key-place 0 cornerrow (translate (wall-locate3 0 0) web-post-bl)))

                          (triangle-hulls
                           (key-place 0  cornerrow web-post-tr)
                           (key-place 1 cornerrow web-post-bl)
                           (key-place 0 cornerrow web-post-br)))))

;switching walls depending on thumb-style used
(def thumb-wall-type
  (case thumb-style
    "default" default-thumb-wall
    "cf" cf-thumb-wall
    "trackpad" default-thumb-wall
  )
)

(def usb-holder-ref (key-position 0 0 (map - (wall-locate2  0  -1) [0 (/ mount-height 2) 0])))

(def usb-holder-position (map + [17 19.3 0] [(first usb-holder-ref) (second usb-holder-ref) 2]))
(def usb-holder-cube   (cube 15 12 2))
(def usb-holder-space  (translate (map + usb-holder-position [0 (* -1 wall-thickness) 1]) usb-holder-cube))
(def usb-holder-holder (translate usb-holder-position (cube 19 12 4)))
(def usb-jack-height 4)
(def usb-jack-width 9.5)
(def usb-jack-cylinder 
  (->>
   (binding [*fn* 36](cylinder (/ usb-jack-height 2) (+ plate-thickness 1) :center true))
   (rdx 90)
   (translate [0 1 0]) 
   
   ))

(def usb-c-torus 
  (->>
   ;(binding [*fn* 36] (circle 1))
  ;(translate [(- (/ usb-jack-height 2) 1) 0 0 ])
   ;(extrude-rotate {:convexity 10})
   (fillet-circle (/ usb-jack-height 2))
   (rdx 90)
   (translate [0 plate-thickness 0])
   )
  )

(defn usb-jack-place [shape]  
  (->> shape
      
       (rdz far-index-splay) 
      (translate (rotate-around-z far-index-splay [-0.5 (first far-index-post-splay-translation) 0])) ;x and y axes get reversed by rotate-around-z 
   (translate (map + usb-holder-position [-12 6.5 3]) )
       
       
   ))
;(def usb-jack (usb-jack-place (cube 9.5 plate-thickness 3.1)))
(def usb-jack 
  (->>
   (union
     (hull
      (translate [(- (/ usb-jack-height 2) (/ usb-jack-width 2)) 0 0] usb-jack-cylinder)
      (translate [(- (/ usb-jack-width 2) (/ usb-jack-height 2) ) 0 0] usb-jack-cylinder)
      
      )
    (union
     (translate [(- (/ usb-jack-height 2) (/ usb-jack-width 2)) 0 0] usb-c-torus)
     (translate [(* (- (/ usb-jack-height 2) (/ usb-jack-width 2) ) 0.75) 0 0] usb-c-torus)
     (translate [(/ (- (/ usb-jack-height 2) (/ usb-jack-width 2) )2) 0 0] usb-c-torus)
     (translate [(/ (- (/ usb-jack-height 2) (/ usb-jack-width 2) ) 4) 0 0] usb-c-torus)
     usb-c-torus
      (translate [(/ (- (/ usb-jack-width 2) (/ usb-jack-height 2)) 4) 0 0] usb-c-torus)
     (translate [(/ (- (/ usb-jack-width 2) (/ usb-jack-height 2)) 2) 0 0] usb-c-torus)
     (translate [(* (- (/ usb-jack-width 2) (/ usb-jack-height 2)) 0.75) 0 0] usb-c-torus)
    (translate [(- (/ usb-jack-width 2) (/ usb-jack-height 2)) 0 0] usb-c-torus))
      
     )
   (translate [0 -2 0])
   (usb-jack-place )
   ))

(def pro-micro-position (map + (key-position 0 1 (wall-locate3 -1 0)) [-6 2 -15]))
(def pro-micro-space-size [4 10 12]) ; z has no wall;
(def pro-micro-wall-thickness 2)
(def pro-micro-holder-size [(+ pro-micro-wall-thickness (first pro-micro-space-size)) (+ pro-micro-wall-thickness (second pro-micro-space-size)) (last pro-micro-space-size)])
(def pro-micro-space
  (->> (cube (first pro-micro-space-size) (second pro-micro-space-size) (last pro-micro-space-size))
       (translate [(- (first pro-micro-position) (/ pro-micro-wall-thickness 2)) (- (second pro-micro-position) (/ pro-micro-wall-thickness 2)) (last pro-micro-position)])))
(def pro-micro-holder
  (difference
   (->> (cube (first pro-micro-holder-size) (second pro-micro-holder-size) (last pro-micro-holder-size))
        (translate [(first pro-micro-position) (second pro-micro-position) (last pro-micro-position)]))
   pro-micro-space))

(def trrs-holder-size [6.2 10 2]) ; trrs jack PJ-320A
(def trrs-holder-hole-size [6.2 10 6]) ; trrs jack PJ-320A
(def trrs-holder-position  (map + usb-holder-position [-13.6 0 0]))
(def trrs-holder-thickness 2)
(def trrs-holder-thickness-2x (* 2 trrs-holder-thickness))
(def trrs-holder
  (union
   (->> (cube (+ (first trrs-holder-size) trrs-holder-thickness-2x) (+ trrs-holder-thickness (second trrs-holder-size)) (+ (last trrs-holder-size) trrs-holder-thickness))
        (translate [(first trrs-holder-position) (second trrs-holder-position) (/ (+ (last trrs-holder-size) trrs-holder-thickness) 2)]))))
(def trrs-holder-hole
  (union

  ; circle trrs hole
   (->>
    (->> (binding [*fn* 30] (cylinder 2.55 20))) ; 5mm trrs jack
    (rotate (deg2rad  90) [1 0 0])
    (translate [(first trrs-holder-position) (+ (second trrs-holder-position) (/ (+ (second trrs-holder-size) trrs-holder-thickness) 2)) (+ 3 (/ (+ (last trrs-holder-size) trrs-holder-thickness) 2))])) ;1.5 padding

  ; rectangular trrs holder
   (->> (apply cube trrs-holder-hole-size) (translate [(first trrs-holder-position) (+ (/ trrs-holder-thickness -2) (second trrs-holder-position)) (+ (/ (last trrs-holder-hole-size) 2) trrs-holder-thickness)]))))

(defn screw-insert-shape [bottom-radius top-radius height]
  (union
   (->> (binding [*fn* 30]
          (cylinder [bottom-radius top-radius] height)))
   (translate [0 0 (/ height 2)] (->> (binding [*fn* 30] (sphere top-radius))))))

(defn screw-insert [column row bottom-radius top-radius height offset]
  (let [shift-right   (= column lastcol)
        shift-left    (= column 0)
        shift-up      (and (not (or shift-right shift-left)) (= row 0))
        shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
        position      (if shift-up     (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
                          (if shift-down  (key-position column row (map - (wall-locate2  0 -1) [0 (/ mount-height 2) 0]))
                              (if shift-left (map + (left-key-position row 0) (wall-locate3 -1 0))
                                  (key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0])))))]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate (map + offset [(first position) (second position) (/ height 2)])))))

(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union 
         (screw-insert lastcol 0         bottom-radius top-radius height [-152 9 0])
;(screw-insert 0 lastrow   bottom-radius top-radius height [-50 7 0])
   (screw-insert 0 0         bottom-radius top-radius height [9 6 0])
         (screw-insert 0 lastrow   bottom-radius top-radius height [-17 -7 0])
    (screw-insert 2 0  bottom-radius top-radius height [0 -1 0])
(screw-insert 1 lastrow         bottom-radius top-radius height [0 -6 0])
          (screw-insert lastcol lastrow  bottom-radius top-radius height [-5 13 0])
          (screw-insert lastcol 0         bottom-radius top-radius height [-3 6 0])
       
   
         ))

; Hole Depth Y: 4.4
(def screw-insert-height 4)

; Hole Diameter C: 4.1-4.4
(def screw-insert-bottom-radius (/ 4.4 2))
(def screw-insert-top-radius (/ 4.4 2))
(def screw-insert-holes  (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))

; Wall Thickness W:\t1.65
(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1.5)))
(def screw-insert-screw-holes  (screw-insert-all-shapes 1.7 1.7 350))

(def pinky-connectors
  (apply union
         (concat
          ;; Row connections
          (for [row (range 0 lastrow)]
            (triangle-hulls
             (key-place lastcol row web-post-tr)
             (key-place lastcol row wide-post-tr)
             (key-place lastcol row web-post-br)
             (key-place lastcol row wide-post-br)))

          ;; Column connections
          (for [row (range 0 cornerrow)]
            (triangle-hulls
             (key-place lastcol row web-post-br)
             (key-place lastcol row wide-post-br)
             (key-place lastcol (inc row) web-post-tr)
             (key-place lastcol (inc row) wide-post-tr)))
          ;;
          )))

(def pinky-walls
  (union
   (key-wall-brace lastcol cornerrow 0 -1 web-post-br lastcol cornerrow 0 -1 wide-post-br)
   (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 0 1 wide-post-tr)))