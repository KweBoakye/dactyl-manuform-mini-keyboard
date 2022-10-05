(ns dactyl-keyboard.low.dactyl-low
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.sa-keycaps :refer :all]
            [dactyl-keyboard.low.placement-functions-low :refer :all]
            [dactyl-keyboard.low.web-connecters-low :refer :all]
            [dactyl-keyboard.low.thumbs-low :refer :all]
            [dactyl-keyboard.low.case-low :refer :all]
            [dactyl-keyboard.low.case-low-functions :refer :all]
            [dactyl-keyboard.low.case-low-polyhedron-functions :refer :all]
            [dactyl-keyboard.low.case-low-polyhedron :refer :all]
            [dactyl-keyboard.oled :refer :all]
            [dactyl-keyboard.low.oled-low-placements :refer :all]
            [dactyl-keyboard.cirque-circle-trackpad :refer :all]
            [dactyl-keyboard.low.cirque-circle-trackpad-placement-functions-low :refer :all]
            [dactyl-keyboard.low.aviator-low :refer :all]
            [dactyl-keyboard.tps-65 :refer :all]
            [dactyl-keyboard.pico-standoffs :refer :all]
            [dactyl-keyboard.low.palm-rest-low :refer :all]
            [dactyl-keyboard.IS31FL3743A-mount :refer :all]
            [dactyl-keyboard.EVQWGD001 :refer :all]
            [dactyl-keyboard.vybronics-vl91022 :refer :all]
            [dactyl-keyboard.drv2605l-standoffs :refer :all]
            [dactyl-keyboard.RP2040-Plus :refer :all]
            [dactyl-keyboard.six-pin-ffc-adapter-board :refer :all]
            [dactyl-keyboard.metal-tactile-button :refer :all]
            [dactyl-keyboard.AST1109MLTRQ :refer :all]
            [dactyl-keyboard.dovetail :refer :all]
            [dactyl-keyboard.MxLEDBitPCB-holder :refer :all] 
            ))


(def MxLEDBitPCB-placed
  (apply union
         (for [column columns
               row rows
               :when (check-last-row-middle-and-fourth-keys-only column row)] 
                (key-place column row MxLEDBitPCB))))


(def pcb
  (->>
   (cube 17.8 17.8 3.05)
   (translate [0 0 (- 3.05)])))

(def pcb-cutout
  (->>
   (cube 18.5 18.5 3.05)
   (translate [0 0 (- 3.05)])))

(def pcb-place
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [2 3] column)
                         (not= row lastrow))]
           (key-place column row pcb))))

(def mxd (multmatrix-translate [4 4 4]))
;; (def model-right (difference
;;                   (union
;;                    key-holes
;;                    (-# pinky-connectors)
;;                    ;pinky-walls
;;                    connectors

;;                    ;(hull
;;                   ;  aviator-neck
;;                    ;aviator-neck-support-left
;;                    ;aviator-neck-support-right
;;                   ; (-# aviator-assembly)
;;                    ;)
;;                   ;(color [1 0 0 1] pcb-place))
;;                 ; (color [0 1 0 1] (thumb-1x-layout pcb))
;;                 ; (color [0 1 0 1] (thumb-15x-layout  pcb))
;;                    (EVQWGD001-place EVQWGD001-holder)
;;                  ; ( -#(thumb-b1-place-multmatrix (cube 5 5 5)))

;;                  ; (pico-standoffs-place pico-standoffs)
;;                  ;  (IS31FL3743A-standoff-place IS31FL3743A-standoffs)
;;                  ;   (color [1 0 0 1] aviator-male-connecter-clearence-test)
;;                  ;  (translate [0 -10 0] (color [0 1 0 1] aviator-female-connecter-clearence-test))
;;                 ;(color [1 0 0 1](translate [-8 4 8] (left-wall-plate-place 0 (+ left-wall-y-modifier 0.3) (rotate (deg2rad -15) [1 0 0] ST7789-240x320))))
;;                 ;(color [0 1 0 1] (translate [-8 4 8] (left-wall-plate-place 0 (+ left-wall-y-modifier 0.3) (rotate (deg2rad -15) [1 0 0] ST7789-240x320-display))))
;;                     ;(translate [4 0 8] (left-wall-plate-place 0 (+ left-wall-y-modifier 0.3) (rotate (deg2rad -15) [1 0 0] (rotate (deg2rad 90) [0 0 1] ST7789-240x240-154))))
;;                     ;(color [0 0 1 1] (translate [4 0 8] (left-wall-plate-place 0 (+ left-wall-y-modifier 0.3) (rotate (deg2rad -15) [1 0 0] (rotate (deg2rad 90) [0 0 1]  ST7789-240x240-154-display)))))

;;                   ; (color [1 1 0 1](translate [-8 0 8] (left-wall-plate-place 0 (+ left-wall-y-modifier 0.3) (rotate (deg2rad -15) [1 0 0] (rotate (deg2rad 90) [0 0 1] ST7789-240x240-13)))))
;; ;(color [1 0 1 1] (translate [-8 0 8] (left-wall-plate-place 0 (+ left-wall-y-modifier 0.3) (rotate (deg2rad -15) [1 0 0] (rotate (deg2rad 90) [0 0 1]  ST7789-240x240-13-display)))))
;;                    ;(color [1 0 0 1] (tps-65-place tps-65))
;;                    thumb-wall-type
;;                    thumb-type
;;                    (difference
;;                     thumb-connector-type
;;                     (thumb-tr-place pcb-cutout)
;;                     (thumb-tr-place (translate [0 0 -2] pcb-cutout)))
;;                     ;left-section
;;                     ;;(cirque-TM040040-place cirque-TM040040-mount)
;;                    ;;(cirque-TM040040-thumb-place (translate [0 0 3](cylinder 20.57 12 :center false)))
;;                    ;(cirque-TM040040-thumb-place cirque-TM040040-mount)
;;                    (difference (union (difference
;;                                        case-walls
;;                                        (usb-jack-place usb-jack))
;;                                       (tps-65-place tps-65-mount)
;;                                       screw-insert-outers
;;                                      ;(rp2040-plus-place rp2040-plus)
;;                                       (color [1 0 0 1] (rp2040-plus-place rp2040-plus-mount))
;;                                       (tps-65-translate-and-place-at-position [10 0 (- vybronics-vl91022-z-axis)] (rdz -90 vybronics-vl91022-mount))
;;                                       ;pro-micro-holder
;;                                       ;usb-holder-holder
;;                                       ;trrs-holder
;;                                       )
;;                                (tps-65-place tps-65-mount-cutout)
;;                                (tps-65-place (translate [0 0 (+ 0 1)] tps-65-mount-main-cutout))
;;                                (cond
;;                                  (= screen-holder-mount-position "screen-holder-mount-top") (screen-holder-place screen-holder-cut)
;;                                  (= screen-holder-mount-position "screen-holder-mount-side") (screen-holder-place-side screen-holder-cut))

;;                                ;usb-holder-space

;;                                aviator-hole
;;                                aviator-recess-hole
;;                                ;trrs-holder-hole
;;                                screw-insert-holes))
;;                   (translate palm-hole-origin (palm-rest-hole-rotate palm-buckle-holes))
;;                   (translate [0 0 -20] (cube 350 350 40))))

(def model-polyhedron
  (let [steps 36
      steps-low 8
      steps-mid 16]
    (union

     (polyhedron-thumb-walls steps)
     thumb-type
     ;thumb-connector-type
     key-holes
     (thumb-connecters-polyhedron steps-low)
       (key-web-connecters-polyhedron steps-low)
     (EVQWGD001-place EVQWGD001-holder)
     
      (union
       (difference 
        (union
         (polyhedron-left-section steps)
        aviator-assembly-polyhedron)
        aviator-assembly-diffs
        )
       (right-side-polyhedron steps)
       (difference
        (polyhedron-case-walls steps)
        (usb-jack-place usb-jack-polyhedron))
       (difference 
        (tps-65-place tps-65-mount)
        (tps-65-place tps-65-mount-cutout)
    (tps-65-place (translate [0 0 (+ 0 1)] tps-65-mount-main-cutout))
        (tps-65-translate-and-place-with-radius (mapv + tps-65-mount-corner-cylinder-bottom-left-position [0 0 (/ (- tps-65-depth tps-65-depth-tolerance 0.25) 1)])
                                                (- 0.5 tps-65-mount-corner-radius) (- 0.5 tps-65-mount-corner-radius)
                                                (rdz 120 (binding [*fn* 3] (cylinder 1 (+ tps-65-depth tps-65-depth-tolerance) :center false))))
        )
       ;(color [1 0 0 1] (rp2040-plus-place rp2040-plus-mount))
      (difference screw-insert-outers
             screw-insert-holes
             )
       (vybronics-vl91022-place vybronics-vl91022-mount)
       (difference
        (cond
         (= screen-holder-mount-position "screen-holder-mount-top") (screen-holder-place screen-holder)
         (= screen-holder-mount-position "screen-holder-mount-side") (screen-holder-place-side screen-holder))
        (cond
          (= screen-holder-mount-position "screen-holder-mount-top") (screen-holder-place screen-holder-cut)
          (= screen-holder-mount-position "screen-holder-mount-side") (screen-holder-place-side screen-holder-cut))
        ) 
      ))))

 (spit "things-low/model-polyhedron.scad"
       (write-scad model-polyhedron))

(def gx16 (import "GX16-4P.STL"))

 ;(spit"things-low/multmatrix-test.scad"
 ;(write-scad (thumb-b1-place-multmatrix (cube 5 5 5))))
;; (spit "things-low/right.scad"
;;       (write-scad
;;        tps-65-includes
;;        aviator-includes
;;        ;(include "../BOSL/shapes.scad")
;; ;(include "../BOSL/constants.scad")
;;        model-right))

;; (spit "things-low/left.scad"
;;       (write-scad
;;        tps-65-includes
;;        aviator-includes
;;        ;(include "../BOSL/shapes.scad")
;; ;(include "../BOSL/constants.scad")
;;        (mirror [-1 0 0] model-right)))

;; (spit "things-low/right-test.scad"
;;       (write-scad
;;        tps-65-includes
;;        aviator-includes
;;       ; (include "../BOSL/shapes.scad")
;; ;(include "../BOSL/constants.scad")
;;        (difference
;;         (union
;;          model-right
;;          ;thumbcaps-type
;;          dsa-thumbcaps
;;          ;caps
;;          (EVQWGD001-place EVQWGD001)
;;          dsa-caps
;;          (aviator-place-shape 
;;           (map +  [15 aviator-plug-connecter-length 9.5]  aviator-position  aviator-offset) 
;;           ;(translate (map +  aviator-offset [14 aviator-male-connecter-length 9.5])
;;                                                  (rdy -90 gx16)
;;           ;                                       )
;;           )
;;          )

;;         (translate [0 0 -20] (cube 350 350 40)))))


;; (def bottom-plate
;;   (extrude-linear
;;    {:height 2.6 :center false :convexity 10}
;;    (project
;;     (difference
;;      (union
;;       case-walls
;;       screw-insert-outers
;;       thumb-wall-type
;;       key-holes
;;       (tps-65-place tps-65-base)
;;       pinky-connectors
;;       extra-connectors
;;       connectors
;;       inner-connectors
;;       thumb-type
;;       thumb-connector-type
;;       thumbcaps-fill-type
;;       (EVQWGD001-place    (->> (cube  EVQWGD001-mount-width EVQWGD001-mount-length EVQWGD001-mount-height)
;;                                (translate [0 0 (/ EVQWGD001-mount-height 2)])))
;;       caps-fill)
;;      (translate [0 0 -10] screw-insert-screw-holes)))))

(def bottom-plate-for-polyhedron-model
  (let [steps 36
    steps-low 8
    steps-mid 16 
    back-wall-polyhedron-points    (back-wall-polyhedron steps :bottom-plate true)
    left-section-back-points    (left-section-back steps :bottom-plate true)
    back-left-wall-to-screen-points (back-left-wall-to-screen steps :bottom-plate true)
        screen-holder-bottom-left-outside-floor-point-and-screen-holder-bottom-right-outside-floor-point [ screen-holder-bottom-left-outside-floor-point screen-holder-bottom-right-outside-floor-point]
        left-section-front-polyhedron-bottom-points (left-section-front-polyhedron-bottom steps)
        polyhedron-thumb-walls-points (polyhedron-thumb-walls steps :bottom-plate true) 
        thumb-connecters-polyhedron-points (thumb-connecters-polyhedron steps :bottom-plate true)
        front-wall-polyhedron-points (front-wall-polyhedron steps :bottom-plate true)
        right-wall-polyhedron-points (right-wall-polyhedron steps :bottom-plate true)
        bottom-plate-points (concat 
                             back-wall-polyhedron-points
                             left-section-back-points
                             back-left-wall-to-screen-points 
                                    screen-holder-bottom-left-outside-floor-point-and-screen-holder-bottom-right-outside-floor-point
                                    left-section-front-polyhedron-bottom-points
                             thumb-connecters-polyhedron-points       
                             polyhedron-thumb-walls-points
                                    front-wall-polyhedron-points
                                    right-wall-polyhedron-points)
        ]
    ;(extrude-linear
   ;{:height 2.6 :center false :convexity 10}
   
   ; (difference 
     (union
      ;(plot-bezier-points right-wall-polyhedron-points (sphere 0.1))
      (polygon (map drop-last bottom-plate-points))
      ;(polyhedron-thumb-walls steps)
      ;thumbcaps-fill-type
      ;thumb-type
      ;(thumb-connecters-polyhedron steps-low)
      ;key-holes
      ;thumb-type
      ;thumbcaps-fill-type
      ;caps-fill
      ;(thumb-connecters-polyhedron steps-low)
      ;(key-web-connecters-polyhedron steps-low)
      ;(EVQWGD001-place    (->> (cube  EVQWGD001-mount-width EVQWGD001-mount-length EVQWGD001-mount-height)
       ;                        (translate [0 0 (/ EVQWGD001-mount-height 2)])))
      ;(polyhedron-left-section steps)
      ;(right-side-polyhedron steps)
      ;(polyhedron-case-walls steps)
      ;(tps-65-place tps-65-base)
      ;screw-insert-outers 
      ;(cond
       ; (= screen-holder-mount-position "screen-holder-mount-top") (screen-holder-place screen-holder)
        ;(= screen-holder-mount-position "screen-holder-mount-side") (screen-holder-place-side screen-holder))
      
     ; )
     ;(translate [0 0 -10] screw-insert-screw-holes)
     ;)
     ;)
     ;)
     )
     ))

(spit "things-low/bottom-plate-for-polyhedron-model.scad"
      (write-scad bottom-plate-for-polyhedron-model)
      )

;; (def bottom-plate-old
;;   (cut
;;    (translate [0 0 -0.1]
;;               (difference (union case-walls
;;                                  pinky-walls
;;                                  screw-insert-outers
;;                                  thumb-wall-type)
;;                           (translate [0 0 -10] screw-insert-screw-holes)))))

(def screen-test
  (union
   (-# (difference
        (union
         screen-holder
    ;(-# (rdz 90 view-bezel))
   ; (rotate (deg2rad 0) [1 0 0] (rotate (deg2rad 90) [0 0 1] ST7789-240x240-154-holder-old))
         )
        screen-holder-cut))
   ST7789-240x240))

;; (spit "things-low/right-plate.scad"
;;       (write-scad
;;        bottom-plate))

;; (spit "things-low/test.scad"
;;       (write-scad
;;        (difference trrs-holder trrs-holder-hole)))

;; (spit "things-low/tps-65-overlay.scad"
;;       (write-scad tps-65-overlay))

;; (spit "things-low/tps-65-mount-cutout.scad"
;;       (write-scad tps-65-mount-cutout))

;; (spit "things-low/tps-65-mount-test.scad"
;;       (write-scad tps-65-mount-test))

;; (spit "things-low/tps-65-mount-test-2.scad" 
;;       (write-scad 
;;        tps-65-includes

;;        tps-65-mount))



;; (spit "things-low/pico-standoffs-test.scad"
;;       (write-scad pico-standoff-test))

;;  (spit "things-low/IS31FL3743A-standoff-test.scad"
;;        (write-scad IS31FL3743A-standoff-test))

(spit "things-low/screen-test.scad"
      (write-scad screen-test))

(spit "things-low/EVQWGD001-test.scad"
      (write-scad EVQWGD001-holder))

(spit "things-low/vybronics-vl91022-mount.scad"
      (write-scad (union
                   (difference
                    (tps-65-place tps-65-mount)
                    (tps-65-place tps-65-mount-cutout)
                    (tps-65-place (translate [0 0 (+ 0 1)] tps-65-mount-main-cutout))
                    (tps-65-translate-and-place-with-radius (mapv + tps-65-mount-corner-cylinder-bottom-left-position [0 0 (/ (- tps-65-depth tps-65-depth-tolerance 0.5) 1)])
                                                            (- 0.5 tps-65-mount-corner-radius) (- 0.5 tps-65-mount-corner-radius)
                                                            (rdz 120 (binding [*fn* 3] (cylinder 1 (+ tps-65-depth tps-65-depth-tolerance) :center false))))
                    )
                   (tps-65-translate-and-place-at-position [10 0 (- (+ tps-65-depth tps-65-depth-tolerance))] (rdy 180 (rdz -90 vybronics-vl91022-mount)))
                   
                   )))

;; (spit "things-low/drv2605l.scad"
;;       (write-scad drv2605l))

;;  (spit "things-low/drv2605l-standoffs-test.scad"
;;        (write-scad drv2605l-standoffs-test)) 

;;  (spit "things-low/drv2605l-standoffs-print-test.scad"
;;        (write-scad drv2605l-standoffs-print-test)
;;        )


;;  (spit "things-low/rp2040-plus-mount-test.scad"
;;        (write-scad 
;;         (difference
;;          (union


;;            (rp2040-plus-place rp2040-plus-mount)
;;           (-# (rp2040-plus-place rp2040-plus))
;;            (difference
;;             (union (for [x (range 0 1)] (key-wall-brace x 0 0 1 oled-post-tl x       0 0 1 oled-post-tr))
;;                    (for [x (range 1 1)] (key-wall-brace x 0 0 1 oled-post-tl (dec x) 0 0 1 oled-post-tr)))
;;             (-# usb-jack))
;;           )

;;          ;(translate [ -40 20 (+ rp2040-plus-mount-height 20)] (cube 30 30 40))
;;          (translate [0 0 -20] (cube 350 350 40))
;;          )))

;;  (spit "things-low/rp2040-plus.scad"
;;        (write-scad rp2040-plus))



(spit "things-low/aviator-assembly.scad"
      (write-scad 

(difference
  ;(-# aviator-assembly) 
 (union
 aviator-assembly-polyhedron
  (left-section-back 36))
   
 aviator-assembly-diffs

 )
 
;(aviator-place-shape (translate  [9.5 -10.75 (- aviator-plug-connecter-length) ] (rdy -90 gx16)))
 
       ))
;; (spit "things-low/back-wall-test.scad"
;;       (write-scad back-wall)
;;       )

;;   (spit "things-low/left-section-test.scad"
;;         (write-scad 
;;          (difference 
;;           (union
;;           ;(EVQWGD001-place EVQWGD001-test)
;;           ;thumb-side-EVQWGD001-mount
;;           left-wall
;;            ;left-section
;;            ;aviator-assembly
;;           ;(rp2040-plus-place rp2040-plus-mount)
;;           back-wall
;;           ; thumb-type
;;            ;thumb-connector-type
;;            ;thumb-wall-type
;;           )
;;           ; aviator-assembly-diffs
;;          (translate [0 0 -20] (cube 350 350 40))
;;           )
;;          )
;;         )

;; (spit "things-low/six-pin-ffc-adapter-test.scad"
;;       (write-scad six-pin-ffc-adapter-test)
;;       )

;; (spit "things-low/six-pin-ffc-adapter-print-test.scad"
;;       (write-scad 


;;         (union 
;;         (translate [0 0 six-pin-ffc-adapter-board-thickness ] six-pin-ffc-adapter-standoffs)
;;        (translate [0 -5 (/ six-pin-ffc-adapter-board-thickness 2)](cube (+ six-pin-ffc-adapter-board-width 10) 20 six-pin-ffc-adapter-board-thickness)
;;         )))
;;       )

;; (spit "things-low/plate-arrangement-test.scad"
;;       (write-scad
;;        (union
;;        ; right-wall
;;         ;back-wall
;;         left-wall
;;        ; front-wall
;;         ;thumb-wall-type
;;         (IS31FL3743A-standoff-place IS31FL3743A-standoff-test)
;;         (drv2605l-place drv2605l-standoffs-test)
;;         (six-pin-ffc-adapter-place six-pin-ffc-adapter-test)
;;         (rp2040-plus-place rp2040-plus-mount )
;;         )
;;        ))


;; (spit "things-low/IS31FL3743A-standoff-print-test.scad"
;; (write-scad IS31FL3743A-standoff-print-test))

;; (spit "things-low/AST1109MLTRQ-holder-test.scad"
;;       (write-scad AST1109MLTRQ-holder)
;;       )

;; (spit "things-low/dovetail-test.scad"
;;       (write-scad dovetail-test))

;; (spit "things-low/splay-test.scad"
;;       (write-scad 
;;        (union
;;         key-holes
;;         thumb-type
;;         ;; (-# (union
;;         ;;      (for [column [0 1 3 4]
;;         ;;            row (range 0 (cond (= column 3) nrows :else (- nrows 1)))
;;         ;;            ]

;;         ;;        (key-place-test column row single-plate)
;;         ;;        )))
;;         ))
;;       )

;; (spit "things-low/screen-position-test.scad"
;;       (write-scad 
;;        (difference
;;         (union
;;         between-screen-and-trackpad
;;         thumb-side
;;          thumb-side-to-trackpad-mount
;;         top-side
;;         (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 1 0 oled-post (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 oled-post wall-xy-offset wall-xy-offset)
;;         (screen-holder-place-side screen-holder)
;;          (hull (bottom-hull (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2) 0 oled-post))  (wall-brace-xy-half-top (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left) 0 -1 oled-post wall-xy-offset))
;;          (hull (bottom-hull (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2) 0 oled-post))  (wall-brace-xy-half-bottom (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left) 0 -1 oled-post wall-xy-offset))
;;          )
;;         (translate [0 0 -20] (cube 350 350 40))
;;         )))

;; (spit "things-low/tps-65-position-test.scad"
;;       (write-scad
;;        (union
;;         right-side
;;         dsa-thumbcaps 

;;         (tps-65-place tps-65-mount)

;;         (for [column [0 1]
;;               row (range 0 (cond (= column 3) nrows :else (- nrows 1)))]

;;           (union
;;            (key-place column row single-plate)
;;           (key-place column row dsa-cap))
;;           )
;;         thumb-type)
;;        )

;;       )

;; (spit "things-low/side-test.scad"
;;       (write-scad
;;        (union
;;         right-wall
;;         )
;;        )
;;       )



;(defn )



(spit "things-low/left-curve-test.scad"

      (write-scad
       (difference (union
        ;(screen-holder-place-side screen-holder)
       ;(curved-corner-xy 1 0 1 1 0 1 (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) oled-post wall-xy-offset)
       ;(-# (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset)) 1 1 oled-post (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 1 0 oled-post wall-xy-offset wall-xy-offset))
       ;;  (difference 
       ;;   (tps-65-place tps-65-base)
       ;;   (tps-65-place tps-65-mount-cutout)
       ;;   )
                    (back-left-wall-to-screen 36)
                    (EVQWGD001-place EVQWGD001-holder)
       ; (EVQWGD001-place EVQWGD001)
                    thumb-type
                    (left-section-back 36)
                    under-screen
       ; dsa-thumbcaps
        ;screen-to-EVQWGD001
       ;;  (difference (screen-holder-place-side screen-holder)
       ;;             (screen-holder-place-side screen-holder-cut) )

       ;(right-side-polyhedron 36) 
                    (left-section-front-polyhedron 36)
        ;(left-section-back 36)
        ;; thumb-walls-polyhedron
        ;;  thumb-corners-polyhedron
        ;;  thumb-tweeners-polyhedron
                    (union
                     (for [column [0]
                           row (range 0 (cond (= column 3) nrows :else (- nrows 1)))]        (key-place column row single-plate)))
        ;(-# (thumb-wall-brace thumb-bl-place -1  0 oled-post-tl thumb-bl-place -1  0 oled-post-bl thumb-bl-rotate thumb-bl-rotate))
                    ;under-screen
                    )
        ;(translate [0 0 -20] (cube 350 350 40))
                   )))

;; (spit "things-low/between-trackpad-and-keys.scad"
;;       (write-scad 
;;        (union
;;         (right-side-polyhedron 10)
;;         thumb-type
;;         connectors
;;         (-# thumb-connector-type)
;;         thumb-connectors-polyhedron
;;         ;(-# right-side)
;;         key-holes

;;         (wall-brace-quadratic-polyhedron (partial key-place 4 2) 0 -1 "br" :radians (partial key-place 4 2) 1 -1 "br" :radians (partial key-place 4 2) 1 0 "br" :radians 20)
;;         (wall-brace-quadratic-polyhedron (partial key-place lastcol 0) 1 0 "tr" :radians (partial key-place lastcol 0) 1 1 "tr" :radians (partial key-place lastcol 0) 0 1 "tr" :radians 20)
;;         (-#      (tps-65-place tps-65-base))
;;         back-wall-polyhedron
;;         right-wall-polyhedron
;;         thumb-walls-polyhedron
;;         thumb-corners-polyhedron
;;         thumb-tweeners-polyhedron
;;         front-wall-polyhedron
;; ;(tps-65-place tps-65-mount-cutout)
;;         ))

;;       )




;; (spit "things-low/thumb-wall-test.scad"
;;       (write-scad
;;        (union
;;         thumb-type
;;         key-holes
;;         (right-side-polyhedron 36)
;;         thumb-walls-polyhedron
;;         thumb-corners-polyhedron
;;         thumb-tweeners-polyhedron
;;         (thumb-connecters-polyhedron 36)
;;         front-wall-polyhedron)))

(spit "things-low/render-test.scad"
      (write-scad
       (union
       ;thumb-walls-polyhedron
       ;front-wall-polyhedron
       ;thumb-corners-polyhedron
       key-holes
       switches
       dsa-caps
       MxLEDBitPCB-placed
      ;thumb-type
      ;;  thumb-tweeners-polyhedron
      ;;   thumb-walls-polyhedron
      ;;   thumb-corners-polyhedron
       ; (thumb-connecters-polyhedron 12)
       (key-web-connecters-polyhedron 8)
        )
       ))

;; (spit "things-low/single-plate-test.scad"
;;       (write-scad (union
;;                    single-plate
;;                    web-post-tl
;;                    web-post-tr 
;;                    web-post-bl
;;                    web-post-br
;;                    (translate (get-single-plate-corner-position-vector "tr") web-post)
;;                    (translate (get-single-plate-corner-position-vector "tl") web-post)
;;                    (translate (get-single-plate-corner-position-vector "bl") web-post)
;;                    (translate (get-single-plate-corner-position-vector "br") web-post)
;;                    ))
;;       )



;; (spit "things-low/curved-corner-test.scad"
;;       (write-scad 
;;        (curved-corner 0 -1 1 -1 1 0 (partial key-place 4 cornerrow) oled-post-br)
;;        )
;;       )

;; (spit "things-low/right-wall-test.scad"
;;       (write-scad
;;        right-wall-polyhedron
;;        ))

;; (spit "things-low/switch-test.scad"
;;       (write-scad
;;        (union
;;         (-# single-plate)
;;         switch-model
;;         (-# dsa-cap)
;;         MxLEDBitPCB
;;         MxLEDBitPCB-holder-leg-1
;;         MxLEDBitPCB-holder-leg-2
;;         )))

  (spit "things-low/back-wall-polyhedron.scad"
        (write-scad (union 
                     (difference 
                     (back-wall-polyhedron 36)
                     (usb-jack-place usb-jack-polyhedron))
                     (rp2040-plus-place rp2040-plus-mount)
                     (-# (left-section-back 36))
                     ))
        )
  
  (spit "things-low/usb-jack-test.scad"
        (write-scad (union 
                     (usb-jack-place (-# usb-jack))
                     (usb-jack-place usb-jack-polyhedron)
                     )))
  
  

;; (spit "things-low/front-and-thumb-wall-test.scad"
;;       (write-scad
;;        (union
;;         front-wall-polyhedron
;;         (polyhedron-thumb-walls 36)
;;         key-holes
;;         thumb-type
;;         ) 
;;        )
;;       )

(defn -main [dum] 1)  ; dummy to make it easier to batch