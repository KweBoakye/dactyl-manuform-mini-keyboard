(ns dactyl-keyboard.low.dactyl-low
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [ mmul shape to-vector coerce]]
          ;   [chisel.curves :as chisel-curves :refer [direct-nurbs-evaluation  b-spline clamped-b-spline direct-nurbs-evaluation]]
           ; [chisel.protocols :as chisel-protocols :refer [PParametricCurve]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
             
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
            [dactyl-keyboard.des-caps :refer :all]
            [dactyl-keyboard.cornelius-thumbs-with-sprues :refer :all]
            ))

;(set-current-implementation :vectorz)


(def MxLEDBitPCB-placed
  (apply union
         (for [column columns
               row rows
               :when (check-last-row-middle-and-fourth-keys-only column row)] 
                (key-place column row MxLEDBitPCB))))

(def MxLEDBitPCB-holder-legs-placed-on-keywells
  (union
   (apply union
         (for [column columns
               row rows
               :when (and 
                      (check-last-row-middle-and-fourth-keys-only column row)
                      (or (not= column 0) (not= row 2)))]
           (key-place column row MxLEDBitPCB-holder-legs)))
   (key-place 0 2 
              (union
               MxLEDBitPCB-holder-leg-1
               MxLEDBitPCB-holder-leg-3 
               ))
   ))

(def  MxLEDBitPCB-placed-on-thumbs
  (union
  ; (thumb-tr-place MxLEDBitPCB-holder-legs)
   (thumb-tl-place MxLEDBitPCB-holder-legs)
   (thumb-mr-place MxLEDBitPCB-holder-legs)
   (thumb-bl-place MxLEDBitPCB-holder-legs)
   (thumb-br-place MxLEDBitPCB-holder-legs)))


(def kailh-hotswap-mx 
  (translate [0.75 -4.75 (- plate-thickness)] (import "../parts/Kailh Hotswap MX v22.stl")))

(def kailh-hotswap-mx-thumbs 
  (union
   (thumb-1x-layout kailh-hotswap-mx)
   (thumb-15x-layout kailh-hotswap-mx)
   )
  )


(def pcb
  (->>
   (cube MxLEDBitPCB-holder-width MxLEDBitPCB-holder-length MxLEDBitPCB-holder-thickness)
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
           (key-place column row MxLEDBitPCB))))

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


(def index-bottom-row-MxLEDBitPCB-holders
 (key-place 0 2
            (union
             single-plate

             (intersection
              MxLEDBitPCB-holder-leg-2
              MxLEDBitPCB-clearance)
             MxLEDBitPCB-holder-leg-1
             MxLEDBitPCB-holder-leg-3))
 )

(def thumb-tr-MxLEDBitPCB-holders
 (thumb-tr-place (union
                  (intersection
                   (union
                    MxLEDBitPCB-holder-leg-1
                    MxLEDBitPCB-holder-leg-3)
                   MxLEDBitPCB-clearance)
                  MxLEDBitPCB-holder-leg-2))
 )

(def abe-dua "../svg/Abe_dua.svg")
(def hw3-me-dua "../svg/HWEMUDUA.svg")
(def nea-onnim-no-sua-a-ohu "../svg/Nea_onnim_no_sua_a_ohu.svg")
(def nkyinkyim "../svg/nkyinkyim.svg")
(def odenkyem "../svg/Odenkyem.svg")
(def okuafo-pa "../svg/Okuafo pa.svg")

(def FUNTUNFUNEFU-DENKYEMFUNEFU-on-left-section-front
  (let [corner-position (calculate-point-between-points   (assoc screen-holder-top-right-outside-point 2 (/ (nth screen-holder-top-right-outside-point 2) 2))
                                                          (assoc (transform-position thumb-bl-place web-post-tl-translation-vector) 2 (/ (nth (transform-position thumb-bl-place web-post-tl-translation-vector) 2) 2)) [0 0 0])]
    (place-symbol "../FUNTUNFUNEFU-DENKYEMFUNEFU.svg" {:z-rotation -20 :height 3 :center true :scale-x 0.15 :scale-y 0.14 :rotation #(rdx 80 %)
                                                     :place corner-position :orientation-angle -45
                                                     :translation (mapv +  (rotate-around-z-in-degrees -20 [-2 -2 0]))})))
(def Ananse-Ntontan-on-thumb-bl
  (place-symbol-on-thumb-wall "../Ananse-Ntontan.svg"
                              {:height 3 :place thumb-bl-place
                               :offset (map + [(+ (/ mount-width -2) extra-width) (* extra-height 2) 0] (rotate-around-z-in-degrees -11 [0 3 0]))
                               :scale-x 0.125 :scale-y 0.125 :position "lm" :z-rotation -11 :orientation-angle 0 :rotation #(rdy 5 %)}))

(def oodenkyem-on-thumb-br 
  (place-symbol-on-thumb-wall odenkyem
                                 {:place thumb-br-place :height 2 :orientation-angle -85
                                  :offset (map + [(+ (/ mount-width -2) extra-width) (* extra-height 2) 0] [1 4 0])
                                  :scale-x 0.07 :scale-y 0.07 :position "lm" :z-rotation 0 :rotation #(rdy 10 %)}))

(def model-polyhedron
  (let [steps 36
      steps-low 8
      steps-mid 16]
    (union

     (polyhedron-thumb-walls-for-convex-cluster steps);renders
     thumb-type
     ;thumb-connector-type
     key-holes
     MxLEDBitPCB-holder-legs-placed-on-keywells
     index-bottom-row-MxLEDBitPCB-holders
     thumb-tr-MxLEDBitPCB-holders
     MxLEDBitPCB-placed-on-thumbs
     (difference
      (thumb-to-body-connecters-polyhedron steps)
      (thumb-tr-place MxLEDBitPCB-clearance)
(key-place 0 2 MxLEDBitPCB-clearance))
     FUNTUNFUNEFU-DENKYEMFUNEFU-on-left-section-front
     oodenkyem-on-thumb-br
     Ananse-Ntontan-on-thumb-bl
     (left-section-to-thumb-cluster-convex-walls steps)
     (difference
      (left-section-to-thumb-cluster-convex-connecters steps)
      (key-place 0 2 MxLEDBitPCB-clearance))
     
     (thumb-connecters-polyhedron 36) ;renders
       (key-web-connecters-polyhedron steps-low)
     ;(EVQWGD001-place EVQWGD001-holder)
     (front-wall-connecters-polyhedron steps);renders
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
        (usb-jack-place usb-jack-polyhedron)
         (key-place 2 2 MxLEDBitPCB-clearance))
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
 
 (spit "things-low/model-polyhedron-test.scad"
       (write-scad (color D_BLA (union 
                    model-polyhedron
                    ;(EVQWGD001-place EVQWGD001)
                    (aviator-place-shape (translate  [9.5 -10.75 (- aviator-plug-connecter-length)] (rdy -90 gx16)))
                    switches
                   ; pcb-place
                    ;dsa-caps
                    ;dsa-thumbcaps
                    (des-caps {:style :des-scooped})
                                 des-cornelius-thumbs
                                 ))))

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
    back-wall-polyhedron-points    (back-wall-polyhedron-catmull-rom steps :bottom-plate true)
    left-section-back-points    (left-section-back steps :bottom-plate true)
    back-left-wall-to-screen-points (back-left-wall-to-screen steps :bottom-plate true)
        screen-holder-bottom-left-outside-floor-point-and-screen-holder-bottom-right-outside-floor-point [ screen-holder-bottom-left-outside-floor-point screen-holder-bottom-right-outside-floor-point]
        left-section-front-polyhedron-bottom-points (reverse (left-section-to-thumb-cluster-convex-walls steps :bottom-plate true))
        polyhedron-thumb-walls-points (polyhedron-thumb-walls-for-convex-cluster steps :bottom-plate true) 
        thumb-connecters-polyhedron-points  (thumb-to-body-connecters-polyhedron steps :bottom-plate true)
        front-wall-polyhedron-points (front-wall-polyhedron steps :bottom-plate true)
        right-wall-polyhedron-points (right-wall-polyhedron-catmull-rom-spline steps :bottom-plate true)
        bottom-plate-points (concat 
                             back-wall-polyhedron-points 
                             left-section-back-points
                             back-left-wall-to-screen-points 
                                  screen-holder-bottom-left-outside-floor-point-and-screen-holder-bottom-right-outside-floor-point
                                    left-section-front-polyhedron-bottom-points 
                             polyhedron-thumb-walls-points
                             thumb-connecters-polyhedron-points
                                    front-wall-polyhedron-points
                                    right-wall-polyhedron-points
                             ) 
;;     max-x (apply max (map #(nth % 0) bottom-plate-points))
;;     min-x (apply min (map #(nth % 0) bottom-plate-points))
;;     x-distance (- max-x min-x)
;;     max-y (apply max (map #(nth % 1) bottom-plate-points))
;;         min-y (apply min (map #(nth % 1) bottom-plate-points))
;;         y-distance (- max-y min-y)
;;     max-z (apply max (map #(nth % 2) bottom-plate-points))
;; min-z (apply min (map #(nth % 2) bottom-plate-points))
        ;x-distance (- max-x min-z)
        ] 
;;    (println "max-x " max-x)
;;    (println "min-x " min-x)
;;    (println "x-distance " x-distance)
;;    (println "max-y " max-y)
;; (println "min-y " min-y)
;;    (println "y-distance " y-distance)
;;    (println "max-z " max-z)
;; (println "min-z " min-z)
   (union 
    (translate [0 0 -1.5](extrude-linear
   {:height 1.5 :center false :convexity 10} 
      (polygon (map drop-last bottom-plate-points)) 
     )) 
  ;  (println polyhedron-thumb-walls-points)
) 
   
  )
)
(def header  (translate [0 -12 (+ usb-jack-height (* rp2040-plus-thickness 2) 3)](rdz 90 (import "../parts/ImageToStl.com_pin header 1x16 th pitch 2.54mm.stl"))))
(spit "things-low/bottom-plate-for-polyhedron-model.scad"
      (write-scad (union
                   (difference 
                    bottom-plate-for-polyhedron-model
                    (translate [0 0 -10] screw-insert-screw-holes))
                   (IS31FL3743A-standoff-place IS31FL3743A-fillet-standoffs)
                   (drv2605l-place drv2605l-standoffs)
                  (six-pin-ffc-adapter-place six-pin-ffc-adapter-standoffs)
                 (rp2040-plus-place rp2040-plus-mount) 
                   ))
      )


(spit "things-low/bottom-plate-for-polyhedron-model-placement-test.scad"
 (write-scad
  (let [tactile-switch-place-holder (import "../parts/tactileswitch-short.STL")]
    (union
     bottom-plate-for-polyhedron-model
 pcb-place
(IS31FL3743A-standoff-place IS31FL3743A-standoff-test)
(drv2605l-place drv2605l-standoffs-test)
(six-pin-ffc-adapter-place six-pin-ffc-adapter-test)
(rp2040-plus-place rp2040-plus-mount)
(screen-holder-place-side ST7789-240x240)
;(aviator-neck-support-place (- aviator-assembly-left-or-right-translation) tactile-switch-place-holder)
(aviator-place-shape (translate  [9.5 -10.75 (- aviator-plug-connecter-length)] (rdy -90 gx16)))
(EVQWGD001-place EVQWGD001)
switches)))
)

(spit "things-low/polyhedron-model-with-bottom-plate-test.scad"
      (write-scad (union
                   model-polyhedron
                    bottom-plate-for-polyhedron-model)))

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

(spit "things-low/tps-65-mount-test.scad"
      (write-scad (union
                   (difference
                   (tps-65-place tps-65-mount)
                   (tps-65-place tps-65-mount-cutout))
                  ;(tps-65-place (-# tps-65-mount-main-cutout-smaller))
                   (-# (tps-65-place tps-65-component-cutout))
                   (difference
                    (screen-holder-place-side screen-holder)
                    (screen-holder-place-side screen-holder-cut))
                   (tps-65-place tps-65-connecter-cutout)
                   (vybronics-vl91022-place vybronics-vl91022-mount))))

(spit "things-low/tps-65-mount-print-test.scad"
      (write-scad (difference
                     tps-65-mount
                     tps-65-mount-cutout)
                   ))

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

;; (spit "things-low/vybronics-vl91022-mount.scad"
;;       (write-scad (union
;;                    (difference
;;                     (tps-65-place tps-65-mount)
;;                     (tps-65-place tps-65-mount-cutout)
;;                     (tps-65-place (translate [0 0 (+ 0 1)] tps-65-mount-main-cutout))
;;                     (tps-65-translate-and-place-with-radius (mapv + tps-65-mount-corner-cylinder-bottom-left-position [0 0 (/ (- tps-65-depth tps-65-depth-tolerance 0.5) 1)])
;;                                                             (- 0.5 tps-65-mount-corner-radius) (- 0.5 tps-65-mount-corner-radius)
;;                                                             (rdz 120 (binding [*fn* 3] (cylinder 1 (+ tps-65-depth tps-65-depth-tolerance) :center false))))
;;                     )
;;                    (tps-65-translate-and-place-at-position [10 0 (- (+ tps-65-depth tps-65-depth-tolerance))] (rdy 180 (rdz -90 vybronics-vl91022-mount)))
                   
;;                    )))

(spit "things-low/vybronics-vl91022-mount-test.scad"
      (write-scad vybronics-vl91022-mount))
;; (spit "things-low/drv2605l.scad"
;;       (write-scad drv2605l))

;;  (spit "things-low/drv2605l-standoffs-test.scad"
;;        (write-scad drv2605l-standoffs-test)) 

;;  (spit "things-low/drv2605l-standoffs-print-test.scad"
;;        (write-scad drv2605l-standoffs-print-test)
;;        )


 (spit "things-low/rp2040-plus-mount-test.scad"
       (write-scad 
        (difference
         (union
           (rp2040-plus-place rp2040-plus-mount) 
          ;;  (difference
          ;;   (key-wall-brace-polyhedron 0 0 0 1 "tr" 0 0 0 1 "tl")
          ;;   (usb-jack-place usb-jack-polyhedron))
          )

         ;(translate [ -40 20 (+ rp2040-plus-mount-height 20)] (cube 30 30 40))
         ;(translate [0 0 -20] (cube 350 350 40))
         )))

;;  (spit "things-low/rp2040-plus.scad"
;;        (write-scad rp2040-plus))



(spit "things-low/aviator-assembly.scad"
      (write-scad 

(difference 
 ;;(intersection
  
 
    (union
  aviator-assembly-polyhedron
   (left-section-back 36)
    
    )
  ;;  (->>
  ;;        ;(cube 40 35  (* (+ wall-thickness wall-xy-offset) 2) )
  ;;      (cube 28 30  (* (+ wall-thickness wall-xy-offset) 2))
  ;;      (translate [-2 0 0])
  ;;      aviator-place-shape)
  ;;)
   
 aviator-assembly-diffs

 )


 
;(aviator-place-shape (translate  [9.5 -10.75 (- aviator-plug-connecter-length) ] (rdy -90 gx16)))
 
       ))

(spit "things-low/aviator-ring-test.scad"
      (write-scad
       (difference
        (translate [0 0 1.5] (cube 25 23 3))
        (translate [0 0 0] (binding [*fn* 6] (cylinder (/ (+ aviator-plug-connecter-ring-diameter 4) 2) 3 :center false))))))

(spit "things-low/aviator-assembly-buttons-test.scad"
      (write-scad

       (difference
        (intersection


         (union
          aviator-assembly-polyhedron
          (left-section-back 36))
         (-# (->>
         ;(cube 40 35  (* (+ wall-thickness wall-xy-offset) 2) )
          (cube 11 27  (* (+ wall-thickness wall-xy-offset) 2))
          (translate [0 -8 0])
          (aviator-neck-support-place (- aviator-assembly-left-or-right-translation))))
         )

        aviator-assembly-diffs)
      

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



;; (spit "things-low/left-curve-test.scad"

;;       (write-scad
;;        (difference (union
;;         ;(screen-holder-place-side screen-holder)
;;        ;(curved-corner-xy 1 0 1 1 0 1 (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) oled-post wall-xy-offset)
;;        ;(-# (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset)) 1 1 oled-post (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 1 0 oled-post wall-xy-offset wall-xy-offset))
;;        ;;  (difference 
;;        ;;   (tps-65-place tps-65-base)
;;        ;;   (tps-65-place tps-65-mount-cutout)
;;        ;;   )
;;                     (difference screw-insert-outers
;;                                 screw-insert-holes)
;;                    ; (translate [0 0 0] bottom-plate-for-polyhedron-model)
;;                     (polyhedron-thumb-walls-for-convex-cluster 36)
;;                     (back-left-wall-to-screen 36)
                    
;;        ; (EVQWGD001-place EVQWGD001)
;;                     thumb-type
;;                     (left-section-back 36)
;;                     under-screen
;;        ; dsa-thumbcaps
;;         ;screen-to-EVQWGD001
;;        ;;  (difference (screen-holder-place-side screen-holder)
;;        ;;             (screen-holder-place-side screen-holder-cut) )

;;        ;(right-side-polyhedron 36) 
;;                     ;(left-section-front-polyhedron 36)
;;         ;(left-section-back 36)
;;         ;; thumb-walls-polyhedron
;;         ;;  thumb-corners-polyhedron
;;         ;;  thumb-tweeners-polyhedron
;;                     (union
;;                      (for [column [0]
;;                            row (range 0 (cond (= column 3) nrows :else (- nrows 1)))]        (key-place column row single-plate)))
;;         ;(-# (thumb-wall-brace thumb-bl-place -1  0 oled-post-tl thumb-bl-place -1  0 oled-post-bl thumb-bl-rotate thumb-bl-rotate))
;;                     ;under-screen
;;                     )
;;         ;(translate [0 0 -20] (cube 350 350 40))
;;                    )))

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
;;      (let[curve-points (wall-brace-cubic-polyhedron-curves (points-for-curved-wall-from-thumb-br-bl-to-mr-br 36))
;;           thumb-br-bl-to-br (web-post-linear thumb-br-place "bl" :degrees thumb-br-place "br" :degrees 12)
;;           thumb-br-br-to-mr-bl (web-post-linear thumb-br-place "br" :degrees thumb-mr-place "bl" :degrees 12)
;;           thumb-mr-bl-to-br (web-post-linear thumb-mr-place "bl" :degrees thumb-mr-place "br" :degrees 12)
;;           thumb-bl-to-mr-linear-top (concat (drop-last (thumb-br-bl-to-br :top))
;;                                             (drop-last (thumb-br-br-to-mr-bl :top))
;;                                             (thumb-mr-bl-to-br :top))
;;           thumb-bl-to-mr-linear-bottom  (concat 
;;                                                  (drop-last (thumb-mr-bl-to-br :bottom) )
;;                                             (drop-last (thumb-br-br-to-mr-bl :bottom))
;;                                          (thumb-br-bl-to-br :bottom)
;;                                          ) 
;;           ] 
;;       (write-scad
;;        (union
;;         thumb-type
;;       ;;    (generate-bezier-along-bezier-polyhedron-from-points-list-linear 
;;       ;;     thumb-bl-to-mr-linear-top (curve-points :web-post-top-curve)
;;       ;;     thumb-bl-to-mr-linear-bottom (curve-points :web-post-bottom-curve)
;;       ;;     36)
;;       ;;   (thumb-br-place switch-model)
;;       ;;   (thumb-mr-place switch-model)
;;       ;;   (chained-hull-for-four-lists 
;;       ;;    (plot-bezier-points 
;;       ;;     thumb-bl-to-mr-linear-top
;;       ;;     (sphere 0.001)
;;       ;;     )
;;       ;;    (plot-bezier-points
;;       ;;     (curve-points :web-post-top-curve)
;;       ;;     (sphere 0.001))
;;       ;;    (plot-bezier-points
;;       ;;    (reverse thumb-bl-to-mr-linear-bottom)
;;       ;;     (sphere 0.001))
;;       ;;    (plot-bezier-points
;;       ;;     (reverse (curve-points :web-post-bottom-curve))
;;       ;;     (sphere 0.001))
;;       ;;    36
;;       ;;    )
        
;;       ;;    (generate-bezier-to-point-polyhedron
;;       ;;     (take 12(curve-points :web-post-top-curve)) ((web-post-point thumb-br-place "br" :degrees) :top)
;;       ;;     (reverse (take-last 12 (curve-points :web-post-bottom-curve))) ((web-post-point thumb-br-place "br" :degrees) :bottom)
;;       ;;     )
;;       ;;    (generate-bezier-to-point-polyhedron
;;       ;;     (take 15 (take-last 26(curve-points :web-post-top-curve))) ((web-post-point thumb-br-place "br" :degrees) :top)
;;       ;;     (reverse (take-last 15 (take 26 (curve-points :web-post-bottom-curve)))) ((web-post-point thumb-br-place "br" :degrees) :bottom))
;;       ;;   (generate-bezier-to-point-polyhedron
;;       ;;    (take 15 (take-last 26 (curve-points :web-post-top-curve))) ((web-post-point thumb-mr-place "bl" :degrees) :top)
;;       ;;    (reverse (take-last 15 (take 26 (curve-points :web-post-bottom-curve)))) ((web-post-point thumb-mr-place "bl" :degrees) :bottom))
        
;;       ;;   (generate-bezier-to-point-polyhedron
;;       ;;      (take-last 12 (curve-points :web-post-top-curve)) ((web-post-point thumb-mr-place "bl" :degrees) :top)
;;       ;;      (reverse (take 12 (curve-points :web-post-bottom-curve))) ((web-post-point thumb-mr-place "bl" :degrees) :bottom))
        
;;         ;MxLEDBitPCB-holder-legs-placed-on-keywells
        
        
;;         ;(thumb-tr-place MxLEDBitPCB-holder-legs)
        
;;         (polyhedron-thumb-walls-for-convex-cluster 36)
;;         (thumb-connecters-polyhedron 36)
;;         ;front-wall-polyhedron
        
;;       ;;   (key-place 1 2 (union
;;       ;;                   dsa-cap
;;       ;;                   single-plate))
;;         ))))

;; (spit "things-low/render-test.scad"
;;       (write-scad
;;        (let 
;;         [steps 36
;;          steps-low 8](union 
;;                       (polyhedron-thumb-walls steps);renders
;;                       (thumb-connecters-polyhedron steps-low)
;; thumb-type
;;         (key-web-connecters-polyhedron steps-low)
;;                       key-holes
;;                       (thumb-to-body-connecters-polyhedron steps)
;;             (right-side-polyhedron steps)
;;                       (difference
;;                        (tps-65-place tps-65-mount)
;;                        (tps-65-place tps-65-mount-cutout)
;;                        (tps-65-place (translate [0 0 (+ 0 1)] tps-65-mount-main-cutout))
;;                        (tps-65-translate-and-place-with-radius (mapv + tps-65-mount-corner-cylinder-bottom-left-position [0 0 (/ (- tps-65-depth tps-65-depth-tolerance 0.25) 1)])
;;                                                                (- 0.5 tps-65-mount-corner-radius) (- 0.5 tps-65-mount-corner-radius)
;;                                                                (rdz 120 (binding [*fn* 3] (cylinder 1 (+ tps-65-depth tps-65-depth-tolerance) :center false)))))
;;                       (EVQWGD001-place EVQWGD001-holder)
;;                       (difference
;;                        (cond
;;                          (= screen-holder-mount-position "screen-holder-mount-top") (screen-holder-place screen-holder)
;;                          (= screen-holder-mount-position "screen-holder-mount-side") (screen-holder-place-side screen-holder))
;;                        (cond
;;                          (= screen-holder-mount-position "screen-holder-mount-top") (screen-holder-place screen-holder-cut)
;;                          (= screen-holder-mount-position "screen-holder-mount-side") (screen-holder-place-side screen-holder-cut)))
;; (difference
;;  (polyhedron-case-walls steps)
;;  (usb-jack-place usb-jack-polyhedron))
;;                       (front-wall-connecters-polyhedron steps)
;;                      (difference
;;                       (union
;;                        (polyhedron-left-section steps)
;;                        aviator-assembly-polyhedron)
;;                       aviator-assembly-diffs)
;; (vybronics-vl91022-place vybronics-vl91022-mount)
;;                       )
                     
;;                      )
;;        ))

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
;;        (union 
;;         ; (right-wall-polyhedron 36)
;;         (right-wall-polyhedron-catmull-rom-spline 36)
;;        key-holes
;;        )))

;; (spit "things-low/switch-test.scad"
;;       (write-scad
;;        (union
;;         (-# single-plate)
;;         switch-model
;;         (-# dsa-cap)
;;         (translate [0.75 -4.75 (- plate-thickness)](-# (import "../parts/Kailh Hotswap MX v22.stl")))
;;         MxLEDBitPCB
;;         MxLEDBitPCB-holder-legs
;;         )))

;; (spit "things-low/MxLEDBitPCB-holder-legs-test.scad"
;;       (write-scad
;;        (union
;;         MxLEDBitPCB-holder-legs
;;         single-plate
        
;;          (-#(->>
;;           (difference
;;          (cube MxLEDBitPCB-holder-width (+ MxLEDBitPCB-holder-length (* MxLEDBitPCB-holder-leg-thickness 2)) (+ plate-thickness 0.5))
;;            (cube (+ keyswitch-width 3) (+ keyswitch-height 3) (+ plate-thickness 0.5)))
;;            (translate [0 0 (+ (/ plate-thickness 2) (/ MxLEDBitPCB-holder-leg-z-coordinate 4) 0.3)]))) 
;;         )
;;        ))



  (spit "things-low/back-wall-polyhedron.scad"
       (let [corner-position (calculate-point-between-points   (assoc screen-holder-top-right-outside-point 2 (/ (nth screen-holder-top-right-outside-point 2) 2))
                                                               (assoc (transform-position thumb-bl-place web-post-tl-translation-vector) 2 (/ (nth (transform-position thumb-bl-place web-post-tl-translation-vector) 2) 2)) [0 0 0])
             angle (assoc screen-holder-top-right-outside-point 2 (/ (nth screen-holder-top-right-outside-point 2) 2))
             ]
         (write-scad (union 
                      ;; (place-symbol "../Ananse-Ntontan.svg" {:z-rotation 45 :height 2 :center true :scale-x 0.15 :scale-y 0.15 :rotation #(rdx 90 %)
                      ;;                                        :place (transform-position thumb-br-place  [0 0 0]) :orientation-angle 0
                      ;;                                        :translation (mapv + web-post-bm-translation-vector [(- (+ wall-thickness wall-xy-offset)) 0 (/ (nth (transform-position thumb-br-place [0 0 0]) 2) -2)] )})
                    ;;  (place-symbol-on-key-wall "../Ananse-Ntontan.svg" 
                    ;;                            {:column 1 :row 0 :scale-x 0.15 :offset [-2 0 0]
                    ;;                             :scale-y 0.15 :position "tm" :z-rotation 5 })
                    ;;  (place-symbol-on-case-wall "../Ananse-Ntontan.svg"
                    ;;                            {:place (transform-position  thumb-br-place [0 0 0]) :scale-x 0.15 :offset [0 -2 0]
                    ;;                             :scale-y 0.15 :position "bm" :z-rotation 0})
                    ;;  (place-symbol-on-key-wall "../FUNTUNFUNEFU-DENKYEMFUNEFU.svg"
                    ;;                                {:column 0 :row 0  :offset [-2 0 0]
                    ;;                                 :scale-x 0.10 :scale-y 0.10 :position "tm" :z-rotation 5})
                    ;;  (place-symbol-on-key-wall odenkyem
                    ;;                            {:column 4 :row 2  :offset [-2 0 0]
                    ;;                             :scale-x 0.10 :scale-y 0.10 :position "bm" :z-rotation pinky-splay})
                    ;;  (place-symbol-on-key-wall nea-onnim-no-sua-a-ohu
                    ;;                            {:column 4 :row 2  :offset [-2 0 0]
                    ;;                             :scale-x 0.05 :scale-y 0.05 :position "rm" :z-rotation 0})
                      
                    ;;  (place-2d-shape-on-thumb-wall (square 441.2 441.2)
                    ;;                               {:place thumb-br-place  :offset [(/ mount-width 2) (+ (/ mount-height -2) extra-height) 0]
                    ;;                                :scale-x 0.05 :scale-y 0.05 :position "bm" :z-rotation 0})
                      oodenkyem-on-thumb-br
                      Ananse-Ntontan-on-thumb-bl
                     (place-symbol-on-thumb-wall hw3-me-dua
                                                 {:place thumb-br-place  :offset [(+ (/ mount-width -2) extra-width) extra-height 0]
                                                  :scale-x 0.05 :scale-y 0.05 :position "lm" :z-rotation 0})
                     (place-symbol-on-thumb-wall "../FUNTUNFUNEFU-DENKYEMFUNEFU.svg"
                                                 {:place thumb-tr-place  :offset [0 (+ (/ mount-height -2) extra-height)  0]
                                                  :scale-x 0.05 :scale-y 0.05 :position "rm" :z-rotation 0})
                     (place-symbol-on-thumb-wall nkyinkyim
                                                 {:place thumb-mr-place  :offset [(/ mount-width 2) (+ (/ mount-height -2) extra-height 0.5) 0]
                                                  :scale-x 0.05 :scale-y 0.05 :position "bm" :z-rotation 0 })
                     
                      
                      ;; (place-symbol "../FUNTUNFUNEFU-DENKYEMFUNEFU.svg" {:z-rotation -20 :height 3 :center true :scale-x 0.15 :scale-y 0.14 :rotation #(rdx 80 %)
                      ;;                                        :place corner-position :orientation-angle -45
                      ;;                                        :translation (mapv +  (rotate-around-z-in-degrees -20 [-2 -2 0]) )})
                     
                     ;;  (->>(call :import "file = \"../Ananse-Ntontan.svg\"" "center = true")
                    ;;   (scale [0.2 0.2 1])
                    ;;   (extrude-linear {:height 4 :center false})
                    ;;   (rdx -90)
                    ;;   (translate (mapv + (key-position 1 0 [0 0 0])  web-post-tm-translation-vector [0 (+ wall-thickness wall-xy-offset ) (/ (nth (key-position 1 0 [0 0 0]) 2) -2)])) 
                    ;;   )
                    ;;  (difference 
                    ;;   (union 
                    ;;    (back-wall-polyhedron 36)
                    ;;    ;(left-section-back 36)
                    ;;    )
                    ;;   (usb-jack-place usb-jack-polyhedron))
                    ;;  (difference
                      (polyhedron-thumb-walls-for-convex-cluster 36)
                    ;;   (place-2d-shape-on-thumb-wall (square 95.25 95.25)
                    ;;                                 {:place thumb-mr-place  :offset [(/ mount-width 2) (+ (/ mount-height -2) extra-height 0.5) 0]
                    ;;                                  :scale-x 0.075 :scale-y 0.075 :position "bm" :z-rotation 0}))
                     ;(front-wall-polyhedron 36)
                      ;; (difference
                      ;;  (back-wall-polyhedron 36)
                      ;;  (usb-jack-place usb-jack-polyhedron))
                     ;(polyhedron-left-section 36)
                     ; (left-section-to-thumb-cluster-convex-walls 36)
                     ;(right-wall-polyhedron-catmull-rom-spline 36)
                     ;(left-section-to-thumb-cluster-convex-connecetors 36)
                    ;;  (rp2040-plus-place rp2040-plus-mount)
                      ;; (left-section-back 36)
                     )))
        )
  
;;   (spit "things-low/usb-jack-test.scad"
;;         (write-scad (union 
;;                      (usb-jack-place (-# usb-jack))
;;                      (usb-jack-place usb-jack-polyhedron)
;;                      )))
  
  (defn e-place [shape]
    (->>
     (rdz 180 shape)
     (rdx 45)
     (translate [(/ keyswitch-width -2) 0 (/ cap-top-height 2)])
     (thumb-place-convex -1 2)))

(spit "things-low/front-and-thumb-wall-test.scad"
      (write-scad
       (union
        (front-wall-polyhedron 36)
        key-holes
        ;(back-left-wall-to-screen 36)
        ;(thumb-to-body-connecters-polyhedron 36)
        (thumb-connecters-polyhedron 36)
        ;(polyhedron-thumb-walls 36)
        (polyhedron-thumb-walls-for-convex-cluster 36)
      
        
        
        ;(key-place 1 2 dsa-cap)
        ;(left-section-to-thumb-cluster-convex-walls 36)
        ;(back-left-wall-to-screen 36)
        ;(screen-holder-place-side screen-holder)
       ;(left-section-to-thumb-cluster-convex-connecters 36)
        ;(right-side-polyhedron 36)
        ;(key-web-connecters-polyhedron 12)
        ;(thumb-connecters-polyhedron 12)
        ;key-holes
        ;thumb-type
         ;dsa-thumbcaps
        ;(import "../parts/top-left-surface.stl")
        ) 
       )
      )

(spit "things-low/front-wall-polyhedron-test.scad"
      (write-scad (front-wall-polyhedron 36))
      )
  
  ;; (spit "things-low/left-front-test.scad"
  ;;       (write-scad
  ;;        (union
  ;;         ;; (difference
  ;;         ;;  (screen-holder-place-side screen-holder)
  ;;         ;;   (screen-holder-place-side screen-holder-cut))
  ;;         ;(tps-65-place tps-65-mount)
  ;;         (difference
  ;;          (left-section-front-polyhedron 36)
  ;;          (EVQWGD001-place EVQWGD001-main-cutout))
  ;;         ;(EVQWGD001-place EVQWGD001-holder)
  ;;         ;thumb-type
  ;;         ;(polyhedron-thumb-walls 36)
  ;;         (back-left-wall-to-screen 36)
  ;;         ;under-screen
          
  ;;         )))


  
;; (spit "things-low/key-placement-test.scad"
;;       (write-scad
;;        (project
;;         (union
;;         key-holes
;;         thumb-type
;;         ))
;;        )
;;       )
;;   (spit "things-low/key-test.scad"
;;         (write-scad 
;;          (union
;;           (union 
;;           ;switches
;;          ; (thumb-1x-layout switch-model)
;;           ;(thumb-15x-layout switch-model)
;;           MxLEDBitPCB-placed
;;           ;(thumb-1x-layout MxLEDBitPCB)
;; ;(thumb-15x-layout MxLEDBitPCB)
;;            ;(place-per-key MxLEDBitPCB-holder-legs)
          
;;           key-holes
;;           ;kailh-hotswap-mx-thumbs
;;           ;(-# (thumb-bl-place oled-post-tl))
;;           ;(key-web-connecters-polyhedron 12)
;;           ;(des-caps {:style :des-scooped})
;;           des-thumbs
;;           ;dsa-caps
;;           dsa-thumbcaps
;;           ;(place-per-key kailh-hotswap-mx)
;;           ;(thumb-place-convex 0 0 oled-post-tr)
;;           ;(color WHI (thumb-place-convex 0 0 single-plate))
;;           ;(color RED (thumb-place-convex 0 1 single-plate))
;;           ;(color GRE (thumb-place-convex 0 2 single-plate))
;;           ;(color CYA (thumb-place-convex 1 0 single-plate))
;;           ;(color MAG (thumb-place-convex 1 1 single-plate))
;;           ;(color PUR (thumb-place-convex 1 2 single-plate))
;;           ;(color WHI (thumb-place-convex 0 0 dsa-cap))
;;           (color RED (thumb-place-convex 0 1 dsa-cap))
;;           (color GRE (thumb-place-convex 0 2 dsa-cap))
;;           ;(color CYA (thumb-place-convex 1 0 dsa-cap))
;;          ; (color MAG (thumb-place-convex 1 1 dsa-cap))
;;           ;(color PUR (thumb-place-convex 1 2 dsa-cap))
;;           (e-place EVQWGD001)
;;           (e-place EVQWGD001-holder)
;;           thumb-type
;;           (thumb-1x-layout MxLEDBitPCB)
;;           (thumb-15x-layout MxLEDBitPCB)
;;           ;(right-side-polyhedron 36)
;;           (tps-65-place tps-65-mount)
;;           (thumb-connecters-polyhedron 12))
;;          ;  (translate [0 0 -20] (cube 350 350 40))
;;           )))
  
  ;; (spit "things-low/des-test.scad"
  ;;       (write-scad
  ;;        (union
  ;;         (-# des-r1) 
  ;;         switch-model
  ;;         single-plate
  ;;         )
  ;;        ))
  
  ;; (spit "things-low/hole-placement-test.scad"
  ;;       (write-scad 
  ;;        (let 
  ;;         [steps 36]
  ;;          (union
  ;;           (polyhedron-left-section steps)
  ;;         (polyhedron-case-walls steps)
  ;;           (polyhedron-thumb-walls steps)
  ;;           screw-insert-outers
  ;;           (rp2040-plus-place rp2040-plus-mount )

  ;;           ))))

;; (spit "things-low/vybronics-vl91022-mount-hole-cover.scad"
;;       (write-scad vybronics-vl91022-mount-hole-cover))
;; (spit "things-low/currently-editing.scad"
;;       (write-scad
;;        (include "front-and-thumb-wall-test.scad"))
;;       )




;; (spit "things-low/MxLEDBitPCB-on-keywell.scad"
;;       (write-scad 
;;        (let [steps-low 8
;;              old-thumb #(union
;;                           (thumb-place-convex-old 0 0 %)
;;                           (thumb-place-convex-old 0 1 %)
;;                           (thumb-place-convex-old 0 2 %)
;;                           (thumb-place-convex-old 1 1 %)
;;                           (thumb-place-convex-old 1 2 %))
;;              original-thumb #(union
;;                               (thumb-tr-place-standard %)
;;                               (thumb-tl-place-standard %)
;;                               (thumb-mr-place-standard %)
;;                               (thumb-bl-place-standard %)
;;                               (thumb-br-place-standard %) 
;;                               )]
;;          (union
;;  ;       MxLEDBitPCB-placed-on-keywells
;; ;MxLEDBitPCB-placed-on-thumbs
;;   ;        (key-place 0 2 MxLEDBitPCB) 
;;           key-holes
;;          ; (thumb-connecters-polyhedron steps-low) ;renders
;;           ;(key-web-connecters-polyhedron steps-low)
          
;; ;;           (thumb-place-convex 0 3 single-plate)
;; ;;           (thumb-place-convex 0 3 dsa-cap)
;; ;;           (thumb-place-convex 1 3 single-plate)
;; ;; (thumb-place-convex 1 3 dsa-cap)
;;           (left-section-to-thumb-cluster-convex-connecters  36) 
;;           ;dsa-thumbcaps
;;           dsa-caps
;;           des-cornelius-thumbs
;;      ;     (des-caps  {:style :des-scooped})
;;           thumb-type
;;           (-# (original-thumb (union
;;                       single-plate
;;                       dsa-cap)))
;;           ))))
;; (spit "things-low/right-side-polyhedron.scad"
;;       (write-scad
;;        (union
;;         ;(right-side-polyhedron 36)
;;         (difference 
;;     (union
;;   aviator-assembly-polyhedron
;;    (left-section-back 36) 
;;     ) 
;;  aviator-assembly-diffs
;;         )
;;         (vybronics-vl91022-place vybronics-vl91022-mount)
;;          (difference
;;           (tps-65-place tps-65-mount)
;;           (tps-65-place tps-65-mount-cutout))
        
;;         ;(-# key-holes)
;;         )))

;; (spit "things-low/cornelius-thumbs-with-sprues-right.scad"
;;       (write-scad right-with-sprues))

;; (spit "things-low/cornelius-thumbs-with-sprues-left.scad"
;;       (write-scad left-with-sprues))

(spit "things-low/back-wall-polyhedron-catmull-rom-test.scad" 
      (write-scad (union
                    (difference
                     (back-wall-polyhedron-catmull-rom 36)
                       (usb-jack-place usb-jack-polyhedron))
                   ;key-holes
                   (left-section-back 36)
                   (right-wall-polyhedron-catmull-rom-spline 36)
                   aviator-assembly-polyhedron
                   (rp2040-plus-place rp2040-plus-mount)
                   ;bottom-plate-for-polyhedron-model
                   )))

(spit "things-low/generate-bezier-along-bezier-polyhedron-all-side-test.scad"
      (let [steps 36
            min-z 0
            max-z 10
            min-y -5
            max-y 5
            min-x -5
            max-x 5
            top-outside (bezier-linear [min-x min-y max-z] [max-x min-y max-z] steps)
            top-inside (bezier-linear  [max-x max-y max-z] [min-x max-y max-z]   steps)
            bottom-outside (bezier-linear [min-x min-y min-z] [max-x min-y min-z] steps)
            bottom-inside (bezier-linear   [max-x max-y min-z] [min-x max-y min-z]  steps)
            outer-points (into [] 
                               (apply concat 
                                      (for [index (range 0 (inc steps))] 
                                        (bezier-linear 
                                         (nth top-outside index) 
                                         (nth bottom-outside index) 
                                         steps) 
                                        )))
            inner-points (into []
                               (apply concat
                                      (for [index (range 0 (inc steps))]
                                        (bezier-linear
                                         (nth top-inside index)
                                         (nth bottom-inside index)
                                         steps))))
            wall-left (wall-brace-polyhedron-curve-points (partial key-place 0 1) 0 -1 "bl" :radians steps)
            wall-right (wall-brace-polyhedron-curve-points (partial key-place 0 1) 0 -1 "br" :radians steps)
            wall-outer (into []
                             (apply concat
                                    (for [index (range 0 (inc steps))]
                                      (bezier-linear 
                                       (nth (wall-right :outer-points) index)
                                       (nth (wall-left :outer-points) index)
                                       steps))))
            wall-inner (into []
                             (apply concat
                                    (for [index (range 0 (inc steps))
                                          :let [left (reverse (wall-left :inner-points))
                                                right (reverse (wall-right :inner-points))]]
                                      (bezier-linear 
                                       (nth right index)
                                       (nth  left index) 
                                       steps))))
            spline-test (cubic-hermite-spline-curve-segment [0 0 0] [1 1 1] [1 0 0] [0 1 0] 20)
            curve-1 (bezier-quadratic [-2 2 0] [-2 0 0] [-1 0 0] 20)
            
            test-m [[4 7]
                    [2 6]]
            test-m-inv (/ 1 (- (* (nth (nth test-m 0) 0) (nth (nth test-m 0) 0))))
            basis-inv (matrix-inverse [[(/ -3  1) 0 (/ 3 1) 0 0]
                                       [1 4 1 0 0]
                                       [0 1 4 1 0]
                                       [0 0 1 4 1]
                                       [0 0 (/ -3 1) 0 (/ 3 1)]])
            basis (generate-basis-matrix-to-find-cubic-uniform-b-spline-points-from-knots 2)
            basis-2 (calculate-non-vanishing-basis-functions 3
                                                             1 3.0 [0 0 0 0 1 1 1 1])
            nurbs-test   (nurbs-with-homogenous-coordinates [[0 0 0 1] [0 1 0 1] [5 0 0 5] [2 1 0 1] [2 0 0 1]] 2 [0 0 0 1 2 3 3 3] 20)
            nurps-segment-1 (nurbs-segment 4 2 [0 0 0 1 2 3 3 3] [[0 0 0 1] [0 1 0 1] [1 0 0 1]] 20)
             nurps-segment-2 (nurbs-segment 4 2 [0 0 0 1 2 3 3 3] [[0 0 0 1] [0 1 0 1] [1 0 0 1] [2 1 0 1] [2 0 0 1]] 20 :u-start 1.0)
           ; nurbs-test-size (count nurbs-test)
            ;nurbs-test (mapv #(subvec (coerce :persistent-vector %) 0 3) (b-spline-wrapper [[0 0 0 1] [0 1 0 1] [1 0 1 0.5] [2 1 2 1] [2 0 2 1]] 2 [0 0 0 (/ 1 3) (/ 2 3) 1 1 1] false 11))
            ] 
        
        (write-scad 
      ; (wall-brace-polyhedron  (partial key-place 0 1) 0 -1 "bl" :radians (partial key-place 0 1) 0 -1 "br" :radians wall-xy-offset wall-xy-offset true steps)
        ;;  (wall-brace-quadratic-polyhedron (partial key-place 1 cornerrow) 1 -0.1 "br" :radians
        ;;                           (partial key-place 2 cornerrow) 0.25 -1 "bl" :radians
        ;;                           (partial key-place 2 cornerrow) 0 -1 "bm" :radians
        ;;                           wall-xy-offset-medium-thin wall-xy-offset-thin wall-xy-offset-thin
        ;;                                   true
        ;;                           steps)
        ;;  (wall-brace-quadratic-polyhedron (partial key-place 2 cornerrow) 1 -1 "bl" :radians
        ;;                           (partial key-place 2 cornerrow) 0 -1 "br" :radians
        ;;                           (partial key-place 3 cornerrow) -1 0 "bl" :radians
        ;;                           wall-xy-offset-thin wall-xy-offset-thin wall-xy-offset-mid
        ;;                           steps)
        ;;  (wall-brace-catmull-rom-spline 
        ;;   (points-fn-deg thumb-tr-place 1 0 "br" wall-xy-offset)
        ;;   (points-fn-deg thumb-tr-place 1 -1 "tr" wall-xy-offset-mid) 
        ;;  (points-fn-rad  (partial key-place 1 cornerrow) 1 0.2 "br"   wall-xy-offset-thin)
        ;;   (points-fn-rad (partial key-place 2 cornerrow) 1 -1 "bl" wall-xy-offset-thin)
        ;;   ;(points-fn-rad  (partial key-place 2 cornerrow) 0 -1 "br" wall-xy-offset-thin)
        ;;   ;(points-fn-rad  (partial key-place 3 cornerrow) -1 0 "bl" wall-xy-offset-mid)
        ;;   steps
        ;;   :web-post-top-style :curved :extra-points-for-sides-and-top-and-bottom true)
        ;;  (wall-brace-catmull-rom-spline
        ;;   ;(points-fn-deg thumb-tr-place 1 0 "br" wall-xy-offset)
        ;;   (points-fn-deg thumb-tr-place 1 -1 "tr" wall-xy-offset-mid)
        ;;   (points-fn-rad  (partial key-place 1 cornerrow) 1 0.2 "br"   wall-xy-offset-thin)
        ;;   (points-fn-rad (partial key-place 2 cornerrow) 1 -1 "bl" wall-xy-offset-thin)
        ;;   (points-fn-rad  (partial key-place 2 cornerrow) 0 -1 "br" wall-xy-offset-thin)
        ;;   ;(points-fn-rad  (partial key-place 3 cornerrow) -1 0 "bl" wall-xy-offset-mid)
        ;;   steps
        ;;   ;:web-post-top-style :curved
        ;;   :extra-points-for-sides-and-top-and-bottom true)
      ;;  (key-place 1 cornerrow (union
      ;;                          dsa-cap
      ;;                          single-plate))
         ;(generate-bezier-along-bezier-polyhedron-all-sides wall-outer wall-inner steps)
         ;(generate-bezier-along-bezier-polyhedron-all-sides outer-points inner-points steps)
        ;;  (plot-bezier-points spline-test (sphere 0.05))
        ;;  (plot-bezier-points (cubic-hermite-tension-spline-curve [0 0 0] [2 0 0] [2 1 0] [2 -1 0] 0.5 20) (sphere 0.05))
        ;;  (plot-bezier-points (cubic-hermite-tension-spline-curve [0 0 0] [2 0 0] [2 1 0] [2 -1 0] 1 20) (sphere 0.05))
        ;;  (plot-bezier-points (cubic-hermite-tension-spline-curve [0 0 0] [2 0 0] [2 1 0] [2 -1 0] 2 20) (sphere 0.05))
        ;;  (color [1 0 0 1] (plot-bezier-points curve-1 (sphere 0.05)))
        ;;  (color [1 0 0 1] (plot-bezier-points (bezier-linear [1 1 1] [1 2 1] 20) (sphere 0.05)) )
        ;;  (color [0 1 0 1](plot-bezier-points (bezier-cubic [-2 6 0] [-1 4 0] [1 4 0] [2 6 0] 20) (sphere 0.05)))
        ;;  (plot-bezier-points (catmull-rom-spline-as-bezier-cubic [-2 6 0] [-1 4 0] [1 4 0] [2 6 0]  20) (sphere 0.05))
         ;(plot-bezier-points (quadratic-uniform-b-spline [[1 0 0] [1 1 0] [2 1 0] [2 0 0]] 20) (sphere 0.05))
         ;(plot-bezier-points (quadratic-uniform-b-spline-through-terminal-endpoint [[1 0 0] [1 1 0] [2 1 0] [2 0 0]] 40) (sphere 0.05))
         ;(plot-bezier-points (cubic-uniform-b-spline [[0 0 0] [0 1 0] [1 1 0] [2 1 0] [2 0 0]] 20) (sphere 0.05))
         ;(plot-bezier-points (cubic-uniform-b-spline [[0 0 0] [1 1 0] [1 1 0] [1 1 0] [2 0 0]] 20) (sphere 0.05))
         ;(plot-bezier-points (cubic-uniform-b-spline-through-terminal-endpoints [[0 0 0] [0 1 0] [1 1 0] [2 1 0] [2 0 0]] 40) (sphere 0.05))
         ;(plot-bezier-points (cubic-uniform-b-spline-closed [[0 0 0] [0 1 0] [1 1 0] [2 1 0] [2 0 0]] 50) (sphere 0.05))
         ;(plot-bezier-points (cubic-b-spline-with-tension [[0 0 0] [0 1 0] [1 1 0]  [1 0 0]] 3 20) (sphere 0.05))
         ;(plot-bezier-points (cubic-b-spline-with-tension [[0 0 0] [0 1 0] [1 1 0]  [1 0 0]] 5 20) (sphere 0.05))
         ;(plot-bezier-points (cubic-b-spline-with-tension [[0 0 0] [0 1 0] [1 1 0]  [1 0 0]] 0 20) (sphere 0.05))
        
       ;  (plot-bezier-points (quartic-uniform-b-spline-segment [0 0 0] [1 1 0] [4 4 0] [8 2 0] [12 0 0] 20) (sphere 0.05))
        ; (plot-bezier-points (bezier-cubic [-5 0 0] [-2.5 5 0] [2.5 5 0] [5 0 0] 10) (sphere 0.05))
         ;(color [1 0 0 1](plot-bezier-points (n-degree-bezier-curve [[-5 0 0] [-2.5 5 0] [2.5 5 0] [5 0 0]] 10) (sphere 0.05)))
         
         ;(plo(n-degree-bezier-curve [[-5 0 0] [0 5 0] [5 0 0]] 10))
         ;(for [index (range 0 (inc 3)) ](println (bezier-basis-fn-times-point 3 index [0 0 0] 0)))
         ;(hermite-straight-segment [10 10 1] [20 10 1] [12 10 1] [24 10 1] steps)
       ;  (println (det (identity-matrix 2)))
         ;(println (matrix-inverse  [[4 7] [2 6]]))
       ;(println (mmul [[4 7 7] [2 6 7] [2 4 3]] (matrix-inverse [[4 7 7] [2 6 7] [2 4 3]]) ))
      ;;  (println (determinant [[4 -2 1]
      ;;                         [5 0 3]
      ;;                         [-1 2 6]]))
      ;;    (println (element-apply  (mmul  [[4 -2 1]
      ;;                     [5 0 3]
      ;;                     [-1 2 6]]
      ;;                    (matrix-inverse [[4 -2 1]
      ;;                              [5 0 3]
      ;;                              [-1 2 6]])) 
      ;;                             #(Math/round %)))
        ; (for [p basis-inv] (println p))
        ;(println (element-apply (mmul [[0.25] [(/ 1 6)] [1] [(/ 11 6)] [0.25]] basis-inv ) #(Math/round %)))
          ;;  (for [p basis-inv]
          ;;  (println   (reduce + (mapv * p (mapv #(* % 6)  (into [] (apply concat [[0.5] [(/ 1 6)] [1] [(/ 11 6)] [0.5]]))))))
          ;;   )
          ;(plot-bezier-points (cubic-uniform-b-spline [[0 0 0] [0 1 0] [1 1 0] [2 1 0] [2 0 0]] 10) (sphere 0.05))
          ;(plot-bezier-points (kochanek-bartels-spline-curve [[-1 -1 0] [0 0 0]  [4 6 0] [10 -1 0] [11 -2 0]] 40 :tension-values [0 -1 0] :continuinity-values [0 0 0] :bias-values [0 0 0] ) (sphere 0.05))
        ; (color [1 0 0 1](plot-bezier-points (cubic-uniform-b-spline-through-points [0.5 0.5 0] [ [(/ 1 6) (/ 5 6) 0] [1 1 0] [(/ 11 6) (/ 5 6) 0] ] [0.5 -0.5 0] 10) (sphere 0.05)))
         ;(for [b basis] (println b))
        ;(plot-bezier-points (bezier-cubic [-2 6 0] [-1 4 0] [1 4 0] [2 6 0] 20) (sphere 0.05))
         ;(color [0 1 0 1] (plot-bezier-points (reparametrized-cubic-bezier-curve [-2 6 0] [-1 4 0] [1 4 0] [2 6 0] 1 1.5 10) (sphere 0.05)))
         
         ;(plot-bezier-points (linear-non-uniform-b-spline-segment [-5 -5 0] [5 -5 0] 20) (sphere 0.05))
         ;(println (shape [[0 0 0 0.5] [0 1 0 0.5] [1 0 0 0.5] [2 1 0 0.5] [2 0 0 0.5]]))
         ;(plot-bezier-points nurbs-test (sphere 0.05))
      ;   (mapv (partial println) nurbs-test)
        ;;  (mapv (partial println)  (chisel-protocols/polyline (chisel-curves/b-spline {:control-points [[0.0 0.0 0.0 1.0] [1.0 1.0 1.0 1.0] [2.0 2.0 2.0 0.5] [3.0 1.0 3.0 1.0] [4.0 0.0 4.0 1.0]]
        ;;                                                                               :order 2 :knot-vector [0 0 0 0.333 0.666 1 1 1] :reversed-evaluation? false}) 11))
        ;;  (for [index (range 0 11)
        ;;        :let [i (/ index 10)]]
        ;;    (println (direct-nurbs-evaluation i 2 [[0.0 0.0 0.0 1.0] [1.0 1.0 1.0 1.0] [2.0 2.0 2.0 0.5] [3.0 1.0 3.0 1.0] [4.0 0.0 4.0 1.0]] [0 0 0 0.333 0.666 1 1 1] ))
        ;;    )
         ;[[0.0 0.0 0.0 1.0] [0.0 1.0 0.0 1.0] [1.0 0.0 1.0 0.5] [2.0 1.0 2.0 1.0] [2.0 0.0 2.0 1.0]]
        ;;  (println (for [i (range 0 11)]
        ;;             (nip i 2 [0 0 0 1 2 3 4 4 5 5 5] (/ 5 2))))
         
        ;; (println (calculate-knot-span-index 3 3 1 [0 0 0 0 1 1 1 1]))
        ;; ;(println basis-2)
        ;;    (doseq [basis-function basis-2]
        ;;      (println basis-function)
        ;;      ) 
        ;;  (println (calculate-nurbs-curve-point 3 3 [0 0 0 0 1 1 1 1] [[-4 -4 0 1] [-2 4 0 1] [2 -4 0 1] [4 4 0 1]] 1))
        (println nurbs-test)
        (for [index (range 0 (count nurbs-test))]
          (color [1 (/ index (count nurbs-test)) 0 1](translate (nth nurbs-test index) (sphere 0.05)))
          )
         ;(plot-bezier-points nurbs-test (sphere 0.05)) 
        )))

(defn -main [dum] 1)  ; dummy to make it easier to batch