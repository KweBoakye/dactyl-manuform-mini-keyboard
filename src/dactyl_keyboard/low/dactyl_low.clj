(ns dactyl-keyboard.low.dactyl-low
  (:refer-clojure :exclude [use import])
  (:require ;   [chisel.curves :as chisel-curves :refer [direct-nurbs-evaluation  b-spline clamped-b-spline direct-nurbs-evaluation]]
 ; [chisel.protocols :as chisel-protocols :refer [PParametricCurve]]
            [clojure.core.matrix :refer [magnitude]]
            [clojure.math :refer [ceil floor sqrt]]
            [dactyl-keyboard.AST1109MLTRQ :refer :all] ;[dactyl-keyboard.cirque-circle-trackpad :refer :all]
            [dactyl-keyboard.cornelius-thumbs-with-sprues :refer :all]
            [dactyl-keyboard.des-caps :refer :all]
            [dactyl-keyboard.dovetail :refer :all]
            [dactyl-keyboard.drv2605l-standoffs :refer :all]
            [dactyl-keyboard.EVQWGD001 :refer :all]
            [dactyl-keyboard.IS31FL3743A-mount :refer :all]
            [dactyl-keyboard.klor.klor-points :refer [pcb-points-list]]
            [dactyl-keyboard.lib.affine-transformations :refer [rotate-around-z-in-degrees]] ;
            [dactyl-keyboard.lib.algebra :refer [find-point-on-line-using-z]]
            [dactyl-keyboard.lib.curvesandsplines.b-spline-surface :refer [b-spline-surface]]
            [dactyl-keyboard.lib.curvesandsplines.beziers :refer [bezier-cubic
                                                                  bezier-linear bezier-quintic bezier-sextic n-degree-bezier-curve]]
            [dactyl-keyboard.lib.curvesandsplines.curve-fitting :refer [global-curve-interp global-curve-interp-with-end-derivatives
                                                                        global-curve-interp-with-end-unit-derivatives-curve local-cubic-curve-interpolation
                                                                        local-cubic-curve-interpolation-with-calculated-tangents
                                                                        local-cubic-curve-interpolation-with-calculated-tangents-curve]]
            [dactyl-keyboard.lib.curvesandsplines.curve-utils :refer [homogenize-cooridinates]] ;[dactyl-keyboard.lib.curvesandsplines.linear-surface :refer [bilinear-surface]]
            [dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline :refer :all]
            [dactyl-keyboard.lib.curvesandsplines.splines :refer [catmull-rom-spline-as-bezier-cubic catmull-rom-spline-curve cubic-hermite-tension-spline-curve
                                                                  kochanek-bartels-spline-curve]]
            [dactyl-keyboard.lib.curvesandsplines.uniform-b-splines :refer [cubic-uniform-b-spline-through-terminal-endpoints]]
            [dactyl-keyboard.lib.geometry :refer [deg2rad]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vnf :refer :all]
            [dactyl-keyboard.lib.openscad.hull :refer [chained-hull]]
            [dactyl-keyboard.lib.transformations :refer [rdx rdy rdz]]
            [dactyl-keyboard.lib.vectors :refer [calculate-point-between-points]]
            [dactyl-keyboard.low.aviator-low :refer :all]
            [dactyl-keyboard.low.case-low :refer :all]
            [dactyl-keyboard.low.case-low-functions :refer :all]
            [dactyl-keyboard.low.case-low-polyhedron :refer :all]
            [dactyl-keyboard.low.case-low-polyhedron-functions :refer :all]
            [dactyl-keyboard.low.EVQWGD001-placement-functions :refer :all]
            [dactyl-keyboard.low.oled-low-placements :refer :all]
            [dactyl-keyboard.low.palm-rest-low :refer :all]
            [dactyl-keyboard.low.placement-functions-low :refer :all]
            [dactyl-keyboard.low.screen-holder-placement-functions :refer :all]
            [dactyl-keyboard.low.screen-holder-placement-points :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.low.thumbs-low :refer :all]
            [dactyl-keyboard.low.tps-65-placement-functions :refer :all]
            [dactyl-keyboard.low.tps-65-placement-points :refer :all]
            [dactyl-keyboard.low.vvybronics-vl91022-placement-functions :refer :all] ;[dactyl-keyboard.low.cirque-circle-trackpad-placement-functions-low :refer :all]
            [dactyl-keyboard.low.web-connecters-low :refer :all]
            [dactyl-keyboard.metal-tactile-button :refer :all]
            [dactyl-keyboard.MxLEDBitPCB-holder :refer :all]
            [dactyl-keyboard.oled :refer :all]
            [dactyl-keyboard.pico-standoffs :refer :all]
            [dactyl-keyboard.RP2040-Plus :refer :all]
            [dactyl-keyboard.sa-keycaps :refer :all]
            [dactyl-keyboard.six-pin-ffc-adapter-board :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.tps-65 :refer :all]
            [dactyl-keyboard.utils :refer [D_BLA plot-bezier-points]]
            [dactyl-keyboard.vybronics-vl91022 :refer :all]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))

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
               MxLEDBitPCB-holder-leg-3))))

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
   (thumb-15x-layout kailh-hotswap-mx)))


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
              MxLEDBitPCB-holder-leg-3)))

(def thumb-tr-MxLEDBitPCB-holders
  (thumb-tr-place (union
                   (intersection
                    (union
                     MxLEDBitPCB-holder-leg-1
                     MxLEDBitPCB-holder-leg-3)
                    MxLEDBitPCB-clearance)
                   MxLEDBitPCB-holder-leg-2)))

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
  (let [steps 60
        steps-low 15
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

     (thumb-connecters-polyhedron 60) ;renders
     (key-web-connecters-polyhedron steps-low)
     ;(EVQWGD001-place EVQWGD001-holder)
     (front-wall-connecters-polyhedron steps);renders
     (union
      (difference
       (union
        (polyhedron-left-section steps)
        aviator-assembly-polyhedron)
       aviator-assembly-diffs)
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
                                               (rdz 120 (binding [*fn* 3] (cylinder 1 (+ tps-65-depth tps-65-depth-tolerance) :center false)))))
       ;(color [1 0 0 1] (rp2040-plus-place rp2040-plus-mount))
      (difference screw-insert-outers
                  screw-insert-holes)
      (vybronics-vl91022-place vybronics-vl91022-mount)
      (difference
       (cond
         (= screen-holder-mount-position "screen-holder-mount-top") (screen-holder-place screen-holder)
         (= screen-holder-mount-position "screen-holder-mount-side") (screen-holder-place-side screen-holder))
       (cond
         (= screen-holder-mount-position "screen-holder-mount-top") (screen-holder-place screen-holder-cut)
         (= screen-holder-mount-position "screen-holder-mount-side") (screen-holder-place-side screen-holder-cut)))))))

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
                                des-cornelius-thumbs))))

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
  (let [steps 60
        steps-low 8
        steps-mid 16
        back-wall-polyhedron-points    (back-wall-polyhedron-catmull-rom steps :bottom-plate true)
        left-section-back-points    (left-section-back steps :bottom-plate true)
        back-left-wall-to-screen-points (back-left-wall-to-screen steps :bottom-plate true)
        screen-holder-bottom-left-outside-floor-point-and-screen-holder-bottom-right-outside-floor-point [screen-holder-bottom-left-outside-floor-point screen-holder-bottom-right-outside-floor-point]
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
                             right-wall-polyhedron-points)
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
     (translate [0 0 -1.5] (extrude-linear
                            {:height 1.5 :center false :convexity 10}
                            (polygon (map drop-last bottom-plate-points))))
  ;  (println polyhedron-thumb-walls-points)
     )))
(def header  (translate [0 -12 (+ usb-jack-height (* rp2040-plus-thickness 2) 3)] (rdz 90 (import "../parts/ImageToStl.com_pin header 1x16 th pitch 2.54mm.stl"))))
(spit "things-low/bottom-plate-for-polyhedron-model.scad"
      (write-scad (union
                   (difference
                    bottom-plate-for-polyhedron-model
                    (translate [0 0 -10] screw-insert-screw-holes))
                   (IS31FL3743A-standoff-place IS31FL3743A-fillet-standoffs)
                   (drv2605l-place drv2605l-standoffs)
                   (six-pin-ffc-adapter-place six-pin-ffc-adapter-standoffs)
                   (rp2040-plus-place rp2040-plus-mount))))


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
          switches))))

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
                   tps-65-mount-cutout)))

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
         (left-section-back 60))
  ;;  (->>
  ;;        ;(cube 40 35  (* (+ wall-thickness wall-xy-offset) 2) )
  ;;      (cube 28 30  (* (+ wall-thickness wall-xy-offset) 2))
  ;;      (translate [-2 0 0])
  ;;      aviator-place-shape)
  ;;)

        aviator-assembly-diffs)



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
          (left-section-back 60))
         (-# (->>
         ;(cube 40 35  (* (+ wall-thickness wall-xy-offset) 2) )
              (cube 11 27  (* (+ wall-thickness wall-xy-offset) 2))
              (translate [0 -8 0])
              (aviator-neck-support-place (- aviator-assembly-left-or-right-translation)))))

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
;;                     (polyhedron-thumb-walls-for-convex-cluster 60)
;;                     (back-left-wall-to-screen 60)

;;        ; (EVQWGD001-place EVQWGD001)
;;                     thumb-type
;;                     (left-section-back 60)
;;                     under-screen
;;        ; dsa-thumbcaps
;;         ;screen-to-EVQWGD001
;;        ;;  (difference (screen-holder-place-side screen-holder)
;;        ;;             (screen-holder-place-side screen-holder-cut) )

;;        ;(right-side-polyhedron 60) 
;;                     ;(left-section-front-polyhedron 60)
;;         ;(left-section-back 60)
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
;;      (let[curve-points (wall-brace-cubic-polyhedron-curves (points-for-curved-wall-from-thumb-br-bl-to-mr-br 60))
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
;;       ;;     60)
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
;;       ;;    60
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

;;         (polyhedron-thumb-walls-for-convex-cluster 60)
;;         (thumb-connecters-polyhedron 60)
;;         ;front-wall-polyhedron

;;       ;;   (key-place 1 2 (union
;;       ;;                   dsa-cap
;;       ;;                   single-plate))
;;         ))))

;; (spit "things-low/render-test.scad"
;;       (write-scad
;;        (let 
;;         [steps 60
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

(spit "things-low/single-plate-test.scad"
      (write-scad (let [sh (->> (cube post-size post-size web-thickness :center true)
                                (translate web-post-translation-vector))] (union
                                                                           (-# single-plate)
                                                                           web-post-tl
                                                                           web-post-tr
                                                                           web-post-bl
                                                                           web-post-br
                                                                           curve-post-tr
                                                                           curve-post-tl
                                                                           curve-post-bl
                                                                           curve-post-br
                                                                           (translate (get-single-plate-corner-position-vector "tr") sh)
                                                                           (translate (get-single-plate-corner-position-vector "tl") sh)
                                                                           (translate (get-single-plate-corner-position-vector "bl") sh)
                                                                           (translate (get-single-plate-corner-position-vector "br") sh)
                                                                           (translate (get-curve-corner-translation-vector :bl) curve-post)
                                                                           (color [0 1 0 1] (translate (get-curve-corner-translation-vector "tr") sh))
                                                                           (color [1 0 0 1] (translate (get-curve-corner-translation-vector "tl") sh))
                                                                           (color [0 0 1 1] (translate (get-curve-corner-translation-vector "bl") sh))
                                                                           (color [1 1 1 1] (translate (get-single-plate-corner-position-vector "br") curve-post))))))



;; (spit "things-low/curved-corner-test.scad"
;;       (write-scad 
;;        (curved-corner 0 -1 1 -1 1 0 (partial key-place 4 cornerrow) oled-post-br)
;;        )
;;       )

;; (spit "things-low/right-wall-test.scad"
;;       (write-scad
;;        (union 
;;         ; (right-wall-polyhedron 60)
;;         (right-wall-polyhedron-catmull-rom-spline 60)
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
            angle (assoc screen-holder-top-right-outside-point 2 (/ (nth screen-holder-top-right-outside-point 2) 2))]
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
                                                  :scale-x 0.05 :scale-y 0.05 :position "bm" :z-rotation 0})


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
                    ;;    (back-wall-polyhedron 60)
                    ;;    ;(left-section-back 60)
                    ;;    )
                    ;;   (usb-jack-place usb-jack-polyhedron))
                    ;;  (difference
                     (polyhedron-thumb-walls-for-convex-cluster 60)
                    ;;   (place-2d-shape-on-thumb-wall (square 95.25 95.25)
                    ;;                                 {:place thumb-mr-place  :offset [(/ mount-width 2) (+ (/ mount-height -2) extra-height 0.5) 0]
                    ;;                                  :scale-x 0.075 :scale-y 0.075 :position "bm" :z-rotation 0}))
                     ;(front-wall-polyhedron 60)
                      ;; (difference
                      ;;  (back-wall-polyhedron 60)
                      ;;  (usb-jack-place usb-jack-polyhedron))
                     ;(polyhedron-left-section 60)
                     ; (left-section-to-thumb-cluster-convex-walls 60)
                     ;(right-wall-polyhedron-catmull-rom-spline 60)
                     ;(left-section-to-thumb-cluster-convex-connecetors 60)
                    ;;  (rp2040-plus-place rp2040-plus-mount)
                      ;; (left-section-back 60)
                     ))))

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
        (front-wall-polyhedron 60)
        key-holes
        ;(back-left-wall-to-screen 60)
        ;(thumb-to-body-connecters-polyhedron 60)
        (thumb-connecters-polyhedron 60)
        ;(polyhedron-thumb-walls 60)
        (polyhedron-thumb-walls-for-convex-cluster 60)



        ;(key-place 1 2 dsa-cap)
        ;(left-section-to-thumb-cluster-convex-walls 60)
        ;(back-left-wall-to-screen 60)
        ;(screen-holder-place-side screen-holder)
       ;(left-section-to-thumb-cluster-convex-connecters 60)
        ;(right-side-polyhedron 60)
        ;(key-web-connecters-polyhedron 12)
        ;(thumb-connecters-polyhedron 12)
        ;key-holes
        ;thumb-type
         ;dsa-thumbcaps
        ;(import "../parts/top-left-surface.stl")
        )))

(spit "things-low/front-wall-polyhedron-test.scad"
      (write-scad (front-wall-polyhedron 60)))

  ;; (spit "things-low/left-front-test.scad"
  ;;       (write-scad
  ;;        (union
  ;;         ;; (difference
  ;;         ;;  (screen-holder-place-side screen-holder)
  ;;         ;;   (screen-holder-place-side screen-holder-cut))
  ;;         ;(tps-65-place tps-65-mount)
  ;;         (difference
  ;;          (left-section-front-polyhedron 60)
  ;;          (EVQWGD001-place EVQWGD001-main-cutout))
  ;;         ;(EVQWGD001-place EVQWGD001-holder)
  ;;         ;thumb-type
  ;;         ;(polyhedron-thumb-walls 60)
  ;;         (back-left-wall-to-screen 60)
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
;;           ;(right-side-polyhedron 60)
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
  ;;         [steps 60]
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
;;           (left-section-to-thumb-cluster-convex-connecters  60) 
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
;;         ;(right-side-polyhedron 60)
;;         (difference 
;;     (union
;;   aviator-assembly-polyhedron
;;    (left-section-back 60) 
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
                    (back-wall-polyhedron-catmull-rom 30)
                    (usb-jack-place usb-jack-polyhedron))
                   key-holes
                   (left-section-back 30)
                   (right-wall-polyhedron-catmull-rom-spline 30)
                   ;aviator-assembly-polyhedron
                   ;(rp2040-plus-place rp2040-plus-mount)
                   ;bottom-plate-for-polyhedron-model
                   )))

(spit "things-low/generate-bezier-along-bezier-polyhedron-all-side-test.scad"
      (let [steps 10
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
            ;; outer-points (into [] 
            ;;                    (apply concat 
            ;;                           (for [index (range 0 (inc steps))] 
            ;;                             (bezier-linear 
            ;;                              (nth top-outside index) 
            ;;                              (nth bottom-outside index) 
            ;;                              steps) 
            ;;                             )))
            ;; inner-points (into []
            ;;                    (apply concat
            ;;                           (for [index (range 0 (inc steps))]
            ;;                             (bezier-linear
            ;;                              (nth top-inside index)
            ;;                              (nth bottom-inside index)
            ;;                              steps))))
            ;; wall-left (wall-brace-polyhedron-curve-points (partial key-place 0 1) 0 -1 "bl" :radians steps)
            ;; wall-right (wall-brace-polyhedron-curve-points (partial key-place 0 1) 0 -1 "br" :radians steps)
            ;; wall-outer (into []
            ;;                  (apply concat
            ;;                         (for [index (range 0 (inc steps))]
            ;;                           (bezier-linear 
            ;;                            (nth (wall-right :outer-points) index)
            ;;                            (nth (wall-left :outer-points) index)
            ;;                            steps))))
            ;; wall-inner (into []
            ;;                  (apply concat
            ;;                         (for [index (range 0 (inc steps))
            ;;                               :let [left (reverse (wall-left :inner-points))
            ;;                                     right (reverse (wall-right :inner-points))]]
            ;;                           (bezier-linear 
            ;;                            (nth right index)
            ;;                            (nth  left index) 
            ;;                            steps))))
            ;; spline-test (cubic-hermite-spline-curve-segment [0 0 0] [1 1 1] [1 0 0] [0 1 0] 20)
            ;; curve-1 (bezier-quadratic [-2 2 0] [-2 0 0] [-1 0 0] 20)

            ;; test-m [[4 7]
            ;;         [2 6]]
            ;; test-m-inv (/ 1 (- (* (nth (nth test-m 0) 0) (nth (nth test-m 0) 0))))
            ;; basis-inv (matrix-inverse [[(/ -3  1) 0 (/ 3 1) 0 0]
            ;;                            [1 4 1 0 0]
            ;;                            [0 1 4 1 0]
            ;;                            [0 0 1 4 1]n
            ;;                            [0 0 (/ -3 1) 0 (/ 3 1)]])
            ;; basis (generate-basis-matrix-to-find-cubic-uniform-b-spline-points-from-knots 2)
            ;; basis-2 (calculate-non-vanishing-basis-functions 3
            ;;                                                  1 3.0 [0 0 0 0 1 1 1 1])
             ;nurbs-test   (nurbs-with-homogenous-coordinates [[0 0 0 1] [0 1 0 1] [5 0 0 5] [2 1 0 1] [2 0 0 1]] 2 [0 0 0 1 2 3 3 3] 20)
            nurbs-test   (nurbs [[0 0 0] [0 1 0] [1 0 0] [2 1 0] [2 0 0]] 2 [0 0 0 1 2 3 3 3] [1 1 5 1 1] 20)
            ;; nurps-segment-1 (nurbs-segment 4 2 [0 0 0 1 2 3 3 3] [[0 0 0 1] [0 1 0 1] [1 0 0 1]] 20)i.
            ;;  nurps-segment-2 (nurbs-segment 4 2 [0 0 0 1 2 3 3 3] [[0 0 0 1] [0 1 0 1] [1 0 0 1] [2 1 0 1] [2 0 0 1]] 20 :u-start 1.0)
            u-k (u-k-chordal 4 [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]])
            ;; knot-vec (calcula-knot-vector-from-u-k u-k 4 3)

            ; nurbs-test-size (count nurbs-test)
            ;nurbs-test (mapv #(subvec (coerce :persistent-vector %) 0 3) (b-spline-wrapper [[0 0 0 1] [0 1 0 1] [1 0 1 0.5] [2 1 2 1] [2 0 2 1]] 2 [0 0 0 (/ 1 3) (/ 2 3) 1 1 1] false 11))
            cu (wall-brace-polyhedron-curve-points (partial key-place lastcol cornerrow) 0 -1 "bl" :radians 60)
            pp (wall-brace-polyhedron-points (partial key-place lastcol cornerrow) 0 -1 "bl" :radians)
            bb (wall-brace-polyhedron-points (partial key-place lastcol cornerrow) -1 0 "bl" :radians)
            test-control-points (wall-brace-polyhedron-points (partial key-place lastcol cornerrow) -1 0 :tl :radians)
            pp-u-k (u-k-centripetal 5 [(pp :web-post-position-top) (pp :point-on-tangent-from-plate) (pp :wall-locate-1-to-3-curve-for-polyhedron-control-point)
                                       (pp :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (pp :wall-locate3-point) (pp :wall-locate3-point-floor)])
            pp-knot-vector (calculate-averaged-knot-vector-from-u-k pp-u-k 5 3)
           ;  nn (nurbs-with-calculated-knot-vector (pp ))
            vnf (vnf-vertex-array [[[1 0 0] [10 0 0] [10 10 0] [1 10 0]]
                                   [[1 0 2] [10 0 2] [10 10 2] [1 10 2]]
                                   [[1 0 4] [10 0 4] [10 10 4] [1 10 4]]
                                   [[1 0 6] [10 0 6] [10 10 6] [1 10 6]]
                                   [[1 0 8] [10 0 8] [10 10 8] [1 10 8]]
                                   [[1 0 10] [10 0 10] [10 10 10] [1 10 10]]] :reverse true)
            Q-global-test [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]]
            global-test (global-curve-interp Q-global-test  3 :point-paramater-calculation-method :centripetal)
            global-test-spline (non-uniform-b-spline (global-test :P) 3 (global-test :U) 30)
            global-test-deriv (global-curve-interp-with-end-derivatives Q-global-test  3 [0 0 0] [-2 -3 0] :point-paramater-calculation-method :chordal)
            global-test-deriv-spline (non-uniform-b-spline (global-test-deriv :P) 3 (global-test-deriv :U) 30)
            local-inte (local-cubic-curve-interpolation-with-calculated-tangents  [[0 0 0] [3 4 0] [-1 4 0] [-4 0 0] [-4 -3 0]])
            local-spline (non-uniform-b-spline (local-inte :P) 3 (local-inte :U) 30)
            local-spline-uni (cubic-uniform-b-spline-through-terminal-endpoints (local-inte :P) 60)
            pp-global-test (global-curve-interp [(pp :web-post-position-top) (pp :point-on-tangent-from-plate)
                                                 (pp :wall-locate-1-to-3-curve-for-polyhedron-control-point)
                                                 (pp :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (pp :wall-locate3-point)
                                                 (pp :wall-locate3-point-floor)] 3)

            pp-global-test-end-deriv (global-curve-interp-with-end-derivatives [(pp :web-post-position-top) (pp :point-on-tangent-from-plate)
                                                                                (pp :wall-locate-1-to-3-curve-for-polyhedron-control-point)
                                                                                (pp :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (pp :wall-locate3-point)
                                                                                (pp :wall-locate3-point-floor)] 3 (mapv - (pp :web-post-position-top) (pp :opposite-web-post-position-top))
                                                                               (mapv - (pp :wall-locate3-point-below-floor) (pp :wall-locate3-point-floor)) :point-paramater-calculation-method :chordal)
            ;b-surface (bilinear-surface [0 0 1] [1 0 0] [1 1 1] [0 1 0] 30)
            q-surface    (b-spline-surface 3 3 3 2  [0 0 0 0 1 1 1 1] (mapv (partial * 2) [0 0 0 0.5 1 1 1])
                                           [[0 0 0] [0 4 0] [0 8 -3] [0 10 -3]
                                            [2 0 6] [2 4 0] [2 8 0] [2 10 0]
                                            [4 0 0] [4 4 0] [4 8 3] [4 10 3]
                                            [6 0 0] [6 4 -3] [6 8 0] [6 10 0]
                                            [8 0 0] [8 4 -3] [8 8 0] [8 10 0]]
                                           10)
            ;; (bicubic-uniform-b-spline-surface [[0 0 0] [0 1 0] [0 2 0] [0 3 0]
            ;;                                              [1 0 0] [1 1 1] [1 2 0] [1 3 0]
            ;;                                              [2 0 0] [2 1 0] [2 2 0] [2 3 0]
            ;;                                              [3 0 0] [3 1 0] [3 2 0] [3 3 0]] 10) 
            ;(biquadratic-uniform-b-spline-surface [[0 0 0] [0 1 0] [0 2 0] [1 0 0] [1 1 1] [1 2 0] [2 0 0] [2 1 0] [2 2 0]] 10) 
            ;; (sixteen-point-bicubic-surface-patch [[0 0 0] [1 0 0] [2 0 0] [3 0 0]
            ;;                                                 [0 1 0] [1 1 1] [2 1 0.5] [3 1 0]
            ;;                                                 [0 2 -0.5] [1 2 0] [2 2 0.5] [3 2 0]
            ;;                                                 [0 3 0] [1 3 0] [2 3 0] [3 3 0]] 10) 
            ;(nine-point-biquadratic-surface-patch [0 0 0] [1 0 0] [2 0 0] [0 1 0] [1 1 1] [2 1 -0.5] [0 2 0] [1 2 0] [2 2 0] 10)
            post-position :bl
            web-corner-translation-vector (get-single-plate-corner-position-vector post-position)
            top-position (mapv + [0 0 10] (get-web-post-position-top web-corner-translation-vector))
            web-post-position-top (vec  top-position)
            dx 0
            dy -1
            wall-locate-1-to-3-curve-for-polyhedron-control-point-point   (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx dy top-position)  (get-curve-post-outer-x-and-y-vector dx dy))
            wall-locate-1-to-3-curve-for-polyhedron-second-control-point  (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx dy top-position) (get-curve-post-outer-x-and-y-vector dx dy))
            wall-locate3-point   (mapv + (wall-locate3-for-polyhedron-point dx dy wall-xy-offset) top-position (get-curve-post-outer-x-and-y-vector dx dy))
            point-at-floor-from-tangent-from-wall-locate-1-to-3-curve-for-polyhedron-second-control-point-to-wall-locate3-point (find-point-on-line-using-z wall-locate-1-to-3-curve-for-polyhedron-second-control-point
                                                                                                                                                            wall-locate3-point
                                                                                                                                                            0)
            wall-locate3-point-assoc (assoc (vec wall-locate3-point) 2 0)
            wall-locate3-point-floor (let [x-coord (if (zero? dx) (nth point-at-floor-from-tangent-from-wall-locate-1-to-3-curve-for-polyhedron-second-control-point-to-wall-locate3-point 0) (nth wall-locate3-point 0))
                                           y-coord (if (zero? dy) (nth point-at-floor-from-tangent-from-wall-locate-1-to-3-curve-for-polyhedron-second-control-point-to-wall-locate3-point 1) (nth wall-locate3-point 1))]
                                       [x-coord y-coord 0.0]);(assoc (vec wall-locate3-point) 2 0)
            bezier-points [[0 10 0] [5 15 0] [7.5 20 0] [10 15 0] [12.5 20 0] [15 15 0] [20 10 0]]]

        (write-scad
         (include "../BOSL2/std.scad")
         (color [1 0 0 1] (key-place 2 0  sphere-post-tl))
         


         (plot-bezier-points (apply  bezier-sextic (conj bezier-points 10)) (sphere 0.5))
         (-# (plot-bezier-points (n-degree-bezier-curve bezier-points 10) (cube 0.5 0.5 0.5)))
         (translate [0 0 10] single-plate)
         (translate web-post-position-top (cube 0.5 0.5 0.5))
         (translate wall-locate-1-to-3-curve-for-polyhedron-control-point-point (cube 0.5 0.5 0.5))
         (translate wall-locate-1-to-3-curve-for-polyhedron-second-control-point (cube 0.5 0.5 0.5))
         (color [1 0 0 1] (translate wall-locate3-point (cube 0.5 0.5 0.5)))

         (translate wall-locate3-point-floor (cube 0.5 0.5 0.5))
         (color [0 1 0 1] (translate point-at-floor-from-tangent-from-wall-locate-1-to-3-curve-for-polyhedron-second-control-point-to-wall-locate3-point (cube 0.5 0.5 0.5)))
      ; (wall-brace-polyhedron  (partial key-place 0 1) 0 -1 "bl" :radians (partial key-place 0 1) 0 -1 "br" :radians wall-xy-offset wall-xy-offset true steps)
        ;;  (wall-brace-quadratic-polyhedron (partial key-place 1 cornerrow) 1 -0.1 "br" :radiansn
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
        ;;  (wall-brace-catmull-rom-spline local-cubic-curve-interpolation-with-calculated-tangents
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
        ;;   stelocal-cubic-curve-interpolation-with-calculated-tangentslocal-cubic-curve-interpolation-with-calculated-tangentslocal-cubic-curve-interpolation-with-calculated-tangentsps
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
         ;(plot-bezier-points (cubic-uniform-b-spline [[nhrough-terminal-endpoints [[0 0 0] [0 1 0] [1 1 0] [2 1 0] [2 0 0]] 40) (sphere 0.05))
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
          ;(plot-bezier-points (kochanek-bartels-spline-curve [[-1 -1 0] [0 0 0]  [4 6 0] [10 -1 0] [11 -2 0]] 40 :tension-values [0 -1 0] :continuinity-values [0 0 0] :bias-values [0 0 0] ) (sphere 0.05))i
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
        ;; (println nurbs-test)
         (for [index (range 0 (count nurbs-test))]
           (color [1 (/ index (count nurbs-test)) 0 1] (translate (nth nurbs-test index) (sphere 0.05))))
         (plot-bezier-points (vec (apply concat q-surface)) (sphere 0.1))
         (println "u-k is " u-k)
         (println "u-k knot vec is" (calculate-averaged-knot-vector-from-u-k u-k 4 3))
         (vnf-polyhedron
          (vnf-vertex-array q-surface :caps false :col-wrap false :row-wrap false :reverse false :style :default) 1)
        ;; (println "knot-vector is " knot-vec)
         ;(plot-bezier-points nurbs-test (sphere 0.05)) 
        ;; (doseq [index (range 0 5)
        ;;         :let [uk-i (nth u-k index)
        ;;               span (calculate-knot-span-index 4 3 (nth u-k index) knot-vec)
        ;;               start-index (- span 3)
        ;;               basis-funs (calculate-non-vanishing-basis-functions span uk-i 3 knot-vec)
        ;;               basis-funs-size (count basis-funs)]]
        ;;   (println "index is " index " span is " span)
        ;;   (println "row  is " (into [] (concat (repeat start-index 0.0) basis-funs (repeat (- 5 (+ basis-funs-size start-index)) 0.0)))))
         ;(-# (translate [0 0 -10] (cube 200 200 20)))
         (key-place lastcol cornerrow single-plate)
         ;(key-wall-brace-polyhedron lastcol cornerrow 0 -1 "bl" lastcol cornerrow 0 -1 "br" :steps steps)
         ;(print (count (cu :outer-points)))
         ;(plot-bezier-points (cu :outer-points) (sphere 0.1))
         ;(plot-bezier-points (cu :inner-points) (sphere 0.1))
        ;;  (for [[k v] pp]
        ;;    ;(println v)
         

        ;;    (do (println (vec v)) 
        ;;      (translate (vec v) (if (= k :point-on-tangent-from-plate ) (color [1 0 0 1] (sphere 0.1))
        ;;                     (sphere 0.1))))
        ;;    )
         (println "pp-u-k is " pp-u-k)
         (println "pp-knot-vector is " pp-knot-vector)
         (translate (bb :web-post-position-top)   (cube 0.5 0.5 0.5)) 
         (color [1 0 0 1] (translate (bb :point-on-tangent-from-plate)   (sphere 0.5)))
         (color [1 0 0 1] (translate (bb :wall-locate-1-to-3-curve-for-polyhedron-control-point)   (cube 0.5 0.5 0.5)))
         (translate (bb :web-post-position-top)   (cube 0.5 0.5 0.5))
         (translate (pp :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)   (cube 0.5 0.5 0.5)) 
         (translate (bb :wall-locate3-point)   (cube 0.5 0.5 0.5))
         (translate (bb :wall-locate3-point-floor)   (cube 0.5 0.5 0.5))

         (translate (pp :wall-locate3-point)  (sphere 0.5))
(translate (pp :wall-locate3-point-floor)   (sphere 1 ))
         (color [1 0 0 1] (translate (pp :point-on-tangent-from-plate)   (sphere 0.5)))
;(color [0 1 0 1] (translate (pp :wall-locate1-point)   (sphere 0.1)))
;-(color [0 0 1 1] (translate (pp :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)   (cube 0.5 0.5 1)))
         (color [1 0 0 1] (translate (pp :wall-locate-1-to-3-curve-for-polyhedron-control-point)   (cube 0.5 0.5 0.5)))
;(translate (pp :wall-locate3-point)   (cube 0.5 0.5 0.5))
;(translate (pp :wall-locate3-point-floor)   (cube 0.5 0.5 0.5))
         ;(chained-hull (plot-bezier-points (non-uniform-b-spline (pp-global-test :P) 3 (pp-global-test :U) 30) (sphere 0.2)))
         ;(color [1 0 0 1](chained-hull (plot-bezier-points (non-uniform-b-spline (pp-global-test-end-deriv :P) 3 (pp-global-test-end-deriv :U) 30) (sphere 0.2))))
;;          (color [0 1 0 1] (chained-hull (plot-bezier-points [(pp :web-post-position-top) (pp :point-on-tangent-from-plate)
;; (pp :wall-locate-1-to-3-curve-for-polyhedron-control-point)
;; (pp :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (pp :wall-locate3-point)
;; (pp :wall-locate3-point-floor)] (cube 0.5 0.5 0.5))))
         (plot-bezier-points  (bezier-quintic (pp :web-post-position-top) (pp :wall-locate1-point) (pp :wall-locate-1-to-3-curve-for-polyhedron-control-point)
                                              (pp :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (pp :wall-locate3-point)
                                              (pp :wall-locate3-point-floor) 30)
                              (sphere 0.5))
        ;;  (chained-hull (plot-bezier-points [(pp :opposite-web-post-position-top)
        ;;                                     (pp :web-post-position-top)
        ;;                                     (assoc (find-point-on-line-using-y (pp :web-post-position-top) (pp :point-on-tangent-from-plate) (nth (pp :wall-locate-1-to-3-curve-for-polyhedron-control-point) 1)) 2 (nth (pp :wall-locate-1-to-3-curve-for-polyhedron-control-point) 2))] 
        ;;                                    (cube 0.5 0.5 0.1)))
         (plot-bezier-points (outer-wall-curve-nurbs test-control-points 30) (sphere 0.1))
         ;(color [0 0 1 1](plot-bezier-points (outer-wall-curve-bezier-quintic test-control-points 30) (sphere 0.1)))

         (color [1 1 0 1] (plot-bezier-points (nurbs-with-calculated-knot-vector [(pp :web-post-position-top) (pp :point-on-tangent-from-plate)
                                                                                  (pp :wall-locate-1-to-3-curve-for-polyhedron-control-point)
                                                                                  (pp :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (pp :wall-locate3-point) (pp :wall-locate3-point-floor)]
                                                                                 3 [1 0.9 0.8 0.6 0.75 1] 50 :style :centripetal) (sphere 0.2)))
         (println "homogenize-cooridinates " (homogenize-cooridinates [[0 0 0] [3 3 3] [3 3 3]] [1 1 0.5]))
        ; (println "strrrrrtarr " (vec (for [coordinate ] (if (not= coordinate 3) (* coordinate weight) coordinate))))
        ;(println (nurbs-with-calculated-knot-vector [[0 0 0] [0 1 0] [1 2 0] [2 4 0] [3 1 2] [5 -4 8] [8 0 2]] 3 [1 1 1 1 1 1 1] 30))
        ;;  (generate-polyhedron (concat (into [] (apply concat (for [index (range (inc steps))]
        ;;                                                        (bezier-linear
        ;;                                                         (nth top-outside index)
        ;;                                                         (nth bottom-outside index)
        ;;                                                         8)
        ;;                                                        )))
        ;;                               (into [] (apply concat 
        ;;                                               (for [index (range (inc steps))
        ;;                                                     :let [top (reverse top-inside)
        ;;                                                           bottom (reverse bottom-inside)]]
        ;;                                                 (bezier-linear
        ;;                                                  (nth top-inside index)
        ;;                                                  (nth bottom-inside index)
        ;;                                                  8))))) 

        ;;                       (inc 8) (inc steps)
        ;;                       )



         ;(color [1 0 0 1] (plot-bezier-points (cubic-uniform-b-spline [(pp :opposite-web-post-position-top) (pp :web-post-position-top) (pp :wall-locate3-point-floor) (pp :wall-locate3-point-below-floor)] 50) (sphere 0.2)))
         (color [0.3 0 1 1] (plot-bezier-points (cubic-uniform-b-spline-through-terminal-endpoints [(pp :web-post-position-top) (pp :point-on-tangent-from-plate)
                                                                                                    (pp :wall-locate-1-to-3-curve-for-polyhedron-control-point)
                                                                                                    (pp :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (pp :wall-locate3-point) (pp :wall-locate3-point-floor)] 50) (sphere 0.2)))
         ;(chained-hull (plot-bezier-points (catmull-rom-spline-curve [(pp :opposite-web-post-position-bottom) (pp :web-post-position-bottom) (pp :point-on-tangent-from-plate-bottom) (pp :wall-locate-2-bottom) (pp :wall-locate-2-bottom-floor) (pp :wall-locate-2-bottom-below-floor)] 30 :alphaType :uniform) (cube 0.5 0.5 0.5)))
         (chained-hull (plot-bezier-points [(pp :opposite-web-post-position-bottom) (pp :web-post-position-bottom) (pp :point-on-tangent-from-plate-bottom) (pp :wall-locate-2-bottom) (pp :wall-locate-2-bottom-floor)] (cube 0.5 0.5 0.5)))
         ;(color [1 0 0 1](chained-hull (plot-bezier-points (catmull-rom-spline-curve-matrix [(pp :opposite-web-post-position-bottom) (pp :web-post-position-bottom) (pp :point-on-tangent-from-plate-bottom) (pp :wall-locate-2-bottom) (pp :wall-locate-2-bottom-floor) (pp :wall-locate-2-bottom-below-floor)] 30) (sphere 0.1))))
         ;( println "catmull-rom-spline-curve-matrix " (catmull-rom-spline-curve-matrix [(pp :opposite-web-post-position-bottom) (pp :web-post-position-bottom) (pp :point-on-tangent-from-plate-bottom) (pp :wall-locate-2-bottom) (pp :wall-locate-2-bottom-floor) (pp :wall-locate-2-bottom-below-floor)] 30) )
         (color [1 0 0 1]  (plot-bezier-points (catmull-rom-spline-as-bezier-cubic [(pp :opposite-web-post-position-bottom) (pp :web-post-position-bottom) (pp :point-on-tangent-from-plate-bottom) (pp :wall-locate-2-bottom) (pp :wall-locate-2-bottom-floor) (pp :wall-locate-2-bottom-below-floor)] 10) (sphere 0.1)))

        ;(color [1 0 0 1] (plot-bezier-points (bezier-cubic (pp :web-post-position-bottom) (pp :wall-locate-2-top) (pp :wall-locate-2-bottom) (pp :wall-locate-2-bottom-floor)  30) (sphere 0.1)))
        ;(color [1 0 1 1] (plot-bezier-points (cubic-uniform-b-spline [(pp :opposite-web-post-position-bottom) (pp :web-post-position-bottom) (pp :wall-locate-2-top) (pp :wall-locate-2-bottom) (pp :wall-locate-2-bottom-floor) (pp :wall-locate-2-bottom-below-floor)]  30) (sphere 0.1)))
         (color [1 0 0 1] (translate (pp :point-on-tangent-from-plate-bottom) (cube 0.5 0.5 0.5)))
         (color [1 1 0 1] (translate (pp :wall-locate-2-top) (sphere 0.5)))
         (plot-bezier-points [(pp :web-post-position-bottom)  (pp :wall-locate-2-top)
                              (pp :wall-locate-2-bottom) (pp :wall-locate-2-bottom-floor)] (cube 0.5 0.5 0.5))



         (color [1 0.2 0.5 1] (plot-bezier-points
                               (nurbs-with-calculated-knot-vector [(pp :web-post-position-bottom) (pp :point-on-tangent-from-plate-bottom) (pp :wall-locate-2-top)
                                                                   (pp :wall-locate-2-bottom) (pp :wall-locate-2-bottom-floor)]
                                                                  4 [1 0.9 0.6 0.75 1] 30) (sphere 0.1)))
         (color [1 1 1 1] (plot-bezier-points (kochanek-bartels-spline-curve [(pp :opposite-web-post-position-bottom) (pp :web-post-position-bottom)
                                                                              (pp :wall-locate-2-bottom) (pp :wall-locate-2-bottom-floor) (pp :wall-locate-2-bottom-below-floor)] 30
                         :bias-values [0 0 0 0 0 0]) (sphere 0.1)))
         (debug-vnf vnf)

         (color [1 0 1 1](plot-bezier-points (nurbs [[20 0 0] [20 20 0] [0 20 0] [-20 20 0] [-20 0 0]] 2 [0 0 0 1.5 1.5 3 3 3] [1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1]  30) (sphere 0.5)))
         ;(color [1 0 0 1](plot-bezier-points Q-global-test (sphere 0.6)))
         ;(plot-bezier-points global-test-spline (sphere 0.5))
         ;(color [0 0 1 1](plot-bezier-points global-test-deriv-spline  (sphere 0.5)))
         ;(color [0 1 0 1](plot-bezier-points (global-test :P) (sphere 0.5)))
         ;(color [1 0 1 0] (plot-bezier-points (global-test-deriv :P) (sphere 0.5)))
         ;(color [1 1 0 1] (plot-bezier-points local-spline-uni (sphere 0.5)))

        ;;  (for [index (range 0 (count (local-inte :P)) )]
        ;;    (color [0 (/ index (count (local-inte :P))) 0 1](translate (nth (local-inte :P) index) (cylinder 0.8 0.8 )))
        ;;    )
        ; (color [0.5 1 0.5] (plot-bezier-points local-spline (sphere 0.5)))
        ;;  (color  [0.5 1 0.5] (plot-bezier-points (local-inte :P) (sphere 0.8)))
         (println " (global-test :P)" (global-test :P))
         (println " (global-test :U)" (global-test :U))
         (println "global-test-spline" global-test-spline)
         (println "global-test-deriv-spline " global-test-deriv-spline)
         (println "global-test-deriv :P" (global-test-deriv :P))
         (println "global-test-deriv :U" (global-test-deriv :U))
         (println "(local-inte :U)" (local-inte :U))
         (println "(local-inte :P)" (local-inte :P))
         (println "local-spline" local-spline))))


(spit "things-low/wall-section-test.scad"
      (write-scad
       (include "../BOSL2/std.scad")
       (union
        key-holes


        (vnf-polyhedron (wall-vnf (wall-section (wall-section-parameter
                                                 [;(wall-cross-section-parameter (key-wall-position 3 2 1 -1 :bl))
                                                  ;(wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bm))
                                                  ;(wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :br))
                                                  (wall-cross-section-parameter (key-wall-position 1 2 1 0 :br  :slant :no-slant :xy 3))
                                                  (wall-cross-section-parameter (key-wall-position 2 2 1 -1 :bl  :slant :no-slant :xy 3))
                                                  (wall-cross-section-parameter (key-wall-position 2 2 0 -1 :bm :slant :no-slant :xy 3))
                                                  ;(wall-cross-section-parameter (key-wall-position lastcol 2 -1 -1 :bl :xy 5 :offset [-0.1 -0.1 0]))
                                                  (wall-cross-section-parameter (key-wall-position 2 2 -1 -1 :br :slant :no-slant :xy 3))
                                                  (wall-cross-section-parameter (key-wall-position 3 2 -1 0 :bl :slant :no-slant :xy 3))

                                                    ;(wall-cross-section-parameter (key-wall-position lastcol 2 1 -1 :br))
                                                  ]
                                                 (one-eighty-degree-arc-nurbs-parameter))
                                                30 30)
                                  {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
        (vnf-polyhedron
         (wall-vnf (wall-section (wall-section-parameter
                                  [;(wall-cross-section-parameter (key-wall-position 3 2 1 -1 :bl))
                                                  ;(wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bm))
                                                  ;(wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :br))
                                   (wall-cross-section-parameter (key-wall-position 3 2 -1 0 :bl :offset [-0.000000000001 0 0] :slant :no-slant  :xy 3))
                                                  ;(wall-cross-section-parameter (key-wall-position lastcol 2 -1 -1 :bl :xy 5 :offset [-0.1 -0.1 0]))
                                   (wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :bl :slant :no-slant :xy 3))
                                   (wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bl :offset [0 -0.000000000001 0] :xy 5))
                                                    ;(wall-cross-section-parameter (key-wall-position lastcol 2 1 -1 :br))
                                   ]
                                  (local-cubic-curve-interpolation-with-calculated-tangents-parameter))
                                 30 30)
                   {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
        ;; (vnf-polyhedron (wall-vnf (wall-section (wall-section-parameter
        ;;                                          [;(wall-cross-section-parameter (key-wall-position 3 2 1 -1 :bl))
        ;;                                           ;(wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bm))
        ;;                                           ;(wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :br))
        ;;                                           (wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bl :xy 4))
        ;;                                           (wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :br  :xy 4))
        ;;                                           ;(wall-cross-section-parameter (key-wall-position lastcol 2 -1 -1 :bl :xy 5 :offset [-0.1 -0.1 0]))
        ;;                                           (wall-cross-section-parameter (key-wall-position lastcol 2 -1 0 :bl :slant :no-slant))

        ;;                                             ;(wall-cross-section-parameter (key-wall-position lastcol 2 1 -1 :br))
        ;;                                           ]
        ;;                                          (ninety-degree-arc-nurbs-parameter))
        ;;                                         30 30)
        ;;                           {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
        ;; (vnf-polyhedron
        ;;  (wall-vnf (wall-section (wall-section-parameter
        ;;                           [
        ;;                            (wall-cross-section-parameter (key-wall-position lastcol 2 -1 0 :bl :offset [-0.000000000001 0 0] :slant :no-slant))
        ;;                             (wall-cross-section-parameter (key-wall-position lastcol 2 -1 -1 :bl :slant :no-slant))
        ;;                            (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :bl :offset [0 -0.000000000001 0]))
        ;;                            ]
        ;;                           (ninety-degree-arc-nurbs-parameter))
        ;;                          30 30)
        ;;            {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
        (vnf-polyhedron (wall-vnf (wall-section (wall-section-parameter
                                                 [;(wall-cross-section-parameter (key-wall-position 3 2 1 -1 :bl))
                                                  ;(wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bm))
                                                  ;(wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :br))

                                                  ;(wall-cross-section-parameter (key-wall-position lastcol 2 -1 -1 :bl :xy 5 :offset [-0.1 -0.1 0])) 
                                                  (wall-cross-section-parameter (key-wall-position lastcol 2 1 -1 :br :offset [-0.01 -0.1 0] :xy 3))
                                                    ;(wall-cross-section-parameter (key-wall-position lastcol 2 1 -1 :br))
                                                  (wall-cross-section-parameter (key-wall-position lastcol 2 1 1 :br))
                                                  (wall-cross-section-parameter (key-wall-position lastcol 2 1 0 :tr))
                                                  (wall-cross-section-parameter (key-wall-position lastcol 1 1 0 :br))
                                                  (wall-cross-section-parameter (key-wall-position lastcol 1 1 0 :tr))
                                                  (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :br))
                                                  (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :tr))]
                                                    ;; (vec (apply concat 
                                                    ;;                                    (for [index (range 0 (inc cornerrow))]
                                                    ;;                          [(wall-cross-section-parameter (key-wall-position lastcol (- cornerrow index) 1 0 :br))
                                                    ;;                           (wall-cross-section-parameter (key-wall-position lastcol (- cornerrow index) 1 0 :tr))
                                                    ;;                           ])))
                                                 (catmull-rom-spline-parameters :alpha :chordal))
                                                30 30)
                                  {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
        (vnf-polyhedron (wall-vnf (wall-section (wall-section-parameter
                                                 (vec (apply concat
                                                             (for [index (range 0 (inc lastcol))
                                                                   :let [col (- lastcol index)]]
                                                               [(wall-cross-section-parameter (key-wall-position col 0 0 1 :tr))
                                                                (wall-cross-section-parameter (key-wall-position col 0 0 1 :tl))])))
                                                    ;; (vec (apply concat 
                                                    ;;                                    (for [index (range 0 (inc cornerrow))]
                                                    ;;                          [(wall-cross-section-parameter (key-wall-position lastcol (- cornerrow index) 1 0 :br))
                                                    ;;                           (wall-cross-section-parameter (key-wall-position lastcol (- cornerrow index) 1 0 :tr))
                                                    ;;                           ])))
                                                 (catmull-rom-spline-parameters :alpha :centripetal :linear-outer-top true :linear-inner-top true))
                                                30 30)
                                  {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
        (vnf-polyhedron
         (wall-vnf (wall-section (wall-section-parameter
                                  [;(wall-cross-section-parameter (key-wall-position 3 2 1 -1 :bl))
                                                  ;(wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bm))
                                                  ;(wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :br))

                                   (wall-cross-section-parameter (key-wall-position lastcol 2 1 0 :tr))
                                   (wall-cross-section-parameter (key-wall-position lastcol 2 1 0 :br :offset [-0.000000000001 0 0] :slant :no-slant))
                                   (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :br))
                                   (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :bl)) 
                                   (wall-cross-section-parameter (key-wall-position lastcol 2 -1 0 :bl :offset [-0.000000000001 0 0] :slant :no-slant)) 
                                   (wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :br :offset [0 -0.000000000001 0]))
                                   (wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bm))
                                   (wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bl :offset [0 -0.000000000001 0]))
                                   (wall-cross-section-parameter (key-wall-position 3 2 -1 0 :bl :offset [-0.000000000001 0 0]))


                                   ;(wall-cross-section-parameter (key-wall-position lastcol 2 -1 -1 :bl :xy 5 :offset [-0.1 -0.1 0]))




                                                    ;(wall-cross-section-parameter (key-wall-position lastcol 2 1 -1 :br))
                                   ]
                                  (catmull-rom-spline-parameters))
                                 30 30)
                   {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse true :style :default}))
        

         (vnf-polyhedron
          (wall-vnf (wall-section (wall-section-parameter
                                   [;(wall-cross-section-parameter (key-wall-position 3 2 1 -1 :bl))
                                                  ;(wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bm))
                                                  ;(wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :br))
                                    
                                    
                                    (wall-cross-section-parameter (key-wall-position 1 2 0 -1 :br))
                                    (wall-cross-section-parameter (key-wall-position 1 2 0 -1 :bl))
                                    (wall-cross-section-parameter (key-wall-position 0 2 0 -1 :br))
                                    (wall-cross-section-parameter (key-wall-position 0 2 0 -1 :bl))
                      


                                   ;(wall-cross-section-parameter (key-wall-position lastcol 2 -1 -1 :bl :xy 5 :offset [-0.1 -0.1 0]))




                                                    ;(wall-cross-section-parameter (key-wall-position lastcol 2 1 -1 :br))
                                    ]
                                   (local-cubic-curve-interpolation-with-calculated-tangents-parameter))
                                  30 30)
                    {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse true :style :default}))
        ;; (vnf-polyhedron
        ;;  (wall-vnf (wall-section (wall-section-parameter
        ;;                           [
        ;;                           (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :bl))
        ;;                            (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :br)) 
        ;;                            ]
        ;;                           (n-degree-bezier-curve-paramaters))
        ;;                          30 30)
        ;;            {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
        ;; (vnf-polyhedron   (wall-vnf (wall-section (wall-section-parameter
        ;;                                            [(wall-cross-section-parameter (key-wall-position 3 2 1 -1 :bl)) 
        ;;                                             (wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bm))
        ;;                                             (wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :br))
        ;;                                             (wall-cross-section-parameter (key-wall-position lastcol 2 -1 0 :bl))
        ;;                                             ;(wall-cross-section-parameter (key-wall-position lastcol 2 -1 -1 :bl))
        ;;                                             (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :bl))
        ;;                                             (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :br))
        ;;                                             ;(wall-cross-section-parameter (key-wall-position lastcol 2 1 -1 :br))
        ;;                                             (wall-cross-section-parameter (key-wall-position lastcol 2 1 0 :br))
        ;;                                             (wall-cross-section-parameter (key-wall-position lastcol 2 1 0 :tr))
        ;;                                             (wall-cross-section-parameter (key-wall-position lastcol 1 1 0 :br))
        ;;                                             (wall-cross-section-parameter (key-wall-position lastcol 1 1 0 :tr))
        ;;                                             (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :br))
        ;;                                             (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :tr))]
        ;;                                             ;; (vec (apply concat 
        ;;                                             ;;                                    (for [index (range 0 (inc cornerrow))]
        ;;                                             ;;                          [(wall-cross-section-parameter (key-wall-position lastcol (- cornerrow index) 1 0 :br))
        ;;                                             ;;                           (wall-cross-section-parameter (key-wall-position lastcol (- cornerrow index) 1 0 :tr))
        ;;                                             ;;                           ])))
        ;;                                            (catmull-rom-spline-parameters :alpha :chordal :linear-outer-top true :linear-inner-top true))
        ;;                                           60 60)
        ;;                             {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
        )))

(spit "things-low/tangent-test.scad"
      (let [interp (local-cubic-curve-interpolation [[0 5 0] [5 0 0] [5 -2 0] [10 -2 0]] [[1.25 0 0] [0 -1.25 0] [0.5 0 0] [1 0 0]])
       curve (non-uniform-b-spline (:P interp) 3 (:U interp) 30) 
            points [[-2 0 0] [0 0 0] [0 2 0] [2 0 0] [2 2 0]]
            weights [1 1 (/ (sqrt 2) 2) 1 1]
            knot-vector [0 0 0.75 0.75 2.25 2.25 3 3]
            nurbs-curve (nurbs-with-calculated-knot-vector points 2 weights 30)
            ](write-scad
       (union
        (plot-bezier-points nurbs-curve (sphere 0.1))
        ))))


(spit "things-low/catmull-rom-to-nurbs-conversion-test.scad"
      (write-scad
       (let [points [[-4 -8 0] [-2 0 0] [2 0 0] [4 8 0]]
            steps 20
            catmull-rom (catmull-rom-spline-curve points steps :alphaType :uniform)
            nurbs (apply catmull-rom-segment-to-cubic-nurbs (conj points steps :alphaType :uniform))]
        (union
         (-# (chained-hull (plot-bezier-points catmull-rom (binding [*fn* 18](sphere 0.1)))))
         (chained-hull (plot-bezier-points nurbs (binding [*fn* 18] (sphere 0.1))))
         (plot-bezier-points points (cube 0.2 0.2 0.2)))
        ))
      )


(def dat-orig [[94.559095 145.418507] [91.244778 139.706599]
          [213.627717 86.501225] [211.688487 97.396163]
          [95.405304 144.924887] [95.54634 145.13644] [95.475822 145.453768] [95.229012 145.594801]
          [210.595467 71.622056] [210.595468 50.255285]
          [92.196762 56.989695] [92.196762 49.514851]
          [153.123792 135.546073] [153.511637 135.546072] [153.828965 135.228744] [153.828964 134.840899]
          [231.186547 90.273904] [231.327583 89.886062] [231.080772 89.568733] [230.728185 89.498213]
          [148.892748 49.514851] [102.457044 49.514851]
          [105.524551 146.652563] [106.264983 146.158939] [107.040675 145.700576] [107.816364 145.277472]
          [214.438665 72.29197] [210.595467 71.622056]
          [207.281151 119.468107] [207.563221 119.503364] [207.739512 119.785435] [207.668995 120.032246]
          [81.724929 49.51485] [81.301826 49.514851] [80.984498 49.832182] [80.984498 50.220026]
          [92.196762 49.514851] [82.641655 49.514851]
          [93.148749 156.666031] [94.805906 155.008874] [96.533581 153.422231] [98.331777 151.941367]
          [85.321318 163.541477] [85.603388 163.823548] [86.026491 163.823549] [86.308561 163.541477]
          [98.75488 150.566277] [105.524551 146.652563]
          [96.533581 148.838601] [96.392549 148.591789] [96.463065 148.30972] [96.709876 148.168686]
          [209.572966 103.989537] [210.066587 101.133585]
          [169.801155 121.795181] [184.891877 122.852943]
          [214.967546 73.067661] [215.038063 72.715076] [214.755993 72.362487] [214.438665 72.29197]
          [211.018573 101.309877] [210.524951 104.165831]
          [88.670893 135.122968] [81.019757 135.122969]
          [98.331777 151.941367] [96.533581 148.838601]
          [97.379791 148.344982] [97.379791 148.344982] [98.437552 150.178432] [98.649106 150.531019]
          [208.832533 107.938513] [208.867792 107.656442] [209.149861 107.480152] [209.396671 107.550667]
          [209.855036 49.514851] [208.90305 49.514851]
          [211.124348 97.784007] [210.842278 97.74875] [210.665985 97.466679] [210.736503 97.219869]
          [80.984498 133.430551] [80.984499 133.818395] [81.301827 134.135726] [81.68967 134.135726]
          [80.984498 50.220026] [80.984498 133.430551]
          [91.244778 139.706599] [91.103745 139.459789] [91.174263 139.177719] [91.421072 139.036683]
          [78.340095 141.822121] [71.46465 148.697567]
          [148.892747 46.235793] [148.892748 49.514851]
          [184.292479 126.343555] [184.257221 126.69614] [184.504032 127.013468] [184.821359 127.083985]
          [169.906933 49.514851] [169.906932 46.235792]


          [193.153965 57.52018]
          [207.553965 57.520181]
          [207.553965 68.920181]
          [193.153965 68.920181]

          [209.396671 107.550667] [209.678741 107.585926] [209.855036 107.867996] [209.784517 108.114806]
          [207.422185 115.977496] [208.832533 107.938513]
          [86.308561 163.541477] [93.148749 156.666031]
          [210.595468 50.255285] [210.595468 49.832181] [210.278139 49.550111] [209.855036 49.514851]
          [204.954077 129.975198] [206.717012 119.855952]
          [210.524951 104.165831] [210.489692 104.447903] [210.207623 104.624196] [209.960813 104.553677]
          [95.229012 145.594801] [94.982201 145.735836] [94.70013 145.665317] [94.559095 145.418507]
          [102.457044 49.514851] [102.457044 56.989694]
          [153.828964 121.795181] [169.801155 121.795181]
          [143.321874 135.546073] [153.123792 135.546073]
          [210.066587 101.133585] [210.101845 100.851517] [210.383914 100.675221] [210.630726 100.745738]
          [224.452135 128.3533] [231.186547 90.273904]
          [96.709876 148.168686] [96.956688 148.027654] [97.238758 148.09817] [97.379791 148.344982]
          [214.368148 76.487756] [214.967546 73.067661]
          [184.821359 127.083985] [204.178385 130.504079]
          [204.178385 130.504079] [204.530971 130.574596] [204.883557 130.327785] [204.954077 129.975198]
          [149.562662 45.565877] [149.210076 45.565877] [148.892747 45.883205] [148.892747 46.235793]
          [230.728185 89.498213] [213.627717 86.501225]
          [71.46465 148.697567] [71.182579 148.979637] [71.18258 149.402739] [71.46465 149.684811]
          [89.940206 136.357022] [89.693397 136.498058] [89.411328 136.427539] [89.270291 136.18073]
          [89.199773 134.135726] [90.116499 135.687108]
          [89.270291 136.18073] [88.670893 135.122968]
          [107.816364 145.277472] [113.351981 142.068933] [119.310701 139.600822] [124.952091 138.084698]
          [206.717012 119.855952] [206.752271 119.573882] [207.034339 119.39759] [207.281151 119.468107]
          [124.952091 138.084698] [131.122366 136.427539] [137.433669 135.581333] [143.321874 135.546073]
          [209.784517 108.114806] [208.374169 116.153788]
          [81.019757 135.122969] [81.019757 139.177719]
          [209.960813 104.553677] [209.678742 104.518419] [209.502448 104.236349] [209.572966 103.989537]
          [82.641655 49.514851] [81.724929 49.51485]
          [71.46465 149.684811] [85.321318 163.541477]
          [210.736503 97.219869] [214.368148 76.487756]
          [91.421072 139.036683] [91.667884 138.895648] [91.94995 138.966166] [92.090987 139.212976]
          [169.906932 46.235792] [169.906933 45.883207] [169.589604 45.565878] [169.237016 45.565878]
          [208.374169 116.153788] [208.33891 116.435858] [208.056841 116.612152] [207.810029 116.541636]
          [223.676445 128.917439] [224.064291 128.987956] [224.381618 128.741144] [224.452135 128.3533]
          [92.090987 139.212976] [95.405304 144.924887]
          [210.630726 100.745738] [210.877537 100.780996] [211.053831 101.063065] [211.018573 101.309877]
          [81.019757 139.177719] [80.103031 140.023927] [79.186303 140.940653] [78.340095 141.822121]
          [207.810029 116.541636] [207.563219 116.506377] [207.351667 116.224308] [207.422185 115.977496]
          [208.90305 49.514851] [169.906933 49.514851]
          [102.457044 56.989694] [92.196762 56.989695]
          [81.68967 134.135726] [89.199773 134.135726]
          [206.646493 125.88519] [223.676445 128.917439]
          [169.237016 45.565878] [149.562662 45.565877]
          [153.828964 134.840899] [153.828964 121.795181]
          [90.116499 135.687108] [90.257536 135.933919] [90.187018 136.215988] [89.940206 136.357022]
          [207.668995 120.032246] [206.646493 125.88519]
          [98.649106 150.531019] [98.684364 150.566277] [98.719622 150.566277] [98.75488 150.566277]
          [184.891877 122.852943] [184.292479 126.343555]
          [211.688487 97.396163] [211.653229 97.678232] [211.37116 97.854524] [211.124348 97.784007]])

(def dat [  
   
    
     
     
   
     
     
    
  
      
     
 
  
 

  


    
 
   
 
  
  
 
  
   
 
   
  
  
   
  
  
      
    
    
    
      
   
     
   
   
     
     
     
     
     
   
     
     
     
   
    
    
      
     
   
    
   
     
     
   
     
   
   
   
   
   
     
     
   
   
   
    
     
     
    
     
    
    
     
   
    
    ])

(spit "things-low/klor-attempt.scad"
      (write-scad (let [bezier-1 (mapv #(subvec % 0 2) (bezier-cubic  [95.405304 144.924887 0.0] [95.54634 145.13644 0.0]  [95.475822 145.453768 0.0] [95.229012 145.594801 0.0] 30))
                        dat-size (count dat)]
                    (union
                    ;;  (->>
                    ;;   (import "../parts/klor1_3-klor1_3.stl")
                    ;;   (rdz 180)
                    ;;   (rdy 180)
                    ;;   (translate [840 150 0]))
                    (-#(polygon pcb-points-list))
                     (translate        [153.828964 121.795181];   [105.524551 146.652563]
                       (sphere 2))
                    (->>(import "../parts/klor1_3-klor1_3.stl")
                    (rdx 180)
                    (translate [90 109 0]) )
                     (for [index (range dat-size)
                           :let [col (* index (/ 1 dat-size))]] 
                       (color [col 0 0 1] (translate (nth dat index) (sphere 0.5)))
                       )
                    ;;  (->> 
                    ;;   (cube 43 40 2)
                    ;;   (rdz 90)
                    ;;   (translate [90 98 0]))
                    ;;  (->> 
                    ;;   (cube 27 27 4)
                    ;;   (translate [90 62 0]))
                     ))))

(spit "things-low/left-section-test.scad"
      (write-scad 
       (include "../BOSL2/std.scad")
       (let [;cross-s (wall-cross-section (wall-cross-section-parameter (tps-65-wall-position :tr :north)) 30)
             ;cross-s-outer (:points (:outer-wall-curve cross-s))
             ;cross-s-inner (:points (:inner-wall-curve cross-s))
             steps 30
             above-screen-outer-keywords [:opposite-web-post-position-top
                                          :web-post-position-top :wall-locate-1-to-3-curve-for-polyhedron-second-control-point
                                          :wall-locate3-point]
             above-screen-inner-keywords [:opposite-web-post-position-bottom
                                          :web-post-position-bottom
                                          :wall-locate-2-top
                                          :wall-locate-2-bottom]
             left-section-top-right-control-points   (calculate-control-points (tps-65-wall-position :tr :north))
             left-section-top-left-north-control-points   (calculate-control-points (tps-65-wall-position :tl :north :offset [0 0.0000001 0.0]))
             left-section-top-left-north-west-control-points   (calculate-control-points (tps-65-wall-position :tl :north-west))
             left-section-top-left-west-control-points   (calculate-control-points (tps-65-wall-position :tl :west :offset [0.00000001 0.0 0.0]))
             left-section-left-mid-control-points  (calculate-control-points (tps-65-to-screen-wall-position :lm :west))
             left-section-bottom-left-west-control-points (calculate-control-points (tps-65-to-screen-wall-position :bl :west :offset [-0.0001 0 0]))
             left-section-bottom-left-south-west-control-points (calculate-control-points (tps-65-to-screen-wall-position :bl :south-west))
             left-section-bottom-left-south-control-points (calculate-control-points (tps-65-to-screen-wall-position :bl :south :offset [0 -0.0001 0]))
             thumb-bl-tl-points (wall-brace-polyhedron-points thumb-bl-place -1 0 :tl :degrees)
             left-section-top-right-outer  (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words left-section-top-right-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                              3 default-weights-for-vertical-nurbs steps)
             left-section-top-left-north-outer (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words left-section-top-left-north-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                                  3 default-weights-for-vertical-nurbs steps)
             left-section-top-left-north-west-outer (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words left-section-top-left-north-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                                       3 default-weights-for-vertical-nurbs steps)
             left-section-top-left-west-outer (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words left-section-top-left-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                                 3 default-weights-for-vertical-nurbs steps)


             left-section-top-right-north-inner  (n-degree-bezier-curve (get-curve-control-points-by-key-words left-section-top-right-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps)
             left-section-top-left-north-inner  (n-degree-bezier-curve (get-curve-control-points-by-key-words left-section-top-left-north-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps)
             left-section-top-left-north-west-inner  (n-degree-bezier-curve (get-curve-control-points-by-key-words left-section-top-left-north-west-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps)
             left-section-top-left-west-inner  (n-degree-bezier-curve (get-curve-control-points-by-key-words left-section-top-left-west-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps)
             left-section-left-mid-outer (apply tps-to-screen-side-catmull (conj (vec (get-curve-control-points-by-key-words  left-section-left-mid-control-points outer-wall-catmull-rom-spline-parameters)) steps))
             left-section-left-mid-inner (apply tps-to-screen-side-catmull (conj (vec (get-curve-control-points-by-key-words  left-section-left-mid-control-points inner-wall-catmull-rom-spline-parameters)) steps))
             left-section-bottom-left-west-outer (apply tps-to-screen-side-catmull (conj (vec (get-curve-control-points-by-key-words  left-section-bottom-left-west-control-points outer-wall-catmull-rom-spline-parameters)) steps))
             left-section-bottom-left-west-inner (apply tps-to-screen-side-catmull (conj (vec (get-curve-control-points-by-key-words  left-section-bottom-left-west-control-points inner-wall-catmull-rom-spline-parameters)) steps))
             left-section-bottom-left-south-west-outer (apply tps-to-screen-side-catmull (conj (vec (get-curve-control-points-by-key-words  left-section-bottom-left-south-west-control-points outer-wall-catmull-rom-spline-parameters)) steps))
             left-section-bottom-left-south-west-inner (apply tps-to-screen-side-catmull (conj (vec (get-curve-control-points-by-key-words  left-section-bottom-left-south-west-control-points inner-wall-catmull-rom-spline-parameters)) steps))
             left-section-bottom-left-south-outer (apply tps-to-screen-side-catmull (conj (vec (get-curve-control-points-by-key-words  left-section-bottom-left-south-control-points outer-wall-catmull-rom-spline-parameters)) steps))
             left-section-bottom-left-south-inner (apply tps-to-screen-side-catmull (conj (vec (get-curve-control-points-by-key-words  left-section-bottom-left-south-control-points inner-wall-catmull-rom-spline-parameters)) steps))
             thumb-bl-tl-points-outer  (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tl-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                          3 default-weights-for-vertical-nurbs steps)
             thumb-bl-tl-points-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tl-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps)
             left-section-above-screen-left-mid-outer (catmull-rom-spline-curve (get-curve-control-points-by-key-words left-section-left-mid-control-points above-screen-outer-keywords) steps)
             left-section-above-screen-left-mid-inner (catmull-rom-spline-curve (get-curve-control-points-by-key-words left-section-left-mid-control-points above-screen-inner-keywords) steps)
             left-section-above-screen-bottom-left-west-outer (catmull-rom-spline-curve (get-curve-control-points-by-key-words left-section-bottom-left-west-control-points above-screen-outer-keywords) steps)
             left-section-above-screen-bottom-left-west-inner (catmull-rom-spline-curve (get-curve-control-points-by-key-words left-section-bottom-left-west-control-points above-screen-inner-keywords) steps)
             outer-wall (vec (for [index (range (inc steps))
                                   :let [outer-steps (* steps 4)]]
                               (do (println "index " index)
                                   (nurbs
                                    [
                                    (nth left-section-top-right-outer index)
                                     (nth left-section-top-left-north-outer index)
                                     (nth left-section-top-left-north-west-outer index)
                                      (nth left-section-top-left-west-outer index)
                                     (nth left-section-left-mid-outer index)
                                      (nth left-section-bottom-left-west-outer index)
                                     (nth left-section-bottom-left-south-west-outer index)
                                     (nth left-section-bottom-left-south-outer index ) 
                                     ]
                                    2
                                    ;(mapv (partial * 5)[0 0 0 0 (/ 1 7) (/ 3 7) (/ 4 7) (/ 5 7) 1 1 1 1])
                                    (mapv (partial * 6) [0 0 0  (/ 1 7) (/ 3 7) (/ 3.6 7) (/ 4 7) (/ 5 7)  1 1 1])
                                    [1 1 (/ (sqrt 2) 2) 1 2 1 (/ (sqrt 2) 2) 1]
                                    ;(nth thumb-bl-tl-points-outer index)
                                    outer-steps))))
             inner-wall (vec (for [index (range (inc steps))
                                   :let [inner-steps (* steps 4)]]
                               (do
                                 (println "index " index)
                                 (nurbs
                                  [
                                   (nth left-section-bottom-left-south-inner index)
                                   (nth left-section-bottom-left-south-west-inner index)
                                   (nth left-section-bottom-left-west-inner index)
                                   (nth left-section-left-mid-inner index)
                                   (nth left-section-top-left-west-inner index)
                                   (nth left-section-top-left-north-west-inner index)
                                   (nth left-section-top-left-north-inner index)
                                   (nth left-section-top-right-north-inner index)
                                   ]
                                  3
                                  (mapv (partial * 5) [0 0 0 0 (/ 2 7) (/ 3 7) (/ 4 7) (/ 6 7) 1 1 1 1])
                                  [1 (/ (sqrt 2) 2) 1 8 1 1 1 1]
                                  ;(nth thumb-bl-tl-points-inner index)
                                  inner-steps))))
             outer-wall-above-screen (vec (for [index (range (inc steps))]
                                            (bezier-linear
                                             (nth left-section-above-screen-left-mid-outer index)
                                             (nth left-section-above-screen-bottom-left-west-outer index)
                                             steps)))
             inner-wall-above-screen (vec (for [index (range (inc steps))]
                                            (bezier-linear
                                             (nth left-section-above-screen-bottom-left-west-inner index)
                                             (nth left-section-above-screen-left-mid-inner index) 
                                             steps)))
             vnf-array (wall-vnf-array outer-wall inner-wall default-vnf-vertex-array-args)
             above-wall-vnf-array (wall-vnf-array outer-wall-above-screen inner-wall-above-screen default-vnf-vertex-array-args)] 
         (union
          ;; (for [point cross-s-outer]
          ;;   (translate point (sphere 2.5)))
          ;; (for [point cross-s-inner]
          ;;   (translate point (cube 2.5 2.5 2.5)))
        (difference (tps-65-place tps-65-mount)
                    (tps-65-place tps-65-mount-cutout)
                    (tps-65-place (translate [0 0 (+ 0 1)] tps-65-mount-main-cutout)))
          ;; (difference
          ;;  (screen-holder-place-side screen-holder)
          ;;    (screen-holder-place-side screen-holder-cut))
          
        ;(translate tps-65-mid-right-outer (sphere 5))
          ;(translate tps-65-top-mid-outer (sphere 5)) 
          
                 (difference 
                  (screen-holder-place-side screen-holder)
                   (screen-holder-place-side screen-holder-cut))
        ;(vnf-polyhedron  above-wall-vnf-array)
       (difference
         (vnf-polyhedron vnf-array)
               (screen-holder-place-side screen-holder)
                   (screen-holder-place-side (translate [0 0 3]screen-holder))
                     (screen-holder-place-side (translate [0 0 -4] screen-holder
                                                        )
         ))
        )
        )
          ;;  (difference 
      ;;   (vnf-polyhedron
      ;;    (wall-vnf (wall-section (wall-section-parameter
      ;;                             [;(wall-cross-section-parameter (tps-65-wall-position :tr :north))
      ;;                              ;(wall-cross-section-parameter (tps-65-wall-position :br :south))
      ;;                             ;;  (wall-cross-section-parameter (tps-65-to-screen-wall-position :bl :south :offset [0 -0.00001 0])
      ;;                             ;;                                :outer-wall-parameters left-section-to-screen-vectical-curve-parameters)
      ;;                             ;;  (wall-cross-section-parameter (tps-65-to-screen-wall-position :bl :south-west)
      ;;                             ;;                                :outer-wall-parameters left-section-to-screen-vectical-curve-parameters)
      ;;                              (wall-cross-section-parameter (tps-65-to-screen-wall-position :bl :west :offset [-0.00001 0 0])
      ;;                                                            :outer-wall-parameters left-section-to-screen-vectical-curve-parameters)
      ;;                              (wall-cross-section-parameter (tps-65-to-screen-wall-position :lm :west)
      ;;                                                            :outer-wall-parameters left-section-to-screen-vectical-curve-parameters)
      ;;                              (wall-cross-section-parameter (tps-65-wall-position :tl :west))
      ;;                              ;(wall-cross-section-parameter (tps-65-wall-position :tl :north-west))
      ;;                              ;(wall-cross-section-parameter (tps-65-wall-position :tl :north))
      ;;                              ;(wall-cross-section-parameter (tps-65-wall-position :tr :north))
      ;;                              ]
      ;;                             (local-cubic-curve-interpolation-with-calculated-tangents-parameter)
      ;;                             :calculation-order :vertical-first
      ;;                             )
      ;;                            30 30)
      ;;              {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse true :style :default})
      ;;              )
      ;;              (screen-holder-place-side screen-holder)
      ;;              (screen-holder-place-side (translate [0 0 3]screen-holder))
      ;;              (screen-holder-place-side (translate [0 0 -3] screen-holder)))
        ;;   (difference 
        ;;  (vnf-polyhedron
        ;;  (wall-vnf (wall-section (wall-section-parameter
        ;;                           [;(wall-cross-section-parameter (tps-65-wall-position :tr :north))
        ;;                            (wall-cross-section-parameter (tps-65-wall-position :br :south))
        ;;                            (wall-cross-section-parameter (tps-65-to-screen-wall-position :bl :south)
        ;;                                                          :outer-wall-parameters left-section-to-screen-vectical-curve-parameters)
        ;;                            (wall-cross-section-parameter (tps-65-to-screen-wall-position :bl :south-west)
        ;;                                                          :outer-wall-parameters left-section-to-screen-vectical-curve-parameters)
        ;;                            (wall-cross-section-parameter (tps-65-to-screen-wall-position :bl :west)
        ;;                                                          :outer-wall-parameters left-section-to-screen-vectical-curve-parameters)
        ;;                            (wall-cross-section-parameter (tps-65-to-screen-wall-position :lm :west) 
        ;;                                                          :outer-wall-parameters left-section-to-screen-vectical-curve-parameters)
        ;;                            (wall-cross-section-parameter (tps-65-wall-position :tl :west))
        ;;                            (wall-cross-section-parameter (tps-65-wall-position :tl :north-west))
        ;;                            (wall-cross-section-parameter (tps-65-wall-position :tl :north ))
        ;;                            (wall-cross-section-parameter (tps-65-wall-position :tr :north )) 
                                   
        ;;                            ]
        ;;                           (nurbs-parameters 2 [1 1 (/ (sqrt 2) 2) 1  1 1 (/ (sqrt 2) 2) 1 1] :knot-vector (mapv (partial * 7)[0 0 0 (/ 1 8) (/ 3 8) (/ 4 8)  (/ 4 8) (/ 5 8) (/ 7 8) 1 1 1]))
        ;;                          :calculation-order :vertical-first
        ;;                           )
        ;;                           30 30)
        ;;            {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse true :style :default}))
        ;;            (screen-holder-place-side screen-holder))
        ))
(comment
   (catmull-rom-spline-curve [ [-119.49722893641112 -16.21014881119986 4.515465759170517] [-106.44557428759029 -61.77207981460153 3.791066581107142] [-106.44557428759029 -61.77207981460153 3.791066581107142] [-75.03006450718398 -75.88351008320753 1.5074627534415863]]
                             30)
  )

(comment 
  (calculate-control-points (tps-65-to-screen-wall-position :bl :south-west :offset [1 0 2])))

(spit "things-low/horizontal-first-test.scad"
      (write-scad
       (include "../BOSL2/std.scad")
       key-holes
       (let [thumb-tr (wall-brace-polyhedron-points thumb-tr-place 1 0 :tr :degrees :slant :no-slant :xy 3)]
         (union 
         (translate (:wall-locate3-point thumb-tr) (sphere 1))
          (translate (:wall-locate-2-bottom thumb-tr) (sphere 1))
          )
         )
       
       (union (let [curve-paramater (nurbs-parameters 2 (vec (reverse [ 1 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1  1]))
                                                      :knot-vector (let [denom 10]
                                                                     (mapv (partial * (dec denom)) [0 0 0 (/ 1 denom) (/ 2 denom) (/ 3 denom) (/ 5 denom) (/ 6 denom) (/ 7 denom) (/ 7.5 denom) (/ 9.0 denom)  1 1 1])) 
                                                      :linear-outer-top false
                                                      :linear-inner-top false)
                    wall-section-parameter (wall-section-parameter
                                            [
                                             ;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :tr :xy 3 :slant :parallel-by-d-opposite))
                                             ;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :tr :xy 3 :slant :parallel-by-d-opposite :offset [0 3 -3]))
                                             ;(wall-cross-section-parameter (key-wall-position 1 2 1 -1 :br :xy 3 :offset [-1 -1 10]))
                                             (wall-cross-section-parameter (key-wall-position 1 2 1 0 :br :xy 3 :slant :no-slant))
                                             ;(wall-cross-section-parameter (key-wall-position 2 2 -1 -1 :bl  :xy 3 :slant :no-slant))
                                             (wall-cross-section-parameter (key-wall-position 2 2 1 -1 :bl  :xy 3 :slant :no-slant))
                                             (wall-cross-section-parameter (key-wall-position 2 2 -1 -1 :br :slant :no-slant :xy 4))
                                             (wall-cross-section-parameter (key-wall-position 3 2 -1 0 :bl :offset [0.000001 0 0] :slant :no-slant :xy 4))
                                             (wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :bl :slant :no-slant :xy 4.5))
                                             (wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bl :offset [0 0.000001 0]))
                                             (wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :br :slant :no-slant))
                                             (wall-cross-section-parameter (key-wall-position lastcol 2 -1 0 :bl :slant :no-slant :offset [0.000001 0 0]))
                                             (wall-cross-section-parameter (key-wall-position lastcol 2 -1 -1 :bl :slant :no-slant))
                                             (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :bl :slant :no-slant :offset [0 0.000001 0]))
                                             (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :br))
                                                      ;(wall-cross-section-parameter (key-wall-position 3 2 -1 0 :bl))
                                             ]
                                            curve-paramater)
                    wall-cross-section-steps 30
                    wall-section-steps 30
                    wall-cross-section-parameters (:wall-cross-section-parameters wall-section-parameter)
                    steps-distrubution (:steps-distrubution wall-section-parameter)
                    curve-parameters (:curve-parameters wall-section-parameter)]
                (vnf-polyhedron (wall-vnf (wall-section wall-section-parameter wall-cross-section-steps wall-section-steps)
                                          {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default})))
              thumb-type
              ;; (vnf-polyhedron (wall-vnf (wall-section 
              ;;                            (wall-section-parameter
              ;;                             [;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :br))
              ;;                              ;(wall-cross-section-parameter (thumb-wall-position thumb-tl-place 1 0 :br :xy 5 :slant :no-slant))
              ;;                              ;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 0 -1 :bl :xy 3))
              ;;                              ;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :br :xy 3 )  :slant :parallel-by-d-opposite)
              ;;                              ;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :tr :xy 4 :slant :parallel-by-d-opposite :bottom-offset [0 0 -10]))
              ;;                              ;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 0 1 :tl :xy 1 :slant :no-slant))
              ;;                              ;(wall-cross-section-parameter (key-wall-position 1 2 -1 -1 :br :xy 3))
              ;;                              ;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :tr :xy 3 :slant :no-slant))
                                           
              ;;                              ;(wall-cross-section-parameter (key-wall-position 2 2 -1 -1 :br :xy 3 :slant :no-slant )) 
              ;;                             ; (wall-cross-section-parameter (key-wall-position 1 2 1 -1 :br :xy 3 :ofsset [0 0.01 0]))
              ;;                              ;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :tr :xy 2 :slant :parallel-by-d-opposite))
              ;;                              ;(wall-cross-section-parameter (key-wall-position 1 2 1 0 :br :xy 3 :slant :no-slant :offset [-3 -5 14]))
              ;;                              ;(wall-cross-section-parameter (key-wall-position 1 2 1 0 :br :xy 3 :offset [-4 -3.5 10]))
              ;;                              (wall-cross-section-parameter (key-wall-position 1 2 0 -1 :br :xy 3 :slant :no-slant  :offset [0 -0.0001 0]))
              ;;                              (wall-cross-section-parameter (key-wall-position 1 2 1 -1 :br :xy 3 :slant :no-slant ))
              ;;                              (wall-cross-section-parameter (key-wall-position 1 2 1 0 :br :xy 3  :slant :no-slant :offset [0.00001 0 0]) )
              ;;                              ;(wall-cross-section-parameter (key-wall-position 2 2 1 -1 :bl :xy 3))
              ;;                              ;(wall-cross-section-parameter (key-wall-position 2 2 0 -1 :bm :xy 3 :slant :no-slant))
              ;;                              ]
              ;;                             ;(nurbs-parameters 2 [ 1 (/ (sqrt 2) 2) 1 ] :knot-vector [0 0 0 1 1 1])
              ;;                             (nurbs-parameters 2 [1 (/ (sqrt 2) 2) 1] [0 0 0 1 1 1]
              ;;                              ;:linear-outer-top true :linear-inner-top true
              ;;                              ) 
              ;;                             )
              ;;                            30 30)
              ;;                           {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
              (vnf-polyhedron (wall-vnf (wall-section
                                         (wall-section-parameter
                                          [;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :br)) 
                                           ;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :br :xy 2 :slant :parallel-by-d-opposite :pre-rotation [0 20 0] ))
                                           ;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :rm :xy 2 :slant :parallel-by-d-opposite :offset [0.00001 0 0]))
                                           (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :br :xy 3 :slant :parallel-by-d-opposite ) 
                                                                         ;:outer-wall-parameters (outer-wall-vertical-nurbs-with-knot-vector-parameters [0 0 0 0 1 2 2 2 2] :weights [1 1  0.6 0.75 1])
                                                                         )
                                           (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :tr :xy 3 :slant :parallel-by-d-opposite :pre-rotation [-30 0 -10] ) )
                                           (wall-cross-section-parameter (key-wall-position 1 2 1 0 :br :xy 3 :slant :no-slant :offset [0 -1 0] :pre-rotation [(deg2rad -10) (deg2rad 0) (deg2rad 0)])) 
                                           (wall-cross-section-parameter (key-wall-position 1 2 1 0 :br :xy 3 :slant :no-slant))
                                           ;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 0 1 :tr :xy 0 :slant :parallel-by-d-opposite :offset [0 0.0001 0 ]))
                                      
                                     
                                           ]
                                          ;(nurbs-parameters 2 [ 1 (/ (sqrt 2) 2) 1 ] :knot-vector [0 0 0 1 1 1])
                                          (catmull-rom-spline-parameters
                                           ;nurbs-parameters 3 [1 (/ (sqrt  2) 2) (/ (sqrt  2) 2) 1] [0 0 0 0 1 1 1 1]
                                           ;:linear-outer-top true :linear-inner-top true
                                                            ))
                                         30 30)
                                        {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
              )))

(comment (mapv - (web-post-point-top thumb-tr-place :tr :degrees) (web-post-point-top (partial key-place 1 2) :br :radians)))


(def thumb-wall-section 
  (union 
    (vnf-polyhedron (wall-vnf (wall-section
                               (wall-section-parameter
                                [;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 -1 :tr :xy 3 :slant :parallel-by-d-opposite :offset [-0.000001 -0.000001 0]))
                                 (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :br :xy 4  :slant :no-slant))
                                 (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :rm :xy 4  :slant :no-slant))
                                 (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 -1 :tr :xy 4 :slant :parallel-by-d-opposite))
                                     ;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :tr :xy 3 :offset [-0.5 1 0] :slant :parallel-by-d-opposite))
                                 (wall-cross-section-parameter (key-wall-position 1 2 1 0 :br :xy 4 :slant :no-slant))
                                     ;(wall-cross-section-parameter (key-wall-position 2 2 1 -1  :bl :xy 3 :slant :no-slant)) 
                                 (wall-cross-section-parameter (key-wall-position 2 2 0 -1  :bm :xy 3 :slant :no-slant))]
                                (local-cubic-curve-interpolation-with-calculated-tangents-parameter ;:linear-outer-top true :linear-inner-top true
                                 )
                                :calculation-order :vertical-first)
                               30 30)
                              {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
(vnf-polyhedron (wall-vnf (wall-section
                           (wall-section-parameter
                            [;(wall-cross-section-parameter (thumb-wall-position thumb-mr-place 0 -1 :bl :xy 5 :offset [(/ extra-width -2) 0 0])) 
                             (wall-cross-section-parameter (thumb-wall-position thumb-mr-place 0 -1 :br :xy 5 :slant :no-slant))
                             (wall-cross-section-parameter (thumb-wall-position thumb-mr-place 1 -1 :br :xy 4))
                             (wall-cross-section-parameter (thumb-wall-position thumb-mr-place 1 0 :br :xy 5 :slant :no-slant))
                             (wall-cross-section-parameter (thumb-wall-position thumb-tl-place 1 -1 :br :xy 4))
                             (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant))
                             (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 -1 :br :xy 5 :slant :no-slant))
                             (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :br :xy 4 :slant :no-slant))


                                           ;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 0 1 :tr :xy 0 :slant :parallel-by-d-opposite :offset [0 0.0001 0 ]))
                             ]
                                          ;(nurbs-parameters 2 [ 1 (/ (sqrt 2) 2) 1 ] :knot-vector [0 0 0 1 1 1])
                            (nurbs-parameters 2 [1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1] :knot-vector (let [denom 6]
                                                                                                                      (mapv (partial * (dec denom)) [0 0 0  (/ 2 denom) (/ 2 denom)  (/ 4 denom) (/ 4 denom)
                                                                                                                                                     1 1 1]))
                                           ;nurbs-parameters 3 [1 (/ (sqrt  2) 2) (/ (sqrt  2) 2) 1] [0 0 0 0 1 1 1 1]
                                           ;:linear-outer-top true :linear-inner-top true
                                              ))
                           30 30)
                          {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
(vnf-polyhedron (wall-vnf (wall-section
                           (wall-section-parameter
                            [(wall-cross-section-parameter (thumb-wall-position thumb-br-place -1 -1 :bl :xy 5))
                             (wall-cross-section-parameter (thumb-wall-position thumb-br-place 0 -1 :bl :xy 5))
                             (wall-cross-section-parameter (thumb-wall-position thumb-br-place 0 -1 :br :xy 5))
                             (wall-cross-section-parameter (thumb-wall-position thumb-mr-place 0 -1 :bl :xy 5))
                             (wall-cross-section-parameter (thumb-wall-position thumb-mr-place 0 -1 :br :xy 5 :slant :no-slant))
                             (wall-cross-section-parameter (thumb-wall-position thumb-mr-place 1 -1 :br :xy 4))
                                     ;(wall-cross-section-parameter (thumb-wall-position thumb-mr-place 0 -1 :bl :xy 5 ))
                             ]
                            (catmull-rom-spline-parameters ;:linear-outer-top true :linear-inner-top true 
                             )
                                    ;:calculation-order :vertical-first
                            )
                           30 30)
                          {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
(vnf-polyhedron (wall-vnf (wall-section
                           (wall-section-parameter
                            [(wall-cross-section-parameter (thumb-wall-position thumb-br-place -1 0 :bl :xy 5 :offset [-0.000001 0 0]))
                             (wall-cross-section-parameter (thumb-wall-position thumb-br-place -1 -1 :bl :xy 4))
                             (wall-cross-section-parameter (thumb-wall-position thumb-br-place 0 -1 :bl :xy 5 :offset [0 -0.000001 0]))]
                            (nurbs-parameters 2 [1 (/ (sqrt 2) 2) 1] [0 0 0 1 1 1];:linear-outer-top true :linear-inner-top true
                                              ))
                           30 30)
                          {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
(vnf-polyhedron (wall-vnf (wall-section
                           (wall-section-parameter
                            [(wall-cross-section-parameter (thumb-wall-position thumb-bl-place -1 0 :tl :xy 5))
                             (wall-cross-section-parameter (thumb-wall-position thumb-bl-place -1 0 :lm :xy 5))
                             (wall-cross-section-parameter (thumb-wall-position thumb-br-place -1 0 :tl :xy 5))
                             (wall-cross-section-parameter (thumb-wall-position thumb-br-place -1 0 :bl :xy 5))]
                            (global-curve-interpolation-parameters 2 ;:linear-outer-top true :linear-inner-top true
                                                                   )
                                    ;:calculation-order :vertical-first
                            )
                           30 30)
                          {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
   )
  )
(spit "things-low/thumb-section-test.scad"
      (write-scad 
       (include "../BOSL2/std.scad")
       (union
        thumb-type
        key-holes
        thumb-wall-section
        )))


(comment (let [key-points-fn (fn [top-or-bottom col left-or-right]
                               (let [web-post-point-fn (case top-or-bottom
                                                         :top web-post-point-top
                                                         :bottom web-post-point-top)
                                     key-top (case left-or-right
                                               :l :tl
                                               :r :tr)
                                     key-bottom (case left-or-right
                                                  :l :bl
                                                  :r :br)]
                                 (vec (apply concat (for [row (range -1 (inc cornerrow))]
                                        [(web-post-point-fn (partial key-place col row) key-bottom :radians)
                                         (web-post-point-fn (partial key-place col (inc row)) key-top :radians)])))))]
           (key-points-fn :top 0 :l)))

(comment (let [steps 30
               total-steps (* steps (inc (* cornerrow 2)))
               steps-increment (/ total-steps (+ keyswitch-height extra-height))
               
               key-steps-increment  (/ (floor(* steps-increment keyswitch-height)) 1)
               gap-steps-increment  (/ (ceil (* steps-increment extra-height)) 1)
               ]
           [total-steps steps-increment key-steps-increment gap-steps-increment]
           ))

(defn tps-65-to-keys [steps]
  (let [segments (inc (* cornerrow 2))
        total-steps (* steps segments)
       ; total-steps (* steps (inc (* cornerrow 2)))
        steps-increment (/ total-steps (+ keyswitch-height extra-height))

        key-steps  (/ (floor (* steps-increment keyswitch-height)) (inc cornerrow))
        gap-steps  (/ (ceil (* steps-increment extra-height)) cornerrow)
        tps-65-control-outer (catmull-rom-spline-curve
                              [screen-holder-top-right-outside-point
                              tps-65-top-left-outer 
                              tps-65-top-right-outer
                              (mapv + [0 1 0] tps-65-top-right-outer)]
                              total-steps)
        tps-65-control-outer-mid (bezier-linear
                                  ;; [screen-holder-top-left-outside-point
                                  ;;  tps-65-mid-left-outer
                                  ;;  tps-65-mid-right-outer
                                  ;;  tps-65-top-right-outer]
                                  tps-65-mid-left-outer
                                  tps-65-mid-right-outer
                                  total-steps)
        tps-65-control-inner (bezier-linear
                              tps-65-top-left-inner
                              tps-65-top-right-inner
                              total-steps)
        tps-65-control-inner-mid (bezier-linear
                              tps-65-mid-left-inner
                              tps-65-mid-right-inner
                              total-steps)
        tps-65-outer (bezier-linear
                      tps-65-bottom-left-outer
                      tps-65-bottom-right-outer
                      total-steps)
        tps-65-inner (bezier-linear
                      tps-65-bottom-left-inner
                      tps-65-bottom-right-inner
                      total-steps)
        key-points-fn (fn [top-or-bottom col left-or-right]
                        (let [web-post-point-fn (case top-or-bottom
                                                  :top web-post-point-top
                                                  :bottom web-post-point-bottom)
                              key-top (case left-or-right
                                        :l :tl
                                        :m :tm
                                        :r :tr)
                              key-mid (case left-or-right
                                        :l :lm
                                        :m :centre
                                        :r :rm)
                              key-bottom (case left-or-right
                                           :l :bl
                                           :m :bm
                                           :r :br)]
                          (vec (apply concat
                                      (for [row (range cornerrow  -1 -1)]
                                        [(web-post-point-fn (partial key-place col row) key-bottom :radians)
                                         (web-post-point-fn (partial key-place col row) key-mid :radians)
                                         (web-post-point-fn (partial key-place col row)  key-top :radians)])))))
        nurbs-fn (fn [points] (let [denom 10] (nurbs points 3 (mapv (partial * 8) [0 0 0 0 (/ 2 denom) (/ 3 denom)  (/ 4 denom) (/ 5 denom)
                                                                                   (/ 6 denom) (/ 7 denom) (/ 8 denom) 1 1 1 1])
                                                     [1 0.2 1 1 1 1 1 1 1 0.1 1] total-steps)))
        key-points-catmull-fn (fn [points-coll alpha] (vec (apply concat
                                                                  (for [index (range segments)
                                                                        :let [segment-steps (if (even? index) key-steps gap-steps)
                                                      ;endrop-last (if (= index (dec segments)) false true )
                                                                              ]]
                                                                    (if (= index (dec segments))
                                                                      (catmull-rom-spline-curve
                                                                       [(nth points-coll index)
                                                                        (nth points-coll (inc index))
                                                                        (nth points-coll (+ index 2))
                                                                        (nth points-coll (+ index 3))]
                                                                       segment-steps
                                                                       :alpha-type alpha)
                                                                      (drop-last
                                                                       (catmull-rom-spline-curve
                                                                        [(nth points-coll index)
                                                                         (nth points-coll (inc index))
                                                                         (nth points-coll (+ index 2))
                                                                         (nth points-coll (+ index 3))]
                                                                        segment-steps
                                                                        :alpha-type alpha)))))))
        gap-fix-outer (catmull-rom-spline-curve
                       [(web-post-point-top thumb-tl-place :tr :degrees)
                        (web-post-point-top (partial key-place 0 cornerrow) :bl :radians)
                        (web-post-point-top (partial key-place 0 cornerrow) :tl :radians)
                        (web-post-point-top (partial key-place 0 (dec cornerrow)) :bl :radians)]
                       key-steps)
        gap-fix-outer-key-side (bezier-linear
                                (web-post-point-top (partial key-place 0 cornerrow) :bl :radians)
                                (web-post-point-top (partial key-place 0 cornerrow) :tl :radians)
                                key-steps)
        gap-fix-inner (catmull-rom-spline-curve
                       [(web-post-point-bottom thumb-tl-place :tr :degrees)
                        (web-post-point-bottom (partial key-place 0 cornerrow) :bl :radians)
                        (web-post-point-bottom (partial key-place 0 cornerrow) :tl :radians)
                        (web-post-point-bottom (partial key-place 0 (dec cornerrow)) :bl :radians)]
                       key-steps)
        gap-fix-inner-key-side (bezier-linear
                                (web-post-point-bottom (partial key-place 0 cornerrow) :bl :radians)
                                (web-post-point-bottom (partial key-place 0 cornerrow) :tl :radians)
                                key-steps)

        key-points-tangent-point (mapv + (web-post-point-top (partial key-place 0 cornerrow) :bl :radians)
                                       (mapv #(/ % (magnitude %)) (mapv - (web-post-point-top (partial key-place 0 cornerrow) :bl :radians) (web-post-point-top thumb-tl-place :tr :degrees))))
        key-points-control-tangent-point-mid (mapv + (web-post-point-top (partial key-place 0 cornerrow) :bm :radians)
                                                   (mapv #(/ % (magnitude %)) (mapv - (web-post-point-top (partial key-place 0 cornerrow) :bm :radians)
                                                                                    (web-post-point-top thumb-tr-place :tm :degrees))))

        key-points-control-tangent-point (mapv + (web-post-point-top (partial key-place 0 cornerrow) :br :radians)
                                               (mapv #(/ % (magnitude %)) (mapv - (web-post-point-top (partial key-place 0 cornerrow) :br :radians)
                                                                                (web-post-point-top thumb-tr-place :tr :degrees))))

        key-points-tangent-point-top (mapv - (web-post-point-top (partial key-place 0 0) :bl :radians)
                                           (mapv #(/ % (magnitude %)) (mapv - (:point-on-tangent-from-plate (wall-brace-polyhedron-points (partial key-place 0 0) 0 1 :tl :radians))
                                                                            (web-post-point-top (partial key-place 0 0) :bl :radians))))
        key-points-control-tangent-point-top-mid (mapv - (web-post-point-top (partial key-place 0 0) :bm :radians)
                                                       (mapv #(/ % (magnitude %)) (mapv - (:point-on-tangent-from-plate (wall-brace-polyhedron-points (partial key-place 0 0) 0 1 :tm :radians))
                                                                                        (web-post-point-top (partial key-place 0 0) :bm :radians))))
        key-points-control-tangent-point-top (mapv - (web-post-point-top (partial key-place 0 0) :br :radians)
                                                   (mapv #(/ % (magnitude %)) (mapv - (:point-on-tangent-from-plate (wall-brace-polyhedron-points (partial key-place 0 0) 0 1 :tm :radians))
                                                                                    (web-post-point-top (partial key-place 0 0) :br :radians))))

        key-points-tangent-point-inner (mapv + (web-post-point-bottom (partial key-place 0 cornerrow) :bl :radians)
                                             (mapv #(/ % (magnitude %)) (mapv - (web-post-point-bottom (partial key-place 0 cornerrow) :bl :radians) (web-post-point-bottom thumb-tl-place :tr :degrees))))
        key-points-control-tangent-point-inner-mid (mapv + (web-post-point-bottom (partial key-place 0 cornerrow) :bm :radians)
                                                         (mapv #(/ % (magnitude %)) (mapv - (web-post-point-bottom (partial key-place 0 cornerrow) :bm :radians)
                                                                                          (web-post-point-bottom thumb-tr-place :tm :degrees))))
        key-points-control-tangent-point-inner (mapv + (web-post-point-bottom (partial key-place 0 cornerrow) :br :radians)
                                                     (mapv #(/ % (magnitude %)) (mapv - (web-post-point-bottom (partial key-place 0 cornerrow) :br :radians)
                                                                                      (web-post-point-bottom thumb-tr-place :tr :degrees))))

        key-points-tangent-point-top-inner (mapv - (web-post-point-bottom (partial key-place 0 0) :bl :radians)
                                                 (mapv #(/ % (magnitude %)) (mapv - (:point-on-tangent-from-plate-bottom (wall-brace-polyhedron-points (partial key-place 0 0) 0 1 :tl :radians))
                                                                                  (web-post-point-bottom (partial key-place 0 0) :bl :radians))))
        key-points-control-tangent-point-top-inner-mid (mapv - (web-post-point-bottom (partial key-place 0 0) :bm :radians)
                                                             (mapv #(/ % (magnitude %)) (mapv - (:point-on-tangent-from-plate-bottom (wall-brace-polyhedron-points (partial key-place 0 0) 0 1 :tm :radians))
                                                                                              (web-post-point-bottom (partial key-place 0 0) :bm :radians))))

        key-points-control-tangent-point-top-inner (mapv - (web-post-point-bottom (partial key-place 0 0) :br :radians)
                                                         (mapv #(/ % (magnitude %)) (mapv - (:point-on-tangent-from-plate-bottom (wall-brace-polyhedron-points (partial key-place 0 0) 0 1 :tr :radians))
                                                                                          (web-post-point-bottom (partial key-place 0 0) :br :radians))))

        build-point-list-fn (fn [first-tangent-point points second-tangent-point]
                              (let [list-size (count points)
                                    first-point (nth points 0)
                                    last-point (peek points)
                                    mid-list (subvec points 1 (dec list-size))]
                                (vec (apply concat [[first-point first-tangent-point] mid-list [second-tangent-point last-point]]))))
        key-points-outer (nurbs-fn
                          (build-point-list-fn key-points-tangent-point (key-points-fn :top 0 :l) key-points-tangent-point-top))
        key-points-inner (nurbs-fn
                          (build-point-list-fn key-points-tangent-point-inner (key-points-fn :bottom 0 :l) key-points-tangent-point-top-inner))
        key-points-control-inner-mid (nurbs-fn
                                      (build-point-list-fn key-points-control-tangent-point-inner-mid (key-points-fn :bottom 0 :m) key-points-control-tangent-point-top-inner-mid))
        key-points-control-outer-mid (nurbs-fn
                                      (build-point-list-fn key-points-control-tangent-point-mid (key-points-fn :top 0 :m) key-points-control-tangent-point-top-mid))
        

        key-points-control-outer (nurbs-fn
                                  (build-point-list-fn key-points-control-tangent-point (key-points-fn :top 0 :r) key-points-control-tangent-point-top))
        key-points-control-inner (nurbs-fn
                                  (build-point-list-fn key-points-control-tangent-point-inner (key-points-fn :bottom 0 :r) key-points-control-tangent-point-top-inner))
        outer-wall (mapv #(subvec % (* (/ steps 5) (/ 4 5))
                                  (inc (* (/ 12 5) (/ steps 5)))) (vec (for [index (range (inc total-steps))]
                                  (nurbs
                                   [(nth  key-points-control-outer index)
                                    (nth key-points-control-outer-mid index)
                                    (nth key-points-outer index)
                                    (nth  tps-65-outer index)
                                    (nth tps-65-control-outer-mid index)
                                    (nth tps-65-control-outer index)]
                                   2
                                   (let [denom 5]
                                     (mapv (partial * (- denom 1))
                                           [0 0 0  (/ 1 denom) (/ 2 denom) (/ 3 denom) 1 1 1]))
                                   [1 1 0.5 1 1 1]
                                   steps))))
        inner-wall (mapv #(subvec % (* (/ 4 5) (/ steps 5))
                                  (inc (* (/ 12 5) (/ steps 5)))) (vec (for [index (range (inc total-steps))]
                                  (nurbs
                                   [(nth tps-65-control-inner index)
                                    (nth  tps-65-control-inner-mid index)
                                    (nth  tps-65-inner index)
                                    (nth key-points-inner index)
                                    (nth key-points-control-inner-mid index)
                                    (nth  key-points-control-inner index)]
                                   2
                                   (let [denom 5]
                                     (mapv (partial * (- denom 1))
                                           [0 0 0  (/ 1 denom) (/ 2 denom) (/ 3 denom) 1 1 1]))
                                   [1 1 1 1 1 1]
                                   steps))))
        gap-fix-outer-wall (vec (for [index (range key-steps)]
                                  (bezier-linear
                                   (nth gap-fix-outer-key-side index)
                                   (nth gap-fix-outer index)
                                   4)))
        gap-fix-inner-wall (vec (for [index (range key-steps)]
                                  (bezier-linear
                                   (nth gap-fix-inner index)
                                   (nth gap-fix-inner-key-side index)
                                   4)))
        vnf-array (wall-vnf-array outer-wall inner-wall default-vnf-vertex-array-args)
        gap-fix-vnf-array (wall-vnf-array gap-fix-outer-wall gap-fix-inner-wall
                                          {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :min-edge})]
    
    (union
     (vnf-polyhedron vnf-array)
    ;(vnf-polyhedron gap-fix-vnf-array)
     )
    )
  )

(spit "things-low/tps-65-to-keys-test.scad"
      (write-scad 
       (include "../BOSL2/std.scad")
       (union
        thumb-type
        (let [bc (web-post-point-top (partial key-place 0 cornerrow) :bl :radians)
              tc-points (wall-brace-polyhedron-points (partial  key-place 0 0) 0 1 :tl :radians )
              tc-tangent-point (:point-on-tangent-from-plate tc-points)
              thumb-tl-tr (web-post-point-top thumb-tl-place :tr :degrees)
              thumb-tl-tr-to-bc (mapv - bc thumb-tl-tr)
              tc-vec (mapv - tc-tangent-point (:web-post-position-top tc-points))
              t1 (mapv + bc  (mapv #(/ % (magnitude thumb-tl-tr-to-bc)) thumb-tl-tr-to-bc))
              t2 (mapv - (:web-post-position-top tc-points)  (mapv #(/ % (magnitude tc-vec)) tc-vec))
              points [bc 
                      t1
                      (web-post-point-top (partial key-place 0 cornerrow) :tl :radians)
                      (web-post-point-top (partial key-place 0 1) :bl :radians)
                      (web-post-point-top (partial key-place 0 1) :tl :radians)
                      (web-post-point-top (partial key-place 0 0) :bl :radians)
                      t2
                      (web-post-point-top (partial key-place 0 0) :tl :radians)]
              curve (nurbs points 2 (mapv (partial * 5) [0 0 0 (/ 2 7) (/ 3 7) (/ 3.5 7) (/ 4 7) (/ 5 7) 1 1 1]) [1 0.8 0.5 1 1 0.5 0.8 1] 30)
              ]
          (println points)
          (println t2 (last points) tc-tangent-point)
          ;(plot-bezier-points curve (sphere 1))
          )
        (tps-65-place tps-65-mount)
        
        key-holes
        (tps-65-to-keys 30)
        )))


(spit "things-low/front-section-test.scad"
      (write-scad
       (include "../BOSL2/std.scad")
       (union
        (screen-holder-place-side screen-holder)
        (let 
         [steps 30
          steps-times-2 (* 3 steps)
          steps-times-3 (* 3 steps)
          steps-times-6 (* 6 steps)
          multiplier 2
          tps-65-line-to-first-column-br-outer (vec (concat (drop-last (bezier-linear tps-65-top-left-outer tps-65-bottom-left-outer  steps-times-3))
                                                            (catmull-rom-spline-curve
                                                             [tps-65-top-left-outer
                                                              tps-65-bottom-left-outer
                                                              (web-post-point-top (partial key-place 0 2) :bl :radians)
                                                              (web-post-point-top (partial key-place 0 2) :bm :radians)
                                                              (web-post-point-top (partial key-place 0 2) :br :radians)
                                                              (web-post-point-top (partial key-place 1 2) :bl :radians)]
                                                             steps-times-2
                                                             :alpha-type :uniform)))
          tps-65-line-inner (vec (concat (drop-last (bezier-linear tps-65-top-left-inner tps-65-bottom-left-inner steps-times-3))
                                         (catmull-rom-spline-curve
                                          [tps-65-top-left-inner
                                           tps-65-bottom-left-inner
                                           (web-post-point-bottom (partial key-place 0 2) :bl :radians)
                                           (web-post-point-bottom (partial key-place 0 2) :br :radians)
                                           (web-post-point-bottom (partial key-place 1 2) :bl :radians)]
                                          steps-times-2
                                          :alpha-type :uniform)))
          tps-65-line-control-outer (vec (concat (drop-last
                                                  (bezier-linear tps-65-top-right-outer  tps-65-bottom-right-outer  steps-times-3))
                                                 (catmull-rom-spline-curve
                                                  [tps-65-top-right-outer
                                                   tps-65-bottom-right-outer
                                                   (web-post-point-top (partial key-place 0 0) :tl :radians)
                                                   (web-post-point-top (partial key-place 0 0) :tm :radians)
                                                   (web-post-point-top (partial key-place 0 0) :tr :radians)
                                                   (web-post-point-top (partial key-place 1 0) :tl :radians)]
                                                  steps-times-2
                                                  :alpha-type :uniform)))
          tps-65-line-control-inner (vec (concat (drop-last (bezier-linear tps-65-top-right-inner tps-65-bottom-right-inner  steps-times-3))
                                                 (catmull-rom-spline-curve
                                                  [tps-65-top-right-inner
                                                   tps-65-bottom-right-inner
                                                   (web-post-point-bottom (partial key-place 0 0) :tl :radians)
                                                   (web-post-point-bottom (partial key-place 0 0) :tm :radians)
                                                   (web-post-point-bottom (partial key-place 0 0) :tr :radians)
                                                   (web-post-point-bottom (partial key-place 1 0) :tl :radians)]
                                                  steps-times-2
                                                  :alpha-type :uniform)))

          thumb-bl-tl-points (wall-brace-polyhedron-points thumb-bl-place -1 0 :tl :degrees)
          thumb-bl-br-points (wall-brace-polyhedron-points thumb-bl-place -1 0 :br :degrees)
          thumb-bl-tm-points (wall-brace-polyhedron-points thumb-bl-place 0 -1 :tm :degrees)
          thumb-bl-tr-points (wall-brace-polyhedron-points thumb-bl-place 0 1 :tr :degrees)
          thumb-bl-rm-points (wall-brace-polyhedron-points thumb-bl-place 0 0 :rm :degrees)
          thumb-tl-tl-points (wall-brace-polyhedron-points thumb-tl-place -1 0 :tl :degrees)
          thumb-bl-lm-points (wall-brace-polyhedron-points thumb-bl-place 0 1 :lm :degrees)
          thumb-catmull-control-outer  (drop-last (global-curve-interp-with-end-unit-derivatives-curve
                                                   [screen-holder-bottom-right-outside-point
                                                    (:point-on-tangent-from-plate thumb-bl-tl-points)]
                                                   3
                                                   screen-holder-bottom-left-outside-point
                                                   (:wall-locate3-point-floor thumb-bl-tm-points)
                                                   steps))
          thumb-catmull-control-inner  (drop-last (global-curve-interp-with-end-unit-derivatives-curve
                                                   [screen-holder-bottom-right-inside-point
                                                    (:point-on-tangent-from-plate-bottom thumb-bl-tl-points)]
                                                   3
                                                   screen-holder-bottom-left-inside-point
                                                   (:wall-locate-2-bottom-floor thumb-bl-tm-points)
                                                   steps))

          outer-horizontal-catmull-curve  (drop-last (global-curve-interp-with-end-unit-derivatives-curve
                                                      [screen-holder-top-right-outside-point
                                                       (web-post-point-top thumb-bl-place :tl :degrees)]
                                                      3
                                                      screen-holder-top-left-outside-point
                                                      (web-post-point-top thumb-bl-place :tr :degrees)
                                                      steps))
          inner-horizontal-catmull-curve  (drop-last (global-curve-interp-with-end-unit-derivatives-curve
                                                      [screen-holder-top-right-inside-point
                                                       (web-post-point-bottom thumb-bl-place :tl :degrees)]
                                                      3
                                                      screen-holder-top-left-inside-point
                                                      (web-post-point-bottom thumb-bl-place :tr :degrees)
                                                      steps))
          thumb-line-outer (let [d1-vec (mapv - (web-post-point-top thumb-bl-place :tl :degrees)
                                              screen-holder-top-right-outside-point)
                                 dn-vec (mapv - (web-post-point-top thumb-tl-place :tr :degrees) (web-post-point-top thumb-tl-place :tl :degrees))
                                 p 3
                                 interp-data (global-curve-interp-with-end-derivatives [(web-post-point-top thumb-bl-place :tl :degrees)
                                                                                        (web-post-point-top thumb-bl-place :tr :degrees)
                                                                                        (web-post-point-top thumb-tl-place :tl :degrees)]
                                                                                       p (mapv #(/ % (magnitude d1-vec)) d1-vec)
                                                                                       (mapv #(/ % (magnitude dn-vec)) dn-vec))]
                             (non-uniform-b-spline (:P interp-data) p (:U interp-data) (* (dec multiplier) steps)))
          thumb-line-outer-floor  (global-curve-interp-with-end-unit-derivatives-curve [(web-post-point-top thumb-bl-place :lm :degrees)
                                                                                        (web-post-point-top thumb-bl-place :rm :degrees)
                                                                                        (web-post-point-top thumb-tl-place :lm :degrees)]
                                                                                       3
                                                                                       screen-holder-bottom-right-outside-point
                                                                                       (web-post-point-top thumb-tl-place :rm :degrees)
                                                                                       (* 3 steps))
          thumb-line-inner (global-curve-interp-with-end-unit-derivatives-curve
                            [(web-post-point-bottom thumb-bl-place :tl :degrees)
                             (web-post-point-bottom thumb-bl-place :tr :degrees)
                             (web-post-point-bottom thumb-tl-place :tl :degrees)]
                            3
                            screen-holder-top-right-inside-point
                            (web-post-point-bottom thumb-tl-place :tr :degrees)
                            (* (dec multiplier) steps))
          thumb-line-inner-floor (global-curve-interp-with-end-unit-derivatives-curve
                                  [(web-post-point-bottom thumb-bl-place :lm :degrees)
                                   (web-post-point-bottom thumb-bl-place :rm :degrees)
                                   (web-post-point-bottom thumb-tl-place :lm :degrees)]
                                  3
                                  screen-holder-bottom-right-inside-point
                                  (web-post-point-bottom thumb-bl-place :rm :degrees)
                                  (* (dec multiplier) steps))
          outer-screen-to-thumb (vec (concat outer-horizontal-catmull-curve thumb-line-outer))
          inner-screen-to-thumb (vec (concat inner-horizontal-catmull-curve thumb-line-inner))
          outer-screen-to-thumb-control (vec (concat thumb-catmull-control-outer thumb-line-outer-floor))
          inner-screen-to-thumb-control (vec (concat thumb-catmull-control-inner thumb-line-inner-floor))
          test-curve (global-curve-interp-with-end-unit-derivatives-curve
                      [screen-holder-top-right-outside-point
                       (:point-on-tangent-from-plate thumb-bl-tl-points)
                       (web-post-point-top  thumb-bl-place :tl :degrees)
                       (web-post-point-top  thumb-bl-place :tm :degrees)
                       (web-post-point-top  thumb-bl-place :tr :degrees)
                       (web-post-point-top  thumb-tl-place :tl :degrees)]
                      3
                      screen-holder-top-left-outside-point
                      (web-post-point-top  thumb-tl-place :tr :degrees)
                      steps)
          test-curve-inner (global-curve-interp-with-end-unit-derivatives-curve
                            [screen-holder-top-right-inside-point
                             (:point-on-tangent-from-plate-bottom thumb-bl-tl-points)
                             (web-post-point-bottom  thumb-bl-place :tl :degrees)
                             (web-post-point-bottom  thumb-bl-place :tm :degrees)
                             (web-post-point-bottom  thumb-bl-place :tr :degrees)
                             (web-post-point-bottom  thumb-tl-place :tl :degrees)]
                            3
                            screen-holder-top-left-inside-point
                            (web-post-point-bottom  thumb-tl-place :tr :degrees)
                            steps)
          test-curve-control-outer (catmull-rom-spline-curve
                                    [screen-holder-bottom-left-outside-point
                                     screen-holder-bottom-right-outside-point
                                     ;(:point-on-tangent-from-plate thumb-bl-lm-points)
                                     (web-post-point-top  thumb-bl-place :lm :degrees)
                                     ;(web-post-point-top  thumb-bl-place :centre :degrees)
                                     (web-post-point-top  thumb-bl-place :rm :degrees)
                                     (web-post-point-top  thumb-tl-place :lm :degrees)
                                     (web-post-point-top  thumb-tl-place :centre :degrees)
                                     (web-post-point-top  thumb-tl-place :rm :degrees)
                                     (web-post-point-top  thumb-tr-place :lm :degrees)
                                     (web-post-point-top  thumb-tr-place :rm :degrees)]
                                    steps-times-6
                                    :alpha-type :centripetal)
          test-curve-control-inner (catmull-rom-spline-curve
                                    [screen-holder-bottom-left-inside-point
                                     screen-holder-bottom-right-inside-point
                                     ;(:point-on-tangent-from-plate-bottom thumb-bl-lm-points)
                                     (web-post-point-bottom  thumb-bl-place :lm :degrees)
                                     ;(web-post-point-bottom  thumb-bl-place :centre :degrees)
                                     (web-post-point-bottom  thumb-bl-place :rm :degrees)
                                     (web-post-point-bottom  thumb-tl-place :rm :degrees)
                                     (web-post-point-bottom  thumb-tr-place :lm :degrees)
                                     (web-post-point-bottom  thumb-tr-place :rm :degrees)]
                                    steps-times-6
                                    :alpha-type :chordal)

          crom-test (catmull-rom-spline-curve [screen-holder-top-left-outside-point
                                               screen-holder-top-right-outside-point
                                               (web-post-point-top thumb-bl-place :tl :degrees)
                                               (web-post-point-top thumb-bl-place :tm :degrees)
                                               (web-post-point-top thumb-bl-place :tr :degrees)
                                               (web-post-point-top thumb-tl-place :tl :degrees)
                                               (web-post-point-top thumb-tl-place :tm :degrees)
                                               (web-post-point-top thumb-tl-place :tr :degrees)
                                               (web-post-point-top thumb-tr-place :tl :degrees)]
                                              steps-times-6
                                              :alpha-type :chordal)
          crom-test-inner (catmull-rom-spline-curve [screen-holder-top-left-inside-point
                                                     screen-holder-top-right-inside-point
                                                     (web-post-point-bottom thumb-bl-place :tl :degrees)
                                                     (web-post-point-bottom thumb-bl-place :tm :degrees)
                                                     (web-post-point-bottom thumb-bl-place :tr :degrees)
                                                     (mapv + [0 0 0] (web-post-point-bottom thumb-tl-place :tl :degrees))
                                                     (mapv + [0 0 0] (web-post-point-bottom thumb-tl-place :tm :degrees))
                                                     (web-post-point-bottom thumb-tl-place :tr :degrees)
                                                     (web-post-point-bottom thumb-tr-place :tl :degrees)]
                                                    steps-times-6
                                                    :alpha-type :chordal)
          top-curve-test-nurbs-points [tps-65-top-left-outer
                                       tps-65-mid-left-outer
                                       tps-65-bottom-left-outer
                                       (web-post-point-top (partial key-place 0 cornerrow) :bl :radians)
                                       (web-post-point-top (partial key-place 0 cornerrow) :bm :radians)

                                       (mapv - (web-post-point-top (partial key-place 0 cornerrow) :br :radians)
                                             (mapv #(/ % (magnitude %))
                                                   (mapv - (web-post-point-top (partial key-place 1 cornerrow) :bl :radians) (web-post-point-top (partial key-place 0 cornerrow) :br :radians))))

                                       (web-post-point-top (partial key-place 0 cornerrow) :br :radians)]
          top-curve-test-nurbs-degree 3
          top-curve-test-nurbs-knot-vector   (let [denom 6]
                                               (mapv (partial * 4)
                                                     [0 0 0 0  (/ 2 denom) (/ 2.5 denom)  (/ 4 denom) 1 1 1 1])) 
          top-curve-test-nurbs-weights [1 1 10 1 1 0.5 1]
          top-curve-test-nurbs (nurbs
                                top-curve-test-nurbs-points
                                top-curve-test-nurbs-degree
                                 top-curve-test-nurbs-knot-vector
                                top-curve-test-nurbs-weights
                                steps-times-6)
          top-curve-test-nurbs-control (nurbs
                                        [tps-65-top-mid-outer
                                         tps-65-centre-outer
                                         tps-65-bottom-mid-outer
                                         (web-post-point-top (partial key-place 0 (dec cornerrow)) :bl :radians)
                                         (web-post-point-top (partial key-place 0 (dec cornerrow)) :bm :radians)
                                         (web-post-point-top (partial key-place 0 (dec cornerrow)) :br :radians)]
                                        3
                                        (let [denom 3]
                                          (mapv (partial * 3)
                                                [0 0 0 0 (/ 1 denom) (/ 2 denom)  1 1 1 1]))
                                        [1 1 10 1 1 1]
                                        steps-times-6)
          top-curve-test-nurbs-inner-points [tps-65-top-left-inner
                                             tps-65-mid-left-inner
                                             tps-65-bottom-left-inner
                                             (web-post-point-bottom (partial key-place 0 cornerrow) :bl :radians)
                                             (web-post-point-bottom (partial key-place 0 cornerrow) :bm :radians)
                                             (web-post-point-bottom (partial key-place 0 cornerrow) :br :radians)]
          top-curve-test-nurbs-inner-degree 3
          top-curve-test-nurbs-inner-knot-vector (let [denom 3]
                                                   (mapv (partial * denom)
                                                         [0 0 0 0  (/ 1 denom) (/ 2 denom) 1 1 1 1]))
          top-curve-test-nurbs-inner-weights [1 1 0.5 1 1 1]
          top-curve-test-nurbs-inner (nurbs
                               top-curve-test-nurbs-inner-points
                                top-curve-test-nurbs-inner-degree
                                      top-curve-test-nurbs-inner-knot-vector
                                top-curve-test-nurbs-inner-weights
                                steps-times-6)
top-curve-test-nurbs-control-inner (nurbs
                              [tps-65-top-right-inner
                               tps-65-mid-right-inner
                               tps-65-bottom-right-inner
                               (web-post-point-bottom (partial key-place 0 0) :tl :radians)
                               (web-post-point-bottom (partial key-place 0 0) :tm :radians)
                               (web-post-point-bottom (partial key-place 0 0) :tr :radians)]
                              2
                              (let [denom 5]
                                (mapv (partial * (- denom 1))
                                      [0 0 0  (/ 1 denom) (/ 2 denom) (/ 3 denom) 1 1 1]))
                              [1 1 1 1 1 1]
                              steps-times-6)
check (calculate-nurbs-curve-point (dec (count top-curve-test-nurbs-points)) top-curve-test-nurbs-degree top-curve-test-nurbs-knot-vector
                                           (homogenize-cooridinates  top-curve-test-nurbs-points
                                                                    top-curve-test-nurbs-weights)
                                            4)
top-curve-test-end-derivative (nurbs-deriv-deboor (dec (count top-curve-test-nurbs-points)) top-curve-test-nurbs-degree top-curve-test-nurbs-knot-vector
                                                  top-curve-test-nurbs-points
                           4 top-curve-test-nurbs-weights )
top-curve-test-end-derivative-inner (nurbs-deriv-deboor (dec (count top-cu)) top-curve-test-nurbs-degree top-curve-test-nurbs-knot-vector
                                                  top-curve-test-nurbs-points
                                                  4 top-curve-test-nurbs-weights)
thumb-tl-tr-to-tr-tl-catmull-outer (catmull-rom-spline-curve
                              [(web-post-point-top thumb-tl-place :tl :degrees)
                              (web-post-point-top thumb-tl-place :tr :degrees)
                              (web-post-point-top thumb-tr-place :tl :degrees)
                              (web-post-point-top thumb-tr-place :tr :degrees)]
                              steps)

thumb-tl-tr-to-tr-tl-catmull-inner (catmull-rom-spline-curve
                                    [(web-post-point-bottom thumb-tl-place :tl :degrees)
                                    (web-post-point-bottom thumb-tl-place :tr :degrees)
                                    (web-post-point-bottom thumb-tr-place :tl :degrees)
                                    (web-post-point-bottom thumb-tr-place :tr :degrees)] 
                                    steps)
inner-index-corrnerrow-br-to-index-cornerrow-bl-outer (cubic-hermite-tension-spline-curve 
                                                       (web-post-point-top (partial key-place 0 cornerrow) :br :radians)
                                                       (web-post-point-top (partial key-place 1 cornerrow) :bl :radians)
                                                      (nth  top-curve-test-end-derivative 1)
                                                        (mapv #(/ % (magnitude %)) (mapv - (web-post-point-top (partial key-place 1 cornerrow) :br :radians)
                                                                                              (web-post-point-top (partial key-place 1 cornerrow) :bl :radians)))
                                                        1
                                                        steps)
inner-index-corrnerrow-br-to-index-cornerrow-bl-inner (cubic-hermite-tension-spline-curve
                                                       (web-post-point-bottom (partial key-place 0 cornerrow) :br :radians)
                                                       (web-post-point-bottom (partial key-place 1 cornerrow) :bl :radians)
                                                       (nth  top-curve-test-end-derivative 1)
                                                       (mapv #(/ % (magnitude %)) (mapv - (web-post-point-bottom (partial key-place 1 cornerrow) :br :radians)
                                                                                        (web-post-point-bottom (partial key-place 1 cornerrow) :bl :radians)))
                                                       1
                                                       steps)
          outer-wall (vec (for [index (range  (inc  steps-times-6))]
                 
                     (global-curve-interp-with-end-unit-derivatives-curve
                      [
                       (nth crom-test index)
                       (nth top-curve-test-nurbs index)] 
                      3
                      (nth test-curve-control-outer index)
                      (nth top-curve-test-nurbs-control index)
                      steps 
                      )))
          inner-wall (vec (for [index (range (inc  steps-times-6))]
                            (global-curve-interp-with-end-unit-derivatives-curve
                             [
                              (nth top-curve-test-nurbs-inner index)
                              (nth crom-test-inner index)
                              ]
                             3
                             (nth top-curve-test-nurbs-control-inner index)
                             (nth test-curve-control-inner index)
                             steps 
                      )))
          lower-crom (catmull-rom-spline-curve [screen-holder-top-left-outside-point
                                                screen-holder-top-right-outside-point
                                                (web-post-point-top thumb-bl-place :tl :degrees)
                                                (web-post-point-top thumb-bl-place :tr :degrees)]
                                               steps
                                               :alpha-type :chordal)
          lower-crom-inner (catmull-rom-spline-curve [screen-holder-top-left-inside-point
                                                      screen-holder-top-right-inside-point
                                                      (web-post-point-bottom thumb-bl-place :tl :degrees)
                                                      (web-post-point-bottom thumb-bl-place :tr :degrees)]
                                                     steps
                                                     :alpha-type :chordal)
          bottom-outer-2 (catmull-rom-spline-curve
                          [screen-holder-bottom-left-outside-point
                           screen-holder-bottom-right-outside-point
                           (:wall-locate3-point-floor thumb-bl-tl-points)
                           (:wall-locate3-point-floor thumb-bl-lm-points)]
                          steps)
          bottom-inner-2 (catmull-rom-spline-curve
                          [screen-holder-bottom-left-inside-point
                           screen-holder-bottom-right-inside-point
                           (:wall-locate-2-bottom-floor thumb-bl-tl-points)
                           (:wall-locate-2-bottom-floor thumb-bl-lm-points)]
                          steps)
          control-bottom-outer-2 (catmull-rom-spline-curve
                                  [(mapv + [0 0 (- plate-thickness)] screen-holder-bottom-left-outside-floor-point)
                                   (mapv + [0 0 (- plate-thickness)] screen-holder-bottom-right-outside-point)
                                   (:wall-locate3-point-below-floor thumb-bl-tl-points)
                                   (:wall-locate3-point-below-floor thumb-bl-lm-points)]
                                  steps)
          control-bottom-inner-2 (catmull-rom-spline-curve
                                  [(mapv + [0 0 (- plate-thickness)] screen-holder-bottom-left-inside-point)
                                   (mapv + [0 0 (- plate-thickness)] screen-holder-bottom-right-inside-point)
                                   (:wall-locate-2-bottom-below-floor thumb-bl-tl-points)
                                   (:wall-locate-2-bottom-below-floor thumb-bl-lm-points)]
                                  steps)
          outer-wall-2 (vec (for [index (range (inc steps))]
                              (catmull-rom-spline-curve
                               [(nth control-bottom-outer-2 index)
                                (nth bottom-outer-2 index)
                                (nth lower-crom index)
                                (nth tps-65-line-to-first-column-br-outer index)]
                               steps)))
          inner-wall-2 (vec (for [index (range (inc steps))]
                              (catmull-rom-spline-curve
                               [(nth tps-65-line-inner index)
                                (nth lower-crom-inner index)
                                (nth  bottom-inner-2 index)
                                (nth control-bottom-inner-2 index)]
                               steps)))
          bottom-section-screen-side-control-outer (screen-side-catmull
                                                    tps-65-top-left-outer
                                                    screen-holder-top-left-outside-point
                                                    screen-holder-bottom-left-outside-point
                                                    screen-holder-bottom-left-outside-floor-point
                                                    (mapv + [0 0 (- plate-thickness)] screen-holder-bottom-left-outside-floor-point)
                                                    steps)
          bottom-section-screen-side-control-inner (screen-side-catmull
                                                    tps-65-top-left-inner
                                                    screen-holder-top-left-inside-point
                                                    screen-holder-bottom-left-inside-point
                                                    screen-holder-bottom-left-inside-floor-point
                                                    (mapv + [0 0 (- plate-thickness)] screen-holder-bottom-left-inside-floor-point)

                                                    steps)
          bottom-section-screen-side-outer (screen-side-catmull
                                            tps-65-top-left-outer
                                            screen-holder-top-right-outside-point
                                            screen-holder-bottom-right-outside-point
                                            screen-holder-bottom-right-outside-floor-point
                                            (mapv + [0 0 (- plate-thickness)] screen-holder-bottom-right-outside-floor-point)

                                            steps)
          bottom-section-screen-side-inner (screen-side-catmull
                                            tps-65-top-left-inner
                                            screen-holder-top-right-inside-point
                                            screen-holder-bottom-right-inside-point
                                            screen-holder-bottom-right-inside-floor-point
                                            (mapv + [0 0 (- plate-thickness)] screen-holder-bottom-right-inside-floor-point)
                                            steps)

          bottom-section-thumb-side-outer  (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tl-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                              3 default-weights-for-vertical-nurbs steps)
          bottom-section-thumb-side-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tl-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps)
          bottom-section-thumb-side-outer-control (bezier-linear (web-post-point-top  thumb-bl-place :tr :degrees) (web-post-point-top  thumb-bl-place :br :degrees) steps)
          ;; (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tr-points wall-vertical-outer-nurbs-control-points-keywords)
          ;;                                                                             3 default-weights-for-vertical-nurbs steps)
          bottom-section-thumb-side-inner-control (bezier-linear (web-post-point-bottom  thumb-bl-place :tr :degrees) (web-post-point-bottom  thumb-bl-place :br :degrees) steps)
          ;; (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tr-points inner-wall-curve-bezier-cubic-nurbs-keywords)
          ;;                                                                steps)
          make-sure-bottom-not-below-zero (fn [wall]
                                            (let [wall-size (count wall)
                                                  last-row (dec wall-size)]
                                              (assoc wall last-row
                                                     (mapv #(assoc % 2 0.0) (nth wall last-row)))))
          bottom-section-outer-wall (->> (vec (for [index (range (inc steps))]
                                                (catmull-rom-spline-curve
                                                 [(nth  bottom-section-screen-side-control-outer index)
                                                  (nth bottom-section-screen-side-outer index)
                                                  (nth bottom-section-thumb-side-outer index)
                                                  (nth  bottom-section-thumb-side-outer-control index)]
                                                 steps
                                            ;:alphaType  :chordal
                                                 )))
                                         make-sure-bottom-not-below-zero)
          bottom-section-inner-wall (->> (vec (for [index (range (inc steps))]
                                                (catmull-rom-spline-curve
                                                 [(nth  bottom-section-thumb-side-inner-control index)
                                                  (nth bottom-section-thumb-side-inner index)
                                                  (nth bottom-section-screen-side-inner index)
                                                  (nth  bottom-section-screen-side-control-inner index)]
                                                 steps
                                            ;:alphaType :chordal
                                                 )))
                                         make-sure-bottom-not-below-zero)
          vnf-array (wall-vnf-array outer-wall inner-wall {:caps false :cap1 false :cap2 false :col-wrap false :row-wrap true :reverse true :style :default})
          vnf-array-lower (wall-vnf-array outer-wall-2 inner-wall-2 {:caps false :cap1 false :cap2 false :col-wrap false :row-wrap true :reverse true :style :quincunx})
          vnf-array-bottom-section (wall-vnf-array bottom-section-outer-wall bottom-section-inner-wall {:caps false :cap1 false :cap2 false :col-wrap false :row-wrap true :reverse true :style :alt})
          top-curve-test (local-cubic-curve-interpolation-with-calculated-tangents-curve
                          [tps-65-top-left-outer
                           tps-65-mid-left-outer
                           tps-65-bottom-left-outer
                           (web-post-point-top (partial key-place 0 cornerrow) :bl :radians)
                           (web-post-point-top (partial key-place 0 cornerrow) :bm :radians)
                           (web-post-point-top (partial key-place 0 cornerrow) :br :radians)]
                          steps-times-6)
          top-curve-test-global (global-curve-interp-with-end-unit-derivatives-curve
                                 [tps-65-top-left-outer
                                  tps-65-mid-left-outer
                                  tps-65-bottom-left-outer
                                  (web-post-point-top (partial key-place 0 cornerrow) :bl :radians)
                                  (web-post-point-top (partial key-place 0 cornerrow) :bm :radians)
                                  (web-post-point-top (partial key-place 0 cornerrow) :br :radians)]
                                 2
                                 tps-65-top-mid-outer
                                 (web-post-point-top (partial key-place 1 cornerrow) :bl :radians)
                                 steps-times-6)]
          (union
          ;(plot-bezier-points top-curve-test (sphere 1))
           ;(color [1 0 0 1] (plot-bezier-points top-curve-test-global (sphere 1)))
           ;(color [0 1 0 1] (plot-bezier-points top-curve-test-nurbs (sphere 1)))
           ;(color [0 1 0 1] (plot-bezier-points top-curve-test-nurbs (sphere 0.25)))
           (color [0 1 0 1] (plot-bezier-points top-curve-test-nurbs-control (sphere 1))) 
           (color [1 0 0 1] (translate tps-65-bottom-mid-outer (sphere 2)))
           ;(color [1 0 01] (translate check (sphere 2)))
           (plot-bezier-points thumb-tl-tr-to-tr-tl-catmull-outer (sphere 0.5))
           (plot-bezier-points inner-index-corrnerrow-br-to-index-cornerrow-bl-outer (sphere 0.5))
           ;(println "bottom-section-outer-wall bottom" (peek bottom-section-outer-wall))
           (println "bottom-section-inner-wall " (peek bottom-section-inner-wall))
           (println "nurps-key" (get-curve-control-points-by-key-words thumb-bl-tl-points wall-vertical-outer-nurbs-control-points-keywords))
           (-#  (thumb-tl-place des-cornelus-c3l))
           (-# (thumb-bl-place des-cornelus-c3l))
          ; (plot-bezier-points crom-test (sphere 0.5))
           ;         (plot-bezier-points test-curve (sphere 0.5))
          ;;  (for [index (range (inc steps))]
          ;;    (color [(+ 0 (/ index steps)) 0 0 1] (translate (nth thumb-nurbs index) (sphere 0.5)))
          ;;    )
           (println "point-diff" (mapv - tps-65-bottom-left-outer check))
           ;(translate  tps-65-bottom-left-outer (cube 1 1 1))
           (vnf-polyhedron vnf-array )
          ; (vnf-polyhedron vnf-array-bottom-section)
           ;(tps-65-to-keys steps)
          ;;  (vnf-polyhedron (wall-vnf (wall-section
          ;;                             (wall-section-parameter
          ;;                              [(wall-cross-section-parameter (screen-holder-wall-position :sl) :outer-wall-parameters (outer-wall-screen-catmull-rom-parameters)
          ;;                                                             :inner-wall-parameters (inner-wall-screen-catmull-rom-parameters))
          ;;                               (wall-cross-section-parameter (screen-holder-wall-position :sr) :outer-wall-parameters (outer-wall-screen-catmull-rom-parameters)
          ;;                                                             :inner-wall-parameters (inner-wall-screen-catmull-rom-parameters))
          ;;                               (wall-cross-section-parameter (thumb-wall-position thumb-bl-place -1 0 :tl :xy 5))
          ;;                               (wall-cross-section-parameter (thumb-wall-position thumb-bl-place -1 0 :tr :xy 5))]
          ;;                              (n-degree-bezier-curve-paramaters ;:linear-outer-top true :linear-inner-top true
          ;;                                                                                          )
          ;;                           ;:calculation-order :vertical-first
          ;;                              )
          ;;                             30 30)
          ;;                            {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
           
           (vnf-polyhedron (wall-vnf (wall-section
                                      (wall-section-parameter
                                       [;(wall-cross-section-parameter (thumb-wall-position thumb-bl-place -1 -1 :tl :xy 5 :offset [-0.00001 0 0]))
                                        (wall-cross-section-parameter (thumb-wall-position thumb-bl-place -1 0 :tl :xy 5))
                                        (wall-cross-section-parameter (thumb-wall-position thumb-bl-place -1 0 :lm :xy 5))
                                        (wall-cross-section-parameter (thumb-wall-position thumb-br-place -1 0 :tl :xy 5))
                                        (wall-cross-section-parameter (thumb-wall-position thumb-br-place -1 0 :bl :xy 5))]
                                       (global-curve-interpolation-with-end-derivatives-parameters 2
                                                                                                   (wall-cross-section-parameter (thumb-wall-position thumb-bl-place -1 1 :lm :xy 5 ;:offset [(- (+ keyswitch-width extra-width)) 0 0]
                                                                                                                                                      ))
                                                                                                   (wall-cross-section-parameter (thumb-wall-position thumb-br-place -1 -1 :bl :xy 5 :offset [-0.00001 -0.00001 0]) )  ;:linear-outer-top true :linear-inner-top true
                                                                                                   )
                                       
                                       )
                                      30 30)
                                     {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
           )
          )
        (mapv #(vnf-polyhedron (vnf-vertex-array % {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :alt})) (generate-polyhedron-main-body-connecters))
        thumb-type
        key-holes
        (tps-65-place (difference  tps-65-mount-new
                                   tps-65
                                   tps-65-mount-cutout
                                   (translate [0 0 (+ 0 1)] tps-65-mount-main-cutout)))
        ;; (vnf-polyhedron (wall-vnf (wall-section
        ;;                            (wall-section-parameter 
        ;;                             [(wall-cross-section-parameter (screen-holder-wall-position :sl) :outer-wall-parameters
        ;;                                                            (outer-wall-screen-catmull-rom-parameters)
        ;;                                                            :inner-wall-parameters
        ;;                                                            (inner-wall-screen-catmull-rom-parameters))
        ;;                              (wall-cross-section-parameter (screen-holder-wall-position :sr) :outer-wall-parameters
        ;;                                                            (outer-wall-screen-catmull-rom-parameters)
        ;;                                                            :inner-wall-parameters
        ;;                                                            (inner-wall-screen-catmull-rom-parameters))
        ;;                              (wall-cross-section-parameter (thumb-wall-position thumb-bl-place -1 0 :tl :xy 5))
        ;;                              (wall-cross-section-parameter (thumb-wall-position thumb-bl-place -1 0 :lm :xy 5))
        ;;                              ]
        ;;                             (catmull-rom-spline-parameters ;:linear-outer-top true :linear-inner-top true
        ;;                                                                    )
        ;;                             :calculation-order :vertical-first
        ;;                             )
        ;;                            30 30)
        ;;                           {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
        )
       )
      )


(spit "things-low/connector-test.scad"
      (write-scad
       (include "../BOSL2/std.scad")
       (union
        ;key-holes
;;         (vnf-polyhedron (vnf-vertex-array (generate-polyhedron-web-connecters-curve
;;          (main-body-key-corner 0 0 :tr )
;;          (main-body-key-corner 0 0 :br )
;;          (main-body-key-corner 0 1 :tr )
;; (main-body-key-corner 0 1 :br )
;;          (main-body-key-corner 1 0 :tl )
;; (main-body-key-corner 1 0 :bl )
;; (main-body-key-corner 1 1 :tl )
;; (main-body-key-corner 1 1 :bl )
;;          :curve-type :catmull)
;;                   default-vnf-vertex-array-args))
        
;;         (vnf-polyhedron (vnf-vertex-array (generate-polyhedron-web-connecters-curve
                                           
;;                                            (main-body-key-corner 0 1 :bl)
;;                                            (main-body-key-corner 0 1 :br)
;;                                            (main-body-key-corner 1 1 :bl)
;;                                            (main-body-key-corner 1 1 :br)
                                           
;;                                            (main-body-key-ocorner 0 1 :tl)
                                           
;;                                            (main-body-key-corner 0 1 :tr)
                                           
                                           
;;                                            (main-body-key-corner 1 1 :tl)
                                           
;;                                            (main-body-key-corner 1 1 :tr)
;;                                            :curve-type :catmull)
;;                                           default-vnf-vertex-array-args))
        
          (mapv #(vnf-polyhedron (vnf-vertex-array % {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :alt})
                                 ) (generate-polyhedron-main-body-connecters)) 
       ; (mapv #(vnf-polyhedron (vnf-vertex-array % default-vnf-vertex-array-args)) (generate-all-polyhedron-row-column-crossroads-connecters :curve-type :catmull))
          ;(key-web-connecters-polyhedron 20)
          
        )
       ))
(comment (subvec [[0 0 0] [2 4 0] [4 1 0] [7 5 0] [10 0 0]] 1 4))
(spit "things-low/nurbs-tangent-test.scad"
      (write-scad 
       (let [points [[0 0 0] [2 4 0] [4 1 0] [7 5 0] [10 0 0]]
             t1 [-2 3 0]
             t2 [12 -3 0]
             t1-vector (mapv - (nth points 0) t1)
             t1-point (mapv + (nth points 0) (mapv #(/ % (magnitude t1-vector)) t1-vector))
             t2-vector (mapv - t2 (last points))
             t2-point (mapv - (last points) (mapv #(/ % (magnitude t2-vector)) t2-vector))
             new-points (vec (apply concat [(first points) t1-point]  (subvec points 1 4) [[t2-point][ (last points)]]))
             nurbs-curve (nurbs new-points 3 (mapv (partial * 4) [0 0 0 0 (/ 1 6) (/ 3 6) (/ 5 6) 1 1 1 1]) [1 1 0.5 0.5 0.5 1 1] 30)
             ]
         (println new-points)
         (union 
          (color [0 1 0 1](plot-bezier-points points (sphere 0.5)))
          (color [1 0 0 1](translate t1 (sphere 0.5)))
(color [1 0 0 1](translate t2 (sphere 0.5)))
          (plot-bezier-points nurbs-curve (sphere 0.5))
          )
         
         )))

(spit "things-low/tps-65-redo-test.scad"
      (write-scad
       (include "../BOSL2/std.scad")
       (union
        ;(-# tps-65-mount)
        (difference 
         tps-65-mount-new
                     tps-65-mount-cutout
                    (translate [0 0 (+ 0 1)] tps-65-mount-main-cutout)
          )
        ;tps-65-mount-base
        ; tps-65-mount
        )))

(comment (local-cubic-curve-interpolation-with-calculated-tangents
[[0 0 0] [2 3 8] [4 3 9]]))
(defn -main [dum] 1)  ; dummy to make it easier to batch