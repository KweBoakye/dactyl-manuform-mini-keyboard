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
            [dactyl-keyboard.dovetail :refer :all]))










;; ;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Placement Functions ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def columns (range 0 ncols))
;; (def rows (range 0 nrows))

;; (def cap-top-height (+ plate-thickness sa-profile-key-height))
;; (def row-radius (+ (/ (/ (+ mount-height extra-height) 2)
;;                       (Math/sin (/ α 2)))
;;                    cap-top-height))
;; (def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
;;                          (Math/sin (/ β 2)))
;;                       cap-top-height))
;; (def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))

;; (defn offset-for-column [col]
;;   (if (and (true? pinky-15u) (= col lastcol)) 5.5 0))
;; (defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
;;   (let [column-angle (* β (- centercol column))
;;         placed-shape (->> shape
;;                           (translate-fn [(offset-for-column column) 0 (- row-radius)])
;;                           (rotate-x-fn  (* α (- centerrow row)))
;;                           (translate-fn [0 0 row-radius])
;;                           (translate-fn [0 0 (- column-radius)])
;;                           (rotate-y-fn  column-angle)
;;                           (translate-fn [0 0 column-radius])
;;                           (translate-fn (column-offset column)))
;;         column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
;;         placed-shape-ortho (->> shape
;;                                 (translate-fn [0 0 (- row-radius)])
;;                                 (rotate-x-fn  (* α (- centerrow row)))
;;                                 (translate-fn [0 0 row-radius])
;;                                 (rotate-y-fn  column-angle)
;;                                 (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
;;                                 (translate-fn (column-offset column)))
;;         placed-shape-fixed (->> shape
;;                                 (rotate-y-fn  (nth fixed-angles column))
;;                                 (translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
;;                                 (translate-fn [0 0 (- (+ row-radius (nth fixed-z column)))])
;;                                 (rotate-x-fn  (* α (- centerrow row)))
;;                                 (translate-fn [0 0 (+ row-radius (nth fixed-z column))])
;;                                 (rotate-y-fn  fixed-tenting)
;;                                 (translate-fn [0 (second (column-offset column)) 0]))]
;;     (->> (case column-style
;;            :orthographic placed-shape-ortho
;;            :fixed        placed-shape-fixed
;;            placed-shape)
;;          (rotate-y-fn  tenting-angle)
;;          (translate-fn [0 0 keyboard-z-offset]))))

;; (defn key-place [column row shape]
;;   (apply-key-geometry translate
;;                       (fn [angle obj] (rotate angle [1 0 0] obj))
;;                       (fn [angle obj] (rotate angle [0 1 0] obj))
;;                       column row shape))

;; (defn rotate-around-x [angle position]
;;   (mmul
;;    [[1 0 0]
;;     [0 (Math/cos angle) (- (Math/sin angle))]
;;     [0 (Math/sin angle)    (Math/cos angle)]]
;;    position))

;; (defn rotate-around-y [angle position]
;;   (mmul
;;    [[(Math/cos angle)     0 (Math/sin angle)]
;;     [0                    1 0]
;;     [(- (Math/sin angle)) 0 (Math/cos angle)]]
;;    position))

;; (defn key-position [column row position]
;;   (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))

;; (def key-holes
;;   (apply union
;;          (for [column columns
;;                row rows
;;                :when (or (.contains [2 3] column)
;;                          (not= row lastrow))]
;;            (->> single-plate
;;                 (key-place column row)))))

;; (def caps
;;   (apply union
;;          (for [column columns
;;                row rows
;;                :when (or (.contains [2 3] column)
;;                          (not= row lastrow))]
;;            (->> (sa-cap (if (and (true? pinky-15u) (= column lastcol)) 1.5 1))
;;                 (key-place column row)))))

;; ;;;;;;;;;;;;;;;;;;;;
;; ;; Web Connectors ;;
;; ;;;;;;;;;;;;;;;;;;;;

;; (def web-thickness 4.5)
;; (def post-size 0.1)
;; (def web-post (->> (cube post-size post-size web-thickness)
;;                    (translate [0 0 (+ (/ web-thickness -2)
;;                                       plate-thickness)])))

;; (def post-adj (/ post-size 2))
;; (def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
;; (def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
;; (def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
;; (def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))

;; ; wide posts for 1.5u keys in the main cluster

;; (if (true? pinky-15u)
;;   (do (def wide-post-tr (translate [(- (/ mount-width 1.2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
;;       (def wide-post-tl (translate [(+ (/ mount-width -1.2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
;;       (def wide-post-bl (translate [(+ (/ mount-width -1.2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
;;       (def wide-post-br (translate [(- (/ mount-width 1.2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post)))
;;   (do (def wide-post-tr web-post-tr)
;;       (def wide-post-tl web-post-tl)
;;       (def wide-post-bl web-post-bl)
;;       (def wide-post-br web-post-br)))

;; (defn triangle-hulls [& shapes]
;;   (apply union
;;          (map (partial apply hull)
;;               (partition 3 1 shapes))))

;; (def connectors
;;   (apply union
;;          (concat
;;           ;; Row connections
;;           (for [column (range 0 (dec ncols))
;;                 row (range 0 lastrow)]
;;             (triangle-hulls
;;              (key-place (inc column) row web-post-tl)
;;              (key-place column row web-post-tr)
;;              (key-place (inc column) row web-post-bl)
;;              (key-place column row web-post-br)))

;;           ;; Column connections
;;           (for [column columns
;;                 row (range 0 cornerrow)]
;;             (triangle-hulls
;;              (key-place column row web-post-bl)
;;              (key-place column row web-post-br)
;;              (key-place column (inc row) web-post-tl)
;;              (key-place column (inc row) web-post-tr)))

;;           ;; Diagonal connections
;;           (for [column (range 0 (dec ncols))
;;                 row (range 0 cornerrow)]
;;             (triangle-hulls
;;              (key-place column row web-post-br)
;;              (key-place column (inc row) web-post-tr)
;;              (key-place (inc column) row web-post-bl)
;;              (key-place (inc column) (inc row) web-post-tl))))))

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
(def model-right (difference
                  (union
                   key-holes
                   (-# pinky-connectors)
                   ;pinky-walls
                   connectors

                   ;(hull
                  ;  aviator-neck
                   ;aviator-neck-support-left
                   ;aviator-neck-support-right
                  ; (-# aviator-assembly)
                   ;)
                  ;(color [1 0 0 1] pcb-place))
                ; (color [0 1 0 1] (thumb-1x-layout pcb))
                ; (color [0 1 0 1] (thumb-15x-layout  pcb))
                   (EVQWGD001-place EVQWGD001-holder)
                 ; ( -#(thumb-b1-place-multmatrix (cube 5 5 5)))

                 ; (pico-standoffs-place pico-standoffs)
                 ;  (IS31FL3743A-standoff-place IS31FL3743A-standoffs)
                 ;   (color [1 0 0 1] aviator-male-connecter-clearence-test)
                 ;  (translate [0 -10 0] (color [0 1 0 1] aviator-female-connecter-clearence-test))
                ;(color [1 0 0 1](translate [-8 4 8] (left-wall-plate-place 0 (+ left-wall-y-modifier 0.3) (rotate (deg2rad -15) [1 0 0] ST7789-240x320))))
                ;(color [0 1 0 1] (translate [-8 4 8] (left-wall-plate-place 0 (+ left-wall-y-modifier 0.3) (rotate (deg2rad -15) [1 0 0] ST7789-240x320-display))))
                    ;(translate [4 0 8] (left-wall-plate-place 0 (+ left-wall-y-modifier 0.3) (rotate (deg2rad -15) [1 0 0] (rotate (deg2rad 90) [0 0 1] ST7789-240x240-154))))
                    ;(color [0 0 1 1] (translate [4 0 8] (left-wall-plate-place 0 (+ left-wall-y-modifier 0.3) (rotate (deg2rad -15) [1 0 0] (rotate (deg2rad 90) [0 0 1]  ST7789-240x240-154-display)))))

                  ; (color [1 1 0 1](translate [-8 0 8] (left-wall-plate-place 0 (+ left-wall-y-modifier 0.3) (rotate (deg2rad -15) [1 0 0] (rotate (deg2rad 90) [0 0 1] ST7789-240x240-13)))))
;(color [1 0 1 1] (translate [-8 0 8] (left-wall-plate-place 0 (+ left-wall-y-modifier 0.3) (rotate (deg2rad -15) [1 0 0] (rotate (deg2rad 90) [0 0 1]  ST7789-240x240-13-display)))))
                   ;(color [1 0 0 1] (tps-65-place tps-65))
                   thumb-wall-type
                   thumb-type
                   (difference
                    thumb-connector-type
                    (thumb-tr-place pcb-cutout)
                    (thumb-tr-place (translate [0 0 -2] pcb-cutout)))
                    ;left-section
                    ;;(cirque-TM040040-place cirque-TM040040-mount)
                   ;;(cirque-TM040040-thumb-place (translate [0 0 3](cylinder 20.57 12 :center false)))
                   ;(cirque-TM040040-thumb-place cirque-TM040040-mount)
                   (difference (union (difference
                                       case-walls
                                       usb-jack)
                                      (tps-65-place tps-65-mount)
                                      screw-insert-outers
                                     ;(rp2040-plus-place rp2040-plus)
                                      (color [1 0 0 1] (rp2040-plus-place rp2040-plus-mount))
                                      (tps-65-translate-and-place-at-position [10 0 (- vybronics-vl91022-z-axis)] (rdz -90 vybronics-vl91022-mount))
                                      ;pro-micro-holder
                                      ;usb-holder-holder
                                      ;trrs-holder
                                      )
                               (tps-65-place tps-65-mount-cutout)
                               (tps-65-place (translate [0 0 (+ 0 1)] tps-65-mount-main-cutout))
                               (cond
                                 (= screen-holder-mount-position "screen-holder-mount-top") (screen-holder-place screen-holder-cut)
                                 (= screen-holder-mount-position "screen-holder-mount-side") (screen-holder-place-side screen-holder-cut))

                               ;usb-holder-space

                               aviator-hole
                               aviator-recess-hole
                               ;trrs-holder-hole
                               screw-insert-holes))
                  (translate palm-hole-origin (palm-rest-hole-rotate palm-buckle-holes))
                  (translate [0 0 -20] (cube 350 350 40))))

(def gx16 (import "GX16-4P.STL"))

 ;(spit"things-low/multmatrix-test.scad"
 ;(write-scad (thumb-b1-place-multmatrix (cube 5 5 5))))
(spit "things-low/right.scad"
      (write-scad
       tps-65-includes
       aviator-includes
       ;(include "../BOSL/shapes.scad")
;(include "../BOSL/constants.scad")
       model-right))

(spit "things-low/left.scad"
      (write-scad
       tps-65-includes
       aviator-includes
       ;(include "../BOSL/shapes.scad")
;(include "../BOSL/constants.scad")
       (mirror [-1 0 0] model-right)))

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


;; (def bottom-plate-old
;;   (cut
;;    (translate [0 0 -0.1]
;;               (difference (union case-walls
;;                                  pinky-walls
;;                                  screw-insert-outers
;;                                  thumb-wall-type)
;;                           (translate [0 0 -10] screw-insert-screw-holes)))))

(def screen-test
  (difference
   (union
    screen-holder
    ;(-# (rdz 90 view-bezel))
   ; (rotate (deg2rad 0) [1 0 0] (rotate (deg2rad 90) [0 0 1] ST7789-240x240-154-holder-old))
    )
   screen-holder-cut))

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

;;  (spit "things-low/screen-test.scad"
;;        (write-scad screen-test))

;;  (spit "things-low/EVQWGD001-test.scad"
;;       (write-scad EVQWGD001-test))

(spit "things-low/vybronics-vl91022-mount.scad"
      (write-scad vybronics-vl91022-mount))

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



;; (spit "things-low/aviator-assembly.scad"
;;       (write-scad 

;; (difference
;;  aviator-assembly
;;  aviator-assembly-diffs

;;  )

                  ;;  ))
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

(defn back-left-wall-to-screen [{:keys [dx1 dy1 place1  post-position-1
                                        dxmid1 dymid1 place-mid1  post-position-mid1
                                        dxmid2 dymid2 place-mid2  post-position-mid2
                                        dx2 dy2 place2  post-position-2 
                                              steps] :or {steps 20}}]
  (let [screen-holder-top-left-outside-point (transform-position 
                                              (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                              (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
        screen-holder-top-left-inside-point (transform-position 
                                             (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                             (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
        screen-holder-bottom-left-outside-point (transform-position
                                                 (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                                 (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
        screen-holder-bottom-left-inside-point (transform-position
                                                (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                                (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
        tps-65-top-right-web-post-outside-point (transform-position
                                                 (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset)
                                                 [0 0 (/ web-thickness 2)])
        screen-holder-bottom-left-outside-floor-point (assoc (vec screen-holder-bottom-left-outside-point) 2 0)
        screen-holder-bottom-left-inside-floor-point (assoc (vec screen-holder-bottom-left-inside-point) 2 0)
        tps-65-top-right-web-post-inside-point (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset [0 0 (- (/ web-thickness))])
        outside-vertical-curve-origin [0 0 (/ curve-post-size 2)]
        tps-65-top-right-wall-locate3-outside-point (transform-position
                                                     (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset)
                                                     (mapv + (wall-locate3-xy-for-polyhedron-point 1 0 wall-xy-offset) [(/ curve-post-size 2) (/ curve-post-size 1) 0]
                                                           curve-post-translation-vector))
        tps-65-top-right-wall-locate3-outside-control-point (transform-position
                                                             (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset)
                                                             (mapv + (wall-locate3-xy-for-polyhedron-point 0 1 wall-xy-offset) [0 (/ curve-post-size 1)  0] curve-post-translation-vector))
        tps-65-top-right-wall-locate2-inside-point (transform-position
                                                    (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset)
                                                    (mapv + (wall-locate2-xy 1 0 wall-xy-offset) [(/ curve-post-size -1) curve-post-size  0] oled-translation-vector))
        tps-65-top-right-wall-locate2-inside-control-point (transform-position
                                                            (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset)
                                                            (mapv + (wall-locate2-xy 0 1 wall-xy-offset) [(/ curve-post-size -1) curve-post-size 0] oled-translation-vector))
        tps-65-top-right-inside-level-with-screen-bottom-left (assoc (vec tps-65-top-right-wall-locate2-inside-point) 2 (last screen-holder-bottom-left-inside-point))
        tps-65-top-right-inside-level-with-screen-bottom-left-control-point (assoc (vec tps-65-top-right-wall-locate2-inside-control-point) 2 (last screen-holder-bottom-left-inside-point))
         tps-65-top-right-level-with-screen-bottom-left (assoc (vec tps-65-top-right-wall-locate3-outside-point) 2 (last screen-holder-bottom-left-outside-point))
         tps-65-top-right-level-with-screen-bottom-left-control-point (assoc (vec tps-65-top-right-wall-locate3-outside-control-point) 2 (last screen-holder-bottom-left-outside-point))
              tps-65-top-right-floor-bottom-left (assoc (vec tps-65-top-right-wall-locate3-outside-point) 2 0)
tps-65-top-right-floor-bottom-left-control-point(assoc (vec tps-65-top-right-wall-locate3-outside-control-point) 2 0)
        tps-65-top-right-inside-bottom-point  (assoc (vec tps-65-top-right-wall-locate2-inside-point) 2 0)
         tps-65-top-right-inside-bottom-control-point (assoc (vec tps-65-top-right-wall-locate2-inside-control-point) 2 0)
        top-bezier-points (bezier-quadratic tps-65-top-right-wall-locate3-outside-point tps-65-top-right-wall-locate3-outside-control-point screen-holder-top-left-outside-point steps)
        mid-bezier-points (bezier-quadratic tps-65-top-right-level-with-screen-bottom-left tps-65-top-right-level-with-screen-bottom-left-control-point screen-holder-bottom-left-outside-point steps)
        bottom-bezier-points (bezier-quadratic tps-65-top-right-floor-bottom-left tps-65-top-right-floor-bottom-left-control-point screen-holder-bottom-left-outside-floor-point steps)
        top-inside-bezier-points (bezier-quadratic  screen-holder-top-left-inside-point tps-65-top-right-wall-locate2-inside-control-point  tps-65-top-right-wall-locate2-inside-point steps)
        mid-inside-bezier-points (bezier-quadratic screen-holder-bottom-left-inside-point tps-65-top-right-inside-level-with-screen-bottom-left-control-point tps-65-top-right-inside-level-with-screen-bottom-left steps)
        bottom-inside-bezier-points (bezier-quadratic screen-holder-bottom-left-inside-floor-point  tps-65-top-right-inside-bottom-control-point  tps-65-top-right-inside-bottom-point steps)
        wall-curve-points  (into [](concat top-bezier-points mid-bezier-points bottom-bezier-points top-inside-bezier-points mid-inside-bezier-points bottom-inside-bezier-points))
        top-bezier-points-size (count top-bezier-points)
        top-bezier-points-end (- top-bezier-points-size 1)
        mid-bezier-points-size (count mid-bezier-points)
        mid-bezier-points-start top-bezier-points-size
        mid-bezier-points-end (+ mid-bezier-points-start (dec mid-bezier-points-size))
        bottom-bezier-points-size (count bottom-bezier-points)
        bottom-bezier-points-start (inc mid-bezier-points-end)
        bottom-bezier-points-end (+ bottom-bezier-points-start (dec bottom-bezier-points-size))
        top-inside-bezier-points-start (inc bottom-bezier-points-end)
        top-inside-bezier-points-end (+ top-inside-bezier-points-start  steps)
        mid-inside-bezier-points-start (inc top-inside-bezier-points-end) 
        mid-inside-bezier-points-end (+ mid-inside-bezier-points-start  steps)
        bottom-inside-bezier-points-start (inc mid-inside-bezier-points-end)
         bottom-inside-bezier-points-end (+ bottom-inside-bezier-points-start steps) 
 ;top inside to mid inside

        wall-curve-faces (into [] (concat

                                   (for [index (range top-bezier-points-end)]
                                     [index (inc index) (+ (inc index) (inc steps))])
                                   (for [index (range mid-bezier-points-start mid-bezier-points-end)]
                                     [(- index mid-bezier-points-size) (inc index) index])
                                   (for [index (range mid-bezier-points-start  mid-bezier-points-end)]
                                     [index (inc index) (+ (inc index) (inc steps))])
                                   (for [index (range bottom-bezier-points-start bottom-bezier-points-end)]
                                     [index (- index bottom-bezier-points-size) (inc index)])
                                   (for [index (range top-inside-bezier-points-start top-inside-bezier-points-end)]
                                     [index (inc index) (+ (inc index) (inc steps))])
                                   (for [index (range mid-inside-bezier-points-start mid-inside-bezier-points-end)]
                                     [(- index (inc steps)) (inc index) index])
                                   (for [index (range mid-inside-bezier-points-start  mid-inside-bezier-points-end)]
                                     [index (inc index) (+ (inc index) (inc steps))])
                                   (for [index (range bottom-inside-bezier-points-start bottom-inside-bezier-points-end)]
                                     [index (- index (inc steps)) (inc index)])
                                  [[top-bezier-points-end mid-inside-bezier-points-start mid-bezier-points-end]
[top-bezier-points-end top-inside-bezier-points-start mid-inside-bezier-points-start]
[mid-bezier-points-end bottom-inside-bezier-points-start bottom-bezier-points-end]
[mid-inside-bezier-points-start bottom-inside-bezier-points-start mid-bezier-points-end]]

[[top-inside-bezier-points-end mid-bezier-points-start mid-inside-bezier-points-end]
[top-inside-bezier-points-end 0 mid-bezier-points-start ]
;[top-inside-bezier-points-end 0 mid-inside-bezier-points-start]
[mid-inside-bezier-points-end mid-bezier-points-start bottom-bezier-points-start]
[mid-inside-bezier-points-end bottom-bezier-points-start bottom-inside-bezier-points-end]]

                                   (for [index (range 0 top-bezier-points-end)] 
                                     [(inc index) index (- top-inside-bezier-points-end index)]
                                     )
                                   (for [index (range 1  (inc top-bezier-points-end))]
                                     [(- top-inside-bezier-points-end (dec index))  (- top-inside-bezier-points-end  index) index ])
                                   
                       (for [index (range bottom-bezier-points-start bottom-bezier-points-end)]
                         [index (inc index)  (- bottom-inside-bezier-points-end (-  index bottom-bezier-points-start))])
(for [index (range bottom-bezier-points-start   bottom-bezier-points-end)]
  [(- bottom-inside-bezier-points-end  (- index bottom-bezier-points-start)) (inc index) (- bottom-inside-bezier-points-end (inc (- index bottom-bezier-points-start))) ])              
                                   ))
        wall-curve (polyhedron wall-curve-points, wall-curve-faces)
        ;; f1pts1 (fillet-about-point dx1 dy1 20)
        ;; f1ptsmid1 (fillet-about-point dxmid1 dymid1 20)
        ;; f1ptsmid2 (fillet-about-point dxmid2 dymid2 20)
        ;; f1pts2 (fillet-about-point dx2 dy2 20)
        ;; bezier-fn (fn [index]
        ;;             (bezier-cubic
        ;;              (place1  (mapv + (get-curve-corner-translation-vector post-position-1) (nth f1pts1 index)))
        ;;              (place-mid1    (mapv + (get-curve-corner-translation-vector post-position-mid1)  (nth f1ptsmid1 index)))
        ;;              (place-mid2    (mapv + (get-curve-corner-translation-vector post-position-mid2)  (nth f1ptsmid2 index)))
        ;;              (place2  (mapv + (get-curve-corner-translation-vector post-position-2) (nth f1pts2 index)))
        ;;              steps))
        ;; top-upper-curve-points (for [index (range 0 20)]  (bezier-fn index))
        ;; top-upper-curve-points-flattend (apply concat top-upper-curve-points)
        ;; top-upper-curve-faces (into []
        ;;                             (for [index (range 0 (- (count top-upper-curve-points-flattend) steps 1))]
        ;;                               [index (inc index) (+ (inc index) steps) (+ index steps)]))
        ;; top-upper-curve-polyhedron (polyhedron top-upper-curve-points-flattend top-upper-curve-faces)
        ] 
    ;(println top-bezier-points)
    (union
     ;top-upper-curve-polyhedron
      ;; (for [index (range 0 20)]
      ;;   (plot-bezier-points (bezier-fn index) (convert-to-curve-post oled-post)))
     ;(cube 5 5 5)
    
     wall-curve
      (-# (hull
       (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
       (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)))
     )
    
    ))

;(defn )
(def under-screen 
  (let 
   [steps 20
    screen-holder-bottom-left-outside-point (transform-position
                                             (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                             (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
    screen-holder-bottom-left-inside-point (transform-position
                                            (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                            (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
    screen-holder-bottom-right-outside-point (transform-position
                                              (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                              (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
    screen-holder-bottom-right-inside-point (transform-position
                                             (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                             (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
    screen-holder-bottom-left-outside-floor-point (assoc (vec screen-holder-bottom-left-outside-point) 2 0) 
                                        
                                        
screen-holder-bottom-left-inside-floor-point (assoc (vec screen-holder-bottom-left-inside-point) 2 0)
                                        
                                        
screen-holder-bottom-right-outside-floor-point (assoc (vec screen-holder-bottom-right-outside-point) 2 0)
                                        
                                        
screen-holder-bottom-right-inside-floor-point (assoc (vec screen-holder-bottom-right-inside-point) 2 0)
    
    points [screen-holder-bottom-left-outside-point
           screen-holder-bottom-right-outside-point
           screen-holder-bottom-left-outside-floor-point
           screen-holder-bottom-right-outside-floor-point
           screen-holder-bottom-left-inside-point
            screen-holder-bottom-right-inside-point
            screen-holder-bottom-left-inside-floor-point
            screen-holder-bottom-right-inside-floor-point
            ]
    
    faces [[0 3 2] [0 1 3]
           [4 7 5] [4 6 7]
           [4 5 1] [4 1 0]
           [2 7 6] [2 3 7]
           [4 2 6]  [4 0 2]
           [1 7 3] [1 5 7] 
           ] 
   ;under-screen-polyhedron (polyhedron points faces)
    ]
    (polyhedron points faces)
    ))
(def screen-to-EVQWGD001
  (let [steps 20
        screen-holder-top-right-outside-point (transform-position
                                               (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                               (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
        screen-holder-top-right-inside-point (transform-position
                                              (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                              (mapv +  [0 0 (/ oled-holder-thickness 2)]))
        screen-holder-bottom-right-outside-point (transform-position
                                                  (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                                  (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))



        screen-holder-bottom-right-inside-point (transform-position
                                                 (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                                 (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))

        screen-holder-bottom-right-outside-floor-point (assoc (vec screen-holder-bottom-right-outside-point) 2 0)
        screen-holder-bottom-right-inside-floor-point (assoc (vec screen-holder-bottom-right-inside-point) 2 0)
        EVQWGD001-mount-top-left-outside (transform-position
                                          (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-left)
                                          (mapv + [(/ oled-post-size -2) (/ oled-post-size 2) (- (/ EVQWGD001-mount-y-modifier 2))] [0 0 (/ EVQWGD001-mount-height 2)]))
        EVQWGD001-mount-top-left-inside (transform-position
                                         (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-left)
                                         (mapv + [(/ oled-post-size -2) (/ oled-post-size 2) (- (/ EVQWGD001-mount-y-modifier 2))] [0 0 (/ (- plate-thickness) 2)]))
        EVQWGD001-mount-bottom-left-outside (transform-position
                                             (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left)
                                             (mapv + [(/ oled-post-size -2) (/ oled-post-size -2) (- (/ EVQWGD001-mount-y-modifier 2))] [0 0 (/ EVQWGD001-mount-height 2)]))
        EVQWGD001-mount-bottom-left-inside (transform-position
                                            (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left)
                                            (mapv + [(/ oled-post-size -2) (/ oled-post-size -2) (- (/ EVQWGD001-mount-y-modifier 2))] [0 0 (/ (- plate-thickness) 2)]))
        EVQWGD001-mount-bottom-right-outside (transform-position
                                              (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right)
                                              (mapv + [(/ oled-post-size 2) (/ oled-post-size -2) (- (/ EVQWGD001-mount-y-modifier 2))] [0 0 (/  EVQWGD001-mount-height 2)]))
        EVQWGD001-mount-bottom-right-inside (transform-position
                                             (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right)
                                             (mapv + [(/ oled-post-size 2) (/ oled-post-size -2) (- (/ EVQWGD001-mount-y-modifier 2))] [0 0 (/  (- plate-thickness) 2)]))
        
        EVQWGD001-mount-top-right-outside (transform-position
                                              (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right)
                                              (mapv + [(/ oled-post-size 2) (/ oled-post-size 2) (- (/ EVQWGD001-mount-y-modifier 2))] [0 0 (/  EVQWGD001-mount-height 2)]))
        EVQWGD001-mount-top-right-inside (transform-position
                                           (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right)
                                           (mapv + [(/ oled-post-size 2) (/ oled-post-size 2) (- (/ EVQWGD001-mount-y-modifier 2))]  [0 0 (/  (- plate-thickness) 2)]))
        
        EVQWGD001-mount-bottom-right-outside-floor (translate-to-floor EVQWGD001-mount-bottom-right-outside)
        EVQWGD001-mount-bottom-right-inside-floor (translate-to-floor EVQWGD001-mount-bottom-right-inside)
        thumb-bl-tl-outside (transform-position
                             (partial thumb-bl-place) (mapv + (wall-locate3-for-polyhedron-point -1 0) curve-post-tl-translation-vector curve-post-translation-vector [(/ curve-post-size 2) (/ curve-post-size 2) (- oled-holder-thickness curve-post-size)]))
        thumb-bl-tl-inside (transform-position
                            (partial thumb-bl-place) (mapv + (wall-locate2 -1 0) oled-post-tl-translation-vector oled-translation-vector [(/ oled-post-size 2) (/ oled-post-size 2)  0]))
        thumb-bl-tl-outside-floor (assoc (vec thumb-bl-tl-outside) 2 0)
        thumb-bl-tl-inside-floor (assoc (vec thumb-bl-tl-inside) 2 0)

        upper-outside-control-point (mapv + [0 -4 0] (mapv + EVQWGD001-mount-top-left-outside (mapv  (fn [point] (/ point 2)) (mapv - screen-holder-top-right-outside-point EVQWGD001-mount-top-left-outside))))
        upper-inside-control-point (mapv + [0 -4 0] (mapv + EVQWGD001-mount-top-left-inside (mapv  (fn [point] (/ point 2)) (mapv - screen-holder-top-right-inside-point EVQWGD001-mount-top-left-inside))))
        mid-inside-control-point (mapv + EVQWGD001-mount-bottom-left-inside [0 -4 0] (mapv (fn [point] (/ point 2)) (mapv - screen-holder-bottom-right-inside-point EVQWGD001-mount-bottom-left-inside)))
        mid-outside-control-point (mapv + EVQWGD001-mount-bottom-left-outside [0 -4 0] (mapv (fn [point] (/ point 2)) (mapv - screen-holder-bottom-right-outside-point EVQWGD001-mount-bottom-left-outside)))
        bottom-outside-control-point (mapv + EVQWGD001-mount-bottom-right-outside [0 -4 0] (mapv (fn [point] (/ point 2)) (mapv - screen-holder-bottom-right-outside-floor-point EVQWGD001-mount-bottom-right-outside)))
        bottom-inside-control-point (mapv + EVQWGD001-mount-bottom-right-inside [0 -4 0] (mapv (fn [point] (/ point 2)) (mapv - screen-holder-bottom-right-inside-floor-point EVQWGD001-mount-bottom-right-inside)))
        thumb-bl-tl-curve (map #(transform-position
                                 (partial thumb-bl-place) %)  (fillet-about-point -1 0 steps (mapv +  curve-post-tl-translation-vector curve-post-translation-vector [(/ curve-post-size -2) (/ curve-post-size 2) (+ curve-post-size)])))
         EVQWGD001-mount-bottom-right-outside-to-thumb-bl-outside (bezier-quadratic  EVQWGD001-mount-bottom-right-outside (calculate-point-between-points EVQWGD001-mount-bottom-right-outside thumb-bl-tl-outside [0 1 0]) thumb-bl-tl-outside  steps)
        EVQWGD001-mount-bottom-right-outside-to-thumb-bl-web-post-tl (bezier-linear
                                                                             EVQWGD001-mount-bottom-right-outside
                                                                             (transform-position (partial thumb-bl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
                                                                             steps)
        EVQWGD001-mount-bottom-right-outside-to-thumb-bl-to-top-of-tl-curve (bezier-linear
                                                                             EVQWGD001-mount-bottom-right-outside
                                                                             (transform-position
                                                                              (partial thumb-bl-place)  (nth (fillet-about-point -1 0 steps (mapv +  curve-post-tl-translation-vector curve-post-translation-vector [(/ curve-post-size -2) (/ curve-post-size 2) (+ curve-post-size)])) 0))
                                                                             steps)
        thumb-bl-tl-web-post-bottom (transform-position (partial thumb-bl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        thumb-bl-tr-web-post-top (transform-position (partial thumb-bl-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-bl-tr-web-post-bottom (transform-position (partial thumb-bl-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        screen-holder-bottom-right-outside-floor-to-EVQWGD001-mount-bottom-right-floor-outside-control (calculate-point-between-points screen-holder-bottom-right-outside-floor-point EVQWGD001-mount-bottom-right-inside-floor [0 -4 0])
        screen-holder-bottom-right-inside-floor-to-EVQWGD001-mount-bottom-right-floor-inside-control (calculate-point-between-points screen-holder-bottom-right-inside-floor-point EVQWGD001-mount-bottom-right-inside-floor [0 -4 0])
        EVQWGD001-mount-bottom-right-outside-to-thumb-bl-tl-outside-control-point (calculate-point-between-points EVQWGD001-mount-bottom-right-outside thumb-bl-tl-outside [0 -4 0])
        EVQWGD001-mount-bottom-right-inside-to-thumb-bl-tl-inside-control-point (calculate-point-between-points EVQWGD001-mount-bottom-right-inside thumb-bl-tl-inside [0 -4 0])
        EVQWGD001-mount-bottom-right-outside-floor-to-thumb-bl-tl-outside-floor-control-point (calculate-point-between-points EVQWGD001-mount-bottom-right-outside-floor thumb-bl-tl-outside-floor [0 -4 0])
        EVQWGD001-mount-bottom-right-inside-floor-to-thumb-bl-tl-inside-floor-control-point (calculate-point-between-points EVQWGD001-mount-bottom-right-inside-floor thumb-bl-tl-inside-floor [0 -4 0])
        EVQWGD001-mount-top-right-outside-to-thumb-bl-tr-web-post-top-control-point (calculate-point-between-points EVQWGD001-mount-top-right-outside thumb-bl-tr-web-post-top [0 1 0])
        EVQWGD001-mount-top-right-inside-to-thumb-bl-tr-web-post-bottom-control-point (calculate-point-between-points EVQWGD001-mount-top-right-inside thumb-bl-tr-web-post-bottom [0 1 0]) 
         
        screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-points (bezier-quadratic screen-holder-top-right-outside-point upper-outside-control-point EVQWGD001-mount-top-left-outside steps)
        screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-points (bezier-quadratic EVQWGD001-mount-top-left-inside upper-inside-control-point screen-holder-top-right-inside-point  steps)
        screen-holder-bottom-right-outside-to-EVQWGD001-mount-bottom-left-outside-points (bezier-quadratic screen-holder-bottom-right-outside-point mid-outside-control-point EVQWGD001-mount-bottom-left-outside steps)
        screen-holder-bottom-right-inside-to-EVQWGD001-mount-bottom-left-inside-points (bezier-quadratic  EVQWGD001-mount-bottom-left-inside mid-inside-control-point screen-holder-bottom-right-inside-point steps)
        screen-holder-bottom-right-outside-floor-to-EVQWGD001-mount-bottom-right-outside-points (bezier-quadratic screen-holder-bottom-right-outside-floor-point   bottom-outside-control-point EVQWGD001-mount-bottom-right-outside steps)
        screen-holder-bottom-right-inside-floor-to-EVQWGD001-mount-bottom-right-inside-points (bezier-quadratic EVQWGD001-mount-bottom-right-inside bottom-inside-control-point screen-holder-bottom-right-inside-floor-point  steps)

        screen-holder-bottom-right-inside-floor-to-EVQWGD001-mount-bottom-right-floor-inside-points (bezier-quadratic EVQWGD001-mount-bottom-right-inside-floor bottom-inside-control-point screen-holder-bottom-right-inside-floor-point  steps)
;;          EVQWGD001-mount-bottom-right-outside-to-thumb-bl-tl-outside-points (bezier-quadratic EVQWGD001-mount-bottom-right-outside )
;;  EVQWGD001-mount-bottom-right-inside-to-thumb-bl-tl-inside-points (bezier-quadratic)
;;  EVQWGD001-mount-bottom-right-outside-floor-to-thumb-bl-tl-outside-floor-points (bezier-quadratic)
;;  EVQWGD001-mount-bottom-right-inside-floor-to-thumb-bl-tl-inside-floor-points (bezier-quadratic)
        upper-points-start 0
        upper-points-end (dec (count screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-points))
        mid-points-size (count screen-holder-bottom-right-outside-to-EVQWGD001-mount-bottom-left-outside-points)
        mid-points-start (inc upper-points-end)
        mid-points-end (dec (+ mid-points-start (count screen-holder-bottom-right-outside-to-EVQWGD001-mount-bottom-left-outside-points)))
        bottom-outside-points-start (inc mid-points-end)
        bottom-outside-points-end (dec (+ bottom-outside-points-start (count screen-holder-bottom-right-outside-floor-to-EVQWGD001-mount-bottom-right-outside-points)))
        upper-wall-curve-points (into [] (concat screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-points screen-holder-bottom-right-outside-to-EVQWGD001-mount-bottom-left-outside-points screen-holder-bottom-right-outside-floor-to-EVQWGD001-mount-bottom-right-outside-points))
        screen-holder-top-and-bottom-to-EVQWGD001-mount-top-and-bottom-left-polyhedron (generate-bezier-polyhedron
                                                                                        screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-points
                                                                                        screen-holder-bottom-right-outside-to-EVQWGD001-mount-bottom-left-outside-points
                                                                                        screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-points
                                                                                        screen-holder-bottom-right-inside-to-EVQWGD001-mount-bottom-left-inside-points
                                                                                        steps)
        screen-holder-bottom-and-floor-point-to-EVQWGD001-bottom-left-and-right-polyhedron (generate-bezier-polyhedron
                                                                                            screen-holder-bottom-right-outside-to-EVQWGD001-mount-bottom-left-outside-points
                                                                                            screen-holder-bottom-right-outside-floor-to-EVQWGD001-mount-bottom-right-outside-points
                                                                                            screen-holder-bottom-right-inside-to-EVQWGD001-mount-bottom-left-inside-points
                                                                                            screen-holder-bottom-right-inside-floor-to-EVQWGD001-mount-bottom-right-inside-points
                                                                                            steps)
        screen-holder-bottom-and-floor-point-to-EVQWGD001-bottom-right-and-floor-polyhedron (generate-bezier-quadratic-polyhedron-from-points
                                                                                             screen-holder-bottom-right-outside-point EVQWGD001-mount-bottom-right-outside screen-holder-bottom-right-outside-floor-point EVQWGD001-mount-bottom-right-outside-floor
                                                                                             screen-holder-bottom-right-inside-point EVQWGD001-mount-bottom-right-inside screen-holder-bottom-right-inside-floor-point EVQWGD001-mount-bottom-right-inside-floor
                                                                                             steps)
        EVQWGD001-bottom-right-and-floor-thumb-bl-tl-and-floor-polyhedron (generate-bezier-quadratic-polyhedron-from-points-and-control-vectors
                                                                           EVQWGD001-mount-bottom-right-outside thumb-bl-tl-outside
                                                                           EVQWGD001-mount-bottom-right-outside-floor thumb-bl-tl-outside-floor
                                                                           EVQWGD001-mount-bottom-right-inside thumb-bl-tl-inside
                                                                           EVQWGD001-mount-bottom-right-inside-floor thumb-bl-tl-inside-floor
                                                                           steps
                                                                           {:outside-upper-control-point-vector [0 1 0] :outside-lower-control-point-vector [0 1 0] 
                                                                           :inside-upper-control-point-vector [0 1 0] :inside-lower-control-point-vector [0 1 0]}
                                                                           
                                                                           )
       EVQWGD001-mount-bottom-right-to-thumb-bl-polyhedron (generate-bezier-polyhedron
            
            thumb-bl-tl-curve 
           EVQWGD001-mount-bottom-right-outside-to-thumb-bl-outside 
            
            ( bezier-linear 
             thumb-bl-tl-inside
             thumb-bl-tl-web-post-bottom
              steps
              )
            (bezier-quadratic thumb-bl-tl-inside (calculate-point-between-points EVQWGD001-mount-bottom-right-inside thumb-bl-tl-inside  [0 1 0])   EVQWGD001-mount-bottom-right-inside steps)
            
            
            
            steps
            )

       EVQWGD001-mount-bottom-right-to-thumb-bl-web-post-tl-polyhedron (generate-bezier-polyhedron 
            EVQWGD001-mount-bottom-right-outside-to-thumb-bl-web-post-tl
            EVQWGD001-mount-bottom-right-outside-to-thumb-bl-to-top-of-tl-curve
            
            (bezier-linear 
             
             thumb-bl-tl-web-post-bottom
             EVQWGD001-mount-bottom-right-inside
             steps
             )
            (bezier-linear 
             thumb-bl-tl-web-post-bottom
             EVQWGD001-mount-bottom-right-inside
             steps)
            steps 
            )
       
       EVQWGD001-mount-left-right-to-thumb-bl-web-post-tr-polyhedron (generate-bezier-polyhedron
                                                                      (bezier-quadratic
                                                                       EVQWGD001-mount-top-right-outside EVQWGD001-mount-top-right-outside-to-thumb-bl-tr-web-post-top-control-point thumb-bl-tr-web-post-top steps 
                                                                       )
                                                                      EVQWGD001-mount-bottom-right-outside-to-thumb-bl-web-post-tl
                                                                      (bezier-quadratic
                                                                       thumb-bl-tr-web-post-bottom EVQWGD001-mount-top-right-inside-to-thumb-bl-tr-web-post-bottom-control-point EVQWGD001-mount-top-right-inside  steps
                                                                       )
                                                                       (bezier-linear

                                                                        thumb-bl-tl-web-post-bottom
                                                                        EVQWGD001-mount-bottom-right-inside
                                                                        steps)
                                                                      steps
                                                                      )
        ;; EVQWGD001-mount-bottom-right-and-floor-to-thumb-bl-tl-and-floor-polyhedron (generate-bezier-polyhedron)
        wall-curve-faces (fn [outside-upper-start outside-upper-end outside-lower-start outside-lower-end inner-upper-start inner-upper-end inner-lower-start inner-lower-end steps] (into [] (concat

                                                                                                                                                                                               (for [index (range outside-upper-start outside-upper-end)]
                                                                                                                                                                                                 [index (inc index) (+ (inc index) (inc steps))])
                                                                                                                                                                                               (for [index (range outside-lower-start outside-lower-end)]
                                                                                                                                                                                                 [(- index (inc steps)) (inc index) index])
                                                                                                                                                                                               ()
                                  ;;  (for [index (range mid-points-start mid-points-end)]
                                  ;;    [index (inc index) (+ (inc index) mid-points-size)]
                                  ;;    )
                                  ;;  (for [index (range bottom-outside-points-start bottom-outside-points-end)]
                                  ;;    [(- index (inc steps)) (inc index) index ])
                                                                                                                                                                                               )))
        upper-wall-curve (polyhedron upper-wall-curve-points wall-curve-faces)]
    
    (union
     screen-holder-top-and-bottom-to-EVQWGD001-mount-top-and-bottom-left-polyhedron
     screen-holder-bottom-and-floor-point-to-EVQWGD001-bottom-left-and-right-polyhedron
     screen-holder-bottom-and-floor-point-to-EVQWGD001-bottom-right-and-floor-polyhedron
     EVQWGD001-bottom-right-and-floor-thumb-bl-tl-and-floor-polyhedron
     ;wall-curve
     EVQWGD001-mount-bottom-right-to-thumb-bl-polyhedron
     EVQWGD001-mount-bottom-right-to-thumb-bl-web-post-tl-polyhedron
     EVQWGD001-mount-left-right-to-thumb-bl-web-post-tr-polyhedron
     (-# (plot-and-translate-bezier-points 
            thumb-bl-place  (fillet-about-point -1 0 steps ) curve-post-tl))
     )
    ))

(spit "things-low/left-curve-test.scad"
      (write-scad
       (difference (union 
        ;S(screen-holder-place-side screen-holder)
       ;(curved-corner-xy 1 0 1 1 0 1 (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) oled-post wall-xy-offset)
       ;(-# (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset)) 1 1 oled-post (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 1 0 oled-post wall-xy-offset wall-xy-offset))
        (tps-65-place tps-65-base)
       (back-left-wall-to-screen {:dx1 1 :dy1 0 :place1 (partial transform-position (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset))
                                  :post-position-1 ""
                                  :dxmid1 0 :dymid1 1 :place-mid1 (partial transform-position (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset))
                                  :post-position-mid1 ""
                                  :dxmid2 0 :dymid2 1 :place-mid2 (partial transform-position (partial tps-65-translate-and-place-with-radius [(/ (- (/ tps-65-width 2) tps-65-corner-radius) 2)
                                                                                                                   (- (/ tps-65-length 2) tps-65-corner-radius)
                                                                                                                   0] tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset))
                                  :post-position-mid2 ""
                                  :dx2 0 :dy2 0  :place2 (partial transform-position (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0))  
                                  :post-position-2 "" :steps 20}) 
        (EVQWGD001-place EVQWGD001-holder)
        ;(EVQWGD001-place EVQWGD001)
        thumb-type
        dsa-thumbcaps
        screen-to-EVQWGD001
        (-# (thumb-wall-brace thumb-bl-place -1  0 oled-post-tl thumb-bl-place -1  0 oled-post-bl thumb-bl-rotate thumb-bl-rotate))
        under-screen
        )
        ;(translate [0 0 -20] (cube 350 350 40))
        )
       )
      )




(spit "things-low/thumb-wall-test.scad"
      (write-scad
      (difference (union

          ;(-# (curved-corner-quadratic  1 0 (partial thumb-mr-place) oled-post-br-translation-vector 
          ;                  ; 0 -1 (partial thumb-mr-place) web-post-tr-translation-vector  
          ;                         1 -1 (partial thumb-mr-place) oled-post-tr-translation-vector
          ;                          -1 -1 (partial thumb-tr-place) oled-post-br-translation-vector
          ;                         oled-post))

        ;;thumb-wall-type
                   
        front-wall
                   key-holes
                  ; connectors
        thumb-type
        ;thumb-connector-type 
        )
                   (translate [0 0 -20] (cube 350 350 40))
                  )))


      
;; (spit "things-low/curved-corner-test.scad"
;;       (write-scad 
;;        (curved-corner 0 -1 1 -1 1 0 (partial key-place 4 cornerrow) oled-post-br)
;;        )
;;       )

(defn -main [dum] 1)  ; dummy to make it easier to batch