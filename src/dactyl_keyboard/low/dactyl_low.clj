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
            ))  










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
   (translate [ 0 0 (- 3.05)])
   ))

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
                 ( EVQWGD001-place EVQWGD001-test)
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
                                        usb-jack
                                       )
                                      (tps-65-place tps-65-mount)
                                      screw-insert-outers
                                     (rp2040-plus-place rp2040-plus)
                                      (color [1 0 0 1](rp2040-plus-place rp2040-plus-mount))
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

(spit "things-low/right-test.scad"
      (write-scad
       tps-65-includes
       aviator-includes
      ; (include "../BOSL/shapes.scad")
;(include "../BOSL/constants.scad")
       (difference
        (union
         model-right
         ;thumbcaps-type
         dsa-thumbcaps
         ;caps
         (EVQWGD001-place EVQWGD001)
         dsa-caps
         (aviator-place-shape 
          (map +  [15 aviator-male-connecter-length 9.5]  aviator-position  aviator-offset) 
          ;(translate (map +  aviator-offset [14 aviator-male-connecter-length 9.5])
                                                 (rdy -90 gx16)
          ;                                       )
          )
         )
        
        (translate [0 0 -20] (cube 350 350 40)))))


(def bottom-plate
  (extrude-linear
   {:height 2.6 :center false}
   (project
    (difference
     (union
      case-walls 
      pinky-walls
     screw-insert-outers
     thumb-wall-type
      key-holes
      (tps-65-place tps-65-base)

      
pinky-connectors
extra-connectors
connectors
inner-connectors
thumb-type
thumb-connector-type
case-walls
thumbcaps-fill-type
caps-fill
      ))
    
    )
   )
  )


(def bottom-plate-old 
  (cut
   (translate [0 0 -0.1]
              (difference (union case-walls
                                 pinky-walls
                                 screw-insert-outers
                                 thumb-wall-type)
                          (translate [0 0 -10] screw-insert-screw-holes)))))

(def screen-test
  (difference
   (union
      screen-holder
   ; (rotate (deg2rad 0) [1 0 0] (rotate (deg2rad 90) [0 0 1] ST7789-240x240-154-holder-old))
    )
   screen-holder-cut))
(spit "things-low/right-plate.scad"
      (write-scad
       
       (difference
        (union 
         bottom-plate
         )
        (translate [0 0 -20] (cube 350 350 40))
        )
       ))

(spit "things-low/test.scad"
      (write-scad
       (difference trrs-holder trrs-holder-hole)))

(spit "things-low/tps-65-overlay.scad"
      (write-scad tps-65-overlay))

(spit "things-low/tps-65-mount-cutout.scad"
      (write-scad tps-65-mount-cutout))

(spit "things-low/tps-65-mount-test.scad"
      (write-scad tps-65-mount-test))

(spit "things-low/tps-65-mount-test-2.scad" 
      (write-scad 
       tps-65-includes
       ;(include "../BOSL/shapes.scad")
;(include "../BOSL/constants.scad")
       tps-65-mount))



;; (spit "things-low/pico-standoffs-test.scad"
;;       (write-scad pico-standoff-test))

;; (spit "things-low/IS31FL3743A-standoff-test.scad"
;;       (write-scad IS31FL3743A-standoff-test))

;; (spit "things-low/screen-test.scad"
;;       (write-scad screen-test))

;; (spit "things-low/EVQWGD001-test.scad"
;;       (write-scad EVQWGD001-test))

;; (spit "things-low/vybronics-vl91022-mount.scad"
;;       (write-scad vybronics-vl91022-mount))

;; (spit "things-low/drv2605l.scad"
;;       (write-scad drv2605l))

;; (spit "things-low/drv2605l-standoffs-test.scad"
;;       (write-scad drv2605l-standoffs-test)) 

;; (spit "things-low/rp2040-plus-mount-test.scad"
;;       (write-scad rp2040-plus-mount))

;; (spit "things-low/rp2040-plus.scad"
;;       (write-scad rp2040-plus))



(spit "things-low/aviator-assembly.scad"
      (write-scad 
       aviator-includes
aviator-assembly
                   
                   ))
(spit "things-low/back-wall-test.scad"
      (write-scad back-wall)
      )

(spit "things-low/left-section-test.scad"
      (write-scad 
       (union
        ;(EVQWGD001-place EVQWGD001-test)
        ;left-section
        left-wall
        (rp2040-plus-place rp2040-plus-mount)
        ;back-wall
        )
       )
      )







(defn -main [dum] 1)  ; dummy to make it easier to batch