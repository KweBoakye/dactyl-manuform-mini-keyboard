(ns dactyl-keyboard.oled
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.web-connecters :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer :all]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.shapes-3d :refer [cuboid]]
            [dactyl-keyboard.lib.transformations :refer [rdx rdy rdz ]]
            [dactyl-keyboard.lib.geometry :refer [deg2rad]]
            ))

;from https://github.com/oysteinkrog/dactyl-manuform-mini-keyboard

(def screen-rotation-angle -5)

;;;;;;;;;;;;;;;;;;;;;;;;
;; OLED screen holder ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(def oled-pcb-size [27.35 28.3 (- plate-thickness 1)])
(def oled-screen-offset [0 -0.5 0])
(def oled-screen-size [24.65 16.65 (- plate-thickness 1)])
(def oled-viewport-size [24.0 13.0 (+ 0.1 plate-thickness)])
(def oled-viewport-offset [0 1.0 0])
(def oled-mount-size [22.35 23.3 0.5])
(def oled-holder-width (+ 3 (nth oled-pcb-size 0)))
(def oled-holder-length (+ 3 (nth oled-pcb-size 1)))
(def oled-holder-thickness plate-thickness)
(def oled-holder-size [oled-holder-width oled-holder-length oled-holder-thickness])
(def oled-mount-rotation-x-old  20)
(def oled-mount-rotation-z-old  -3)


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
      (translate (div-vec oled-mount-size [x y 1]) (binding [*fn* 36] (cylinder (/ 2.5 2) 20))))
    )
   (rdy 180)
   (translate [0 0 (/ oled-holder-thickness 2)])))

(def oled-holder
  (->>
    ; main body
   (apply cube oled-holder-size)
   (rdy 180)
   (translate [0 0 (/ oled-holder-thickness 2)])))

;;;;;;;;;;;;;;;;;;;;;;;;
;; ILI9341 screen holder ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(def ILI9341-pcb-size [42.6 77.0 (- plate-thickness 1)])
(def ILI9341-screen-size [42.6 60.26 (- plate-thickness 1)])
(def ILI9341-viewport-size [37.72 48.96 (+ 0.1 plate-thickness)])
(def ILI9341-viewport-offset [0 1.0 0])
(def ILI9341-mount-size [20.2 23.75 0.5])

(def ILI9341 (cube 42.6 77.0 4.2))

(def ST7789-240x320 (cube 60 36.5 3.6))
(def ST7789-240x320-display (cube 40.8 30.6 3.6))

(def ST7789-240x240-154 (cube 32.00 43.72 3.6))
(def ST7789-240x240-154-display (cube 27.27 27.27 3.6))

(def ST7789-240x240-13 (cube 27.78 39.22 3.6))
(def ST7789-240x240-13-display (cube 23.4 23.4 3.6))

;;;;;;;;;;;;;;;;;;;;;;;;
;; ST7789-240x240-1.54 screen holder ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(def ST7789-240x240-154-pcb-size [33 44.5 (- plate-thickness 1)])
(def ST7789-240x240-154-screw-size 2.5)
(def ST7789-240x240-154-screw-countersink-length 1.6)
(def ST7789-240x240-154-screw-head-diameter 5)
(def ST7789-240x240-154-screen-offset [0 0 0])
(def ST7789-240x240-154-screen-size [32 35 (- plate-thickness 1)])
(def ST7789-240x240-154-viewport-size [28 28 (+ 0.1 plate-thickness)])
(def ST7789-240x240-154-viewport-offset [0 -1.55 0])
(def ST7789-240x240-154-mount-size [27 38.72  0.5])
(def ST7789-240x240-154-holder-width (+ 3 (nth ST7789-240x240-154-pcb-size 0)))
(def ST7789-240x240-154-holder-height (+ 3 (nth ST7789-240x240-154-pcb-size 1)))
(def ST7789-240x240-154-holder-thickness plate-thickness)
(def ST7789-240x240-154-holder-thickness-modifier 2)
(def ST7789-240x240-154-holder-size [ST7789-240x240-154-holder-width ST7789-240x240-154-holder-height ST7789-240x240-154-holder-thickness])
(def ST7789-240x240-154-mount-rotation-x-old (deg2rad 20))
(def ST7789-240x240-154-mount-rotation-z-old (deg2rad -3))
(def ST7789-240x240 (rdz -90 (import "../parts/1.54 TFT IPS - Module - Assembly.stl")))

;(def ST7789-240x240-154-screen-cutout)
(def countersink-chamfer
  (->>
   (hull 
    (translate [0 0 ST7789-240x240-154-screw-countersink-length](cylinder (/ ST7789-240x240-154-screw-size 2) 0.001))
   (cylinder (/ ST7789-240x240-154-screw-head-diameter 2) 0.001))
   (translate [0 0 (- (+ (/ ST7789-240x240-154-holder-thickness 2) 0.5))])
   (with-fn 36)
   )
  )
(def view-bezel

  (->>
   (hull

    (cube (+ (first ST7789-240x240-154-viewport-size) 1) (+ (second ST7789-240x240-154-viewport-size) 1) 0.001)
    (translate [0 0 0.5]
               (cube (first ST7789-240x240-154-viewport-size) (second ST7789-240x240-154-viewport-size) 0.001)))


   (translate ST7789-240x240-154-viewport-offset)
   (translate [0 0 (- (+ (/ ST7789-240x240-154-holder-thickness 2) 0))])))

(def all-countersink-chamfers
  (for [x [-2 2] y [-2 2]]
    (translate (div-vec ST7789-240x240-154-mount-size [x y 1])
               countersink-chamfer))
  )

(def ST7789-240x240-154-viewport-cut
  (->> (translate ST7789-240x240-154-viewport-offset (apply cube ST7789-240x240-154-viewport-size))
       (rdy 180)
       (translate [0 0 (/ ST7789-240x240-154-holder-thickness 2)]))
  )

(def ST7789-240x240-154-screen-cut
  (->> (translate ST7789-240x240-154-screen-offset (apply cube ST7789-240x240-154-screen-size))
       (rdy 180)
       (translate [0 0 (/ ST7789-240x240-154-holder-thickness 2)])))

(def ST7789-240x240-154-holder-cut
  (->>
   (union
    view-bezel
    all-countersink-chamfers
      ; cut for ST7789-240x240-154 pcb
    (difference
     (translate [0 0 1] (apply cube (add-vec [0.5 0.5 0.1] ST7789-240x240-154-pcb-size)))
     (for [x [-2 2] y [-2 2]]
       (translate (mapv + (div-vec ST7789-240x240-154-mount-size [x y 1]) [0 0 -0.3])
                  (binding [*fn* 36] (cylinder 2.5 (- ST7789-240x240-154-holder-thickness 2.5))))))
      ; cut for ST7789-240x240-154 screen
    (translate ST7789-240x240-154-screen-offset (apply cube ST7789-240x240-154-screen-size))
      ; cut for ST7789-240x240-154 screen viewport
    (translate ST7789-240x240-154-viewport-offset (apply cube ST7789-240x240-154-viewport-size))
      ; cutout for ST7789-240x240-154 cable
    (->> (cube 10 2 10)
         (translate ST7789-240x240-154-screen-offset)
         (translate [0 (- (+ (/ (nth ST7789-240x240-154-screen-size 1) 2) 1)) (+ plate-thickness 1.0)]))
    (for [x [-2 2] y [-2 2]]
      (translate (div-vec ST7789-240x240-154-mount-size [x y 1]) (binding [*fn* 36] (cylinder (/ 2.5 2) 10)))))
   (rdy 180)
   (translate [0 0 (/ ST7789-240x240-154-holder-thickness 2)])))

(def ST7789-240x240-154-holder-base
  (->>
   (let [adjustment 0.5 radius 1 corner (square radius radius)] (hull
                                                                 (translate [(- adjustment (/ ST7789-240x240-154-holder-width 2)) (- (/ ST7789-240x240-154-holder-height 2) adjustment) 0] corner)
                                                                 (translate [(- (/ ST7789-240x240-154-holder-width 2) adjustment) (- (/ ST7789-240x240-154-holder-height 2) adjustment) 0] corner)
                                                                 (translate [(- adjustment (/ ST7789-240x240-154-holder-width 2)) (- adjustment (/ ST7789-240x240-154-holder-height 2)) 0] corner)
                                                                 (translate [(- (/ ST7789-240x240-154-holder-width 2) adjustment) (- adjustment (/ ST7789-240x240-154-holder-height 2)) 0] corner)))
   (with-fn 20)))

(def ST7789-240x240-154-holder-new
  (let [height (+ (-  ST7789-240x240-154-holder-thickness ST7789-240x240-154-holder-thickness-modifier) ST7789-240x240-154-holder-thickness-modifier)]
    (->> (cuboid [ST7789-240x240-154-holder-width ST7789-240x240-154-holder-height height]
                 :rounding 1.8 :edges [TOP] :fn 36
                 ;[(mapv + RIGHT TOP)]
                 )
         (translate [0 0 (/ height 2)]))))
(def ST7789-240x240-154-holder
  ST7789-240x240-154-holder-new
  ;; (->>
  ;;   ; main body
  ;;  (hull
  ;;   (for [i [0 0.25 0.5 0.75 1]
  ;;         :let [y (* (Math/cos (* i 90)) ST7789-240x240-154-holder-thickness-modifier) x  (+ -0.95 (* (Math/sin (* i 90)) 1))]]

  ;;     (extrude-linear {:height (+ (-  ST7789-240x240-154-holder-thickness ST7789-240x240-154-holder-thickness-modifier) y) :center false :convexity 10}
  ;;                     (offset-delta {:delta x  :chamfer false :r false} ST7789-240x240-154-holder-base))))

  ;;  ;(apply cube ST7789-240x240-154-holder-size)
  ;;  ;(translate [0 0 (/ ST7789-240x240-154-holder-thickness 2)])
  ;;  ;(rdy 180)

  ;;  (translate [0 0 0]))
  )



(comment 
  (spit "things-low/screen-holder-test.scad"
      (write-scad
       (include include-bosl2)
       (union
  (-#  ST7789-240x240-154-holder)
        ST7789-240x240-154-holder-new)
       )))

(def ST7789-240x240-154-holder-old
  (->>
    ; main body

   (apply cube ST7789-240x240-154-holder-size)
   ;(translate [0 0 1])
   (rdy 180)

   (translate [0 0 (/ ST7789-240x240-154-holder-thickness 2)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ST7789-240x240-1.54 screen holder ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ST7789-135*240-pcb-width 31.4)
(def ST7789-135*240-pcb-length 28.0)
(def ST7789-135*240-pcb-thickness 1.2)
(def ST7789-135*240-screen-length 17.6)
(def ST7789-135*240-screen-width 31.00)
(def ST7789-135*240-holder-x-offset 2)
(def ST7789-135*240-holder-y-offset 2)
(def ST7789-135*240-holder-x-inner-offset 1)
(def ST7789-135*240-holder-y-inner-offset 1)
(def ST7789-135*240-holder-width (+ ST7789-135*240-pcb-width ST7789-135*240-holder-x-offset))
(def ST7789-135*240-holder-length (+ ST7789-135*240-pcb-length ST7789-135*240-holder-y-offset))
(def ST7789-135*240-holder-inner-width (+ ST7789-135*240-pcb-width ST7789-135*240-holder-x-inner-offset))
(def ST7789-135*240-holder-inner-length (+ ST7789-135*240-pcb-length ST7789-135*240-holder-y-inner-offset))
(def ST7789-135*240-viewport-width 28)
(def ST7789-135*240-viewport-length 18)
(def ST7789-135*240-viewport-x-distance-from-left 0)
(def ST7789-135*240-viewport-x-position (+ (/ (- ST7789-135*240-viewport-width ST7789-135*240-pcb-width) 2) ST7789-135*240-viewport-x-distance-from-left))
(def ST7789-135*240-viewport-y-distance-from-top 5.4)
(def ST7789-135*240-viewport-y-position 
  (- (/ ST7789-135*240-pcb-length 2) 
    (/ ST7789-135*240-viewport-length  2)
     ST7789-135*240-viewport-y-distance-from-top 
     ) 
  )
(def ST7789-135*240-thickness 3.0)
(def ST7789-135*240-screen-thickness 1.6)
(def ST7789-135*240-viewport-thickness 1.8)
(def ST7789-135*240-mounting-hole-diameter 2)
(def ST7789-135*240-horizontal-distance-between-mounting-holes 26.4)
(def ST7789-135*240-vertical-distance-between-mounting-holes 23)
(def ST7789-135*240-vertical-distance-from-bottom-left-corner-to-centre-of-mounting-hole 2.5)
(def ST7789-135*240-horzontal-distance-from-bottom-left-corner-to-centre-of-mounting-hole 2.5)

(def ST7789-135*240-pcb (->>
                         (cube ST7789-135*240-pcb-width ST7789-135*240-pcb-length ST7789-135*240-pcb-thickness)
                         (translate [0 0 (/ ST7789-135*240-pcb-thickness 2)])))
(def ST7789-135*240-screen
  (->> (cube ST7789-135*240-screen-width ST7789-135*240-screen-length ST7789-135*240-screen-thickness)
       (translate [0 0 (+ ST7789-135*240-pcb-thickness (/ ST7789-135*240-screen-thickness 2))])))
(def ST7789-135*240-viewport (->> (cube ST7789-135*240-viewport-width ST7789-135*240-viewport-length ST7789-135*240-screen-thickness)
                                (translate [ST7789-135*240-viewport-x-position ST7789-135*240-viewport-y-position (+ ST7789-135*240-pcb-thickness (/ ST7789-135*240-screen-thickness 2))])))

(def ST7789-135*240-mounting-hole (->> (cylinder (/ ST7789-135*240-mounting-hole-diameter 2) 10 :center false)
                                       (binding [*fn* 36] )
                                       (translate [0 0 -0.1])))

(def ST7789-135*240-bottom-right-mounting-hole-position 
  [(- (/ ST7789-135*240-pcb-width 2) ST7789-135*240-horzontal-distance-from-bottom-left-corner-to-centre-of-mounting-hole)
   (+ (/ ST7789-135*240-pcb-length -2) ST7789-135*240-vertical-distance-from-bottom-left-corner-to-centre-of-mounting-hole)
   0])

(def ST7789-135*240-top-right-mounting-hole-position
  (mapv + ST7789-135*240-bottom-right-mounting-hole-position 
        [0 ST7789-135*240-vertical-distance-between-mounting-holes 0]))

(def ST7789-135*240-top-left-mounting-hole-position 
  (mapv + ST7789-135*240-bottom-right-mounting-hole-position
        [(- ST7789-135*240-horizontal-distance-between-mounting-holes)
         ST7789-135*240-vertical-distance-between-mounting-holes
         0]))

(def ST7789-135*240-bottom-left-mounting-hole-position
  (mapv + ST7789-135*240-bottom-right-mounting-hole-position
        [(- ST7789-135*240-horizontal-distance-between-mounting-holes)
         0 
         0])
  )



(def ST7789-135*240-mounting-hole-positions 
  [ST7789-135*240-bottom-right-mounting-hole-position
   ST7789-135*240-top-right-mounting-hole-position
   ST7789-135*240-top-left-mounting-hole-position
   ST7789-135*240-bottom-left-mounting-hole-position])

(defn ST7789-135*240-mounting-place [shape]
  (->>
   (mapv #(translate % shape) ST7789-135*240-mounting-hole-positions)
   (apply union)))
(def ST7789-135*240-mounting-holes 
  (ST7789-135*240-mounting-place ST7789-135*240-mounting-hole))

(def ST7789-135*240-mounting-standoff
  (->> (cylinder (/ (inc ST7789-135*240-mounting-hole-diameter ) 2) ST7789-135*240-viewport-thickness :center false)
       (binding [*fn* 36])
       (translate [0 0 ST7789-135*240-pcb-thickness])))
(def ST7789-135*240-mounting-standoffs
  (ST7789-135*240-mounting-place ST7789-135*240-mounting-standoff))
(def ST7789-135*240-holder

  (union
   (->>
    (cuboid [ST7789-135*240-holder-width ST7789-135*240-holder-length (+ ST7789-135*240-thickness 0.4 0.)]
            :rounding 1.5  :fn 36
            :edges Z)
    (translate [0 0 (- ST7789-135*240-viewport-thickness 0.2)]))
  ;;  (->>
  ;;  (cube ST7789-135*240-holder-width ST7789-135*240-holder-length (+ ST7789-135*240-thickness 0.4))
  ;;  (translate [0 0 (- ST7789-135*240-viewport-thickness 0.2)])
  ;;        (-#))
   (translate [0 0 0.5] ST7789-135*240-viewport)))

(comment (- ST7789-135*240-viewport-thickness 0.2 (/ 0.4 2)))

(comment (/ ST7789-135*240-pcb-length 2))
(def ST7789-135*240-holder-cut
  (let [clearance 0.4]
    (union
     (difference 
     (union
   (->> (cube (+ ST7789-135*240-viewport-width clearance) (+ ST7789-135*240-viewport-length clearance) 6)
        (translate [ST7789-135*240-viewport-x-position ST7789-135*240-viewport-y-position  (+ ST7789-135*240-pcb-thickness 0)]))
   (->> (cube ST7789-135*240-holder-inner-width ST7789-135*240-holder-inner-length (+ ST7789-135*240-thickness 0.4 clearance))
        (translate [0 0 (- ST7789-135*240-viewport-thickness 0.2 clearance)])) 
                       )
     ST7789-135*240-mounting-standoffs)
     ST7789-135*240-mounting-holes
     )))


(def ST7789-135*240
  (union(difference 
   (union  (color [0 0 1 1] ST7789-135*240-pcb)
          ST7789-135*240-mounting-standoffs)
   ST7789-135*240-mounting-holes)
   
   (translate [0 0 0.1](color [0 1 1 1] ST7789-135*240-viewport))
   (color [0 1 0 1] ST7789-135*240-screen )
   )
  )

(spit "things-low/ST7789-135-240-test.scad"
      (write-scad
       (include include-bosl2)
       (union 
        ;(import "../parts/1.14 TFT IPS - Module.stl")
        ST7789-135*240
        (color [1 0 0 0.2] (difference 
         ST7789-135*240-holder
         ST7789-135*240-holder-cut))
        ) 
       
       ))





