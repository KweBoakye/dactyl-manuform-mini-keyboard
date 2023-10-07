(ns dactyl-keyboard.low.fractyl.fractyl-bottom-plate
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.drv2605l-standoffs :refer [drv2605l-length
                                                        drv2605l-place
                                                        drv2605l-standoffs drv2605l-width]]
            [dactyl-keyboard.IS31FL3743A-mount :refer [IS31FL3743A-fillet-standoffs
                                                       IS31FL3743A-fillet-standoffs-reverse IS31FL3743A-standoff-place]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [include-bosl2]]
            [dactyl-keyboard.lib.transformations :refer [rdy rdz]]
            [dactyl-keyboard.low.fractyl.fractyl-case-walls :refer :all]
            [dactyl-keyboard.low.fractyl.fractyl-screw-inserts :refer [fractyl-screw-insert-screw-holes]]
            [dactyl-keyboard.RP2040-Plus :refer [rp2040-plus-mount
                                                 rp2040-plus-place]]
            [dactyl-keyboard.six-pin-ffc-adapter-board :refer [six-pin-ffc-adapter-place
                                                               six-pin-ffc-adapter-standoffs]]
            [dactyl-keyboard.sk8707-51 :refer :all]
            [dactyl-keyboard.spi-eeprom-breakout :refer [spi-eeprom-breakout-mount
                                                         spi-eeprom-breakout-place
                                                         spi-eeprom-breakout]]
            [dactyl-keyboard.tps-65-breakout :refer :all]
            [dactyl-keyboard.utils :refer [plot-bezier-points]]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))


(defn fractyl-bottom-points [wall-cross-section-steps wall-section-steps ;&{:keys []}
                             ]
  
   (let [{thumb-single-row-wall-section :wall-section
          outer-key-gap-fn-coll :outer-key-gap-fn-coll
          inner-key-gap-fn-coll :inner-key-gap-fn-coll} (thumb-wall-section-for-single-thumb-row-fn wall-cross-section-steps wall-section-steps)
         fractyl-back-wall (fractyl-back-wall wall-cross-section-steps wall-section-steps)
         fractyl-back-wall-outer-bottom-points (:outer-floor-points fractyl-back-wall)
         fractyl-back-wall-inner-bottom-points (:inner-floor-points fractyl-back-wall)
         local-thumb-floor-points (:outer-floor-points (thumb-tr-br-to-middle-lm-local-cubic-curve-interpolation-wall-section-fn wall-cross-section-steps wall-section-steps))
             ;front-wall-nurbs-polyhedron (front-wall-nurbs wall-cross-section-steps wall-section-steps)
         {left-section-data :left-section-data
          thumb-outer-points-fn :thumb-outer-points-fn
          thumb-inner-points-fn :thumb-inner-points-fn
          inner-index-to-index-connector-outer-curve-fn :inner-index-to-index-connector-outer-curve-fn
          inner-index-to-index-connector-inner-curve-fn :inner-index-to-index-connector-inner-curve-fn} (left-section-data wall-section-steps :screen-outer-curve-type :local)
         {left-section-vnf-array :vnf-array
          trackpad-to-main-body-data :trackpad-to-main-body-data
          left-section-outer-floor :outer-floor-points
          left-section-inner-floor :inner-floor-points
          thumb-bl-to-tl-outer-curve-fn :thumb-bl-to-tl-outer-curve-fn
          thumb-tl-to-tr-outer-curve-fn :thumb-tl-to-tr-outer-curve-fn
          thumb-bl-to-tl-inner-curve-fn :thumb-bl-to-tl-inner-curve-fn
          thumb-tl-to-tr-inner-curve-fn :thumb-tl-to-tr-inner-curve-fn} left-section-data
         {fractyl-right-wall-vnf :fractyl-right-wall-vnf
          key-gap-outer-curve-fn-coll :key-gap-outer-curve-fn-coll
          key-gap-inner-curve-fn-coll :key-gap-inner-curve-fn-coll
          fractyl-right-wall-bottom-outer :bottom-outer
          fractyl-right-wall-bottom-inner :bottom-inner} (fractyl-right-wall wall-cross-section-steps (/ wall-section-steps 2))
         {thumb-tr-rm-to-index-br-vnf-array :vnf-array
          thumb-tr-rm-to-index-br-outer-bottom-points :outer-bottom-points
          thumb-tr-rm-to-index-br-inner-bottom-points :inner-bottom-points} (thumb-tr-rm-to-index-br wall-cross-section-steps wall-section-steps)
         {trackpad-to-main-body-vnf :trackpad-to-main-body-vnf
          left-side-key-gap-outer-curve-fn-coll :left-side-key-gap-outer-curve-fn-coll
          left-side-key-gap-inner-curve-fn-coll :left-side-key-gap-inner-curve-fn-coll} trackpad-to-main-body-data
         {thumb-to-left-section-outer-floor-points :outer-floor-points
          thumb-to-left-section-inner-floor-points :inner-floor-points} (thumb-to-left-section-2 wall-cross-section-steps wall-section-steps thumb-outer-points-fn thumb-inner-points-fn)
         front-wall (:front-wall-wall-section (front-wall-nurbs wall-cross-section-steps wall-section-steps))
         front-wall-outer-floor-points (reverse (:outer-floor-points front-wall))
         front-wall-inner-floor-points (:inner-floor-points front-wall)
         outer-points (vec (concat (reverse fractyl-back-wall-outer-bottom-points)
                              left-section-outer-floor
                              thumb-to-left-section-outer-floor-points
                              (reverse (:outer-floor-points thumb-single-row-wall-section))
                              thumb-tr-rm-to-index-br-outer-bottom-points
                              front-wall-outer-floor-points
                              fractyl-right-wall-bottom-outer
                                   ))
         inner-points (vec (concat (reverse fractyl-back-wall-inner-bottom-points)
                              left-section-inner-floor
                              thumb-to-left-section-inner-floor-points
                              (reverse (:inner-floor-points thumb-single-row-wall-section))
                              thumb-tr-rm-to-index-br-inner-bottom-points
                              front-wall-inner-floor-points
                              fractyl-right-wall-bottom-inner))] 
         {:outer-points outer-points  :inner-points inner-points}))

(defn fractyl-bottom-plate [wall-cross-section-steps wall-section-steps &{:keys [show-inner-points] :or {show-inner-points false}}]
  (let [{outer-points :outer-points
         inner-points  :inner-points} (fractyl-bottom-points wall-cross-section-steps wall-section-steps)]
    (union (->> (extrude-linear
          {:height 1.5 :center false :convexity 10} (polygon  (map drop-last outer-points)))
         (translate [0 0 -1.5]))
           (cond show-inner-points (plot-bezier-points inner-points (sphere 0.5))))
    )
  )

(spit "things-low/IS31FL3743x_Breakout-test.scad"
 (write-scad
  (union
   (->>
    (import "../parts/IS31FL3743x_Breakout.stl")
    (translate [-100 100 -0.8])
    (rdy 180)
    ;(translate [2 0 0])
                 (rdz 180)
    ;(translate [0 0 4])
    ;(IS31FL3743A-standoff-place)
    (-#))
   (->>
    (import "../parts/IS31FL3743x_Breakout.stl")
    (translate [-100 100 -0.8])
                 ;(rdy 180)
                 ;(rdz 180)
    ;(translate [0 0 4])
    ;(IS31FL3743A-standoff-place)
    )
   )
  )
)

(spit "things-low/fractyl-bottom-plate-test.scad"
      (write-scad
       (include include-bosl2)
       (let [steps 10
             wall-cross-section-steps 10
             wall-section-steps  10
             hole-depth 10 
             
             ]
            ;(println local-thumb-floor-points)
            ;(println (:outer-floor-points (:front-wall-wall-section (front-wall-nurbs wall-cross-section-steps wall-section-steps))))
            
            (union
            ;;  (->>
            ;;   (import "../parts/Tps-65-breakout.stl")
            ;;   (six-pin-ffc-adapter-place)
            ;;   ;(translate [-220 70 2])
            ;;   (translate [-132.5 60 2])
            ;;   (color [1 0 0 1]))
             (->>
              (import "../parts/IS31FL3743x_Breakout.stl")
              (translate [-100 100 -0.8])
              (rdy 180)
              ;(rdz 180)
              (translate [0 0 4])
              (IS31FL3743A-standoff-place)
              (-#)
              )
            ;;  (->>
            ;;   (import "../parts/IS31FL3743x_Breakout.stl")
            ;;   (translate [-100 100 0])
            ;;   ;(rdy 180)
            ;;   ;(rdz 180)
            ;;   (translate [0 0 4])
            ;;   (IS31FL3743A-standoff-place)
            ;;   )
             (tps-65-breakout-place (union tps-65-breakout-mounting-standoffs
                                           tps-65-breakout))
             (difference (union (sk8707-51-place (sk8707-51-mount))
                                (difference (hull (sk8707-51-place (translate [0 0 (- 2)] (sk8707-51-mount :sk8707-51-mount-thickness 0.1)))
                                                  (extrude-linear {:height 0.1 :center false} (project (sk8707-51-place (translate [0 0 (- 2)] (sk8707-51-mount :sk8707-51-mount-thickness 0.1))))))
                                            (sk8707-51-place (sk8707-51-mount-cutout :sk8707-51-mount-thickness 10))))
                         sk8707-51-mount-heat-insert-holes)
             (IS31FL3743A-standoff-place IS31FL3743A-fillet-standoffs-reverse)
             (drv2605l-place drv2605l-standoffs)
             (drv2605l-place (translate [(/ drv2605l-width -2) (/ drv2605l-length -2) 4] (import "../parts/2305 DRV2605L.stl")))
             ;(six-pin-ffc-adapter-place six-pin-ffc-adapter-standoffs)
             ;(rp2040-plus-place rp2040-plus-mount)
             (sk8707-51-breakout-place (union (translate [0 0 4] sk8707-51-breakout)
                                           sk8707-51-breakout-mount))
            ;;  (->> (import "../parts/spi_eeprom_breakout.stl")
            ;;       (translate [-160 40 0]))
             (spi-eeprom-breakout-place (union 
                                         spi-eeprom-breakout-mount
                                         spi-eeprom-breakout))
             (rp2040-plus-place rp2040-plus-mount :place-fn (fn [shape] (usb-jack-place-new shape :extra-z-rot -1.5)))
             (difference (fractyl-bottom-plate wall-cross-section-steps wall-section-steps :show-inner-points true) 
                         (translate [0 0 -1.5] fractyl-screw-insert-screw-holes)
                         sk8707-51-mount-heat-insert-holes)
             ))))

(defn fractyl-bottom-plate-and-mounts [wall-cross-section-steps wall-section-steps &{:keys [side trackpoint-mount]
                                                                                     :or {side :right trackpoint-mount false}}]
  (let [trackpoint-mount-array (union (sk8707-51-place (sk8707-51-mount))
                                      (difference (hull (sk8707-51-place (translate [0 0 (- 2)] (sk8707-51-mount :sk8707-51-mount-thickness 0.1)))
                          (extrude-linear {:height 0.1 :center false} (project (sk8707-51-place (translate [0 0 (- 2)] (sk8707-51-mount :sk8707-51-mount-thickness 0.1))))))
                    (sk8707-51-place (sk8707-51-mount-cutout :sk8707-51-mount-thickness 10))))
        mount-heat-insert-holes (sk8707-51-place sk8707-51-mount-heat-insert-holes)](cond->>
   (union
    (IS31FL3743A-standoff-place 
     (if (= side :left) (mirror [1 0 0] IS31FL3743A-fillet-standoffs-reverse)
         IS31FL3743A-fillet-standoffs-reverse))
   (drv2605l-place drv2605l-standoffs)
     (tps-65-breakout-place tps-65-breakout-mounting-standoffs)
    (spi-eeprom-breakout-place  spi-eeprom-breakout-mount)
   (rp2040-plus-place rp2040-plus-mount :place-fn (fn [shape] (usb-jack-place-new shape :extra-z-rot -1.5)))
   (difference (fractyl-bottom-plate wall-cross-section-steps wall-section-steps )
               (translate [0 0 -1.5] fractyl-screw-insert-screw-holes)
               mount-heat-insert-holes)
    
    (cond trackpoint-mount (union 
                            (difference trackpoint-mount-array mount-heat-insert-holes)
                             (sk8707-51-breakout-place sk8707-51-breakout-mount)))
    ) 
   (= side :left) (mirror [1 0 0]))))

(spit "things-low/fractyl-bottom-plate-right.scad"
      (write-scad
(include include-bosl2) 
       (fractyl-bottom-plate-and-mounts 10 10 :trackpoint-mount true))
      )

(spit "things-low/fractyl-bottom-plate-left.scad"
      (write-scad
       (include include-bosl2)
       (fractyl-bottom-plate-and-mounts 10 10 :side :left)))

(spit "things-low/fractyl-bottom-plate-left-test.scad"
      (write-scad
       (include include-bosl2)
       (union (fractyl-bottom-plate-and-mounts 10 10 :side :left)
              
               (->>
                (import "../parts/IS31FL3743x_Breakout.stl")
                (translate [-100 100 -0.8])
                (rdy 180)
                             ;(rdz 180)
                (translate [0 0 4])
                (translate [66 -20 0])
                (-#)
                ))))