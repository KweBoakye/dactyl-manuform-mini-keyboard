(ns dactyl-keyboard.low.fractyl.fractyl-bottom-plate
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.drv2605l-standoffs :refer [drv2605l-place
                                                        drv2605l-standoffs]]
            [dactyl-keyboard.IS31FL3743A-mount :refer [IS31FL3743A-fillet-standoffs
                                                       IS31FL3743A-standoff-place]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [include-bosl2]]
            [dactyl-keyboard.low.fractyl.fractyl-case-walls :refer :all]
            [dactyl-keyboard.low.fractyl.fractyl-screw-inserts :refer [fractyl-screw-insert-screw-holes]]
            [dactyl-keyboard.RP2040-Plus :refer [rp2040-plus-mount
                                                 rp2040-plus-place]]
            [dactyl-keyboard.six-pin-ffc-adapter-board :refer [six-pin-ffc-adapter-place
                                                               six-pin-ffc-adapter-standoffs]]
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
         outer-points (vec (concat fractyl-back-wall-outer-bottom-points
                              left-section-outer-floor
                              thumb-to-left-section-outer-floor-points
                              (reverse (:outer-floor-points thumb-single-row-wall-section))
                              thumb-tr-rm-to-index-br-outer-bottom-points
                              front-wall-outer-floor-points
                              fractyl-right-wall-bottom-outer))
         inner-points (vec (concat fractyl-back-wall-inner-bottom-points
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
(spit "things-low/fractyl-bottom-plate-test.scad"
      (write-scad
       (include include-bosl2)
       (let [steps 10
             wall-cross-section-steps 10
             wall-section-steps  10
             
             ]
            ;(println local-thumb-floor-points)
            ;(println (:outer-floor-points (:front-wall-wall-section (front-wall-nurbs wall-cross-section-steps wall-section-steps))))
            
            (union
             (IS31FL3743A-standoff-place IS31FL3743A-fillet-standoffs)
             (drv2605l-place drv2605l-standoffs)
             (six-pin-ffc-adapter-place six-pin-ffc-adapter-standoffs)
             ;(rp2040-plus-place rp2040-plus-mount)
             (rp2040-plus-place rp2040-plus-mount :place-fn (fn [shape] (usb-jack-place-new shape :extra-z-rot -1.5)))
             (difference (fractyl-bottom-plate wall-cross-section-steps wall-section-steps :show-inner-points true)
                         (translate [0 0 -1.5] fractyl-screw-insert-screw-holes))))))