(ns dactyl-keyboard.low.fractyl.fractyl-bottom-plate
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [include-bosl2]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vnf :refer [default-vnf-vertex-array-args vnf-polyhedron]]
            [dactyl-keyboard.low.case-low-polyhedron-functions :refer [wall-vnf]]
            [dactyl-keyboard.low.fractyl.fractyl-case-walls :refer :all]
            [dactyl-keyboard.utils :refer [plot-bezier-points]]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))

(spit "things-low/fractyl-bottom-plate-test.scad"
      (write-scad
       (include include-bosl2)
       (let [steps 10
             wall-cross-section-steps 10
             wall-section-steps  10
             {thumb-single-row-wall-section :wall-section
              outer-key-gap-fn-coll :outer-key-gap-fn-coll
              inner-key-gap-fn-coll :inner-key-gap-fn-coll} (thumb-wall-section-for-single-thumb-row-fn wall-cross-section-steps wall-section-steps)
             fractyl-back-wall (fractyl-back-wall wall-cross-section-steps wall-section-steps)
             fractyl-back-wall-bottom-points (:outer-floor-points fractyl-back-wall)
             local-thumb-floor-points (:outer-floor-points (thumb-tr-br-to-middle-lm-local-cubic-curve-interpolation-wall-section-fn wall-cross-section-steps wall-section-steps))
             ;front-wall-nurbs-polyhedron (front-wall-nurbs wall-cross-section-steps wall-section-steps)
             {left-section-data :left-section-data
              thumb-outer-points-fn :thumb-outer-points-fn
              thumb-inner-points-fn :thumb-inner-points-fn
              inner-index-to-index-connector-outer-curve-fn :inner-index-to-index-connector-outer-curve-fn
              inner-index-to-index-connector-inner-curve-fn :inner-index-to-index-connector-inner-curve-fn} (left-section-data steps)
             {left-section-vnf-array :vnf-array
              trackpad-to-main-body-data :trackpad-to-main-body-data
              left-sction-outer-floor :outer-floor-points
              thumb-bl-to-tl-outer-curve-fn :thumb-bl-to-tl-outer-curve-fn
              thumb-tl-to-tr-outer-curve-fn :thumb-tl-to-tr-outer-curve-fn
              thumb-bl-to-tl-inner-curve-fn :thumb-bl-to-tl-inner-curve-fn
              thumb-tl-to-tr-inner-curve-fn :thumb-tl-to-tr-inner-curve-fn} left-section-data
             {fractyl-right-wall-vnf :fractyl-right-wall-vnf
              key-gap-outer-curve-fn-coll :key-gap-outer-curve-fn-coll
              key-gap-inner-curve-fn-coll :key-gap-inner-curve-fn-coll
              fractyl-right-wall-bottom-outer :bottom-outer} (fractyl-right-wall wall-cross-section-steps (/ wall-section-steps 2))
             {trackpad-to-main-body-vnf :trackpad-to-main-body-vnf
              left-side-key-gap-outer-curve-fn-coll :left-side-key-gap-outer-curve-fn-coll
              left-side-key-gap-inner-curve-fn-coll :left-side-key-gap-inner-curve-fn-coll} trackpad-to-main-body-data
             {thumb-to-left-section-outer-floor-points :outer-floor-points
              thumb-to-left-section-inner-floor-points :inner-floor-points} (thumb-to-left-section steps thumb-outer-points-fn thumb-inner-points-fn)]
            ;(println local-thumb-floor-points)
            ;(println (:outer-floor-points (:front-wall-wall-section (front-wall-nurbs wall-cross-section-steps wall-section-steps))))
            
            (union 
             ;(vnf-polyhedron (wall-vnf (:front-wall-wall-section (front-wall-nurbs wall-cross-section-steps wall-section-steps)) default-vnf-vertex-array-args))
            ;;  (plot-bezier-points fractyl-back-wall-bottom-points (sphere 1))
            ;;        (plot-bezier-points left-sction-outer-floor (sphere 1))
            ;;        (plot-bezier-points thumb-to-left-section-outer-floor-points (sphere 1)) 
            ;;        (plot-bezier-points (:outer-floor-points thumb-single-row-wall-section) (sphere 1))
            ;;        (plot-bezier-points  local-thumb-floor-points (sphere 1))
            ;;        (plot-bezier-points (:outer-floor-points (:front-wall-wall-section (front-wall-nurbs wall-cross-section-steps wall-section-steps))) (sphere 1))
            ;;        (plot-bezier-points fractyl-right-wall-bottom-outer (sphere 1))
             (extrude-linear
              {:height 1.5 :center false :convexity 10} (polygon  (map drop-last (concat fractyl-back-wall-bottom-points
                                                                                         left-sction-outer-floor
                                                                                         thumb-to-left-section-outer-floor-points
                                                                                         (reverse (:outer-floor-points thumb-single-row-wall-section))
                                                                                         local-thumb-floor-points
                                                                                         (reverse (:outer-floor-points (:front-wall-wall-section (front-wall-nurbs wall-cross-section-steps wall-section-steps))))
                                                                                         fractyl-right-wall-bottom-outer))))
                   ))))