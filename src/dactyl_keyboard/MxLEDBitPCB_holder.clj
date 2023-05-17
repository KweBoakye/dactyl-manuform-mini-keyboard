(ns dactyl-keyboard.MxLEDBitPCB-holder
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix.protocols :refer [sqrt]]
            [clojure.math :refer [tan]]
            [dactyl-keyboard.lib.curvesandsplines.beziers :refer [bezier-cubic
                                                                  bezier-linear n-degree-bezier-curve]]
            [dactyl-keyboard.lib.curvesandsplines.curve-fitting :refer [global-curve-interp-with-calculated-first-derivatives-curve]]
            [dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline :refer [nurbs]]
            [dactyl-keyboard.lib.geometry :refer [deg2rad]]
            [dactyl-keyboard.lib.matrices :refer [rotate-matrix]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [include-bosl2]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vnf :refer [vnf-polyhedron
                                                                     vnf-vertex-array]]
            [dactyl-keyboard.lib.openscad.polyhedrons :refer [generate-bezier-along-bezier-polyhedron-from-points-list-linear]]
            [dactyl-keyboard.lib.transformations :refer [rdz]]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))



(def MxLEDBitPCB-holder-width 18.1)
(def MxLEDBitPCB-holder-length 18.1)
(def MxLEDBitPCB-holder-thickness 1.6)
(def MxLEDBitPCB-holder-central-hole-diameter 4.4)
(def MxLEDBitPCB-holder-central-hole-radius (/ MxLEDBitPCB-holder-central-hole-diameter 2))
(def MxLEDBitPCB-holder-central-hole-distance-from-bottom 7)
(def MxLEDBitPCB-holder-central-hole-distance-from-right 7) 
(def MxLEDBitPCB-holder-leg-thickness 2)
(def MxLEDBitPCB-holder-crush-rib-thickness 0.2)
(def MxLEDBitPCB-holder-leg-depth (+ (* MxLEDBitPCB-holder-thickness 2) 0.5))
(def MxLEDBitPCB-holder-leg-z-coordinate (-  (+ (/ MxLEDBitPCB-holder-leg-depth 2) 0.5)))
(def MxLEDBitPCB-holder-leg-position-1 [(+ (/ MxLEDBitPCB-holder-width -2) 9)
                                        (+ (/ MxLEDBitPCB-holder-length 2) (/ MxLEDBitPCB-holder-leg-thickness 2))
                                        MxLEDBitPCB-holder-leg-z-coordinate])
(def MxLEDBitPCB-holder-leg-position-2 [(+ (/ MxLEDBitPCB-holder-width -2) 7.3)
                                        (+ (/ MxLEDBitPCB-holder-length -2) (/ MxLEDBitPCB-holder-leg-thickness -2))
                                        MxLEDBitPCB-holder-leg-z-coordinate])
(def MxLEDBitPCB-holder-leg-position-3 [(+ (/ MxLEDBitPCB-holder-width -2) 5)
                                        (+ (/ MxLEDBitPCB-holder-length 2) (/ MxLEDBitPCB-holder-leg-thickness 2))
                                        MxLEDBitPCB-holder-leg-z-coordinate])
(def MxLEDBitPCB-holder-leg-hook-coordinates-top [[0 (/ MxLEDBitPCB-holder-leg-thickness -2) 0] 
                                              [0 (/ MxLEDBitPCB-holder-leg-thickness -4) (+ (/ MxLEDBitPCB-holder-leg-thickness -4) (/ MxLEDBitPCB-holder-leg-depth -4))]
                                              [0 0 (+ (/ MxLEDBitPCB-holder-leg-thickness -2) (/ MxLEDBitPCB-holder-leg-depth -4))]
                                              [0 (+ (/ MxLEDBitPCB-holder-leg-thickness 2) 0.25) 0]])

(def MxLEDBitPCB-holder-leg-hook-coordinates-bottom [ [0 (+ (/ MxLEDBitPCB-holder-leg-thickness 2) 0.25) 0] [0 (/ MxLEDBitPCB-holder-leg-thickness -2) 0]])

(defn MxLEDBitPCB-holder-leg-hook-coordinates-to-the-left [points]
  (mapv #(mapv + [(/ MxLEDBitPCB-holder-leg-thickness -2) 0 0] %) points)
  )

(defn MxLEDBitPCB-holder-leg-hook-coordinates-to-the-right [points]
  (mapv #(mapv + [(/ MxLEDBitPCB-holder-leg-thickness 2) 0 0] %) points))

(def MxLEDBitPCB-holder-leg-hook
  (let [steps 36]
  (generate-bezier-along-bezier-polyhedron-from-points-list-linear
   (apply bezier-cubic (concat (MxLEDBitPCB-holder-leg-hook-coordinates-to-the-right MxLEDBitPCB-holder-leg-hook-coordinates-top) [steps]))(apply bezier-cubic (concat (MxLEDBitPCB-holder-leg-hook-coordinates-to-the-left MxLEDBitPCB-holder-leg-hook-coordinates-top) [steps]))
      (apply bezier-linear (concat (MxLEDBitPCB-holder-leg-hook-coordinates-to-the-right MxLEDBitPCB-holder-leg-hook-coordinates-bottom) [steps])) (apply bezier-linear (concat (MxLEDBitPCB-holder-leg-hook-coordinates-to-the-left MxLEDBitPCB-holder-leg-hook-coordinates-bottom) [steps])) 
   steps
   ))
  
  )

(def MxLEDBitPCB-body
  (cube MxLEDBitPCB-holder-width MxLEDBitPCB-holder-length MxLEDBitPCB-holder-thickness))

(def MxLEDBitPCB-holder-central-hole
  (->>
   (binding [*fn* 36] (cylinder MxLEDBitPCB-holder-central-hole-radius (+ MxLEDBitPCB-holder-thickness 0.2)))
   (translate [(- (/ MxLEDBitPCB-holder-width 2) MxLEDBitPCB-holder-central-hole-radius MxLEDBitPCB-holder-central-hole-distance-from-right) 
               (+ (/ MxLEDBitPCB-holder-length -2) MxLEDBitPCB-holder-central-hole-radius MxLEDBitPCB-holder-central-hole-distance-from-bottom) 0]) 
   )
  )

(def MxLEDBitPCB-holder-crush-rib 
  (->>
   (binding [*fn* 36](cylinder [(/ MxLEDBitPCB-holder-crush-rib-thickness 2) MxLEDBitPCB-holder-crush-rib-thickness] (/ MxLEDBitPCB-holder-leg-depth 2)))
   (translate [0 (- (/ MxLEDBitPCB-holder-leg-thickness 2)(/ MxLEDBitPCB-holder-crush-rib-thickness 2)) (/ MxLEDBitPCB-holder-leg-depth 4)]))
  )

(def MxLEDBitPCB-holder-leg 
   (union
   (->>
    (cube MxLEDBitPCB-holder-leg-thickness MxLEDBitPCB-holder-leg-thickness (/ MxLEDBitPCB-holder-leg-depth 2))
    (translate [0 0 (/ MxLEDBitPCB-holder-leg-depth 4)]))
    MxLEDBitPCB-holder-crush-rib
    MxLEDBitPCB-holder-leg-hook
    (->>
     (cube MxLEDBitPCB-holder-leg-thickness MxLEDBitPCB-holder-leg-thickness (/ MxLEDBitPCB-holder-leg-depth 2)) 
     (translate [0 0 (- MxLEDBitPCB-holder-leg-z-coordinate)])
     )
    ) 
  )
 
(def MxLEDBitPCB-holder-leg-1
  
  (->>( rdz 180 MxLEDBitPCB-holder-leg)
   (translate MxLEDBitPCB-holder-leg-position-1 ))
  )

(def MxLEDBitPCB-holder-leg-2
  (translate MxLEDBitPCB-holder-leg-position-2 MxLEDBitPCB-holder-leg))

(def MxLEDBitPCB-holder-leg-3

  (->> (rdz 180 MxLEDBitPCB-holder-leg)
       (translate MxLEDBitPCB-holder-leg-position-3)))

(def MxLEDBitPCB-holder-legs
  (union
;   MxLEDBitPCB-holder-leg-1
MxLEDBitPCB-holder-leg-2
MxLEDBitPCB-holder-leg-3
   )
  )
(def MxLEDBitPCB
  (->>
   (difference 
    MxLEDBitPCB-body
          
   MxLEDBitPCB-holder-central-hole
   )
   (translate [0 0 (- (/ MxLEDBitPCB-holder-thickness 2) (/ plate-thickness 2) 0.25)])
   ) 
  )

(def MxLEDBitPCB-clearance
  (->>
   (cube (+ MxLEDBitPCB-holder-width 0.5) (+ MxLEDBitPCB-holder-length 0.5) (+ MxLEDBitPCB-holder-thickness 2))
   (translate [0 0 (- (/ (+ MxLEDBitPCB-holder-thickness ) 2) (/ plate-thickness 2) 0.85 0.25)])
   ))

(def MxLEDBitPCB-clearance-smaller
  (->>
   (cube (+ MxLEDBitPCB-holder-width 0.2) (+ MxLEDBitPCB-holder-length 0.2) (+ MxLEDBitPCB-holder-thickness 0.5))
   (translate [0 0 (- (/ (+ MxLEDBitPCB-holder-thickness) 2) (/ plate-thickness 2) 0.25)])))

(def new-thickness (* plate-thickness 0.5))
(def end-thickness (* new-thickness 0.5))
(def hook-thickness (- end-thickness 0.5))
(def length (- MxLEDBitPCB-holder-leg-depth 0.3))
(def insertion-angle 30)
(def retention-face-angle 85)
(def retention-face-depth (+ hook-thickness 0.5) )
(def width length)
(def end-width (/ length 3))
(def hook-length (* length 0.2))
(def fillet-radius 0.25)
(def hook-z-offset (* retention-face-depth (tan (deg2rad (- 90 retention-face-angle)))))

(def inner-right-top-pos [(/ width 2) (/ new-thickness -2) 0])
(def inner-right-top-base-fillet-pos (mapv + inner-right-top-pos [0 (- fillet-radius) 0]))
(def inner-right-top-upper-fillet-pos (mapv + inner-right-top-pos [0 0 (- fillet-radius)]))
(def inner-left-top-pos [(/ width -2) (/ new-thickness -2) 0])
(def inner-left-top-base-fillet-pos (mapv + inner-left-top-pos [0 (- fillet-radius) 0]))
(def inner-left-top-upper-fillet-pos (mapv + inner-left-top-pos [0 0 (- fillet-radius)]))
(def outer-right-top-pos [(/ width 2) (/ new-thickness 2) 0])
(def outer-right-top-base-fillet-pos (mapv + outer-right-top-pos [0 fillet-radius 0]))
(def outer-right-top-upper-fillet-pos (mapv + outer-right-top-pos [0 0 (- fillet-radius)]))
(def outer-left-top-pos [(/ width -2) (/ new-thickness 2) 0])
(def outer-left-top-base-fillet-pos (mapv + outer-left-top-pos [0 fillet-radius 0]))
(def outer-left-top-upper-fillet-pos (mapv + outer-left-top-pos [0 0 (- fillet-radius)]))
(def inner-right-bottom-pos [(/ end-width 2) (/ new-thickness -2) (- length)])
(def inner-left-bottom-pos [(/ end-width -2) (/ new-thickness -2) (- length)])
(def outer-right-bottom-pos [(/ end-width 2) (/ end-thickness 2) (- (+ length hook-length))])
(def mid-right-bottom-pos [(/ end-width 2) (/ new-thickness -4) (- 0.2 (+ length hook-length))])
(def outer-left-bottom-pos [(/ end-width -2) (/ end-thickness 2) (- (+ length hook-length))])
(def mid-left-bottom-pos [(/ end-width -2) (/ new-thickness -4) (- 0.2 (+ length hook-length))])
(def hook-left-pos [(/ end-width -2) (- (/ hook-thickness -2) end-thickness ) (- (+ length hook-z-offset))])
(def hook-right-pos [(/ end-width 2) (- (/ hook-thickness -2) end-thickness) (- (+ length hook-z-offset))])

(def right-curve 
  (vec(concat (drop-last (nurbs
                      [inner-right-top-base-fillet-pos
                       inner-right-top-pos
                       inner-right-top-upper-fillet-pos
                       inner-right-bottom-pos]
                      2
                      [0 0 0 (/ 4 3) 2 2 2]
                      [1 (/ (sqrt 2) 2) 1 1]
                      10)
                     )
          (drop-last (global-curve-interp-with-calculated-first-derivatives-curve
                      [inner-right-bottom-pos
                       hook-right-pos
                       mid-right-bottom-pos
                       outer-right-bottom-pos
                       outer-right-top-upper-fillet-pos
            ;outer-right-top-base-fillet-pos
                       ]
                      [(mapv - hook-right-pos inner-right-bottom-pos)
                       (mapv - mid-right-bottom-pos hook-right-pos)
                       (mapv - outer-right-bottom-pos mid-right-bottom-pos)
                       (mapv - outer-right-top-upper-fillet-pos outer-right-bottom-pos)
                       (mapv -  outer-right-top-pos outer-right-top-upper-fillet-pos)]
                      2
                      20
                      :point-paramater-calculation-method :centripetal)
                     )
          (nurbs
           [outer-right-top-upper-fillet-pos
            outer-right-top-pos
            outer-right-top-base-fillet-pos]
           2
           [0 0 0 1 1 1]
           [1 (/ (sqrt 2) 2) 1]
           10)
          ))
  ;; (global-curve-interp-curve 
  ;;  [inner-right-top-base-fillet-pos 
  ;;   inner-right-top-upper-fillet-pos
  ;;   inner-right-bottom-pos
  ;;   hook-right-pos
  ;;   outer-right-bottom-pos
  ;;   outer-right-top-upper-fillet-pos
  ;;   outer-right-top-base-fillet-pos]
  ;;  2 
  ;;  30
  ;;  :point-paramater-calculation-method :circular)
  )

(def left-curve (vec (concat (drop-last (nurbs
                                         [inner-left-top-base-fillet-pos
                                          inner-left-top-pos
                                          inner-left-top-upper-fillet-pos
                                          inner-left-bottom-pos]
                                         2
                                         [0 0 0 (/ 4 3) 2 2 2]
                                         [1 (/ (sqrt 2) 2) 1 1]
                                         10))
                             (drop-last (global-curve-interp-with-calculated-first-derivatives-curve
                                         [inner-left-bottom-pos
                                          hook-left-pos
                                          mid-left-bottom-pos
                                          outer-left-bottom-pos
                                          outer-left-top-upper-fillet-pos
            ;outer-left-top-base-fillet-pos
                                          ]
                                         [(mapv - hook-left-pos inner-left-bottom-pos)
                                          (mapv - mid-left-bottom-pos hook-left-pos)
                                          (mapv - outer-left-bottom-pos mid-left-bottom-pos) 
                                          (mapv - outer-left-top-upper-fillet-pos outer-left-bottom-pos)
                                          (mapv -  outer-left-top-pos outer-left-top-upper-fillet-pos)]
                                         2
                                         20
                                         :point-paramater-calculation-method :dynamic-centripetal))
                             (nurbs
                              [outer-left-top-upper-fillet-pos
                               outer-left-top-pos
                               outer-left-top-base-fillet-pos]
                              2
                              [0 0 0 1 1 1]
                              [1 (/ (sqrt 2) 2) 1]
                              10))))

(def shape (vec (for [index (range (count right-curve))]
             (n-degree-bezier-curve 
              [(nth right-curve index)
               (nth left-curve index) 
               ]
              2))))
(def holder-leg (union
             (vnf-polyhedron (vnf-vertex-array (rotate-matrix shape) :caps true :col-wrap true))
             (translate [0 (- 0.2 new-thickness) (+ (* MxLEDBitPCB-holder-leg-z-coordinate 1.5) 0.2)] MxLEDBitPCB-holder-crush-rib)))

(def single-key-pcb-holder-north-leg
  (translate [0 (/ (+ MxLEDBitPCB-holder-length new-thickness) 2) (- (/ MxLEDBitPCB-holder-leg-z-coordinate -2) 0.2)] (union
                                                                                                                       holder-leg))
  )

(def single-key-pcb-holder-south-leg
   (translate [0 (/ (+ MxLEDBitPCB-holder-length new-thickness) -2) (- (/ MxLEDBitPCB-holder-leg-z-coordinate -2) 0.2)] (rdz 180 holder-leg))
  )
(def single-key-pcb-holder 
  (union
   single-key-pcb-holder-north-leg
   single-key-pcb-holder-south-leg
  )
  )
(spit "things-low/new-holder-test.scad"
      (write-scad
       (include include-bosl2)
       (union 
        (translate mid-right-bottom-pos (sphere 0.2))
        ;(translate inner-right-top-base-fillet-pos (sphere 0.2))
        ;(translate inner-right-top-upper-fillet-pos (sphere 0.2))
        ;(translate inner-right-top-pos (sphere 0.2))
        ;; (translate inner-left-top-pos (sphere 0.2))
        ;; (translate inner-left-top-base-fillet-pos (sphere 0.2))
        ;; (translate inner-left-top-upper-fillet-pos (sphere 0.2)) 
        ;; (translate outer-left-top-pos (sphere 0.2))
        ;; (translate outer-left-top-base-fillet-pos (sphere 0.2))
        ;; (translate outer-left-top-upper-fillet-pos (sphere 0.2))
        ;; (translate outer-right-top-pos (sphere 0.2))
        ;; (translate outer-right-top-base-fillet-pos (sphere 0.2))
        ;; (translate outer-right-top-upper-fillet-pos (sphere 0.2))
        ;; (translate inner-right-bottom-pos (sphere 0.2))
        ;; (translate inner-left-bottom-pos (sphere 0.2))
        ;; (translate outer-left-bottom-pos (sphere 0.2))
        ;; (translate outer-right-bottom-pos (sphere 0.2))
        ;; (translate hook-left-pos (sphere 0.2))
        ;; (translate hook-right-pos (sphere 0.2))
        ;; (color [1 0 0 1](plot-bezier-points
        ;;  right-curve
        ;;  (sphere 0.3)
        ;;  ))
        ;; (color [1 0 0 1] (plot-bezier-points
        ;;                   left-curve
        ;;                   (sphere 0.3)))
        holder-leg
        )))




(spit "things-low/switch-test.scad"
     (write-scad
      (include include-bosl2)
      (union
       ;(-# single-plate)
       
       (translate [0 (/ (+ MxLEDBitPCB-holder-length new-thickness) 2) (- (/ MxLEDBitPCB-holder-leg-z-coordinate -2) 0.2)] (union 
                                                                                                                            holder-leg))
       (translate [0 (/ (+ MxLEDBitPCB-holder-length new-thickness) -2) (- (/ MxLEDBitPCB-holder-leg-z-coordinate -2) 0.2)] (rdz 180 holder-leg))
       ;switch-model
       ;(-# dsa-cap)
       ;(translate [0.75 -4.75 (- plate-thickness)](-# (import "../parts/Kailh Hotswap MX v22.stl")))
       (-# MxLEDBitPCB)
       MxLEDBitPCB-holder-legs
       )))


  