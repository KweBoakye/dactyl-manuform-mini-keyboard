(ns dactyl-keyboard.MxLEDBitPCB-holder
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]))



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
    MxLEDBitPCB-holder-leg-hook) 
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
   MxLEDBitPCB-holder-leg-1
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

  