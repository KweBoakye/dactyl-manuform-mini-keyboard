(ns dactyl-keyboard.EVQWGD001
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]))


(def EVQWGD001-width 16.71)
(def EVQWGD001-length 13.9)
(def EVQWGD001-height 13.4)
(def EVQWGD001-roller-radius 6.95)
(def EVQWGD001-rolle-height 12.5)


(def EVQWGD001-gap-for-plastic 2.4)
(def EVQWGD001-gap-for-pins-width 3.85)
(def EVQWGD001-gap-for-pins-height (+ plate-thickness 4))
(def EVQWGD001-plastic-height 5.20)
(def EVQWGD001-mount-width (+ EVQWGD001-width 2))
(def EVQWGD001-mount-length (+ EVQWGD001-length 2))
(def EVQWGD001-mount-height (+ EVQWGD001-plastic-height plate-thickness))
(def EVQWGD001-mount-corner-radius 2)
(def EVQWGD001-mount-y-modifier 2)

(def EVQWGD001-top-left  [(- (/ EVQWGD001-width 2)) (/ EVQWGD001-length 2) 0])
(def EVQWGD001-top-right  [(/ EVQWGD001-width 2) (/ EVQWGD001-length 2) 0])
(def EVQWGD001-bottom-left  [(- (/ EVQWGD001-width 2)) (- (/ EVQWGD001-length 2)) (- (/ EVQWGD001-plastic-height 2) (/ plate-thickness 2))])
(def EVQWGD001-bottom-right   [(/ EVQWGD001-width 2) (- (/ EVQWGD001-length 2)) 0])

(def EVQWGD001-mount-top-left  [(- (/ EVQWGD001-mount-width 2)) (/ EVQWGD001-mount-length 2) 1])
(def EVQWGD001-mount-top-right  [(/ EVQWGD001-mount-width 2) (/ EVQWGD001-mount-length 2) 1])
(def EVQWGD001-mount-bottom-left  [(- (/ EVQWGD001-mount-width 2)) (- (/ EVQWGD001-mount-length 2)) 1])
(def EVQWGD001-mount-bottom-left-adjusted  [(- (+ (/ EVQWGD001-mount-width 2) 2)) (- (/ EVQWGD001-mount-length 2)) 1])
(def EVQWGD001-mount-bottom-right   [(/ EVQWGD001-mount-width 2) (- (/ EVQWGD001-mount-length 2)) 1])

(def EVQWGD001-holder-base-top-left  [(- (/ EVQWGD001-mount-width 2)) (/ EVQWGD001-mount-length 2) 0])
(def EVQWGD001-holder-base-top-right  [(/ EVQWGD001-mount-width 2) (/ EVQWGD001-mount-length 2) 0])
(def EVQWGD001-holder-base-bottom-left  [(- (/ EVQWGD001-mount-width 2)) (- (/ EVQWGD001-mount-length 2)) 0])
(def EVQWGD001-holder-base-bottom-right   [(/ EVQWGD001-mount-width 2) (- (/ EVQWGD001-mount-length 2)) 0])


(def  EVQWGD001-test-base
  (->>
   (cube (+ EVQWGD001-width 2) (+ EVQWGD001-length 2) (+ EVQWGD001-plastic-height plate-thickness))
   (translate [0 0 (- (/ EVQWGD001-plastic-height 2) (/ plate-thickness 2))])))
(def EVQWGD001-main-cutout
  (->>
   (cube  EVQWGD001-width EVQWGD001-length EVQWGD001-mount-height)
   (translate [0 0 (/ EVQWGD001-plastic-height 2)])))

(def EVQWGD001-holder-base
  (->> (let [corner (square EVQWGD001-mount-corner-radius EVQWGD001-mount-corner-radius)] (hull

                                                                                           (translate EVQWGD001-holder-base-top-left corner)
                                                                                           (translate EVQWGD001-holder-base-top-right corner)
                                                                                           (translate EVQWGD001-holder-base-bottom-left corner)
                                                                                           (translate EVQWGD001-holder-base-bottom-right corner)))
       (with-fn 36)))

(def EVQWGD001-holder-body
  (->> (hull
        (for [i [0 0.25 0.5 0.75 1]
              :let [y (* (Math/cos (* i 90)) EVQWGD001-mount-y-modifier) x  (+ -3 (* (Math/sin (* i 90)) 3))]]

          (extrude-linear {:height (+ (- (+ EVQWGD001-plastic-height plate-thickness) 0) y) :center false}
                          (offset-delta {:delta x  :chamfer false :r false} EVQWGD001-holder-base))))
       (translate [0 0 (- (/ (+ EVQWGD001-mount-height EVQWGD001-mount-y-modifier) 2))])))

(def EVQWGD001-cutout-for-pins
  (->>
   (cube EVQWGD001-gap-for-pins-width EVQWGD001-length EVQWGD001-gap-for-pins-height)
   (translate [(- (/ EVQWGD001-width 2) (/ EVQWGD001-gap-for-pins-width 2)) 0 (- (/ EVQWGD001-gap-for-pins-height 2))])))

(def EVQWGD001-cutout-for-plastic
  (->>
   (cube EVQWGD001-gap-for-plastic EVQWGD001-length EVQWGD001-gap-for-pins-height)
   (translate [(- (/ EVQWGD001-gap-for-plastic 2) (/ EVQWGD001-width 2)) 0 (- (/ EVQWGD001-gap-for-pins-height 2))])))

(def EVQWGD001 (translate [(- (/ EVQWGD001-width 2)) (/ EVQWGD001-length 2) (- EVQWGD001-plastic-height)] (rdz -90 (import "Encoder.stl"))))

(def EVQWGD001-holder
  (union

   (difference
    (union
    ;EVQWGD001-test-base

     EVQWGD001-holder-body)
    EVQWGD001-main-cutout
    (translate [0 0 0.2] EVQWGD001-main-cutout)

    EVQWGD001-cutout-for-pins
    EVQWGD001-cutout-for-plastic)))


