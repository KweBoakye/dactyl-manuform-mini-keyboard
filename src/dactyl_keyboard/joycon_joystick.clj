(ns dactyl-keyboard.joycon-joystick
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.lib.geometry :refer [deg2rad]]
            [dactyl-keyboard.thumbs :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.web-connecters :refer :all]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))

;;;;;;;;;;;;;;;;;;;;;
;; Joycon Joystick ;;
;;;;;;;;;;;;;;;;;;;;;

(def joycon-thumbstick-hole-diameter 16)
(def joycon-thumbstick-hole-radius (/ joycon-thumbstick-hole-diameter 2))
(def joycon-joystick-case-width 17)
(def joycon-joystick-case-length 19.1)
(def joycon-joystick-case-depth 5.3)
(def joycon-joystick-case-tolerance 0.2)

(def joycon-joystick-case-width-from-hole-centerline-to-left 9.25)
(def joycon-joystick-case-width-from-hole-centerline-to-right 7.75)
(def joycon-joystick-case-length-from-hole-centerline-to-top 9.5)
(def joycon-joystick-case-length-from-hole-centerline-to-bottom 9.6)
(def joycon-joystick-case-depth-top-to-screw-mount 2.25)
(def joycon-joystick-case-depth-bottom-to-screw-mount 1)

(def joycon-joystick-screw-mount-hole-diameter 1.6)
(def joycon-joystick-screw-mount-hole-radius (/ joycon-joystick-screw-mount-hole-diameter 2))
(def joycon-joystick-screw-mount-width 3.8)
(def joycon-joystick-screw-mount-depth 1.25)
(def joycon-joystick-top-right-screw-mount-length 3.3)
(def joycon-joystick-bottom-left-screw-mount-length 3.6)
(def joycon-joystick-distance-from-edge-of-case-to-screw-mount 0.3)
(def joycon-joystick-top-right-screw-mount-x-position (- (+ (/ joycon-joystick-case-width 2) (/ joycon-joystick-top-right-screw-mount-length 2))))
(def joycon-joystick-top-right-screw-mount-y-position (- (/ joycon-joystick-case-length 2) (/ joycon-joystick-screw-mount-width 2) joycon-joystick-distance-from-edge-of-case-to-screw-mount))
(def joycon-joystick-bottom-left-screw-mount-x-position (- (/ joycon-joystick-case-width 2) (/ joycon-joystick-screw-mount-width 2) joycon-joystick-distance-from-edge-of-case-to-screw-mount))
(def joycon-joystick-bottom-left-screw-mount-y-position (- (+ (/ joycon-joystick-case-length 2) (/ joycon-joystick-bottom-left-screw-mount-length 2))))

(def joycon-joystick-distance-from-top-edge-of-screw-mount-to-screw-hole 1.1)
(def joycon-joystick-distance-from-top-edge-of-screw-mount-to-screw-hole-centre (+ joycon-joystick-distance-from-top-edge-of-screw-mount-to-screw-hole joycon-joystick-screw-mount-hole-radius))

(def joycon-joystick-hole-postion-x-offset (/ (- joycon-joystick-case-width-from-hole-centerline-to-right joycon-joystick-case-width-from-hole-centerline-to-left) 2))
(def joycon-joystick-hole-postion-y-offset (/ (- joycon-joystick-case-length-from-hole-centerline-to-bottom joycon-joystick-case-length-from-hole-centerline-to-top) 2))

(def joycon-joystick-screw-mount-corner-rounding-mask
  (->>
   (difference
    (cube joycon-joystick-screw-mount-width joycon-joystick-distance-from-top-edge-of-screw-mount-to-screw-hole  joycon-joystick-case-depth-top-to-screw-mount)
   ;(translate [0 (- (+ (/ joycon-joystick-distance-from-top-edge-of-screw-mount-to-screw-hole 4) 0.05)) -0.1] (cube (+ joycon-joystick-screw-mount-width 0.2) (+ (/ joycon-joystick-distance-from-top-edge-of-screw-mount-to-screw-hole 2) 0.1) (+ joycon-joystick-case-depth-top-to-screw-mount 0.4)))
    (translate [0 0 -0.1] (binding [*fn* 36] (cylinder (/ joycon-joystick-distance-from-top-edge-of-screw-mount-to-screw-hole 2) (+ joycon-joystick-case-depth-top-to-screw-mount 0.3) :center true))))
   (rotate (deg2rad 180) [0 0 1])))
   ;(translate [joycon-joystick-bottom-left-screw-mount-x-position joycon-joystick-bottom-left-screw-mount-y-position joycon-joystick-screw-mount-depth])



(def joycon-joystick-top-right-screw-mount
  (->>
   (cube  joycon-joystick-top-right-screw-mount-length joycon-joystick-screw-mount-width joycon-joystick-case-depth-top-to-screw-mount)
   (translate [joycon-joystick-top-right-screw-mount-x-position joycon-joystick-top-right-screw-mount-y-position  joycon-joystick-screw-mount-depth])))


(def joycon-joystick-top-right-screw-mount-clearance
  (->> (cube   (+ joycon-joystick-top-right-screw-mount-length 0.4) (+ joycon-joystick-screw-mount-width 0.4) 12)
       (translate [joycon-joystick-top-right-screw-mount-x-position
                   joycon-joystick-top-right-screw-mount-y-position
                   (- (+ joycon-joystick-case-depth-top-to-screw-mount joycon-joystick-screw-mount-depth 1))])))



(def joycon-joystick-bottom-left-screw-mount
  (->>
   (cube joycon-joystick-screw-mount-width joycon-joystick-bottom-left-screw-mount-length joycon-joystick-case-depth-top-to-screw-mount)
   (translate [joycon-joystick-bottom-left-screw-mount-x-position joycon-joystick-bottom-left-screw-mount-y-position joycon-joystick-screw-mount-depth])))

(def joycon-joystick-bottom-left-screw-mount-clearance
  (->>
   (cube (+ joycon-joystick-screw-mount-width 0.4) (+ joycon-joystick-bottom-left-screw-mount-length 0.4) 8)
   (translate [joycon-joystick-bottom-left-screw-mount-x-position joycon-joystick-bottom-left-screw-mount-y-position (- (+ joycon-joystick-case-depth-top-to-screw-mount joycon-joystick-screw-mount-depth))])))

(def joycon-joystick-screw-mount-hole
  (binding [*fn* 36] (cylinder joycon-joystick-screw-mount-hole-radius 20)))


(def joycon-joystick-top-right-screw-mount-hole
  (translate [(- joycon-joystick-top-right-screw-mount-x-position (- (/ joycon-joystick-top-right-screw-mount-length 2) joycon-joystick-distance-from-top-edge-of-screw-mount-to-screw-hole-centre))
              joycon-joystick-top-right-screw-mount-y-position
              10]
             joycon-joystick-screw-mount-hole))

(def joycon-joystick-top-right-screw-mount-hole-position-test
  (->> (cube joycon-joystick-distance-from-top-edge-of-screw-mount-to-screw-hole joycon-joystick-distance-from-top-edge-of-screw-mount-to-screw-hole joycon-joystick-distance-from-top-edge-of-screw-mount-to-screw-hole)
       (translate [(+ (- joycon-joystick-top-right-screw-mount-x-position (/ joycon-joystick-top-right-screw-mount-length 2)) 0.55) joycon-joystick-top-right-screw-mount-y-position 0])))

(def joycon-joystick-bottom-left-screw-mount-hole
  (translate [joycon-joystick-bottom-left-screw-mount-x-position
              (- joycon-joystick-bottom-left-screw-mount-y-position (- (/ joycon-joystick-bottom-left-screw-mount-length 2) joycon-joystick-distance-from-top-edge-of-screw-mount-to-screw-hole-centre))
              10]
             joycon-joystick-screw-mount-hole))

(def joycon-joystick-bottom-left-screw-mount-hole-position-test
  (->>
   (cube joycon-joystick-distance-from-top-edge-of-screw-mount-to-screw-hole joycon-joystick-distance-from-top-edge-of-screw-mount-to-screw-hole joycon-joystick-distance-from-top-edge-of-screw-mount-to-screw-hole)
   (translate [joycon-joystick-bottom-left-screw-mount-x-position
               (+ (- joycon-joystick-bottom-left-screw-mount-y-position (/ joycon-joystick-bottom-left-screw-mount-length 2)) 0.55)
               0])))







(def joycon-joystick-hole
  (translate [joycon-joystick-hole-postion-x-offset joycon-joystick-hole-postion-y-offset 0] (binding [*fn* 36] (cylinder joycon-thumbstick-hole-radius 6))))

(def joycon-joystick-case
  (cube joycon-joystick-case-width joycon-joystick-case-length joycon-joystick-case-depth))

(def joycon-joystick-case-clearance
  (->>
   (cube (+ joycon-joystick-case-width joycon-joystick-case-tolerance) (+ joycon-joystick-case-length joycon-joystick-case-tolerance) (+ joycon-joystick-case-depth 20 joycon-joystick-case-tolerance))
   (translate [0 0 -10])))


(def joycon-joystick-test-cut
  (cube 90  joycon-joystick-case-length joycon-joystick-case-depth))



(def joycon-joystick-support-screw-mount-height (- (+ joycon-joystick-case-depth joycon-joystick-case-depth-bottom-to-screw-mount) 0.2))

(def joycon-joystick-support-top-right-screw-mount
  (->>
   (cube  joycon-joystick-top-right-screw-mount-length joycon-joystick-screw-mount-width joycon-joystick-support-screw-mount-height)
   (translate [joycon-joystick-top-right-screw-mount-x-position joycon-joystick-top-right-screw-mount-y-position  (/ joycon-joystick-support-screw-mount-height 2)])))

(def joycon-joystick-support-bottom-left-screw-mount
  (->>
   (cube joycon-joystick-screw-mount-width joycon-joystick-bottom-left-screw-mount-length joycon-joystick-support-screw-mount-height)
   (translate [joycon-joystick-bottom-left-screw-mount-x-position joycon-joystick-bottom-left-screw-mount-y-position (/ joycon-joystick-support-screw-mount-height 2)])))

(def joycon-joystick-support
  (difference
   (union
    (translate [0 0 (/ joycon-joystick-case-depth 2)] joycon-joystick-case)
    joycon-joystick-support-top-right-screw-mount
    joycon-joystick-support-bottom-left-screw-mount)
   (translate [0 0 4] joycon-joystick-top-right-screw-mount-hole)
   (translate [0 0 4] joycon-joystick-bottom-left-screw-mount-hole)))


;(->> (cube joycon-joystick-case-width joycon-joystick-case-length 2)
;     (translate [0 0 (+ (/ joycon-joystick-case-depth 2) 0.5)])
; )

(defn joycon-joystick-place [shape]
  (minithumb-tl-place
   (rotate (deg2rad -110) [0 0 4] (translate [0 2 0] shape))))

(def joycon-joystick-case-cover
  (difference  (hull
                (minithumb-tl-place web-post-tl)
                (minithumb-tl-place web-post-tr)
                (minithumb-tl-place web-post-bl)
                (minithumb-tl-place web-post-br))))