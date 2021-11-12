(ns dactyl-keyboard.hotswap
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.shape-parameters :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.placement-functions :refer :all]
            [dactyl-keyboard.thumbs :refer :all]
            [dactyl-keyboard.hand :refer :all]
            [unicode-math.core :refer :all]))

;;;;;;;;;;;;;
;; Hotswap ;;
;;;;;;;;;;;;;

(def pin-cutout-height 0.7)
(def pin-offset 1.7)
(def socket-pin  (translate [0 (/ pin-cutout-height 2) 0] (union
                                                           (translate [0 0 2] (cube 0.8 pin-cutout-height 4))
                                                           (translate [0 0 (+ 4 2.5)] (cube 2.2 pin-cutout-height 5))
                                                           (translate [0 0 (+ 9 0.5)] (cube 1.7 pin-cutout-height 1)))))
;; Hotswap socket test
(def socket-distance 5.5)
(def socket-height 5.5)
(def socket-width (+ socket-distance 4))
(def hotswap-buckle-length 4)
(def grip-length 1)
(defn pins-place [socket-pin]
  (union
   (translate [(- (/ socket-distance 2)) 0 (- -4 (/ socket-height 2))] socket-pin)
   (translate [(/ socket-distance 2) 0 (- -4 (/ socket-height 2))] socket-pin)))
(def hotswap-socket-pins (pins-place socket-pin))
(def socket-join-height (if printed-hotswap? (- socket-height 3) 3))
(def hotswap-clamp
  (let [grip-width    2
        grip-height 3
        grip            (polygon [[0 0] [grip-width 0] [grip-width grip-length] [0 grip-length]])
        thickness 1
        width (+ socket-width 0.25) ; give some wiggle room
        length (+ hotswap-buckle-length 0.15) ; give some wiggle room
        grip-offset     (+ (/ width 2) thickness)
        socket-slot-height (- socket-height 1)
        flat-model (union
                    (translate [(/ width 2) (- length)] (square thickness length :center false))
                    (translate [(/ width -2) (- length)] (mirror [1 0] (square thickness length :center false)))
                    (translate [0 (+ (- (+ length (/ thickness 2))) (/ pin-offset 2))] (square (+ width (* 2 thickness)) (+ thickness pin-offset))))
        flat-grip-model (union
                         (translate [(- grip-offset) 0] grip)
                         (translate [grip-offset 0] (mirror [1 0] grip)))]
    (union
     (extrude-linear {:height socket-slot-height} flat-model)
     (translate [0 0 (/ (- grip-height socket-slot-height) 2)] (extrude-linear {:height grip-height} flat-grip-model))
                           ; Bottom part of the holder
     (let [bottom-width (+ width thickness thickness)
           bottom-length (+ length thickness grip-length)]
       (difference
        (translate [0 (+ (/ bottom-length -2) grip-length) (- (/ socket-slot-height -2) (/ thickness 2))] (cube bottom-width bottom-length thickness))
        (translate [0 (- (- length pin-offset)) 0] hotswap-socket-pins))))))
(def hotswap-socket (difference
                     (translate [0 (/ (- hotswap-buckle-length pin-offset) 2) 0] (cube socket-width (- hotswap-buckle-length pin-offset) socket-height))
                     hotswap-socket-pins))
;                     (translate [insert-path-x-offset 0.7 0] insert-path)
;                     (translate [(- insert-path-x-offset) 0.7 0] insert-path)
;                     (translate [0 -1.5 0] (pins-place socket-pin-square))


(defn official-hotswap [width length height wings?] (translate [0 0 0] (difference
                                                                        (union
                                                                         (translate [0 -0.4 0] (cube width length height))
                                                                         (translate [(* 0.866 socket-distance) (* -0.5 socket-distance) 0] (cube width length height))
                                                                         (if wings?
                                                                           (union
                                                                            (translate [(/ width -2) -0.4 0] (cube width 2.5 height))
                                                                            (translate [(+ (* 0.866 socket-distance) (/ width 2)) (* -0.5 socket-distance) 0] (cube width 2.5 height)))
                                                                           nil)))))
(def official-hotswap-clamp (translate [0 -2.5 0] (difference
                                                   (official-hotswap 6.25 6.25 5.5 false)
                                                   (translate [0 0 2.5] (official-hotswap 5.25 5.25 2 true))
                                                   ; The middle piece
                                                   (->>
                                                    (cube 2 5 2)
                                                    (translate [(+ (/ (* 0.866 socket-distance) 2) 0.5) (+ (/ (* 0.5 socket-distance) -1) 2) 2.5])
                                                    (rotate (deg2rad -30) [0 0 1])))))


(def plate-mount-buckle-width (- keyswitch-width 4))
(defn position-socket-clamp [shape] (->>
                                     shape
                                     (translate [0 hotswap-buckle-length 0])
                                     (rotate (deg2rad -30) [0 0 1])
                                     (translate [-3 0.5 (/ socket-height 2)])))
(def distance-from-socket 1.6)
(defn position-official-socket-clamp [shape] (->>
                                              shape
                                              (translate [0 hotswap-buckle-length 0])
                                              (translate [-5 (+ distance-from-socket 0.8) (/ socket-height 2)])))

(def rotated-socket-clamp
  (->>
   hotswap-clamp
   position-socket-clamp))

(def clamp-buckle-y-offset (+ -1 (- distance-from-socket)))
(def plate-mount-buckle-height 2)
(def clamp-buckle (->>
                   (buckle
                    :include-middle      false
                    :triangle-length     1.75
                    :triangle-width      3.4
                    :buckle-width-adjust 0
                    :buckle-width        plate-mount-buckle-width
                    :buckle-thickness    1.8
                    :buckle-length       (+ socket-height plate-thickness -0.1) ; Remove some length to make less wiggle room
                    :buckle-end-length   0
                    :buckle-height       (+ plate-mount-buckle-height 0.35)) ; Add more thickness than the holes to account for wanting no wiggle room
                   (rotate (deg2rad 90) [1 0 0])
                   (translate [0  clamp-buckle-y-offset (+ socket-height plate-thickness)])))
(def hotswap-clamp-key-mount
  (union
   rotated-socket-clamp
   clamp-buckle
   ; Connect the left buckle to the socket
   (hull
    (translate [(- (/ plate-mount-buckle-width -2) -3.5) -1.2 (/ socket-join-height 2)]
               (rotate (deg2rad -30) [0 0 1] (cube 7 0.1 socket-join-height)))
    (translate [(- (/ plate-mount-buckle-width -2) 0) clamp-buckle-y-offset (/ socket-join-height 2)]
               (cube 2 plate-mount-buckle-height socket-join-height)))
   ; Connect the right buckle to the socket
   (hull
    (->> (cube 0.1 hotswap-buckle-length socket-join-height)
         (translate
          [(/ (+ socket-width 0.6) 2)
           (/ (+ hotswap-buckle-length 0.5) -2)
           (+ (/ socket-join-height -2))])
         position-socket-clamp)
    (translate [(- (/ plate-mount-buckle-width 2) 1) -1.5 (/ socket-join-height 2)]
               (cube 1 1 socket-join-height))
    (translate [(+ (/ plate-mount-buckle-width 2) 0.5) clamp-buckle-y-offset (/ socket-join-height 2)]
               (cube 1 plate-mount-buckle-height socket-join-height)))))

(def official-hotswap-clamp-key-mount (union
                                       (position-official-socket-clamp official-hotswap-clamp)
                                       clamp-buckle
                                       ; Connect the buckles together with a cube
                                       (difference
                                        (translate [(- (/ plate-mount-buckle-width -2) 1.8) (- clamp-buckle-y-offset (/ (+ plate-mount-buckle-height 0.35) 2)) 0]
                                                   (cube (+ 1.8 plate-mount-buckle-width) (+ (- clamp-buckle-y-offset) (/ (+ plate-mount-buckle-height 0.35) 2) 2) socket-join-height :center false))
                                        (position-official-socket-clamp (translate [0 -2.5 2.5] (official-hotswap 6 6 4 true))))))
(def buckle-hole-y-translate (+ (/ keyswitch-height 2) plate-mount-buckle-height distance-from-socket))
(def buckle-holes-on-key (->>
                          (buckle-holes
                           :buckle-thickness 1.8
                           :buckle-width plate-mount-buckle-width
                           :buckle-width-adjust 0
                           :buckle-length (+ socket-height plate-thickness)
                           :triangle-width 3.4
                           :triangle-length 1.75
                           :buckle-height plate-mount-buckle-height)
                          (rotate (deg2rad 90) [1 0 0])
                          (translate [0 buckle-hole-y-translate (+ (- socket-height))])))
(def single-plate-with-hotswap (difference
                                (translate [0 2 (/ plate-thickness 2)] (cube (+ keyswitch-width 4) (+ keyswitch-height 7) 3))
                                (translate [0 0 (/ plate-thickness 2)] (cube keyswitch-width keyswitch-height 3))
                                buckle-holes-on-key))

(defn hotswap-place [hotswap] (let [bottom-hotswap (rotate (deg2rad 180) [0 0 1] hotswap)] (union
                                        ; Bottom mounts
                                                                                            (apply union
                                                                                                   (for [column columns
                                                                                                         row [0 1]
                                                                                                         :when (or (.contains [2 3] column)
                                                                                                                   (not= row lastrow))]
                                                                                                     (->> bottom-hotswap
                                                                                                          (key-place column row))))
                                                                                            (apply union
                                                                                                   (for [column columns
                                                                                                         row [2 3]
                                                                                                         :when (or (.contains [2 3] column)
                                                                                                                   (not= row lastrow))]
                                                                                                     (->> hotswap
                                                                                                          (key-place column row))))
                                                                                            (minithumb-mr-place (if trackball-enabled bottom-hotswap hotswap))
                                                                                            (minithumb-br-place hotswap)
                                                                                            (if trackball-enabled nil (minithumb-tl-place bottom-hotswap))
                                                                                            (minithumb-bl-place bottom-hotswap)
                                                                                            (minithumb-tr-place bottom-hotswap))))

(def hotswap-holes (hotswap-place buckle-holes-on-key))

(def unified-pin-hotswap-mount (translate
                                [0 (- buckle-hole-y-translate distance-from-socket plate-mount-buckle-height 0.25) (- socket-height)]
                                (rotate (deg2rad 180) [0 0 1]
                                        (if printed-hotswap? (union
                                                              hotswap-clamp-key-mount
                                                              (->>
                                                               (union
                                                                hotswap-socket-pins
                                                                hotswap-socket)
                                                               (translate [0 (- (- hotswap-buckle-length pin-offset)) 0])
                                                               position-socket-clamp)) official-hotswap-clamp))))

(def hotswap-tester (hotswap-place unified-pin-hotswap-mount))

(def single-hotswap-clearance
  (->>
   (cube (+ socket-width 4) (+ hotswap-buckle-length 4) (+ socket-height 3))
   (translate [0 (+ distance-from-socket) -1.5])
   (translate [0 (- hotswap-buckle-length) 0])
   position-socket-clamp
   (rotate (deg2rad 180) [0 0 1])
   (translate
    [0 (- buckle-hole-y-translate distance-from-socket plate-mount-buckle-height 0.25) (- socket-height)])))

(def hotswap-clearance (hotswap-place single-hotswap-clearance))
