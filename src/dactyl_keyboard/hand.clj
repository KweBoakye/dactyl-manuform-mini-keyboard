(ns dactyl-keyboard.hand
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            ))


;;;;;;;;;;
;; Hand ;;
;;;;;;;;;;


(defn finger [one two three finger-radius]
  (let
   [three-cyl-height (- three finger-radius)
    height-loss (* finger-radius (Math/sin 15))]
    (union
          ;; First joint to second joint
     (translate [0 0 (/ one 2)]
                (cylinder finger-radius one))
     (translate [0 0 one]
                (rotate (deg2rad 15) [1 0 0]
                        (union
                               ;; Second joint to third
                         (translate [0 0 (/ two 2)]
                                    (cylinder finger-radius two))
                               ;; Third to end
                         (translate [0 (* -1 (- three-cyl-height height-loss) (Math/cos (deg2rad 75))) (+ two (/ three-cyl-height 2))]
                                    (rotate (deg2rad 15) [1 0 0]
                                            (union
                                             (cylinder finger-radius three-cyl-height)
                                                    ;; Make the fingertip round
                                             (translate [0 0 (/ three-cyl-height 2)] (sphere finger-radius)))))))))))

(def fingers
  ;; Move over by half the width of index finger to half index finger at 0 on x
  (translate [10.5 0 0]
             (union
               ;; Index
              (finger 55 27.3 25.5 8.5)
               ;; Middle
              (translate [25.5 0 0] (finger 53.5 29 22 9.5))
               ;; Ring
              (translate [(+ 20 25.5) 0 0] (finger 51.8 33.2 24.9 10.2))
               ;; Pinky
              (translate [(+ 20 25.5 22) 0 0] (finger 38.5 27.9 26.6 7.5)))))

(def palm
  (translate [42.5 0 -40] (union
                           (cube 85 30 80)
                           (rotate (deg2rad 35) [1 0 0]
                                   (translate [(+ 7 (/ -85 2)) -25 25]
                                              (cylinder 10.5 100))))))

(def hand
  (union
   fingers
   (rotate (deg2rad -45) [1 0 0] palm)))

(defn buckle [& {:keys [triangle-length triangle-width  buckle-width buckle-thickness buckle-length buckle-end-length buckle-height include-middle ]}]
  (let
   [buckle-end-width (- buckle-width (* 2 buckle-thickness))
    palm-buckle-triangle (polygon [[0 triangle-length] [triangle-width 0] [0 0]])
    palm-buckle-side (translate [0 (- (+ buckle-length buckle-end-length))]
                                (square buckle-thickness (+ buckle-length buckle-end-length) :center false))
    palm-buckle-2d (union
                     ; Triangles
                    (translate [(/ buckle-width 2) 0 0] palm-buckle-triangle)
                    (translate [(- (/ buckle-width 2)) 0 0]
                               (mirror [1 0] palm-buckle-triangle))
                     ; Sticks on the triangles
                    (translate [(/ buckle-width 2) 0 0] palm-buckle-side)
                    (translate [(- (/ buckle-width 2)) 0 0]
                               (mirror [1 0] palm-buckle-side))
                    (if include-middle
                      (union
                        ; Square in the middle
                       (translate [0 (- (+ buckle-length (/ buckle-end-length 2)))]
                                  (square buckle-end-width buckle-end-length))
                        ; Bar at the end
                       (translate [0 (- (+ buckle-length buckle-end-length (/ buckle-thickness 2)))]
                                  (square (+ buckle-width (* 2 buckle-thickness)) buckle-thickness)))
                      nil))]
    (extrude-linear {:height buckle-height} palm-buckle-2d)))

(defn buckle-holes [& {:keys [buckle-thickness buckle-length buckle-width buckle-width-adjust triangle-length triangle-width buckle-height]}]
  (let [hole-x-translate (- (/ (+ buckle-width buckle-width-adjust) 2) (- triangle-width buckle-thickness) 0.2)]
    (union
     (translate [hole-x-translate 0 0]
                (cube (+ triangle-width 0.5) 10 (+ buckle-height 0.5) :center false))
     (translate [(+ hole-x-translate (- triangle-width buckle-thickness)) buckle-length 0] ; clear out some space on the other end of the buckle
                (cube (+ triangle-width 0.25) 2 (+ buckle-height 0.5) :center false))
     (translate [(- hole-x-translate) 0 0]
                (mirror [1 0] (cube (+ triangle-width 0.5) 10 (+ buckle-height 0.5) :center false)))
     (translate [(- (- hole-x-translate) (- triangle-width buckle-thickness)) buckle-length 0] ;clear out some space on the other end of the buckle
                (mirror [1 0] (cube (+ triangle-width 0.25) 2 (+ buckle-height 0.5) :center false))))))