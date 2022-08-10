(ns dactyl-keyboard.screw-inserts
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.shape-parameters :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.placement-functions :refer :all]
            [dactyl-keyboard.case :refer :all]
            [unicode-math.core :refer :all]))

; Screw insert definition & position
(defn screw-insert-shape [bottom-radius top-radius height]
  (union
   (->> (binding [*fn* 30]
          (cylinder [bottom-radius top-radius] height)))))

(defn screw-insert [column row bottom-radius top-radius height offset]
  (let [shift-right   (= column lastcol)
        shift-left    (= column 0)
        shift-up      (and (not (or shift-right shift-left)) (= row 0))
        shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
        position      (if shift-up     (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
                          (if shift-down  (key-position column row (map - (wall-locate2  0 -2.5) [0 (/ mount-height 2) 0]))
                              (if shift-left (map + (left-key-position row 0) (wall-locate3 -1 0))
                                  (key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0])))))]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate (map + offset [(first position) (second position) (/ height 2)])))))

; Offsets for the screw inserts dependent on extra-row & pinky-15u
(when (and pinky-15u extra-row)
  (def screw-offset-tr [1 7 0])
  (def screw-offset-br [7 14 0]))
(when (and pinky-15u (false? extra-row))
  (def screw-offset-tr [1 7 0])
  (def screw-offset-br [6.5 15.5 0]))
(when (and (false? pinky-15u) extra-row)
  (def screw-offset-tr [-3.5 6.5 0])
  (def screw-offset-br [-3.5 -6.5 0]))
(when (and (false? pinky-15u) (false? extra-row))
  (def screw-offset-tr [-4 6.5 0])
  (def screw-offset-br [-6 13 0]))

; Offsets for the screw inserts dependent on thumb-style & inner-column
(when (and (= thumb-style "cf") inner-column)
  (def screw-offset-bl [9 4 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [13 -7 0]))
(when (and (= thumb-style "cf") (false? inner-column))
  (def screw-offset-bl [-7.7 2 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [13 -7 0]))
(when (and (= thumb-style "mini") inner-column)
  (def screw-offset-bl [14 8 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [-1 -7 0]))
(when (and (= thumb-style "mini") (false? inner-column))
  (def screw-offset-bl [-1 4.2 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [-1 -7 0]))
(when (and (= thumb-style "default") inner-column)
  (def screw-offset-bl [5 -6 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [8 -1 0]))
(when (and (= thumb-style "default") (false? inner-column))
  (def screw-offset-bl [-11.7 -8 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [8 -1 0]))

(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union (screw-insert 0 1         bottom-radius top-radius height [6 21.5 0])
         (screw-insert 0 cornerrow   bottom-radius top-radius height (map + [-7 0 0] screw-offset-bl))
         (screw-insert lastcol lastrow  bottom-radius top-radius height screw-offset-br)
         (screw-insert lastcol 0         bottom-radius top-radius height screw-offset-tr)
         (screw-insert (+ 2 innercol-offset) 0         bottom-radius top-radius height  (map + [-6 3 0] screw-offset-tm))
         (screw-insert (+ 1 innercol-offset) lastrow         bottom-radius top-radius height (map + [-5 -3 0] screw-offset-bm))))



; Hole Depth Y: 4.4
(def screw-insert-height 6)

; Hole Diameter C: 4.1-4.4
(def screw-insert-bottom-radius (/ 4.0 2))
(def screw-insert-top-radius (/ 3.9 2))
(def screw-insert-holes  (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))

; Wall Thickness W:\t1.65
(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1)))
(def screw-insert-screw-holes  (screw-insert-all-shapes 1.7 1.7 350))

(defn screw-insert-top-fn [bottom-radius top-radius height]
  (union
   (screw-insert 0 0 bottom-radius top-radius height [3 -5 93])
   (screw-insert 0 lastrow bottom-radius top-radius height [-3 19 90])
   (screw-insert lastcol lastrow bottom-radius top-radius height [-27 16 15])
   (screw-insert lastcol 0         bottom-radius top-radius height [-13 4 30])
   (screw-insert (+ 1 innercol-offset) lastrow bottom-radius top-radius height [10 6 52]))) ;thumb


(def screw-insert-top-obj (screw-insert-top-fn (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1.5)))
(def screw-insert-top-holes (screw-insert-top-fn 1.7 1.7 350))
(def screw-insert-top
  (difference screw-insert-top-obj
              screw-insert-top-holes))

(defn OLED [posx posy posz]
  (translate [posx posy posz]
             (rotate (/ π -6)  [0 1 0]
                     (rotate (/ π 2) [0 0 1]
                             (union
                              (cube 39.5 13.5 3)
                              (translate [1 0 2] (cube 26 12 4)) ;5 0 2
                              (translate [18 0 -4] (cube 3.5 10 8))))))) ;36 0 6
