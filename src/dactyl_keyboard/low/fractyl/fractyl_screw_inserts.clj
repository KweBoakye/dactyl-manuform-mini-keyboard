(ns dactyl-keyboard.low.fractyl.fractyl-screw-inserts
  (:refer-clojure :exclude [use import])
  (:require 
   [scad-clj.model :refer :all]
   [scad-clj.scad :refer :all]
   [dactyl-keyboard.low.shape-parameters-low :refer :all]
   [dactyl-keyboard.low.placement-functions-low :refer :all]
   [dactyl-keyboard.switch-hole :refer [mount-width]]
   [dactyl-keyboard.low.case-low-polyhedron-functions :refer :all]))

(defn fractyl-screw-insert-shape [bottom-radius top-radius height]
  (union
   (->> (binding [*fn* 30]
          (cylinder [bottom-radius top-radius] height)))
   (translate [0 0 (/ height 2)] (->> (binding [*fn* 30] (sphere top-radius))))))

;; (defn screw-insert [column row bottom-radius top-radius height offset]
;;   (let [shift-right   (= column lastcol)
;;         shift-left    (= column 0)
;;         shift-up      (and (not (or shift-right shift-left)) (= row 0))
;;         shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
;;         position      (if shift-up     (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
;;                           (if shift-down  (key-position column row (map - (wall-locate2  0 -1) [0 (/ mount-height 2) 0]))
;;                               (if shift-left (map + (left-key-position row 0) (wall-locate3 -1 0))
;;                                   (key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0])))))]
;;     (->> (fractyl-screw-insert-shape bottom-radius top-radius height)
;;          (translate (map + offset [(first position) (second position) (/ height 2)])))))
; Hole Diameter C: 4.1-4.4

(defn fractyl-screw-insert [wall-position bottom-radius top-radius height &{:keys [offset] :or {offset [0 0 0]}}]
  (let [wall-inner-floor-position (:wall-locate-2-bottom-floor (calculate-control-points wall-position))]
    (->> (fractyl-screw-insert-shape bottom-radius top-radius height)
       (translate (mapv + offset wall-inner-floor-position [0 0 (/ height 2)]))))
  )
(def fractyl-screw-insert-bottom-radius (/ 4.4 2))
(def fractyl-screw-insert-top-radius (/ 4.4 2))
(def fractyl-screw-insert-height 4)

(defn fractyl-screw-insert-all-shapes [bottom-radius top-radius height]
  (let [left-section-top-left   (fractyl-screw-insert (tps-65-wall-position :tl :north-west) bottom-radius top-radius height :offset [(/ bottom-radius 1.5) (/ bottom-radius -1) 0])
        ;left-section-bottom (screw-insert 0 lastrow   bottom-radius top-radius height [-50 7 0]) 
        left-section-top-right  (fractyl-screw-insert (tps-65-wall-position :tr :north) bottom-radius top-radius height :offset [(* 3 (- bottom-radius)) (/ bottom-radius -3)  0])
        left-section-top-mid  (fractyl-screw-insert (tps-65-wall-position :tm :north) bottom-radius top-radius height )
        left-section-left-mid  (fractyl-screw-insert (tps-65-wall-position :tl-lm :west) bottom-radius top-radius height)
        left-section-bm (fractyl-screw-insert (tps-65-wall-position :bm :south) bottom-radius top-radius height :offset [0 (/ bottom-radius 4) 0])
        ;thumb-bottom-left  (fractyl-screw-insert (tps-65-wall-position :tl :north-east) bottom-radius top-radius height)
        ;thumb-bottom-right  (fractyl-screw-insert (tps-65-wall-position :tl :north-east) bottom-radius top-radius height)
        top-mid  (fractyl-screw-insert (key-wall-position 2 0 0 1 :tm ) bottom-radius top-radius height :offset [0 (/ bottom-radius 2) 0])
        top-right  (fractyl-screw-insert (key-wall-position lastcol 0 1 1 :tr) bottom-radius top-radius height
                                         :offset [(+ (/ bottom-radius -1.5) -1) (+ (/ bottom-radius -1.5) -1) 0])
        bottom-right  (fractyl-screw-insert (key-wall-position lastcol cornerrow 1 -1 :br :slant :no-slant) bottom-radius top-radius height :offset [(/ bottom-radius -2) (+ (/ bottom-radius 2) 0.5) 0]) ]
    (union
     left-section-bm
     left-section-top-left
     ;left-section-top-mid
     ;left-section-left-mid
     left-section-top-right
     ;thumb-bottom-left
     top-mid
     ;thumb-bottom-right
     top-right
     bottom-right)))

(def fractyl-screw-insert-holes  (fractyl-screw-insert-all-shapes fractyl-screw-insert-bottom-radius fractyl-screw-insert-top-radius fractyl-screw-insert-height))

; Wall Thickness W:\t1.65
(def fractyl-screw-insert-outers (fractyl-screw-insert-all-shapes (+ fractyl-screw-insert-bottom-radius 1.65) (+ fractyl-screw-insert-top-radius 1.65) (+ fractyl-screw-insert-height 1.5)))
(def fractyl-screw-insert-screw-holes  (fractyl-screw-insert-all-shapes 1.7 1.7 350))