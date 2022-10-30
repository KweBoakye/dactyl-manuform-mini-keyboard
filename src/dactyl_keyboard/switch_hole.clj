(ns dactyl-keyboard.switch-hole
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [unicode-math.core :refer :all]))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def create-side-nubs? false)

(def keyswitch-height 14.15) ;; Was 14.1, then 14.25
(def keyswitch-width 14.15)

(def sa-profile-key-height 12.7)

(def plate-thickness 4)
(def side-nub-thickness 4)
(def retention-tab-thickness 1.5)
(def retention-tab-hole-thickness  (- (+ plate-thickness 0.5) retention-tab-thickness))
(def mount-width (+ keyswitch-width 3.2))
(def mount-height (+ keyswitch-height 2.7))
(def side-nub  (->> (binding [*fn* 30] (cylinder 1 2.75))
                    (rotate (/ π 2) [1 0 0])
                    (translate [(+ (/ keyswitch-width 2)) 0 1])
                    (hull (->> (cube 1.5 2.75 side-nub-thickness)
                               (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                           0
                                           (/ side-nub-thickness 2)])))
                    (translate [0 0 (- plate-thickness side-nub-thickness)])))
(def top-nub (->> (cube 5 5 retention-tab-hole-thickness)
                  (translate [(+ (/ keyswitch-width 2.5)) 0 (- (/ retention-tab-hole-thickness 2) 0.5)])))

(def single-plate
  (let [top-wall (->> (cube (+ keyswitch-width 3) 1.5 (+ plate-thickness 0.5))
                      (translate [0
                                  (+ (/ 1.5 2) (/ keyswitch-height 2))
                                  (- (/ plate-thickness 2) 0.25)]))
        left-wall (->> (cube 1.8 (+ keyswitch-height 3) (+ plate-thickness 0.5))
                       (translate [(+ (/ 1.8 2) (/ keyswitch-width 2))
                                   0
                                   (- (/ plate-thickness 2) 0.25)]))
     ;;    side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
     ;;                  (rotate (/ π 2) [1 0 0])
     ;;                  (translate [(+ (/ keyswitch-width 2)) 0 1])
     ;;                  (hull (->> (cube 1.5 2.75 side-nub-thickness)
     ;;                             (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
     ;;                                         0
     ;;                                         (/ side-nub-thickness 2)])))
     ;;                  (translate [0 0 (- plate-thickness side-nub-thickness)]))
        plate-half (union top-wall left-wall (if create-side-nubs? (with-fn 100 side-nub)))
        
        top-nub-pair (union top-nub
                            (->> top-nub
                                 (mirror [1 0 0])
                                 (mirror [0 1 0])))]
    (difference
     (union plate-half
            (->> plate-half
                 (mirror [1 0 0])
                 (mirror [0 1 0])))
     (->>
      top-nub-pair
      (rotate (/ π 2) [0 0 1])))))