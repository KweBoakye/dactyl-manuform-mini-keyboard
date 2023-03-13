(ns dactyl-keyboard.aviator-and-reset
   (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.case :refer :all]
            [dactyl-keyboard.lib.geometry :refer [deg2rad]]
            [dactyl-keyboard.placement-functions :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))

; code adapted from https://gist.github.com/jamiehs/de163e7d469e4fb4220e504b58613806
(def aviator-start (map + [-40 -12  20] (key-position 0 0 (map + (wall-locate3 0 1) [0 (/ mount-height  2) 0]))))
(def aviator-position [(first aviator-start) (second aviator-start) 12])
(def aviator-diameter 16.2)
(def aviator-male-connecter-ring-diameter 17.6)
(def aviator-female-connecter-ring-diameter 21.5)
(defn aviator-place [position diameter]
  (translate position
             (rotate (deg2rad 90) [1 0 0]
                   ;(rotate (deg2rad 45) [0 1 0]
                     (translate [3 0 0]
                                (cylinder (/  diameter 2) 20)))))
;(def aviator-hole (translate aviator-position
;                             (rotate (deg2rad 90) [1 0 0]
;                                     ;(rotate (deg2rad 45) [0 1 0]
;                                     (translate [3 0 0]
;                                                (cylinder (/  aviator-diameter 2) 20)))))

(def aviator-hole (aviator-place  aviator-position aviator-diameter))
(def aviator-male-connecter-clearence-test (translate [0 10 0] (aviator-place aviator-position aviator-male-connecter-ring-diameter)))
(def aviator-female-connecter-clearence-test (aviator-place aviator-position aviator-female-connecter-ring-diameter))

(def resetswitch-start (map + [0 -3  20] (key-position 0 0 (map + (wall-locate3 0 1) [0 (/ mount-height  2) 0]))))
(def resetswitch-position [(first resetswitch-start) (second resetswitch-start) 12])
(def resetswitch-diameter 8.8)

(def reset-hole
  (union
  ; (->> (union (binding [*fn* 36] (cylinder [(/ resetswitch-diameter 2) (/ resetswitch-diameter 2)] 10)))
  ;      (rotate (/ π 2) [1 0 0])
  ;      (translate [(+ 4  (first aviator-start)) (- (second aviator-start) 1) (/ (+ 44 aviator-diameter 0) 2)]))
  ;    ; thinner wall
   (->> (union (binding [*fn* 36] (cylinder [(/ resetswitch-diameter 2) (/ resetswitch-diameter 2)] 20))) ;; depth here matters; has been eyeballed
        (rotate (/ π 2) [1 0 0])
        (translate [(+ 32 (first resetswitch-start)) (- (second resetswitch-start) 2) (/ (+ 8 aviator-diameter 0) 2)]))))

(def resetswitch-hole (translate aviator-position
                                 (rotate (deg2rad 90) [1 0 0]
                                     ;(rotate (deg2rad 45) [0 1 0]
                                         (translate [4 0 0]
                                                    (cylinder (/  resetswitch-diameter 2) 20)))))