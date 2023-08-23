(ns dactyl-keyboard.lib.affine-transformations
  
  (:require
   [clojure.core.matrix :refer [mmul]]
   [dactyl-keyboard.lib.geometry :refer [deg2rad]])
  )


(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle)    (Math/cos angle)]]
   position))

(defn rotate-around-x-in-degrees [angle-in-degrees position] (rotate-around-x (deg2rad angle-in-degrees) position))

(defn rotate-around-y [angle position]
  (mmul
   [[(Math/cos angle)     0 (Math/sin angle)]
    [0                    1 0]
    [(- (Math/sin angle)) 0 (Math/cos angle)]]
   position))

(defn rotate-around-y-in-degrees [angle-in-degrees position] (rotate-around-y (deg2rad angle-in-degrees) position))

(defn rotate-around-z [angle position]
  (mmul
   [[(Math/cos angle), (- (Math/sin angle)), 0]
    [(Math/sin angle),  (Math/cos angle),    0]
    [0,                0,                  1]]
   position))

(defn rotate-around-z-in-degrees [angle-in-degrees position] (rotate-around-z (deg2rad angle-in-degrees) position))

(defn rotate-around-point-z-axis [angle point position]
  (let [rotation-point (mapv - position point)]
    (->>
     (rotate-around-z angle rotation-point)
     (mapv + point)))
  )

(defn rotate-around-point-z-axis-in-degrees [angle-in-degrees point position]
  (rotate-around-point-z-axis (deg2rad angle-in-degrees) point position)
  )