(ns dactyl-keyboard.lib.trigonometry
  (:require [clojure.core.matrix :refer [magnitude magnitude-squared]]
            
            [clojure.math :refer [PI sqrt]])
  )

(defn deg2rad [degrees]
  (* (/ degrees 180) PI))

(defn rad2deg [radians]
  (/ (* radians 180) PI))

(defn direction-cosines [end-point start-point]
  (let [tangent-vector (mapv - start-point end-point)
        vector-magnitude (magnitude tangent-vector)] 
    (mapv #(/ % vector-magnitude) (reverse tangent-vector)))
  )

(comment (direction-cosines [1 2 3] [5 5 5]))

(comment (* 0.3713906763541037 (sqrt 29)))