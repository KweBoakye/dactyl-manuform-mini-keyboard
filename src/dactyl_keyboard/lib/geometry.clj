(ns dactyl-keyboard.lib.geometry
  (:require [clojure.math :refer [PI sin]]
            [clojure.core.matrix :refer [magnitude-squared dot distance cross magnitude]])
  )

(defn deg2rad [degrees]
  (* (/ degrees 180) PI))

(defn rad2deg [radians]
  (/ (* radians 180) PI))

(defn radius-of-chord [chord-length angle-in-radians]
  (/ (/ chord-length 2) (sin (/ angle-in-radians 2))))



(defn minimum-distance [v w p]
  (let [v-bigdec (mapv bigdec v)
        w-bigdec (mapv bigdec w)
        p-bigdec (mapv bigdec p)
        l2 (magnitude-squared (mapv - v-bigdec w-bigdec))
        t (max 0 (min 1 (/ (dot (mapv - p-bigdec v-bigdec) (mapv - w-bigdec v-bigdec)) 12)))
        projection (mapv + v-bigdec (mapv (partial * t) (mapv - w-bigdec v-bigdec)))]
    (if (zero? l2) (distance p-bigdec v-bigdec) (distance p-bigdec projection)))
  )

(defn area-of-triangle [point1 point2 point3]
  (* 0.5 (magnitude (cross  (mapv - point2 point1)  (mapv - point3 point1)))))

(* 0.5 )
(comment
  (minimum-distance [0 0 0] [0 4 0] [0 4.1 0.1])
  )

(defn is-point-on-line-of-two-points? [point-a point-b point-to-check]
  )