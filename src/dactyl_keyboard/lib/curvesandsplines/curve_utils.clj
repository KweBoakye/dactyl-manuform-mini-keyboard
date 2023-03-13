(ns dactyl-keyboard.lib.curvesandsplines.curve-utils
  (:require 
   [clojure.core.matrix :refer [mmul mul mget]])
  )

(defn lerp [a b t]
  (+ a (*  (- b a) t)))

(defn point-column-matrix-to-scalar [matrix dimension]
  (for [point matrix]
    (mapv (fn [point] (nth point dimension)) point)))

(defn mmul-scalar-to-vector [scalar-matrix vector-matrix]
  (let [vector-matrix-size (count vector-matrix)
        x-matrix (point-column-matrix-to-scalar vector-matrix 0)
        y-matrix (point-column-matrix-to-scalar vector-matrix 1)
        z-matrix (point-column-matrix-to-scalar vector-matrix 2)
        scalar-matrix-dot-x-matrix (mmul scalar-matrix x-matrix)
        scalar-matrix-dot-y-matrix (mmul scalar-matrix y-matrix)
        scalar-matrix-dot-z-matrix (mmul scalar-matrix z-matrix)]
    (into [] (for [index (range 0 vector-matrix-size)]
               (into [] (concat (nth scalar-matrix-dot-x-matrix index) (nth scalar-matrix-dot-y-matrix index) (nth scalar-matrix-dot-z-matrix index)))))))

(defn get-drop-last-point-if-not-last-segment [number-of-segments drop-last-point-of-segment]
  (fn [i  segment-points] 
    (if (and (not= i number-of-segments) drop-last-point-of-segment)
      (drop-last segment-points)
      segment-points)
    ))

(defn project-coordinate [coordinate]
  (mul (/ 1 (mget coordinate 3)) coordinate))

(defn homogenize-cooridinates [control-points weights]
  (mapv #(conj (mapv (partial * %2) %1) %2) control-points weights))