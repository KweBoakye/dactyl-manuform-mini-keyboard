(ns dactyl-keyboard.lib.vectors
  (:require [clojure.math :refer [pow sqrt acos]]
            [clojure.core.matrix :refer [dot magnitude magnitude-squared mul]]))

(defn vector-magnitude [vector]
  (sqrt (reduce + (mapv #(pow % 2) vector))))

(defn add-third-dimension [point]
  (into [] (concat point [0])))

(defn vec-to-matrix [point]
  (into [] (concat point [1])))


(defn vec-drop-by-index [vector index]
  (concat (subvec vector 0 index)
          (subvec vector (inc index))))

(defn vec-if-not [collection]
  (if (vector? collection) collection (vec collection)))

(defn vector-distance [v1 v2]
  (sqrt (+ (pow (- (nth v1 0) (nth v2 0)) 2)
           (pow (- (nth v1 1) (nth v2 1)) 2)
           (pow (- (nth v1 2) (nth v2 2)) 2)))
  )

(defn calculate-point-between-points [point1 point2 movement-vector]
  (mapv + point2 movement-vector (mapv (fn [point] (/ point 2)) (mapv - point1 point2))))

(defn project-vector-a-onto-vector-b [vector-a vector-b]
  (mapv (partial * (/ (dot vector-a vector-b) (magnitude-squared vector-b) )) vector-b)
  )

(defn two-d-colinearity [vector-a vector-b]
  (- (* (nth vector-a 0) (nth vector-b 1)) (* (nth vector-a 1) (nth vector-b 0))))

(defn are-two-d-points-colinear [vector-a vector-b] 
  (two-d-colinearity vector-a vector-b) )

(defn x [vector]
  (nth vector 0))

(defn y [vector] 
  (nth vector 1))

(defn z [vector]
  (nth vector 2))

(defn angle-between-vectors [a b]
  (acos (/ (dot a b) (* (magnitude a) (magnitude b))))
  )

;; (defn two-d-intersection [v1-start v1-end v2-start v2-end]
;;   (let [x ])
;;   )

(defn three-d-intersection [v1-start v1-end v2-start v2-end]
  (let [t (/ (- (nth v1-start 1) (nth v2-start 1) (* (nth v2-end 1) (/ (- (nth v1-start 0) (nth v2-start 0)) (nth v2-end 0))))
             (- (* (nth v2-end 1) (/ (nth v1-end 0) (nth v2-end 0))) (nth v1-end 1)))
        u (/ (- (+ (nth v1-start 0) (* (nth v1-end 0) t)) (nth v2-start 0)) (nth v2-end 0))
        v1-point (mapv double (mapv + v1-start (mul t v1-end)))
        v2-point (mapv double (mapv + v2-start (mul u v2-end)))
        intersect (< (magnitude (mapv - v1-point v2-point)) 0.0002)]
    (if intersect v1-point nil)))


(comment (acos (/ 2 14)) )
(comment (magnitude [1 2 3]))
(comment (sqrt 14))
(comment (angle-between-vectors [1 2 3] [3 -2 1]))