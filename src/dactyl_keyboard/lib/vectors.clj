(ns dactyl-keyboard.lib.vectors
  (:require [clojure.math :refer [pow sqrt acos]]
            [clojure.core.matrix :refer [dot magnitude magnitude-squared]]))

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

(defn x [vector]
  (nth vector 0))

(defn y [vector] 
  (nth vector 1))

(defn z [vector]
  (nth vector 2))

(defn angle-between-vectors [a b]
  (acos (/ (dot a b) (* (magnitude a) (magnitude b))))
  )

(comment (acos (/ 2 14)) )
(comment (magnitude [1 2 3]))
(comment (sqrt 14))
(comment (angle-between-vectors [1 2 3] [3 -2 1]))