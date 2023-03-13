(ns dactyl-keyboard.lib.openscad.hull
  (:refer-clojure :exclude [use import])
  (:require
   [scad-clj.scad :refer :all]
   [scad-clj.model :refer :all])
  )

(defn join-bezier-sides [points]
  (for [i (range 0 (- (count points) 3))]
    (hull
     (last points)
     (nth points i)
     (nth points (+ i 1)))))

(defn join-beziers [bezier-1 bezier-2]
  (let [bezier-larger (cond (>= (count bezier-1) (count bezier-2)) bezier-1 :else bezier-2)
        bezier-smaller (cond (identical? bezier-1 bezier-larger) bezier-2 :else bezier-1)
        penultimate-l (- (count bezier-larger) 2) penultimate-s (- (count bezier-smaller) 2)]
    (union
     (hull
      (first bezier-larger)
      (last bezier-larger)
      (first bezier-smaller)
      (last bezier-smaller))
     (for [i (range 0 penultimate-l)]
       (cond  (< penultimate-s i)

              (hull
               (first bezier-smaller)
               (nth bezier-larger (- penultimate-l i))
               (nth bezier-larger (+ (- penultimate-l i) 1)))

              :else (hull
                     (nth bezier-larger (- penultimate-l i))
                     (nth bezier-larger (+ (- penultimate-l i) 1))
                     (nth bezier-smaller (- penultimate-s i))
                     (nth bezier-smaller (+ (- penultimate-s i) 1)))))
     (join-bezier-sides bezier-larger)
     (join-bezier-sides bezier-smaller))))

(defn plot-list-of-shapes [list-of-shapes]
  (for [i (range 0 (- (count list-of-shapes) 2))]
    (concat (hull
             (nth list-of-shapes i)
             (nth list-of-shapes (+ i 1))))))

(defn plot-list-of-shapes-and-join-first-and-last [list-of-shapes]
  (concat (plot-list-of-shapes
           (hull (first list-of-shapes) (nth list-of-shapes (- (count list-of-shapes) 1))))))



(defn chained-hull [shapes]
  (for [index (range 0 (- (count shapes) 1))]
    (hull (nth shapes index) (nth shapes (+ index 1)))))

;; (defn chained-bottom-hull [shapes]

;;   (for [index (range 0 (- (count shapes) 1))]
;;     (bottom-hull (nth shapes index) (nth shapes (+ index 1)))))

(defn chained-hull-for-two-lists [shapes1 shapes2 steps]
  (for [index (range 0 (- steps 1))]
    (hull (nth shapes1 index) (nth shapes2 index) (nth shapes1 (+ index 1)) (nth shapes2 (+ index 1)))))

(defn chained-hull-for-three-lists [shapes1 shapes2 shapes3 steps]
  (for [index (range 0 (- steps 1))]
    (hull (nth shapes1 index) (nth shapes2 index) (nth shapes3 index) (nth shapes1 (+ index 1)) (nth shapes2 (+ index 1)) (nth shapes3 (+ index 1)))))
(defn chained-hull-for-four-lists [shapes1 shapes2 shapes3 shapes4 steps]
  (println "steps is " steps)
  (for [index (range 0 (- steps 1))]
    (hull (nth shapes1 index) (nth shapes2 index) (nth shapes3 index) (nth shapes4 index) (nth shapes1 (+ index 1)) (nth shapes2 (+ index 1)) (nth shapes3 (+ index 1)) (nth shapes4 (+ index 1)))))

(defn chained-hull-to-points [upper-list upper-point lower-list lower-point steps]
  (for [index (range 0 (dec steps))]
    (hull (nth upper-list index) upper-point (nth lower-list index) lower-point (nth upper-list (inc index)) (nth lower-list (inc index)))))
(defn chained-hull-with-function [function shapes]
  (for [index (range 0 (- (count shapes) 1))]
    (hull (function (nth shapes index)) (function (nth shapes (+ index 1))))))

;; (defn bottom-hull [& p]
;;   (hull p (bottom 0.001 p)))

;; (defn chained-bottom-hull-for-two-lists [shapes1 shapes2 steps]
;;   (for [index (range 0 (- steps 1))]
;;     (bottom-hull (nth shapes1 index) (nth shapes2 index) (nth shapes1 (+ index 1)) (nth shapes2 (+ index 1)))))

;; (defn chained-bottom-hull-translate-for-two-lists [shapes1 shapes2 steps]
;;   (for [index (range 0 (- steps 1))]
;;     (bottom-hull-translate (nth shapes1 index) (nth shapes2 index) (nth shapes1 (+ index 1)) (nth shapes2 (+ index 1)) )
;;     ))

(defn chained-hull-var [steps nested-shapes]

  (let [index-max (- steps 1)]
    (for [index (range 0  index-max)]
      (hull (for [i (range 0 index-max)]
              (union (nth (nth nested-shapes index) i) (nth (nth nested-shapes (+ index 1)) i)))))))