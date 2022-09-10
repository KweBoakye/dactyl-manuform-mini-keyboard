(ns dactyl-keyboard.low.placement-functions-low
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.sa-keycaps :refer :all]
            [dactyl-keyboard.utils :refer :all]
  ))



;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range 0 ncols))
(def rows (range 0 nrows))

(def innercolumn 0)
(def innerrows (range 0 (- nrows 2)))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(defn row-radius [column] (+ (/ (/ (+ mount-height extra-height) 2)
                      (Math/sin (/ (α column) 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))

 (defn offset-for-column [col]
   (if (and (true? pinky-15u) (= col lastcol)) 5.5 0))
;; (defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn rotate-z-fn column row shape]
;;   (let [column-angle (* β (- centercol column))
;;         placed-shape (->> shape
;;                           (translate-fn [(offset-for-column column) 0 (- (row-radius column))])
;;                           (rotate-x-fn  (* (α column) (- (centerrow column) row)))
;;                           ;(rotate-z-fn  (γ column))
;;                           (translate-fn [0 0 (row-radius column)])
;;                           (translate-fn [0 0 (- column-radius)])
;;                           (rotate-y-fn  column-angle)
;;                           (translate-fn [0 0 column-radius])
;;                           (translate-fn (column-offset column)) 
                          
;;         )
;;         column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
;;         placed-shape-ortho (->> shape
;;                                 (translate-fn [0 0 (- (row-radius column))])
;;                                 (rotate-x-fn  (* (α column) (- (centerrow column) row)))
;;                                 (translate-fn [0 0 (row-radius column)])
;;                                 (rotate-y-fn  column-angle)
;;                                 (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
;;                                 (translate-fn (column-offset column)))
;;         placed-shape-fixed (->> shape
;;                                 (rotate-y-fn  (nth fixed-angles column))
;;                                 (translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
;;                                 (translate-fn [0 0 (- (+ (row-radius column) (nth fixed-z column)))])
;;                                 (rotate-x-fn  (* (α column) (- (centerrow column) row)))
;;                                 (translate-fn [0 0 (+ (row-radius column) (nth fixed-z column))])
;;                                 (rotate-y-fn  fixed-tenting)
;;                                 (translate-fn [0 (second (column-offset column)) 0]))]
;;     (->> (case column-style
;;            :orthographic placed-shape-ortho
;;            :fixed        placed-shape-fixed
;;            placed-shape)
;;          (rotate-y-fn  tenting-angle)
         
;;          (translate-fn [0 0 keyboard-z-offset]))))
(defn apply-key-geometry-rotation-values [column row]
  (let [column-angle (* β (- centercol column))
        x (* (α column) (- (centerrow column) row)) 
        y column-angle 
        z (cond (not= row lastrow) (γ column) :else 0)
        ]   
   {:x x
    :y y
    :z z 
    })
  )

(defn apply-key-geometry-rotation ([ column row shape] (apply-key-geometry-rotation rx ry rz column row shape))
  ([rotate-x-fn rotate-y-fn rotate-z-fn column row shape]
   (let [column-angle (* β (- centercol column))] 
   (->> shape
       (rotate-x-fn  (* (α column) (- (centerrow column) row)))
  (rotate-z-fn  (cond (not= row lastrow) (γ column) :else 0))
  (rotate-y-fn  column-angle)
        )
     )
   )
  )

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn rotate-z-fn column row shape]
  (let [column-angle (* β (- centercol column))
        post-splay-translation-vector (cond (and (not= (γ column) 0) (not= row lastrow))
                                            (post-splay-translation column)
                                            :else [0 0 0]
                                            )
        placed-shape (->> shape
                          (translate-fn [(offset-for-column column) 0 (- (row-radius column))])
                          
                          (rotate-x-fn  (* (α column) (- (centerrow column) row)))
                           (rotate-z-fn  (cond (not= row lastrow) (γ column) :else 0))
                          (translate-fn [0 0 (row-radius column)])
                          (translate-fn [0 0 (- column-radius)])
                         
                          (rotate-y-fn  column-angle)
                          
                          (translate-fn [0 0 column-radius])
                          (translate-fn (column-offset column))
                          
                          )
        column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
        placed-shape-ortho (->> shape
                                (translate-fn [0 0 (- (row-radius column))])
                                (rotate-x-fn  (* (α column) (- (centerrow column) row)))
                                (translate-fn [0 0 (row-radius column)])
                                (rotate-y-fn  column-angle)
                                (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
                                (translate-fn (column-offset column)))
        placed-shape-fixed (->> shape
                                (rotate-y-fn  (nth fixed-angles column))
                                (translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
                                (translate-fn [0 0 (- (+ (row-radius column) (nth fixed-z column)))])
                                (rotate-x-fn  (* (α column) (- (centerrow column) row)))
                                (translate-fn [0 0 (+ (row-radius column) (nth fixed-z column))])
                                (rotate-y-fn  fixed-tenting)
                                (translate-fn [0 (second (column-offset column)) 0]))]
    (->> (case column-style
           :orthographic placed-shape-ortho
           :fixed        placed-shape-fixed
           placed-shape)
         (rotate-y-fn  tenting-angle)
         (translate-fn post-splay-translation-vector)
         (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place ([column row shape] 
                 (key-place
                  column row
                  translate  (fn [angle obj] (rotate angle [1 0 0] obj))
(fn [angle obj] (rotate angle [0 1 0] obj))
(fn [angle obj] (rotate angle [0 0 1] obj)) shape)
                 )
  ([column row translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape] 
   (apply-key-geometry translate-fn 
                       rotate-x-fn
                       rotate-y-fn
                       rotate-z-fn 
                       column row shape)))

;; (defn key-place-test [column row shape]
;;   (apply-key-geometry-test translate
;;                       (fn [angle obj] (rotate angle [1 0 0] obj))
;;                       (fn [angle obj] (rotate angle [0 1 0] obj))
;;                       (fn [angle obj] (rotate angle [0 0 1] obj))
;;                       column row shape))


(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle)    (Math/cos angle)]]
   position))

(defn rotate-around-x-in-degrees [angle-in-degrees position](rotate-around-x (deg2rad angle-in-degrees) position))

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
    [0,                0,                  1]
   ]
   position))

(defn rotate-around-z-in-degrees [angle-in-degrees position] (rotate-around-z (deg2rad angle-in-degrees) position))
(defn rotation-transformations [position] [rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees position])
(defn rotate-position [function position] (apply function (rotation-transformations position)))
(defn affine-transformations [position] [(partial mapv +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees position])
(defn transform-position [function position]
  (apply function (affine-transformations position))
  )
(defn affine-transformations-radians [position] [(partial map +) rotate-around-x rotate-around-y rotate-around-z position])
(defn transform-position-radians [function position]
  (apply function (affine-transformations-radians position)))

(defn transform-position-partial [function]
 (fn [position] (apply function (affine-transformations position))))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y rotate-around-z column row position))

(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [2 3] column)
                         (not= row lastrow))]
           (->> single-plate
                (key-place column row)))))

(def caps
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [2 3] column)
                         (not= row lastrow))]
           (->> (sa-cap (if (and (true? pinky-15u) (= column lastcol)) 1.5 1))
                (key-place column row)))))

(def dsa-cap 
  (->>
   (import "DSA 1u.stl")
   (rdx 90)
   (translate [0 0 (+ 7 plate-thickness)])
   ))
(def dsa-caps
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [2 3] column)
                         (not= row lastrow))] 
                (key-place column row dsa-cap))))

(def caps-fill
  (apply union
         (for[column columns
              row rows
              :when (or (.contains[2 3] column)
                        (not= row lastrow))]
          (key-place column row keyhole-fill))
         )
  )