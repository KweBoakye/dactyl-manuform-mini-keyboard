(ns dactyl-keyboard.low.placement-functions-low
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.sa-keycaps :refer :all]
           ; [dactyl-keyboard.utils :refer ]
            [dactyl-keyboard.lib.transformations :refer [rx ry rz rdx rdy rdz]]
            [dactyl-keyboard.lib.affine-transformations :refer [rotate-around-x rotate-around-y rotate-around-z
                                                                rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees]]
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
(defn column-radius [column] (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ (β column) 2)))
                      cap-top-height))
(defn column-x-delta [column] (+ -1 (- (* (column-radius column) (Math/sin (β column))))))

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
  (let [column-angle (* (β column) (- centercol column))
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
   (let [column-angle (* (β column) (- centercol column))] 
   (->> shape
       (rotate-x-fn  (* (α column) (- (centerrow column) row)))
  (rotate-z-fn  (cond (not= row lastrow) (γ column) :else 0))
  (rotate-y-fn  column-angle)
        )
     )
   )
  )

(defn should-splay-key [column row]
  (if (and (= row lastrow) last-row-middle-and-fourth-keys-only (false? splay-last-row)) 0
        (γ column)
        )
  )

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn rotate-z-fn column row shape]
  (let [column-angle (* (β column) (- centercol column))
        post-splay-translation-vector (cond (and (not= (γ column) 0) (not= row lastrow))
                                            (post-splay-translation column)
                                            :else [0 0 0]
                                            )
        placed-shape (->> shape
                          (translate-fn [(offset-for-column column) 0 (- (row-radius column))])
                          
                          (rotate-x-fn  (* (α column) (- (centerrow column) row)))
                           (rotate-z-fn  (should-splay-key column row))
                          (translate-fn [0 0 (row-radius column)])
                          (translate-fn [0 0 (- (column-radius column))])
                         
                          (rotate-y-fn  column-angle)
                          
                          (translate-fn [0 0 (column-radius column)])
                          (translate-fn (column-offset column))
                          
                          )
        column-z-delta (* (column-radius column) (- 1 (Math/cos column-angle)))
        placed-shape-ortho (->> shape
                                (translate-fn [0 0 (- (row-radius column))])
                                (rotate-x-fn  (* (α column) (- (centerrow column) row)))
                                (translate-fn [0 0 (row-radius column)])
                                (rotate-y-fn  column-angle)
                                (translate-fn [(- (* (- column centercol) (column-x-delta column))) 0 column-z-delta])
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
                 (key-place column row
                  translate  (fn [angle obj] (rotate angle [1 0 0] obj))
(fn [angle obj] (rotate angle [0 1 0] obj))
(fn [angle obj] (rotate angle [0 0 1] obj))  shape)
                 )
  ([column row translate-fn rotate-x-fn rotate-y-fn rotate-z-fn  shape] 
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




(defn rotation-transformations [position] [rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees position])
(defn rotate-position [function position] (apply function (rotation-transformations position)))
(defn affine-transformations [position] [(partial mapv +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees position])
(defn transform-position [function position]
  (vec (apply function (affine-transformations position)))
  )
(defn affine-transformations-radians [position] [(partial map +) rotate-around-x rotate-around-y rotate-around-z position])
(defn transform-position-radians [function position]
  (vec (apply function (affine-transformations-radians position))))

(defn transform-position-partial [function]
 (fn [position] (apply function (affine-transformations position))))

 (defn get-transform-fn [rad-or-deg1 fn-to-transform]
   (cond (= rad-or-deg1 :radians) (fn [position] (transform-position-radians fn-to-transform position))
       (= rad-or-deg1 :degrees) (fn [position] (transform-position fn-to-transform position))))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y rotate-around-z column row position))

(defn check-last-row-middle-and-fourth-keys-only [column row]
  (case last-row-style 
    :no-last-row (not= row lastrow)   
    :last-row-middle-and-fourth-keys-only (or (.contains [2 3] column)
                                      (not= row lastrow))
    :all-columns   
     true )
  )

(defn place-per-key ([shape] (place-per-key translate rx ry rz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
  (apply union
         (for [column columns
               row rows
               :when (check-last-row-middle-and-fourth-keys-only column row)]
           (->> shape
                (key-place column row translate-fn rotate-x-fn rotate-y-fn rotate-z-fn)))) 
   )
  )

(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (check-last-row-middle-and-fourth-keys-only column row)]
           (->> single-plate
                (key-place column row)))))



(def caps
  (apply union
         (for [column columns
               row rows
               :when (check-last-row-middle-and-fourth-keys-only column row)]
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
               :when (check-last-row-middle-and-fourth-keys-only column row)] 
                (key-place column row dsa-cap))))

(def switch-model
  (->>
   (import "../parts/Kailh Polia.stl")
   (rdz -90)
   (translate [0 0 plate-thickness])
   )
  )

(def switches (apply union
         (for[column columns
              row rows
              :when (check-last-row-middle-and-fourth-keys-only column row)]
          (key-place column row switch-model))
         ))

(def caps-fill
  (apply union
         (for[column columns
              row rows
              :when (check-last-row-middle-and-fourth-keys-only column row)]
          (key-place column row keyhole-fill))
         )
  )