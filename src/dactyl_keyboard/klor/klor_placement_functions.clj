(ns dactyl-keyboard.klor.klor-placement-functions
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.klor.klor-config :refer :all]
            [dactyl-keyboard.klor.klor-points :refer :all]
            [dactyl-keyboard.klor.klor-constants :refer :all]
            [dactyl-keyboard.lib.affine-transformations :refer [rotate-around-x-in-degrees
                                                                rotate-around-z-in-degrees]]
            [dactyl-keyboard.lib.transformations :refer [rdz]]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all])
  )

(defn klor-apply-key-geometry [translate-fn rotate-z-fn vector-rotate-fn column row shape]
  (->> shape 
       
       
       (rotate-z-fn anchor-rotation)
       (translate-fn   (vector-rotate-fn anchor-rotation (pre-translation column)))
       (translate-fn  (vector-rotate-fn anchor-rotation (column-offset column)))
       (translate-fn   (vector-rotate-fn anchor-rotation [0 (*  key-spacing-vertical row) 0])) 
       (translate-fn (vector-rotate-fn (+ anchor-rotation ) [(* key-spacing-horizontal column) 0 0]))
       (rotate-z-fn (column-rotation column))
       (translate-fn   (vector-rotate-fn (+ anchor-rotation (column-rotation column)) (mapv (partial * -1)(pre-translation column)))) 
       )
  )


(defn klor-key-place [column row shape]
  (klor-apply-key-geometry translate rdz rotate-around-z-in-degrees column row shape)
  )

(defn klor-key-position [column row vector]
  (klor-apply-key-geometry (partial mapv +) rotate-around-z-in-degrees rotate-around-z-in-degrees column row vector))


(defn klor-key-place-with-offset [column row offset shape]
  (translate (klor-key-position column row offset ) (rdz anchor-rotation shape)))
(defn klor-apply-key-geometry-thumb [translate-fn rotate-z-fn vector-rotate-fn column shape]
  (->> shape
       (klor-apply-key-geometry translate-fn rotate-z-fn vector-rotate-fn 0 0)
       
       (translate-fn (vector-rotate-fn anchor-rotation thumb-anchor))
       (translate-fn  (vector-rotate-fn anchor-rotation thumb-origin))
       (translate-fn (vector-rotate-fn anchor-rotation [-11 0 0]))
       
       (translate-fn (vector-rotate-fn (+ anchor-rotation ) [(* column thumb-spread) 0 0]))
       (rotate-z-fn (* column  thumb-splay))
       (translate-fn (case column 
                       0 [0 0 0]
                       1 (vector-rotate-fn (+ anchor-rotation thumb-splay )
                                         [-4.75 -2.5 0])
                       2 (vector-rotate-fn (+ anchor-rotation (* column thumb-splay))
                                           [-9 1.75 0])
                       3 (vector-rotate-fn (+ anchor-rotation (* column thumb-splay))
                                           [-11.25 12.5 0])))
       ;(translate-fn  thumb-anchor)
       ;(rotate-z-fn anchor-rotation)
       
      
       
       
       
       ;(translate-fn (mapv (partial * -1) thumb-anchor))
       
       
       
       
       
       
       
       ))

(defn klor-thumb-place [column  shape]
  (klor-apply-key-geometry-thumb translate rdz rotate-around-z-in-degrees column  shape))
(defn klor-thumb-position [column vector]
  (klor-apply-key-geometry-thumb (partial mapv +) rotate-around-z-in-degrees rotate-around-z-in-degrees column  vector))


(def columns (range 0 ncols))
(def rows (range 0 nrows))
(def thumb-keys (range 0 nthumb-keys))

(defn klor-point-place [point]
  (->> point
   (rotate-around-x-in-degrees 180)
(rotate-around-z-in-degrees 10)
(mapv + [-138.5 87 0]))
  )

(spit "things-low/klor.scad"
      (write-scad 
       (let [key-shape (difference (square (dec key-spacing-horizontal) (dec key-spacing-vertical)) 
                         (square 14 14))]
         
       (union
        (translate (klor-thumb-position 3 [-12.5 -12.5 0]) (cylinder 0.5 20))
        (translate (klor-point-place (mapv + bottom-left-thumb-key-bottom-right-corner [2 2 0])) (cylinder 0.5 20))
        (translate  (klor-point-place [170.303964 118.080182 0] ) (cylinder 3.2 20))
        (color [1 0 0 1](klor-key-place-with-offset 0 1 [-32.5 -10 12] (cube 40 43 2)))
        (color [0 1 0 1](klor-key-place-with-offset 0 2 [-30 7 11] (import "../parts/oled.stl")))
        (->> (import "../parts/klor-ks27-polydactyl-body-right.stl")
             (rdz 10)
             (translate [715.75 85 0]))
        ;; (->> (import "../parts/klor1_3-klor1_3.stl")
        ;;      (rdz 10)
        ;;      (translate [-31 -4.5 0])
        ;;      )
        ;; (->> (polygon pcb-points-list)
        ;;      (rdx 180)
        ;;      (rdz 10)
        ;;      (translate [-138.5 87 0 ]))
        (apply union
              (for [column columns
                    row rows
                    :when (false? (and (= row 2) (= column 5)))] 
                (klor-key-place column row key-shape) 
                ))
        (apply union
               (for [thumb-key thumb-keys]
                 (klor-thumb-place thumb-key key-shape)))
        (-# (->>
             (import "../parts/KLOR_polydactyl_3DP_switchplate.stl")
             
             (translate [-50.25 -53.7 0])
             (rdz anchor-rotation)))
        )
       )
       ))