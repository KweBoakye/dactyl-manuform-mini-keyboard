(ns dactyl-keyboard.klor.klor-placement-functions
  (:refer-clojure :exclude [use import])
  (:require [clojure.math :refer [sqrt]]
            [dactyl-keyboard.des-caps :refer :all] 
            [dactyl-keyboard.klor.klor-config :refer :all]
            [dactyl-keyboard.klor.klor-constants :refer :all]
            [dactyl-keyboard.klor.klor-points :refer :all]
            [dactyl-keyboard.lib.affine-transformations :refer [rotate-around-x-in-degrees
                                                                rotate-around-z-in-degrees
                                                                mirror-x]]
            [dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline :refer [nurbs nurbs-with-calculated-knot-vector]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vnf :refer [vnf-polyhedron
                                                                     vnf-vertex-array]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [include-bosl2]]
            [dactyl-keyboard.lib.transformations :refer [rdz]]
            [dactyl-keyboard.oled :refer [oled-holder-length oled-holder-width]]
            [dactyl-keyboard.utils :refer [plot-bezier-points select-values]]
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


(defn klor-key-place-with-offset [column row offset shape &{:keys [translate-fn rotate-z-fn vector-rotate-fn]
                                                            :or {translate-fn translate rotate-z-fn rdz vector-rotate-fn rotate-around-z-in-degrees}}]
  (translate-fn (klor-key-position column row offset ) (rotate-z-fn anchor-rotation shape)))
(defn klor-apply-key-geometry-thumb [translate-fn rotate-z-fn vector-rotate-fn column shape]
  (->> shape
       (klor-apply-key-geometry translate-fn rotate-z-fn vector-rotate-fn 0 0)
       
       (translate-fn (vector-rotate-fn anchor-rotation thumb-anchor))
       (translate-fn  (vector-rotate-fn anchor-rotation thumb-origin))
       (translate-fn (vector-rotate-fn anchor-rotation [-11 0 0]))
       
       (translate-fn (vector-rotate-fn (+ anchor-rotation ) [(* column thumb-spread) 0 0]))
       (rotate-z-fn (* column  thumb-splay))
       (translate-fn (case column 
                       0 [-0.035 -0.075 0]
                       1 (vector-rotate-fn (+ anchor-rotation thumb-splay )
                                         [-4.6 -2.55 0])
                       2 (vector-rotate-fn (+ anchor-rotation (* column thumb-splay))
                                           [-8.865 1.78 0])
                       3 (vector-rotate-fn (+ anchor-rotation (* column thumb-splay))
                                           [-11.18 12.52 0])))
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


(defn klor-screen-place [shape &{:keys [ offset height translate-fn rotate-z-fn side mirror-fn] :or {offset [0 0 0] height  11  translate-fn translate rotate-z-fn rdz mirror-fn #(mirror [-1 0 0])}}]
  (let [x-offset (case screen-type
                   :SSD1306 -30
                   :ST7789-135*240 -34)]
    (klor-key-place-with-offset 0 2 (mapv + [x-offset 7 height] offset) (if (= side :left )(mirror-fn shape) shape) :translate-fn translate-fn :rotate-z-fn rotate-z-fn)))

(defn klor-screen-position [offset &{:keys [height side] :or { height 0 side :right}}]
  (klor-screen-place [0 0 0] :offset offset :height height :translate-fn (partial mapv +) 
                     :rotate-z-fn rotate-around-z-in-degrees :side side :mirror-fn mirror-x))

(defn klor-tps-43-place [shape & {:keys [offset height translate-fn rotate-z-fn] :or {offset [0 0 0] height (- klor-case-walls-height 1) translate-fn translate rotate-z-fn rdz}}] 
  (klor-key-place-with-offset 0 1 (mapv + [-32.5 -10 height] offset) shape :translate-fn translate-fn :rotate-z-fn rotate-z-fn))
(defn klor-tps-43-position [offset & {:keys [height] :or {height 0}}]
  (klor-tps-43-place [0 0 0] :offset offset :height height :translate-fn (partial mapv +) :rotate-z-fn rotate-around-z-in-degrees))


