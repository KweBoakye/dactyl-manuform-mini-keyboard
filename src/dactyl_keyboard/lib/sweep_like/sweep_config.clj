(ns dactyl-keyboard.lib.sweep-like.sweep-config
  (:refer-clojure :exclude [use import])
  (:require 
   [scad-clj.model :refer :all]
  [scad-clj.scad :refer :all]
   [clojure.core.matrix :refer [negate]]
   [dactyl-keyboard.lib.affine-transformations :refer [rotate-around-z-in-degrees]]
   [dactyl-keyboard.lib.transformations :refer [rdz]]))

(def U 19.05)
(def columns 5)
(def rows 3)
(def ring-key-stagger (* 0.66 U))
(def middle-key-stagger (+ ring-key-stagger (* 0.25 U)))
(def index-key-stagger (+ middle-key-stagger (* -0.25 U)))
(def inner-index-key-stagger (+ index-key-stagger (* -0.15 U)))




(def thumb-shift [(* -0.66 U) (* -1.25 U) 0])
(def thumb-z-rotation 10)


(defn column-offset [column]
  (let [y  (cond
    (= column 0) inner-index-key-stagger
    (= column 1) index-key-stagger
    (= column 2) middle-key-stagger
    (= column 3) ring-key-stagger 
    :else 0)]
    [0 y 0]
    ))

(defn key-place [column row shape]
  (->> shape
       (translate (column-offset column)) 
       (translate [0 (* (- (dec rows ) row) U)  0])
       ;(rdz (if (= column 4) -10 0))
       (translate [(* column U) 0 0])
       )
  )


(spit "things-low/sweep-ergogen-test.scad"
      (write-scad 
       
       (let [key (difference (cube 18 18 1 :center true) 
                  (cube 14 14 1.1 :center true))]
         (union
          (->>
           (import "demo.dxf")
           (mirror [1 0 0])
           (translate [76.1 0 0])
           (-#))
          (for [column (range columns) row (range rows)]
            (cond->> (key-place column row key)
              (= column 4) (color [1 0 0]))
            )
          (color [0 1 0 1](->> key
                           (rdz thumb-z-rotation)
                                  (key-place 1 2)
                                  (translate thumb-shift)))
          (->> key
               (translate  [(* -0.5 U) (* 0.5 U) 0])
               (rdz 15)
               (translate (negate [(* -0.5 U) (* 0.5 U) 0]))
            (translate [(- U) 0 0])
               (rdz thumb-z-rotation)
               (key-place 1 2)
               (translate thumb-shift)

            
               
               ) 
          )
         )))