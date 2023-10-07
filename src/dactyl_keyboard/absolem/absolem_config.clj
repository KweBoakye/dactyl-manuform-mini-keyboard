(ns dactyl-keyboard.absolem.absolem-config
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
(def ring-key-stagger 12)
(def middle-key-stagger (+ ring-key-stagger 5))
(def index-key-stagger (+ middle-key-stagger -6))
(def inner-index-key-stagger (+ index-key-stagger -2))

(defn column-offset [column]
  (let [y  (cond
             (= column 0) inner-index-key-stagger
             (= column 1) index-key-stagger
             (= column 2) middle-key-stagger
             (= column 3) ring-key-stagger
             :else 0)]
    [0 y 0]))



(defn splay-rotation [column shape]
  (if (= column 4) shape
      (->> shape 
           (translate  [-19 -12 0];[(* -12 (- columns column)) 19 0]
                      )
           (rdz (splay column))
           (translate (negate [-19 -12 0]);[(* 12 (- columns column)) -19 0]
                      ))
      )
  )

(defn key-place [column row shape]
  (->> shape 
       (translate (column-offset column)) 
       (translate [0 (* (- (dec rows) row) U)  0]) 
       (rdz (splay column))
       (translate (if (=  column 4) [0 0 0] 
                      (rotate-around-z-in-degrees 5 [-12 -19 0]))) 
       (translate [(* column U) 0 0]) 
       (rdz -5) 
       (rdz 20) 
       ))

(spit "things-low/absolem-ergogen-test.scad"
      (let [key (difference (cube 18 18 1 :center true)
                            (cube 14 14 1.1 :center true))]
        (write-scad
       (union
        (->>
         (import "absolem.dxf") 

         (translate (rotate-around-z-in-degrees -5 [-161.25 5.75 0])) 
         (-# ))
        (for [column (range columns) row (range rows)]
          (cond->> (key-place column row key)
            (= column 4) (color [1 0 0])))
        )
       )))