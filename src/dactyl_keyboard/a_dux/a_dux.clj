(ns dactyl-keyboard.a-dux.a-dux
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
(def ring-key-stagger 18)
(def middle-key-stagger (+ ring-key-stagger (/ 17 3)))
(def index-key-stagger (+ middle-key-stagger (/ 17 -3)))
(def inner-index-key-stagger (+ index-key-stagger (/ 17 -6)))

(defn column-offset [column]
  (let [y  (cond
             (= column 0) inner-index-key-stagger
             (= column 1) index-key-stagger
             (= column 2) middle-key-stagger
             (= column 3) ring-key-stagger
             :else 0)]
    [0 y 0]))

(def pinky-key-splay 15)
(def ring-key-splay (+ pinky-key-splay -10))
(def middle-key-splay (+ ring-key-splay -5))
(def index-key-splay (+ middle-key-splay -5))
(def inner-index-key-splay (+ index-key-splay 0))

(defn splay [column]
  (cond
    (= column 0) inner-index-key-splay
    (= column 1) index-key-splay
    (= column 2) middle-key-splay
    (= column 3) ring-key-splay
    (= column 4) pinky-key-splay
    :else 0))

(defn padding [row]
  (if (= row 0) 17 17)
  )

(defn key-place [column row shape]
  (->> shape 
       (translate (column-offset column))
       
       (translate [0 (* (- (dec rows) row) (padding row))  0])
       (translate [0 17 0])
       (rdz (splay column))
       (translate [0 -17 0])
       (translate [(* (- (dec columns) column) 18) 0 0])
       ))

(spit "things-low/a-dux-test.scad"
      (let [key (difference (cube 18 17 1 :center true)
                            (cube 14 14 1.1 :center true))]
        (write-scad
         (union
          (->>
           (import "a-dux.dxf")
           (-#))
          (for [column (range columns) row (range rows)]
            (cond->> (key-place column row key)
              (and (= column 4) (= row 2)) (color [0 1 0]))))
         )))