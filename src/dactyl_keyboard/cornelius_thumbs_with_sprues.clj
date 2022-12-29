(ns dactyl-keyboard.cornelius-thumbs-with-sprues
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.des-caps :refer :all]
            ;[dactyl-keyboard.placement-functions :refer :all]
            ;[dactyl-keyboard.web-connecters :refer :all]
           ; [dactyl-keyboard.case :refer :all]
            [unicode-math.core :refer :all]))

(def sprue-height 1.5)
(def sprue-width 1.5)
(def sprue-length 4)
(def sprue (->> 
            (cube  sprue-length sprue-height sprue-width) 
            (translate [0 0 (/ sprue-height 2)])))
(def des-cornelus-c1r-width 18)
(def des-cornelus-c2r-width 18.5)
(def des-cornelus-c3r-width 18)

(def des-cornelus-c1r-amount 3)
(def des-cornelus-c2r-amount 3)
(def des-cornelus-c3r-amount 3)

(def des-cornelus-c1l-amount 3)
(def des-cornelus-c2l-amount 3)
(def des-cornelus-c3l-amount 3)

(defn des-cornelus-c1r-x-pos [index]
  (* index (+ des-cornelus-c1r-width sprue-length))
  )
(defn des-cornelus-c1r-sprue-x-pos [index]
  (+ (/ des-cornelus-c1r-width 2) (/ sprue-length 2) (* index (+ des-cornelus-c1r-width sprue-length)))
  )

(defn des-cornelus-c2r-x-pos [index]
  (+ (des-cornelus-c1r-x-pos des-cornelus-c1r-amount) (* index (+ des-cornelus-c2r-width sprue-length))))
(defn des-cornelus-c2r-sprue-x-pos [index]
  (+ (des-cornelus-c1r-x-pos des-cornelus-c1r-amount) (/ des-cornelus-c2r-width 2) (/ sprue-length 2) (* index (+ des-cornelus-c2r-width sprue-length))))

(defn des-cornelus-c3r-x-pos [index]
  (+  (des-cornelus-c2r-x-pos des-cornelus-c2r-amount) (* index (+ des-cornelus-c3r-width sprue-length))))
(defn des-cornelus-c3r-sprue-x-pos [index]
  (+ (des-cornelus-c2r-x-pos des-cornelus-c2r-amount) (/ des-cornelus-c3r-width 2) (/ sprue-length 2) (* index (+ des-cornelus-c3r-width sprue-length))))

(defn des-cornelus-c1l-x-pos [index]
  (- (* index (+ des-cornelus-c1r-width sprue-length))))
(defn des-cornelus-c1l-sprue-x-pos [index]
  (- (+ (/ des-cornelus-c1r-width 2) (/ des-cornelus-c1l-amount 2) (* index (+ des-cornelus-c1r-width sprue-length)))))

(defn des-cornelus-c2l-x-pos [index]
 (- (des-cornelus-c1l-x-pos des-cornelus-c1l-amount) (* index (+ des-cornelus-c2r-width sprue-length))))
(defn des-cornelus-c2l-sprue-x-pos [index]
  (- (des-cornelus-c1l-x-pos des-cornelus-c1l-amount) (/ des-cornelus-c2r-width 2) (/ sprue-length 2) (* index (+ des-cornelus-c2r-width sprue-length))))

(defn des-cornelus-c3l-x-pos [index]
   (-  (des-cornelus-c2l-x-pos des-cornelus-c2l-amount) (* index (+ des-cornelus-c3r-width sprue-length))))
(defn des-cornelus-c3l-sprue-x-pos [index]
  (- (des-cornelus-c2l-x-pos des-cornelus-c2l-amount) (/ des-cornelus-c3r-width 2) (/ sprue-length 2) (* index (+ des-cornelus-c3r-width sprue-length))))
(def right-with-sprues 
  
   (apply union
          (for [index (range 0 des-cornelus-c1r-amount)]
            (translate [ (des-cornelus-c1r-x-pos index) 0 (- des-height)] des-cornelus-c1r)
            )
          (for [index (range 0 des-cornelus-c1r-amount)]
            (translate [(des-cornelus-c1r-sprue-x-pos index) 0 0] sprue))
          (for [index (range 0 des-cornelus-c2r-amount)]
            (translate [(des-cornelus-c2r-x-pos index) 0 (- des-height)] des-cornelus-c2r))
           (for [index (range 0 des-cornelus-c2r-amount)]
             (translate [(des-cornelus-c2r-sprue-x-pos index) 0 0] sprue))
          (for [index (range 0 des-cornelus-c3r-amount)]
            (translate [(des-cornelus-c3r-x-pos index) 0 (- des-height)] des-cornelus-c3r))
           (for [index (range 0 (dec des-cornelus-c3r-amount))]
             (translate [(des-cornelus-c3r-sprue-x-pos index) 0 0] sprue))
          
;;    (translate [0 0 (- des-height)] des-cornelus-c1r)
;;    (translate [(+ (/ des-cornelus-c1r-width 2) (/ sprue-length 2)) 0 0] sprue)
;;    (translate [(+ des-cornelus-c1r-width sprue-length) 0 (- des-height)] des-cornelus-c2r)
;;    (translate [(+ (/ des-cornelus-c1r-width 2) des-cornelus-c2r-width sprue-length (/ sprue-length 2)) 0 0] sprue)
;;    (translate [(+ des-cornelus-c2r-width des-cornelus-c1r-width (* sprue-length 2)) 0 (- des-height)] des-cornelus-c3r)
   )
  ;;   (union
;;    (translate [0 0 (- des-height)] des-cornelus-c3r)
;;    (-# (translate [0 0 5] (cube des-cornelus-c3r-width 15 10) )))
  )

(def left-with-sprues

  (apply union
         (for [index (range 0 des-cornelus-c1l-amount)]
           (translate [(des-cornelus-c1l-x-pos index) 0 (- des-height)] des-cornelus-c1l))
         (for [index (range 0 des-cornelus-c1l-amount)]
           (translate [(des-cornelus-c1l-sprue-x-pos index) 0 0] sprue))
         (for [index (range 0 des-cornelus-c2l-amount)]
           (translate [(des-cornelus-c2l-x-pos index) 0 (- des-height)] des-cornelus-c2l))
         (for [index (range 0 des-cornelus-c2l-amount)]
           (translate [(des-cornelus-c2l-sprue-x-pos index) 0 0] sprue))
         (for [index (range 0 des-cornelus-c3l-amount)]
           (translate [(des-cornelus-c3l-x-pos index) 0 (- des-height)] des-cornelus-c3l))
         (for [index (range 0 (dec des-cornelus-c3l-amount))]
           (translate [(des-cornelus-c3l-sprue-x-pos index) 0 0] sprue))
         
  )
  )