(ns dactyl-keyboard.sk8707-51
  (:refer-clojure :exclude [use import])
  (:require
   [scad-clj.model :refer :all]
   [scad-clj.scad :refer :all]
   ))

(def sk8707-51-width 31.5)
(def sk8707-51-length 27.00)
(def sk8707-51-pcb-thickness 1.38)
(def sk8707-51-pcb-and-connecter-thickness 1.79)
(def sk8707-51-mounting-hole-diameter 2.2)
(def sk8707-51-bottom-left-mounting-hole-position [2.5 2.5 0])
(def sk8707-51-bottom-right-mounting-hole-position [21.5 2.5 0])
(def sk8707-51-top-mounting-hole-position [12.0 22.0 0])
(def sk8707-51-stem-holder-diameter 3.15)
(def sk8707-51-stem-holder-height 2.35)
(def sk8707-51-stem-holder-position [12 10 sk8707-51-pcb-thickness])
(def sk8707-51-stem-hole-diameter 1.2)
(def sk8707-51-mounting-hole (->> (binding [*fn* 36] (cylinder (/ sk8707-51-mounting-hole-diameter 2) (+ sk8707-51-pcb-thickness 0.2) :center false))
                                  (translate [0 0 -0.1])))
(def sk8707-51-bottom-left-mounting-hole (translate  sk8707-51-bottom-left-mounting-hole-position  sk8707-51-mounting-hole))
(def sk8707-51-bottom-right-mounting-hole (translate  sk8707-51-bottom-right-mounting-hole-position sk8707-51-mounting-hole))
(def sk8707-51-top-mounting-hole (translate  sk8707-51-top-mounting-hole-position  sk8707-51-mounting-hole))

(def sk8707-51-stem-holder (->> (binding [*fn* 36] (cylinder (/ sk8707-51-stem-holder-diameter 2) sk8707-51-stem-holder-height :center false))
                                (translate sk8707-51-stem-holder-position)))
(def sk8707-51-stem (->> (binding [*fn* 36] (cylinder 0.6 20 :center false))
                                (translate (mapv + sk8707-51-stem-holder-position [0 0 sk8707-51-stem-holder-height]))))
(def sk8707-51-pcb (cube sk8707-51-width sk8707-51-length sk8707-51-pcb-thickness :center false))

(def sk8707-51
(let [stem-extension (->>(cylinder 0.4 8 :center false)
                      (translate (mapv + sk8707-51-stem-holder-position [0 0 (+ sk8707-51-stem-holder-height 16)]))
                      (color [1 0 0 1]))]
  (translate (mapv #(* -1 %) sk8707-51-stem-holder-position)(difference 
 (union sk8707-51-pcb
        sk8707-51-stem
        (->> (sphere 2)
             (binding [*fn* 36]) 
             (translate (mapv + sk8707-51-stem-holder-position [0 0 (+ sk8707-51-stem-holder-height 20 2)]))
             (-#))
        ;stem-extension
        (color [0 0 1 1] sk8707-51-stem-holder))
 sk8707-51-bottom-left-mounting-hole
 sk8707-51-bottom-right-mounting-hole
 sk8707-51-top-mounting-hole
 ))))

(spit "things-low/sk8707-51-test.scad"
      (write-scad 
       sk8707-51)
      )