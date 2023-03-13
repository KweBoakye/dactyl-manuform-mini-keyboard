(ns dactyl-keyboard.des-caps
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.lib.transformations :refer [rdz]]
            [dactyl-keyboard.low.placement-functions-low :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.low.thumbs-low :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))

(def des-height (+ plate-thickness 6.5))
(defn des-translate [shape] (translate [0 0 des-height] shape))
(defn des-thumb-translate [shape] (rdz 90 (des-translate shape)))
(def des-r1 (des-translate (import "../parts/caps/DES-R1.stl")))
(def des-r2 (des-translate (import "../parts/caps/DES-R2.stl")))
(def des-r3 (des-translate (import "../parts/caps/DES-R3.stl")))
(def des-r4 (des-translate (import "../parts/caps/DES-R4.stl")))
(def des-r5 (des-translate (import "../parts/caps/DES-R5.stl")))
(def des-kyria-r1t0 (des-thumb-translate (import "../parts/caps/DES-kyria-R1T0.stl")))
(def des-kyria-r1t1 (des-thumb-translate(import "../parts/caps/DES-kyria-R1T1.stl")))
(def des-kyria-r1t2 (des-thumb-translate (import "../parts/caps/DES-kyria-R1T2.stl")))
(def des-kyria-r1t3 (des-thumb-translate (import "../parts/caps/DES-kyria-R1T3.stl")))
(def des-cornelus-c1r (des-translate (import "../parts/caps/C1R.stl")))
(def des-cornelus-c2r (des-translate (import "../parts/caps/C2R_fixed.stl")))
(def des-cornelus-c3r (des-translate (import "../parts/caps/C3R.stl")))
(def des-cornelus-c1l (des-translate (import "../parts/caps/C1L.stl")))
(def des-cornelus-c2l (des-translate (import "../parts/caps/C2L.stl")))
(def des-cornelus-c3l (des-translate (import "../parts/caps/C3L.stl")))

(defn des-scooped [row]
  (case row 
    0 (rdz 180 des-r1)
    1 des-r5
    2 (rdz 0 des-r2)
    )
  )
(defn des-standard [row]
  (case row 
    0 des-r4
  1 des-r3
  2 des-r2
    )
  )

(keyword "des-scooped")
(keyword "des-standard")

(defn des-caps [{:keys [style]}]
  (let [shape (case style
                :des-scooped des-scooped
                :des-standard des-standard)]
    (apply union
         (for [column columns
               row rows
               :when (check-last-row-middle-and-fourth-keys-only column row)]
           (key-place column row (shape row))))))

(def des-thumbs
  (union
   (thumb-tr-place des-kyria-r1t1)
   (thumb-tl-place des-kyria-r1t0)
   (thumb-mr-place des-kyria-r1t0)
   (thumb-bl-place des-kyria-r1t2)
   (thumb-br-place des-kyria-r1t2)))

(def des-cornelius-thumbs
  (union
   (thumb-tr-place des-cornelus-c1r)
(thumb-tl-place des-cornelus-c2r)
(thumb-mr-place des-cornelus-c2r)
(thumb-bl-place des-cornelus-c3r)
(thumb-br-place des-cornelus-c3r)
   ))