(ns dactyl-keyboard.sk8707-06
  (:refer-clojure :exclude [use import])
(:require
 [scad-clj.model :refer :all]
 [scad-clj.scad :refer :all]))

(def sk8707-06-width 23.0)
(def sk8707-06-pcb-width 13.2)
(def sk8707-06-pcb-height 0.8)
(def sk8707-06-length-from-top-to-stem-centre 7.55)
(def sk8707-06-length-from-top-to-mounting-hole-centre sk8707-06-length-from-top-to-stem-centre)
(def sk8707-06-tab-width (/ (- sk8707-06-width sk8707-06-pcb-width) 2))
(def sk8707-06-length-from-stem-centre-to-pcb-bottom 11.8)
(def sk8707-06-length-from-mounting-hole-centre-to-pcb-bottom sk8707-06-length-from-stem-centre-to-pcb-bottom)
(def sk8707-06-length-from-stem-centre-to-mounting-tab-bottom 4.8)
(def sk8707-06-mounting-tab-length (+ sk8707-06-length-from-top-to-stem-centre sk8707-06-length-from-stem-centre-to-mounting-tab-bottom))
(def sk8707-06-mounting-tab-thickness 0.5)
(def sk8707-06-length-from-mounting-hole-centre-to-mounting-tab-bottom sk8707-06-length-from-stem-centre-to-mounting-tab-bottom)
(def sk8707-06-pcb-length (+ sk8707-06-length-from-top-to-stem-centre sk8707-06-length-from-stem-centre-to-pcb-bottom))
(def sk8707-06-pcb-width-between-mounting-holes 19.0)
(def sk8707-06-mounting-hole-diameter 2.0)
(def sk8707-06-stem-holder-height 2.4)
(def sk8707-06-stem-holder-diameter 2.4)

(def sk8707-06-pcb (->>(cube sk8707-06-pcb-width sk8707-06-pcb-length sk8707-06-pcb-height :center false)
                    (translate [(/ sk8707-06-pcb-width -2) (- sk8707-06-length-from-stem-centre-to-pcb-bottom) 0])))
(defn sk8707-06-mounting-tab [&{:keys [side] :or {side :right}}] (->>(cube sk8707-06-tab-width sk8707-06-mounting-tab-length sk8707-06-mounting-tab-thickness :center false)
                             (translate [(case side
                                           :right (+ (/ sk8707-06-pcb-width 2))
                                           :left (- (+ (/ sk8707-06-pcb-width 2) sk8707-06-tab-width))) 
                                         (- sk8707-06-length-from-mounting-hole-centre-to-mounting-tab-bottom) 
                                         sk8707-06-pcb-height])))
(def sk8707-06-stem-holder (->>
                            (binding [*fn* 36] (cylinder (/ sk8707-06-stem-holder-diameter 2) sk8707-06-stem-holder-height :center false))
                            (translate [0 0 sk8707-06-pcb-height])))

(defn sk8707-06-mounting-hole [& {:keys [side] :or {side :right}}]
  (->> (binding [*fn* 36] (cylinder (/ sk8707-06-mounting-hole-diameter 2) (+ sk8707-06-mounting-tab-thickness 0.1) :center false))
       (translate [(case side
                     :left (/ sk8707-06-pcb-width-between-mounting-holes -2)
                     :right (/ sk8707-06-pcb-width-between-mounting-holes 2))
                   0 
                   (- sk8707-06-pcb-height 0.05)])))

(def sk8707-06-stem (->> (binding [*fn* 36] (cylinder 0.6 20 :center false))
                         (translate [0 0 (+ sk8707-06-pcb-height sk8707-06-stem-holder-height)])))

(def sk8707-06 
  (union
      sk8707-06-pcb
      (difference (sk8707-06-mounting-tab) (sk8707-06-mounting-hole))
      (difference (sk8707-06-mounting-tab :side :left) (sk8707-06-mounting-hole :side :left))
   (->> (sphere 2)
        (binding [*fn* 36])
        (translate [0 0 (+ sk8707-06-pcb-height sk8707-06-stem-holder-height 22)])
        (-#))   
   (-# sk8707-06-stem-holder)
   sk8707-06-stem))
(spit "things-low/sk8707-06-test.scad"
      (write-scad
       sk8707-06
       ))

