(ns dactyl-keyboard.sk8707-51
  (:refer-clojure :exclude [use import])
  (:require [clojure.math :refer [cos sqrt ]]
            [dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline :refer [nurbs]]
            [dactyl-keyboard.lib.geometry :refer [deg2rad]]
            [dactyl-keyboard.lib.vectors :refer [calculate-point-between-points]]
            [dactyl-keyboard.utils :refer [plot-bezier-points multiple-standoffs]]
            [clojure.core.matrix :refer [div mul negate]]

            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))

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
(def sk8707-51-heat-insert-depth 3.5)
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
(def sk8707-51-breakout (import "../parts/sk8707-5x-breakout.stl"))
(def sk8707-51-breakout-mounting-hole-position-left [124.968 -57.8104])
(def sk8707-51-breakout-mounting-hole-position-right [140.716 -57.8612])
(def sk8707-51-breakout-mounting-hole-mid-x-coordinate (/ (+ (nth sk8707-51-breakout-mounting-hole-position-left 0) (nth sk8707-51-breakout-mounting-hole-position-right 0)) -2))
(def sk8707-51-breakout-mounting-standoff-inner-diameter 3.2)
(def sk8707-51-breakout-mounting-standoff-outer-diameter (+ sk8707-51-breakout-mounting-standoff-inner-diameter 2))
(def sk8707-51-breakout-mounting-standoff-inner-radius (/ sk8707-51-breakout-mounting-standoff-inner-diameter 2))
(def sk8707-51-breakout-mounting-standoff-outer-radius (/ sk8707-51-breakout-mounting-standoff-outer-diameter 2))
(def sk8707-51-breakout-mounting-standoff-height 4)  

(def sk8707-51-breakout-mounting-standoffs
  (multiple-standoffs sk8707-51-breakout-mounting-standoff-inner-radius
                      sk8707-51-breakout-mounting-standoff-outer-radius
                      sk8707-51-breakout-mounting-standoff-height
                      [sk8707-51-breakout-mounting-hole-position-left sk8707-51-breakout-mounting-hole-position-right]))
(def sk8707-51-breakout-support-bar
  (->>
   (cube 4 (+ sk8707-51-breakout-mounting-standoff-outer-diameter 2) sk8707-51-breakout-mounting-standoff-height)
   (translate [(- sk8707-51-breakout-mounting-hole-mid-x-coordinate)
               (+ (min (nth sk8707-51-breakout-mounting-hole-position-left 1) (nth sk8707-51-breakout-mounting-hole-position-right 1)) 4)
               (/ sk8707-51-breakout-mounting-standoff-height 2)])))

(spit "things-low/sk8707_51-breakout-test.scad"
      (write-scad
       (union
        (translate [0 0 4] sk8707-51-breakout)
        sk8707-51-breakout-mounting-standoffs
        sk8707-51-breakout-support-bar
        ;(translate sk8707-51-breakout-mounting-hole-position-left (cylinder 1 8))
        )))
(defn sk8707-51-breakout-place [shape]
  (translate [-140 50 0] shape)
  )
(def sk8707-51-breakout-mount 
  (union sk8707-51-breakout-mounting-standoffs
     sk8707-51-breakout-support-bar))
(def sk8707-51-stem-cutout 
       (translate (mapv #(* -1 %) sk8707-51-stem-holder-position) (->> (binding [*fn* 36] (cylinder 1.0 20 :center false))
                                                                       (translate (mapv + sk8707-51-stem-holder-position [0 0 sk8707-51-stem-holder-height]))))
       )
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

(defn sk8707-51-mount-shape [radius &{:keys [func] :or {func #(mapv + [0 0 0] %)}}]
  (let [;radius (/ (+ sk8707-51-mounting-hole-diameter 4) 2)
        a (cos (deg2rad 30))
        top-point (func (mapv + (mapv #(* -1 %) sk8707-51-stem-holder-position) sk8707-51-top-mounting-hole-position))
        bottom-left-point (func (mapv + (mapv #(* -1 %) sk8707-51-stem-holder-position) sk8707-51-bottom-left-mounting-hole-position))
        bottom-right-point (func (mapv + (mapv #(* -1 %) sk8707-51-stem-holder-position) sk8707-51-bottom-right-mounting-hole-position))
        points (mapv  (fn [point]
                        (mapv + point ;(mapv #(* -1 %) sk8707-51-stem-holder-position)
                              ))
                      [(mapv + [(* -1 a radius) (* 0.5 radius) 0] top-point)
                       (mapv + [0 (* 2 radius) 0] top-point)
                       (mapv + [(*  a radius) (* 0.5 radius) 0] top-point)
                       (calculate-point-between-points (mapv +  [(*  a radius) (* 0.5 radius) 0] bottom-right-point)
                                                       (mapv + [(*  a radius) (* 0.5 radius) 0] top-point)
                                                       [0 0 0])
  
                       (mapv +  [(*  a radius) (* 0.5 radius) 0] bottom-right-point)
                       (mapv +  [(* 2 a radius) (* -1 radius) 0] bottom-right-point)
                       (mapv +  [0 (* -1 radius) 0] bottom-right-point)
  
                       
                       (calculate-point-between-points (mapv +  [0 (* -1 radius) 0] bottom-left-point)
                                                       (mapv +  [0 (* -1 radius) 0] bottom-right-point)
                                                       [0 0 0])
                       (mapv +  [0 (* -1 radius) 0] bottom-left-point)
                       (mapv +  [(* -2 a radius) (* -1 radius) 0] bottom-left-point)
                       (mapv +  [(* -1 a radius) (* 0.5 radius) 0] bottom-left-point) 
                       (calculate-point-between-points (mapv + [(* -1 a radius) (* 0.5 radius) 0] top-point)
                                                       (mapv +  [(* -1 a radius) (* 0.5 radius) 0] bottom-left-point)
                                                       [0 0 0])
                       (mapv + [(* -1 a radius) (* 0.5 radius) 0] top-point)])
        circ-weight (/ (sqrt 2) 2)
        weights [1 0.5 1
                 1
                          ;circ-weight
                 1 0.5 1
                 1
                 1 0.5 1
                 1
                          ;;1 circ-weight 1 circ-weight
                          ;1 circ-weight
                 1]
        segments (- (count points) 2)
        divisor (dec (count points))
        knot-vector (mul segments
                         (div [0 0 0 2 2 4 4 6 6 8 8 10 10      (* 1 divisor) (* 1 divisor) (* 1 divisor)]
                              divisor))]
    
    
     
      (nurbs points 2 knot-vector weights 60)
     ))
(defn sk8707-51-mount-cutout [& {:keys [sk8707-51-mount-thickness] :or {sk8707-51-mount-thickness 2}}]
  (let [cutout-scaffold (->> (binding [*fn* 36] (cylinder 0.2 (+ sk8707-51-mount-thickness 0.2) :center false))
                             (translate [0 0 -0.1]))]
    (translate (mapv + [0 0 (- sk8707-51-mount-thickness)] (mapv #(* -1 %) sk8707-51-stem-holder-position))
               (hull
     (translate  (mapv +  sk8707-51-bottom-left-mounting-hole-position [sk8707-51-mounting-hole-diameter sk8707-51-mounting-hole-diameter 0])  cutout-scaffold)
     (translate  (mapv +  sk8707-51-bottom-right-mounting-hole-position [(- sk8707-51-mounting-hole-diameter) sk8707-51-mounting-hole-diameter 0])  cutout-scaffold)
     (translate  (mapv +  sk8707-51-top-mounting-hole-position [0 (- (+ sk8707-51-mounting-hole-diameter 1)) 0])  cutout-scaffold)))
    
    ))

(defn sk8707-51-mount [&{:keys [sk8707-51-mount-thickness] :or {sk8707-51-mount-thickness 2}}]  
  (let [
        holder-cyl (->> (binding [*fn* 36] (cylinder (/ (+ sk8707-51-mounting-hole-diameter 4) 2) sk8707-51-mount-thickness :center false))
                         (translate [0 0 -0.1]))
         holder-hole (->> (binding [*fn* 36] (cylinder (/ 3.4 2) (+ sk8707-51-mount-thickness 0.2) :center false))
                          (translate [0 0 -0.1]))
         ]
    
               (difference
                (translate (mapv + [0 0 (- sk8707-51-mount-thickness)] (mapv #(* -1 %) sk8707-51-stem-holder-position)) (hull
                 (translate  sk8707-51-bottom-left-mounting-hole-position  holder-cyl)
                 (translate  sk8707-51-bottom-right-mounting-hole-position holder-cyl)
                 (translate  sk8707-51-top-mounting-hole-position  holder-cyl)))
                (translate (mapv + [0 0 (- sk8707-51-mount-thickness)] (mapv #(* -1 %) sk8707-51-stem-holder-position))(union (translate  sk8707-51-bottom-left-mounting-hole-position  holder-hole)
                (translate  sk8707-51-bottom-right-mounting-hole-position holder-hole)
                (translate  sk8707-51-top-mounting-hole-position  holder-hole)))
                (sk8707-51-mount-cutout :sk8707-51-mount-thickness sk8707-51-mount-thickness))
    )
  )

(def sk8707-51-mount-heat-insert-holes
  (let [sk8707-51-heat-insert-hole (->> (binding [*fn* 36] (cylinder (/ 3.4 2) sk8707-51-heat-insert-depth :center false))
                                        (translate [0 0 -0.1]))]
    (union
   (mapv #(translate (mapv + (negate sk8707-51-stem-holder-position) % [0 0 (- sk8707-51-heat-insert-depth)]) sk8707-51-heat-insert-hole) [sk8707-51-bottom-right-mounting-hole-position sk8707-51-bottom-left-mounting-hole-position sk8707-51-top-mounting-hole-position]))))

(comment (spit "things-low/sk8707-51-test.scad"
      (write-scad 
       
         (union 
          (plot-bezier-points (sk8707-51-mount-shape (/ (+ sk8707-51-mounting-hole-diameter 4) 2)) (sphere 0.1))
          
        (-# sk8707-51)
          sk8707-51-mount
        ))
      ))