(ns dactyl-keyboard.dilemma.dilemma-test
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [div mul pow]]
            [clojure.math :refer [get-exponent sqrt]]
            [dactyl-keyboard.klor.klor-case-walls :refer [tps-43-breakout-cutout-body]]
            [dactyl-keyboard.lib.affine-transformations :refer [rotate-around-x-in-degrees
                                                                rotate-around-z-in-degrees]]
            [dactyl-keyboard.lib.constants :refer [epsilon]]
            [dactyl-keyboard.lib.curvesandsplines.beziers :refer [bezier-linear-spline
                                                                  n-degree-bezier-curve]]
            [dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline :refer [decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                                                                               decompose-non-homogoneus-nurbs-to-nurbs-params
                                                                               nurbs]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [BACK
                                                                           include-bosl2
                                                                           LEFT
                                                                           RIGHT]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.mask-3d :refer [rounding-edge-mask]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.shapes-3d :refer [cuboid]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.skin :refer [skin]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vnf :refer [vnf-polyhedron
                                                                     vnf-vertex-array]]
            [dactyl-keyboard.lib.openscad.hull :refer [chained-hull-for-four-lists]]
            [dactyl-keyboard.lib.transformations :refer [rdx rdy rdz]]
            [dactyl-keyboard.low.case-low-polyhedron-functions :refer [wall-vnf-array]]
            [dactyl-keyboard.oled :refer [ST7789-135*240-holder
                                          ST7789-135*240-holder-cut
                                          ST7789-135*240-holder-height
                                          ST7789-135*240-holder-length
                                          ST7789-135*240-holder-width
                                          ST7789-135*240-viewport-thickness]]
            [dactyl-keyboard.tps-43 :refer [tps-43-cutout tps-43-mount
                                            tps-43-mount-body
                                            tps-43-mount-length
                                            tps-43-mount-width]]
            [dactyl-keyboard.utils :refer [plot-bezier-points]]
            [dactyl-keyboard.vybronics-vl91022 :refer [holder
                                                       vybronics-vl91022-mount-body
                                                       vybronics-vl91022-mount-body-subtract]]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))
  

(def x-distance-between-bottom-holes 31)
(def y-distance-between-top-and-bottom-holes 32)
(def x-distance-between-top-holes 28)
(def x-distance-from-top-right-hole-to-bottom-right-hole 1.4)
(def x-distance-from-top-left-hole-to-bottom-left-hole -1.6)
(def hole-diameter 3)
(def hole-radius (/  hole-diameter 2))
(def insert-hole-diameter 4.6)
(def insert-hole-radius (/ insert-hole-diameter 2))
(def wall-thickness 1.6)
(def insert-holder-radius (+ insert-hole-radius wall-thickness))
(def blind-hole-length-offset 1.1)
(def hole-depth 5.7)


(def bottom-left-hole-position 
  [(/ x-distance-between-bottom-holes -2) (/ y-distance-between-top-and-bottom-holes -2) 0])
(def bottom-right-hole-position
  [(+ (/ x-distance-between-bottom-holes 2) 0.2) (/ y-distance-between-top-and-bottom-holes -2) 0])
(def top-left-hole-position
  [(- (/ x-distance-between-bottom-holes -2) x-distance-from-top-left-hole-to-bottom-left-hole)
   (/ y-distance-between-top-and-bottom-holes 2) 0])
(def top-right-hole-position
  [(+  (- (/ x-distance-between-bottom-holes -2) x-distance-from-top-left-hole-to-bottom-left-hole)
       x-distance-between-top-holes)
   (/ y-distance-between-top-and-bottom-holes 2) 0])

(comment (mapv - top-right-hole-position top-left-hole-position))

(comment (mapv - bottom-right-hole-position bottom-left-hole-position))

(def tps-43-mount-height 14.5)
(def screen-height 6)
(def extra-height 2)
(defn tps-43-mount-place [shape &{:keys [translation-fn] :or {translation-fn translate}}]
  (->> shape
           (translation-fn [21.5 20 0])
           (translation-fn [210 172.35 (+ tps-43-mount-height extra-height)])))

(defn tps-43-mount-point-place [point] (tps-43-mount-place point :translation-fn #(mapv + %1 %2)))
(defn screen-place [shape &{:keys [rotate-x-fn translation-fn] :or {rotate-x-fn rdx translation-fn translate}}]
  (->> shape
       (rotate-x-fn 20)
       (translation-fn [239.838808 (- 191.4 34.5) (+ screen-height extra-height)]))) 

(defn screen-place-fn [point] (screen-place point :rotate-x-fn rotate-around-x-in-degrees :translation-fn #(mapv + %1 %2)))
(def tps-43-top-left-position  (tps-43-mount-point-place [(/ tps-43-mount-width -2) (/ tps-43-mount-length 2) 0]))
(def tps-43-top-mid-position  (tps-43-mount-point-place [0 (/ tps-43-mount-length 2) 0]))
(def tps-43-mid-left-position  (tps-43-mount-point-place [(/ tps-43-mount-width -2) 0 0]))
(def tps-43-top-right-position (tps-43-mount-point-place [(/ tps-43-mount-width 2) (/ tps-43-mount-length 2) 0]))
(def tps-43-mid-right-position (tps-43-mount-point-place [(/ tps-43-mount-width 2) 0 0]))
(def tps-43-bottom-left-position (tps-43-mount-point-place [(/ tps-43-mount-width -2) (/ tps-43-mount-length -2) 0]))
(def tps-43-bottom-right-position (tps-43-mount-point-place [(/ tps-43-mount-width 2) (/ tps-43-mount-length -2) 0]))

(def ST7789-135*240-top-left-position  (screen-place-fn [(/ ST7789-135*240-holder-width -2) (/ ST7789-135*240-holder-length 2) ST7789-135*240-holder-height]))
(def ST7789-135*240-top-right-position (screen-place-fn [(/ ST7789-135*240-holder-width 2) (/ ST7789-135*240-holder-length 2) ST7789-135*240-holder-height]))
(def ST7789-135*240-mid-right-position (screen-place-fn [(/ ST7789-135*240-holder-width 2) 0 ST7789-135*240-holder-height]))
(def ST7789-135*240-bottom-position (screen-place-fn [0 (/ ST7789-135*240-holder-length -2) ST7789-135*240-holder-height]))
(def ST7789-135*240-bottom-left-position (screen-place-fn [(/ ST7789-135*240-holder-width -2) (/ ST7789-135*240-holder-length -2) ST7789-135*240-holder-height]))
(def ST7789-135*240-bottom-right-position (screen-place-fn [(/ ST7789-135*240-holder-width 2) (/ ST7789-135*240-holder-length -2) ST7789-135*240-holder-height]))

(def left-curve-points [tps-43-top-left-position
                        ST7789-135*240-top-left-position
                        tps-43-bottom-left-position
                        ST7789-135*240-bottom-left-position])

(def right-curve-points [tps-43-top-right-position
                         tps-43-bottom-right-position
                         ST7789-135*240-top-right-position
                         ST7789-135*240-bottom-right-position])
                         
(def x-offset 1)
(def y-offset 1)
(def outer-points [(mapv + [(- x-offset)  y-offset 0] tps-43-top-left-position) 
                   (mapv + [(- x-offset)  0 0] tps-43-mid-left-position)
                   (mapv + [(- x-offset)  (- y-offset) 0] tps-43-bottom-left-position) 
                   ST7789-135*240-top-left-position 
                   (mapv + [(- x-offset)  (- y-offset) 0] ST7789-135*240-bottom-left-position)
                   (mapv + [0 (- y-offset) 0] ST7789-135*240-bottom-position)
                   (mapv + [ x-offset  (- y-offset) 0]ST7789-135*240-bottom-right-position)
                   (mapv + [x-offset  0 0] ST7789-135*240-mid-right-position) 
                   (mapv + [x-offset  0 0]  ST7789-135*240-top-right-position) 
                   (mapv + [x-offset  y-offset 0]  tps-43-bottom-right-position)
                   (mapv + [x-offset  0 0]  tps-43-mid-right-position)
                   (mapv + [x-offset  y-offset 0]  tps-43-top-right-position)
                   (mapv + [0  y-offset 0]  tps-43-top-mid-position)
                   (mapv + [(- x-offset)  y-offset 0] tps-43-top-left-position)])
             


(def circ-weight (/ (sqrt 2) 2))
(def hole (binding [*fn* 8] (cylinder hole-radius 10)))
(def inner-points [tps-43-top-left-position
                   tps-43-mid-left-position
                   tps-43-bottom-left-position 
                   ST7789-135*240-top-left-position
                   ST7789-135*240-bottom-left-position
                   ST7789-135*240-bottom-right-position
                   ST7789-135*240-top-right-position
                   tps-43-bottom-right-position
                   tps-43-top-right-position
                   tps-43-top-left-position])
(def outer-curve-segments  (- (count outer-points) 2))
(def outer-curve-divisor (dec (count outer-points)))
(def outer-curve-knot-vector (mul outer-curve-segments
                                  (div [0 0 0 2 2 4 4 6 6 8 8 10 11 11  (* 1 outer-curve-divisor) (* 1 outer-curve-divisor) (* 1 outer-curve-divisor)]
                                       outer-curve-divisor)))
(def outer-curve-weights [1 1 1
                          circ-weight 1 1
                          1 1 1 1
                          circ-weight 1 1 1 1])
(defn outer-curve [steps &{:keys [level] :or {level :top}}]  
  (let [offset (cond (= level :top) [0 0 0]
                     (= level :bottom) [0 0 (- ST7789-135*240-holder-height)])]
        
        
        
        
    
    
    (nurbs (mapv #(mapv + offset %  )outer-points) 2
           outer-curve-knot-vector
           outer-curve-weights
           steps)))

(defn inner-wall-points [steps]
  (let [top-points (bezier-linear-spline inner-points steps)
        bottom-points (bezier-linear-spline (mapv #(mapv + [0 0 (- ST7789-135*240-holder-height)] %) inner-points) steps)]
        
    (for [index (range (inc steps))]
      [   (nth bottom-points index) (nth top-points index)])))
    
  

(defn outer-wall-points [steps]
  (let [top-points  (outer-curve steps)
        bottom-points (outer-curve steps :level :bottom)]
    (for [index (range (inc steps))]
      [(nth top-points index) (nth bottom-points index)])))

(defn outer-wall-surface [steps]
  (let [top-points  (outer-curve steps)
        bottom-points (outer-curve steps :level :bottom)]
    
    (union (skin [top-points bottom-points] :refine 20 :sampling :length
                 :method :reindex :style :quincunx))))
           ;(plot-bezier-points top-points (cylinder 1 1))
           ;(plot-bezier-points bottom-points (cylinder 1 1))
           

(defn inner-wall-surface [steps]
  (let [top-points (bezier-linear-spline inner-points steps)
        bottom-points (bezier-linear-spline (mapv #(mapv + [0 0 (- ST7789-135*240-holder-height)] %) inner-points) steps)]
    (println  top-points bottom-points)
    (skin   [top-points bottom-points])))

(defn mount-faces [steps]
  ;; (let [top-inner-points (bezier-linear-spline inner-points steps)
  ;;       bottom-inner-points (bezier-linear-spline (mapv #(mapv + [0 0 ST7789-135*240-holder-height] %) inner-points) steps)
  ;;       top-outer-points  (outer-curve steps)
  ;;       bottom-outer-points (outer-curve steps :level :bottom)])
  (let [outer-wall (outer-wall-points steps)
        inner-wall (inner-wall-points steps)]
    (vnf-vertex-array outer-wall inner-wall {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default})))
    
    
  

(defn mount-faces-hull [steps]
  (let [top-inner-points (bezier-linear-spline inner-points steps)
           bottom-inner-points (bezier-linear-spline (mapv #(mapv + [0 0 (- ST7789-135*240-holder-height)] %) inner-points) steps)
           top-outer-points  (outer-curve steps)
           bottom-outer-points (outer-curve steps :level :bottom)]
    (apply chained-hull-for-four-lists (conj (mapv #(plot-bezier-points % (sphere epsilon))[top-inner-points bottom-inner-points bottom-outer-points top-outer-points]) (inc steps)))))
    
  

(defn mount-body [steps]
  (union (outer-wall-surface steps)))
         
              ;(inner-wall-surface steps)
             ; (tps-43-mount-place (tps-43-mount))
              ;(screen-place ST7789-135*240-holder)
              
  

(defn mount-body-part [steps start-index end-index]
  (let [p 2
        
        top-points (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                    p
                    outer-curve-knot-vector outer-points outer-curve-weights start-index end-index steps)
        bottom-points (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                       p
                       outer-curve-knot-vector (mapv #(mapv + [0 0 (- ST7789-135*240-holder-height)] %) outer-points) 
                       outer-curve-weights start-index end-index steps)]
    (skin [top-points bottom-points])))
  

(defn mount-body-part-1 [steps]
  (let [top-points (conj (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                            2
                            outer-curve-knot-vector outer-points outer-curve-weights 1 3 steps)
                         ST7789-135*240-top-right-position)
        bottom-points (conj (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                             2
                             outer-curve-knot-vector (mapv #(mapv + [0 0 (- ST7789-135*240-holder-height)] %) outer-points)
                             outer-curve-weights 1 3 steps)
                            (mapv + [0 0 (- ST7789-135*240-holder-height)] ST7789-135*240-top-right-position))]
        
    (skin [top-points bottom-points])))
  

(defn mount-body-part-2 [steps]
  (let [top-points (into (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                          2
                          outer-curve-knot-vector outer-points outer-curve-weights 0 0 steps)
                         (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                          2
                          outer-curve-knot-vector outer-points outer-curve-weights 4 6 steps))
        bottom-points (into (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                             2
                             outer-curve-knot-vector (mapv #(mapv + [0 0 (- ST7789-135*240-holder-height)] %) outer-points)
                             outer-curve-weights 0 0 steps)
                            (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                             2
                             outer-curve-knot-vector (mapv #(mapv + [0 0 (- ST7789-135*240-holder-height)] %) outer-points)
                             outer-curve-weights 4 6 steps))]
 
    (skin [top-points bottom-points])))

(defn screw-mount-place [shape &{:keys [translate-fn] :or {translate-fn translate}}] 
  (translate-fn [226.85 198.35 -2] shape))

(def mounting-cylinder-body (translate [0 0 2](binding [*fn* 36] (cylinder insert-holder-radius (+ 11.5 extra-height) :center false))))
(defn screw-mount-place-point [point]
  (screw-mount-place point :translate-fn #(mapv + %1 %2)))

(def main-mount
  
  (let [screen-holder (rdz 180 ST7789-135*240-holder)
        screen-holder-cut (rdz 180 ST7789-135*240-holder-cut)]
    (difference (union 
                 (tps-43-mount-place (tps-43-mount))
                 (difference (screw-mount-place (union (hull (translate bottom-left-hole-position mounting-cylinder-body)
                                                        (translate top-left-hole-position mounting-cylinder-body))
                                                       (translate bottom-right-hole-position mounting-cylinder-body)
                                                      (translate top-right-hole-position mounting-cylinder-body)))
;;  (->> (cube 22 22 11 :center false)
;;      (translate [-11 -11 0])
;;      (translate [224.7 180.25 0]))
                             (screw-mount-place (map #(translate [0 0 2] (translate % (binding [*fn* 36] (cylinder insert-hole-radius hole-depth :center false))))
                                                     [bottom-left-hole-position
                                                      bottom-right-hole-position
                                                      top-left-hole-position
                                                      top-right-hole-position])))
                 (-# (screw-mount-place (translate [5 (nth bottom-left-hole-position 1) 12.5] (cube 46 8 1.5))))
                 (screw-mount-place (translate [4.7 (+ (nth top-left-hole-position 1) 2) 14.5] 
                                               (cuboid [47 6 4] :rounding 2 :edges [(mapv + RIGHT BACK ) (mapv + LEFT BACK)] :fn 36)))
                                 
                 (->> (difference (translate [0 0 0]screen-holder)
                                  (-# (translate [0 0 (- (/ ST7789-135*240-viewport-thickness 2) )]screen-holder-cut)))
                              ;(rdx 20)
                              ;(translate [237.838808 (- 191.4 34.5) 8])
                      (screen-place))
                 (difference
                  (union (mount-body-part 80 2 3)
                         (mount-body-part-1 80)
                         (mount-body-part-2 80))
                  (screen-place (translate [0 0 (- ST7789-135*240-viewport-thickness 0.2)] screen-holder))
                  (screen-place  screen-holder)
                  (tps-43-mount-place (union (tps-43-mount-body :fn 8)
                                             (translate [0 0 -1.5] tps-43-cutout)))))
                (tps-43-mount-place (mirror [1 0 0](-# (union (translate  (mapv + [-8.1 -5.35 -2.25] (rotate-around-z-in-degrees 135 [0 2 0]))
                                                                          (rdz 135 (cube 12 8 2.5)))
                                                        tps-43-breakout-cutout-body)))))))
                                                      
                               

(defn main-mount2 [steps &{:keys [side] :or {side :left}}] 

  (let 
   [cond-mirror  #(if (= side :right) (mirror [1 0 0] %) %)
    trackpad-mount (cond-mirror (tps-43-mount))
    screen-holder (cond-mirror ST7789-135*240-holder)
    cond-bottom-left-hole-position (if (= side  :right) (mapv + [0.2 0 0] bottom-left-hole-position) bottom-left-hole-position)
    bottom-right-short-cyl-body (screw-mount-place (translate (mapv + [0 0 2] bottom-right-hole-position) (binding [*fn* 36] (cylinder insert-holder-radius (+ hole-depth 1) :center false))))]
   
   (cond-mirror (difference (union
                             (tps-43-mount-place trackpad-mount)
                             (difference (union (screw-mount-place (union (union (translate cond-bottom-left-hole-position mounting-cylinder-body)
                                                                           (translate top-left-hole-position mounting-cylinder-body))
                                                                   ;(translate bottom-right-hole-position mounting-cylinder-body)
                                                                    (translate top-right-hole-position mounting-cylinder-body)))
                                                bottom-right-short-cyl-body)
                                 
;;  (->> (cube 22 22 11 :center false)
;;      (translate [-11 -11 0])
;;      (translate [224.7 180.25 0]))
                                         (screw-mount-place (map #(translate [0 0 2] (translate % (binding [*fn* 36] (cylinder insert-hole-radius hole-depth :center false))))
                                                                 [cond-bottom-left-hole-position
                                                                  bottom-right-hole-position
                                                                  top-left-hole-position
                                                                  top-right-hole-position])))
                             (difference 
                              (hull bottom-right-short-cyl-body
                               (screw-mount-place (translate (mapv + [11.6 0 (+ (/ (inc hole-depth) 2) 2)] bottom-right-hole-position) 
                                                             (cube 1.8 (* insert-holder-radius 2) (inc hole-depth)))))
                              (hull (screw-mount-place (translate (mapv + [0 0 2] bottom-right-hole-position) (binding [*fn* 36] (cylinder insert-hole-radius (+ hole-depth 1) :center false))))
                                    (screw-mount-place (translate (mapv + [9.6 0 (+ (/ (inc hole-depth) 2) 2)] bottom-right-hole-position)
                                                                  (cube 1.8 (* insert-hole-radius 2) (inc hole-depth))))))
                             (screw-mount-place (translate (mapv + [11.6
                                                                    0 (+ (/ (+ 11.5 extra-height) 2) 2)] bottom-right-hole-position)
                                                           (cube 1.8 (* insert-holder-radius 2) (+ 11.5 extra-height))))
                    ;(-# (screw-mount-place (translate [5 (nth cond-bottom-left-hole-position 1) 12.5] (cube 46 8 1.5))))
                             (screw-mount-place (translate [4.7 (+ (nth top-left-hole-position 1) 2) (+ 14.5 extra-height)]
                                                           (cuboid [47 6 4] :rounding 2 :edges [(mapv + RIGHT BACK) (mapv + LEFT BACK)] :fn 36)))
                             (->> (difference (translate [0 0 0] screen-holder)
                                              (translate [0 0 (- (/ ST7789-135*240-viewport-thickness 2))] (cond-mirror (rdz 180 ST7789-135*240-holder-cut))))
               ;(rdx 20)
               ;(translate [237.838808 (- 191.4 34.5) 8])
                                  (screen-place))
                             (difference
                              (union (mount-body-part steps 2 3)
                                     (mount-body-part-1 steps)
                                     (mount-body-part-2 steps))
                              (screen-place (translate [0 0 (- ST7789-135*240-viewport-thickness 0.2)] screen-holder))
                              (screen-place  screen-holder)
                              (tps-43-mount-place (union (tps-43-mount-body :fn 8)
                                                         (translate [0 0 -1.5] tps-43-cutout)))))))))
                   ;; (tps-43-mount-place (mirror [1 0 0] (-# (union (cond-mirror (translate  (mapv + [-8.1 -5.35 -2.25] (rotate-around-z-in-degrees 135 [0 2 0]))
                   ;;                                                            (rdz 135 (cube 12 8 2.5))))
                   ;;                                                (cond-mirror tps-43-breakout-cutout-body)))))
                    
(spit "things-low/dilmma-tps-2.scad"
      (write-scad
       (include include-bosl2)
       (let [steps 30
             s-points  (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                        2
                        outer-curve-knot-vector (mapv #(mapv + [0 0 (- ST7789-135*240-holder-height)] %) outer-points)
                        outer-curve-weights 3 3 20) 
             s-points-2  (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                          2
                          outer-curve-knot-vector (mapv #(mapv + [0 0 (- ST7789-135*240-holder-height)] %) outer-points)
                          outer-curve-weights 3 5 steps) 
             s-points-2-floor (mapv #(assoc % 2 0.0) s-points-2)  
             s-points-2-inner  (mapv #(mapv + [(- x-offset) 0 0] %) s-points-2)
             s-points-2-inner-floor (mapv #(assoc % 2 0.0) s-points-2-inner)
             front-points (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                           2
                           outer-curve-knot-vector (mapv #(mapv + [0 0 (- ST7789-135*240-holder-height)] %) outer-points)
                           outer-curve-weights 2 2 steps)
             front-points-floor (mapv #(assoc % 2 0.0) front-points)
             front-points-inner (n-degree-bezier-curve [ ST7789-135*240-bottom-left-position ST7789-135*240-bottom-right-position] steps)
             front-points-inner-floor (mapv #(assoc % 2 0.0) front-points-inner)
             side-points (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                          2
                          outer-curve-knot-vector (mapv #(mapv + [0 0 (- ST7789-135*240-holder-height)] %) outer-points)
                          outer-curve-weights 1 1 steps)
             side-bottom-max-height (last (mapv + [0 0 (- ST7789-135*240-holder-height)] tps-43-bottom-left-position))
             steps-exponent (pow (get-exponent (inc steps)) (inc steps))
             side-points-floor (vec (for [index (range (inc steps))
                                          :let [
                                                index-pc (/ (pow (inc index) 2) (pow (inc steps) 2)  ) 
                                                height (- side-bottom-max-height (* side-bottom-max-height index-pc)) 
                                                ]] 
                                        (assoc (nth side-points index) 2 height)
                                      ))
             side-points-inner (nurbs (mapv #(mapv + [0 0 (- ST7789-135*240-holder-height)] %) [tps-43-bottom-left-position
                                                                                                ST7789-135*240-top-left-position
                                                                                                ST7789-135*240-bottom-left-position])
                                      2
                                      [0 0 0 1 1 1]
                                      [1 (/ (sqrt 2) 2) 1]  steps);(mapv #(mapv + % [1 0 0]) side-points)
             side-points-inner-floor (vec (for [index (range (inc steps))
                                                :let [index-pc (/ (pow (inc index) 2) (pow (inc steps) 2))
                                                      height (- side-bottom-max-height (* side-bottom-max-height index-pc))]]
                                            (assoc (nth side-points-inner index) 2 height)))
             b-point (assoc (mapv + [0  26 0] (nth s-points 0)) 2 0)
             b-point-1 (mapv + [0  26 6] (nth s-points 0))
             b-point-2 (mapv + [0  35 6] (nth s-points 0))
             b-point-3 (mapv + [0  37 6] (nth s-points 0))
             b-point-3-floor (assoc b-point-3 2 0.0) 
             right-cutout-point-top (assoc b-point-3 1 (nth (screw-mount-place-point (mapv + [11.6 (- (/ insert-holder-radius -2) 1.95) (+ (/ (inc hole-depth) 2) 2)] bottom-right-hole-position)) 1))
             right-cutout-point-2 (assoc right-cutout-point-top 2 (inc hole-depth))
             right-cutout-point-floor (assoc right-cutout-point-top 2 0.0)

             b-points [(nth s-points 0) b-point b-point-1 b-point-2 b-point-3]
             catmull-points [ (assoc (nth s-points 0) 2 0.0) b-point b-point-1 b-point-2 b-point-3 (last s-points)]
             outer-wall (vec (for [index (range (inc steps))]
                               (n-degree-bezier-curve 
                                [(nth s-points-2-floor index)
                                 (nth s-points-2 index)
                                 ]
                                1)
                               ))
             inner-wall (vec (for [index (range (inc steps))]
                               (n-degree-bezier-curve
                                [(nth s-points-2-inner index)
                                 (nth s-points-2-inner-floor index)]
                                1)))
             front-outer-wall (vec (for [index (range (inc steps))]
                                     (n-degree-bezier-curve
                                      [(nth front-points-floor index)
                                       (nth front-points index)]
                                      1)))
             front-inner-wall (vec (for [index (range (inc steps))]
                                     (n-degree-bezier-curve
                                      [(nth front-points-inner index)
                                       (nth front-points-inner-floor index)]
                                      1)))
             side-outer-wall (vec (for [index (range (inc steps))]
                                     (n-degree-bezier-curve
                                      [(nth side-points-floor index)
                                       (nth side-points index)]
                                      1)))
             side-inner-wall (vec (for [index (range (inc steps))]
                                     (n-degree-bezier-curve
                                      [(nth side-points-inner index)
                                       (nth side-points-inner-floor index)]
                                      1)))
             cutout-points (nurbs [b-point b-point-1 b-point-2 b-point-3] 2 (mapv #(* % (/ 2 3)) [0 0 0 2 3 3 3]) [1 (/ (sqrt 2) 1.5) 1 1] steps)
             cutout-outer-wall (vec (for [index (range (inc steps))]
                                      (n-degree-bezier-curve 
                                       [b-point-3-floor
                                        (nth cutout-points index)]
                                       1)
                                      ))
             cutout-inner-offset -4
             cutout-points-inner (mapv #(mapv + [cutout-inner-offset 0 0] %) cutout-points)
             b-point-3-floor-inner (mapv + b-point-3-floor [cutout-inner-offset 0 0])
             cutout-inner-wall (vec (for [index (range (inc steps))]
                                      (n-degree-bezier-curve
                                       [(nth cutout-points-inner index)
                                        b-point-3-floor-inner]
                                       1)))
             right-cutout-points (nurbs [b-point-3 right-cutout-point-top right-cutout-point-2 right-cutout-point-floor] 2 (mapv #(* % (/ 2 3)) [0 0 0 2 3 3 3]) [1 (/ (sqrt 2) 2) 1 1] steps)
              right-cutout-outer-wall (vec (for [index (range (inc steps))]
                                       (n-degree-bezier-curve
                                        [b-point-3-floor
                                         (nth right-cutout-points index)]
                                        1)))
             right-cutout-points-inner (mapv #(mapv + [cutout-inner-offset 0 0] %) right-cutout-points)
             right-cutout-inner-wall (vec (for [index (range (inc steps))]
                                      (n-degree-bezier-curve
                                       [(nth right-cutout-points-inner index)
                                        b-point-3-floor-inner]
                                       1)))
             ]
         ;(println "p2" (count p2))
         (println "side-points" (count side-points))
         (difference (union 
          ;            (plot-bezier-points p2  (sphere 1))
                      
        ;;(plot-bezier-points s-points (sphere 1))
        ;;(tps-43-mount-place (translate [(+ (/ tps-43-mount-width 2) x-offset) (+ (/ tps-43-mount-width 2) y-offset) 0] (sphere 1)))
        ;;(tps-43-mount-place (translate [(+ (/ tps-43-mount-width 2) x-offset) (+ (/ tps-43-mount-width -2) (- y-offset)) 0] (sphere 1)))
        ;;(main-mount2 80 :side :right)
                      (main-mount2 80)
                      
                      ;; (-#(screw-mount-place (translate (mapv + [11.6 (- (/ insert-holder-radius -2) 1.8) (+ (/ (inc hole-depth) 2) 2)] bottom-right-hole-position)
                      ;;                               (sphere 1.8))))
                      
          ;;(plot-bezier-points front-points (sphere 1))
          ;; (plot-bezier-points s-points (sphere 1))
          ;; (plot-bezier-points s-points-2 (sphere 1))
          ;; (plot-bezier-points s-points-2-inner (sphere 1))
          ;; (plot-bezier-points s-points-2-floo/(sphere 1))
                      ;; (->> (cylinder 2 (* 2 ST7789-135*240-holder-height))
                      ;;      (binding [*fn* 36])
                      ;;      (translate (mapv + ST7789-135*240-bottom-left-position [1 1 (- ST7789-135*240-holder-height)]))
                      ;;      (-#)
                      ;;      )
                   ;   (vnf-polyhedron (wall-vnf-array  side-outer-wall side-inner-wall) {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse true :style :default})
                      ;(plot-bezier-points (nurbs [(mapv + [-1 1 0] ST7789-135*240-bottom-left-position) (mapv + [-1 -1 0] ST7789-135*240-bottom-left-position) (mapv + [1 -1 0] ST7789-135*240-bottom-left-position)] 2 [0 0 0 1 1 1]  [1 (/ ( sqrt 2) 2) 1] 10) (sphere 0.5))
                      (vnf-polyhedron (wall-vnf-array  side-outer-wall side-inner-wall) {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse true :style :default})
                      
                      (difference (union
                                   (vnf-polyhedron (wall-vnf-array  outer-wall inner-wall) {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse true :style :default})
                                   (vnf-polyhedron (wall-vnf-array  front-outer-wall front-inner-wall) {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse true :style :default})
                                   )
                                  (vnf-polyhedron (wall-vnf-array  cutout-outer-wall cutout-inner-wall) {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default})
                                  (vnf-polyhedron (wall-vnf-array  right-cutout-outer-wall right-cutout-inner-wall) {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
                      
          ;; (->>
          ;;  (difference
          ;;   (union
          ;;    vybronics-vl91022-mount-body
          ;;    holder
          ;;    (translate [-3.5 0 1] (cube 47 14 2)))
          ;;   vybronics-vl91022-mount-body-subtract)
          ;;  (rdx 180)
          ;;  (translate [235 200 12]))
          ;;  (translate b-point (sphere 1))
          ;;  (translate b-point-1 (sphere 1))
          ;; (translate b-point-2 (sphere 1))
          ;;  (translate b-point-3 (sphere 1))
          ;(plot-bezier-points (nurbs [b-point b-point-1 b-point-2 b-point-3] 2 (mapv #(* % (/ 2 3))[0 0 0 2 3 3 3] ) [1 (/ (sqrt 2) 1.5) 1 1] 20) (sphere 1))
          ;(plot-bezier-points (catmull-rom-spline-curve catmull-points 20) (sphere 1))
          ;; (plot-bezier-points b-points (sphere 1))
          ;; (color [1 0 0 1](translate (mapv + [(- x-offset) 0 0](last s-points)) (binding [*fn* 36](sphere 0.2))))
                      (->> (cube 0.6 1.25 1)
                           (translate [-0.7 -0.0 -3])
                           (translate ST7789-135*240-bottom-left-position)
                           (-#))
          ;;             (->>
          ;;              (import "../parts/pj320a.stl")
          ;;              (translate [0 0 -12])
          ;;              (rdz 180)
          ;;              (translate [243.5 172.75 0]))
          ;;  (->> (cylinder 5 20 :center false)
          ;;       (binding [*fn* 36])
          ;;       (rdy 90)
          ;;       (translate [243.5 (-  172.75 0.25) (- 4 1.5)])
          ;;       (-# )
          ;;       )
                      
                      
                      )
          ;; (->> (rounding-edge-mask 45 :r 0.5 :fn 36)
          ;;      (rdy 90)
          ;;      (rdx 0)
          ;;      (translate [0 -1.5 0])
          ;;                ;(rdz 90)
          ;;      (translate (mapv #(/ % 2) (mapv + tps-43-bottom-left-position tps-43-bottom-right-position))))
          (->> (cylinder 1 20)
               (binding [*fn* 36])
               (translate (mapv + tps-43-bottom-left-position [0 -2.25 0]))
               )
          ;; (->> (rounding-edge-mask 20 :r 0.5 :fn 36)
          ;;      (rdz 180)
          ;;      (translate (mapv + ST7789-135*240-top-right-position [1 0 0])))
          (->> (rounding-edge-mask 20 :r 2 :fn 36)
               (rdz 90)
               (translate (mapv + ST7789-135*240-bottom-right-position [1 -1 0])))
          (difference (->> (rounding-edge-mask 20 :r 1 :fn 36)
          
               (translate (mapv + tps-43-bottom-left-position [-1 -1 0]))
               )
                      (->> (cylinder 0.4 ST7789-135*240-holder-height :center false)
                           (binding [*fn* 3])
                           (translate [-0.4 0 (- ST7789-135*240-holder-height)])
                           (rdz -57)
                           (translate (mapv + tps-43-bottom-left-position [-0 -1.1 0]))
                           )
                      )
          (->> (rounding-edge-mask 20 :r 2 :fn 36)
          
               (translate (mapv + ST7789-135*240-bottom-left-position [-1 -1 0]))
               (-#))
          ;; (->> (cylinder 0.4 ST7789-135*240-holder-height :center false)
          
          ;;      (binding [*fn* 3])
          ;;      (translate [-0.4 0 (- ST7789-135*240-holder-height)])
          ;;      (rdz 3)
          ;;      (translate (mapv + tps-43-bottom-left-position [-0 -1.1 0])))
          ))))

(spit "things-low/dilmma-tps-2-with-haptic-mount.scad"
      (write-scad
       (include include-bosl2)
       (let [s-points  (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                        2
                        outer-curve-knot-vector (mapv #(mapv + [0 0 (- ST7789-135*240-holder-height)] %) outer-points)
                        outer-curve-weights 3 3 20)
             b-point (mapv + [0  26 0] (nth s-points 0))
             b-point-1 (mapv + [0  27 6] (nth s-points 0))
             b-point-2 (mapv + [0  35 6] (nth s-points 0))
             b-point-3 (mapv + [0  37 10.5] (nth s-points 0))
             b-points [(nth s-points 0) b-point b-point-1 b-point-2 b-point-3]
             catmull-points [(last s-points) (nth s-points 0) b-point b-point-1 b-point-2 b-point-3 (last s-points)]]
         (union
        ;;(plot-bezier-points s-points (sphere 1))
        ;;(tps-43-mount-place (translate [(+ (/ tps-43-mount-width 2) x-offset) (+ (/ tps-43-mount-width 2) y-offset) 0] (sphere 1)))
        ;;(tps-43-mount-place (translate [(+ (/ tps-43-mount-width 2) x-offset) (+ (/ tps-43-mount-width -2) (- y-offset)) 0] (sphere 1)))
        ;;(main-mount2 80 :side :right)
          (main-mount2 80)

          (->>
           (difference
            (union
             vybronics-vl91022-mount-body
             holder
             (translate [-3.5 0 1] (cube 47 14 2)))
            vybronics-vl91022-mount-body-subtract)
           (rdx 180)
           (translate [235 200 (+ 12 extra-height)]))
         
         
         
          ))))
        ;; (plot-bezier-points (global-curve-interp-with-end-unit-derivatives-curve b-points 2
        ;;                      (mapv - b-point (nth s-points 0) ) (mapv - b-point-3 b-point-2) 20) (sphere 0.5))
           ;(plot-bezier-points (catmull-rom-spline-curve catmull-points 20) (sphere 0.5)) 
          
           
(def main-mount-with-haptic
  (union main-mount
         (->>
          (difference
           (union
            vybronics-vl91022-mount-body
            holder
            (translate [-3.5 0 1](cube 47 14 2)))
            
           vybronics-vl91022-mount-body-subtract)
          (rdx 180)
          (translate [235 200 12]))))
          
         
  
(spit "things-low/dilemma-tps-43-and-screen-mount.scad"
      (write-scad
       (include include-bosl2)
       main-mount))
(spit "things-low/dilemma-tps-43-and-screen-mount-haptic.scad"
      (write-scad
       (include include-bosl2)
       main-mount-with-haptic))
(spit "things-low/dilemma_test.scad"
      (write-scad
       (include include-bosl2)
       (union
        (main-mount2 80)
        (->>
         (import "../parts/azoteq_dilemma_adapter.stl")
         (translate [-86 45 0])
         (rdz 180)
         (rdy 180)
         (translate [224 66 0]))
        ;; (->> (main-mount2 30 :side :right)
        ;;      (rdz 180)
        ;;      (translate [0 253 0]))
        ;; (plot-bezier-points(local-cubic-curve-interpolation-with-calculated-tangents-curve
        ;;                     [(mapv + [(- x-offset)  y-offset 0] tps-43-top-left-position)
        ;;                      ;(mapv + [(- x-offset)  0 0] tps-43-mid-left-position)
        ;;                      (mapv + [(- x-offset)  (- y-offset) 0] tps-43-bottom-left-position)
        ;;                      ;ST7789-135*240-top-left-position
        ;;                      (mapv + [(- x-offset)  (- y-offset) 0] ST7789-135*240-bottom-left-position)
        ;;                      ;(mapv + [0 (- y-offset) 0] ST7789-135*240-bottom-position)
        ;;                      (mapv + [x-offset  (- y-offset) 0] ST7789-135*240-bottom-right-position)
        ;;                      ;(mapv + [x-offset  0 0] ST7789-135*240-mid-right-position)
        ;;                      (mapv + [x-offset  0 0]  ST7789-135*240-top-right-position)
        ;;                      (mapv + [x-offset  y-offset 0]  tps-43-bottom-right-position)
        ;;                      ;(mapv + [x-offset  0 0]  tps-43-mid-right-position)
        ;;                      (mapv + [x-offset  y-offset 0]  tps-43-top-right-position)
        ;;                      ;(mapv + [0  y-offset 0]  tps-43-top-mid-position)
        ;;                      (mapv + [(- x-offset)  y-offset 0] tps-43-top-left-position)]
        ;;                     80)
        ;;   (sphere 1))
        (->> (import "../parts/dilemma.stl")
             (rdx 180)
             (-#))
              
        (->>
         (screw-mount-place (map #(translate [0 0 -2] 
                                            (translate % (binding [*fn* 36] (cylinder hole-radius hole-depth :center false))))
                                [(mapv + [0.2 0 0] bottom-left-hole-position)
                                 bottom-right-hole-position
                                 top-left-hole-position
                                 top-right-hole-position]))
         (mirror [1 0 0])
         (rdz 180)
         (translate [0 253 0]))
       
        ;; (->>
        ;;  (import "../parts/tps-43.stl")
        ;;  (translate [210 172.35 14.5])
        ;;  (-#)) 
        
        ;;  (->> (tps-43-mount)
        ;;       (translate [21.5 20 0])
        ;;       (translate [210 172.35 14.5])
        ;;       )
        

        ;;  (->>
        ;;   vybronics-vl91022-mount
        ;;   (rdx 180)
        ;;   (translate [230 200 12]))
        ;; (->>
        ;;  (import "../parts/top.stl")
        ;;  (rdx 90)
        ;;  (rdz 90)
        ;;  (translate [0 0 0])
        ;;  (translate [242.85 200.35 0])) 
        (->>
         (import "../parts/tps-65.stl")
         (translate [-32.5 -24.5 0])
         (rdz 90)
         (translate [24.5 32.5 0])
         (translate [210 158.35 16])
         (color [0 0 1 1]))
           (->>
         (import "../parts/tps-65.stl")
         (translate [-32.5 -24.5 0])
                 ;o(rdz 90)
         (translate [24.5 32.5 0])
         (translate [218 162.35 16])
         (color [0 1 0 1]))
        ;;  (->> ST7789-135*240
        ;;      (translate [238 142 4]))
        ;; (->> ST7789-135*240
        ;;      (rdx 15)
        ;;      (translate [228 215 ]))
        
        ;; (->>
        ;;  (import "../parts/ST7789.stl")
        ;;  (translate [228 215 4])
        ;(plot-bezier-points left-curve-points (sphere 2))
        ;(plot-bezier-points right-curve-points (sphere 2))
        
        ;; (->>
        ;;  (import "../parts/vl91022-160-320h.stl")
        ;;  (translate [0 0 4.5])
        ;;  (rdy 90)
        ;;  (rdz 90)
        ;;  (translate [235 200 8]))
        (mapv #(->>
                (import "../parts/Kailh Polia.stl")
                (rdz 90)
                (translate [197.8 191.4 2])
                (-#)
                (translate [0 % 0]))
              [0 19 -19])
        (->>
         (union (import "../parts/Kailh Polia.stl")
                (translate [0 0 6.5] (import "../parts/caps/DES-R2.stl")))
         (rdz ( + 90 150)) 
         (translate [207.838808 (- 191.4 47.55) 2])
         (-#))
         
        
        (->>
         (import "../parts/Tps-65-breakout.stl")
         (translate [-131.9 66.5 0])
         (translate [224.7 180.25 2.54])
         (-#))
        (->> 
         (import "../parts/2305 DRV2605L.stl")
         (translate [217.5 173 (+ (* 2.54 2) 1.6)]))
         
       
        ;; (->>
        ;;  (import "../parts/pim452.stl")
        ;;  (translate [230 190 0]))
        ;; (->> ST7789-135*240
        ;;      ;(rdz (+ 150)) 
        ;;      ;(rdx 20)
        ;;      ;(translate [237.838808 (- 191.4 34.5) 6])
        ;;      (screen-place)
        ;;      ;(translate [228 215 4])
        ;;      )
        ;(plot-bezier-points (outer-curve) (color [0 1 0 1] (sphere 1)))
        ;(plot-bezier-points (outer-curve :level :bottom) (color [0 0 1 1] (sphere 1)))
        ;(vnf-polyhedron (mount-faces 80))
        ;(mount-faces-hull 80)
        
        
        
        (->> 
         (import "../parts/pj320a.stl")
         (translate [0 0 -12])
         (rdz 180)
         (translate [243.5 172.75 0])) 
        (mapv #(->>
                (import "../parts/caps/DES-R2.stl")
         ;(rdz 90)
                (translate [197.8 191.4 6.5])
                (-#)
                (translate [0 % 0]))
               [0 19 -19])
        ))) 
        
        ;; (screw-mount-place (map #(-# (translate % hole)) [bottom-left-hole-position 
        ;;                                                              bottom-right-hole-position
        ;;                                                              top-left-hole-position
        ;;                                                              top-right-hole-position]))
        
        



(comment (- 191.4 61.7475))
(comment (- 109.2975 61.7475))