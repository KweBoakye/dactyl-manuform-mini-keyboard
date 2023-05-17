(ns dactyl-keyboard.klor.klor-case-functions
  (:refer-clojure :exclude [use import])
  (:require [clojure.math :refer [sqrt]]
            [dactyl-keyboard.klor.klor-config :refer :all]
            [dactyl-keyboard.klor.klor-constants :refer :all]
            [dactyl-keyboard.klor.klor-placement-functions :refer :all]
            [dactyl-keyboard.klor.klor-points :refer :all]
            [dactyl-keyboard.lib.curvesandsplines.beziers :refer [n-degree-bezier-curve]]
            [dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline :refer [nurbs-with-calculated-knot-vector]]
            [dactyl-keyboard.utils :refer [select-values]])
  )


(defn klor-wall-locate1 [dx dy] [(* dx klor-wall-thickness) (* dy klor-wall-thickness) -1])



(defn klor-wall-locate2-for-polyhedron-point ([dx dy] (klor-wall-locate2-for-polyhedron-point dx dy [0 0 0]))
  ([dx dy orig-point] (klor-wall-locate2-for-polyhedron-point dx dy orig-point klor-wall-xy-offset))
  ([dx dy orig-point xy] (mapv + orig-point [(* dx xy) (* dy xy) klor-wall-z-offset])))

(defn klor-wall-locate-1-to-3-curve-for-polyhedron-control-point ([dx dy] (klor-wall-locate-1-to-3-curve-for-polyhedron-control-point dx dy [0.0 0.0 0.0]))
  ([dx dy orig-point] (mapv + orig-point [(* (double dx) (double klor-wall-thickness)) (* (double dy) (double klor-wall-thickness)) 0.0])))

(defn klor-wall-locate-1-to-3-curve-for-polyhedron-second-control-point
  ([dx dy] (klor-wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx dy [0 0 0]))
  ([dx dy orig-point] (klor-wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx dy orig-point klor-wall-xy-offset))
  ([dx dy orig-point xy] (mapv + [(* dx (+ xy  klor-wall-thickness))
                                  (* dy (+ xy  klor-wall-thickness))
                                  (- klor-wall-z-offset 0)] orig-point)))
(defn klor-wall-locate3-for-polyhedron-point ([dx dy] (klor-wall-locate3-for-polyhedron-point dx dy klor-wall-xy-offset))
  ([dx dy xy] (klor-wall-locate3-for-polyhedron-point dx dy xy [0 0 0]))
  ([dx dy xy orig-point]
   (mapv +
         [(* dx (+ xy klor-wall-thickness klor-case-offset))
          (* dy (+ xy klor-wall-thickness klor-case-offset))
          (+ 0.5 (- klor-wall-z-offset klor-switchplate-thickness))]
         orig-point)))

(defn get-klor-dx-dy [direction]
  (case direction
    :north  {:dx 0 :dy 1}
    :north-east {:dx 1 :dy 1}
    :east  {:dx 1 :dy 0}
    :south-east {:dx 1 :dy -1}
    :south   {:dx 0 :dy -1}
    :south-west {:dx -1 :dy -1}
    :west {:dx -1 :dy 0}
    :north-west {:dx -1 :dy 1}))

(defn get-position-offset [direction]
  (case direction
    :north [0 klor-case-offset 0]
    :north-east [klor-case-offset klor-case-offset 0]
    :east [klor-case-offset 0 0]
    :south-east [klor-case-offset (- klor-case-offset)  0]
    :south [0 (- klor-case-offset) 0]
    :south-west [(- klor-case-offset) (- klor-case-offset) 0]
    :west [(- klor-case-offset) 0 0]
    :north-west [(- klor-case-offset) klor-case-offset 0]))

(defn klor-wall-control-points [place corner offset direction
                                & {:keys [height dx-and-dy xy position-offset
                                          outer-spacing-fn inner-spacing-fn]
                                   :or {height klor-case-walls-height dx-and-dy (get-klor-dx-dy direction)
                                        xy klor-wall-xy-offset  position-offset (get-position-offset direction)
                                        outer-spacing-fn klor-spacing inner-spacing-fn klor-inner-spacing}}]
  (let [{dx :dx
         dy :dy} dx-and-dy
        corner-outer (mapv + (outer-spacing-fn corner) offset [0 0 height] position-offset)
        corner-inner (mapv + (inner-spacing-fn corner) offset [0 0 height] position-offset)
        wall-top-outer (place corner-outer)
        wall-top-inner (place corner-inner)
        wall-locate1-point (place (mapv + (klor-wall-locate1 dx dy) corner-outer))
        wall-locate-1-to-3-curve-for-polyhedron-control-point-point (place (mapv + (klor-wall-locate-1-to-3-curve-for-polyhedron-control-point dx dy corner-outer)))
        wall-locate-1-to-3-curve-for-polyhedron-second-control-point  (place (mapv + (klor-wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx dy corner-outer xy)))
        wall-locate3-point (place (mapv + (klor-wall-locate3-for-polyhedron-point dx dy xy corner-outer)))
        wall-locate3-point-floor (assoc wall-locate3-point 2 0.0)
        wall-locate-2-top (place (klor-wall-locate2-for-polyhedron-point dx dy corner-inner))
        wall-locate-2-bottom (place (klor-wall-locate2-for-polyhedron-point dx dy (mapv + corner-inner [0 0 (- klor-switchplate-thickness)])))
        wall-locate-2-bottom-floor (assoc wall-locate-2-bottom 2 0.0)
        wall-bottom-inner (assoc wall-top-inner 2 0.0)]
    
    {:wall-top-outer wall-top-outer
     :wall-locate1-point wall-locate1-point
     :wall-locate-1-to-3-curve-for-polyhedron-control-point-point wall-locate-1-to-3-curve-for-polyhedron-control-point-point
     :wall-locate-1-to-3-curve-for-polyhedron-second-control-point wall-locate-1-to-3-curve-for-polyhedron-second-control-point
     :wall-locate3-point wall-locate3-point
     :wall-locate3-point-floor wall-locate3-point-floor
     :wall-top-inner wall-top-inner
     :wall-locate-2-top wall-locate-2-top
     :wall-locate-2-bottom wall-locate-2-bottom
     :wall-locate-2-bottom-floor wall-locate-2-bottom-floor
     :wall-bottom-inner wall-bottom-inner}))
(defn klor-main-body-wall-points [column row corner offset direction & {:keys [height xy]
                                                                        :or {height klor-case-walls-height xy klor-wall-xy-offset}}]
  (klor-wall-control-points (partial klor-key-position column row) corner offset direction
                            :height height :xy xy)
  )
(defn klor-thumb-wall-control-points [column corner offset direction & {:keys [height xy] :or {height klor-thumb-walls-height xy klor-wall-xy-offset}}]
  (klor-wall-control-points (partial klor-thumb-position column) corner offset direction :height height :xy xy))

(defn klor-pcb-top-right-corner-control-points [direction & {:keys [offset height xy dx-and-dy  position-offset]
                                                             :or {offset [4 -2.5 0] height klor-case-walls-height
                                                                  dx-and-dy (update (get-klor-dx-dy direction) :dy #(* -1 %))
                                                                  xy klor-wall-xy-offset
                                                                  position-offset (update (get-position-offset direction) 1 #(* -1 %))}}]
  ;; (klor-wall-control-points (partial klor-point-place) :tr (mapv + offset pcb-top-right-corner (mapv * [-1 -1 0] (klor-spacing :tr))) direction
  ;;                           :height height :dx-and-dy dx-and-dy :xy xy :position-offset position-offset)
  (klor-main-body-wall-points  2 2 :tr [46.5 -1 0] direction))

(defn klor-oled-position-control-points [corner direction
                                         & {:keys [height xy offset]
                                            :or {height klor-case-walls-height xy klor-wall-xy-offset offset [0 0 0]}}]
  (klor-wall-control-points (partial klor-oled-position) corner offset direction :height height
                            :outer-spacing-fn oled-holder-spacing :inner-spacing-fn oled-holder-spacing-inner
                            :xy xy))

(defn klor-tps-43-wall-control-points [corner direction
                                       & {:keys [height xy offset]
                                          :or {height klor-case-walls-height xy klor-wall-xy-offset offset [0 0 0]}}]
  (klor-wall-control-points (partial klor-tps-43-position) corner offset direction :height height
                            :outer-spacing-fn tps-43-mount-spacing :inner-spacing-fn tps-43-mount-inner-spacing
                            :xy xy))

(def klor-outer-wall-vertical-nurbs-control-points-keywords [:wall-top-inner :wall-top-outer :wall-locate1-point :wall-locate-1-to-3-curve-for-polyhedron-second-control-point
                                                             :wall-locate3-point :wall-locate3-point-floor])

(defn klor-outer-wall-vertical-nurbs [all-control-points steps &{:keys [degree control-point-keywords weights
                                                                               point-paramater-calculation-method knot-vector-generation-method] :or 
                                                     {degree 3 control-point-keywords  klor-outer-wall-vertical-nurbs-control-points-keywords
                                                      weights [1 1 (/ (sqrt 2) 2) 0.6 0.75 1]
                                                      point-paramater-calculation-method :dynamic-centripetal
                                                      knot-vector-generation-method :natural}}] 
  (nurbs-with-calculated-knot-vector (select-values all-control-points control-point-keywords) degree
                                     weights 
                                   steps :point-paramater-calculation-method point-paramater-calculation-method
                                   :knot-vector-generation-method knot-vector-generation-method))

(defn klor-inner-wall-vertical-linear [all-control-points steps]
  (n-degree-bezier-curve (select-values all-control-points [:wall-top-inner :wall-bottom-inner]) steps)
  )

(defmulti klor-wall-control-points-from-map (fn [data-map] (:type data-map)))

(defmethod  klor-wall-control-points-from-map :main-body 
  [data-map]
  (let [{:keys [column row corner offset direction height xy]
         :or {height klor-case-walls-height xy klor-wall-xy-offset}} data-map]
    (klor-main-body-wall-points column row corner offset direction :height height :xy xy)
    ))

(defmethod  klor-wall-control-points-from-map :thumb
  [data-map]
  (let [{:keys [column corner offset direction height xy]
         :or {height klor-thumb-walls-height xy klor-wall-xy-offset}} data-map]
    (klor-thumb-wall-control-points column corner offset direction :height height :xy xy)))

(defmethod klor-wall-control-points-from-map :pcb-top-right 
  [data-map]
  (let [{:keys [direction]} data-map]
(klor-pcb-top-right-corner-control-points direction)
    )
  )

(defmethod  klor-wall-control-points-from-map :oled
  [data-map]
  (let [{:keys [corner direction height xy offset]
         :or {height klor-case-walls-height xy klor-wall-xy-offset offset [0 0 0]}} data-map]
    (klor-oled-position-control-points corner direction :height height :xy xy :offset offset)))

(defmethod klor-wall-control-points-from-map :tps-43 
  [data-map]
  (let [{:keys [corner direction height xy offset]
         :or {height klor-case-walls-height xy klor-wall-xy-offset offset [0 0 0]}} data-map]
    (klor-tps-43-wall-control-points corner direction :height height :xy xy :offset offset)
    )
  )


(defn outer-wall-tangent-vector [tangent-vector-end-point tangent-vector-start-point 
                                 &{:keys [tangent-vector-end-point-curve-fn tangent-vector-start-point-curve-fn]
                                   :or {tangent-vector-end-point-curve-fn (fn [all-control-points steps]
                                                                            (klor-outer-wall-vertical-nurbs all-control-points steps))
                                        tangent-vector-start-point-curve-fn (fn [all-control-points steps]
                                                                              (klor-outer-wall-vertical-nurbs all-control-points steps))}}])