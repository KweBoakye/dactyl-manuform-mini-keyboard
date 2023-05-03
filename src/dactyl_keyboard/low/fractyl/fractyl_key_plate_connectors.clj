(ns dactyl-keyboard.low.fractyl.fractyl-key-plate-connectors 
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [magnitude mul]]
            [dactyl-keyboard.lib.constants :refer [epsilon]]
            [dactyl-keyboard.lib.curvesandsplines.beziers :refer [n-degree-bezier-curve]]
            [dactyl-keyboard.lib.curvesandsplines.coons-surface :refer [bicubic-coons-surface]]
            [dactyl-keyboard.lib.curvesandsplines.curve-fitting :refer [global-curve-interp-with-calculated-first-derivatives
                                                                        global-curve-interp-with-end-unit-derivatives-curve]]
            [dactyl-keyboard.lib.curvesandsplines.linear-surface :refer [lofted-surface]]
            [dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline :refer [decompose-b-spline-curve decompose-b-spline-curve-and-calculate-bezier-curves
                                                                               get-function-for-u-k-values non-uniform-b-spline
                                                                               total-chord-length-derivative-magnitude-estimation]]
            [dactyl-keyboard.lib.curvesandsplines.splines :refer [catmull-rom-spline-curve]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [include-bosl2]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vnf :refer [default-vnf-vertex-array-args vnf-polyhedron]]
            [dactyl-keyboard.lib.openscad.hull :refer [chained-hull-for-four-lists]]
            [dactyl-keyboard.low.case-low-polyhedron-functions :refer [calculate-control-points key-wall-position main-body-web-post-point-bottom
                                                                       main-body-web-post-point-top thumb-web-post-point-bottom thumb-web-post-point-top
                                                                       tps-65-wall-position wall-vnf-array web-post-point-bottom web-post-point-top]]
            [dactyl-keyboard.low.placement-functions-low :refer :all]
            [dactyl-keyboard.low.screen-holder-placement-points :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.low.thumbs-low :refer :all]
            [dactyl-keyboard.low.tps-65-placement-functions :refer :all]
            [dactyl-keyboard.low.tps-65-placement-points :refer :all]
            [dactyl-keyboard.tps-65 :refer [tps-65 tps-65-mount-cutout
                                            tps-65-mount-main-cutout tps-65-mount-new]]
            [dactyl-keyboard.utils :refer [plot-bezier-points]]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))


(defn fractyl-row-connecters [row steps &{:keys [degree 
                                                      point-paramater-calculation-method knot-vector-generation-method
                                                      magnitude-estimation-method
                                                  bottom-first-deriv]
                                               :or {degree 2
                                                    point-paramater-calculation-method :centripetal
                                                    knot-vector-generation-method :average
                                                    magnitude-estimation-method :arc
                                                    bottom-first-deriv false}}]
  (let [P-u-zero-coll-outer-points (vec (apply concat (for [col (range (inc lastcol))]
                                                               [(main-body-web-post-point-top col row :tl)
                                                        ;(main-body-web-post-point-top col 0 :bm)
                                                                (main-body-web-post-point-top col row :tr)]))) 
        P-u-zero-coll-outer-tangents  (vec (apply concat (for [col (range (inc lastcol))]
                                                 [(mapv - (main-body-web-post-point-top col 0 :tm) (main-body-web-post-point-top col 0 :tl))
                                                          ;(mapv - (main-body-web-post-point-top col 0 :br) (main-body-web-post-point-top col 0 :bm))
                                                  (mapv - (main-body-web-post-point-top col 0 :tr) (main-body-web-post-point-top col 0 :tm))])))
        P-u-zero-coll-outer-params (global-curve-interp-with-calculated-first-derivatives
                                    P-u-zero-coll-outer-points
                                    P-u-zero-coll-outer-tangents
                                    degree
                                    :point-paramater-calculation-method point-paramater-calculation-method
                                    :magnitude-estimation-method magnitude-estimation-method)
        P-u-zero-coll-outer-global (vec (for [index (range lastcol)]
               (decompose-b-spline-curve-and-calculate-bezier-curves degree (:U P-u-zero-coll-outer-params) (:P P-u-zero-coll-outer-params) (+ (* index 4) 2) (+ (* index 4) 3) steps)))
        P-u-zero-coll-outer-catmull (vec (for [col (range lastcol)]
                                   (catmull-rom-spline-curve
                                    [(main-body-web-post-point-top col row :tl)
                                     (main-body-web-post-point-top col row :tr)
                                     (main-body-web-post-point-top (inc col) row :tl)
                                     (main-body-web-post-point-top (inc col) row :tr)]
                                    steps)))
        P-u-zero-coll-outer (if (zero? row) P-u-zero-coll-outer-catmull P-u-zero-coll-outer-global)
        P-u-one-coll-outer-points (vec (apply concat (for [col (range (inc lastcol))]
                                                       [(main-body-web-post-point-top col row :bl)
                                                        ;(main-body-web-post-point-top col 0 :bm)
                                                        (main-body-web-post-point-top col row :br) 
                                                        ])))
        P-u-one-coll-outer-tangents (vec (apply concat (for [col (range (inc lastcol))]
                                                         [(mapv - (main-body-web-post-point-top col row :bm) (main-body-web-post-point-top col row :bl))
                                                          ;(mapv - (main-body-web-post-point-top col 0 :br) (main-body-web-post-point-top col 0 :bm))
                                                          (mapv - (main-body-web-post-point-top col row :br) (main-body-web-post-point-top col row :bm))])))
        P-u-one-coll-outer-params (global-curve-interp-with-calculated-first-derivatives
                                   P-u-one-coll-outer-points
                                   P-u-one-coll-outer-tangents
                                   degree 
                                   :point-paramater-calculation-method point-paramater-calculation-method
                                   :magnitude-estimation-method magnitude-estimation-method)
        P-u-one-coll-outer-control-points (:P P-u-one-coll-outer-params)
        P-u-one-coll-outer-knot-vector (:U P-u-one-coll-outer-params) 
        P-u-one-coll-outer (vec (for [index (range lastcol)]
                                  (decompose-b-spline-curve-and-calculate-bezier-curves degree P-u-one-coll-outer-knot-vector P-u-one-coll-outer-control-points (+ (* index 4) 2) (+ (* index 4) 3) steps))) 
        ;; (vec (for [col (range lastcol)]
        ;;                           (global-curve-interp-with-end-unit-derivatives-curve
        ;;                            [(main-body-web-post-point-top col 0 :br)
        ;;                             (main-body-web-post-point-top (inc col) 0 :bl)]
        ;;                            degree
        ;;                            (main-body-web-post-point-top col 0 :bl)
        ;;                            (main-body-web-post-point-top (inc col) 0 :br)
        ;;                            steps
        ;;                            :point-paramater-calculation-method point-paramater-calculation-method
        ;;                            :knot-vector-generation-method knot-vector-generation-method
        ;;                            :magnitude-estimation-method magnitude-estimation-method
        ;;                            :tangent-endpoints true)))
        P-u-zero-coll-inner (vec (for [col (range lastcol)]
                                   (global-curve-interp-with-end-unit-derivatives-curve
                                    [(main-body-web-post-point-bottom col row :br)
                                     (main-body-web-post-point-bottom (inc col) row :bl)]
                                    degree
                                    (main-body-web-post-point-bottom col row :bl)
                                    (main-body-web-post-point-bottom (inc col) row :br)
                                    steps
                                    :point-paramater-calculation-method :chordal
                                    :knot-vector-generation-method knot-vector-generation-method
                                    :magnitude-estimation-method magnitude-estimation-method
                                    :tangent-endpoints true)))
        P-u-zero-coll-inner-points (vec (apply concat (for [col (range (inc lastcol))]
                                                        [(main-body-web-post-point-bottom col row :bl)
                                                         (main-body-web-post-point-bottom col row :br)])))
        P-u-zero-coll-inner-tangents (vec (apply concat (for [col (range (inc lastcol))]
                                                          [(mapv - (main-body-web-post-point-bottom col row :br) (main-body-web-post-point-bottom col row :bl))
                                                           (mapv - (main-body-web-post-point-bottom col row :br) (main-body-web-post-point-bottom col row :bm))])))
        P-u-zero-coll-inner-params (global-curve-interp-with-calculated-first-derivatives
                                    P-u-zero-coll-inner-points
                                    P-u-zero-coll-inner-tangents
                                    degree
                                    :point-paramater-calculation-method point-paramater-calculation-method
                                    :magnitude-estimation-method magnitude-estimation-method)
        P-u-zero-coll-inner-control-points (:P P-u-zero-coll-inner-params)
        P-u-zero-coll-inner-knot-vector (:U P-u-zero-coll-inner-params)
        P-u-zero-coll-inner-2 (vec (for [index (range lastcol)]
                                     (decompose-b-spline-curve-and-calculate-bezier-curves degree P-u-zero-coll-inner-knot-vector P-u-zero-coll-inner-control-points (+ (* index 4) 2) (+ (* index 4) 3) steps :reverse-curve false)))
        P-u-one-coll-inner-points (vec (apply concat (for [col (range (inc lastcol))]
                                                        [(main-body-web-post-point-bottom col row :tl)
                                                        ;(main-body-web-post-point-top col 0 :bm)
                                                         (main-body-web-post-point-bottom col row :tr)])))
P-u-one-coll-inner-tangents  (vec (apply concat (for [col (range (inc lastcol))]
                                                   [(mapv - (main-body-web-post-point-bottom col 0 :tm) (main-body-web-post-point-bottom col 0 :tl))
                                                          ;(mapv - (main-body-web-post-point-top col 0 :br) (main-body-web-post-point-top col 0 :bm))
                                                    (mapv - (main-body-web-post-point-bottom col 0 :tr) (main-body-web-post-point-bottom col 0 :tm))])))
P-u-one-coll-inner-params (global-curve-interp-with-calculated-first-derivatives
                            P-u-one-coll-inner-points
                            P-u-one-coll-inner-points
                            degree
                            :point-paramater-calculation-method point-paramater-calculation-method
                            :magnitude-estimation-method magnitude-estimation-method)
P-u-one-coll-inner-global (vec (for [index (range lastcol)]
                                  (decompose-b-spline-curve-and-calculate-bezier-curves degree (:U P-u-one-coll-inner-params) (:P P-u-one-coll-inner-params) (+ (* index 4) 2) (+ (* index 4) 3) steps)))
        P-u-one-inner-linear (vec (for [col (range (inc lastcol))]
                                    (n-degree-bezier-curve [(main-body-web-post-point-bottom col row :tr)
                                                        ;(main-body-web-post-point-top col 0 :bm)
                                     (main-body-web-post-point-bottom (inc col) row :tl)]
                                                           steps)))
        P-u-one-coll-inner (if (zero? row) 
                             (vec
                            (for [col (range lastcol)]
                              (catmull-rom-spline-curve
                               [(main-body-web-post-point-bottom col row :tl)
                                (main-body-web-post-point-bottom col row :tr)
                                (main-body-web-post-point-bottom (inc col) row :tl)
                                (main-body-web-post-point-bottom (inc col) row :tr)]
                               steps)))
                               P-u-one-coll-inner-global) 
        connecter-outer-surfaces (vec
                                  (for [index (range lastcol)]
                                    (lofted-surface  (nth P-u-zero-coll-outer index) (nth P-u-one-coll-outer index) steps steps)))
        connecter-inner-surfaces
        (vec
         (for [index (range lastcol)]
           (lofted-surface  (nth P-u-zero-coll-inner index) (nth P-u-one-inner-linear index) steps steps)))
        connecter-polyhedrons (for [index (range lastcol)]
                                (vnf-polyhedron (wall-vnf-array (nth connecter-outer-surfaces index)
                                                                (nth connecter-inner-surfaces index))))]
    (union connecter-polyhedrons
           

          
           ;(plot-bezier-points (decompose-b-spline-curve-and-calculate-bezier-curves degree pp-U pp-P 0 1 steps) (sphere 0.5))
           ;(plot-bezier-points (nth P-u-one-coll-outer 2) (sphere 1))
           )))

(defn fractyl-col-to-col-connecter-curves [col-to-left row top-or-bottom steps
                                    & {:keys [degree
                                              outer-point-paramater-calculation-method
                                              inner-point-paramater-calculation-method
                                              knot-vector-generation-method
                                              magnitude-estimation-method
                                              outer-catmull-rom-alpha
                                              inner-catmull-rom-alpha 
                                              outer-fn
                                              inner-fn]
                                       :or {degree 2
                                            outer-point-paramater-calculation-method :centripetal
                                            inner-point-paramater-calculation-method :centripetal
                                            knot-vector-generation-method :average
                                            magnitude-estimation-method :arc
                                            outer-catmull-rom-alpha  :centripetal
                                            inner-catmull-rom-alpha :centripetal
                                            outer-fn :global
                                            inner-fn :global}}] 
  (let [left-position-fn (fn [top-or-bottom]
                           (case top-or-bottom
                             :top :tl
                             :bottom :bl))
        middle-position-fn (fn [top-or-bottom]
                             (case top-or-bottom
                               :top :tm
                               :bottom :bm))
        right-position-fn (fn [top-or-bottom]
                            (case top-or-bottom
                              :top :tr
                              :bottom :br))
        get-web-position-fn (fn [inner-or-outer]
                              (case inner-or-outer
                                :outer main-body-web-post-point-top
                                :inner main-body-web-post-point-bottom))
        point-paramater-calculation-method (fn [inner-or-outer]
                                             (case inner-or-outer
                                               :outer outer-point-paramater-calculation-method
                                               :inner inner-point-paramater-calculation-method))
        catmull-rom-alpha (fn [inner-or-outer]
                            (case inner-or-outer
                              :outer outer-catmull-rom-alpha
                              :inner inner-catmull-rom-alpha))
        
        global-end-deriv-fn (fn [top-or-bottom inner-or-outer steps]
                              
                              (let [left-position (left-position-fn top-or-bottom) 
                                    right-position (right-position-fn top-or-bottom)
                                    web-position-fn (get-web-position-fn inner-or-outer)]
                                (global-curve-interp-with-end-unit-derivatives-curve
                             [(web-position-fn col-to-left row right-position)
                              (web-position-fn (inc col-to-left) row left-position)]
                             degree
                             (web-position-fn col-to-left row left-position)
                             (web-position-fn (inc col-to-left) row right-position)
                             steps
                             :point-paramater-calculation-method (point-paramater-calculation-method inner-or-outer)
                             :knot-vector-generation-method knot-vector-generation-method
                             :magnitude-estimation-method magnitude-estimation-method
                             :tangent-endpoints true)))
        global-first-deriv-fn (fn [top-or-bottom inner-or-outer steps]
                                (let [left-position (left-position-fn top-or-bottom)
                                      middle-position (middle-position-fn top-or-bottom)
                                      right-position (right-position-fn top-or-bottom)
                                      web-position-fn (get-web-position-fn inner-or-outer)
                                      outer-points (vec (apply concat (for [col (range (inc lastcol))]
                                                                        [(web-position-fn col row left-position)
                                                                         (web-position-fn col row right-position)])))
                                      outer-tangents (vec (apply concat (for [col (range (inc lastcol))]
                                                                          [(mapv - (web-position-fn col row middle-position) (web-position-fn col row left-position))
                                                                           (mapv - (web-position-fn col row right-position) (web-position-fn col row middle-position))])))
                                      params (global-curve-interp-with-calculated-first-derivatives
                                              outer-points
                                              outer-tangents
                                              degree
                                              :point-paramater-calculation-method (point-paramater-calculation-method inner-or-outer)
                                              :magnitude-estimation-method magnitude-estimation-method
                                              :knot-vector-generation-method knot-vector-generation-method)]
                                  (decompose-b-spline-curve-and-calculate-bezier-curves degree (:U params) (:P params) (+ (* col-to-left 4) 2) (+ (* col-to-left 4) 3) steps)))
        catmull-rom-spline-curve-fn (fn [top-or-bottom inner-or-outer steps]
                                      (let [left-position (left-position-fn top-or-bottom)
                                            middle-position (middle-position-fn top-or-bottom)
                                            right-position (right-position-fn top-or-bottom)
                                            web-position-fn (get-web-position-fn inner-or-outer)]
                                        (catmull-rom-spline-curve
                                         [(web-position-fn col-to-left row left-position)
                                          (web-position-fn col-to-left row right-position)
                                          (web-position-fn (inc col-to-left) row left-position)
                                          (web-position-fn (inc col-to-left) row (if (and (= col-to-left (dec lastcol) ) (zero? row)) middle-position right-position))]
                                         steps
                                         :alphaType (catmull-rom-alpha inner-or-outer))))
        linear-fn (fn [top-or-bottom inner-or-outer steps]
                    (let [left-position (left-position-fn top-or-bottom) 
                          right-position (right-position-fn top-or-bottom)
                          web-position-fn (get-web-position-fn inner-or-outer)
                          ]
                      (n-degree-bezier-curve [(web-position-fn col-to-left row right-position)
                                              (web-position-fn (inc col-to-left) row left-position)] steps)))
        curve-fn (fn [top-or-bottom inner-or-outer curve-type steps]
                   (case curve-type
                     :global (global-first-deriv-fn top-or-bottom inner-or-outer steps)
                     :global-end (global-end-deriv-fn top-or-bottom inner-or-outer steps)
                     :catmull (catmull-rom-spline-curve-fn top-or-bottom inner-or-outer steps)
                     :linear (linear-fn top-or-bottom inner-or-outer steps)
                      (curve-type steps)))
        outer-curve (curve-fn top-or-bottom :outer outer-fn steps) 
        inner-curve (curve-fn top-or-bottom :inner inner-fn steps) 
        ]
    {:outer-curve outer-curve
     :inner-curve inner-curve}
    ))

;; (defn fractyl-col-to-col-connecter [col-to-left row steps
;;                                     & {:keys [degree
;;                                               point-paramater-calculation-method knot-vector-generation-method
;;                                               magnitude-estimation-method
;;                                               catmull-rom-alpha
;;                                               top-outer-fn
;;                                               top-inner-fn
;;                                               bottom-outer-fn
;;                                               bottom-inner-fn]
;;                                        :or {degree 2
;;                                             point-paramater-calculation-method :centripetal
;;                                             knot-vector-generation-method :average
;;                                             magnitude-estimation-method :arc
;;                                             catmull-rom-alpha :centripetal
;;                                             top-outer-fn :global
;;                                             top-inner-fn :global
;;                                             bottom-outer-fn :global
;;                                             bottom-inner-fn :global}}]
;;   (let [left-position-fn (fn [top-or-bottom]
;;                            (case top-or-bottom
;;                              :top :tl
;;                              :bottom :bl))
;;         middle-position-fn (fn [top-or-bottom]
;;                              (case top-or-bottom
;;                                :top :tm
;;                                :bottom :bm))
;;         right-position-fn (fn [top-or-bottom]
;;                             (case top-or-bottom
;;                               :top :tr
;;                               :bottom :br))
;;         get-web-position-fn (fn [inner-or-outer]
;;                               (case inner-or-outer
;;                                 :outer main-body-web-post-point-top
;;                                 :inner main-body-web-post-point-bottom))
;;         global-first-deriv-fn (fn [top-or-bottom inner-or-outer steps]
;;                                 (let [left-position (left-position-fn top-or-bottom)
;;                                       middle-position (middle-position-fn top-or-bottom)
;;                                       right-position (right-position-fn top-or-bottom)
;;                                       web-position-fn (get-web-position-fn inner-or-outer)
;;                                       outer-points (vec (apply concat (for [col (range (inc lastcol))]
;;                                                                         [(web-position-fn col row left-position)
;;                                                                          (web-position-fn col row right-position)])))
;;                                       outer-tangents (vec (apply concat (for [col (range (inc lastcol))]
;;                                                                           [(mapv - (web-position-fn col row middle-position) (web-position-fn col row left-position))
;;                                                                            (mapv - (web-position-fn col row right-position) (web-position-fn col row middle-position))])))
;;                                       params (global-curve-interp-with-calculated-first-derivatives
;;                                               outer-points
;;                                               outer-tangents
;;                                               degree
;;                                               :point-paramater-calculation-method point-paramater-calculation-method
;;                                               :magnitude-estimation-method magnitude-estimation-method
;;                                               :knot-vector-generation-method knot-vector-generation-method)]
;;                                   (decompose-b-spline-curve-and-calculate-bezier-curves degree (:U params) (:P params) (+ (* col-to-left 4) 2) (+ (* col-to-left 4) 3) steps)))
;;         catmull-rom-spline-curve-fn (fn [top-or-bottom inner-or-outer steps]
;;                                       (let [left-position (left-position-fn top-or-bottom)
;;                                             middle-position (middle-position-fn top-or-bottom)
;;                                             right-position (right-position-fn top-or-bottom)
;;                                             web-position-fn (get-web-position-fn inner-or-outer)]
;;                                         (catmull-rom-spline-curve
;;                                          [(web-position-fn col-to-left row left-position)
;;                                           (web-position-fn col-to-left row right-position)
;;                                           (web-position-fn (inc col-to-left) row left-position)
;;                                           (web-position-fn (inc col-to-left) row right-position)]
;;                                          steps
;;                                          :alphaType catmull-rom-alpha)))
;;         curve-fn (fn [top-or-bottom inner-or-outer curve-type steps]
;;                    (case curve-type
;;                      :global (global-first-deriv-fn top-or-bottom inner-or-outer steps)
;;                      :catmull (catmull-rom-spline-curve-fn top-or-bottom inner-or-outer steps)
;;                      :else (curve-type steps)))
;;         P-u-zero-coll-outer (curve-fn :top :outer top-outer-fn steps)
;;         P-u-one-coll-outer (curve-fn :bottom :outer bottom-outer-fn steps)
;;         P-u-one-coll-inner (curve-fn :top :inner top-inner-fn steps)
;;         P-u-zero-coll-inner (curve-fn :bottom :inner bottom-inner-fn steps)
;;         lofted-outer-surface (lofted-surface  P-u-zero-coll-outer P-u-one-coll-outer steps steps)
;;         lofted-inner-surface (lofted-surface  P-u-zero-coll-inner P-u-one-coll-inner steps steps)]
;;     (vnf-polyhedron (wall-vnf-array lofted-outer-surface lofted-inner-surface))))

(defn fractyl-column-curve [steps col row side
                                         top-or-bottom
                                         & {:keys [degree]
                                            :or {degree 2}}]
  (let [bottom-point-fn (fn [side] (case side
                                     :right :br
                                     :left :bl))
        top-position-fn (fn [side] (case side
                                     :right :tr
                                     :left :tl))
        get-web-post-position-fn (fn [top-or-bottom] (case
                                                      top-or-bottom
                                                       :top main-body-web-post-point-top
                                                       :bottom main-body-web-post-point-bottom))
        bottom-position (bottom-point-fn side)
        top-position (top-position-fn side)
        web-post-position-fn (get-web-post-position-fn top-or-bottom)
        params (global-curve-interp-with-calculated-first-derivatives
                [(web-post-position-fn col 2 bottom-position)
                 (web-post-position-fn col 2 top-position)
                 (web-post-position-fn col 1 bottom-position)
                 (web-post-position-fn col 1 top-position)
                 (web-post-position-fn col 0 bottom-position)
                 (web-post-position-fn col 0 top-position)]
                [(mapv - (web-post-position-fn col 2 bottom-position) (web-post-position-fn col 2 :bm))
                 (mapv - (web-post-position-fn col 2 top-position) (web-post-position-fn col 2 bottom-position))
                 (mapv - (web-post-position-fn col 1 bottom-position) (web-post-position-fn col 2 top-position))
                 (mapv - (web-post-position-fn col 1 top-position) (web-post-position-fn col 1 bottom-position))
                 (mapv - (web-post-position-fn col 0 bottom-position) (web-post-position-fn col 1 top-position))
                 (mapv - (web-post-position-fn col 0 top-position) (web-post-position-fn col 0 bottom-position))]
                degree
                        ;segments-steps
                )]
    (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U params) (:P params) (+ (* (- (dec cornerrow) row) 4) 4) (inc (+ (* (- (dec cornerrow) row) 4) 4))
                                                          steps)))

(defn fractyl-column-curve-between-rows [steps col row-above side
                            top-or-bottom
                            & {:keys [degree]
                               :or {degree 2}}]
  (let [bottom-point-fn (fn [side] (case side
                                     :right :br
                                     :left :bl))
top-position-fn (fn [side] (case side
                             :right :tr
                             :left :tl))
get-web-post-position-fn (fn [top-or-bottom] (case
                                              top-or-bottom
                                               :top main-body-web-post-point-top
                                               :bottom main-body-web-post-point-bottom))
bottom-position (bottom-point-fn side)
top-position (top-position-fn side)
web-post-position-fn (get-web-post-position-fn top-or-bottom)
        params (global-curve-interp-with-calculated-first-derivatives
                [(web-post-position-fn col 2 bottom-position)
                 (web-post-position-fn col 2 top-position)
                 (web-post-position-fn col 1 bottom-position)
                 (web-post-position-fn col 1 top-position)
                 (web-post-position-fn col 0 bottom-position)
                 (web-post-position-fn col 0 top-position)]
                [(mapv - (web-post-position-fn col 2 bottom-position) (web-post-position-fn col 2 :bm))
                 (mapv - (web-post-position-fn col 2 top-position) (web-post-position-fn col 2 bottom-position))
                 (mapv - (web-post-position-fn col 1 bottom-position) (web-post-position-fn col 2 top-position))
                 (mapv - (web-post-position-fn col 1 top-position) (web-post-position-fn col 1 bottom-position))
                 (mapv - (web-post-position-fn col 0 bottom-position) (web-post-position-fn col 1 top-position))
                 (mapv - (web-post-position-fn col 0 top-position) (web-post-position-fn col 0 bottom-position))]
                degree
                        ;segments-steps
                )
]
(decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U params) (:P params) (+ (* (- (dec cornerrow) row-above) 4) 2) (inc (+ (* (- (dec cornerrow) row-above) 4) 2))
                                                      steps)
))
(defn fractyl-column-row-connecters-curves [steps ; wall-section-steps
                                            & {:keys [degree]
                                               :or {degree 2}}]
  (let [n 5
        
        bottom-point-fn (fn [side] (case side
                                     :right :br
                                     :left :bl))
        top-position-fn (fn [side] (case side
                                     :right :tr
                                     :left :tl)) 
        get-web-post-position-fn (fn [top-or-bottom] (case
                                                      top-or-bottom
                                                       :top main-body-web-post-point-top
                                                       :bottom main-body-web-post-point-bottom))
        segment-steps (* steps 2)
        curve-fn  (fn [side top-or-bottom] (vec (for [col (range 0 (inc lastcol))
                                                      :let [bottom-position (bottom-point-fn side)
                                                            top-position (top-position-fn side)
                                                            web-post-position-fn (get-web-post-position-fn top-or-bottom)
                                                            params (global-curve-interp-with-calculated-first-derivatives
                                                                    [(web-post-position-fn col 2 bottom-position)
                                                                     (web-post-position-fn col 2 top-position)
                                                                     (web-post-position-fn col 1 bottom-position)
                                                                     (web-post-position-fn col 1 top-position)
                                                                     (web-post-position-fn col 0 bottom-position)
                                                                     (web-post-position-fn col 0 top-position)]
                                                                    [(mapv - (web-post-position-fn col 2 bottom-position) (web-post-position-fn col 2 :bm))
                                                                     (mapv - (web-post-position-fn col 2 top-position) (web-post-position-fn col 2 bottom-position))
                                                                     (mapv - (web-post-position-fn col 1 bottom-position) (web-post-position-fn col 2 top-position))
                                                                     (mapv - (web-post-position-fn col 1 top-position) (web-post-position-fn col 1 bottom-position))
                                                                     (mapv - (web-post-position-fn col 0 bottom-position) (web-post-position-fn col 1 top-position))
                                                                     (mapv - (web-post-position-fn col 0 top-position) (web-post-position-fn col 0 bottom-position))]
                                                                    degree
                        ;segments-steps
                                                                    )]]
                                                  [(decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U params) (:P params) 2 3
                                                                                                         steps)
                                                   (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U params) (:P params) 6 7
                                                                                                         steps)])))

        right-outer-curves (curve-fn :right :top)
        left-outer-curves (curve-fn :left :top)
        right-inner-curves (curve-fn :right :bottom)
        left-inner-curves (curve-fn :left :bottom)
        ]
    {:right-outer-curves right-outer-curves
     :left-outer-curves  left-outer-curves 
     :right-inner-curves right-inner-curves
     :left-inner-curves  left-inner-curves}
    )
  
  
  )


(defn fractyl-column-row-connecters [steps ; wall-section-steps
                                     &{:keys [degree]
                                       :or {degree 2}}]
  (let [
         
        {right-outer-curves :right-outer-curves
        left-outer-curves :left-outer-curves
        right-inner-curves :right-inner-curves
        left-inner-curves :left-inner-curves} (fractyl-column-row-connecters-curves steps :degree degree)
        segment-steps (* steps 2)
        ruled-surfaces (apply concat (vec (for [col (range 1 lastcol)
                                                :let [left (nth left-outer-curves col)
                                                      right (nth right-outer-curves col)
                                                      left-inner (nth left-inner-curves col)
                                                      right-inner (nth right-inner-curves col)]]
                                            (vec (for [row (range cornerrow)]
                                                   (vnf-polyhedron (wall-vnf-array (lofted-surface (nth left row) (nth right row) steps steps)
                                                                                   (lofted-surface (nth right-inner row) (nth left-inner row)  steps steps)
                                                                                   default-vnf-vertex-array-args)))))))]
    ruled-surfaces
    ))

(defn horizontal-curves-for-fractyl-column-row-connecter [col row-above steps]
  (let [{right-outer-curves :right-outer-curves
         left-outer-curves :left-outer-curves
         right-inner-curves :right-inner-curves
         left-inner-curves :left-inner-curves} (fractyl-column-row-connecters-curves steps)
        ]
    {:right-outer-curve (get-in right-outer-curves [col row-above]) 
      :left-outer-curve (get-in left-outer-curves [col row-above])
      :right-inner-curve (get-in right-inner-curves [col row-above])
      :left-inner-curve (get-in left-inner-curves [col row-above])}))

(defn fractyl-column-row-connecters-for-inner-index-column [left-side-outer-curves left-side-inner-curves steps
                                                            & {:keys [degree]
                                                               :or {degree 2}}
                                                            ]
(let [segment-steps (* steps 2)]
  (for [row (range 0 cornerrow) 
        :let [{right-outer-curve :right-outer-curve 
               right-inner-curve  :right-inner-curve
               } (horizontal-curves-for-fractyl-column-row-connecter 0 row steps)
              left-outer-curve ((nth left-side-outer-curves row) segment-steps)]]
    (vnf-polyhedron (wall-vnf-array (lofted-surface (reverse ((nth left-side-outer-curves row) steps)) right-outer-curve steps steps)
                                    (lofted-surface right-inner-curve  (reverse ((nth left-side-inner-curves row) steps))  steps steps)
                                    default-vnf-vertex-array-args))
    )) 
  )

(defn fractyl-column-row-connecters-for-pinky-column [right-side-outer-curves right-side-inner-curves steps
                                                            & {:keys [degree]
                                                               :or {degree 2}}] 
  (let [segment-steps (* steps 2)]
    (for [row (range 0 cornerrow)
          :let [{left-outer-curves :left-outer-curve 
                 left-inner-curves :left-inner-curve} (horizontal-curves-for-fractyl-column-row-connecter lastcol row steps)
                ]]
      (vnf-polyhedron (wall-vnf-array (lofted-surface left-outer-curves ((nth right-side-outer-curves row) steps) steps steps)
                                      (lofted-surface ((nth right-side-inner-curves row) steps) left-inner-curves  steps steps)
                                      default-vnf-vertex-array-args)))))
(defn fractyl-col-to-col-connecter [col-to-left row steps
                                     & {:keys [vertical-degree
                                               horizontal-degree
                                               upper-outer-point-paramater-calculation-method
                                               upper-inner-point-paramater-calculation-method
                                               lower-outer-point-paramater-calculation-method
                                               lower-inner-point-paramater-calculation-method
                                               magnitude-estimation-method
                                               knot-vector-generation-method
                                               upper-outer-catmull-rom-alpha
                                               upper-inner-catmull-rom-alpha
                                               lower-inner-catmull-rom-alpha
                                               lower-outer-catmull-rom-alpha
                                               upper-horizontal-outer-curve-type upper-horizontal-inner-curve-type
                                               lower-horizontal-outer-curve-type lower-horizontal-inner-curve-type
                                               render-method 
                                               vnf-style]
                                        :or {vertical-degree 2
                                             horizontal-degree 2
                                             upper-outer-point-paramater-calculation-method :centripetal
                                             upper-inner-point-paramater-calculation-method :centripetal
                                             lower-outer-point-paramater-calculation-method :centripetal
                                             lower-inner-point-paramater-calculation-method :centripetal
                                             knot-vector-generation-method :average
                                             magnitude-estimation-method :arc
                                             upper-outer-catmull-rom-alpha :centripetal
                                             upper-inner-catmull-rom-alpha :centripetal
                                             lower-outer-catmull-rom-alpha :centripetal
                                             lower-inner-catmull-rom-alpha :centripetal
                                             upper-horizontal-outer-curve-type :global
                                             upper-horizontal-inner-curve-type :global
                                             lower-horizontal-outer-curve-type :global
                                             lower-horizontal-inner-curve-type :global
                                             render-method :polyhedron
                                             vnf-style :default}}]
  (let [right-outer-curve (fractyl-column-curve steps (inc col-to-left) row :left :top)
        left-outer-curve (fractyl-column-curve steps col-to-left row :right :top)
        right-inner-curve (fractyl-column-curve steps (inc col-to-left) row :left :bottom)
        left-inner-curve (fractyl-column-curve steps col-to-left row :right :bottom)
        {upper-horizontal-outer-curve :outer-curve
         upper-horizontal-inner-curve :inner-curve} (fractyl-col-to-col-connecter-curves col-to-left row :top steps :degree horizontal-degree
                                                                                  :outer-point-paramater-calculation-method upper-outer-point-paramater-calculation-method
                                                                                  :inner-point-paramater-calculation-method upper-inner-point-paramater-calculation-method
                                                                                  :point-paramater-calculation-method upper-outer-point-paramater-calculation-method
                                                                                  :knot-vector-generation-method knot-vector-generation-method
                                                                                  :magnitude-estimation-method magnitude-estimation-method
                                                                                  :outer-catmull-rom-alpha upper-outer-catmull-rom-alpha
                                                                                  :inner-catmull-rom-alpha upper-inner-catmull-rom-alpha
                                                                                  :outer-fn upper-horizontal-outer-curve-type :inner-fn upper-horizontal-inner-curve-type)
        {lower-horizontal-outer-curve :outer-curve
         lower-horizontal-inner-curve :inner-curve} (fractyl-col-to-col-connecter-curves col-to-left row :bottom steps :degree horizontal-degree
                                                                                  :outer-point-paramater-calculation-method lower-outer-point-paramater-calculation-method
                                                                                  :inner-point-paramater-calculation-method lower-inner-point-paramater-calculation-method
                                                                                  :knot-vector-generation-method knot-vector-generation-method
                                                                                  :magnitude-estimation-method magnitude-estimation-method
                                                                                  :outer-catmull-rom-alpha lower-outer-catmull-rom-alpha
                                                                                  :inner-catmull-rom-alpha lower-inner-catmull-rom-alpha
                                                                                  :outer-fn lower-horizontal-outer-curve-type :inner-fn lower-horizontal-inner-curve-type)
        outer-surface (lofted-surface upper-horizontal-outer-curve lower-horizontal-outer-curve steps steps)
         inner-surface (lofted-surface  lower-horizontal-inner-curve upper-horizontal-inner-curve steps steps)
        ]

    (union
     
      (case render-method
        :polyhedron (vnf-polyhedron
       (wall-vnf-array   outer-surface inner-surface  :args {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style vnf-style}))
      :chained-hull (apply chained-hull-for-four-lists (conj (mapv #(plot-bezier-points % (sphere epsilon))[upper-horizontal-outer-curve upper-horizontal-inner-curve  lower-horizontal-outer-curve lower-horizontal-inner-curve ]) steps)))
     ;(vnf-polyhedron (vnf-vertex-array outer-surface :caps false :col-wrap false))
     ;(vnf-polyhedron (vnf-vertex-array inner-surface :caps false :col-wrap false))
     ;(plot-bezier-points row-above-outer-curve (sphere 0.5))
     ;(translate (nth left-inner-curve 0) (cube 1 1 1))
     ;(color [0 1 1 1](plot-bezier-points row-below-outer-curve (sphere 0.5)))
     ;(color [1 0 0 1] (translate (nth upper-horizontal-outer-curve 0) (cube 0.5 0.5 0.5)))
     ;(color [1 0 0 1] (translate (nth right-outer-curve 0) (cube 1 1 1)))
     
     ;(plot-bezier-points right-outer-curve (sphere 1))
     ;(color [1 0 0 1](plot-bezier-points left-outer-curve (sphere 1)))
     )))

(defn fractyl-crossroads-connecters [col-to-left row-above steps
                                     & {:keys [vertical-degree
                                               horizontal-degree
                                               upper-outer-point-paramater-calculation-method
                                               upper-inner-point-paramater-calculation-method
                                               lower-outer-point-paramater-calculation-method
                                               lower-inner-point-paramater-calculation-method
                                               magnitude-estimation-method
                                               knot-vector-generation-method
                                               upper-outer-catmull-rom-alpha
                                               upper-inner-catmull-rom-alpha
                                               lower-inner-catmull-rom-alpha
                                               lower-outer-catmull-rom-alpha
                                               row-above-outer-curve-type row-above-inner-curve-type
                                               row-below-outer-curve-type row-below-inner-curve-type]
                                        :or {vertical-degree 2
                                             horizontal-degree 2
                                             upper-outer-point-paramater-calculation-method :centripetal
                                             upper-inner-point-paramater-calculation-method :centripetal
                                             lower-outer-point-paramater-calculation-method :centripetal
                                             lower-inner-point-paramater-calculation-method :centripetal
                                             knot-vector-generation-method :average
                                             magnitude-estimation-method :arc
                                             upper-outer-catmull-rom-alpha :centripetal
                                             upper-inner-catmull-rom-alpha :centripetal
                                             lower-outer-catmull-rom-alpha :centripetal
                                             lower-inner-catmull-rom-alpha :centripetal
                                             row-above-outer-curve-type :global
                                             row-above-inner-curve-type :global
                                             row-below-outer-curve-type :global
                                             row-below-inner-curve-type :global}}]
  (let [right-outer-curve (fractyl-column-curve-between-rows steps (inc col-to-left) row-above :left :top)
        left-outer-curve (fractyl-column-curve-between-rows steps col-to-left row-above :right :top)
        right-inner-curve (fractyl-column-curve-between-rows steps (inc col-to-left) row-above :left :bottom)
        left-inner-curve (fractyl-column-curve-between-rows steps col-to-left row-above :right :bottom) 
        {row-above-outer-curve :outer-curve 
         row-above-inner-curve :inner-curve }(fractyl-col-to-col-connecter-curves col-to-left row-above :bottom steps :degree horizontal-degree
                                                                                  :outer-point-paramater-calculation-method upper-outer-point-paramater-calculation-method
                                                                                  :inner-point-paramater-calculation-method upper-inner-point-paramater-calculation-method
                                                                                  :point-paramater-calculation-method upper-outer-point-paramater-calculation-method
                                                                                  :knot-vector-generation-method knot-vector-generation-method
                                                                                  :magnitude-estimation-method magnitude-estimation-method 
                                                                                  :outer-catmull-rom-alpha upper-outer-catmull-rom-alpha
                                                                                  :inner-catmull-rom-alpha upper-inner-catmull-rom-alpha
                                                                                  :outer-fn row-above-outer-curve-type :inner-fn row-above-inner-curve-type)
        {row-below-outer-curve :outer-curve
         row-below-inner-curve :inner-curve} (fractyl-col-to-col-connecter-curves col-to-left (inc row-above) :top steps :degree horizontal-degree
                                                                                  :outer-point-paramater-calculation-method lower-outer-point-paramater-calculation-method
                                                                                  :inner-point-paramater-calculation-method lower-inner-point-paramater-calculation-method 
                                                                                  :knot-vector-generation-method knot-vector-generation-method
                                                                                  :magnitude-estimation-method magnitude-estimation-method 
                                                                                  :outer-catmull-rom-alpha lower-outer-catmull-rom-alpha
                                                                                  :inner-catmull-rom-alpha lower-inner-catmull-rom-alpha
                                                                                  :outer-fn row-below-outer-curve-type :inner-fn row-below-inner-curve-type)
        outer-surface (bicubic-coons-surface            (nth row-below-outer-curve 0) (nth row-above-outer-curve 0)  (peek row-below-outer-curve)  (peek row-above-outer-curve)         
                                                       left-outer-curve right-outer-curve row-below-outer-curve row-above-outer-curve  steps steps 
                                                                                        :boundary-curves-generated true)
        inner-surface (bicubic-coons-surface   (nth row-above-inner-curve 0) (nth row-below-inner-curve 0)  (peek row-above-inner-curve)  (peek row-below-inner-curve)    
                                             (reverse left-inner-curve) (reverse right-inner-curve)  row-above-inner-curve row-below-inner-curve steps steps 
                                             :boundary-curves-generated true)
        ]
    
    (union
      (vnf-polyhedron
       (wall-vnf-array  inner-surface outer-surface    :args {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
     ;(vnf-polyhedron (vnf-vertex-array outer-surface :caps false :col-wrap false))
     ;(vnf-polyhedron (vnf-vertex-array inner-surface :caps false :col-wrap false))
     ;(plot-bezier-points row-above-outer-curve (sphere 0.5))
     ;(translate (nth left-inner-curve 0) (cube 1 1 1))
     ;(color [0 1 1 1](plot-bezier-points row-below-outer-curve (sphere 0.5)))
    
     
     ;(plot-bezier-points right-outer-curve (sphere 1))
     ;(color [1 0 0 1](plot-bezier-points left-outer-curve (sphere 1)))
     )
    ))
(defn main-body-middle-row-connecters [steps & {:keys [degree
                                                       point-paramater-calculation-method knot-vector-generation-method
                                                       magnitude-estimation-method]
                                                :or {degree 2
                                                     point-paramater-calculation-method :centripetal
                                                     knot-vector-generation-method :average
                                                     magnitude-estimation-method :arc}}]
  (let [P-u-zero-coll-outer (vec (for [col (range lastcol)]
                                   (global-curve-interp-with-end-unit-derivatives-curve
                                    [(main-body-web-post-point-top (inc col) 1 :tl)
                                     (main-body-web-post-point-top col 1 :tr)]
                                    degree
                                    (main-body-web-post-point-top (inc col) 1 :tr)
                                    (main-body-web-post-point-top col 1 :tl)
                                    steps
                                    :point-paramater-calculation-method point-paramater-calculation-method
                                    :knot-vector-generation-method knot-vector-generation-method
                                    :magnitude-estimation-method magnitude-estimation-method
                                    :tangent-endpoints true)))
        P-u-one-coll-outer (vec (for [col (range lastcol)]
                                  (global-curve-interp-with-end-unit-derivatives-curve
                                   [(main-body-web-post-point-top (inc col) 1 :bl)
                                    (main-body-web-post-point-top col 1 :br)]
                                   degree
                                   (main-body-web-post-point-top (inc col) 0 :br)
                                   (main-body-web-post-point-top col 1 :bl)
                                   steps
                                   :point-paramater-calculation-method point-paramater-calculation-method
                                   :knot-vector-generation-method knot-vector-generation-method
                                   :magnitude-estimation-method magnitude-estimation-method
                                   :tangent-endpoints true)))
        P-u-zero-coll-inner (vec (for [col (range lastcol)]
                                   (global-curve-interp-with-end-unit-derivatives-curve
                                    [(main-body-web-post-point-bottom (inc col) 1 :bl)
                                     (main-body-web-post-point-bottom col 1 :br)]
                                    degree
                                    (main-body-web-post-point-bottom (inc col) 1 :br)
                                    (main-body-web-post-point-bottom col 1 :bl)
                                    steps
                                    :point-paramater-calculation-method point-paramater-calculation-method
                                    :knot-vector-generation-method knot-vector-generation-method
                                    :magnitude-estimation-method magnitude-estimation-method
                                    :tangent-endpoints true)))
        P-u-one-coll-inner (vec
                            (for [col (range lastcol)]
                              (global-curve-interp-with-end-unit-derivatives-curve
                               [(main-body-web-post-point-bottom (inc col) 1 :tl)
                                (main-body-web-post-point-bottom col 1 :tr)]
                               degree
                               (main-body-web-post-point-bottom (inc col) 1 :tr)
                               (main-body-web-post-point-bottom col 1 :tl)
                               steps
                               :point-paramater-calculation-method point-paramater-calculation-method
                               :knot-vector-generation-method knot-vector-generation-method
                               :magnitude-estimation-method magnitude-estimation-method
                               :tangent-endpoints true)))
        connecter-outer-surfaces (vec
                                  (for [index (range lastcol)]
                                    (lofted-surface (nth P-u-one-coll-outer index) (nth P-u-zero-coll-outer index) steps steps)))
        connecter-inner-surfaces
        (vec
         (for [index (range lastcol)]
           (lofted-surface  (nth P-u-one-coll-inner index) (nth P-u-zero-coll-inner index) steps steps)))
        connecter-polyhedrons (for [index (range lastcol)]
                                (vnf-polyhedron (wall-vnf-array (nth connecter-outer-surfaces index)
                                                                (nth connecter-inner-surfaces index))))]
    (union
     connecter-polyhedrons
     )))

(defn main-body-bottom-row-connecters [steps & {:keys [degree
                                                       point-paramater-calculation-method knot-vector-generation-method
                                                       magnitude-estimation-method]
                                                :or {degree 3
                                                     point-paramater-calculation-method :centripetal
                                                     knot-vector-generation-method :natural
                                                     magnitude-estimation-method :arc}}]
  (let [P-u-zero-coll-outer (vec (for [col (range 2 lastcol)]
                                   (global-curve-interp-with-end-unit-derivatives-curve
                                    [(main-body-web-post-point-top (inc col) 2 :tl)
                                     (main-body-web-post-point-top col 2 :tr)]
                                    degree
                                    (main-body-web-post-point-top (inc col) 2 :tr)
                                    (main-body-web-post-point-top col 2 :tl)
                                    steps
                                    :point-paramater-calculation-method point-paramater-calculation-method
                                    :knot-vector-generation-method knot-vector-generation-method
                                    :magnitude-estimation-method magnitude-estimation-method
                                    :tangent-endpoints true)))
        P-u-one-coll-outer (vec (for [col (range 2 lastcol)]
                                  (global-curve-interp-with-end-unit-derivatives-curve
                                   [(main-body-web-post-point-top (inc col) 2 :bl)
                                    (main-body-web-post-point-top col 2 :br)]
                                   degree
                                   (main-body-web-post-point-top (inc col) 0 :br)
                                   (main-body-web-post-point-top col 2 :bl)
                                   steps
                                   :point-paramater-calculation-method point-paramater-calculation-method
                                   :knot-vector-generation-method knot-vector-generation-method
                                   :magnitude-estimation-method magnitude-estimation-method
                                   :tangent-endpoints true)))
        P-u-zero-coll-inner (vec (for [col (range 2 lastcol)]
                                   (global-curve-interp-with-end-unit-derivatives-curve
                                    [(main-body-web-post-point-bottom (inc col) 2 :bl)
                                     (main-body-web-post-point-bottom col 2 :br)]
                                    degree
                                    (main-body-web-post-point-bottom (inc col) 1 :br)
                                    (main-body-web-post-point-bottom col 2 :bl)
                                    steps
                                    :point-paramater-calculation-method point-paramater-calculation-method
                                    :knot-vector-generation-method knot-vector-generation-method
                                    :magnitude-estimation-method magnitude-estimation-method
                                    :tangent-endpoints true)))
        P-u-one-coll-inner (vec
                            (for [col (range 2 lastcol)]
                              (global-curve-interp-with-end-unit-derivatives-curve
                               [(main-body-web-post-point-bottom (inc col) 2 :tl)
                                (main-body-web-post-point-bottom col 2 :tr)]
                               degree
                               (main-body-web-post-point-bottom (inc col) 2 :tr)
                               (main-body-web-post-point-bottom col 2 :tl)
                               steps
                               :point-paramater-calculation-method point-paramater-calculation-method
                               :knot-vector-generation-method knot-vector-generation-method
                               :magnitude-estimation-method magnitude-estimation-method
                               :tangent-endpoints true)))
        connecter-outer-surfaces (vec
                                  (for [index (range (- lastcol 2))]
                                    (lofted-surface (nth P-u-one-coll-outer index) (nth P-u-zero-coll-outer index) steps steps)))
        connecter-inner-surfaces
        (vec
         (for [index (range (- lastcol 2))]
           (lofted-surface  (nth P-u-one-coll-inner index) (nth P-u-zero-coll-inner index) steps steps)))
        connecter-polyhedrons (for [index (range (- lastcol 2))]
                                (vnf-polyhedron (wall-vnf-array (nth connecter-outer-surfaces index)
                                                                (nth connecter-inner-surfaces index))))]
    connecter-polyhedrons))

(defn main-body-column-connecters [corner-to-middle-start-outer-curve-fn middle-to-first-row-start-outer-curve-fn
                                   corner-to-middle-end-outer-curve-fn middle-to-first-row-end-outer-curve-fn
                                   corner-to-middle-start-inner-curve-fn middle-to-first-row-start-inner-curve-fn
                                   corner-to-middle-end-inner-curve-fn middle-to-first-row-end-inner-curve-fn
                                   steps
                                   & {:keys [degree
                                             point-paramater-calculation-method knot-vector-generation-method
                                             magnitude-estimation-method]
                                      :or {degree 3
                                           point-paramater-calculation-method :centripetal
                                           knot-vector-generation-method :natural
                                           magnitude-estimation-method :arc}}]
  (let [P-u-zero-coll-outer (vec (for [col (range (inc lastcol))]
                                   (vec (for [row (range cornerrow 0 -1)]
                                          (cond
                                            (and (zero? col) (= cornerrow row)) (corner-to-middle-start-outer-curve-fn steps)
                                            (and (zero? col) (= (dec cornerrow) row)) (middle-to-first-row-start-outer-curve-fn steps)
                                            :else (global-curve-interp-with-end-unit-derivatives-curve
                                                   [(main-body-web-post-point-top col (dec row) :bl)
                                                    (main-body-web-post-point-top col row :tl)]
                                                   degree
                                                   (main-body-web-post-point-top col (dec row) :tl)
                                                   (main-body-web-post-point-top col row :bl)
                                                   steps
                                                   :magnitude-estimation-method magnitude-estimation-method
                                                   :point-paramater-calculation-method point-paramater-calculation-method
                                                   :knot-vector-generation-method knot-vector-generation-method
                                                   :tangent-endpoints true))))))
        P-u-one-coll-outer (vec (for [col (range (inc lastcol))]
                                  (vec (for [row (range cornerrow 0 -1)]
                                         (cond
                                           (and (= lastcol col) (= cornerrow row)) (corner-to-middle-end-outer-curve-fn steps)
                                           (and (= lastcol col) (= (dec cornerrow) row)) (middle-to-first-row-end-outer-curve-fn steps)
                                           :else (global-curve-interp-with-end-unit-derivatives-curve
                                                  [(main-body-web-post-point-top col (dec row) :br)
                                                   (main-body-web-post-point-top col row :tr)]
                                                  degree
                                                  (main-body-web-post-point-top col (dec row) :tr)
                                                  (main-body-web-post-point-top col row :br)
                                                  steps
                                                  :magnitude-estimation-method magnitude-estimation-method
                                                  :point-paramater-calculation-method point-paramater-calculation-method
                                                  :knot-vector-generation-method knot-vector-generation-method
                                                  :tangent-endpoints true))))))
        P-u-zero-coll-inner (vec (for [col (range (inc lastcol))]
                                   (vec (for [row (range cornerrow 0 -1)]
                                          (cond
                                            (and (= lastcol col) (= cornerrow row)) (corner-to-middle-end-inner-curve-fn steps)
                                            (and (= lastcol col) (= (dec cornerrow) row)) (middle-to-first-row-end-inner-curve-fn steps)
                                            :else (global-curve-interp-with-end-unit-derivatives-curve
                                                   [(main-body-web-post-point-bottom col (dec row) :br)
                                                    (main-body-web-post-point-bottom col row :tr)]
                                                   degree
                                                   (main-body-web-post-point-bottom col (dec row) :tr)
                                                   (main-body-web-post-point-bottom col row :br)
                                                   steps
                                                   :magnitude-estimation-method magnitude-estimation-method
                                                   :point-paramater-calculation-method point-paramater-calculation-method
                                                   :knot-vector-generation-method knot-vector-generation-method
                                                   :tangent-endpoints true))))))
        P-u-one-coll-inner (vec (for [col (range (inc lastcol))]
                                  (vec (for [row (range cornerrow 0 -1)]
                                         (cond
                                           (and (zero? col) (= cornerrow row)) (corner-to-middle-start-inner-curve-fn steps)
                                           (and (zero? col) (= (dec cornerrow) row)) (middle-to-first-row-start-inner-curve-fn steps)
                                           :else (global-curve-interp-with-end-unit-derivatives-curve
                                                  [(main-body-web-post-point-bottom col (dec row) :bl)
                                                   (main-body-web-post-point-bottom col row :tl)]
                                                  degree
                                                  (main-body-web-post-point-bottom col (dec row) :tl)
                                                  (main-body-web-post-point-bottom col row :bl)
                                                  steps
                                                  :magnitude-estimation-method magnitude-estimation-method
                                                  :point-paramater-calculation-method point-paramater-calculation-method
                                                  :knot-vector-generation-method knot-vector-generation-method
                                                  :tangent-endpoints true))))))
        connecter-outer-surfaces (vec
                                  (for [outer-index (range (inc lastcol))]
                                    (vec (for [inner-index (range (dec cornerrow) -1 -1)]
                                           (lofted-surface (get-in P-u-one-coll-outer [outer-index inner-index]) (get-in P-u-zero-coll-outer [outer-index inner-index]) steps steps)))))
        connecter-inner-surfaces (vec
                                  (for [outer-index (range (inc lastcol))]
                                    (vec (for [inner-index (range (dec cornerrow) -1 -1)]
                                           (lofted-surface    (get-in P-u-one-coll-inner [outer-index inner-index]) (get-in P-u-zero-coll-inner [outer-index inner-index]) steps steps)))))
        connecter-polyhedrons (for [outer-index (range (inc lastcol)) inner-index (range (dec cornerrow) -1 -1)]
                                (vnf-polyhedron (wall-vnf-array (get-in connecter-outer-surfaces [outer-index inner-index])
                                                                (get-in connecter-inner-surfaces [outer-index inner-index]))))]
    connecter-polyhedrons))

(defrecord ThumbConnecterOneRowData [thumb-bl-to-tl-vnf thumb-tl-to-tr-vnf])
(defn thumb-connecter-1-row [steps & {:keys [thumb-bl-to-tl-P-u-zero-outer
                                             thumb-bl-to-tl-P-u-one-outer
                                             thumb-bl-to-tl-P-u-zero-inner
                                             thumb-bl-to-tl-P-u-one-inner
                                             thumb-tl-to-tr-P-u-zero-outer
                                             thumb-tl-to-tr-P-u-one-outer
                                             thumb-tl-to-tr-P-u-zero-inner
                                             thumb-tl-to-tr-P-u-one-inner]
                                      :or {thumb-bl-to-tl-P-u-zero-outer (n-degree-bezier-curve [(thumb-web-post-point-top thumb-tl-place :tl) (thumb-web-post-point-top thumb-bl-place :tr)] steps)
                                           thumb-bl-to-tl-P-u-one-outer (n-degree-bezier-curve [(thumb-web-post-point-top thumb-tl-place :bl) (thumb-web-post-point-top thumb-bl-place :br)] steps)
                                           thumb-bl-to-tl-P-u-zero-inner (n-degree-bezier-curve [(thumb-web-post-point-bottom thumb-tl-place :bl) (thumb-web-post-point-bottom thumb-bl-place :br)] steps)
                                           thumb-bl-to-tl-P-u-one-inner  (n-degree-bezier-curve [(thumb-web-post-point-bottom thumb-tl-place :tl) (thumb-web-post-point-bottom thumb-bl-place :tr)] steps)
                                           thumb-tl-to-tr-P-u-zero-outer (n-degree-bezier-curve [(thumb-web-post-point-top thumb-tr-place :tl) (thumb-web-post-point-top thumb-tl-place :tr)] steps)
                                           thumb-tl-to-tr-P-u-one-outer (n-degree-bezier-curve [(thumb-web-post-point-top thumb-tr-place :bl) (thumb-web-post-point-top thumb-tl-place :br)] steps)
                                           thumb-tl-to-tr-P-u-zero-inner (n-degree-bezier-curve [(thumb-web-post-point-bottom thumb-tr-place :bl) (thumb-web-post-point-bottom thumb-tl-place :br)] steps)
                                           thumb-tl-to-tr-P-u-one-inner  (n-degree-bezier-curve [(thumb-web-post-point-bottom thumb-tr-place :tl) (thumb-web-post-point-bottom thumb-tl-place :tr)] steps)}}]
  (let [thumb-bl-to-tl-outer (lofted-surface thumb-bl-to-tl-P-u-one-outer thumb-bl-to-tl-P-u-zero-outer steps steps)
        thumb-bl-to-tl-inner (lofted-surface thumb-bl-to-tl-P-u-one-inner thumb-bl-to-tl-P-u-zero-inner  steps steps)
        thumb-tl-to-tr-outer (lofted-surface thumb-tl-to-tr-P-u-one-outer thumb-tl-to-tr-P-u-zero-outer steps steps)
        thumb-tl-to-tr-inner (lofted-surface thumb-tl-to-tr-P-u-one-inner thumb-tl-to-tr-P-u-zero-inner  steps steps)]
    (ThumbConnecterOneRowData. (wall-vnf-array thumb-bl-to-tl-outer thumb-bl-to-tl-inner) (wall-vnf-array thumb-tl-to-tr-outer thumb-tl-to-tr-inner))))

(spit "things-low/thumb-connecters-test.scad"
      (write-scad
       (include include-bosl2)
       (let [steps 30
            ;;  thumb-bl-to-tl-P-u-zero-outer (n-degree-bezier-curve [(thumb-web-post-point-top thumb-tl-place :tl) (thumb-web-post-point-top thumb-bl-place :tr)] steps)
            ;;  thumb-bl-to-tl-P-u-one-outer (n-degree-bezier-curve [(thumb-web-post-point-top thumb-tl-place :bl) (thumb-web-post-point-top thumb-bl-place :br)] steps)
            ;;  thumb-bl-to-tl-P-u-zero-inner (n-degree-bezier-curve [(thumb-web-post-point-bottom thumb-tl-place :bl) (thumb-web-post-point-bottom thumb-bl-place :br)] steps)
            ;;  thumb-bl-to-tl-P-u-one-inner  (n-degree-bezier-curve [(thumb-web-post-point-bottom thumb-tl-place :tl) (thumb-web-post-point-bottom thumb-bl-place :tr)] steps)
            ;;  thumb-bl-to-tl-outer (lofted-surface thumb-bl-to-tl-P-u-one-outer thumb-bl-to-tl-P-u-zero-outer steps steps)
            ;;  thumb-bl-to-tl-inner (lofted-surface thumb-bl-to-tl-P-u-one-inner thumb-bl-to-tl-P-u-zero-inner  steps steps)
            ;;  thumb-tl-to-tr-P-u-zero-outer (n-degree-bezier-curve [(thumb-web-post-point-top thumb-tr-place :tl) (thumb-web-post-point-top thumb-tl-place :tr)] steps)
            ;;  thumb-tl-to-tr-P-u-one-outer (n-degree-bezier-curve [(thumb-web-post-point-top thumb-tr-place :bl) (thumb-web-post-point-top thumb-tl-place :br)] steps)
            ;;  thumb-tl-to-tr-P-u-zero-inner (n-degree-bezier-curve [(thumb-web-post-point-bottom thumb-tr-place :bl) (thumb-web-post-point-bottom thumb-tl-place :br)] steps)
            ;;  thumb-tl-to-tr-P-u-one-inner  (n-degree-bezier-curve [(thumb-web-post-point-bottom thumb-tr-place :tl) (thumb-web-post-point-bottom thumb-tl-place :tr)] steps)
            ;;  thumb-tl-to-tr-outer (lofted-surface thumb-tl-to-tr-P-u-one-outer thumb-tl-to-tr-P-u-zero-outer steps steps)
            ;;  thumb-tl-to-tr-inner (lofted-surface thumb-tl-to-tr-P-u-one-inner thumb-tl-to-tr-P-u-zero-inner  steps steps)
             {thumb-bl-to-tl-vnf :thumb-bl-to-tl-vnf
              thumb-tl-to-tr-vnf :thumb-tl-to-tr-vnf} (thumb-connecter-1-row steps)]
         (union
          (vnf-polyhedron thumb-bl-to-tl-vnf)
          (vnf-polyhedron thumb-tl-to-tr-vnf)
          ;thumb-wall-section
          thumb-type))))

(defrecord TrackPadToMainBodyData
           [trackpad-to-main-body-vnf
            left-side-key-gap-outer-curve-fn-coll
            left-side-key-gap-inner-curve-fn-coll])

(defn trackpad-to-main-body [steps & {:keys [thumb-side-border-curve-fn-outer thumb-side-border-tangent-curve-fn-outer
                                             thumb-side-border-curve-fn-inner thumb-side-border-tangent-curve-fn-inner]
                                      :or {thumb-side-border-curve-fn-outer (fn [steps] (catmull-rom-spline-curve
                                                                                         [tps-65-top-left-outer
                                                                                          tps-65-bottom-left-outer
                                                                                          (web-post-point-top (partial key-place 0 cornerrow) :bl :radians)
                                                                                          (web-post-point-top (partial key-place 0 cornerrow) :br :radians)]
                                                                                         steps))
                                           thumb-side-border-curve-fn-inner (fn [steps] (catmull-rom-spline-curve
                                                                                         [tps-65-top-left-inner
                                                                                          tps-65-bottom-left-inner
                                                                                          (web-post-point-bottom (partial key-place 0 cornerrow) :bl :radians)
                                                                                          (web-post-point-bottom (partial key-place 0 cornerrow) :br :radians)]
                                                                                         steps))

                                           thumb-side-border-tangent-curve-fn-outer (fn [steps] (catmull-rom-spline-curve
                                                                                                 [screen-holder-top-right-outside-point-alt
                                                                                                  (web-post-point-top thumb-tl-place :tl :degrees)
                                                                                                  (web-post-point-top thumb-tl-place :tr :degrees)
                                                                                                  (web-post-point-top thumb-tr-place :tm :degrees)]
                                                                                                 steps))
                                           thumb-side-border-tangent-curve-fn-inner (fn [steps] (catmull-rom-spline-curve
                                                                                                 [screen-holder-top-right-outside-point-alt
                                                                                                  (web-post-point-bottom thumb-tl-place :tl :degrees)
                                                                                                  (web-post-point-bottom thumb-tl-place :tr :degrees)
                                                                                                  (web-post-point-bottom thumb-tr-place :tm :degrees)]
                                                                                                 steps
                               ;:alphaType :chordal
                                                                                                 ))}}] 
  (let [end-curve-outer (thumb-side-border-curve-fn-outer steps)
        end-tangent-curve-outer (thumb-side-border-tangent-curve-fn-outer steps)
        end-curve-inner (thumb-side-border-curve-fn-inner steps)

        end-tangent-curve-inner (thumb-side-border-tangent-curve-fn-inner steps)
        left-section-top-mid-control-points   (calculate-control-points (tps-65-wall-position :tm :north))
        tps-65-tr-north-west-control-points (calculate-control-points (tps-65-wall-position :tr :north-west))
        inner-index-top-left-control-points (calculate-control-points (key-wall-position 0 0 0 1 :tl :slant :no-slant))
        inner-index-top-right-control-points (calculate-control-points (key-wall-position 0 0 0 1 :tr :slant :no-slant))
        inner-index-column-left-points-outer (vec (apply concat
                                                         (for [row (range (inc cornerrow))]
                                                           [(web-post-point-top (partial key-place 0 row) :tl :radians)
                                                            (web-post-point-top (partial key-place 0 row) :lm :radians)
                                                            (web-post-point-top (partial key-place 0 row) :bl :radians)])))
        inner-index-column-right-points-outer (vec (apply concat
                                                          (for [row (range (inc cornerrow))]
                                                            [(web-post-point-top (partial key-place 0 row) :tm :radians)
                                                             (web-post-point-top (partial key-place 0 row) :centre :radians)
                                                             (web-post-point-top (partial key-place 0 row) :bm :radians)])))
        inner-index-column-left-points-inner (vec (apply concat
                                                         (for [row (range (inc cornerrow))]
                                                           [(web-post-point-bottom (partial key-place 0 row) :tl :radians)
                                                            (web-post-point-bottom (partial key-place 0 row) :lm :radians)
                                                            (web-post-point-bottom (partial key-place 0 row) :bl :radians)])))
        inner-index-column-right-points-inner (vec (apply concat
                                                          (for [row (range (inc cornerrow))]
                                                            [(web-post-point-bottom (partial key-place 0 row) :tm :radians)
                                                             (web-post-point-bottom (partial key-place 0 row) :centre :radians)
                                                             (web-post-point-bottom (partial key-place 0 row) :bm :radians)])))
        modifier (dec (* 3 (inc cornerrow)))
        uk-fn (get-function-for-u-k-values :dynamic-centripetal)
        uk-values-left-outer (uk-fn modifier inner-index-column-left-points-outer)
        ;uk-values-right-outer (uk-fn modifier inner-index-column-right-points-outer)
        uk-values-left-inner (uk-fn modifier inner-index-column-left-points-inner)
        ;uk-values-right-inner (uk-fn modifier inner-index-column-right-points-inner)
        inner-index-column-length (total-chord-length-derivative-magnitude-estimation inner-index-column-left-points-outer)
        tps-mount-length (magnitude (mapv - tps-65-bottom-left-outer tps-65-bottom-right-outer))
        key-length (magnitude (mapv - (web-post-point-top (partial key-place 0 1) :tl :radians) (web-post-point-top (partial key-place 0 1) :bl :radians)))
        key-length-percentage (* (/ key-length inner-index-column-length) 100)
        tps-65-key-length-equiv (* (/ tps-mount-length 100) key-length-percentage)
        key-gap-length (magnitude (mapv - (web-post-point-top (partial key-place 0 1) :tl :radians) (web-post-point-top (partial key-place 0 0) :bl :radians)))
        key-gap-length-percentage (* (/ key-gap-length inner-index-column-length) 100)
        tps-65-key-gap-length-equiv (* (/ tps-mount-length 100) key-gap-length-percentage)
        tps-right-side-outer-fn (fn [t] (mapv + tps-65-bottom-right-outer (mul t (mapv - tps-65-bottom-left-outer tps-65-bottom-right-outer))))
        tps-left-side-outer-fn (fn [t] (mapv + tps-65-mid-right-outer (mul t (mapv - tps-65-mid-left-outer tps-65-mid-right-outer))))
        tps-right-side-inner-fn (fn [t] (mapv + tps-65-bottom-right-inner (mul t (mapv - tps-65-bottom-left-inner tps-65-bottom-right-inner))))
        tps-left-side-inner-fn (fn [t] (mapv + tps-65-mid-right-inner (mul t (mapv - tps-65-mid-left-inner tps-65-mid-right-inner))))
        tps-65-tr-north-west-to-inner-index-top-tr-tangent-outer-control-curve (catmull-rom-spline-curve
                                                                                [(:wall-locate-1-to-3-curve-for-polyhedron-second-control-point left-section-top-mid-control-points)
                                                                                 (:wall-locate-1-to-3-curve-for-polyhedron-second-control-point tps-65-tr-north-west-control-points)
                                                                                 (:wall-locate-1-to-3-curve-for-polyhedron-second-control-point inner-index-top-left-control-points)
                                                                                 (:wall-locate-1-to-3-curve-for-polyhedron-second-control-point inner-index-top-right-control-points)]
                                                                                steps)
        tps-65-tr-north-west-to-inner-index-top-tr-tangent-inner-control-curve (catmull-rom-spline-curve
                                                                                [(:wall-locate-2-top left-section-top-mid-control-points)
                                                                                 (:wall-locate-2-top tps-65-tr-north-west-control-points)
                                                                                 (:wall-locate-2-top inner-index-top-left-control-points)
                                                                                 (:wall-locate-2-top inner-index-top-right-control-points)]
                                                                                steps)
        outer-curves (conj (vec (for [index (range modifier)
                                      :let [uk (nth uk-values-left-outer index)
                                            tps-left-point (tps-left-side-outer-fn uk)
                                            tps-right-point (tps-right-side-outer-fn uk)
                                            inner-index-left-point (inner-index-column-left-points-outer index)
                                            inner-index-right-point (inner-index-column-right-points-outer index)]]
                                  (catmull-rom-spline-curve
                                   [tps-left-point
                                    tps-right-point
                                    inner-index-left-point
                                    inner-index-right-point]
                                   steps)))
                           end-curve-outer)
        inner-curves (conj (vec (for [index (range modifier -1 -1)
                                      :let [uk (nth uk-values-left-inner index)
                                            tps-left-point (tps-left-side-inner-fn uk)
                                            tps-right-point (tps-right-side-inner-fn uk)
                                            inner-index-left-point (inner-index-column-left-points-inner index)
                                            inner-index-right-point (inner-index-column-right-points-inner index)]]
                                  (catmull-rom-spline-curve
                                   [tps-left-point
                                    tps-right-point
                                    inner-index-left-point
                                    inner-index-right-point]
                                   steps)))
                           end-curve-inner)
        outer-curves-tangent-fn-coll (into [tps-65-tr-north-west-to-inner-index-top-tr-tangent-outer-control-curve]
                                           (conj outer-curves end-tangent-curve-outer))
        outer-curves-tangent-fn (fn [index] (vec (for [i (range (+ modifier 2))]
                                                      ;to make sure curve matches up with keys
                                                      ;(if (zero? (mod (inc i) 3)) (mapv - (nth (nth outer-curves-tangent-fn-coll i) index)
                                                       ;                                     (nth (nth outer-curves-tangent-fn-coll (dec i)) index))
                                                   (mapv - (nth (nth outer-curves-tangent-fn-coll (inc i)) index)
                                                         (nth (nth outer-curves-tangent-fn-coll i) index))
                                                        ;  )
                                                   )))
        inner-curves-tangent-fn-coll (into [end-tangent-curve-inner]
                                           (conj inner-curves tps-65-tr-north-west-to-inner-index-top-tr-tangent-inner-control-curve))
        inner-curves-tangent-fn (fn [index] (vec (for [i (range (+ modifier 2))
                                                       :let [t-start (nth (nth inner-curves-tangent-fn-coll i) index)
                                                             t-end (nth (nth inner-curves-tangent-fn-coll (inc i)) index)]]
                                                   (mapv - t-end t-start))))
        section-steps (* modifier steps)
        outer-face-params-fn (fn [index]
                               (global-curve-interp-with-calculated-first-derivatives
                                (vec (for [i (range (inc modifier))]
                                       (nth (nth outer-curves i) index)))
                                (outer-curves-tangent-fn index)
                                2
                                :point-paramater-calculation-method :chordal
                                :magnitude-estimation-method :arc))
        outer-face-fn (fn [index]
                        (let [params (outer-face-params-fn index)]
                          (non-uniform-b-spline (:P params) 2 (:U params) section-steps)))
        outer-face-points (for [index (range (inc steps))]
                            (outer-face-fn index))
        inner-face-params-fn (fn [index]
                               (global-curve-interp-with-calculated-first-derivatives
                                (vec (for [i (range (inc modifier))]
                                       (nth (nth inner-curves i) index)))
                                (inner-curves-tangent-fn index)
                                2
                                :point-paramater-calculation-method :chordal
                                :magnitude-estimation-method :arc))
        inner-face-fn (fn [index]
                        (let [params (inner-face-params-fn index)]
                          (non-uniform-b-spline (:P params) 2 (:U params) section-steps)))
        inner-face-points (for [index (range (inc steps))]
                            (inner-face-fn index))

        outer-face-end-curve-params (outer-face-params-fn steps)
        inner-face-end-curve-params (inner-face-params-fn steps)
        key-curve-decomposed-outer (let [params (outer-face-params-fn steps)]
                                     (:Q (decompose-b-spline-curve 2  (:U params) (:P params))))
        key-curve-decomposed-inner (let [params (inner-face-params-fn steps)]
                                     (:Q (decompose-b-spline-curve 2  (:U params) (:P params))))
        corner-to-middle-start-outer-curve-fn (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-face-end-curve-params) (:P outer-face-end-curve-params)
                                                                                                                10 11 steps))
        middle-to-first-row-start-outer-curve-fn (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-face-end-curve-params) (:P outer-face-end-curve-params)
                                                                                                                   4 5 steps))


        corner-to-middle-start-inner-curve-fn (fn [steps] (vec (reverse (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-face-end-curve-params) (:P inner-face-end-curve-params)
                                                                                                                              4 5 steps))))
        middle-to-first-row-start-inner-curve-fn (fn [steps] (vec (reverse  (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-face-end-curve-params) (:P inner-face-end-curve-params)
                                                                                                                                  10 11 steps))))

        ;  outer-face (vnf-vertex-array outer-face-points {:caps false :cap1 false :cap2 false :col-wrap false :row-wrap false :reverse true :style :default})
        ;inner-face (vnf-vertex-array inner-face-points {:caps false :cap1 false :cap2 false :col-wrap false :row-wrap false :reverse true :style :default})
        ]
    (TrackPadToMainBodyData. (wall-vnf-array outer-face-points inner-face-points :args {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse true :style :default})
                             [corner-to-middle-start-outer-curve-fn middle-to-first-row-start-outer-curve-fn]
                             [corner-to-middle-start-inner-curve-fn middle-to-first-row-start-inner-curve-fn])))

(spit "things-low/trackpad-to-key-test.scad"
      (write-scad
       (include include-bosl2)
       (let [steps 30
             ;trackpad-to-main-body-data 
             {trackpad-to-main-body-vnf :trackpad-to-main-body-vnf
              left-side-key-gap-outer-curve-fn-coll :left-side-key-gap-outer-curve-fn-coll
              left-side-key-gap-inner-curve-fn-coll :left-side-key-gap-inner-curve-fn-coll} (trackpad-to-main-body steps)
             corner-to-middle-end-outer-curve-fn (fn [steps] (n-degree-bezier-curve [(main-body-web-post-point-top lastcol (dec cornerrow) :br)
                                                                                     (main-body-web-post-point-top lastcol cornerrow :tr)]
                                                                                    steps))
             middle-to-first-row-end-outer-curve-fn (fn [steps] (n-degree-bezier-curve [(main-body-web-post-point-top lastcol (- cornerrow 2) :br)
                                                                                        (main-body-web-post-point-top lastcol (dec cornerrow) :tr)]
                                                                                       steps))
             corner-to-middle-end-inner-curve-fn (fn [steps]  (n-degree-bezier-curve [(main-body-web-post-point-bottom lastcol (dec cornerrow) :br)
                                                                                      (main-body-web-post-point-bottom lastcol cornerrow :tr)]
                                                                                     steps))
             middle-to-first-row-end-inner-curve-fn (fn [steps]  (n-degree-bezier-curve [(main-body-web-post-point-bottom lastcol (- cornerrow 2) :br)
                                                                                         (main-body-web-post-point-bottom lastcol (dec cornerrow) :tr)]
                                                                                        steps))
             main-body-column-connecters-p  (main-body-column-connecters (nth left-side-key-gap-outer-curve-fn-coll 0) (nth left-side-key-gap-outer-curve-fn-coll 1)
                                                                         corner-to-middle-end-outer-curve-fn middle-to-first-row-end-outer-curve-fn
                                                                         (nth left-side-key-gap-inner-curve-fn-coll 0) (nth left-side-key-gap-inner-curve-fn-coll 1)
                                                                         corner-to-middle-end-inner-curve-fn middle-to-first-row-end-inner-curve-fn
                                                                         steps)]
         (union
          key-holes
          (fractyl-row-connecters 0 steps :point-paramater-calculation-method :average-harmonic)
          (main-body-middle-row-connecters steps)
          (main-body-bottom-row-connecters steps)
          (vnf-polyhedron trackpad-to-main-body-vnf)
          main-body-column-connecters-p
          (tps-65-place (difference  tps-65-mount-new
                                     tps-65
                                     tps-65-mount-cutout
                                     (translate [0 0 (+ 0 1)] tps-65-mount-main-cutout)))))))