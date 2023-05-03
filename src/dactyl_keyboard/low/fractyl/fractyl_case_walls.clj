(ns dactyl-keyboard.low.fractyl.fractyl-case-walls
  (:refer-clojure :exclude [use import])
  (:require [clojure.math :refer [sqrt]]
            [dactyl-keyboard.lib.algebra :refer [find-point-on-line-using-x]]
            [dactyl-keyboard.lib.constants :refer [epsilon]]
            [dactyl-keyboard.lib.curvesandsplines.beziers :refer [n-degree-bezier-curve bezier-linear-spline]]
            [dactyl-keyboard.lib.curvesandsplines.linear-surface :refer [lofted-surface]]
            [dactyl-keyboard.lib.curvesandsplines.coons-surface :refer [bicubic-coons-surface triangular-coons-surface]]
            [dactyl-keyboard.lib.curvesandsplines.curve-fitting :refer [calculate-tangents-for-local-cubic-curve-interpolation-from-tangent
                                                                        global-curve-interp-with-calculated-first-derivatives
                                                                        global-curve-interp-with-calculated-first-derivatives-curve local-cubic-curve-interpolation-with-calculated-tangents
                                                                        local-cubic-curve-interpolation-with-tangents-curve]]
            [dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline :refer [decompose-b-spline-curve-and-calculate-bezier-curves
                                                                               decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves get-function-for-u-k-values
                                                                               non-uniform-b-spline nurbs nurbs-with-calculated-knot-vector]]
            [dactyl-keyboard.lib.curvesandsplines.splines :refer [catmull-rom-spline-curve]]
            [dactyl-keyboard.lib.geometry :refer [two-d-intersection-for-3d]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [include-bosl2]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vnf :refer :all]
            [dactyl-keyboard.lib.openscad.hull :refer [chained-hull-to-points]]
            [dactyl-keyboard.low.case-low-polyhedron-functions :refer :all]
            [dactyl-keyboard.low.fractyl.fractyl-key-plate-connectors :refer :all]
            [dactyl-keyboard.low.placement-functions-low :refer :all]
            [dactyl-keyboard.low.screen-holder-placement-points :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.low.thumbs-low :refer :all]
            [dactyl-keyboard.low.tps-65-placement-functions :refer :all]
            [dactyl-keyboard.low.tps-65-placement-points :refer :all]
            [dactyl-keyboard.low.web-connecters-low :refer :all]
            [dactyl-keyboard.tps-65 :refer :all]
            [dactyl-keyboard.utils :refer [plot-bezier-points]]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))

(defn fractyl-back-wall [wall-cross-section-steps wall-section-steps] 
     ;(vnf-polyhedron
     ; (wall-vnf 
       (wall-section
                 (wall-section-parameter
                  (vec (concat
                        [(wall-cross-section-parameter (key-wall-position lastcol 0 1 1 :tr))]
                        (apply concat (for [col-index (range 0 ncols)
                                            :let [col (- lastcol col-index)
                                                  end-position (if (= col lastcol) :tm :tr)
                                                  tr-slant (if (zero? col) :no-slant :parallel-by-d)
                                                  tl-slant (if (or (zero? col) (= col 1)) :no-slant :parallel-by-d)]]
                                        (concat
                                         ;(if (not= col lastcol) [(wall-cross-section-parameter (key-wall-position col 0 0 1 end-position :slant tr-slant))])
                                         ;[(wall-cross-section-parameter (key-wall-position col 0 0 1 :tm :slant tr-slant))]
                                         [(wall-cross-section-parameter (key-wall-position col 0 0 1 end-position :slant tr-slant))]
                                         [(wall-cross-section-parameter (key-wall-position col 0 0 1 :tl :slant tl-slant))])))
                        [(wall-cross-section-parameter (tps-65-wall-position :tr :north-west))]
                        [(wall-cross-section-parameter (tps-65-wall-position :tm :north))]))
                  (catmull-rom-spline-parameters ;:alpha :uniform
                   :linear-outer-top true :linear-inner-top true) 
                  )
                 wall-cross-section-steps wall-section-steps)
                ;{:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :alt}))
       )

(comment (spit "things-low/back-wall-test.scad"
      (write-scad
       (include "../BOSL2/std.scad")
       (let [curve-points (vec (concat
                         [ (main-body-web-post-point-top lastcol 0  :tr)]
                         (apply concat (for [col-index (range 0 ncols)
                                             :let [col (- lastcol col-index)
                                                   end-position (if (= col lastcol) :tm :tr)
                                                   tr-slant (if (zero? col) :no-slant :parallel-by-d)
                                                   tl-slant (if (or (zero? col) (= col 1)) :no-slant :parallel-by-d)]]
                                         (concat
                                          [ (main-body-web-post-point-top col 0  end-position)]
                                          [(main-body-web-post-point-top col 0 :tl)])))
                         [ tps-65-bottom-right-outer]
                         [tps-65-mid-right-outer]))](union
        key-holes 
                                                      (plot-bezier-points (bezier-linear-spline
                                                       (vec (drop-last (drop 1 curve-points))) 
                                                       50)
                                                                          (sphere 0.1))
        (vnf-polyhedron (wall-vnf (fractyl-back-wall 10 30) {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :alt}))
        (tps-65-place tps-65-mount-new))
         )
       )))

(defn thumb-tr-br-to-middle-lm-local-cubic-curve-interpolation-wall-section-fn [wall-cross-section-steps wall-section-steps]
  (wall-section
   (wall-section-parameter
    [;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 -1 :tr :xy 3 :slant :parallel-by-d-opposite :offset [-0.000001 -0.000001 0]))
     (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :br :xy 4  :slant :no-slant))
     (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :rm :xy 4  :slant :no-slant))
     (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 -1 :tr :xy 4 :slant :parallel-by-d-opposite))
                                     ;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :tr :xy 3 :offset [-0.5 1 0] :slant :parallel-by-d-opposite))
     (wall-cross-section-parameter (key-wall-position 1 2 1 0 :br :xy 3 :slant :no-slant))
                                     ;(wall-cross-section-parameter (key-wall-position 2 2 1 -1  :bl :xy 3 :slant :no-slant)) 
     (wall-cross-section-parameter (key-wall-position 2 2 1 -1  :bl :xy 3 :slant :no-slant))]
    (local-cubic-curve-interpolation-with-calculated-tangents-parameter ;:linear-outer-top true :linear-inner-top true
     :start-segment 1 :end-segment 3)
    :calculation-order :vertical-first)
   wall-cross-section-steps wall-section-steps))

(defrecord ThumbWallSectionForASingleThumbRowData [wall-section outer-key-gap-fn-coll inner-key-gap-fn-coll])
(defn thumb-wall-section-for-single-thumb-row-fn [wall-cross-section-steps wall-section-steps]
  (let [tr-tr (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 -1 :tr :xy 4 :slant :parallel-by-d-opposite))
        tr-rm (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :rm :xy 4  :slant :no-slant))
        tr-br-east (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :br :xy 4 :slant :no-slant))
        tr-br-south-east (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 -1 :br :xy 5 :slant :no-slant :offset [0.000001 -0.000001 0.0]))
        tr-br-south (wall-cross-section-parameter
                     (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant :offset [0.0 -0.000001 0.0]))
        tr-bm  (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 0 -1 :bm :xy 5 :slant :no-slant))
        tr-bl-bm  (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 0 -1 :bl-bm :xy 5 :slant :no-slant))
        tr-bl (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 0 -1 :bl :xy 5 :slant :no-slant))
        tl-br (wall-cross-section-parameter (thumb-wall-position thumb-tl-place 0 -1 :br :xy 5 :slant :no-slant))
        tl-bm (wall-cross-section-parameter (thumb-wall-position thumb-tl-place 0 -1 :bm :xy 5 :slant :no-slant))
        tl-bl (wall-cross-section-parameter (thumb-wall-position thumb-tl-place 0 -1 :bl :xy 5 :slant :no-slant))
        bl-br (wall-cross-section-parameter (thumb-wall-position thumb-bl-place 0 -1 :br :xy 5))
        bl-bm (wall-cross-section-parameter (thumb-wall-position thumb-bl-place 0 -1 :bm :xy 5))
        bl-bl-south (wall-cross-section-parameter (thumb-wall-position thumb-bl-place 0 -1 :bl :xy 4))
        bl-bl-south-west (wall-cross-section-parameter
                          (thumb-wall-position thumb-bl-place -1 -1 :bl :xy 4 :slant :no-slant :offset [-0.000001 -0.000001 0.0]))
        bl-bl-west (wall-cross-section-parameter
                    (thumb-wall-position thumb-bl-place -1 0 :bl :xy 4 :slant :no-slant :offset [0.000001 0.0 0.0]))
        bl-lm (wall-cross-section-parameter (thumb-wall-position thumb-bl-place -1 0 :lm :xy 4 :slant :no-slant))
        bl-tl (wall-cross-section-parameter (thumb-wall-position thumb-bl-place -1 0 :tl :xy 4 :slant :no-slant))
        cross-sections [tr-rm
                        tr-br-east
                        ;tr-br-south-east
                        tr-br-south
                        tr-bm
                        tr-bl
                        tl-br
                        tl-bm
                        tl-bl
                        bl-br
                        bl-bm
                        bl-bl-south
                        bl-bl-west
                        bl-lm]
        tangents [(wall-cross-section-tangent-parameter tr-rm tr-tr)
                  (wall-cross-section-tangent-parameter tr-br-south-east tr-rm)
                  ;(wall-cross-section-tangent-parameter tr-bm tr-br-south-east)
                  (wall-cross-section-tangent-parameter tr-bm tr-br-south)
                  (wall-cross-section-tangent-parameter tr-bl tr-bm)
                 ;(wall-cross-section-tangent-parameter tl-br tr-bl)
                  (wall-cross-section-tangent-parameter tr-bl tr-bm)
                  (wall-cross-section-tangent-parameter tl-bm tl-br)
                  (wall-cross-section-tangent-parameter tl-bl tl-bm)
                  (wall-cross-section-tangent-parameter bl-br tl-bl)
                  (wall-cross-section-tangent-parameter bl-bm bl-br)
                  (wall-cross-section-tangent-parameter bl-bl-south bl-bm)
                  (wall-cross-section-tangent-parameter bl-bl-south-west bl-bm)
                  (wall-cross-section-tangent-parameter bl-lm bl-bl-west)
                  (wall-cross-section-tangent-parameter bl-tl bl-lm)]
        point-paramater-calculation-method :chordal
        magnitude-estimation-method :arc
        wall-section (wall-section
                      (wall-section-parameter
                       cross-sections
                       (global-curve-interp-with-first-derivatives-parameters
                        tangents
                        2
                                ;(wall-cross-section-parameter (thumb-wall-position thumb-mr-place 1 0 :rm :xy 5 :slant :no-slant))
                                ;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :rm :xy 4  :slant :no-slant))
                        :point-paramater-calculation-method point-paramater-calculation-method
                                ;:knot-vector-generation-method :natural
                        :magnitude-estimation-method magnitude-estimation-method)
                       :calculation-order :vertical-first)
                      wall-cross-section-steps wall-section-steps)
        wall-cross-sections (:wall-cross-sections wall-section)
        wall-control-points (mapv #(:all-control-points %) wall-cross-sections)
        web-post-position-top-points (mapv #(:web-post-position-top %) wall-control-points)
        web-post-position-bottom-points  (mapv #(:web-post-position-bottom %) wall-control-points)
        wall-cross-section-tangent-web-post-top-and-bottom-vectors (wall-cross-section-tangent-web-post-top-and-bottom-vectors-for-wall-section tangents) 
        web-post-position-top-tangents (mapv #(:wall-cross-section-tangent-vectors-outer %) wall-cross-section-tangent-web-post-top-and-bottom-vectors)
        web-post-position-bottom-tangents (mapv #(:wall-cross-section-tangent-vectors-inner %) wall-cross-section-tangent-web-post-top-and-bottom-vectors) 
        outer-params (global-curve-interp-with-calculated-first-derivatives 
                      web-post-position-top-points
                      web-post-position-top-tangents
                      2 
                      :point-paramater-calculation-method point-paramater-calculation-method
                      :magnitude-estimation-method magnitude-estimation-method)
        inner-params (global-curve-interp-with-calculated-first-derivatives
                      web-post-position-bottom-points
                      web-post-position-bottom-tangents
                      2 
                      :point-paramater-calculation-method point-paramater-calculation-method
                      :magnitude-estimation-method magnitude-estimation-method)
        
        ] 
        (ThumbWallSectionForASingleThumbRowData. wall-section [(fn [steps](decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-params) (:P outer-params) 8 9 steps))
                                                               (fn [steps](decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-params) (:P outer-params) 14 15 steps))]
                                                 [(fn [steps](decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-params) (:P inner-params) 8 9 steps))
                                                  (fn [steps](decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-params) (:P inner-params) 14 15 steps))])))

(defn thumb-wall-section [wall-cross-section-steps wall-section-steps]
  (thumb-wall-section-for-single-thumb-row-fn wall-cross-section-steps wall-section-steps)
  (union
   (vnf-polyhedron (wall-vnf (thumb-tr-br-to-middle-lm-local-cubic-curve-interpolation-wall-section-fn wall-cross-section-steps wall-section-steps)
                             {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))

;;    (vnf-polyhedron (wall-vnf (thumb-wall-section-for-single-thumb-row-fn wall-cross-section-steps wall-section-steps)
;;                              {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse true :style :default}))
   ))
(comment (spit "things-low/thumb-section-test.scad"
      (write-scad
       (include "../BOSL2/std.scad")
       (union
        ;thumb-type
        (thumb-wall-section 30 30)))))


(defn front-wall-nurbs [wall-cross-section-steps wall-section-steps]
  (let [weights [1 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 1];(vec (reverse [1 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 1]))
               knot-vector (let [denom 10]
                             (mapv (partial * (dec denom)) [0 0 0 (/ 1 denom) (/ 2 denom) (/ 3 denom) (/ 5 denom) (/ 6 denom) (/ 7 denom) (/ 7.5 denom) (/ 9.0 denom)  1 1 1]))
               knot-vector-2 (let [denom 10]
                               (mapv (partial * (dec denom)) [0 0 0 (/ 1 denom) (/ 2.5 denom) (/ 3 denom) (/ 4 denom) (/ 5 denom) (/ 7 denom) (/ 8 denom) (/ 9.0 denom)  1 1 1]))
               curve-paramater (nurbs-parameters 2 weights
                                                 :knot-vector knot-vector
                                                 :linear-outer-top false
                                                 :linear-inner-top false)
            ;;    wall-positions [
            ;;                   (key-wall-position 1 2 1 0 :br :xy 3 :slant :no-slant)
               
            ;;                   (key-wall-position 2 2 1 -1 :bl  :xy 3 :slant :no-slant)
            ;;                   (key-wall-position 2 2 -1 -1 :br :slant :no-slant :xy 4)
            ;;                   (key-wall-position 3 2 -1 0 :bl :offset [0.000001 0 0] :slant :no-slant :xy 4)
            ;;                   (key-wall-position 3 2 -1 -1 :bl :slant :no-slant :xy 4.5)
            ;;                   (key-wall-position 3 2 0 -1 :bl :offset [0 0.000001 0])
            ;;                   (key-wall-position 3 2 -1 -1 :br :slant :no-slant)
            ;;                   (key-wall-position lastcol 2 -1 0 :bl :slant :no-slant :offset [0.000001 0 0])
            ;;                   (key-wall-position lastcol 2 -1 -1 :bl :slant :no-slant)
            ;;                   (key-wall-position lastcol 2 0 -1 :bl :slant :no-slant :offset [0 0.000001 0])
            ;;                   (key-wall-position lastcol 2 0 -1 :br)
               
            ;;                    ]
               wall-section-parameter (wall-section-parameter
                                       [;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :tr :xy 3 :slant :parallel-by-d-opposite))
                                             ;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :tr :xy 3 :slant :parallel-by-d-opposite :offset [0 3 -3]))
                                             ;(wall-cross-section-parameter (key-wall-position 1 2 1 -1 :br :xy 3 :offset [-1 -1 10]))
                                        (wall-cross-section-parameter (key-wall-position 1 2 1 0 :br :xy 3 :slant :no-slant))
                                             ;(wall-cross-section-parameter (key-wall-position 2 2 -1 -1 :bl  :xy 3 :slant :no-slant))
                                        (wall-cross-section-parameter (key-wall-position 2 2 1 -1 :bl  :xy 3 :slant :no-slant))
                                        (wall-cross-section-parameter (key-wall-position 2 2 -1 -1 :br :slant :no-slant :xy 4))
                                        (wall-cross-section-parameter (key-wall-position 3 2 -1 0 :bl :offset [0.000001 0 0] :slant :no-slant :xy 4))
                                        (wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :bl :slant :no-slant :xy 4.5))
                                        (wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bl :offset [0 0.000001 0]))
                                        (wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :br :slant :no-slant))
                                        (wall-cross-section-parameter (key-wall-position lastcol 2 -1 0 :bl :slant :no-slant :offset [0.000001 0 0]))
                                        (wall-cross-section-parameter (key-wall-position lastcol 2 -1 -1 :bl :slant :no-slant))
                                        (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :bl :slant :no-slant :offset [0 0.000001 0]))
                                        (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :bm :slant :no-slant))
                                        ;(wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :br ))
                                                      ;(wall-cross-section-parameter (key-wall-position 3 2 -1 0 :bl))
                                        ]
                                       curve-paramater)
               wall-section (wall-section wall-section-parameter wall-cross-section-steps wall-section-steps)
               wall-positions (mapv #(:wall-position %) wall-section)
               wall-cross-section-control-points (mapv #(:all-control-points %) (:wall-cross-sections wall-section))
               upper-points (mapv #(:web-post-position-top %) wall-cross-section-control-points)
               total-steps (dec (count (:outer-wall wall-section)))
               nurbs-curve (nurbs (reverse upper-points) 2 knot-vector
                                  weights (* wall-section-steps 9))
               c1 (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves 2 (mapv double knot-vector-2) upper-points weights   0 8 (* wall-section-steps 10) :reverse-curve true)
               wall-cross-section-parameters (:wall-cross-section-parameters wall-section-parameter)
               wall-top (mapv #(nth % 0) (:outer-wall wall-section))
               inner-wall-top (mapv #(peek %) (:inner-wall wall-section))
               steps-distrubution (:steps-distrubution wall-section-parameter)
               curve-parameters (:curve-parameters wall-section-parameter)
               pinky-bl-to-fourth-bl-outer-curve (subvec wall-top (* (nth knot-vector 4) wall-section-steps) (* (nth knot-vector 8) wall-section-steps))
               pinky-bl-to-fourth-bl-inner-curve (subvec inner-wall-top (* (nth knot-vector 4) wall-section-steps) (* (nth knot-vector 8) wall-section-steps))
               fourth-bl-to-middle-outer-curve (subvec wall-top (* (nth knot-vector 8) wall-section-steps) (+ (* (nth knot-vector 10) wall-section-steps) 3))
               fourth-bl-to-middle-inner-curve (subvec inner-wall-top (* (nth knot-vector 8) wall-section-steps) (+ (* (nth knot-vector 10) wall-section-steps) 3))
               fourth-bl-to-middle-outer-steps (dec (count fourth-bl-to-middle-outer-curve))
               middle-to-index-outer-curve (subvec wall-top (* (nth knot-vector 10) wall-section-steps) (inc (* (nth knot-vector 13) wall-section-steps)))
               middle-to-index-inner-curve (subvec inner-wall-top (* (nth knot-vector 10) wall-section-steps) (inc (* (nth knot-vector 13) wall-section-steps)))
               middle-to-index-outer-steps (dec (count middle-to-index-outer-curve))
               pinky-bl-to-fourth-bl-outer-steps (dec (count pinky-bl-to-fourth-bl-outer-curve))
               pinky-bl-to-fourth-bl-P-zero-w-outer (n-degree-bezier-curve [(main-body-web-post-point-top lastcol cornerrow :bl) (main-body-web-post-point-top 3 cornerrow :br)]
                                                                           pinky-bl-to-fourth-bl-outer-steps)
               pinky-bl-to-fourth-bl-P-one-w-outer (n-degree-bezier-curve [(peek fourth-bl-to-middle-outer-curve) (main-body-web-post-point-top 3 cornerrow :br)]
                                                                          pinky-bl-to-fourth-bl-outer-steps)
               pinky-bl-to-fourth-bl-P-w-one-outer-surface (triangular-coons-surface (main-body-web-post-point-top lastcol cornerrow :bl) (peek fourth-bl-to-middle-outer-curve) (main-body-web-post-point-top 3 cornerrow :br)
                                                                                     (reverse pinky-bl-to-fourth-bl-outer-curve)
                                                                                     pinky-bl-to-fourth-bl-P-zero-w-outer
                                                                                     pinky-bl-to-fourth-bl-P-one-w-outer
                                                                                     pinky-bl-to-fourth-bl-outer-steps pinky-bl-to-fourth-bl-outer-steps
                                                                                     :blending-fn :H-five)
               ;pinky-bl-to-fourth-bl-inner-curve (subvec wall-top (* (nth knot-vector 4) wall-section-steps) (* (nth knot-vector 8) wall-section-steps))
               intersec (two-d-intersection-for-3d (main-body-web-post-point-top lastcol cornerrow :bl) (main-body-web-post-point-top lastcol cornerrow :tl)
                                                   (main-body-web-post-point-top 3 cornerrow :bl) (main-body-web-post-point-top 3 cornerrow :br))
               intersec-web-poit (find-point-on-line-using-x (main-body-web-post-point-top lastcol cornerrow :bl) (main-body-web-post-point-top lastcol cornerrow :tl) (nth intersec 0))
               surf (bicubic-coons-surface (main-body-web-post-point-top lastcol cornerrow :bl) intersec-web-poit (main-body-web-post-point-top 3 cornerrow :bl) (main-body-web-post-point-top 3 cornerrow :br)
                                           (n-degree-bezier-curve [(main-body-web-post-point-top lastcol cornerrow :bl) intersec-web-poit] pinky-bl-to-fourth-bl-outer-steps)
                                           pinky-bl-to-fourth-bl-P-one-w-outer
                                           pinky-bl-to-fourth-bl-outer-curve
                                           (n-degree-bezier-curve [(main-body-web-post-point-top 3 cornerrow :br) intersec-web-poit] pinky-bl-to-fourth-bl-outer-steps)
                                           pinky-bl-to-fourth-bl-outer-steps pinky-bl-to-fourth-bl-outer-steps :boundary-curves-generated true)]

           
           {:front-wall-wall-section wall-section
            :chained-hull-shapes [(chained-hull-to-points (plot-bezier-points pinky-bl-to-fourth-bl-outer-curve (sphere epsilon)) (translate (main-body-web-post-point-top 3 cornerrow :br) (sphere epsilon))
                                                          (plot-bezier-points pinky-bl-to-fourth-bl-inner-curve (sphere epsilon)) (translate (main-body-web-post-point-bottom 3 cornerrow :br) (sphere epsilon))
                                                          pinky-bl-to-fourth-bl-outer-steps)
                                  (chained-hull-to-points (plot-bezier-points fourth-bl-to-middle-outer-curve (sphere epsilon)) (translate (main-body-web-post-point-top 2 cornerrow :br) (sphere epsilon))
                                                          (plot-bezier-points fourth-bl-to-middle-inner-curve (sphere epsilon)) (translate (main-body-web-post-point-bottom 2 cornerrow :br) (sphere epsilon))
                                                          fourth-bl-to-middle-outer-steps)
                                  (chained-hull-to-points (plot-bezier-points middle-to-index-outer-curve (sphere epsilon)) (translate (main-body-web-post-point-top 2 cornerrow :bl) (sphere epsilon))
                                                          (plot-bezier-points middle-to-index-inner-curve (sphere epsilon)) (translate (main-body-web-post-point-bottom 2 cornerrow :bl) (sphere epsilon))
                                                          middle-to-index-outer-steps)]
            }
      ;;      (union
      ;;       (vnf-polyhedron (wall-vnf wall-section
      ;;                                 {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
      ;;       ;(plot-bezier-points c1 (sphere 0.2))
      ;;       ;(color [1 0 0 1](plot-bezier-points nurbs-curve (sphere 0.1)))
      ;;       ;(plot-bezier-points wall-top (sphere 0.2))
      ;;      ;(plot-bezier-points pinky-bl-to-fourth-bl-outer-curve (sphere 0.2))
            
      ;;       ;(translate (nth wall-top (* (nth knot-vector 8) wall-section-steps)) (sphere 0.1))
      ;;       ;(color [1 0 0 1](plot-bezier-points pinky-bl-to-fourth-bl-outer-curve (sphere 0.1)))
      ;;       ;(plot-bezier-points pinky-bl-to-fourth-bl-inner-curve (sphere 0.2))
      ;;       ;; (translate (find-point-on-line-using-x (main-body-web-post-point-top lastcol cornerrow :bl) (main-body-web-post-point-top lastcol cornerrow :tl) (nth intersec 0))
      ;;       ;;            (sphere 1))
      ;;       ;(vnf-polyhedron (vnf_tri_array pinky-bl-to-fourth-bl-P-w-one-outer-surface :reverse true))
            
            
            
      ;;       ;(vnf-polyhedron (vnf-vertex-array pinky-bl-to-fourth-bl-P-w-one-outer-surface :caps false :col-wrap false :reverse true :style :default))
      ;;       )
           
         ))

(comment (spit "things-low/horizontal-first-test.scad"
      (write-scad
       (include "../BOSL2/std.scad")
       (union key-holes
              (front-wall-nurbs 30 30)))))

(defrecord LeftSectionData [vnf-array outer-wall inner-wall outer-floor-points  inner-floor-points trackpad-to-main-body-data
                            thumb-bl-to-tl-outer-curve-fn thumb-tl-to-tr-outer-curve-fn
                            thumb-bl-to-tl-inner-curve-fn thumb-tl-to-tr-inner-curve-fn])
(defn left-section-data [steps]
  (let [wall-section-steps steps
        steps-times-2 (* steps 2)
        left-section-top-right-wall-position    (tps-65-wall-position :tr :north-west)
        left-section-top-mid-wall-position    (tps-65-wall-position :tm :north)
        left-section-top-left-north-wall-position   (tps-65-wall-position :tl :north :offset [0.0000001 0.0000001 0.0])
        left-section-top-left-north-west-wall-position    (tps-65-wall-position :tl :north-west)
        left-section-top-left-west-wall-position (tps-65-wall-position :tl :west :offset [-0.00000001 0.0 0.0])
        left-section-top-left-mid-wall-position  (tps-65-wall-position :tl-lm :west)
        left-section-left-mid-wall-position  (tps-65-to-screen-wall-position :lm :west)
        left-section-bottom-left-mid-south-west-wall-position (tps-65-to-screen-wall-position :bl-lm :south-west)
        left-section-bottom-left-west-wall-position (tps-65-to-screen-wall-position :bl :west :offset [-0.0001 0 0])
        left-section-bottom-left-south-west-wall-position (tps-65-to-screen-wall-position :bl :south-west)
        left-section-bottom-left-south-wall-position (tps-65-to-screen-wall-position :bl :south :offset [0 -0.0001 0])
        left-section-bottom-mid-south-wall-position (tps-65-wall-position :bm :south)
        left-section-bottom-right-south-wall-position (tps-65-wall-position :br :south :xy 1)
        left-section-top-right-control-points   (calculate-control-points left-section-top-right-wall-position)
        left-section-top-mid-control-points   (calculate-control-points  left-section-top-mid-wall-position)
        left-section-top-left-north-control-points   (calculate-control-points left-section-top-left-north-wall-position)
        left-section-top-left-north-west-control-points   (calculate-control-points left-section-top-left-north-west-wall-position)
        left-section-top-left-west-control-points   (calculate-control-points left-section-top-left-west-wall-position)
        left-section-top-left-mid-control-points  (calculate-control-points left-section-top-left-mid-wall-position)
        left-section-left-mid-control-points  (calculate-control-points left-section-left-mid-wall-position)
        left-section-bottom-left-mid-south-west-control-points (calculate-control-points left-section-bottom-left-mid-south-west-wall-position)
        left-section-bottom-left-west-control-points (calculate-control-points left-section-bottom-left-west-wall-position)
        left-section-bottom-left-south-west-control-points (calculate-control-points left-section-bottom-left-south-west-wall-position)
        left-section-bottom-left-south-control-points (calculate-control-points left-section-bottom-left-south-wall-position)
        left-section-bottom-mid-south-control-points (calculate-control-points left-section-bottom-mid-south-wall-position)
        left-section-bottom-right-south-control-points (calculate-control-points left-section-bottom-right-south-wall-position)
        left-section-bottom-right-south-east-control-points (calculate-control-points (tps-65-wall-position :br :south-east :xy 1 :offset [0.00001 0.00001 0]))

        thumb-bl-tl-points (wall-brace-polyhedron-points thumb-bl-place -1 0 :tl :degrees)
        thumb-bl-tr-points (wall-brace-polyhedron-points thumb-bl-place -1 -1 :tr :degrees)
        thumb-bl-br-points (wall-brace-polyhedron-points thumb-bl-place 0 -1 :br :degrees)
        thumb-tl-tl-points (wall-brace-polyhedron-points thumb-tl-place -1 -1 :tl :degrees :offset [-0.001 0.0 0.0])
        thumb-tr-br-control-points (calculate-control-points (thumb-wall-position thumb-tr-place 1 0 :br :xy 4  :slant :no-slant))
        thumb-tr-rm-control-points (calculate-control-points (thumb-wall-position thumb-tr-place 1 0 :rm :xy 4  :slant :no-slant))
        thumb-tr-tr-control-points (calculate-control-points (thumb-wall-position thumb-tr-place 1 -1 :tr :xy 4 :slant :parallel-by-d-opposite))
        index-bottom-br (calculate-control-points (key-wall-position 1 2 1 0 :br :xy 4 :slant :no-slant))
        middle-bottom-bl (calculate-control-points (key-wall-position 2 2 1 -1 :bl :xy 3 :slant :no-slant))
        left-section-top-right-outer  (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words left-section-top-right-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                         3 default-weights-for-vertical-nurbs steps-times-2)
        left-section-top-mid-outer (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words left-section-top-mid-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                      3 default-weights-for-vertical-nurbs steps-times-2)
        left-section-top-left-north-outer (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words left-section-top-left-north-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                             3 default-weights-for-vertical-nurbs steps-times-2)
        left-section-top-left-north-west-outer (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words left-section-top-left-north-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                                  3 default-weights-for-vertical-nurbs steps-times-2)
        left-section-top-left-west-outer (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words left-section-top-left-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                            3 default-weights-for-vertical-nurbs steps-times-2)
        left-section-top-left-mid-west-outer (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words left-section-top-left-mid-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                                3 default-weights-for-vertical-nurbs steps-times-2)
        left-section-bottom-left-mid-south-west-outer (tps-to-screen-side-global-with-first-derivatives-outer-curve left-section-bottom-left-mid-south-west-control-points steps-times-2)

        left-section-bottom-mid-south-outer (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words left-section-bottom-mid-south-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                               3 default-weights-for-vertical-nurbs steps-times-2)


        left-section-bottom-right-south-outer-catmull (catmull-rom-spline-curve [(:opposite-web-post-position-top left-section-bottom-right-south-control-points)
                                                                                 (:web-post-position-top left-section-bottom-right-south-control-points)
                                                                                 (:web-post-position-top thumb-bl-tr-points)
                                                                                 ;(:wall-locate3-point-floor left-section-bottom-mid-south-control-points)
                                                                                 (thumb-web-post-point-top thumb-bl-place :br)]
                                                                                steps-times-2)
        inner-index-bottom-bl-to-thumb-tl-tl-catmull-outer (catmull-rom-spline-curve [(web-post-point-top (partial key-place 0 cornerrow) :tl :radians)
                                                                                      (web-post-point-top (partial key-place 0 cornerrow) :bl :radians)
                                                                                      (web-post-point-top thumb-tl-place :tl :degrees)
                                                                                      (web-post-point-top thumb-tl-place :bl :degrees)]
                                                                                     steps-times-2)
        inner-index-bottom-bm-to-thumb-tl-tm-catmull-outer (catmull-rom-spline-curve [(web-post-point-top (partial key-place 0 cornerrow) :tm :radians)
                                                                                      (web-post-point-top (partial key-place 0 cornerrow) :bm :radians)
                                                                                      (web-post-point-top thumb-tl-place :tm :degrees)
                                                                                      (web-post-point-top thumb-tl-place :bm :degrees)]
                                                                                     steps-times-2)
        inner-index-bottom-br-to-thumb-tl-tr-catmull-outer (catmull-rom-spline-curve [(web-post-point-top (partial key-place 0 cornerrow) :tr :radians)
                                                                                      (web-post-point-top (partial key-place 0 cornerrow) :br :radians)
                                                                                      (web-post-point-top thumb-tl-place :tr :degrees)
                                                                                      (web-post-point-top thumb-tl-place :br :degrees)]
                                                                                     steps-times-2)

        index-bottom-bl-to-thumb-tr-tl-catmull-outer (catmull-rom-spline-curve [(web-post-point-top (partial key-place 1 cornerrow) :tl :radians)
                                                                                (web-post-point-top (partial key-place 1 cornerrow) :bl :radians)
                                                                                (web-post-point-top thumb-tr-place :tl :degrees)
                                                                                (web-post-point-top thumb-tr-place :bl :degrees)]
                                                                               steps-times-2)
        left-section-top-right-north-inner  (n-degree-bezier-curve (get-curve-control-points-by-key-words left-section-top-right-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps-times-2)
        left-section-top-mid-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words left-section-top-mid-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps-times-2)
        left-section-top-left-north-inner  (n-degree-bezier-curve (get-curve-control-points-by-key-words left-section-top-left-north-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps-times-2)
        left-section-top-left-west-inner  (n-degree-bezier-curve (get-curve-control-points-by-key-words left-section-top-left-west-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps-times-2)
        left-section-top-left-mid-west-inner  (n-degree-bezier-curve (get-curve-control-points-by-key-words left-section-top-left-mid-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps-times-2)
        left-section-left-mid-outer (tps-to-screen-side-global-with-first-derivatives-outer-curve left-section-left-mid-control-points steps-times-2)
        left-section-left-mid-inner (tps-to-screen-side-global-with-first-derivatives-inner-curve left-section-left-mid-control-points steps-times-2)
        left-section-bottom-left-mid-south-west-inner (tps-to-screen-side-global-with-first-derivatives-inner-curve left-section-bottom-left-mid-south-west-control-points steps-times-2)
        left-section-bottom-left-west-outer (catmull-rom-spline-curve (get-curve-control-points-by-key-words  left-section-bottom-left-west-control-points tps-65-to-screen-outer-wall-catmull-rom-spline-keywords) steps-times-2)
             ;left-section-bottom-left-west-inner (apply tps-to-screen-side-catmull (conj (vec (get-curve-control-points-by-key-words  left-section-bottom-left-west-control-points inner-wall-catmull-rom-spline-parameters)) steps-times-2))
        left-section-bottom-mid-south-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words left-section-bottom-mid-south-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps-times-2)
        left-section-bottom-right-south-inner-catmull (catmull-rom-spline-curve [;(:opposite-web-post-position-top left-section-bottom-right-south-control-points)
                                                                                 (mapv + [0 -8 10] (:web-post-position-bottom left-section-bottom-right-south-control-points))
                                                                                 ;(mapv + [0 0 10](:web-post-position-bottom left-section-bottom-right-south-control-points))
                                                                                 ;(mapv + [0 0 (- (/ web-thickness 2) (+ tps-65-y-modifier tps-65-depth tps-65-depth-tolerance))] (:web-post-position-bottom left-section-bottom-right-south-control-points)) 

                                                                                 (mapv + [0 0.75 (- (/ web-thickness 2) (+ tps-65-y-modifier tps-65-depth tps-65-depth-tolerance))] (find-point-on-line-using-x tps-65-top-left-inner tps-65-bottom-left-inner (+ (nth tps-65-bottom-left-inner 0) -1)))
                                                                                 (:web-post-position-bottom thumb-bl-tr-points)
                                                                                 (web-post-point-top thumb-bl-place :bl :degrees)

                                                                                 ;(:wall-locate-2-bottom-floor thumb-bl-br-points)
                                                                                 ]
                                                                                steps-times-2
                                                                                :alphaType :chordal)
        inner-index-bottom-bl-to-thumb-tl-tl-catmull-inner (catmull-rom-spline-curve [(web-post-point-top (partial key-place 0 cornerrow) :tl :radians)
                                                                                      (web-post-point-bottom (partial key-place 0 cornerrow) :bl :radians)
                                                                                      (web-post-point-bottom thumb-tl-place :tl :degrees)
                                                                                      (web-post-point-top thumb-tl-place :bl :degrees)]
                                                                                     steps-times-2
                                                                                     :alphaType :chordal)
        inner-index-bottom-bm-to-thumb-tl-tm-catmull-inner (catmull-rom-spline-curve [(web-post-point-bottom (partial key-place 0 cornerrow) :tm :radians)
                                                                                      (web-post-point-bottom (partial key-place 0 cornerrow) :bm :radians)
                                                                                      (web-post-point-bottom thumb-tl-place :tm :degrees)
                                                                                      (web-post-point-bottom thumb-tl-place :bm :degrees)]
                                                                                     steps-times-2)
        inner-index-bottom-br-to-thumb-tl-tr-catmull-inner (catmull-rom-spline-curve [(web-post-point-bottom (partial key-place 0 cornerrow) :tr :radians)
                                                                                      (web-post-point-bottom (partial key-place 0 cornerrow) :br :radians)
                                                                                      (web-post-point-bottom thumb-tl-place :tr :degrees)
                                                                                      (web-post-point-bottom thumb-tl-place :br :degrees)]
                                                                                     steps-times-2
                                                                                     :alphaType :chordal)
        index-bottom-bl-to-thumb-tr-tl-catmull-inner (catmull-rom-spline-curve [(web-post-point-bottom (partial key-place 1 cornerrow) :tl :radians)
                                                                                (web-post-point-bottom (partial key-place 1 cornerrow) :bl :radians)
                                                                                (web-post-point-bottom thumb-tr-place :tl :degrees)
                                                                                (web-post-point-bottom thumb-tr-place :bl :degrees)]
                                                                               steps-times-2)
        left-section-bottom-left-south-west-outer (tps-to-screen-side-global-with-first-derivatives-outer-curve left-section-bottom-left-south-west-control-points steps-times-2)
        left-section-bottom-left-south-west-inner (tps-to-screen-side-global-with-first-derivatives-inner-curve left-section-bottom-left-south-west-control-points steps-times-2)
        left-section-bottom-left-south-outer (tps-to-screen-side-global-with-first-derivatives-outer-curve  left-section-bottom-left-south-control-points steps-times-2)
        left-section-bottom-left-south-inner (catmull-rom-spline-curve  (get-curve-control-points-by-key-words  left-section-bottom-left-south-control-points tps-65-to-screen-inner-wall-catmull-rom-spline-parameters) steps-times-2)
        thumb-bl-tl-points-outer  (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tl-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                     3 default-weights-for-vertical-nurbs steps-times-2)
        thumb-bl-tl-points-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tl-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps-times-2)


        outer-end-curve-tangents (calculate-tangents-for-local-cubic-curve-interpolation-from-tangent
                                  [(:web-post-position-top thumb-tr-br-control-points)
                                   (:web-post-position-top thumb-tr-rm-control-points)
                                   (:web-post-position-top thumb-tr-tr-control-points)
                                   (:web-post-position-top index-bottom-br)
                                   (:web-post-position-top middle-bottom-bl)])
        end-tangent-curve-tangents (calculate-tangents-for-local-cubic-curve-interpolation-from-tangent
                                    [(:point-on-tangent-from-plate thumb-tr-br-control-points)
                                     (:point-on-tangent-from-plate thumb-tr-rm-control-points)
                                     (:point-on-tangent-from-plate thumb-tr-tr-control-points)
                                     (:point-on-tangent-from-plate index-bottom-br)
                                     (:point-on-tangent-from-plate middle-bottom-bl)])
        end-curve-params (local-cubic-curve-interpolation-with-calculated-tangents
                          [(:web-post-position-top thumb-tr-br-control-points)
                           (:web-post-position-top thumb-tr-rm-control-points)
                           (:web-post-position-top thumb-tr-tr-control-points)
                           (:web-post-position-top index-bottom-br)
                           (:web-post-position-top middle-bottom-bl)])
        tt (non-uniform-b-spline (:P end-curve-params) 3 (:U end-curve-params) steps-times-2)
        tt-part (non-uniform-b-spline (:P end-curve-params) 3 (:U end-curve-params) steps-times-2
                                      :u-start (nth (:U end-curve-params) 7) :u-end (nth (:U end-curve-params) 9))
        end-curve-outer (vec (reverse (local-cubic-curve-interpolation-with-tangents-curve [(:web-post-position-top thumb-tr-tr-control-points)
                                                                                            (:web-post-position-top index-bottom-br)]
                                                                                           (subvec  outer-end-curve-tangents 2 4)
                                                                                           steps-times-2)))
        end-tangents-curve-outer (vec (reverse (local-cubic-curve-interpolation-with-tangents-curve [(:point-on-tangent-from-plate thumb-tr-tr-control-points)
                                                                                                     (:point-on-tangent-from-plate index-bottom-br)]
                                                                                                    (subvec  end-tangent-curve-tangents 2 4)
                                                                                                    steps-times-2)))
        inner-end-curve-tangents (calculate-tangents-for-local-cubic-curve-interpolation-from-tangent
                                  (vec (reverse  [(:web-post-position-bottom thumb-tr-br-control-points)
                                                  (:web-post-position-bottom thumb-tr-rm-control-points)
                                                  (:web-post-position-bottom thumb-tr-tr-control-points)
                                                  (:web-post-position-bottom index-bottom-br)
                                                  (:web-post-position-bottom middle-bottom-bl)])))
        inner-end-curve-params (local-cubic-curve-interpolation-with-calculated-tangents
                                [(:web-post-position-bottom thumb-tr-br-control-points)
                                 (:web-post-position-bottom thumb-tr-rm-control-points)
                                 (:web-post-position-bottom thumb-tr-tr-control-points)
                                 (:web-post-position-bottom index-bottom-br)
                                 (:web-post-position-bottom middle-bottom-bl)])
        inner-tangent-end-curve-tangents (calculate-tangents-for-local-cubic-curve-interpolation-from-tangent
                                          (vec (reverse  [(:point-on-tangent-from-plate-bottom thumb-tr-br-control-points)
                                                          (:point-on-tangent-from-plate-bottom thumb-tr-rm-control-points)
                                                          (:point-on-tangent-from-plate-bottom thumb-tr-tr-control-points)
                                                          (:point-on-tangent-from-plate-bottom index-bottom-br)
                                                          (:point-on-tangent-from-plate-bottom middle-bottom-bl)])))
        inner-tangent-end-curve-tangents2 (calculate-tangents-for-local-cubic-curve-interpolation-from-tangent
                                           (vec (reverse  [(:wall-locate-2-top thumb-tr-br-control-points)
                                                           (:wall-locate-2-top thumb-tr-rm-control-points)
                                                           (:wall-locate-2-top thumb-tr-tr-control-points)
                                                           (:wall-locate-2-top index-bottom-br)
                                                           (:wall-locate-2-top middle-bottom-bl)])))
        end-curve-inner (local-cubic-curve-interpolation-with-tangents-curve [(:web-post-position-bottom index-bottom-br)
                                                                                   ;              (:web-post-position-bottom thumb-tr-tr-control-points)
                                                                              (web-post-point-bottom thumb-tr-place :tr :degrees
                                                                                                     :offset [-1 -1 0])]
                                                                             (subvec  inner-end-curve-tangents 1 3)
                                                                             steps-times-2)
        end-tangents-curve-inner  (local-cubic-curve-interpolation-with-tangents-curve [(:point-on-tangent-from-plate-bottom index-bottom-br)
                                                                                        (:point-on-tangent-from-plate-bottom thumb-tr-tr-control-points)]
                                                                                       (subvec  inner-tangent-end-curve-tangents 1 3)
                                                                                       steps-times-2)
        end-tangents-curve-inner2  (local-cubic-curve-interpolation-with-tangents-curve [(:wall-locate-2-top index-bottom-br)
                                                                                         (:wall-locate-2-top thumb-tr-tr-control-points)]
                                                                                        (subvec  inner-tangent-end-curve-tangents2 1 3)
                                                                                        steps-times-2)
        outer-wall-f-control-points (fn [index] [(nth left-section-top-right-outer index)
                                                 (nth left-section-top-mid-outer index)
                                                 (nth left-section-top-left-north-outer index)
                                                      ;(nth left-section-top-left-north-west-outer index)
                                                 (nth left-section-top-left-west-outer index)
                                                 (nth left-section-top-left-mid-west-outer index)
                                                 (nth left-section-left-mid-outer index)
                                                 (nth left-section-bottom-left-mid-south-west-outer index)
                                                 (nth left-section-bottom-left-south-west-outer index)
                                                 (nth left-section-bottom-left-south-outer index)
                                                 (nth left-section-bottom-mid-south-outer index)
                                                 (nth left-section-bottom-right-south-outer-catmull index)
                                                 (nth inner-index-bottom-bl-to-thumb-tl-tl-catmull-outer index)
                                                 (nth inner-index-bottom-bm-to-thumb-tl-tm-catmull-outer index)
                                                 (nth inner-index-bottom-br-to-thumb-tl-tr-catmull-outer index)
                                                 (nth index-bottom-bl-to-thumb-tr-tl-catmull-outer index)
                                                 (nth end-curve-outer index)])
        inner-wall-f-control-points (fn [index] [(nth end-curve-inner index)
                                                 (nth index-bottom-bl-to-thumb-tr-tl-catmull-inner index)
                                                 (nth inner-index-bottom-br-to-thumb-tl-tr-catmull-inner index)
                                                 (nth inner-index-bottom-bm-to-thumb-tl-tm-catmull-inner index)
                                                 (nth inner-index-bottom-bl-to-thumb-tl-tl-catmull-inner index)
                                                 (nth left-section-bottom-right-south-inner-catmull index)
                                                 (nth left-section-bottom-mid-south-inner index)
                                                 (nth left-section-bottom-left-south-west-inner index)
                                                 (nth left-section-bottom-left-mid-south-west-inner index)
                                                 (nth left-section-left-mid-inner index)
                                                 (nth left-section-top-left-mid-west-inner index)
                                                 (nth left-section-top-left-west-inner index)
                                                 (nth left-section-top-left-north-inner index)
                                                 (nth left-section-top-mid-inner index)
                                                 (nth left-section-top-right-north-inner index)])
        inner-wall-tangents-fn (fn [index]
                                 [;(mapv - (:web-post-position-bottom thumb-tr-tr-control-points) (:web-post-position-bottom index-bottom-br))
                                  (mapv -  (nth end-curve-inner index) (nth end-tangents-curve-inner2 index))
                                  (mapv - (nth index-bottom-bl-to-thumb-tr-tl-catmull-inner index) (nth end-curve-inner index))
                              ;;     (mapv - (nth inner-index-bottom-br-to-thumb-tl-tr-catmull-inner index)
                              ;;           (nth index-bottom-bl-to-thumb-tr-tl-catmull-inner index))
                                  (mapv - (nth inner-index-bottom-bm-to-thumb-tl-tm-catmull-inner index)
                                        (nth inner-index-bottom-br-to-thumb-tl-tr-catmull-inner index))
                                  (mapv - (nth inner-index-bottom-bm-to-thumb-tl-tm-catmull-inner index)
                                        (nth inner-index-bottom-br-to-thumb-tl-tr-catmull-inner index))
                                  (mapv - (nth inner-index-bottom-bl-to-thumb-tl-tl-catmull-inner index)
                                        (nth inner-index-bottom-bm-to-thumb-tl-tm-catmull-inner index))
                                 ;(mapv - (nth left-section-bottom-right-south-inner-catmull index) (nth inner-index-bottom-bl-to-thumb-tl-tl-catmull-inner index))
                                  ;(mapv + (mapv - (nth left-section-bottom-right-south-inner-catmull index) (find-point-on-line-using-x (nth left-section-bottom-right-south-inner-catmull index) (nth inner-index-bottom-bl-to-thumb-tl-tl-catmull-inner index) (+ (nth (nth inner-index-bottom-bl-to-thumb-tl-tl-catmull-inner index) 0) 1))))
                                  ;(mapv + [0.0 0 0.0] (mapv - tps-65-bottom-left-inner (find-point-on-line-using-x tps-65-top-left-inner tps-65-bottom-left-inner (+ (nth tps-65-bottom-left-inner 0) -1))))
                                  (mapv - (nth inner-index-bottom-bl-to-thumb-tl-tl-catmull-inner index)
                                        (nth inner-index-bottom-bm-to-thumb-tl-tm-catmull-inner index))
                                  (mapv - tps-65-mid-left-inner tps-65-bottom-left-inner)
                                 ;(mapv - (nth left-section-bottom-mid-south-inner index) (nth left-section-bottom-right-south-inner-catmull index))
                                  (mapv - (nth left-section-bottom-left-south-west-inner index) (nth left-section-bottom-mid-south-inner index))
                                  (mapv - (nth left-section-left-mid-inner index) (nth left-section-bottom-left-mid-south-west-inner index))
                                  (mapv - (nth left-section-top-left-mid-west-inner index) (nth left-section-left-mid-inner index))
                                  (mapv - (nth left-section-top-left-west-inner index) (nth left-section-top-left-mid-west-inner index))
                                  (mapv - (nth left-section-top-left-north-inner index)  (nth left-section-top-left-west-inner index))
                                  (mapv - (nth left-section-top-mid-inner index) (nth left-section-top-left-north-inner index))
                                  (mapv - (nth left-section-top-right-north-inner index) (nth left-section-top-mid-inner index))
                                  (mapv - (nth left-section-top-right-north-inner index) (nth left-section-top-mid-inner index))])

        inner-wall-f-params (vec (for [index (range (inc steps-times-2))]
                                   (global-curve-interp-with-calculated-first-derivatives
                                    (inner-wall-f-control-points index)
                                    (inner-wall-tangents-fn index)
                                    2
                                    :point-paramater-calculation-method :circular)))
        inner-wall (vec (for [index (range (inc steps-times-2))
                              :let [outer-steps (* wall-section-steps 7)
                                    params (nth inner-wall-f-params index)
                                    P (:P params)
                                    U (:U params)]]
                          (non-uniform-b-spline P 2 U outer-steps)))
        outer-wall-f-tangents (fn [index] [(mapv - (nth left-section-top-mid-outer index) (nth left-section-top-right-outer index))
                                           (mapv - (nth left-section-top-left-north-outer index) (nth left-section-top-mid-outer index))
                                           (mapv - (nth left-section-top-left-north-west-outer index) (nth left-section-top-mid-outer index))
                                           (mapv - (nth left-section-top-left-mid-west-outer index) (nth left-section-top-left-west-outer index))
                                           (mapv - (nth left-section-left-mid-outer index) (nth left-section-top-left-mid-west-outer index))
                                           (mapv - (nth left-section-bottom-left-mid-south-west-outer index) (nth left-section-left-mid-outer index))
                                          ;(mapv - tps-65-top-left-outer tps-65-top-mid-outer) 
                                           (mapv - (nth left-section-bottom-left-south-west-outer index) (nth left-section-bottom-left-mid-south-west-outer index))
                                           (mapv - (nth left-section-bottom-left-south-outer index) (nth left-section-bottom-left-mid-south-west-outer index))
                                          ;(mapv - tps-65-top-left-outer tps-65-top-mid-outer)
                                          ; (mapv - (nth left-section-bottom-left-south-outer index) (nth left-section-bottom-left-south-west-outer index)) 
                                           (mapv - (nth left-section-bottom-mid-south-outer index) (nth left-section-bottom-left-south-outer index))
                                          ;(mapv - (nth left-section-bottom-mid-south-outer index) (nth left-section-bottom-left-south-outer index)) 
                                           (mapv -  tps-65-bottom-left-outer tps-65-mid-left-outer)
                                           (mapv - (find-point-on-line-using-x tps-65-top-left-outer tps-65-bottom-left-outer (+ (nth tps-65-bottom-left-outer 0) 10)) tps-65-bottom-left-outer)
                                           (mapv - (nth inner-index-bottom-bm-to-thumb-tl-tm-catmull-outer index) (nth inner-index-bottom-bl-to-thumb-tl-tl-catmull-outer index))
                                           (mapv - (nth inner-index-bottom-br-to-thumb-tl-tr-catmull-outer index) (nth inner-index-bottom-bm-to-thumb-tl-tm-catmull-outer index))
                                           (mapv - (nth inner-index-bottom-br-to-thumb-tl-tr-catmull-outer index) (nth inner-index-bottom-bm-to-thumb-tl-tm-catmull-outer index))
                                           ;(mapv - (nth index-bottom-bl-to-thumb-tr-tl-catmull-outer index) (nth inner-index-bottom-br-to-thumb-tl-tr-catmull-outer index))
                                           (mapv - (nth end-curve-outer index) (nth index-bottom-bl-to-thumb-tr-tl-catmull-outer index))
                                           (mapv - (nth end-tangents-curve-outer index) (nth end-curve-outer index))])             outer-wall-f-point-paramater-calculation-method :chordal
        outer-wall-f-params (vec (for [index (range (inc steps-times-2))]
                                   (global-curve-interp-with-calculated-first-derivatives
                                    (outer-wall-f-control-points index)
                                    (outer-wall-f-tangents index)
                                    2
                                    :magnitude-estimation-method :arc

                                    :point-paramater-calculation-method outer-wall-f-point-paramater-calculation-method)))
        outer-wall (vec (for [index (range (inc steps-times-2))
                              :let [outer-steps (* wall-section-steps 16)
                                    params (nth outer-wall-f-params index)
                                    P (:P params)
                                    U (:U params)]]
                          (non-uniform-b-spline P 2 U outer-steps)))
        bottom-points (outer-wall-f-control-points steps-times-2)
        outer-wall-bottom-params (peek outer-wall-f-params)
        outer-wall-bottom-points (:P outer-wall-bottom-params)
        outer-wall-bottom-U (:U outer-wall-bottom-params)
        outer-wall-bottom-uk ((get-function-for-u-k-values outer-wall-f-point-paramater-calculation-method) (dec (count bottom-points)) bottom-points)
        outer-wall-top-outer-params (nth outer-wall-f-params 0)
        outer-wall-bottom-params (peek outer-wall-f-params)
        inner-wall-top-params (nth inner-wall-f-params 0)
        inner-wall-bottom-params (peek inner-wall-f-params)
        outer-wall-trackpad-to-keys-gap-border-outer-fn (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-wall-top-outer-params)
                                                                                                                          (:P outer-wall-top-outer-params)
                                                                                                                          20 21 steps))
        outer-wall-trackpad-to-keys-gap-border-tangent-outer-fn (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 outer-wall-bottom-U
                                                                                                                                  outer-wall-bottom-points
                                                                                                                                  20 21 steps))
        thumb-tl-to-tr-outer-curve-fn (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-wall-bottom-params)
                                                                                                        (:P outer-wall-bottom-params)
                                                                                                        26 27 steps))
        inner-wall-trackpad-to-keys-gap-border-outer-fn (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-wall-top-params)
                                                                                                                          (:P inner-wall-top-params)
                                                                                                                          8 9 steps))
        inner-wall-trackpad-to-keys-gap-border-tangent-outer-fn (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-wall-bottom-params)
                                                                                                                                  (:P inner-wall-bottom-params)
                                                                                                                                  8 9 steps))
        thumb-tl-to-tr-inner-curve-fn (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-wall-bottom-params)
                                                                                                        (:P inner-wall-bottom-params)
                                                                                                        2 3 steps))
        
        thumb-outer-points-fn (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 outer-wall-bottom-U outer-wall-bottom-points
                                                                                                18 19 steps :reverse-curve true))
        thumb-inner-points-fn (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-wall-bottom-params)
                                                                                                (:P inner-wall-bottom-params)
                                                                                                10 11 steps :reverse-curve true))
        trackpad-to-main-body-data (trackpad-to-main-body steps :thumb-side-border-curve-fn-outer (partial outer-wall-trackpad-to-keys-gap-border-outer-fn)
                                                          :thumb-side-border-tangent-curve-fn-outer outer-wall-trackpad-to-keys-gap-border-tangent-outer-fn
                                                          :thumb-side-border-curve-fn-inner inner-wall-trackpad-to-keys-gap-border-outer-fn
                                                          :thumb-side-border-tangent-curve-fn-inner inner-wall-trackpad-to-keys-gap-border-tangent-outer-fn)
        vnf-array (wall-vnf-array outer-wall inner-wall
                                  {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :quincunx})
         ] 
    {:left-section-data (LeftSectionData. vnf-array outer-wall inner-wall  (filter #(zero? (nth % 2))(peek outer-wall)) (peek inner-wall)  trackpad-to-main-body-data
                                          outer-wall-trackpad-to-keys-gap-border-tangent-outer-fn thumb-tl-to-tr-outer-curve-fn
                                          inner-wall-trackpad-to-keys-gap-border-tangent-outer-fn thumb-tl-to-tr-inner-curve-fn)
     :thumb-outer-points-fn thumb-outer-points-fn
     :thumb-inner-points-fn thumb-inner-points-fn
     :inner-index-to-index-connector-outer-curve-fn (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-wall-top-outer-params)
                                                                        (:P outer-wall-top-outer-params)
                                                                        26 27 steps))
     :inner-index-to-index-connector-inner-curve-fn (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-wall-top-params)
                                                                                                     (:P inner-wall-top-params)
                                                                                                     2 3 steps :reverse-curve true))}))

(defn thumb-to-left-section [steps left-section-to-thumb-outer-points-fn left-section-to-thumb-inner-points-fn]
  (let [thumb-outer-points (left-section-to-thumb-outer-points-fn steps)
        thumb-inner-points (vec (reverse (left-section-to-thumb-inner-points-fn steps)))
        thumb-bl-tm-north-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 1 :tm :xy 4 :slant :no-slant))
        thumb-bl-tm-north-west-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tm-north-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                              3 default-weights-for-vertical-nurbs steps)
        left-section-bottom-mid-south-wall-position (tps-65-wall-position :bm :south)
        left-section-bottom-mid-south-control-points (calculate-control-points left-section-bottom-mid-south-wall-position)
        thumb-bl-tr-tm-north-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tr-tm :xy 4 :slant :no-slant))
        thumb-tl-tl-north-control-points (calculate-control-points (thumb-wall-position thumb-tl-place 0 1 :tl :xy 4))
        thumb-bl-tr-north-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tr :xy 4 :offset [0.00000001 0.0 0.0]  :slant :no-slant))
        thumb-bl-bl-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 0 :bl :xy 4 :slant :no-slant :offset [-0.000001 0.0 0.0]))
        thumb-bl-lm-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 0 :lm :xy 4 :slant :no-slant))
        thumb-bl-tl-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 0 :tl :xy 4 :offset [(- epsilon) 0 0] :slant :no-slant))
        thumb-bl-tl-north-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 1 :tl :xy 4 :slant :no-slant))
        thumb-bl-tl-north-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tl :xy 4 :offset [0 epsilon 0] :slant :no-slant))
        thumb-bl-bl-west-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-bl-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                        3 default-weights-for-vertical-nurbs steps)
        thumb-bl-lm-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-lm-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                   3 default-weights-for-vertical-nurbs steps)
        thumb-bl-tl-west-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tl-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                        3 default-weights-for-vertical-nurbs steps)
        thumb-bl-tl-north-west-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tl-north-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                              3 default-weights-for-vertical-nurbs steps)
        thumb-bl-tl-north-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tl-north-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                         3 default-weights-for-vertical-nurbs steps)
        thumb-bl-tr-tm-north-west-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tr-tm-north-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                                 3 default-weights-for-vertical-nurbs steps)
        thumb-tl-tl-north-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-tl-tl-north-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps)
        thumb-bl-tr-tm-north-west-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tr-tm-north-west-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps)
        thumb-bl-tr-north-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tr-north-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps) 
        thumb-bl-bl-west-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-bl-west-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps)
        thumb-bl-lm-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-lm-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps)
        thumb-bl-tl-west-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tl-west-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps)
        thumb-bl-tl-north-west-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tl-north-west-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps)
        thumb-bl-tl-north-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tl-north-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps)
        thumb-bl-tm-north-west-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tm-north-west-control-points inner-wall-curve-bezier-cubic-nurbs-keywords) steps)
        cross-section-steps (* steps 6)
        outer-thumb-wall (vec (for [index (range (inc steps))]
                                (global-curve-interp-with-calculated-first-derivatives-curve
                                 [;(nth thumb-inner-points  index)
                                  (nth thumb-outer-points  index)
                                  (nth thumb-bl-tm-north-west-outer-curve  index)
                                  (nth  thumb-bl-tl-north-outer-curve index)
                                  (nth  thumb-bl-tl-west-outer-curve index)
                                  (nth  thumb-bl-lm-outer-curve index)]
                                 [;(mapv - (nth thumb-outer-points  index) (nth thumb-inner-points index))
                                  (mapv - (nth thumb-bl-tm-north-west-outer-curve  index) (nth thumb-outer-points  index))
                                       ;(mapv - (:wall-locate3-point-floor left-section-bottom-mid-south-control-points) (:wall-locate3-point left-section-bottom-mid-south-control-points))
                                  (mapv - (nth  thumb-bl-tl-north-outer-curve index) (nth thumb-outer-points  index))
                                  (mapv - (nth  thumb-bl-tl-west-outer-curve index) (nth  thumb-bl-tm-north-west-outer-curve index))
                                  (mapv - (nth  thumb-bl-lm-outer-curve index) (nth  thumb-bl-tl-north-west-outer-curve index))
                                  (mapv - (nth thumb-bl-bl-west-outer-curve index) (nth  thumb-bl-lm-outer-curve index))]
                                 2
                                 cross-section-steps
                                 :point-paramater-calculation-method :chordal)))
        inner-thumb-wall (vec (for [index (range (inc steps))]
                                (global-curve-interp-with-calculated-first-derivatives-curve
                                 [(nth thumb-bl-lm-inner-curve index)
                                  (nth thumb-bl-tl-west-inner-curve index)
                                  (nth thumb-bl-tl-north-inner-curve index)
                                  (nth thumb-bl-tm-north-west-inner-curve index)
                                  (nth thumb-bl-tr-tm-north-west-inner-curve index)
                                  (nth thumb-bl-tr-north-inner-curve index)
                                       ;(nth thumb-inner-points index)
                                  ]
                                 [(mapv - (nth thumb-bl-lm-inner-curve index) (nth thumb-bl-bl-west-inner-curve index))
                                  (mapv - (nth thumb-bl-tl-west-inner-curve index) (nth thumb-bl-lm-inner-curve index))
                                  (mapv - (nth thumb-bl-tm-north-west-inner-curve index) (nth thumb-bl-tl-north-west-inner-curve index))
                                  (mapv - (nth thumb-bl-tr-tm-north-west-inner-curve index) (nth thumb-bl-tm-north-west-inner-curve index)) 
                                  (mapv - (nth thumb-bl-tr-north-inner-curve index) (nth thumb-bl-tr-tm-north-west-inner-curve index))
                                  (mapv - (nth thumb-inner-points index) (nth thumb-bl-tr-north-inner-curve index)) 
                                  ]
                                 2
                                 cross-section-steps)))
        local-outer-thumb-wall (vec (for [index (range (inc steps))]
                                      (local-cubic-curve-interpolation-with-tangents-curve
                                       [
                                             ;(nth thumb-bl-tr-tm-north-west-inner-curve index)
                                        (nth thumb-bl-tr-north-inner-curve index)
                                        (nth thumb-outer-points  index)
                                        (nth thumb-bl-tm-north-west-outer-curve  index)
                                        (nth  thumb-bl-tl-north-outer-curve index)
                                        (nth  thumb-bl-tl-north-outer-curve index)
                                        (nth  thumb-bl-tl-west-outer-curve index)
                                        (nth  thumb-bl-lm-outer-curve index)]
                                       [
                                        (mapv - (nth thumb-bl-tr-north-inner-curve index) (nth thumb-bl-tr-tm-north-west-inner-curve index)) 
                                        (mapv - (nth thumb-bl-tm-north-west-outer-curve  index) (nth thumb-bl-tr-north-inner-curve index))
                                        (mapv - (nth  thumb-bl-tl-north-outer-curve index) (nth thumb-bl-tm-north-west-outer-curve  index))
                                        (mapv - (nth  thumb-bl-tl-north-west-outer-curve index) (nth thumb-bl-tm-north-west-outer-curve  index))
                                        (mapv - (nth  thumb-bl-tl-west-outer-curve index) (nth  thumb-bl-tm-north-west-outer-curve index))
                                        (mapv - (nth  thumb-bl-lm-outer-curve index) (nth  thumb-bl-tl-north-west-outer-curve index))
                                        (mapv - (nth thumb-bl-bl-west-outer-curve index) (nth  thumb-bl-lm-outer-curve index))]
                                       (* cross-section-steps 2))))
        ]
    {:wall-vnf (walls-to-vnf [local-outer-thumb-wall inner-thumb-wall] :args {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}) 
               :outer-floor-points (peek local-outer-thumb-wall) :inner-thumb-points (peek inner-thumb-wall)
               }
    ))

(comment (spit "things-low/left-test.scad"
      (write-scad
       (include "../BOSL2/std.scad")
       (let [steps 30
             thumb-bl-tm-north-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 1 :tm :xy 4 :slant :no-slant))
             thumb-bl-tm-north-west-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tm-north-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                                   3 default-weights-for-vertical-nurbs steps)
             {left-section-data :left-section-data
              thumb-outer-points-fn :thumb-outer-points-fn
              thumb-inner-points-fn :thumb-inner-points-fn} (left-section-data steps)
             {left-section-vnf-array :vnf-array
              trackpad-to-main-body-data :trackpad-to-main-body-data
              thumb-bl-to-tl-outer-curve-fn :thumb-bl-to-tl-outer-curve-fn
              thumb-tl-to-tr-outer-curve-fn :thumb-tl-to-tr-outer-curve-fn
              thumb-bl-to-tl-inner-curve-fn :thumb-bl-to-tl-inner-curve-fn
              thumb-tl-to-tr-inner-curve-fn :thumb-tl-to-tr-inner-curve-fn} left-section-data

             {thumb-bl-to-tl-vnf :thumb-bl-to-tl-vnf
              thumb-tl-to-tr-vnf :thumb-tl-to-tr-vnf} (thumb-connecter-1-row steps :thumb-bl-to-tl-P-u-zero-outer (vec (reverse (thumb-bl-to-tl-outer-curve-fn steps)))
                                                                             :thumb-tl-to-tr-P-u-zero-outer (vec (reverse (thumb-tl-to-tr-outer-curve-fn steps)))
                                                                             :thumb-bl-to-tl-P-u-one-inner (thumb-bl-to-tl-inner-curve-fn steps) :thumb-tl-to-tr-P-u-one-inner (thumb-tl-to-tr-inner-curve-fn steps))
             ]
         (union
          key-holes

          (tps-65-place (difference  tps-65-mount-new
                                     tps-65
                                     tps-65-mount-cutout
                                     (translate [0 0 (+ 0 1)] tps-65-mount-main-cutout)))
          (vnf-polyhedron  (thumb-to-left-section steps thumb-outer-points-fn thumb-inner-points-fn)) 
          (vnf-polyhedron thumb-bl-to-tl-vnf)
          (vnf-polyhedron thumb-tl-to-tr-vnf)
          (vnf-polyhedron (:trackpad-to-main-body-vnf trackpad-to-main-body-data))
          (vnf-polyhedron left-section-vnf-array) 
          )))))

(defrecord FractylRightWallData [fractyl-right-wall-vnf bottom-outer bottom-inner key-gap-outer-curve-fn-coll 
                                  key-gap-inner-curve-fn-coll])
(defn fractyl-right-wall [wall-cross-section-steps wall-section-steps ]
(let [pinky-row-2-bl-south-wall-position  (key-wall-position lastcol 2 0 -1 :bl  :slant :no-slant :offset [0 0.000001 0])
pinky-row-2-bm-south-wall-position  (key-wall-position lastcol 2 0 -1 :bm :slant :no-slant)
pinky-row-2-br-bm-south-wall-position  (key-wall-position lastcol 2 0 -1 :br-bm :slant :no-slant)
pinky-row-2-br-south-wall-position  (key-wall-position lastcol 2 0 -1 :br :slant :no-slant :offset [0 (- epsilon) 0])
pinky-row-2-br-south-east-wall-position  (key-wall-position lastcol 2 1 -1 :br  :slant :no-slant :offset [(- epsilon) (- epsilon) 0])
pinky-row-2-br-east-wall-position  (key-wall-position lastcol 2 1 0 :br  :slant :no-slant)
pinky-row-2-rm-east-wall-position  (key-wall-position lastcol 2 1 0 :rm :slant :no-slant)
pinky-row-2-tr-east-wall-position  (key-wall-position lastcol 2 1 0 :tr)
pinky-row-1-br-east-wall-position  (key-wall-position lastcol 1 1 0 :br) 
pinky-row-1-rm-east-wall-position  (key-wall-position lastcol 1 1 0 :rm) 
pinky-row-1-tr-east-wall-position  (key-wall-position lastcol 1 1 0 :tr) 
pinky-row-0-br-east-wall-position  (key-wall-position lastcol 0 1 0 :br) 
pinky-row-0-rm-east-wall-position  (key-wall-position lastcol 0 1 0 :rm)
pinky-row-0-tr-east-wall-position  (key-wall-position lastcol 0 1 0 :tr :offset [0.0000001 0.0 0.0] :slant :no-slant)
pinky-row-0-tr-north-east-wall-position  (key-wall-position lastcol 0 1 1 :tr :offset [0.0000001 0.0000001 0.0] :slant :no-slant)
pinky-row-0-tr-north-wall-position  (key-wall-position lastcol 0 0 1 :tr)
pinky-row-0-tm-north-wall-position  (key-wall-position lastcol 0 0 1 :tm)
pinky-row-0-tl-north-wall-position  (key-wall-position lastcol 0 0 1 :tl)
ring-row-0-tr-north-wall-position  (key-wall-position 3 0 0 1 :tr)
      
pinky-row-2-bl-south (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :bl  :slant :no-slant :offset [0 0.000001 0]))
      pinky-row-2-bm-south (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :bm :slant :no-slant))
      pinky-row-2-br-bm-south (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :br-bm :slant :no-slant))
      pinky-row-2-br-south (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :br :slant :no-slant :offset [0 (- epsilon) 0]))
      pinky-row-2-br-south-east (wall-cross-section-parameter (key-wall-position lastcol 2 1 -1 :br  :slant :no-slant :offset [(- epsilon) (- epsilon) 0]))
      pinky-row-2-br-east (wall-cross-section-parameter (key-wall-position lastcol 2 1 0 :br  :slant :no-slant ))
      pinky-row-2-rm-east (wall-cross-section-parameter (key-wall-position lastcol 2 1 0 :rm :slant :no-slant))
      pinky-row-2-tr-east (wall-cross-section-parameter (key-wall-position lastcol 2 1 0 :tr))  
      pinky-row-1-br-east (wall-cross-section-parameter (key-wall-position lastcol 1 1 0 :br)
                                                        :outer-wall-parameters
                                                        (outer-wall-vertical-nurbs-parameters :knot-vector-calculation-method :natural
                                                                                              :point-paramater-calculation-method :circular))
      pinky-row-1-rm-east (wall-cross-section-parameter (key-wall-position lastcol 1 1 0 :rm)
                                                        :outer-wall-parameters
                                                        (outer-wall-vertical-nurbs-parameters :knot-vector-calculation-method :natural
                                                                                              :point-paramater-calculation-method :circular))
      pinky-row-1-tr-east (wall-cross-section-parameter (key-wall-position lastcol 1 1 0 :tr)
                                                        :outer-wall-parameters (outer-wall-vertical-nurbs-parameters :knot-vector-calculation-method :natural) 
                                                        :inner-wall-parameters (inner-wall-vertical-bezier-parameters :control-point-keywords inner-wall-curve-bezier-quadratic-keywords))
      pinky-row-0-br-east (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :br )
                                                        :inner-wall-parameters (inner-wall-vertical-bezier-parameters :control-point-keywords inner-wall-curve-bezier-quadratic-keywords))
      pinky-row-0-rm-east (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :rm ))
      pinky-row-0-tr-east (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :tr :offset [0.0000001 0.0 0.0] :slant :no-slant))
      pinky-row-0-tr-north-east (wall-cross-section-parameter (key-wall-position lastcol 0 1 1 :tr :offset [0.0000001 0.0000001 0.0] :slant :no-slant))
      pinky-row-0-tr-north (wall-cross-section-parameter (key-wall-position lastcol 0 0 1 :tr))
      pinky-row-0-tm-north (wall-cross-section-parameter (key-wall-position lastcol 0 0 1 :tm))
      pinky-row-0-tl-north (wall-cross-section-parameter (key-wall-position lastcol 0 0 1 :tl))
      ring-row-0-tr-north (wall-cross-section-parameter (key-wall-position 3 0 0 1 :tr))
      wall-cross-section-parameters [;pinky-row-2-bl-south
                                     pinky-row-2-bm-south
                                     pinky-row-2-br-south
                                     pinky-row-2-br-east
                                     ;pinky-row-2-rm-east
                                      pinky-row-2-tr-east
                                      pinky-row-1-br-east
                                      pinky-row-1-rm-east
                                      pinky-row-1-tr-east
                                      pinky-row-0-br-east
                                      pinky-row-0-rm-east
                                      pinky-row-0-tr-east
                                      pinky-row-0-tr-north
                                      pinky-row-0-tm-north
                                     ]
      tangents [;(wall-cross-section-tangent-parameter pinky-row-2-bm-south  pinky-row-2-bl-south)
                (wall-cross-section-tangent-parameter pinky-row-2-bm-south  pinky-row-2-bl-south)
                (wall-cross-section-tangent-parameter pinky-row-2-br-south   pinky-row-2-bm-south)
                ;(wall-cross-section-tangent-parameter pinky-row-2-rm-east pinky-row-2-br-east)
                (wall-cross-section-tangent-parameter pinky-row-2-tr-east pinky-row-2-br-east)
                (wall-cross-section-tangent-parameter pinky-row-2-tr-east pinky-row-2-br-east)
                 (wall-cross-section-tangent-parameter pinky-row-1-rm-east pinky-row-1-br-east)
                 (wall-cross-section-tangent-parameter pinky-row-1-tr-east pinky-row-1-rm-east)
                 (wall-cross-section-tangent-parameter pinky-row-1-tr-east pinky-row-1-rm-east)
                 (wall-cross-section-tangent-parameter pinky-row-0-rm-east pinky-row-0-br-east)
                 (wall-cross-section-tangent-parameter pinky-row-0-tr-east pinky-row-0-rm-east)
                 (wall-cross-section-tangent-parameter pinky-row-0-tr-north pinky-row-0-rm-east) 
                 (wall-cross-section-tangent-parameter pinky-row-0-tl-north pinky-row-0-tr-north-east)
                 (wall-cross-section-tangent-parameter pinky-row-0-tl-north pinky-row-0-tm-north)
                ]
      point-paramater-calculation-method :chordal
      magnitude-estimation-method :chord
      wall-section (wall-section
                    (wall-section-parameter
                     wall-cross-section-parameters
                     (global-curve-interp-with-first-derivatives-parameters
                      tangents
                      2
                      :point-paramater-calculation-method point-paramater-calculation-method
                      :magnitude-estimation-method magnitude-estimation-method)
                     :calculation-order :vertical-first)
                    wall-cross-section-steps wall-section-steps)
      top-outer (nth (:outer-wall wall-section) 0)
      top-inner (reverse (nth (:inner-wall wall-section) 0))
      top-cap-steps (dec (count top-outer))
      top-cap (vec (for [index (range (inc top-cap-steps))]
                     (n-degree-bezier-curve 
                      [(nth top-outer index)
                      (nth top-inner index)]
                      1)))
      wall-positions [pinky-row-2-bm-south-wall-position
                      pinky-row-2-br-south-wall-position
                      pinky-row-2-br-east-wall-position
                      pinky-row-2-tr-east-wall-position
                      pinky-row-1-br-east-wall-position
                      pinky-row-1-rm-east-wall-position
                      pinky-row-1-tr-east-wall-position
                      pinky-row-0-br-east-wall-position
                      pinky-row-0-rm-east-wall-position
                      pinky-row-0-tr-east-wall-position
                      pinky-row-0-tr-north-wall-position
                      pinky-row-0-tm-north-wall-position
                      ]
      wall-position-tangents-position [
                [pinky-row-2-bm-south-wall-position  pinky-row-2-bl-south-wall-position]
                [pinky-row-2-br-south-wall-position pinky-row-2-bm-south-wall-position]
                [ pinky-row-2-tr-east-wall-position pinky-row-2-br-east-wall-position]
                [ pinky-row-2-tr-east-wall-position pinky-row-2-br-east-wall-position]
                [ pinky-row-1-rm-east-wall-position pinky-row-1-br-east-wall-position]
                [ pinky-row-1-tr-east-wall-position pinky-row-1-rm-east-wall-position]
                [ pinky-row-1-tr-east-wall-position pinky-row-1-rm-east-wall-position]
                [ pinky-row-0-rm-east-wall-position pinky-row-0-br-east-wall-position]
                [ pinky-row-0-tr-east-wall-position pinky-row-0-rm-east-wall-position]
                [pinky-row-0-tr-north-wall-position pinky-row-0-rm-east-wall-position]
                [pinky-row-0-tl-north-wall-position pinky-row-0-tr-north-east-wall-position]
                [pinky-row-0-tl-north-wall-position pinky-row-0-tm-north-wall-position]
                ]
      outer-coll (global-curve-interp-with-calculated-first-derivatives
                  (mapv #(:web-post-position-top (calculate-control-points %)) wall-positions)
                  (mapv #(mapv - (:web-post-position-top (calculate-control-points (nth % 0))) 
                               (:web-post-position-top (calculate-control-points(nth % 1))))
                        wall-position-tangents-position)
                  2 
                  :point-paramater-calculation-method point-paramater-calculation-method
                  :magnitude-estimation-method magnitude-estimation-method
                  )
      inner-coll (global-curve-interp-with-calculated-first-derivatives
                  (mapv #(:web-post-position-bottom (calculate-control-points %)) wall-positions)
                  (mapv #(mapv - (:web-post-position-bottom (calculate-control-points (nth % 0)))
                               (:web-post-position-bottom (calculate-control-points (nth % 1))))
                        wall-position-tangents-position)
                  2
                  top-cap-steps)
      ]
                    (FractylRightWallData. (vnf-join [(wall-vnf wall-section {:caps false :cap1 false :cap2 true :col-wrap true :row-wrap false :reverse false :style :concave})
                                                      (vnf-vertex-array top-cap :caps false :col-wrap false)])
                                           (peek (:outer-wall wall-section)) (peek (:inner-wall wall-section))
                                           [(fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-coll) (:P outer-coll) 6 7 steps))
                                            (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-coll) (:P outer-coll) 12 13 steps))] 
                                           [(fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-coll) (:P inner-coll) 6 7 steps))
                                            (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-coll) (:P inner-coll) 12 13 steps))]
                                           )
;;                     (union 
                     
;;                      (vnf-polyhedron (vnf-join [(wall-vnf wall-section {:caps false :cap1 false :cap2 true :col-wrap true :row-wrap false :reverse false :style :concave})
;;                                                (vnf-vertex-array top-cap :caps false :col-wrap false)]))
;;                      (plot-bezier-points (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-coll) (:P outer-coll) 6 7 (* wall-section-steps 2)) (sphere 1))
;;                      (plot-bezier-points (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-coll) (:P outer-coll) 12 13 (* wall-section-steps 2)) (sphere 1))
;;                      (plot-bezier-points (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-coll) (:P inner-coll) 6 7 (* wall-section-steps 2)) (sphere 1))
;; (plot-bezier-points (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-coll) (:P inner-coll) 12 13 (* wall-section-steps 2)) (sphere 1))
                           
;;                            ;(plot-bezier-points (nth (:outer-wall wall-section) 0) (sphere 0.5))
;;                            ;      (plot-bezier-points (nth (:inner-wall wall-section) 0) (sphere 0.5))
;;                            )
                    )  
  )

(comment (spit "things-low/fractyl-right-wall-test.scad"
      (write-scad
       (include include-bosl2)
       (let []
         (union
        (fractyl-right-wall 5 5) 
        ;(front-wall-nurbs 10 30)
          (fractyl-back-wall 5 5)
        key-holes
        )))))



(comment (spit "things-low/fractyl-case-walls-test.scad"
      (write-scad
       (include include-bosl2)
       (let [steps 30
             
             wall-cross-section-steps 30
             wall-section-steps  30
             {thumb-single-row-wall-section :wall-section 
              outer-key-gap-fn-coll :outer-key-gap-fn-coll 
              inner-key-gap-fn-coll :inner-key-gap-fn-coll} (thumb-wall-section-for-single-thumb-row-fn wall-cross-section-steps wall-section-steps)
             fractyl-back-wall-polyhedron (fractyl-back-wall wall-cross-section-steps wall-section-steps)
             front-wall-nurbs-polyhedron (front-wall-nurbs wall-cross-section-steps wall-section-steps)
             {left-section-data :left-section-data
              thumb-outer-points-fn :thumb-outer-points-fn
              thumb-inner-points-fn :thumb-inner-points-fn
              inner-index-to-index-connector-outer-curve-fn :inner-index-to-index-connector-outer-curve-fn
              inner-index-to-index-connector-inner-curve-fn :inner-index-to-index-connector-inner-curve-fn} (left-section-data steps)
             {left-section-vnf-array :vnf-array
              trackpad-to-main-body-data :trackpad-to-main-body-data
              thumb-bl-to-tl-outer-curve-fn :thumb-bl-to-tl-outer-curve-fn
              thumb-tl-to-tr-outer-curve-fn :thumb-tl-to-tr-outer-curve-fn
              thumb-bl-to-tl-inner-curve-fn :thumb-bl-to-tl-inner-curve-fn
              thumb-tl-to-tr-inner-curve-fn :thumb-tl-to-tr-inner-curve-fn} left-section-data
             {fractyl-right-wall-vnf :fractyl-right-wall-vnf
               key-gap-outer-curve-fn-coll :key-gap-outer-curve-fn-coll
              key-gap-inner-curve-fn-coll :key-gap-inner-curve-fn-coll} (fractyl-right-wall wall-cross-section-steps (/ wall-section-steps 2))
             {trackpad-to-main-body-vnf :trackpad-to-main-body-vnf
             left-side-key-gap-outer-curve-fn-coll :left-side-key-gap-outer-curve-fn-coll
             left-side-key-gap-inner-curve-fn-coll :left-side-key-gap-inner-curve-fn-coll} trackpad-to-main-body-data
             {thumb-bl-to-tl-vnf :thumb-bl-to-tl-vnf
              thumb-tl-to-tr-vnf :thumb-tl-to-tr-vnf} (thumb-connecter-1-row steps :thumb-bl-to-tl-P-u-zero-outer (vec (reverse (thumb-bl-to-tl-outer-curve-fn steps)))
                                                                             :thumb-tl-to-tr-P-u-zero-outer (vec (reverse (thumb-tl-to-tr-outer-curve-fn steps)))
                                                                             :thumb-bl-to-tl-P-u-one-inner (thumb-bl-to-tl-inner-curve-fn steps) 
                                                                             :thumb-tl-to-tr-P-u-one-inner (thumb-tl-to-tr-inner-curve-fn steps)
                                                                             :thumb-bl-to-tl-P-u-one-outer ((nth outer-key-gap-fn-coll 1) steps)
                                                                             :thumb-tl-to-tr-P-u-one-outer ((nth outer-key-gap-fn-coll 0) steps)
                                                                             ;:thumb-bl-to-tl-P-u-zero-inner ((nth inner-key-gap-fn-coll 1) steps)
                                                                             ;:thumb-tl-to-tr-P-u-zero-inner ((nth inner-key-gap-fn-coll 0) steps)
                                                                             )] 
         (union 
          (fractyl-col-to-col-connecter 0 0 steps :upper-horizontal-outer-curve-type :catmull :upper-horizontal-inner-curve-type :catmull
                                        :lower-outer-point-paramater-calculation-method :dynamic-centripetal)
          (fractyl-col-to-col-connecter 1 0 steps :upper-horizontal-outer-curve-type :catmull :upper-horizontal-inner-curve-type :catmull
                                        :lower-outer-point-paramater-calculation-method :dynamic-centripetal)
          (fractyl-col-to-col-connecter 2 0 steps :upper-horizontal-outer-curve-type :catmull :upper-horizontal-inner-curve-type :catmull
                                        :lower-horizontal-outer-curve-type :global-end
                                        :lower-horizontal-inner-curve-type :global-end)
          (fractyl-col-to-col-connecter 3 0 steps :upper-horizontal-outer-curve-type :catmull
                                        :upper-horizontal-inner-curve-type :catmull
                                        :lower-outer-point-paramater-calculation-method :dynamic-centripetal)
          (fractyl-crossroads-connecters 0 0 steps
                                         :upper-outer-point-paramater-calculation-method :dynamic-centripetal)
          (fractyl-crossroads-connecters 1 0 steps
                                         :row-above-inner-curve-type :catmull
                                         :row-below-outer-curve-type :catmull
                                         :row-below-inner-curve-type :catmull
                                         :upper-outer-point-paramater-calculation-method :dynamic-centripetal
                                         :upper-inner-point-paramater-calculation-method :farin-simple
                                         :upper-inner-catmull-rom-alpha :uniform
                                         :lower-inner-catmull-rom-alpha :chordal
                                         :lower-outer-catmull-rom-alpha :chordal)
          (fractyl-crossroads-connecters 2 0 steps
                                         :row-above-inner-curve-type :global-end
                                         :row-above-outer-curve-type :global-end
                                         ;:upper-outer-point-paramater-calculation-method :equal
                                         ;:upper-inner-point-paramater-calculation-method :centripetal
                                         :row-below-outer-curve-type :catmull
                                         :row-below-inner-curve-type :catmull
                                         :upper-inner-catmull-rom-alpha :chordal
                                         ;:lower-inner-catmull-rom-alpha :chordal
                                         :lower-outer-catmull-rom-alpha :chordal)
          (fractyl-crossroads-connecters 3 0 steps
                                         :upper-outer-point-paramater-calculation-method :dynamic-centripetal
                                         :lower-outer-point-paramater-calculation-method :dynamic-centripetal
                                         :lower-inner-point-paramater-calculation-method :dynamic-centripetal
                                         ;:row-below-inner-curve-type :global-end
                                         ;:lower-inner-catmull-rom-alpha :uniform
                                         )

          (fractyl-col-to-col-connecter 0 1 steps)
          (fractyl-col-to-col-connecter 1 1 steps :upper-horizontal-outer-curve-type :catmull
                                        :upper-horizontal-inner-curve-type :catmull
                                        :upper-outer-catmull-rom-alpha :chordal
                                        :upper-inner-catmull-rom-alpha :chordal
                                        :lower-horizontal-outer-curve-type :global-end
                                        :lower-horizontal-inner-curve-type :global-end)

          (fractyl-col-to-col-connecter 2 1 steps
                                        :upper-horizontal-outer-curve-type :catmull :upper-horizontal-inner-curve-type :catmull
                                        :lower-horizontal-outer-curve-type :catmull :lower-horizontal-inner-curve-type :catmull
                                        :upper-outer-catmull-rom-alpha :chordal
                                        :upper-inner-catmull-rom-alpha :chordal
                                        :lower-inner-catmull-rom-alpha :chordal
                                        :lower-outer-catmull-rom-alpha :chordal)
          (fractyl-col-to-col-connecter 3 1 steps
                                        :upper-outer-point-paramater-calculation-method :dynamic-centripetal
                                        :upper-inner-point-paramater-calculation-method :dynamic-centripetal
                                        :lower-horizontal-outer-curve-type :catmull
                                        :lower-horizontal-inner-curve-type :catmull
                                        ;:lower-outer-point-paramater-calculation-method :dynamic-centripetal
                                        ;:lower-inner-point-paramater-calculation-method :dynamic-centripetal

                                        ;:lower-inner-catmull-rom-alpha :chordal 
                                        ;:upper-inner-catmull-rom-alpha :chordal
                                        ;:lower-outer-catmull-rom-alpha :chordal
                                        )
          (fractyl-crossroads-connecters 0 1 steps)
          (fractyl-crossroads-connecters 1 1 steps :row-above-outer-curve-type :global-end
                                         :row-above-inner-curve-type :global-end
                                         :row-below-outer-curve-type :global-end
                                         :row-below-inner-curve-type :global-end
                                         )
          (fractyl-crossroads-connecters 2 1 steps
                                         :row-above-outer-curve-type :catmull
                                         :row-above-inner-curve-type :catmull
                                         :upper-outer-catmull-rom-alpha :chordal
                                         :upper-inner-catmull-rom-alpha :chordal
                                         :row-below-outer-curve-type :global-end
                                         :row-below-inner-curve-type :global-end)
          (fractyl-crossroads-connecters 3 1 steps :row-above-outer-curve-type :catmull
                                         :row-above-inner-curve-type :catmull
                                         :row-below-outer-curve-type :global-end
                                         :row-below-inner-curve-type :global-end)
          (fractyl-col-to-col-connecter 0 2 steps
                                        :lower-horizontal-outer-curve-type inner-index-to-index-connector-outer-curve-fn
                                        :lower-horizontal-inner-curve-type inner-index-to-index-connector-inner-curve-fn
                                        ;:row-above-inner-curve-type :global-end
                                        )
          (fractyl-col-to-col-connecter 1 2 steps :upper-horizontal-outer-curve-type :global-end
                                        :upper-horizontal-inner-curve-type :global-end
                                        :lower-horizontal-outer-curve-type :global-end
                                        :lower-horizontal-inner-curve-type :global-end
                                        :render-method :chained-hull)
          (fractyl-col-to-col-connecter 2 2 steps :upper-horizontal-outer-curve-type :global-end
                                        :upper-horizontal-inner-curve-type :global-end
                                        :lower-horizontal-outer-curve-type :global-end
                                        :lower-horizontal-inner-curve-type :global-end)
          (fractyl-col-to-col-connecter 3 2 steps :upper-horizontal-outer-curve-type :global-end
                                        :upper-horizontal-inner-curve-type :global-end
                                        :lower-horizontal-outer-curve-type :global-end
                                        :lower-horizontal-inner-curve-type :global-end
                                        :render-method :chained-hull)
          

          (fractyl-column-row-connecters steps)
          (fractyl-column-row-connecters-for-inner-index-column
          left-side-key-gap-outer-curve-fn-coll left-side-key-gap-inner-curve-fn-coll steps)
          (fractyl-column-row-connecters-for-pinky-column key-gap-outer-curve-fn-coll key-gap-inner-curve-fn-coll steps)
          thumb-type
          key-holes
          (vnf-polyhedron trackpad-to-main-body-vnf)
          (vnf-polyhedron fractyl-right-wall-vnf)
          (vnf-polyhedron (wall-vnf thumb-single-row-wall-section {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse true :style :default}))
          (vnf-polyhedron  (thumb-to-left-section steps thumb-outer-points-fn thumb-inner-points-fn))
          (vnf-polyhedron left-section-vnf-array)
          (vnf-polyhedron thumb-bl-to-tl-vnf)
          (vnf-polyhedron thumb-tl-to-tr-vnf)
          (vnf-polyhedron (wall-vnf (thumb-tr-br-to-middle-lm-local-cubic-curve-interpolation-wall-section-fn wall-cross-section-steps wall-section-steps)
                                    {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
          fractyl-back-wall-polyhedron
          front-wall-nurbs-polyhedron  
          ))
       )))

(comment (spit "things-low/fractyl-connecter-test.scad"
      (write-scad
       (include include-bosl2)
       (let [wall-cross-section-steps 5
             wall-section-steps 5
             degree 2
             n 5
             segments (+ (- (* 2 n) degree) 2)
             segments-steps (* wall-section-steps segments)
             bottom-point-fn (fn [side] (case side
                                          :right :br
                                          :left :bl))
             top-position-fn (fn [side] (case side
                                          :right :tr
                                          :left :tl))
             steps (* wall-cross-section-steps 2)
             get-web-post-position-fn (fn [top-or-bottom] (case
                                                           top-or-bottom
                                                            :top main-body-web-post-point-top
                                                            :bottom main-body-web-post-point-bottom))
             curve-fn  (fn [side top-or-bottom] (vec (for [col (range 1 lastcol)
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
                                                                                                              (* wall-cross-section-steps 2))
                                                        (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U params) (:P params) 6 7
                                                                                                              (* wall-cross-section-steps 2))])))

             right-outer-curves (curve-fn :right :top)
             left-outer-curves (curve-fn :left :top)
             right-inner-curves (curve-fn :right :bottom)
             left-inner-curves (curve-fn :left :bottom)
             ruled-surfaces (apply concat (vec (for [col (range 0 (dec lastcol))
                                                     :let [left (nth left-outer-curves col)
                                                           right (nth right-outer-curves col)
                                                           left-inner (nth left-inner-curves col)
                                                           right-inner (nth right-inner-curves col)]]
                                                 (vec (for [row (range cornerrow)]
                                                        (vnf-polyhedron (wall-vnf-array (lofted-surface (nth left row) (nth right row) steps steps) 
                                                                                        (lofted-surface (nth right-inner row) (nth left-inner row)  steps steps)
                                                                                        default-vnf-vertex-array-args)))))))]
         
         (union
         ;(mapv #(mapv (fn [curve] (plot-bezier-points curve (sphere 1))) %) (curve-fn :left))
          key-holes
          ruled-surfaces
          ;(fractyl-back-wall wall-cross-section-steps wall-section-steps)
          (fractyl-right-wall wall-cross-section-steps wall-section-steps)
          )
         )
       )))