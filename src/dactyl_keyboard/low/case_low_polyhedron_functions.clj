(ns dactyl-keyboard.low.case-low-polyhedron-functions
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [magnitude]]
            [clojure.math :refer [ceil floor sqrt]]
            [clojure.string :refer [join]]
            [clojure.test :refer :all]
            [dactyl-keyboard.EVQWGD001 :refer :all]
            [dactyl-keyboard.lib.affine-transformations :refer [rotate-around-x-in-degrees
                                                                rotate-around-y-in-degrees rotate-around-z rotate-around-z-in-degrees]]
            [dactyl-keyboard.lib.algebra :refer [find-point-on-line-using-z]]
            [dactyl-keyboard.lib.curvesandsplines.beziers :refer [bezier-cubic
                                                                  bezier-cubic-through-control-points bezier-linear bezier-linear-spline bezier-quadratic
                                                                  bezier-quartic bezier-quintic n-degree-bezier-curve]]
            [dactyl-keyboard.lib.curvesandsplines.curve-fitting :refer [global-curve-interp global-curve-interp-with-calculated-first-derivatives-curve
                                                                        global-curve-interp-with-end-unit-derivatives-curve local-cubic-curve-interpolation
                                                                        local-cubic-curve-interpolation-with-calculated-tangents
                                                                        local-cubic-curve-interpolation-with-calculated-tangents-curve
                                                                        local-cubic-curve-interpolation-with-tangents-curve
                                                                        calculate-tangents-for-local-cubic-curve-interpolation-from-tangent
                                                                        local-cubic-curve-interpolation-with-tangents]]
            [dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline :refer [non-uniform-b-spline nurbs nurbs-with-calculated-knot-vector]]
            [dactyl-keyboard.lib.curvesandsplines.splines :refer [catmull-rom-spline-curve catmull-rom-spline-segment cubic-hermite-tension-spline-curve]]
            [dactyl-keyboard.lib.curvesandsplines.uniform-b-splines :refer [cubic-uniform-b-spline]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vnf :refer [default-vnf-vertex-array-args vnf-polyhedron vnf-vertex-array]]
            [dactyl-keyboard.lib.openscad.polyhedrons :refer [generate-bezier-along-bezier-polyhedron-all-sides generate-bezier-along-bezier-polyhedron-faces
                                                              generate-bezier-along-bezier-polyhedron-from-points-linear
                                                              generate-bezier-along-bezier-polyhedron-from-points-list-linear]]
            [dactyl-keyboard.lib.transformations :refer [rdx rdy rdz]]
            [dactyl-keyboard.low.case-low-functions :refer :all]
            [dactyl-keyboard.low.oled-low-placements :refer :all]
            [dactyl-keyboard.low.placement-functions-low :refer :all]
            [dactyl-keyboard.low.screen-holder-placement-points :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.low.thumbs-low :refer :all]
            [dactyl-keyboard.low.tps-65-placement-functions :refer [tps-65-place]]
            [dactyl-keyboard.low.tps-65-placement-points :refer :all]
            [dactyl-keyboard.low.web-connecters-low :refer :all]
            [dactyl-keyboard.oled :refer :all]
            [dactyl-keyboard.placement-functions :refer [rotate-around-x
                                                         rotate-around-y]]
            [dactyl-keyboard.sa-keycaps :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.tps-65 :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))


;; (defprotocol case-wall-protocol 
;;   ()
;;   )



(defn wall-locate1-circular ([dx dy] (wall-locate1-circular dx dy wall-xy-offset)) 
  ([dx dy xy ][(* dx wall-thickness  xy circle-bezier-approx-b) (* dy wall-thickness xy circle-bezier-approx-b) (* -1 circle-bezier-approx-c)]))

(defn wall-locate3-for-polyhedron-point-circular ([dx dy] (wall-locate3-for-polyhedron-point-circular dx dy wall-xy-offset))
  ([dx dy xy]
   [(* 0.98 (* dx circle-bezier-approx-c (+ xy wall-thickness (+ (/ oled-post-size 2)))))
    (* 0.98 (* dy circle-bezier-approx-c (+ xy wall-thickness (+ (/ oled-post-size 2)))))
     (* (+ 0.5 (- wall-z-offset (/ oled-holder-thickness  1))) circle-bezier-approx-b)]))

(defn wall-locate3-for-polyhedron-point-circular-2 ([dx dy] (wall-locate3-for-polyhedron-point-circular-2 dx dy wall-xy-offset))
  ([dx dy xy]
   [(* 0.98 (* dx circle-bezier-approx-a (+ xy wall-thickness (+ (/ oled-post-size 2)))))
    (* 0.98 (* dy circle-bezier-approx-a (+ xy wall-thickness (+ (/ oled-post-size 2)))))
    (* (+ 0.5 (- wall-z-offset (/ oled-holder-thickness  1))) circle-bezier-approx-b)]))

(defn wall-locate-1-to-3-curve-for-polyhedron-control-point ([dx dy] (wall-locate-1-to-3-curve-for-polyhedron-control-point dx dy [0.0 0.0 0.0]))
  ([dx dy orig-point] (mapv + orig-point [(* (double dx) (double wall-thickness)) (* (double dy) (double wall-thickness)) 0.0])))

(defn wall-locate2-for-polyhedron-point ([dx dy] (wall-locate2-for-polyhedron-point dx dy [0 0 0]))
  ([dx dy orig-point] (wall-locate2-for-polyhedron-point dx dy orig-point wall-xy-offset))
  ([dx dy orig-point xy] (mapv + orig-point [(* dx xy) (* dy xy) wall-z-offset])))

(defn wall-locate-1-to-3-curve-for-polyhedron-second-control-point
  ([dx dy] (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx dy [0 0 0]))
  ([dx dy orig-point] (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx dy orig-point wall-xy-offset))
  ([dx dy orig-point xy] (mapv + [(* dx (+ xy  wall-thickness))
                                  (* dy (+ xy  wall-thickness))
                                  (- wall-z-offset 1)] orig-point)))

(comment 
  (wall-locate-1-to-3-curve-for-polyhedron-second-control-point -1 0 [1 1 0]))


(comment 
  (wall-locate-1-to-3-curve-for-polyhedron-second-control-point 0 -1 (get-web-post-position-top (get-single-plate-corner-position-vector :bl))))

(comment 
  (mapv + (get-web-post-position-top (get-single-plate-corner-position-vector :bl)) (wall-locate-1-to-3-curve-for-polyhedron-second-control-point 0 -1)))
(defn wall-locate3-for-polyhedron-point ([dx dy] (wall-locate3-for-polyhedron-point dx dy wall-xy-offset))
  ([dx dy xy] (wall-locate3-for-polyhedron-point dx dy xy [0 0 0]))
   ([dx dy xy orig-point] 
    (mapv +
          [(* dx (+ xy wall-thickness (+ (/ oled-post-size 2))))
           (* dy (+ xy wall-thickness (+ (/ oled-post-size 2))))
           (+ 0.5 (- wall-z-offset (/ oled-holder-thickness  1)))]
          orig-point)))

(defn wall-locate3-xy-for-polyhedron-point [dx dy xy]
  [(* 0.98 (* dx (+ xy wall-thickness (+ (/ oled-post-size 2)))))
   (* 0.98 (* dy (+ xy wall-thickness (+ (/ oled-post-size 2)))))
   (+ 0.5 (- wall-z-offset (/ oled-holder-thickness  1)))])

(defn wall-brace-polyhedron-outer-floor-point ([place dx dy post-position rad-or-deg] (wall-brace-polyhedron-outer-floor-point place dx dy post-position rad-or-deg wall-xy-offset))
  ([place dx dy post-position rad-or-deg xy]
   (let [transform (get-transform-fn rad-or-deg place)
         curve-corner-translation-vector (get-curve-corner-translation-vector post-position)
         curve-post-position-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector) (get-curve-post-outer-x-and-y-vector dx dy))
         wall-locate3-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate3-for-polyhedron-point dx dy xy) curve-post-position-bottom (get-oled-post-outer-x-and-y-vector dx dy))))
         wall-locate3-point-floor (assoc (vec wall-locate3-point) 2 0)
         ]
     wall-locate3-point-floor
     ) 
   ))

(defmacro thumb-place-fn-from-keyword [keyword]
   `(symbol (join [(name ~keyword) "-place"]))
  )

(comment (macroexpand-1 '(thumb-place-fn-from-keyword :thumb-bl)))

(defn tstt [keyword]
  (name keyword)
  )

(comment (tstt :bl))

(comment (transform-position (thumb-place-fn-from-keyword :thumb-bl) [0 0 0]))

(comment (thumb-place-fn-from-keyword :thumb-bl))
(defrecord WallPosition [place ^Long dx ^Long dy post-position rad-or-deg xy offset slant position-type pre-rotation])

(defn wall-position [place dx dy post-position rad-or-deg position-type &{:keys [xy offset slant pre-rotation] 
                                                                          :or {xy wall-xy-offset offset [0 0 0] slant :parallel-by-d pre-rotation [0 0 0]}} ]
  (WallPosition. place dx dy post-position rad-or-deg xy offset slant position-type pre-rotation)
  )

(defn key-wall-position [column row dx dy post-position &{:keys [xy offset slant pre-rotation] 
                                                          :or {xy wall-xy-offset offset [0 0 0] slant :parallel-by-d pre-rotation [0 0 0]}}]
  (WallPosition. (partial key-place column row) dx dy post-position :radians xy offset slant :main-body-position pre-rotation)
  )
(defn tps-65-wall-position [corner cardinal & {:keys [place xy offset slant pre-rotation]
                                               :or {place (partial tps-65-place) xy wall-xy-offset offset [0 0 0] slant :parallel-by-d
                                                    pre-rotation [0 0 0]}}]
  (let [{dx :dx
         dy :dy} (get-tps-65-dx-dy cardinal)]
    (WallPosition. place dx dy corner :degrees xy offset slant :tps-65-position pre-rotation)) 
  )

(defn tps-65-to-screen-wall-position [corner cardinal 
                                      &{:keys [place xy offset slant pre-rotation] 
                                        :or {place (partial tps-65-place) xy wall-xy-offset offset [0 0 0] slant :parallel-by-d pre-rotation [0 0 0]}}]
(let [{dx :dx
       dy :dy} (get-tps-65-dx-dy cardinal)]
  (WallPosition. place dx dy corner :degrees xy offset slant :tps-65-to-screen-position pre-rotation))
  )

(defn screen-holder-wall-position [screen-side &{:keys [offset] :or {offset [0 0 0]}}]
  (WallPosition. nil nil nil screen-side :degrees nil offset nil :screen-holder-position nil)
  )


;; (defn thumb-place-fn-from-keyword [thumb-key-position-key-word]
;;   (case thumb-key-position-key-word
;;     :thumb-bl thumb-bl-place
;;     :thumb) 
;;   )

(defn thumb-wall-position [thumb-position-fn dx dy post-position &{:keys [xy offset slant pre-rotation]
                                                                   :or {xy wall-xy-offset offset [0 0 0] slant :parallel-by-d pre-rotation [0 0 0]}}]
  (WallPosition. thumb-position-fn dx dy post-position :degrees xy offset slant :main-body-position pre-rotation))
(comment (thumb-wall-position thumb-bl-place 1 1 :bl :offset [0 0 0]))


(deftype Floor-Points [outer-bottom-points inner-bottom-points])
(defrecord Case-Wall [case-positions ^Floor-Points floor-points horizontal-path-function vertical-path-functions])


(defprotocol horizontal-curve-data 
  
  )

(deftype wall-control-point [hor])
(defprotocol vertical-curve-data
  (control-points [this])
  (curve-points [this]) 
  )

;; (defprotocol nien
;;            vertical-curve-data
  
            
;;   )
(deftype nurbs-vertical-curve-data [control-points weights knot-vector]
  vertical-curve-data
  (control-points [_])
  (curve-points [_] )
  )

;; (deftype nurbs-with-generated-knot-vector-vertical-curve-data [control-points weights]
;;   vertical-curve-data
;;   (control-points [_])
;;   (curve-points [_]))

(deftype nurbs-with-generated-knot-vector-vertical-curve-data [degree weights])


(defmulti calculate-horizontal-curve 
  (fn [place dx dy post-position rad-or-deg steps & {:keys [outer-curve-type inner-curve-type]}]
    []))

;(deftype nurbs-vector )
 (defmulti calclate-inner-wall-horizontal-curve (fn [all-control-points curve-type curve-data] [curve-type]))

(defmethod calclate-inner-wall-horizontal-curve :curvetype [all-control-points curve-type curve-data]
  
  )





(defrecord Positions-On-Key [top-left top-right  bottom-left bottom-right 
                             bottom-middle right-middle left-middle top-middle])

(defrecord Upper-Positions-On-Key [top-left top-right  bottom-left bottom-right
                                   bottom-middle right-middle left-middle top-middle])

(defrecord Lower-Positions-On-Key [top-left top-right  bottom-left bottom-right 
                             bottom-middle right-middle left-middle top-middle])
(defrecord Key-Position [column row position ^Positions-On-Key positions-on-key
                         ^Upper-Positions-On-Key upper-positions-on-key
                         ^Lower-Positions-On-Key lower-positions-on-key
                         rad-or-deg] 
  )

;; (defprotocol ParametricCurve
;;   (point-on-curve [this u])
;;   (curve-points [this])
;;   (derivative [this])
;;   (tangent [this])
;;   (normal [this])
;;   (curvature [this])
;;   (center-of-curvature [this]))


;; (defn key-position-to-key-corner-data [^Key-Position key-position]
;;   (let [place-fn (case (:rad-or-deg key-position)
;;                    :radians (fn [column row] (partial key-place column row))
;;                    :degrees (fn [column row] (partial thumb-place-convex column row)))
;;         transform (get-transform-fn (:rad-or-deg key-position) (partial place-fn (:column key-position) (:row key-position)))])
;;   )

(defrecord KeyCornerData [place post-position rad-or-deg])
(defn main-body-key-corner [column row  post-position]
  (KeyCornerData. (partial key-place column row ) post-position :radians )
  )

(defn thumb-key-corner [thumb-place post-position]
  (KeyCornerData. thumb-place post-position :degrees)
  )
(defrecord VerticalCurveFromKeyPositionControlPointParameters [^KeyCornerData key-corner-data transform web-corner-translation-vector opposite-web-corner-translation-vector
                          oled-corner-translation-vector curve-corner-translation-vector])
(defrecord VerticalCurveFromKeyPosition [CurvePoints ^Key-Position key-position])

(defrecord CaseWall [outer-wall-surface-points inner-wall-surface-points wall-surface
                     inner-floor-points outer-floor-points
                     vertical-curve-from-key-positions])

(defrecord CaseWalls [CaseWalls])
(defrecord KeySwitchPlateData [column row position key-switch-plate])

(defrecord ColumnData [column-number switch-plates])

(defrecord SwitchPlate [columns])

(defrecord BottomPlate [points bottom-plate mounts screw-hole-positions])
(defrecord MainBody [^CaseWalls case-walls ^SwitchPlate switch-plate ])




(defprotocol WallParameterProtocol 
  (key-corner-data [this]) 
  (transform [this])
  (opposite-position [this])
  (web-corner-translation-vector [this])
  (opposite-web-corner-translation-vector [this])
  (oled-corner-translation-vector [this])
  (curve-corner-translation-vector [this])
  )



(deftype WallParameter [^KeyCornerData key-corner-data] 
  WallParameterProtocol
  (key-corner-data [_]
    (key-corner-data)              
    )
  (opposite-position [_] (get-opposite-position (:post-position key-corner-data) (:dx key-corner-data) (:dy key-corner-data)))
  (transform [_] (get-transform-fn (:rad-or-deg key-corner-data) (:place key-corner-data)))
  (web-corner-translation-vector [_] (get-single-plate-corner-position-vector (:post-position key-corner-data)))
  (opposite-web-corner-translation-vector [_] (get-single-plate-corner-position-vector (:opposite-position key-corner-data)) )
  (oled-corner-translation-vector [_] (get-oled-corner-translation-vector (:post-position key-corner-data)))
  (curve-corner-translation-vector [_] (get-curve-corner-translation-vector (:post-position key-corner-data))) 
  )






(defn t ([place dx dy post-position rad-or-deg] (t place dx dy post-position rad-or-deg wall-xy-offset))
  ([place dx dy post-position rad-or-deg xy]
   (let [transform (get-transform-fn rad-or-deg place)
opposite-position (get-opposite-position post-position dx dy)
web-corner-translation-vector (get-single-plate-corner-position-vector post-position)
opposite-web-corner-translation-vector (get-single-plate-corner-position-vector opposite-position)])
   ))


;;(defprotocol WallControlPointGenerator 
  ;;(calculate-opposite-web-post-position-top [this])
 ;;(calculate-point-on-tangent-from-plate [this])
;; (calculate-web-post-position-top [this])
;; (calculate-opposite-web-post-position-bottom [this])
;; (calculate-web-post-position-bottom [this])
;; (calculate-point-on-tangent-from-plate-bottom [this])
;; (calculate-oled-post-position-top [this])
;; (calculate-oled-post-position-bottom [this])
;; (calculate-wall-locate1-point [this])
;; (calculate-wall-locate-1-to-3-curve-for-polyhedron-control-point [this])
;; (calculate-wall-locate-1-to-3-curve-for-polyhedron-second-control-point [this])
;; (calculate-wall-locate3-point [this])
;; (calculate-wall-locate3-point-floor [this])
;; (calculate-wall-locate3-point-below-floor [this])
;; (calculate-wall-locate-2-top [this])
;; (calculate-wall-locate-2-bottom [this])
;; (calculate-wall-locate-2-bottom-floor [this])
;; (calculate-wall-locate-2-bottom-below-floor [this])
  ;;)

(defrecord DefaultWallControlPoints 
  [  opposite-web-post-position-top  
   vector-between-web-post-position-top-and-opposite 
   point-on-tangent-from-plate 
 web-post-position-bottom  
 opposite-web-post-position-bottom  
 vector-between-web-post-position-bottom-and-opposite
 point-on-tangent-from-plate-bottom 
 oled-corner-translation-vector  
 oled-post-position-bottom 
 curve-corner-translation-vector 
 curve-post-position-top 
 curve-post-position-middle 
 curve-post-position-bottom
 wall-locate1-point 
 wall-locate-1-to-3-curve-for-polyhedron-control-point
 wall-locate-1-to-3-curve-for-polyhedron-second-control-point
 wall-locate3-point 
 wall-locate3-point-floor  
 wall-locate3-point-below-floor 
 wall-locate-2-top 
 wall-locate-2-bottom 
 wall-locate-2-bottom-floor 
 wall-locate-2-bottom-below-floor]
 )
;; (deftype DefaultWallControlPointGenerator [^WallParameter wall-parameter]
;;   WallControlPointGenerator 
;;     (calculate-opposite-web-post-position-top [_] 
;;                                               ;(println "transform" (:transform key-corner-data))
;;                                               (vec ((.transform wall-parameter ) (get-web-post-position-top (.opposite-web-corner-translation-vector wall-parameter))))        
;;                                               )
;;  (calculate-point-on-tangent-from-plate 
;;   [_]
;;  ((.transform wall-parameter) (get-web-post-position-bottom (.web-corner-translation-vector wall-parameter))) )
;; (calculate-web-post-position-top [this] ((.transform wall-parameter )  (get-web-post-position-bottom web-corner-translation-vector)))
;; (calculate-opposite-web-post-position-bottom [this]  ((.transform wall-parameter )  (get-web-post-position-bottom opposite-web-corner-translation-vector)))
;; (calculate-web-post-position-bottom [this] ((.transform wall-parameter )  (get-web-post-position-bottom web-corner-translation-vector)))
;; (calculate-point-on-tangent-from-plate-bottom [this] (mapv + opposite-web-post-position-bottom (mapv (partial * 1.25) vector-between-web-post-position-bottom-and-opposite)))
;; (calculate-oled-post-position-top [this] (oled-post-position-top oled-corner-translation-vector))
;; (calculate-oled-post-position-bottom [this] (oled-post-position-bottom oled-corner-translation-vector))
;; (calculate-wall-locate1-point [this] ((.transform wall-parameter )  (mapv + (wall-locate1 dx dy) curve-post-position-top)))
;; (calculate-wall-locate-1-to-3-curve-for-polyhedron-control-point [this] 
;;   (make-point-z-value-not-below-zero ((.transform wall-parameter )  (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx dy) curve-post-position-middle (get-oled-post-outer-x-and-y-vector dx dy)))))
;; (calculate-wall-locate-1-to-3-curve-for-polyhedron-second-control-point [this] 
;;                                                                         (make-point-z-value-not-below-zero ((.transform wall-parameter )  (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx dy) curve-post-position-middle (get-oled-post-outer-x-and-y-vector dx dy))))
;;                                                                         )
;; (calculate-wall-locate3-point [this] (make-point-z-value-not-below-zero ((.transform wall-parameter )  (mapv + (wall-locate3-for-polyhedron-point dx dy xy) curve-post-position-bottom (get-oled-post-outer-x-and-y-vector dx dy)))))
;; (calculate-wall-locate3-point-floor [this] (assoc (vec wall-locate3-point) 2 0))
;; (calculate-wall-locate3-point-below-floor [this] (mapv + [0 0 (- plate-thickness)] wall-locate3-point-floor))
;; (calculate-wall-locate-2-top [this] (make-point-z-value-not-below-zero ((.transform wall-parameter )  (wall-locate2-for-polyhedron-point dx dy (mapv + oled-post-position-top (get-oled-post-inner-x-and-y-vector dx dy)) xy))))
;; (calculate-wall-locate-2-bottom [this] (make-point-z-value-not-below-zero ((.transform wall-parameter )  (wall-locate2-for-polyhedron-point dx dy (mapv + oled-post-position-bottom (get-oled-post-inner-x-and-y-vector dx dy)) xy))))
;; (calculate-wall-locate-2-bottom-floor [this] (assoc (vec wall-locate-2-bottom) 2 0))
;; (calculate-wall-locate-2-bottom-below-floor [this] (mapv + [0 0 (- plate-thickness)] wall-locate-2-bottom-floor))
;;   )


;; (def tt (DefaultWallControlPointGenerator. (WallParameter. 
;;                                             (KeyCornerData. (partial key-place 0 0) 1 1  :br :radians))))
;; (comment 
;;   ;(println "key-data" (get-key-corner-data (partial key-place 0 0) 1 1  :br :radians))
;;   (.calculate-opposite-web-post-position-top tt)
       
;;   )

(defn calculate-web-post-position-top [transform top-position]
   (vec (transform top-position)))

(defn  calculate-opposite-web-post-position-top [transform opposite-web-corner-translation-vector]  (transform (get-web-post-position-top opposite-web-corner-translation-vector)))

(comment (let [place (partial key-place lastcol cornerrow)
               post-position :bl 
               rad-or-deg :radians 
               transform (get-transform-fn rad-or-deg place) 
               offset [0.1 0.1 0.1]
               web-corner-translation-vector (mapv + (get-single-plate-corner-position-vector post-position))
               web-corner-translation-vector-offset (mapv + (get-single-plate-corner-position-vector post-position) offset) 
               top-position (get-web-post-position-top web-corner-translation-vector) 
               top-position-offset (get-web-post-position-top web-corner-translation-vector-offset)
               web-post-position-top (vec (transform top-position))
               web-post-position-top-offset (vec (transform top-position-offset))
               web-post-position-bottom (transform (get-web-post-position-bottom web-corner-translation-vector))
               web-post-position-bottom-offset (transform (get-web-post-position-bottom web-corner-translation-vector-offset)) 
               ]
           (println "web-post-position-top " web-post-position-top "web-post-position-top-offset " web-post-position-top-offset)
           (println "web-post-position-bottom " web-post-position-bottom "web-post-position-bottom-offset " web-post-position-bottom-offset) 
           ))
(comment (/ (sqrt 2) 2))

(comment (let [a [2 3 4] 
               b [2 -3 3]
               c (mapv - a b)
               d (mapv + a (mapv #(/ % (magnitude c)) c))]
           d))

(defn get-pre-rotation-fn [rad-or-deg]
  (let [rotation-fns (case rad-or-deg
                       :degrees [rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees]
                       :radians [rotate-around-x rotate-around-y rotate-around-z]) 
        ]
    (fn [rotation-vector position] 
      (->> position
           ((nth rotation-fns 0) (nth rotation-vector 0))
           ((nth rotation-fns 1) (nth rotation-vector 1))
           ((nth rotation-fns 2) (nth rotation-vector 2)))
      )
    )
  )

(keyword "parallel-by-d")
(keyword "parallel-by-d-opposite")
(keyword "parallel")
(keyword "no-slant")
(defn wall-brace-polyhedron-points [place dx dy post-position rad-or-deg 
                                    &{:keys [xy offset slant pre-rotation] 
                                      :or {xy wall-xy-offset offset [0 0 0] slant :parallel-by-d pre-rotation [0 0 0]}}] 
  (let [transform (get-transform-fn rad-or-deg place)
        pre-rotation-fn (get-pre-rotation-fn rad-or-deg)
         opposite-position (get-opposite-position post-position dx dy)
         web-corner-translation-vector  (mapv + (get-single-plate-corner-position-vector post-position) (pre-rotation-fn pre-rotation offset))
         opposite-web-corner-translation-vector (mapv + (get-single-plate-corner-position-vector opposite-position) (pre-rotation-fn pre-rotation offset))
         top-position (get-web-post-position-top web-corner-translation-vector)
         web-post-position-top (vec (transform top-position))
         
         ;point-on-tangent-from-plate (find-point-on-line-using-x)
         opposite-web-post-position-top (transform (get-web-post-position-top opposite-web-corner-translation-vector))
         vector-between-web-post-position-top-and-opposite (mapv - web-post-position-top opposite-web-post-position-top)
         point-on-tangent-from-plate (mapv + web-post-position-top (mapv #(/ % (magnitude vector-between-web-post-position-top-and-opposite)) vector-between-web-post-position-top-and-opposite))
         web-post-position-bottom (transform (get-web-post-position-bottom web-corner-translation-vector)) 
         opposite-web-post-position-bottom (transform (get-web-post-position-bottom opposite-web-corner-translation-vector))
         vector-between-web-post-position-bottom-and-opposite (mapv - web-post-position-bottom opposite-web-post-position-bottom)
         point-on-tangent-from-plate-bottom (mapv + web-post-position-bottom (mapv #(/ % (magnitude vector-between-web-post-position-bottom-and-opposite)) vector-between-web-post-position-bottom-and-opposite))
         oled-corner-translation-vector  (mapv + (get-oled-corner-translation-vector post-position) (pre-rotation-fn pre-rotation offset))
         oled-post-position-top (oled-post-position-top oled-corner-translation-vector)
         oled-post-position-bottom (oled-post-position-bottom oled-corner-translation-vector)
         curve-corner-translation-vector (mapv + (get-curve-corner-translation-vector post-position) offset)
         curve-post-top-position  (curve-post-position-top web-corner-translation-vector) 
         curve-post-position-middle (curve-post-position-middle web-corner-translation-vector)
         curve-post-position-bottom (mapv + (curve-post-position-bottom web-corner-translation-vector) )
         wall-locate1-point (transform (mapv + (wall-locate1 dx dy) curve-post-top-position))
         wall-locate-1-to-3-curve-for-polyhedron-control-point-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx dy top-position)  (get-curve-post-outer-x-and-y-vector dx dy))))
         wall-locate-1-to-3-curve-for-polyhedron-second-control-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx dy top-position xy) (get-curve-post-outer-x-and-y-vector dx dy)))) 
         wall-locate3-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate3-for-polyhedron-point dx dy xy top-position)  (get-curve-post-outer-x-and-y-vector dx dy))))
         point-at-floor-from-tangent-from-wall-locate-1-to-3-curve-for-polyhedron-second-control-point-to-wall-locate3-point (find-point-on-line-using-z wall-locate-1-to-3-curve-for-polyhedron-second-control-point
                                                                                                                                                         wall-locate3-point
                                                                                                                                                         0) 
         wall-locate3-point-floor (if (= slant :no-slant) (assoc wall-locate3-point 2 0.0) 
                                    (let [opposite-vector (mapv - point-at-floor-from-tangent-from-wall-locate-1-to-3-curve-for-polyhedron-second-control-point-to-wall-locate3-point wall-locate3-point)
                                          mag (magnitude (mapv - wall-locate3-point web-post-position-top))
                                          x-coord (cond (or (= slant :parallel) (and (= slant :parallel-by-d) (zero? dx)))
                                                        (nth point-at-floor-from-tangent-from-wall-locate-1-to-3-curve-for-polyhedron-second-control-point-to-wall-locate3-point 0)
                                                        (and (= slant  :parallel-by-d-opposite) (zero? dx)) (- (nth wall-locate3-point 0) (/ (nth opposite-vector 0) mag))
                                                        :else (nth wall-locate3-point 0))
                                          y-coord (cond (or (= slant :parallel) (and (= slant :parallel-by-d) (zero? dy))) (nth point-at-floor-from-tangent-from-wall-locate-1-to-3-curve-for-polyhedron-second-control-point-to-wall-locate3-point 1)
                                                      (and (= slant  :parallel-by-d-opposite) (zero? dy) )(- (nth wall-locate3-point 1) (/ (nth opposite-vector 1) mag))
                                                        :else (nth wall-locate3-point 1))]
                                    [x-coord y-coord 0.0])
                                      )
         wall-locate3-point-below-floor (mapv + [0 0 (- plate-thickness)] wall-locate3-point-floor)
         wall-locate-2-top (make-point-z-value-not-below-zero (transform (mapv + (wall-locate2-for-polyhedron-point dx dy (get-web-post-position-middle oled-corner-translation-vector) xy) (get-oled-post-inner-x-and-y-vector dx dy))) )
         wall-locate-2-bottom (make-point-z-value-not-below-zero (transform (mapv + (wall-locate2-for-polyhedron-point dx dy (get-web-post-position-bottom oled-corner-translation-vector) xy)  (get-oled-post-inner-x-and-y-vector dx dy))))
         point-at-floor-from-tangent-from-wall-locate-2-top-to-bottom (find-point-on-line-using-z wall-locate-2-bottom wall-locate-2-top 0.0)
         wall-locate-2-bottom-floor (if (= slant :no-slant) (assoc (vec wall-locate-2-bottom) 2 0.0)
                                      (let [opposite-vector (mapv - point-at-floor-from-tangent-from-wall-locate-2-top-to-bottom wall-locate-2-bottom)
                                            opposite-vector-magnitude (magnitude (mapv - wall-locate-2-bottom web-post-position-bottom)) 
                                            denom (/ opposite-vector-magnitude 1)
                                            x-coord (cond (or (= slant :parallel) (and (= slant :parallel-by-d) (zero? dx))) (nth point-at-floor-from-tangent-from-wall-locate-2-top-to-bottom 0) 
                                                          (and (= slant :parallel-by-d-opposite ) (zero? dx)) (- (nth wall-locate-2-bottom 0) (/ (nth opposite-vector 0) denom))
                                                        :else (+ (nth wall-locate-2-bottom 0)))
                                          y-coord (cond (or (= slant :parallel) (and (= slant :parallel-by-d) (zero? dy))) (nth point-at-floor-from-tangent-from-wall-locate-2-top-to-bottom 1) 
                                                      (and (= slant :parallel-by-d-opposite) (zero? dy))  (- (nth wall-locate-2-bottom 1) (/ (nth opposite-vector 1) denom))
                                                        :else (+ (nth wall-locate-2-bottom 1)))]
                                      [x-coord y-coord 0.0]))
         wall-locate-2-bottom-below-floor (mapv + [0 0 (- plate-thickness)] wall-locate-2-bottom-floor)
         ] 
     {:opposite-web-post-position-top opposite-web-post-position-top
      :point-on-tangent-from-plate point-on-tangent-from-plate
      :web-post-position-top web-post-position-top
      :opposite-web-post-position-bottom opposite-web-post-position-bottom
      :web-post-position-bottom web-post-position-bottom 
      :point-on-tangent-from-plate-bottom point-on-tangent-from-plate-bottom
      :oled-post-position-top oled-post-position-top
      :oled-post-position-bottom oled-post-position-bottom
      :wall-locate1-point wall-locate1-point
      :wall-locate-1-to-3-curve-for-polyhedron-control-point wall-locate-1-to-3-curve-for-polyhedron-control-point-point
      :wall-locate-1-to-3-curve-for-polyhedron-second-control-point wall-locate-1-to-3-curve-for-polyhedron-second-control-point
      :wall-locate3-point wall-locate3-point
      :wall-locate3-point-floor wall-locate3-point-floor
      :wall-locate3-point-below-floor wall-locate3-point-below-floor
      :wall-locate-2-top wall-locate-2-top
      :wall-locate-2-bottom wall-locate-2-bottom
      :wall-locate-2-bottom-floor wall-locate-2-bottom-floor
      :wall-locate-2-bottom-below-floor wall-locate-2-bottom-below-floor 
      }))

(def left-section-bl-corner-west 
  
    {:tps-65-corner-outer-opposite tps-65-bottom-left-outer 
     :tps-65-corner-outer tps-65-top-left-outer
     :screen-holder-top-outer screen-holder-top-right-outside-point
     :screen-holder-bottom-outer screen-holder-top-right-outside-point
     :screen-holder-floor-outer screen-holder-bottom-right-outside-floor-point
     ::tps-65-corner-inner-opposite tps-65-bottom-left-inner
     :tps-65-corner-inner tps-65-top-left-outer
     :screen-holder-top-inner screen-holder-top-right-inside-point
     :screen-holder-bottom-inner screen-holder-top-right-inside-point
     :screen-holder-floor-inner screen-holder-bottom-right-inside-floor-point} 
  )

(def left-section-bl-corner-south-west

  {:tps-65-corner-outer-opposite tps-65-bottom-right-outer
   :tps-65-corner-outer tps-65-top-left-outer
   :screen-holder-top-outer screen-holder-top-right-outside-point
   :screen-holder-bottom-outer screen-holder-top-right-outside-point
   :screen-holder-floor-outer screen-holder-bottom-right-outside-floor-point
   ::tps-65-corner-inner-opposite tps-65-bottom-left-inner
   :tps-65-corner-inner tps-65-bottom-right-outer
   :screen-holder-top-inner screen-holder-top-right-inside-point
   :screen-holder-bottom-inner screen-holder-top-right-inside-point
   :screen-holder-floor-inner screen-holder-bottom-right-inside-floor-point})

(def left-section-bl-corner-south

  {:tps-65-corner-outer-opposite tps-65-top-right-outer
   :tps-65-corner-outer tps-65-top-left-outer
   :screen-holder-top-outer screen-holder-top-right-outside-point
   :screen-holder-bottom-outer screen-holder-top-right-outside-point
   :screen-holder-floor-outer screen-holder-bottom-right-outside-floor-point
   ::tps-65-corner-inner-opposite tps-65-top-right-inner
   :tps-65-corner-inner tps-65-bottom-right-outer
   :screen-holder-top-inner screen-holder-top-right-inside-point
   :screen-holder-bottom-inner screen-holder-top-right-inside-point
   :screen-holder-floor-inner screen-holder-bottom-right-inside-floor-point})
 
(defn left-section-to-screen [place dx-orig dy-orig corner rad-or-deg
                              & {:keys [xy offset slant]
                                 :or {xy wall-xy-offset offset [0 0 0] slant :parallel-by-d}}]
  (assert (or (= corner :bl) (= corner :lm) (= corner :bl-lm)) "corner must be :bl, :lm or :bl-lm")
  (let [cardinal (dx-dy-to-cardinal dx-orig dy-orig)
        transform (get-transform-fn rad-or-deg place)
        position (get-position-tps-65 corner)
        tps-65-corner-outer (mapv + (:tps-65-corner-outer position) offset)
        tps-65-corner-inner (mapv + (:tps-65-corner-inner position) offset)
        {tps-65-corner-outer-opposite :tps-65-corner-outer-opposite
         tps-65-corner-inner-opposite :tps-65-corner-inner-opposite} (get-opposite-position-tps-65 corner cardinal)
        tps-65-corner-outer-to-opposite (mapv - tps-65-corner-outer tps-65-corner-outer-opposite)
        tps-65-corner-inner-to-opposite (mapv - tps-65-corner-inner tps-65-corner-inner-opposite)
        opposite-position-outer (mapv + tps-65-corner-outer (mapv #(/ % (/ (magnitude tps-65-corner-outer-to-opposite) 1))
                                                                  tps-65-corner-outer-to-opposite))
        opposite-position-inner (mapv + tps-65-corner-inner (mapv #(/ % (/ (magnitude tps-65-corner-inner-to-opposite) 1))
                                                                  tps-65-corner-inner-to-opposite))
        {dx :dx
         dy :dy} (dx-dy-to-tps-65-dx-dy dx-orig dy-orig)
        screen-holder-top-outside (case corner
                                    :lm screen-holder-top-left-outside-point
                                    :bl-lm screen-holder-top-mid-outside-point
                                    :bl (if (= cardinal :south) screen-holder-top-right-outside-point-alt screen-holder-top-right-outside-point))
        screen-holder-top-inside (case corner
                                   :lm screen-holder-top-left-inside-point
                                   :bl-lm screen-holder-top-mid-inside-point
                                   :bl screen-holder-top-right-inside-point)
        screen-holder-bottom-outside (case corner
                                       :lm screen-holder-bottom-left-outside-point
                                       :bl-lm screen-holder-bottom-mid-outside-point
                                       :bl (if (= cardinal :south) screen-holder-bottom-right-outside-point-alt screen-holder-bottom-right-outside-point))
        screen-holder-bottom-inside (case corner
                                      :lm screen-holder-bottom-left-inside-point
                                      :bl-lm screen-holder-bottom-mid-inside-point
                                      :bl screen-holder-bottom-right-inside-point)
        screen-holder-floor-outside (case corner
                                      :lm screen-holder-bottom-left-outside-floor-point
                                      :bl-lm screen-holder-bottom-mid-outside-floor-point
                                      :bl (if (= cardinal :south) screen-holder-bottom-right-outside-floor-point-alt screen-holder-bottom-right-outside-floor-point)
                                      )
        screen-holder-floor-inside (case corner
                                     :lm screen-holder-bottom-left-inside-floor-point
                                     :bl-lm screen-holder-bottom-mid-inside-floor-point
                                     :bl screen-holder-bottom-right-inside-floor-point)]
    {:opposite-web-post-position-top tps-65-corner-outer-opposite
     :point-on-tangent-from-plate opposite-position-outer
     :web-post-position-top tps-65-corner-outer
     :opposite-web-post-position-bottom tps-65-corner-inner-opposite
     :web-post-position-bottom (if (= corner :bl ) (mapv + screen-holder-top-inside offset [0 0 0.1]) tps-65-corner-inner)
     :point-on-tangent-from-plate-bottom opposite-position-inner 
     :wall-locate-1-to-3-curve-for-polyhedron-second-control-point (mapv + offset screen-holder-top-outside)
     :wall-locate3-point (mapv + offset screen-holder-bottom-outside)
     :wall-locate3-point-floor (mapv + offset screen-holder-floor-outside) 
     :wall-locate3-point-below-floor (mapv + screen-holder-floor-outside [0 0 (- plate-thickness) ])
     :wall-locate-2-top (mapv + offset screen-holder-top-inside)
     :wall-locate-2-bottom (mapv + offset screen-holder-bottom-inside) 
     :wall-locate-2-bottom-floor (mapv + offset screen-holder-floor-inside)
     :wall-locate-2-bottom-below-floor (mapv + screen-holder-floor-inside [0 0 (- plate-thickness)])}) 
  )

(defn left-section-corner [place dx-orig dy-orig corner rad-or-deg
                           & {:keys [xy offset slant]
                              :or {xy wall-xy-offset offset [0 0 0] slant :parallel-by-d}}] 
   (let [transform (get-transform-fn rad-or-deg place)
         position (get-position-tps-65 corner)
         cardinal (dx-dy-to-cardinal dx-orig dy-orig)
         tps-65-corner-outer (mapv + (:tps-65-corner-outer position) offset)
         tps-65-corner-inner (mapv + (:tps-65-corner-inner position) offset)
         {tps-65-corner-outer-opposite :tps-65-corner-outer-opposite
          tps-65-corner-inner-opposite :tps-65-corner-inner-opposite} (get-opposite-position-tps-65 corner cardinal)
         tps-65-corner-outer-to-opposite (mapv - tps-65-corner-outer tps-65-corner-outer-opposite)
         tps-65-corner-inner-to-opposite (mapv - tps-65-corner-inner tps-65-corner-inner-opposite)
         opposite-position-outer (mapv + tps-65-corner-outer (mapv #(/ % (magnitude tps-65-corner-outer-to-opposite))
                                                                   tps-65-corner-outer-to-opposite))
         opposite-position-inner (mapv + tps-65-corner-inner (mapv #(/ % (magnitude tps-65-corner-inner-to-opposite))
                                                                   tps-65-corner-inner-to-opposite))
         {dx :dx
          dy :dy} (dx-dy-to-tps-65-dx-dy dx-orig dy-orig)
         wall-locate1-point (transform (mapv + (wall-locate1 dx dy) tps-65-corner-outer))
         corner-translation-vector (get-tps-65-corner-translation-vector corner)
         corner-translation-vector-inner (mapv + corner-translation-vector [0 0 (- (/ web-thickness 2))])
         wall-locate-1-to-3-curve-for-polyhedron-control-point-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx dy corner-translation-vector)  (get-curve-post-outer-x-and-y-vector dx dy))))
         wall-locate-1-to-3-curve-for-polyhedron-second-control-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx dy corner-translation-vector xy) (get-curve-post-outer-x-and-y-vector dx dy))))
         wall-locate3-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate3-for-polyhedron-point dx dy xy corner-translation-vector)  (get-curve-post-outer-x-and-y-vector dx dy))))
         point-at-floor-from-tangent-from-wall-locate-1-to-3-curve-for-polyhedron-second-control-point-to-wall-locate3-point (find-point-on-line-using-z wall-locate-1-to-3-curve-for-polyhedron-second-control-point
                                                                                                                                                         wall-locate3-point
                                                                                                                                                         0)
         wall-locate3-point-floor (if (= slant :no-slant) (assoc wall-locate3-point 2 0.0)
                                      (let [x-coord (if (or (= slant :parallel) (and (= slant :parallel-by-d) (zero? dy)))
                                                      (nth point-at-floor-from-tangent-from-wall-locate-1-to-3-curve-for-polyhedron-second-control-point-to-wall-locate3-point 0)
                                                      (nth wall-locate3-point 0))
                                            y-coord (if (or (= slant :parallel) (and (= slant :parallel-by-d) (zero? dx))) (nth point-at-floor-from-tangent-from-wall-locate-1-to-3-curve-for-polyhedron-second-control-point-to-wall-locate3-point 1)
                                                        (nth wall-locate3-point 1))]
                                        [x-coord y-coord 0.0]))
         wall-locate3-point-below-floor (mapv + [0 0 (- plate-thickness)] wall-locate3-point-floor)
         oled-post-position-top (oled-post-position-top corner-translation-vector-inner)
         oled-post-position-bottom (oled-post-position-bottom corner-translation-vector-inner)
         wall-locate-2-top (make-point-z-value-not-below-zero (transform (mapv + (wall-locate2-for-polyhedron-point dx dy (get-web-post-position-middle corner-translation-vector-inner) xy) (get-oled-post-inner-x-and-y-vector dx dy))))
         wall-locate-2-bottom (make-point-z-value-not-below-zero (transform (mapv + (wall-locate2-for-polyhedron-point dx dy (get-web-post-position-bottom corner-translation-vector-inner) xy)  (get-oled-post-inner-x-and-y-vector dx dy))))
         point-at-floor-from-tangent-from-wall-locate-2-top-to-bottom (find-point-on-line-using-z wall-locate-2-bottom wall-locate-2-top 0.0)
         wall-locate-2-bottom-floor (if (= slant :no-slant) (assoc (vec wall-locate-2-bottom) 2 0.0)
                                        (let [x-coord (if (or (= slant :parallel) (and (= slant :parallel-by-d) (zero? dy))) (nth point-at-floor-from-tangent-from-wall-locate-2-top-to-bottom 0)
                                                          (+ (nth wall-locate-2-bottom 0)))
                                              y-coord (if (or (= slant :parallel) (and (= slant :parallel-by-d) (zero? dx))) (nth point-at-floor-from-tangent-from-wall-locate-2-top-to-bottom 1)
                                                          (+ (nth wall-locate-2-bottom 1)))]
                                          [x-coord y-coord 0.0]))
         wall-locate-2-bottom-below-floor (mapv + [0 0 (- plate-thickness)] wall-locate-2-bottom-floor)] 
     {:opposite-web-post-position-top tps-65-corner-outer-opposite
      :point-on-tangent-from-plate opposite-position-outer
      :web-post-position-top tps-65-corner-outer
      :opposite-web-post-position-bottom tps-65-corner-inner-opposite
      :web-post-position-bottom tps-65-corner-inner
      :point-on-tangent-from-plate-bottom opposite-position-inner
      :oled-post-position-top oled-post-position-top
      :oled-post-position-bottom oled-post-position-bottom
      :wall-locate1-point wall-locate1-point
      :wall-locate-1-to-3-curve-for-polyhedron-control-point wall-locate-1-to-3-curve-for-polyhedron-control-point-point
      :wall-locate-1-to-3-curve-for-polyhedron-second-control-point wall-locate-1-to-3-curve-for-polyhedron-second-control-point
      :wall-locate3-point wall-locate3-point
      :wall-locate3-point-floor wall-locate3-point-floor
      :wall-locate3-point-below-floor wall-locate3-point-below-floor
      :wall-locate-2-top wall-locate-2-top
      :wall-locate-2-bottom wall-locate-2-bottom
      :wall-locate-2-bottom-floor wall-locate-2-bottom-floor
      :wall-locate-2-bottom-below-floor wall-locate-2-bottom-below-floor})
  )


(defn tps-65-points [])


(comment (let [wall-position (WallPosition. (partial key-place 0 1) 1 1 :bl :radians 2 [0 0 0])
               ] 
   (apply wall-brace-polyhedron-points (conj (select-values wall-position [:place :dx :dy :post-position :rad-or-deg]) :xy (:xy-offset wall-position) :offset (:offset wall-position)))
      ) 
           )
         
(defn control-points-from-wall-position [wall-position control-points-fn]
  (let [{xy :xy
         offset :offset
         slant :slant
         pre-rotation :pre-rotation
         } wall-position]
    (apply control-points-fn (conj (select-values wall-position [:place :dx :dy :post-position :rad-or-deg])
                                   :xy xy :offset offset
                                   :slant slant
                                   :pre-rotation pre-rotation)))
  )

(defn wall-brace-polyhedron-points-from-wall-position [wall-position] 
        (control-points-from-wall-position wall-position (partial wall-brace-polyhedron-points)) 
  )

(defn left-section-corner-control-points-from-wall-position [wall-position]
        (control-points-from-wall-position wall-position (partial left-section-corner))
  )

(defn left-section-to-screen-control-points-from-wall-position [wall-position]
  (control-points-from-wall-position wall-position (partial left-section-to-screen)))

(defn screen-control-points-from-wall-position [wall-position]
  (let [{offset :offset
         post-position :post-position}  wall-position
        control-points (case post-position
                         :sr {:tps-65-point-outer (mapv + offset tps-65-top-left-outer)
                              :screen-holder-top-outer (mapv + offset screen-holder-top-right-outside-point)
                              :screen-holder-bottom-outer (mapv + offset screen-holder-bottom-right-outside-point)
                              :screen-holder-floor-outer (mapv + offset screen-holder-bottom-right-outside-floor-point)
                              :screen-holder-below-floor-outer (mapv + offset screen-holder-bottom-right-outside-floor-point [0 0 (- keyboard-z-offset)])
                              :tps-65-point-inner (mapv + offset tps-65-top-left-inner)
                              :screen-holder-top-inner (mapv + offset screen-holder-top-right-inside-point)
                              :screen-holder-bottom-inner (mapv + offset screen-holder-bottom-right-inside-point)
                              :screen-holder-floor-inner (mapv + offset screen-holder-bottom-right-inside-floor-point)
                              :screen-holder-below-floor-inner (mapv + offset screen-holder-bottom-right-inside-floor-point [0 0 (- keyboard-z-offset)])}
                         :sl {:tps-65-point-outer (mapv + offset tps-65-top-mid-outer)
                              :screen-holder-top-outer (mapv + offset screen-holder-top-left-outside-point)
                              :screen-holder-bottom-outer (mapv + offset screen-holder-bottom-left-outside-point)
                              :screen-holder-floor-outer (mapv + offset screen-holder-bottom-left-outside-floor-point)
                              :screen-holder-below-floor-outer (mapv + offset screen-holder-bottom-left-outside-floor-point [0 0 (- keyboard-z-offset)])
                              :tps-65-point-inner (mapv + offset tps-65-top-mid-inner)
                              :screen-holder-top-inner (mapv + offset screen-holder-top-left-inside-point)
                              :screen-holder-bottom-inner (mapv + offset screen-holder-bottom-left-inside-point)
                              :screen-holder-floor-inner (mapv + offset screen-holder-bottom-left-inside-floor-point)
                              :screen-holder-below-floor-inner (mapv + offset screen-holder-bottom-left-inside-floor-point [0 0 (- keyboard-z-offset)])})] 
    control-points
    )
  )

(comment (let [ex-map {:a 1 :b 2}]
           (update-vals ex-map #(+ 1 %))))
(comment (screen-control-points-from-wall-position (screen-holder-wall-position :sl)))
(comment (wall-brace-polyhedron-points-from-wall-position (thumb-wall-position thumb-bl-place 1 1 :bl)))
(defn get-curve-control-points-by-key-words [all-control-points control-points-keywords] 
  (select-values all-control-points control-points-keywords)
  )



(def nurbs-case-wall-vertical-control-points-outer [:web-post-position-top :point-on-tangent-from-plate :wall-locate-1-to-3-curve-for-polyhedron-second-control-point
                                                    :wall-locate3-point :wall-locate3-point-floor])
(def bezier-cubic-verrtical-control-points-outer [:web-post-position-bottom :wall-locate-2-top :wall-locate-2-bottom :wall-locate-2-bottom-floor])

(defmulti case-wall-vertical-outer-curve (fn [all-control-points ]) )
(def case-wall-vertical-outer-nurbs-curve)

;; (defmulti case-wall-vertical-control-points-outer  (fn [type] type))
;; (defmethod case-wall-vertical-control-points-outer [:nurbs] [place dx dy post-position rad-or-deg]
;;   (let [points (wall-brace-polyhedron-points place dx dy post-position rad-or-deg)
;;         nurbs-control-points-key-words [:web-post-position-top :point-on-tangent-from-plate :wall-locate-1-to-3-curve-for-polyhedron-second-control-point
;;                                         :wall-locate3-point :wall-locate3-point-floor]]
;;     (get-curve-control-points-by-key-words points nurbs-control-points-key-words)
;;     ) 
;;   )

;(defn get-curve-control-points-by-key-words)

(def default-weights-for-vertical-nurbs [1 0.9  0.6 0.75 1])
(def wall-vertical-outer-nurbs-control-points-keywords
  [:web-post-position-top :point-on-tangent-from-plate :wall-locate-1-to-3-curve-for-polyhedron-second-control-point
   :wall-locate3-point :wall-locate3-point-floor])

(def outer-wall-curve-bezier-quintic-nurbs-keywords [:web-post-position-top
                                                     :wall-locate1-point
                                                     :wall-locate-1-to-3-curve-for-polyhedron-control-point
                                                     :wall-locate-1-to-3-curve-for-polyhedron-second-control-point
                                                     :wall-locate3-point
                                                     :wall-locate3-point-floor])
(def outer-wall-curve-bezier-quartic-nurbs-keywords [:web-post-position-top 
                                                     :wall-locate-1-to-3-curve-for-polyhedron-control-point
                                                     :wall-locate-1-to-3-curve-for-polyhedron-second-control-point
                                                     :wall-locate3-point
                                                     :wall-locate3-point-floor])

(def inner-wall-curve-bezier-cubic-nurbs-keywords [:web-post-position-bottom
                                                   :wall-locate-2-top
                                                   :wall-locate-2-bottom
                                                   :wall-locate-2-bottom-floor])
(def inner-wall-curve-bezier-quadratic-keywords [:web-post-position-bottom
                                                   :wall-locate-2-top 
                                                   :wall-locate-2-bottom-floor])



(def outer-wall-catmull-rom-spline-parameters 
  [:opposite-web-post-position-top
   :web-post-position-top :wall-locate-1-to-3-curve-for-polyhedron-second-control-point
   :wall-locate3-point :wall-locate3-point-floor :wall-locate3-point-below-floor])
(def tps-65-to-screen-outer-wall-catmull-rom-spline-keywords
  [:opposite-web-post-position-top
   :web-post-position-top :wall-locate-1-to-3-curve-for-polyhedron-second-control-point 
   :wall-locate3-point-floor :wall-locate3-point-below-floor])

(def inner-wall-catmull-rom-spline-keywords
  [:opposite-web-post-position-bottom
   :web-post-position-bottom
   :wall-locate-2-top
   :wall-locate-2-bottom
   :wall-locate-2-bottom-floor
   :wall-locate-2-bottom-below-floor])
(def tps-65-to-screen-inner-wall-catmull-rom-spline-parameters
  [:opposite-web-post-position-bottom
   :web-post-position-bottom
   :wall-locate-2-top 
   :wall-locate-2-bottom-floor
   :wall-locate-2-bottom-below-floor])
(defrecord WallVerticalNurbsParameters [control-point-keywords degree weights point-paramater-calculation-method knot-vector-generation-method])
(defrecord WallVerticalNurbsWithKnotVectorParameters [control-point-keywords degree weights knot-vector])
(defrecord WallVerticalBezierParameters [control-point-keywords])

(defrecord WallVerticalLeftSectionToScreenParameters [control-point-keywords])

(defrecord WallVerticalCatmullRomParameters [control-point-keywords alpha])

(defrecord WallVerticalCatmullRomParametersForScreen [control-point-keywords alpha])

(defn outer-wall-screen-catmull-rom-parameters [&{:keys [alpha] :or {alpha :centripetal} }]
  (WallVerticalCatmullRomParametersForScreen. [:tps-65-point-outer
                                               :screen-holder-top-outer 
                                               :screen-holder-bottom-outer
                                               :screen-holder-floor-outer
                                               :screen-holder-below-floor-outer]
                                              alpha)
  )

(defn inner-wall-screen-catmull-rom-parameters [&{:keys [alpha] :or {alpha :centripetal}}]
  (WallVerticalCatmullRomParametersForScreen. [:tps-65-point-inner
                                               :screen-holder-top-inner 
                                               :screen-holder-bottom-inner
                                               :screen-holder-floor-inner 
                                               :screen-holder-below-floor-inner]
                                              alpha))

(defn outer-wall-vertical-nurbs-parameters [&{:keys [control-point-keywords degree weights point-paramater-calculation-method knot-vector-calculation-method]
                                                :or {control-point-keywords wall-vertical-outer-nurbs-control-points-keywords
                                                     degree 3 weights default-weights-for-vertical-nurbs point-paramater-calculation-method :chordal
                                                     knot-vector-calculation-method :average}}]
  (WallVerticalNurbsParameters. control-point-keywords degree weights point-paramater-calculation-method knot-vector-calculation-method)
  )
(defn outer-wall-vertical-nurbs-with-knot-vector-parameters [knot-vector & {:keys [control-point-keywords degree weights]
                                               :or {control-point-keywords wall-vertical-outer-nurbs-control-points-keywords
                                                    degree 3 weights default-weights-for-vertical-nurbs}}]
  (WallVerticalNurbsWithKnotVectorParameters. control-point-keywords degree weights knot-vector))

(defn outer-wall-vertical-bezier-parameters [&{:keys [control-point-keywords] :or {control-point-keywords outer-wall-curve-bezier-quintic-nurbs-keywords}}]
  (WallVerticalBezierParameters. control-point-keywords))

(defn inner-wall-vertical-bezier-parameters [& {:keys [control-point-keywords] :or {control-point-keywords inner-wall-curve-bezier-cubic-nurbs-keywords}}]
  (WallVerticalBezierParameters. control-point-keywords))

(defn inner-wall-vertical-catmull-rom-parameters [&{:keys [control-point-keywords alpha] 
                                                    :or {control-point-keywords inner-wall-catmull-rom-spline-keywords alpha :centripetal}}]
  (WallVerticalCatmullRomParameters. control-point-keywords alpha))

(def left-section-to-screen-vectical-curve-parameters
  (WallVerticalLeftSectionToScreenParameters. [:opposite-web-post-position-top :web-post-position-top :point-on-tangent-from-plate :wall-locate-1-to-3-curve-for-polyhedron-second-control-point
                                               :wall-locate3-point :wall-locate3-point-floor :wall-locate3-point-below-floor])
  )

(comment (class (outer-wall-vertical-bezier-parameters)))
(defmulti vertical-wall-points (fn [all-control-points vertical-wall-parameters steps] [(class all-control-points)  (class vertical-wall-parameters) (class steps)]))

(defmethod vertical-wall-points [clojure.lang.PersistentHashMap  WallVerticalNurbsParameters Long] 
  vertical-wall-nurbs-with-calculated-knot-vector
  [all-control-points vertical-wall-parameters steps]
  (nurbs-with-calculated-knot-vector (select-values all-control-points (:control-point-keywords vertical-wall-parameters)) 
                                     (:degree vertical-wall-parameters) (:weights vertical-wall-parameters) steps 
                                     :knot-vector-calculation-method (::knot-vector-calculation-method vertical-wall-parameters))
  )

(defmethod vertical-wall-points [clojure.lang.PersistentHashMap  WallVerticalNurbsWithKnotVectorParameters Long]
  vertical-wall-nurbs-with-calculated-knot-vector
  [all-control-points vertical-wall-parameters steps]
  (nurbs (select-values all-control-points (:control-point-keywords vertical-wall-parameters))
         (:degree vertical-wall-parameters)
         (:knot-vector vertical-wall-parameters) (:weights vertical-wall-parameters) steps))

(defmethod vertical-wall-points [clojure.lang.PersistentHashMap WallVerticalBezierParameters Long]
  vertical-wall-bezier 
  [all-control-points vertical-wall-parameters steps]
  (n-degree-bezier-curve (select-values all-control-points (:control-point-keywords vertical-wall-parameters)) steps)
  )

(defmethod vertical-wall-points [clojure.lang.PersistentHashMap WallVerticalCatmullRomParameters Long]
  vertical-wall-catmull-rom
  [all-control-points vertical-wall-parameters steps]
  (let [{control-point-keywords :control-point-keywords
         alpha :alpha} vertical-wall-parameters] 
    (catmull-rom-spline-curve (select-values all-control-points control-point-keywords) steps :alphaType alpha)))

(defmethod vertical-wall-points [clojure.lang.PersistentHashMap WallVerticalCatmullRomParametersForScreen Long]
  vertical-wall-catmull-rom
  [all-control-points vertical-wall-parameters steps]
  (let [{control-point-keywords :control-point-keywords
         alpha :alpha} vertical-wall-parameters
        control-points (select-values all-control-points control-point-keywords)
        [tps-65-point top-point bottom-point floor-point below-floor-point] control-points
        floor-to-bottom  (mapv - bottom-point floor-point)
        bottom-to-top (mapv - top-point bottom-point)
        floor-to-bottom-length (magnitude floor-to-bottom)
        bottom-to-top-length (magnitude bottom-to-top)
        total (+ floor-to-bottom-length bottom-to-top-length)
        steps-increment (/ steps total)
        bottom-to-top-steps (floor (* steps-increment bottom-to-top-length))
        floor-to-bottom-steps (ceil (* steps-increment floor-to-bottom-length))] 
    (vec (concat (drop-last (catmull-rom-spline-curve [tps-65-point top-point bottom-point floor-point] bottom-to-top-steps :alphaType alpha))
    (catmull-rom-spline-curve [ top-point bottom-point floor-point below-floor-point] floor-to-bottom-steps :alphaType alpha)))))

(defn screen-side-catmull [tps-65-point top-point bottom-point floor-point below-floor-point steps &{:keys [alphaType] :or {alphaType :centripetal}}]
  (let [floor-to-bottom  (mapv - bottom-point floor-point)
        bottom-to-top (mapv - top-point bottom-point)
        floor-to-bottom-length (magnitude floor-to-bottom)
        bottom-to-top-length (magnitude bottom-to-top)
        total (+ floor-to-bottom-length bottom-to-top-length)
        steps-increment (/ steps total)
        bottom-to-top-steps (floor (* steps-increment bottom-to-top-length))
        floor-to-bottom-steps (ceil (* steps-increment floor-to-bottom-length))]
    (vec (concat (drop-last (catmull-rom-spline-curve [tps-65-point top-point bottom-point floor-point] bottom-to-top-steps :alphaType alphaType))
                 (catmull-rom-spline-curve [top-point bottom-point floor-point below-floor-point] floor-to-bottom-steps :alphaType alphaType)))
    )
  )

(comment (range 10.0))
(defn tps-to-screen-side-catmull [tps-65-point-opposite tps-65-point top-point bottom-point floor-point below-floor-point steps & {:keys [alphaType] :or {alphaType :centripetal}}]
  (let [half-steps (/ steps 2)
        floor-to-bottom  (mapv - bottom-point floor-point)
        bottom-to-top (mapv - top-point bottom-point)
        top-to-tps (mapv - tps-65-point bottom-point)
        floor-to-bottom-length (magnitude floor-to-bottom)
        bottom-to-top-length (magnitude bottom-to-top)
        top-to-tps-65-length (magnitude top-to-tps)
        total (+ floor-to-bottom-length bottom-to-top-length)
        steps-increment (/ half-steps total)
        bottom-to-top-steps (floor (* steps-increment bottom-to-top-length))
        floor-to-bottom-steps (inc (floor (* steps-increment floor-to-bottom-length)))
        top-to-tps-steps half-steps
        top-to-tps-curve (drop-last (catmull-rom-spline-curve [tps-65-point-opposite tps-65-point top-point bottom-point] top-to-tps-steps :alphaType alphaType))
        bottom-to-top (drop-last (catmull-rom-spline-curve [tps-65-point top-point bottom-point floor-point] bottom-to-top-steps :alphaType alphaType))
        floor-to-bottom-curve (catmull-rom-spline-curve [top-point bottom-point floor-point below-floor-point] floor-to-bottom-steps :alphaType alphaType)
        curve (vec (concat top-to-tps-curve
                     bottom-to-top
                     floor-to-bottom-curve))] 
    curve))
(comment 
  (let  [steps 30
         [tps-65-point-opposite tps-65-point top-point bottom-point floor-point below-floor-point] [tps-65-mid-right-outer tps-65-mid-left-outer screen-holder-top-left-outside-point screen-holder-bottom-left-outside-point screen-holder-bottom-left-outside-floor-point]
        floor-to-bottom  (mapv - bottom-point floor-point)
bottom-to-top (mapv - top-point bottom-point)
top-to-tps (mapv - tps-65-point bottom-point)
floor-to-bottom-length (magnitude floor-to-bottom)
bottom-to-top-length (magnitude bottom-to-top)
top-to-tps-65-length (magnitude top-to-tps)
total (+ floor-to-bottom-length bottom-to-top-length top-to-tps-65-length)
steps-increment (/ steps total)
bottom-to-top-steps (floor (* steps-increment bottom-to-top-length))
floor-to-bottom-steps (ceil (* steps-increment floor-to-bottom-length))
top-to-tps-steps (floor (* steps-increment top-to-tps-65-length))]
    
    (println "bottom-to-top-steps " bottom-to-top-steps " floor-to-bottom-steps " floor-to-bottom-steps "top-to-tps-steps" top-to-tps-steps)
    (+ bottom-to-top-steps floor-to-bottom-steps top-to-tps-steps)
    ))

(defn tps-to-screen-side-global-with-first-derivatives-outer-curve [control-points steps & {:keys [degree point-paramater-calculation-method magnitude-estimation-method]
                                                                                       :or {degree 2 point-paramater-calculation-method :dynamic-centripetal magnitude-estimation-method :arc}}]
  (global-curve-interp-with-calculated-first-derivatives-curve
   [(:web-post-position-top control-points)
    (:wall-locate-1-to-3-curve-for-polyhedron-second-control-point control-points)
    (:wall-locate3-point control-points)
    (:wall-locate3-point-floor control-points)]

   [(mapv - (:web-post-position-top control-points) (:opposite-web-post-position-top control-points))
    (mapv - (:wall-locate3-point control-points) (:wall-locate-1-to-3-curve-for-polyhedron-second-control-point control-points))
    (mapv -  (:wall-locate3-point-floor control-points)  (:wall-locate3-point control-points))
    (mapv - (:wall-locate3-point-below-floor control-points) (:wall-locate3-point-floor control-points))]
   degree
   steps
   :magnitude-estimation-method magnitude-estimation-method
   :point-paramater-calculation-method point-paramater-calculation-method))

(defn tps-to-screen-side-global-with-first-derivatives-inner-curve [control-points steps & {:keys [degree point-paramater-calculation-method magnitude-estimation-method]
                                                                                            :or {degree 2 point-paramater-calculation-method :dynamic-centripetal magnitude-estimation-method :arc}}]
  (global-curve-interp-with-calculated-first-derivatives-curve
   [(:web-post-position-bottom control-points)
    (:wall-locate-2-top control-points)
    (:wall-locate-2-bottom control-points)
    (:wall-locate-2-bottom-floor control-points)]

   [(mapv - (:web-post-position-bottom control-points) (:opposite-web-post-position-bottom control-points))
    (mapv - (:wall-locate-2-bottom control-points) (:wall-locate-2-top control-points))
    (mapv -  (:wall-locate-2-bottom-floor control-points)  (:wall-locate-2-bottom control-points))
    (mapv - (:wall-locate-2-bottom-below-floor control-points) (:wall-locate-2-bottom-floor control-points))]
   degree
   steps
   :magnitude-estimation-method magnitude-estimation-method
   :point-paramater-calculation-method point-paramater-calculation-method))

(defn tps-to-screen-side-local-outer-curve [control-points steps & {:keys [degree point-paramater-calculation-method magnitude-estimation-method]
                                                                                            :or {degree 2 point-paramater-calculation-method :dynamic-centripetal magnitude-estimation-method :arc}}]
  (local-cubic-curve-interpolation-with-tangents-curve
   [(:web-post-position-top control-points)
    (:wall-locate-1-to-3-curve-for-polyhedron-second-control-point control-points)
    (:wall-locate3-point control-points)
    (:wall-locate3-point-floor control-points)]

   [(mapv - (:web-post-position-top control-points) (:opposite-web-post-position-top control-points))
    (mapv - (:wall-locate3-point control-points) (:wall-locate-1-to-3-curve-for-polyhedron-second-control-point control-points))
    (mapv -  (:wall-locate3-point-floor control-points)  (:wall-locate3-point control-points))
    (mapv - (:wall-locate3-point-below-floor control-points) (:wall-locate3-point-floor control-points))]
   
   steps
   ;:magnitude-estimation-method magnitude-estimation-method
   ;:point-paramater-calculation-method point-paramater-calculation-method
   ))
(defmethod vertical-wall-points [clojure.lang.PersistentHashMap WallVerticalLeftSectionToScreenParameters Long]
  vertical-wall-bezier
  [all-control-points vertical-wall-parameters steps]
  (let [top-curve-steps (floor (/ steps 2))
        bottom-curve-steps (/ steps 2)
        control-point-keywords (:control-point-keywords vertical-wall-parameters)
        top-curve-keywords  [:web-post-position-top :point-on-tangent-from-plate :wall-locate-1-to-3-curve-for-polyhedron-second-control-point]
        top-curve (nurbs (select-values all-control-points top-curve-keywords) 2
                         [0 0 0 1 1 1] [1 (/ (sqrt 2) 2) 1] top-curve-steps)
        bottom-curve-keywords [:point-on-tangent-from-plate :wall-locate-1-to-3-curve-for-polyhedron-second-control-point
:wall-locate3-point :wall-locate3-point-floor]
        bottom-curve-control-points (select-values all-control-points bottom-curve-keywords)
        bottom-curve (catmull-rom-spline-curve (conj bottom-curve-control-points (assoc (last bottom-curve-control-points) 2 (- plate-thickness))) bottom-curve-steps)]
    (catmull-rom-spline-curve (select-values all-control-points  control-point-keywords) steps)
    ))



(defmulti vertical-curve-from-horizontal-control-points (fn [control-points vertical-wall-parameter wall-cross-section-steps] [(class vertical-wall-parameter)]))

(defmethod vertical-curve-from-horizontal-control-points [WallVerticalNurbsParameters] [control-points vertical-wall-parameter wall-cross-section-steps]
  (let [degree (:degree vertical-wall-parameter)
        weights (:weights vertical-wall-parameter)
        knot-vector-calculation-method (:knot-vector-calculation-method vertical-wall-parameter)] 
      (nurbs-with-calculated-knot-vector control-points degree weights wall-cross-section-steps
                                          :knot-vector-calculation-method knot-vector-calculation-method)
      )
  )

(defmethod vertical-curve-from-horizontal-control-points [WallVerticalNurbsWithKnotVectorParameters] [control-points vertical-wall-parameter wall-cross-section-steps]
  (let [degree (:degree vertical-wall-parameter)
        weights (:weights vertical-wall-parameter)
        knot-vector (:knot-vector vertical-wall-parameter)]
    (nurbs control-points degree knot-vector weights wall-cross-section-steps
                                       )))

(defmethod vertical-curve-from-horizontal-control-points [WallVerticalLeftSectionToScreenParameters] [control-points vertical-wall-parameter wall-cross-section-steps]
  (let []
    (catmull-rom-spline-curve control-points wall-cross-section-steps)))

(defmethod vertical-curve-from-horizontal-control-points [WallVerticalBezierParameters] [control-points vertical-wall-parameter wall-cross-section-steps] 
           (n-degree-bezier-curve control-points wall-cross-section-steps))

(defmethod vertical-curve-from-horizontal-control-points [WallVerticalCatmullRomParametersForScreen] [control-points vertical-wall-parameter wall-cross-section-steps]
  (let [alpha (:alpha vertical-wall-parameter)
        [tps-65-point top-point bottom-point floor-point below-floor-point] control-points
        floor-to-bottom (mapv - bottom-point floor-point)
        bottom-to-top (mapv - top-point bottom-point)
        floor-to-bottom-length (magnitude floor-to-bottom)
        bottom-to-top-length (magnitude bottom-to-top)
        total (+ floor-to-bottom-length bottom-to-top-length)
        steps-increment (/ wall-cross-section-steps total)
        bottom-to-top-steps (floor (* steps-increment bottom-to-top-length))
        floor-to-bottom-steps (ceil (* steps-increment floor-to-bottom-length))]
    ;;  (vec (concat (drop-last (catmull-rom-spline-curve [tps-65-point top-point bottom-point floor-point] bottom-to-top-steps :alphaType alpha))
    ;;               (catmull-rom-spline-curve [top-point bottom-point floor-point below-floor-point] floor-to-bottom-steps :alphaType alpha)))
  (catmull-rom-spline-curve control-points wall-cross-section-steps :alphaType alpha)
    ))


(defn wall-from-horizontal-control-curves [control-curves vertical-wall-parameters wall-cross-section-steps wall-section-steps total-wall-section-steps] 
  (let [vertical-wall-parameters-divisor (/ total-wall-section-steps (count vertical-wall-parameters))]
    (vec (for [index (range (inc total-wall-section-steps))
             :let [control-points (mapv #(nth % index) control-curves)
                   vertical-wall-parameter-index (/ (if (= index total-wall-section-steps) (dec index)
                                                        index) vertical-wall-parameters-divisor)
                   vertical-wall-parameter (nth vertical-wall-parameters (floor vertical-wall-parameter-index)
                                                )
                   ]] 
           (vertical-curve-from-horizontal-control-points control-points vertical-wall-parameter wall-cross-section-steps)))))
(defrecord WallCurve [points])
(defrecord WallCrossSectionParameter [^WallPosition wall-position outer-wall-parameters inner-wall-parameters])

(defn wall-cross-section-parameter [wall-position &{:keys [outer-wall-parameters inner-wall-parameters] 
                                                     :or {outer-wall-parameters (outer-wall-vertical-nurbs-parameters) inner-wall-parameters (inner-wall-vertical-bezier-parameters)}}]
  (WallCrossSectionParameter. wall-position outer-wall-parameters inner-wall-parameters)
  )

(keyword "main-body-position")
(keyword "tps-65-position")
(keyword "tps-65-to-screen-position")
(keyword "screen-position")
(defrecord WallCrossSection [^WallPosition wall-position all-control-points outer-wall-curve inner-wall-curve cross-section-steps])

(defn wall-cross-section-constructor [^WallPosition wall-position all-control-points outer-wall-curve inner-wall-curve cross-section-steps]
  (WallCrossSection.  wall-position all-control-points outer-wall-curve inner-wall-curve cross-section-steps))
(defmulti calculate-control-points (fn [^WallPosition wall-position] [(:position-type wall-position)]))

(defmethod calculate-control-points [:main-body-position]
  [wall-position]
  (wall-brace-polyhedron-points-from-wall-position wall-position)
  )

(defmethod calculate-control-points [:tps-65-position]
  [wall-position]
  (left-section-corner-control-points-from-wall-position wall-position))

(defmethod calculate-control-points [:tps-65-to-screen-position]
  [wall-position]
  (left-section-to-screen-control-points-from-wall-position wall-position))

(defmethod calculate-control-points [:screen-holder-position]
  [wall-position]
  (screen-control-points-from-wall-position wall-position))

(defn wall-cross-section [wall-cross-section-parameters steps] 
  (let [wall-position (:wall-position wall-cross-section-parameters)
        all-control-points (calculate-control-points wall-position)
        outer-wall-curve (vertical-wall-points all-control-points (:outer-wall-parameters wall-cross-section-parameters) steps)
        inner-wall-curve (vertical-wall-points all-control-points (:inner-wall-parameters wall-cross-section-parameters) steps)
        ]
    (WallCrossSection. wall-position all-control-points (WallCurve. outer-wall-curve) (WallCurve. inner-wall-curve) steps)
    )
  )



(comment (wall-cross-section (WallCrossSectionParameter. (key-wall-position 1 1 1 1 :bl) (outer-wall-vertical-nurbs-parameters) (inner-wall-vertical-bezier-parameters)) 10))
(defrecord WallSectionParameter [wall-cross-section-parameters curve-parameters steps-distrubution calculation-order])

(keyword "per-segment")
(keyword "per-section")
(keyword "horizontal-first")
(keyword "vertical-first")

(defn wall-section-parameter [wall-cross-section-parameters curve-parameters 
                              &{:keys [steps-distrubution calculation-order] :or {steps-distrubution :per-segment calculation-order :horizontal-first}}]
  (WallSectionParameter. wall-cross-section-parameters curve-parameters steps-distrubution calculation-order))
(defrecord GlobalCurveInterpolationParameters [degree  point-paramater-calculation-method knot-vector-generation-method linear-outer-top linear-inner-top])

(defn global-curve-interpolation-parameters [degree &{:keys [point-paramater-calculation-method knot-vector-generation-method linear-outer-top linear-inner-top] 
                                                      :or {point-paramater-calculation-method :chordal knot-vector-generation-method :average
                                                           linear-outer-top false linear-inner-top false}}]
  (GlobalCurveInterpolationParameters. degree point-paramater-calculation-method knot-vector-generation-method linear-outer-top linear-inner-top)
  )

(defrecord GlobalCurveInterpolationWithEndDerivativesParameters [degree tangent-endpoint-zero-wall-cross-section-parameter
                                                                 tangent-endpoint-n-wall-cross-section-parameter
                                                                 point-paramater-calculation-method knot-vector-generation-method
                                                                 magnitude-estimation-method
                                                                 linear-outer-top linear-inner-top])

(defn global-curve-interpolation-with-end-derivatives-parameters [degree tangent-endpoint-zero-wall-cross-section-parameter
                                                                  tangent-endpoint-n-wall-cross-section-parameter 
                                                                  &{:keys [point-paramater-calculation-method knot-vector-generation-method
                                                                            magnitude-estimation-method linear-outer-top linear-inner-top]
                                                       :or {point-paramater-calculation-method :chordal knot-vector-generation-method :average
                                                            magnitude-estimation-method :arc
                                                            linear-outer-top false linear-inner-top false}}]
  (GlobalCurveInterpolationWithEndDerivativesParameters. degree tangent-endpoint-zero-wall-cross-section-parameter
                                                         tangent-endpoint-n-wall-cross-section-parameter point-paramater-calculation-method
                                                         knot-vector-generation-method magnitude-estimation-method linear-outer-top linear-inner-top)
  )

(defrecord WallCrossSectionTangentParameter [end-point-wall-cross-section-parameter start-point-wall-cross-section-parameter])
(defn wall-cross-section-tangent-parameter [end-point-wall-cross-section-parameter start-point-wall-cross-section-parameter] (WallCrossSectionTangentParameter. end-point-wall-cross-section-parameter start-point-wall-cross-section-parameter))

(defn wall-cross-section-tangent-vector [wall-cross-section-tangent-vector-parameter wall-cross-section-steps]
  (let [wall-cross-sections-tangent-parameters-start (:start-point-wall-cross-section-parameter wall-cross-section-tangent-vector-parameter) 
        wall-cross-sections-tangent-parameters-end (:end-point-wall-cross-section-parameter wall-cross-section-tangent-vector-parameter)
        wall-cross-section-tangent-vector-start (wall-cross-section wall-cross-sections-tangent-parameters-start wall-cross-section-steps)
        wall-cross-section-tangent-vector-end (wall-cross-section wall-cross-sections-tangent-parameters-end wall-cross-section-steps)
        wall-cross-section-tangent-vectors-outer (mapv #(mapv - %1 %2)  (:points (:outer-wall-curve wall-cross-section-tangent-vector-end)) (:points (:outer-wall-curve wall-cross-section-tangent-vector-start)))
        ;tangent-direciton reversed for inner points as will be computed in the reverse horizontal directin to the outer
        wall-cross-section-tangent-vectors-inner (mapv #(mapv - %1 %2)   (:points (:inner-wall-curve wall-cross-section-tangent-vector-start)) (:points (:inner-wall-curve wall-cross-section-tangent-vector-end)))] 
    {:wall-cross-section-tangent-vectors-outer wall-cross-section-tangent-vectors-outer :wall-cross-section-tangent-vectors-inner wall-cross-section-tangent-vectors-inner}))

(defn wall-cross-section-tangent-vectors-for-wall-section [wall-cross-section-tangent-parameters wall-cross-section-steps]
  (let [wall-cross-section-tangent-vectors (mapv #(wall-cross-section-tangent-vector %  wall-cross-section-steps) wall-cross-section-tangent-parameters)]
    wall-cross-section-tangent-vectors) 
  )

(defn wall-cross-section-tangent-web-post-top-and-bottom-tangent-vector [wall-cross-section-tangent-vector-parameter]
  (let [wall-cross-sections-tangent-parameters-start (:start-point-wall-cross-section-parameter wall-cross-section-tangent-vector-parameter)
        wall-cross-sections-tangent-parameters-end (:end-point-wall-cross-section-parameter wall-cross-section-tangent-vector-parameter)
        wall-cross-section-tangent-vector-start (calculate-control-points (:wall-position wall-cross-sections-tangent-parameters-start))
        wall-cross-section-tangent-vector-end (calculate-control-points (:wall-position wall-cross-sections-tangent-parameters-end))
        wall-cross-section-tangent-vectors-outer (mapv -   (:web-post-position-top wall-cross-section-tangent-vector-end) (:web-post-position-top wall-cross-section-tangent-vector-start))
        ;tangent-direciton reversed for inner points as will be computed in the reverse horizontal directin to the outer
        wall-cross-section-tangent-vectors-inner (mapv -  (:web-post-position-bottom wall-cross-section-tangent-vector-start)  (:web-post-position-bottom wall-cross-section-tangent-vector-end))]
    {:wall-cross-section-tangent-vectors-outer wall-cross-section-tangent-vectors-outer :wall-cross-section-tangent-vectors-inner wall-cross-section-tangent-vectors-inner}))

(defn wall-cross-section-tangent-web-post-top-and-bottom-vectors-for-wall-section [wall-cross-section-tangent-vector-parameters]
  (let [wall-cross-section-tangent-vectors (mapv #(wall-cross-section-tangent-web-post-top-and-bottom-tangent-vector %  ) wall-cross-section-tangent-vector-parameters)]
    wall-cross-section-tangent-vectors))

(defrecord GlobalCurveInterpolationWithCalculatedFirstDerivativesParameters [tangent-vectors-start-and-endpoint-cross-section-parameters degree point-paramater-calculation-method magnitude-estimation-method] )

(defn global-curve-interp-with-first-derivatives-parameters [tangent-vectors-start-and-endpoint-cross-section-parameters degree &{ :keys [point-paramater-calculation-method magnitude-estimation-method]
                                                                                      :or {point-paramater-calculation-method :chordal magnitude-estimation-method :arc}}]
(GlobalCurveInterpolationWithCalculatedFirstDerivativesParameters. tangent-vectors-start-and-endpoint-cross-section-parameters degree  point-paramater-calculation-method magnitude-estimation-method))
(defrecord LocalCubicCurveInterpolationWithCalculatedTangentParameters [corner-preservation key-corner-style linear-outer-top linear-inner-top start-segment end-segment])

(defn local-cubic-curve-interpolation-with-calculated-tangents-parameter [&{:keys [key-corner-style corner-preservation linear-outer-top linear-inner-top start-segment end-segment] 
                                                                            :or {key-corner-style :curved corner-preservation :smooth 
                                                                                 linear-outer-top false  linear-inner-top false
                                                                                 start-segment 0 end-segment nil}}]
  (LocalCubicCurveInterpolationWithCalculatedTangentParameters. corner-preservation key-corner-style linear-outer-top linear-inner-top start-segment end-segment))

(defrecord LocalCubicCurveInterpolationParameters [tangents tangents-scale linear-outer-top linear-inner-top])

(defn local-cubic-curve-interpolation-parameter [tangents &{:keys [tangents-scale linear-outer-top linear-inner-top]
                                            :or {tangents-scale (repeat (count tangents) [1.0 1.0 1.0])linear-outer-top false  linear-inner-top false}}]
  (LocalCubicCurveInterpolationParameters. tangents tangents-scale linear-outer-top linear-inner-top)
  )
(defrecord CatmullRomSplineParameters [alpha linear-outer-top linear-inner-top])
(defn catmull-rom-spline-parameters [&{:keys [alpha linear-outer-top linear-inner-top] 
                                       :or {alpha :centripetal linear-outer-top false linear-inner-top false }}]
  (CatmullRomSplineParameters. alpha linear-outer-top linear-inner-top)
  )

(defrecord NDegreeBezierCurveParamaters [linear-outer-top linear-inner-top])

(defn n-degree-bezier-curve-paramaters [& {:keys [ linear-outer-top linear-inner-top]
                                            :or { linear-outer-top false linear-inner-top false}}]
  (NDegreeBezierCurveParamaters. linear-outer-top linear-inner-top)
  )

(defrecord NurbsParameters [degree knot-vector weights linear-outer-top linear-inner-top])

(defn nurbs-parameters [degree weights &{:keys [knot-vector linear-outer-top linear-inner-top]
          :or {linear-outer-top false linear-inner-top false}}]
  (NurbsParameters. degree knot-vector weights linear-outer-top linear-inner-top)
  )

(defn ninety-degree-arc-nurbs-parameter 
  "to be used alongside three points that form a 90 degree angle to draw a circular arc using nurbs"
  [&{:keys [linear-outer-top linear-inner-top]
     :or {linear-outer-top false linear-inner-top false}}] 
  (nurbs-parameters 2 [1 (/ (sqrt 2) 2) 1] :knot-vector [0 0 0 1 1 1]  
                    :linear-outer-top linear-outer-top 
                    :linear-inner-top linear-inner-top)
  )

(defn one-eighty-degree-arc-nurbs-parameter 
  "to be used alongside five points that form a 180 degree angle to draw a circular arc using nurbs"
  [& {:keys [linear-outer-top linear-inner-top]
      :or {linear-outer-top false linear-inner-top false}}] 
  (nurbs-parameters 2 [1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1]
                    :knot-vector [0 0 0 1.5 1.5 3 3 3]
                    :linear-outer-top linear-outer-top
                    :linear-inner-top linear-inner-top))

(defrecord CubicHermiteSplineSegmentParameter [tension linear-outer-top linear-inner-top])

(defn cubic-hermite-spline-segment [&{:keys [tension linear-outer-top linear-inner-top]
                                           :or {tension 1 linear-outer-top false linear-inner-top false}}]
  (CubicHermiteSplineSegmentParameter. tension linear-outer-top linear-inner-top)
  )

(defprotocol WallSectionPolyhedronProtocol
  (wall-polyhedron [this])
  )

(defprotocol WallSectionVnfProtocol
  (wall-vnf [this args])
  (wall-vnf-extra-points [this steps reverse style]))


(defn horizontal-first-outer-and-inner-walls [wall-cross-section-parameters outer-wall-curves-fn inner-wall-curves-fn wall-cross-section-steps wall-section-steps total-wall-section-steps ]
  (let [outer-wall-parameters (reverse (mapv #(:outer-wall-parameters %) wall-cross-section-parameters))
        outer-curve-keywords  (mapv #(:control-point-keywords %) outer-wall-parameters)
        inner-wall-parameters (reverse (mapv #(:inner-wall-parameters %) wall-cross-section-parameters))
        inner-curve-keywords   (mapv #(reverse (:control-point-keywords %)) inner-wall-parameters)
        wall-positions (reverse (mapv #(:wall-position %) wall-cross-section-parameters)) 
        all-control-points-coll (mapv #(calculate-control-points %) wall-positions)
        outer-wall-horizontal-curves-control-points (vec (map-indexed (fn [index element]
                                                                   (get-curve-control-points-by-key-words element (nth outer-curve-keywords index))) all-control-points-coll))
        inner-wall-horizontal-curves-control-points (vec (map-indexed (fn [index element]
                                                                   (get-curve-control-points-by-key-words element (nth inner-curve-keywords index))) all-control-points-coll))
        outer-wall-horizontal-curves (outer-wall-curves-fn outer-wall-horizontal-curves-control-points)
        inner-wall-horizontal-curves (inner-wall-curves-fn inner-wall-horizontal-curves-control-points)
        outer-wall (wall-from-horizontal-control-curves outer-wall-horizontal-curves outer-wall-parameters  wall-cross-section-steps wall-section-steps total-wall-section-steps) 
        inner-wall (wall-from-horizontal-control-curves inner-wall-horizontal-curves inner-wall-parameters wall-cross-section-steps wall-section-steps total-wall-section-steps)]
    {:outer-wall outer-wall :inner-wall inner-wall :outer-floor-points (peek outer-wall) :inner-floor-points (peek inner-wall)}))

(defn horizontal-first-outer-and-inner-walls-for-global [wall-cross-section-parameters tangent-endpoint-zero-wall-cross-section-parameter tangent-endpoint-n-wall-cross-section-parameter
                                                         outer-wall-curves-fn inner-wall-curves-fn  
                                                         wall-cross-section-steps wall-section-steps total-wall-section-steps]
  (let [outer-wall-parameters (reverse (mapv #(:outer-wall-parameters %) wall-cross-section-parameters))
        outer-curve-keywords (mapv #(:control-point-keywords %) outer-wall-parameters)
        inner-wall-parameters (reverse (mapv #(:inner-wall-parameters %) wall-cross-section-parameters))
        inner-curve-keywords  (mapv #(reverse (:control-point-keywords %)) inner-wall-parameters)
        {tangent-zero-endpoint-outer-wall-parameter :outer-wall-parameters
         tangent-zero-endpoint-inner-wall-parameter :inner-wall-parameters} tangent-endpoint-zero-wall-cross-section-parameter
        tangent-zero-endpoint-outer-keywords (:control-point-keywords tangent-zero-endpoint-outer-wall-parameter)
        tangent-zero-endpoint-inner-keywords (:control-point-keywords tangent-zero-endpoint-inner-wall-parameter)
        {tangent-n-endpoint-outer-wall-parameter :outer-wall-parameters
         tangent-n-endpoint-inner-wall-parameter :inner-wall-parameters}  tangent-endpoint-n-wall-cross-section-parameter
        tangent-n-endpoint-outer-keywords (:control-point-keywords tangent-n-endpoint-outer-wall-parameter)
        tangent-n-endpoint-inner-keywords (:control-point-keywords tangent-n-endpoint-inner-wall-parameter)

        wall-positions (reverse (mapv #(:wall-position %) wall-cross-section-parameters)) 
        all-control-points-coll (mapv #(calculate-control-points %) wall-positions)
        tangent-zero-endpoint-control-points (calculate-control-points (:wall-position tangent-endpoint-zero-wall-cross-section-parameter))
        tangent-n-endpoint-control-points (calculate-control-points (:wall-position tangent-endpoint-n-wall-cross-section-parameter))
        outer-wall-horizontal-curves-control-points (map-indexed (fn [index element]
                                                                   (get-curve-control-points-by-key-words element (nth outer-curve-keywords index))) all-control-points-coll)
        inner-wall-horizontal-curves-control-points (map-indexed (fn [index element]
                                                                   (get-curve-control-points-by-key-words element (nth inner-curve-keywords index))) all-control-points-coll)
        tangent-zero-endpoint-outer-horizontal-curve-control-points (get-curve-control-points-by-key-words tangent-zero-endpoint-control-points tangent-zero-endpoint-outer-keywords)
        tangent-zero-endpoint-inner-horizontal-curve-control-points (get-curve-control-points-by-key-words tangent-zero-endpoint-control-points tangent-zero-endpoint-inner-keywords) 
        tangent-n-endpoint-outer-horizontal-curve-control-points (get-curve-control-points-by-key-words tangent-n-endpoint-control-points tangent-n-endpoint-outer-keywords)
        tangent-n-endpoint-inner-horizontal-curve-control-points (get-curve-control-points-by-key-words tangent-n-endpoint-control-points tangent-n-endpoint-inner-keywords)
        outer-wall-horizontal-curves (outer-wall-curves-fn outer-wall-horizontal-curves-control-points tangent-n-endpoint-outer-horizontal-curve-control-points tangent-zero-endpoint-outer-horizontal-curve-control-points)
        inner-wall-horizontal-curves (inner-wall-curves-fn inner-wall-horizontal-curves-control-points tangent-zero-endpoint-inner-horizontal-curve-control-points tangent-n-endpoint-inner-horizontal-curve-control-points)
        outer-wall (wall-from-horizontal-control-curves outer-wall-horizontal-curves (into [tangent-n-endpoint-outer-wall-parameter ] (conj outer-wall-parameters tangent-zero-endpoint-outer-wall-parameter))  wall-cross-section-steps wall-section-steps total-wall-section-steps)
        inner-wall (wall-from-horizontal-control-curves inner-wall-horizontal-curves (into [tangent-zero-endpoint-inner-wall-parameter] (conj inner-wall-parameters tangent-n-endpoint-inner-wall-parameter)) wall-cross-section-steps wall-section-steps total-wall-section-steps)]
    {:outer-wall outer-wall :inner-wall inner-wall :outer-floor-points (peek outer-wall) :inner-floor-points (peek inner-wall)}))

(defn wall-cross-section-tangent-vectors-for-horizontal-first-wall-section [wall-cross-section-tangent-vector-parameters ]
  (let [wall-cross-sections-tangent-parameters-start (mapv #(:start-point-wall-cross-section-parameter %) wall-cross-section-tangent-vector-parameters)
        wall-cross-sections-tangent-parameters-end (mapv #(:end-point-wall-cross-section-parameter %) wall-cross-section-tangent-vector-parameters)
        wall-cross-sections-tangent-parameters-start-wall-position (reverse (mapv #(:wall-position %) wall-cross-sections-tangent-parameters-start))
        wall-cross-sections-tangent-parameters-end-wall-position (reverse (mapv #(:wall-position %) wall-cross-sections-tangent-parameters-end))
        wall-cross-sections-tangent-parameters-start-control-points-all (mapv #(calculate-control-points %) wall-cross-sections-tangent-parameters-start-wall-position)
        wall-cross-sections-tangent-parameters-end-control-points-all (mapv #(calculate-control-points %) wall-cross-sections-tangent-parameters-end-wall-position)
        outer-wall-cross-sections-tangent-parameters-start (reverse (mapv #(:outer-wall-parameters %) wall-cross-sections-tangent-parameters-start))
        inner-wall-cross-sections-tangent-parameters-start (reverse (mapv #(:inner-wall-parameters %) wall-cross-sections-tangent-parameters-start))
        outer-wall-cross-sections-tangent-parameters-end (reverse (mapv #(:outer-wall-parameters %) wall-cross-sections-tangent-parameters-end))
        inner-wall-cross-sections-tangent-parameters-end (reverse (mapv #(:inner-wall-parameters %) wall-cross-sections-tangent-parameters-end))
        outer-wall-cross-sections-tangent-parameters-start-keywords (mapv #(:control-point-keywords %) outer-wall-cross-sections-tangent-parameters-start)
        inner-wall-cross-sections-tangent-parameters-start-keywords (mapv #(:control-point-keywords %) inner-wall-cross-sections-tangent-parameters-start)
        outer-wall-cross-sections-tangent-parameters-end-keywords (mapv #(:control-point-keywords %) outer-wall-cross-sections-tangent-parameters-end)
        inner-wall-cross-sections-tangent-parameters-end-keywords (mapv #(:control-point-keywords %) inner-wall-cross-sections-tangent-parameters-end)
        outer-wall-cross-sections-tangent-parameters-start-control-points (vec (map-indexed (fn [index element]
                                                                                        (get-curve-control-points-by-key-words element (nth outer-wall-cross-sections-tangent-parameters-start-keywords index))) wall-cross-sections-tangent-parameters-start-control-points-all))
        outer-wall-cross-sections-tangent-parameters-end-control-points (vec (map-indexed (fn [index element]
                                                                                        (get-curve-control-points-by-key-words element (nth outer-wall-cross-sections-tangent-parameters-end-keywords index))) wall-cross-sections-tangent-parameters-end-control-points-all))
        inner-wall-cross-sections-tangent-parameters-start-control-points (vec (map-indexed (fn [index element]
                                                                   (get-curve-control-points-by-key-words element (nth inner-wall-cross-sections-tangent-parameters-start-keywords index))) wall-cross-sections-tangent-parameters-start-control-points-all))
        inner-wall-cross-sections-tangent-parameters-end-control-points (vec (map-indexed (fn [index element]
                                                                   (get-curve-control-points-by-key-words element (nth inner-wall-cross-sections-tangent-parameters-end-keywords index))) wall-cross-sections-tangent-parameters-end-control-points-all))
        outer-wall-cross-section-tangent-vectors (for [i (range (count outer-wall-cross-sections-tangent-parameters-start-control-points))]
                                                   (for [j (range (count (peek outer-wall-cross-sections-tangent-parameters-start-control-points)))]
                                                     (mapv - (get-in outer-wall-cross-sections-tangent-parameters-start-control-points [i j]) (get-in outer-wall-cross-sections-tangent-parameters-end-control-points [i j]))
                                                     )
                                                   )
        inner-wall-cross-section-tangent-vectors (for [i (range (count outer-wall-cross-sections-tangent-parameters-start-control-points))]
                                                   (for [j (range (count (peek outer-wall-cross-sections-tangent-parameters-start-control-points)))]
                                                     (mapv -  (get-in inner-wall-cross-sections-tangent-parameters-start-control-points [i j]) (get-in inner-wall-cross-sections-tangent-parameters-end-control-points [i j]) ))) 
        ] 
        {:outer-wall-cross-section-tangent-vectors outer-wall-cross-section-tangent-vectors :inner-wall-cross-section-tangent-vectors (reverse inner-wall-cross-section-tangent-vectors)}
        )
  )


(defn horizontal-first-outer-and-inner-walls-with-tangent-vectors [wall-cross-section-parameters tangent-wall-cross-section-parameters outer-wall-curves-fn inner-wall-curves-fn wall-cross-section-steps wall-section-steps total-wall-section-steps]
  (let [outer-wall-parameters (reverse (mapv #(:outer-wall-parameters %) wall-cross-section-parameters))
outer-curve-keywords  (mapv #(:control-point-keywords %) outer-wall-parameters)
inner-wall-parameters (reverse (mapv #(:inner-wall-parameters %) wall-cross-section-parameters))
inner-curve-keywords   (mapv #(reverse (:control-point-keywords %)) inner-wall-parameters)
wall-positions (reverse (mapv #(:wall-position %) wall-cross-section-parameters))
all-control-points-coll (mapv #(calculate-control-points %) wall-positions)
outer-wall-horizontal-curves-control-points (map-indexed (fn [index element]
                                                           (get-curve-control-points-by-key-words element (nth outer-curve-keywords index))) all-control-points-coll)
inner-wall-horizontal-curves-control-points (map-indexed (fn [index element]
                                                           (get-curve-control-points-by-key-words element (nth inner-curve-keywords index))) all-control-points-coll)
      {outer-wall-cross-section-tangent-vectors :outer-wall-cross-section-tangent-vectors
       inner-wall-cross-section-tangent-vectors :inner-wall-cross-section-tangent-vectors} (wall-cross-section-tangent-vectors-for-horizontal-first-wall-section tangent-wall-cross-section-parameters)
      outer-wall-horizontal-curves (outer-wall-curves-fn outer-wall-horizontal-curves-control-points outer-wall-cross-section-tangent-vectors)
      inner-wall-horizontal-curves (inner-wall-curves-fn inner-wall-horizontal-curves-control-points inner-wall-cross-section-tangent-vectors)
      outer-wall (wall-from-horizontal-control-curves outer-wall-horizontal-curves outer-wall-parameters wall-cross-section-steps wall-section-steps total-wall-section-steps)
inner-wall (wall-from-horizontal-control-curves inner-wall-horizontal-curves inner-wall-parameters wall-cross-section-steps wall-section-steps total-wall-section-steps)]
  {:outer-wall outer-wall :inner-wall inner-wall :outer-floor-points (peek outer-wall) :inner-floor-points (peek inner-wall)})
  )
(defn horizontal-first-outer-and-inner-walls-with-tangents [wall-cross-section-parameters tangent-wall-cross-section-parameters outer-wall-curves-fn inner-wall-curves-fn wall-cross-section-steps wall-section-steps total-wall-section-steps]
  (let [outer-wall-parameters (mapv #(:outer-wall-parameters %) wall-cross-section-parameters)
        outer-tangent-parameters (mapv #(:outer-wall-parameters %) tangent-wall-cross-section-parameters) 
        outer-curve-keywords (mapv #(:control-point-keywords %) outer-wall-parameters)
        outer-tangent-keywords (mapv #(:control-point-keywords %) outer-tangent-parameters)
        inner-wall-parameters (mapv #(:inner-wall-parameters %) wall-cross-section-parameters)
        inner-curve-keywords  (mapv #(reverse (:control-point-keywords %)) inner-wall-parameters)
        wall-positions (reverse (mapv #(:wall-position %) wall-cross-section-parameters))
        tangent-wall-positions (reverse (mapv (fn [tangent-end-positions] (mapv #(:wall-position %) tangent-end-positions)) tangent-wall-cross-section-parameters))
        all-control-points-coll (mapv #(wall-brace-polyhedron-points-from-wall-position %) wall-positions)
        outer-wall-horizontal-curves-control-points (map-indexed (fn [index element]
                                                                   (get-curve-control-points-by-key-words element (nth outer-curve-keywords index))) all-control-points-coll)
        inner-wall-horizontal-curves-control-points (map-indexed (fn [index element]
                                                                   (get-curve-control-points-by-key-words element (nth inner-curve-keywords index))) all-control-points-coll)
        outer-wall-horizontal-curves (outer-wall-curves-fn outer-wall-horizontal-curves-control-points)
        inner-wall-horizontal-curves (inner-wall-curves-fn inner-wall-horizontal-curves-control-points)
        outer-wall (wall-from-horizontal-control-curves outer-wall-horizontal-curves (nth outer-wall-parameters 0)  wall-cross-section-steps wall-section-steps total-wall-section-steps)
        inner-wall (wall-from-horizontal-control-curves inner-wall-horizontal-curves (nth inner-wall-parameters 0) wall-cross-section-steps wall-section-steps total-wall-section-steps)]
    {:outer-wall outer-wall :inner-wall inner-wall}))

(defn wall-vnf-array [outer-wall inner-wall &{:keys [args] :or {args default-vnf-vertex-array-args}}]
  (let [points-array (mapv #(vec (apply concat %))
                           (partition 2 (interleave
                                         outer-wall inner-wall)))]
    (vnf-vertex-array points-array args))
  )

(defn walls-to-vnf [wall-point-matrices & {:keys [args] :or {args default-vnf-vertex-array-args}}]
  (let [matrices-count (count wall-point-matrices)
        points-array (mapv #(vec (apply concat %))
                           (partition matrices-count (apply interleave
                                         wall-point-matrices)))]
    (vnf-vertex-array points-array args)
    ))


(defn generate-array-for-vnf [outer-wall inner-wall]
  (let [points-array (mapv #(vec (apply concat %))
                           (partition 2 (interleave
                                         outer-wall inner-wall)))]
    points-array))
(defrecord WallSection [wall-cross-sections outer-wall inner-wall combined-wall outer-floor-points  inner-floor-points]
  WallSectionVnfProtocol
  (wall-vnf [_ args]
            (let [points-array (mapv #(vec (apply concat %))  
                                     (partition 2 (interleave 
                      outer-wall inner-wall)) )]
             (vnf-vertex-array points-array args))) 
  (wall-vnf-extra-points [_ steps reverse style]
                         (let [outer-to-inner-points (mapv (fn [outer inner] (bezier-linear (last outer) (first inner) steps)) outer-wall inner-wall)
                               inner-to-outer-points (mapv (fn [outer inner] (bezier-linear (last inner) (first outer) steps)) outer-wall inner-wall) 
                               points-array (for [index (range (count outer-wall))]
                                              (vec (concat (nth outer-wall index) (nth outer-to-inner-points index) 
                                                           (nth inner-wall index) (nth inner-to-outer-points index)))
                                              )
                               ]
                           (vnf-vertex-array points-array :caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse reverse :style style) 
                           )
  )
  )

(defn wall-section-constructor [wall-cross-sections outer-wall inner-wall combined-wall outer-floor-points  inner-floor-points]
  (WallSection. wall-cross-sections outer-wall inner-wall combined-wall outer-floor-points  inner-floor-points))



(defmulti create-wall-section (fn [wall-section-parameter wall-cross-section-parameters curve-parameters
                                   steps-distrubution  wall-cross-section-steps wall-section-steps] 
                           [(class wall-section-parameter)(class (:curve-parameters wall-section-parameter))]))

(defn wall-section [wall-section-parameter wall-cross-section-steps wall-section-steps]
  (let [wall-cross-section-parameters (:wall-cross-section-parameters wall-section-parameter)
        steps-distrubution (:steps-distrubution wall-section-parameter)
        curve-parameters (:curve-parameters wall-section-parameter)]
    (create-wall-section wall-section-parameter wall-cross-section-parameters curve-parameters
                          steps-distrubution  wall-cross-section-steps wall-section-steps)))

(comment (wall-section (* 1 1) 0 0))
(defn global-curve-interp-wall-section [wall-section-parameter wall-cross-section-parameters curve-parameters
steps-distrubution wall-cross-section-steps wall-section-steps]
  (let [n (dec (count (:wall-cross-section-parameters wall-section-parameter)))
        cross-sections (mapv #(wall-cross-section % wall-cross-section-steps)
                             wall-cross-section-parameters) 
        degree (:degree curve-parameters)
        segments (- n degree)
        total-wall-section-steps (if (= steps-distrubution :per-segment) (* wall-section-steps segments)
                              wall-section-steps)
        global-curve-interp-fn (fn [Q] (global-curve-interp Q degree :point-paramater-calculation-method (:point-paramater-calculation-method curve-parameters)))
        curve-fn (fn [control-points knot-vector] (non-uniform-b-spline control-points degree knot-vector total-wall-section-steps))
        global-interpolation-parameters-fn (fn [cross-section-points]
                                             (for [index (range (inc wall-cross-section-steps))
                                                   :let [Q (mapv #(nth % index) cross-section-points)]]
                                               (global-curve-interp-fn Q)))
        linear-outer-top (:linear-outer-top curve-parameters) 
        linear-inner-top (:linear-inner-top curve-parameters)
        vertical-first-fn (fn [] 
                            (let [outer-wall-cross-section-points (mapv #(:points (:outer-wall-curve %)) cross-sections)
                                  inner-wall-cross-section-points  (reverse (mapv #(:points (:inner-wall-curve %)) cross-sections))
                                  outer-wall-global-interpolation-parameters (global-interpolation-parameters-fn outer-wall-cross-section-points)
                                  inner-wall-global-interpolation-parameters (global-interpolation-parameters-fn inner-wall-cross-section-points)
                                  outer-wall (mapv #(curve-fn (:P %) (:U %)) outer-wall-global-interpolation-parameters)
                                  inner-wall (mapv #(curve-fn (:P %) (:U %)) inner-wall-global-interpolation-parameters)] 
                              {:outer-wall outer-wall :inner-wall inner-wall :outer-floor-points (peek outer-wall) :inner-floor-points (peek inner-wall)}
                              ) 
                            ) 
        horizontal-first-fn (fn []
                              (let [outer-wall-curves-fn  (fn [outer-wall-horizontal-curves-control-points]
                                                            (for [index (range (count (nth outer-wall-horizontal-curves-control-points 0)))
                                                                       :let [points (mapv  #(nth % index) outer-wall-horizontal-curves-control-points)
                                                                             global-curve-interp-params (global-curve-interp-fn points)]]
                                                                   (if (and (zero? index ) linear-outer-top) (bezier-linear-spline points total-wall-section-steps)
                                                                    (curve-fn (:P global-curve-interp-params) (:U global-curve-interp-params)))
                                                                   )
                                                                 )
                                    inner-wall-curves-fn (fn [inner-wall-horizontal-curves-control-points]
                                                           (for [index (range (count (nth inner-wall-horizontal-curves-control-points 0)))
                                                                      :let [points  (mapv  #(nth % index) inner-wall-horizontal-curves-control-points)
                                                                             global-curve-interp-params (global-curve-interp-fn points)]]
                                                                  (if (and (= (dec (count (nth inner-wall-horizontal-curves-control-points 0))) index) linear-inner-top)
                                                                                                             (bezier-linear-spline points total-wall-section-steps) 
                                                                   (curve-fn (:P global-curve-interp-params) (:U global-curve-interp-params))))
                                                                )] 
                                (horizontal-first-outer-and-inner-walls wall-cross-section-parameters outer-wall-curves-fn inner-wall-curves-fn wall-cross-section-steps wall-section-steps total-wall-section-steps)))
        calculation-order (:calculation-order wall-section-parameter)
        walls (cond (= calculation-order :vertical-first) (vertical-first-fn) 
                (= calculation-order :horizontal-first) (horizontal-first-fn)) 
        {outer-wall :outer-wall
         inner-wall :inner-wall
         outer-floor-points :outer-floor-points
         inner-floor-points :inner-floor-points} walls 
        combined-wall (vec (concat inner-wall outer-wall))]
    (WallSection. cross-sections outer-wall inner-wall combined-wall outer-floor-points inner-floor-points)) 
  )

(defn global-curve-interp-with-end-derivatives-wall-section [wall-section-parameter wall-cross-section-parameters curve-parameters
                                        steps-distrubution wall-cross-section-steps wall-section-steps]
  (let [n (dec (count (:wall-cross-section-parameters wall-section-parameter)))
         {linear-outer-top :linear-outer-top
          linear-inner-top :linear-inner-top
          tangent-endpoint-zero-wall-cross-section-parameter  :tangent-endpoint-zero-wall-cross-section-parameter
          tangent-endpoint-n-wall-cross-section-parameter  :tangent-endpoint-n-wall-cross-section-parameter
          point-paramater-calculation-method :point-paramater-calculation-method
          knot-vector-generation-method :knot-vector-generation-method
          magnitude-estimation-method :magnitude-estimation-method
          degree :degree} curve-parameters
        cross-sections (mapv #(wall-cross-section % wall-cross-section-steps)
                             wall-cross-section-parameters)
        tangent-endpoint-zero-wall-cross-section (wall-cross-section tangent-endpoint-zero-wall-cross-section-parameter wall-cross-section-steps)
        tangent-endpoint-n-wall-cross-section (wall-cross-section tangent-endpoint-n-wall-cross-section-parameter wall-cross-section-steps)
        segments (- (+ n 3) degree)
        total-wall-section-steps (if (= steps-distrubution :per-segment) (* wall-section-steps segments)
                                     wall-section-steps)
        global-curve-interp-fn (fn [Q] (global-curve-interp Q degree :point-paramater-calculation-method (:point-paramater-calculation-method curve-parameters)))
        curve-fn (fn [control-points tangent-endpoint-zero tangent-endpoint-n] 
                   (global-curve-interp-with-end-unit-derivatives-curve control-points degree tangent-endpoint-zero tangent-endpoint-n
                                                                        total-wall-section-steps :point-paramater-calculation-method point-paramater-calculation-method
                                                                        :knot-vector-generation-method knot-vector-generation-method
                                                                        :magnitude-estimation-method magnitude-estimation-method))
        global-interpolation-curve-fn (fn [cross-section-points tangent-endpoint-zero-wall-cross-section-points
                                           tangent-endpoint-n-wall-cross-section-points]
                                             (vec (for [index (range (inc wall-cross-section-steps))
                                                   :let [Q (mapv #(nth % index) cross-section-points)
                                                         tangent-endpoint-zero (nth tangent-endpoint-zero-wall-cross-section-points index)
                                                         tangent-endpoint-n (nth tangent-endpoint-n-wall-cross-section-points index)]]
                                               (curve-fn Q tangent-endpoint-zero tangent-endpoint-n)))) 
       
        vertical-first-fn (fn []
                            (let [outer-wall-cross-section-points (mapv #(:points (:outer-wall-curve %)) cross-sections)
                                  inner-wall-cross-section-points  (reverse (mapv #(:points (:inner-wall-curve %)) cross-sections))
                                  outer-tangent-endpoint-zero-wall-cross-section-points (:points (:outer-wall-curve tangent-endpoint-zero-wall-cross-section))
                                  inner-tangent-endpoint-zero-wall-cross-section-points (:points (:inner-wall-curve tangent-endpoint-zero-wall-cross-section))
                                  outer-tangent-endpoint-n-wall-cross-section-points (:points (:outer-wall-curve tangent-endpoint-n-wall-cross-section))
                                  inner-tangent-endpoint-n-wall-cross-section-points (:points (:inner-wall-curve tangent-endpoint-n-wall-cross-section))
                                  
                                  outer-wall (global-interpolation-curve-fn outer-wall-cross-section-points outer-tangent-endpoint-zero-wall-cross-section-points outer-tangent-endpoint-n-wall-cross-section-points)
                                  inner-wall (global-interpolation-curve-fn inner-wall-cross-section-points inner-tangent-endpoint-n-wall-cross-section-points inner-tangent-endpoint-zero-wall-cross-section-points )]
                              {:outer-wall outer-wall :inner-wall inner-wall :outer-floor-points (peek outer-wall) :inner-floor-points (peek inner-wall)}))
        horizontal-first-fn (fn []
                              (let [outer-wall-curves-fn  (fn [outer-wall-horizontal-curves-control-points tangent-endpoint-zero-control-points tangent-endpoint-n-control-points]
                                                            (for [index (range (count (nth outer-wall-horizontal-curves-control-points 0)))
                                                                  :let [points (mapv  #(nth % index) outer-wall-horizontal-curves-control-points)
                                                                        tangent-endpoint-zero (nth tangent-endpoint-zero-control-points index)
                                                                        tangent-endpoint-n (nth tangent-endpoint-n-control-points index)]]
                                                              (if (and (zero? index) linear-outer-top) (bezier-linear-spline points total-wall-section-steps)
                                                                  (curve-fn points tangent-endpoint-zero tangent-endpoint-n))))
                                    inner-wall-curves-fn (fn [inner-wall-horizontal-curves-control-points tangent-endpoint-zero-control-points tangent-endpoint-n-control-points]
                                                           (for [index (range (count (nth inner-wall-horizontal-curves-control-points 0)))
                                                                 :let [points  (mapv  #(nth % index) inner-wall-horizontal-curves-control-points)
                                                                       tangent-endpoint-zero (nth tangent-endpoint-zero-control-points index)
                                                                       tangent-endpoint-n (nth tangent-endpoint-n-control-points index)]]
                                                             (if (and (= (dec (count (nth inner-wall-horizontal-curves-control-points 0))) index) linear-inner-top)
                                                               (bezier-linear-spline points total-wall-section-steps)
                                                               (curve-fn points tangent-endpoint-zero tangent-endpoint-n))))]
                                (horizontal-first-outer-and-inner-walls-for-global wall-cross-section-parameters tangent-endpoint-zero-wall-cross-section-parameter tangent-endpoint-n-wall-cross-section-parameter 
                                                                                   outer-wall-curves-fn inner-wall-curves-fn wall-cross-section-steps wall-section-steps total-wall-section-steps)))
        calculation-order (:calculation-order wall-section-parameter)
        walls (cond (= calculation-order :vertical-first) (vertical-first-fn)
                    (= calculation-order :horizontal-first) (horizontal-first-fn))
        {outer-wall :outer-wall
         inner-wall :inner-wall
         outer-floor-points :outer-floor-points
         inner-floor-points :inner-floor-points} walls
        combined-wall (vec (concat inner-wall outer-wall))]
    (WallSection. cross-sections outer-wall inner-wall combined-wall outer-floor-points inner-floor-points)))



(comment (spit "things-low/horizontal-first-test.scad"
          (write-scad
          (include "../BOSL2/std.scad")
           key-holes 
           (union (let [curve-paramater (global-curve-interpolation-with-end-derivatives-parameters 2 (wall-cross-section-parameter (key-wall-position lastcol 2 1 -1 :br))
                                                                                               (wall-cross-section-parameter (key-wall-position lastcol 0 1 1 :tr))
                                                                                                    :point-paramater-calculation-method :chordal
                                                                                                    :magnitude-estimation-method :arc
                                                                                                    ;  :linear-outer-top true :linear-inner-top true
                                                                                                    ) 
                        wall-section-parameter (wall-section-parameter 
                                       [(wall-cross-section-parameter (key-wall-position lastcol 2 1 0 :br))
                                        (wall-cross-section-parameter (key-wall-position lastcol 2 1 0 :tr))
                                        (wall-cross-section-parameter (key-wall-position lastcol 1 1 0 :br))
                                        (wall-cross-section-parameter (key-wall-position lastcol 1 1 0 :tr))
                                        (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :br))
                                        (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :tr)) 
                                        ]
                                        curve-paramater; :calculation-order
                                                ) 
               wall-cross-section-steps 60 
               wall-section-steps 60
               wall-cross-section-parameters (:wall-cross-section-parameters wall-section-parameter)
               steps-distrubution (:steps-distrubution wall-section-parameter)
               curve-parameters (:curve-parameters wall-section-parameter)]
                    (println "test magnitude-estimation-method" (:magnitude-estimation-method curve-paramater))
           (vnf-polyhedron (wall-vnf (wall-section wall-section-parameter  wall-cross-section-steps wall-section-steps)
            {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))))))
         )

(comment (let [control (repeat 10  [0  2])
               [outer inner] (reduce #(vector (nth 0) ) control)]
           outer))
(defmethod create-wall-section [WallSectionParameter GlobalCurveInterpolationWithEndDerivativesParameters]
  [wall-section-parameter wall-cross-section-parameters curve-parameters
steps-distrubution  wall-cross-section-steps wall-section-steps] 
   (global-curve-interp-with-end-derivatives-wall-section wall-section-parameter wall-cross-section-parameters curve-parameters
steps-distrubution  wall-cross-section-steps wall-section-steps)
  )

(defn global-curve-interp-with-first-derivatives-wall-section [wall-section-parameter wall-cross-section-parameters curve-parameters
                                                             steps-distrubution wall-cross-section-steps wall-section-steps]
  (let [n (dec (count (:wall-cross-section-parameters wall-section-parameter)))
        {linear-outer-top :linear-outer-top
         linear-inner-top :linear-inner-top 
         point-paramater-calculation-method :point-paramater-calculation-method 
         magnitude-estimation-method :magnitude-estimation-method
         tangent-vectors-start-and-endpoint-cross-section-parameters :tangent-vectors-start-and-endpoint-cross-section-parameters 
         degree :degree} curve-parameters
        cross-sections (mapv #(wall-cross-section % wall-cross-section-steps)
                             wall-cross-section-parameters)
        ;tangent-vector-cross-section (mapv tangent-vector-cross-section-fn tangent-vectors-start-and-endpoint-cross-section-parameters) 
        segments (+ (- (* 2 n) degree) 2)
        total-wall-section-steps (if (= steps-distrubution :per-segment) (* wall-section-steps segments)
                                     wall-section-steps)
        global-curve-interp-fn (fn [Q] (global-curve-interp Q degree :point-paramater-calculation-method (:point-paramater-calculation-method curve-parameters)))
        curve-fn (fn [control-points  tangent-vectors]
                   (global-curve-interp-with-calculated-first-derivatives-curve control-points tangent-vectors degree 
                                                                        total-wall-section-steps :point-paramater-calculation-method point-paramater-calculation-method 
                                                                        :magnitude-estimation-method magnitude-estimation-method))
        global-interpolation-curve-fn (fn [cross-section-points tangent-vector-cross-section]
                                        
                                        (vec (for [index (range (inc wall-cross-section-steps))
                                              :let [Q (mapv #(nth % index) cross-section-points)
                                                    tangent-vectors (mapv #(nth % index) tangent-vector-cross-section) ]] 
                                           (curve-fn Q tangent-vectors))))

        vertical-first-fn (fn []
                            (let [outer-wall-cross-section-points (mapv #(:points (:outer-wall-curve %)) cross-sections)
                                  inner-wall-cross-section-points  (reverse (mapv #(:points (:inner-wall-curve %)) cross-sections))
                                  wall-cross-section-tangent-vectors (wall-cross-section-tangent-vectors-for-wall-section tangent-vectors-start-and-endpoint-cross-section-parameters wall-cross-section-steps)
                                  outer-tangent-vector-cross-section-points (mapv #(:wall-cross-section-tangent-vectors-outer %) wall-cross-section-tangent-vectors)
                                  inner-tangent-vector-cross-section-points (reverse (mapv #(:wall-cross-section-tangent-vectors-inner %) wall-cross-section-tangent-vectors))

                                  outer-wall (global-interpolation-curve-fn outer-wall-cross-section-points outer-tangent-vector-cross-section-points)
                                  inner-wall (global-interpolation-curve-fn inner-wall-cross-section-points inner-tangent-vector-cross-section-points)]
                              {:outer-wall outer-wall :inner-wall inner-wall :outer-floor-points (peek outer-wall) :inner-floor-points (peek inner-wall)}))
        horizontal-first-fn (fn []
                              (let [outer-wall-curves-fn  (fn [outer-wall-horizontal-curves-control-points tangent-vectors-for-wall-section]
                                                            (for [index (range (count (nth outer-wall-horizontal-curves-control-points 0)))
                                                                  :let [points (mapv  #(nth % index) outer-wall-horizontal-curves-control-points)
                                                                        tangent-vectors (mapv #(nth % index) tangent-vectors-for-wall-section)]]
                                                              (if (and (zero? index) linear-outer-top) (bezier-linear-spline points total-wall-section-steps)
                                                                  (curve-fn points tangent-vectors))))
                                    inner-wall-curves-fn (fn [inner-wall-horizontal-curves-control-points tangent-vectors-for-wall-section]
                                                           (for [index (range (count (nth inner-wall-horizontal-curves-control-points 0)))
                                                                 :let [points  (mapv  #(nth % index) inner-wall-horizontal-curves-control-points) 
                                                                       tangent-vectors (mapv #(nth % index) tangent-vectors-for-wall-section)]]
                                                             (if (and (= (dec (count (nth inner-wall-horizontal-curves-control-points 0))) index) linear-inner-top)
                                                               (bezier-linear-spline points total-wall-section-steps)
                                                               (curve-fn points tangent-vectors))))]
                                (horizontal-first-outer-and-inner-walls-with-tangent-vectors wall-cross-section-parameters tangent-vectors-start-and-endpoint-cross-section-parameters
                                                                                   outer-wall-curves-fn inner-wall-curves-fn wall-cross-section-steps wall-section-steps total-wall-section-steps)))
        calculation-order (:calculation-order wall-section-parameter)
        walls (cond (= calculation-order :vertical-first) (vertical-first-fn)
                    (= calculation-order :horizontal-first) (horizontal-first-fn))
        {outer-wall :outer-wall
         inner-wall :inner-wall
         outer-floor-points :outer-floor-points
         inner-floor-points :inner-floor-points} walls
        combined-wall (vec (concat inner-wall outer-wall))]
    (WallSection. cross-sections outer-wall inner-wall combined-wall outer-floor-points inner-floor-points)))

(defmethod create-wall-section [WallSectionParameter GlobalCurveInterpolationWithCalculatedFirstDerivativesParameters]
  [wall-section-parameter wall-cross-section-parameters curve-parameters
   steps-distrubution  wall-cross-section-steps wall-section-steps]
  (global-curve-interp-with-first-derivatives-wall-section wall-section-parameter wall-cross-section-parameters curve-parameters
                                                         steps-distrubution  wall-cross-section-steps wall-section-steps))

(comment (spit "things-low/horizontal-first-test.scad"
               (write-scad
                (include "../BOSL2/std.scad")
                key-holes
                (union (let [curve-paramater (global-curve-interp-with-first-derivatives-parameters
                                              [(wall-cross-section-tangent-parameter (wall-cross-section-parameter (key-wall-position lastcol 2 1 0 :tr)) (wall-cross-section-parameter (key-wall-position lastcol 2 1 0 :br)))
                                               (wall-cross-section-tangent-parameter (wall-cross-section-parameter (key-wall-position lastcol 1 1 0 :br)) (wall-cross-section-parameter (key-wall-position lastcol 2 1 0 :tr)))
                                               (wall-cross-section-tangent-parameter (wall-cross-section-parameter (key-wall-position lastcol 1 1 0 :tr))(wall-cross-section-parameter (key-wall-position lastcol 1 1 0 :br)))
                                               (wall-cross-section-tangent-parameter (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :br)) (wall-cross-section-parameter (key-wall-position lastcol 1 1 0 :tr)))
                                               (wall-cross-section-tangent-parameter (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :tr)) (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :br)))
                                               (wall-cross-section-tangent-parameter (wall-cross-section-parameter (key-wall-position lastcol 0 1 1 :tr :offset [0.001 0.0001 0])) (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :tr)))]
                                              3  :point-paramater-calculation-method :circular :magnitude-estimation-method :chord
                                                                                                    ;  :linear-outer-top true :linear-inner-top true
                                                                                                         )
                             wall-section-parameter (wall-section-parameter
                                                     [(wall-cross-section-parameter (key-wall-position lastcol 2 1 0 :br))
                                                      (wall-cross-section-parameter (key-wall-position lastcol 2 1 0 :tr))
                                                      (wall-cross-section-parameter (key-wall-position lastcol 1 1 0 :br))
                                                      (wall-cross-section-parameter (key-wall-position lastcol 1 1 0 :tr))
                                                      (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :br))
                                                      (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :tr))]
                                                     curve-paramater ;:calculation-order :vertical-first
                                                     )
                             wall-cross-section-steps 60
                             wall-section-steps 60
                             wall-cross-section-parameters (:wall-cross-section-parameters wall-section-parameter)
                             steps-distrubution (:steps-distrubution wall-section-parameter)
                             curve-parameters (:curve-parameters wall-section-parameter)]
                         (println "test magnitude-estimation-method" (:magnitude-estimation-method curve-paramater))
                         (vnf-polyhedron (wall-vnf (wall-section wall-section-parameter  wall-cross-section-steps wall-section-steps)
                                                   {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default})))))))

(defn local-cubic-curve-interpolation-with-calculated-tangents-wall-section [wall-section-parameter wall-cross-section-parameters curve-parameters
steps-distrubution wall-cross-section-steps wall-section-steps]
  (let [cross-sections (mapv #(wall-cross-section % wall-cross-section-steps)
                             wall-cross-section-parameters)
        ;curve-parameters (:curve-parameters wall-section-parameter)
        corner-preservaton (:corner-preservation curve-parameters)
        n (dec (count wall-cross-section-parameters))
        total-wall-section-steps (if (= steps-distrubution :per-segment) (* (inc n) 2 wall-section-steps)
            wall-section-steps)
        linear-outer-top (:linear-outer-top curve-parameters)
        linear-inner-top (:linear-inner-top curve-parameters)
        {start-segment :start-segment 
         end-segment :end-segment} curve-parameters 
        start-segment-inner (- n end-segment)
        end-segment-inner (- n start-segment)
        local-curve-interp-fn (fn [points](local-cubic-curve-interpolation-with-calculated-tangents points corner-preservaton))
        local-interpolation-parameters-fn (fn [cross-section-points start-segment-index end-segment-index]
                                            (vec (for [index (range (inc wall-cross-section-steps))
                                                  :let [points (mapv #(nth % index) cross-section-points)
                                                        tangents (calculate-tangents-for-local-cubic-curve-interpolation-from-tangent points)]]
                                                   
                                              ;(local-curve-interp-fn points):start-segment start-segment :end-segment end-segment
                                              (local-cubic-curve-interpolation-with-tangents (subvec points start-segment-index (inc end-segment-index)) (subvec tangents start-segment-index (inc end-segment-index)) ))))
        curve-fn (fn [control-points knot-vector] (non-uniform-b-spline control-points 3 knot-vector total-wall-section-steps))
        vertical-first-fn (fn []
                            (let [outer-wall-cross-section-points (mapv #(:points (:outer-wall-curve %)) cross-sections)
                                  inner-wall-cross-section-points  (reverse (mapv #(:points (:inner-wall-curve %)) cross-sections))
                                  outer-wall-local-interpolation-parameters (local-interpolation-parameters-fn outer-wall-cross-section-points start-segment end-segment)
                                  inner-wall-local-interpolation-parameters (local-interpolation-parameters-fn inner-wall-cross-section-points start-segment-inner end-segment-inner)
                                  outer-wall (mapv #(curve-fn (:P %) (:U %)) outer-wall-local-interpolation-parameters)
                                  inner-wall (mapv #(curve-fn (:P %) (:U %)) inner-wall-local-interpolation-parameters)
                                  ]
                              {:outer-wall outer-wall :inner-wall inner-wall :outer-floor-points (peek outer-wall) :inner-floor-points (peek inner-wall)}))
        horizontal-first-fn (fn []
                              (let [outer-wall-curves-fn  (fn [outer-wall-horizontal-curves-control-points] 
                                                            (for [index (range (count (nth outer-wall-horizontal-curves-control-points 0)))
                                                                  :let [points (mapv  #(nth % index) outer-wall-horizontal-curves-control-points)
                                                                        local-interpolation-parameters (local-curve-interp-fn points)]]
                                                              
                                                                (if (and (zero? index) linear-outer-top) (bezier-linear-spline points total-wall-section-steps)
                                                                  (curve-fn (:P local-interpolation-parameters) (:U local-interpolation-parameters)))))
                                    inner-wall-curves-fn (fn [inner-wall-horizontal-curves-control-points]
                                                           (for [index (range (count (nth inner-wall-horizontal-curves-control-points 0)))
                                                                 :let [points  (mapv  #(nth % index) inner-wall-horizontal-curves-control-points)
                                                                       global-curve-interp-params (local-curve-interp-fn points)]]
                                                             (if (and (= (dec (count (nth inner-wall-horizontal-curves-control-points 0))) index) linear-inner-top)
                                                               (bezier-linear-spline points total-wall-section-steps)
                                                               (curve-fn (:P global-curve-interp-params) (:U global-curve-interp-params)))))]
                                (horizontal-first-outer-and-inner-walls wall-cross-section-parameters outer-wall-curves-fn inner-wall-curves-fn wall-cross-section-steps wall-section-steps total-wall-section-steps)))
        key-corner-style (:key-corner-style curve-parameters)
        ;; top-outer-row (mapv  #(nth % 0) outer-wall-cross-section-points)
        ;; n (dec (count outer-wall-cross-section-points)) 
        calculation-order (:calculation-order wall-section-parameter)
        walls (cond (= calculation-order :vertical-first) (vertical-first-fn)
                    (= calculation-order :horizontal-first) (horizontal-first-fn))
        {outer-wall :outer-wall
         inner-wall :inner-wall
         outer-floor-points :outer-floor-points
         inner-floor-points :inner-floor-points} walls
        combined-wall (vec (concat inner-wall outer-wall))] 
    
    (WallSection. cross-sections outer-wall inner-wall combined-wall outer-floor-points inner-floor-points)
    )
)

(comment (spit "things-low/horizontal-first-test.scad"
               (write-scad
                (include "../BOSL2/std.scad")
                key-holes
                (union (let [curve-paramater (local-cubic-curve-interpolation-with-calculated-tangents-parameter
                                              ;:linear-outer-top true :linear-inner-top true
                                              )
                             wall-section-parameter (wall-section-parameter
                                                     [;(wall-cross-section-parameter (key-wall-position 1 2 0 -1 :br))
                                                      (wall-cross-section-parameter (key-wall-position 2 2 0 -1 :bm ))
                                                      (wall-cross-section-parameter (key-wall-position 3 2 -1 0 :bl :offset [-0.00001 0 0]))
                                                      (wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bl))
                                                      ;(wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bm))
                                                      (wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :br))
                                                      ;(wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bm))
                                                      ;(wall-cross-section-parameter (key-wall-position lastcol 2 -1 0 :lm))
                                                      (wall-cross-section-parameter (key-wall-position lastcol 2 -1 0 :bl :offset [-0.00001 0 0]))
                                                      (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :bl :offset [0 -0.00001 0])) 
                                                      ;(wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :br))
                                                      ;(wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :tr))
                                                      ]
                                                     curve-paramater ;:calculation-order :vertical-first
                                                     )
                             wall-cross-section-steps 60
                             wall-section-steps 60
                             wall-cross-section-parameters (:wall-cross-section-parameters wall-section-parameter)
                             steps-distrubution (:steps-distrubution wall-section-parameter)
                             curve-parameters (:curve-parameters wall-section-parameter)]
                         (vnf-polyhedron (wall-vnf (create-wall-section wall-section-parameter wall-cross-section-parameters curve-paramater
                                                                                     :per-segment wall-cross-section-steps wall-section-steps)
                                                   {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default})))))))

(defmethod create-wall-section [WallSectionParameter LocalCubicCurveInterpolationWithCalculatedTangentParameters]
  [wall-section-parameter wall-cross-section-parameters curve-parameters
steps-distrubution  wall-cross-section-steps wall-section-steps]
  (local-cubic-curve-interpolation-with-calculated-tangents-wall-section wall-section-parameter wall-cross-section-parameters curve-parameters
steps-distrubution  wall-cross-section-steps wall-section-steps))


(defn local-cubic-curve-interpolation-wall-section [wall-section-parameter wall-cross-section-parameters curve-parameters
                                                                             steps-distrubution wall-cross-section-steps wall-section-steps]
  (let [cross-sections (mapv #(wall-cross-section % wall-cross-section-steps)
                             wall-cross-section-parameters)
        ;curve-parameters (:curve-parameters wall-section-parameter) 
        total-wall-section-steps (if (= steps-distrubution :per-segment) (* (count wall-cross-section-parameters) 2 wall-section-steps)
                              wall-section-steps)
        {tangents :tangents
         tangents-scale :tangents-scale
        linear-outer-top :linear-outer-top
        linear-inner-top :linear-inner-top 
         } curve-parameters
        local-curve-interp-fn (fn [points tangents] (local-cubic-curve-interpolation points tangents))
        local-interpolation-parameters-fn (fn [cross-section-points cross-section-tangents]
                                            (for [index (range (inc wall-cross-section-steps))
                                                  :let [points (mapv #(nth % index) cross-section-points)
                                                        tangents (mapv #(nth % index) cross-section-points)]]
                                              (local-curve-interp-fn points tangents)))
        curve-fn (fn [control-points knot-vector] (non-uniform-b-spline control-points 3 knot-vector total-wall-section-steps))
        vertical-first-fn (fn []
                            (let [outer-wall-cross-section-points (mapv #(:points (:outer-wall-curve %)) cross-sections)
                                  inner-wall-cross-section-points  (reverse (mapv #(:points (:inner-wall-curve %)) cross-sections))
                                  outer-wall-cross-section-tangents (mapv #(:points (:outer-wall-curve %)) tangents)
                                  inner-wall-cross-section-tangents (reverse (mapv #(:points (:inner-wall-curve %)) tangents))
                                  outer-wall-local-interpolation-parameters (local-interpolation-parameters-fn outer-wall-cross-section-points outer-wall-cross-section-tangents)
                                  inner-wall-local-interpolation-parameters (local-interpolation-parameters-fn inner-wall-cross-section-points inner-wall-cross-section-tangents)
                                  outer-wall (mapv #(curve-fn (:P %) (:U %)) outer-wall-local-interpolation-parameters)
                                  inner-wall (mapv #(curve-fn (:P %) (:U %)) inner-wall-local-interpolation-parameters)]
                              {:outer-wall outer-wall :inner-wall inner-wall :outer-floor-points (peek outer-wall) :inner-floor-points (peek inner-wall)}))
        horizontal-first-fn (fn []
                              (let [outer-wall-curves-fn  (fn [outer-wall-horizontal-curves-control-points]
                                                            (for [index (range (count (nth outer-wall-horizontal-curves-control-points 0)))
                                                                  :let [points (mapv  #(nth % index) outer-wall-horizontal-curves-control-points)
                                                                        local-interpolation-parameters (local-curve-interp-fn points)]]

                                                              (if (and (zero? index) linear-outer-top) (bezier-linear-spline points total-wall-section-steps)
                                                                  (curve-fn (:P local-interpolation-parameters) (:U local-interpolation-parameters)))))
                                    inner-wall-curves-fn (fn [inner-wall-horizontal-curves-control-points]
                                                           (for [index (range (count (nth inner-wall-horizontal-curves-control-points 0)))
                                                                 :let [points  (mapv  #(nth % index) inner-wall-horizontal-curves-control-points)
                                                                       global-curve-interp-params (local-curve-interp-fn points)]]
                                                             (if (and (= (dec (count (nth inner-wall-horizontal-curves-control-points 0))) index) linear-inner-top)
                                                               (bezier-linear-spline points total-wall-section-steps)
                                                               (curve-fn (:P global-curve-interp-params) (:U global-curve-interp-params)))))]
                                (horizontal-first-outer-and-inner-walls wall-cross-section-parameters outer-wall-curves-fn inner-wall-curves-fn wall-cross-section-steps wall-section-steps total-wall-section-steps)))
        key-corner-style (:key-corner-style curve-parameters)
        ;; top-outer-row (mapv  #(nth % 0) outer-wall-cross-section-points)
        ;; n (dec (count outer-wall-cross-section-points)) 
        calculation-order (:calculation-order wall-section-parameter)
        walls (cond (= calculation-order :vertical-first) (vertical-first-fn)
                    (= calculation-order :horizontal-first) (horizontal-first-fn))
        {outer-wall :outer-wall
         inner-wall :inner-wall
         outer-floor-points :outer-floor-points
         inner-floor-points :inner-floor-points} walls
        combined-wall (vec (concat inner-wall outer-wall))]

    (WallSection. cross-sections outer-wall inner-wall combined-wall outer-floor-points inner-floor-points)))

(defmethod create-wall-section [WallSectionParameter LocalCubicCurveInterpolationParameters]
  [wall-section-parameter wall-cross-section-parameters curve-parameters
   steps-distrubution  wall-cross-section-steps wall-section-steps]
  (local-cubic-curve-interpolation-wall-section wall-section-parameter wall-cross-section-parameters curve-parameters
                                                                         steps-distrubution  wall-cross-section-steps wall-section-steps))

(comment (spit "things-low/horizontal-first-test.scad"
               (write-scad
                (include "../BOSL2/std.scad")
                key-holes
                (union (let [curve-paramater (local-cubic-curve-interpolation-parameter
                                              [[1 0 0] [1 -1 0] [1 0 0] [1 0 0]]
                                              :linear-outer-top true :linear-inner-top true)
                             wall-section-parameter (wall-section-parameter
                                                     [(wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bl))
                                                      ;(wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :br)) 
                                                      (wall-cross-section-parameter (key-wall-position lastcol 2 -1 0 :bl :offset [-0.00001 0 0]))
                                                      (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :bl :offset [0 -0.00001 0]))
                                                      (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :br))
                                                      ;(wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :tr))
                                                      ]
                                                     curve-paramater ;:calculation-order :vertical-first
                                                     )
                             wall-cross-section-steps 60
                             wall-section-steps 60
                             wall-cross-section-parameters (:wall-cross-section-parameters wall-section-parameter)
                             steps-distrubution (:steps-distrubution wall-section-parameter)
                             curve-parameters (:curve-parameters wall-section-parameter)]
                         (vnf-polyhedron (wall-vnf (create-wall-section wall-section-parameter wall-cross-section-parameters curve-paramater
                                                                        :per-segment wall-cross-section-steps wall-section-steps)
                                                   {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default})))))))

(defn catmull-rom-spline-wall-section [wall-section-parameter wall-cross-section-parameters curve-parameters
steps-distrubution  wall-cross-section-steps wall-section-steps]
  (let [wall-cross-section-parameters (:wall-cross-section-parameters wall-section-parameter)
        steps-distrubution (:steps-distrubution wall-section-parameter)
        segments (- (count wall-cross-section-parameters) 3)
        total-wall-section-steps (if (= steps-distrubution :per-segment) (* wall-section-steps segments)
                              wall-section-steps)
        cross-sections (mapv #(wall-cross-section % wall-cross-section-steps)
                             wall-cross-section-parameters)
        curve-parameters (:curve-parameters wall-section-parameter)
        linear-outer-top (:linear-outer-top curve-parameters)
        linear-inner-top  (:linear-inner-top curve-parameters)
        alpha (:alpha curve-parameters)

        linear-segments (- (count wall-cross-section-parameters) 3)
        linear-fn (fn [points] (let [cut-points (subvec points 1 (dec (count points)))]
                                  (vec (apply concat (for [index (range segments)
                                                           :let [is-not-last (not= index (dec segments)) 
                                                                 
                                                                 line (if (even? index) 
                                                                        (catmull-rom-spline-curve [(nth points index) (nth points (inc index)) (nth points (+ index 2)) (nth points (+ index 3))]
                                                                                                  wall-section-steps :alphaType alpha)
                                                                        (n-degree-bezier-curve [(nth points (inc index)) (nth points (+ index 2))]
                                                                                               wall-section-steps)
                                                                          )]]
                                                       (if is-not-last (drop-last line) line))))
                                ;(bezier-linear-spline cut-points  total-wall-section-steps)
                                 ))
        curve-fn (fn [points should-be-linear] (if should-be-linear
                                                 (linear-fn points)
                                                 (catmull-rom-spline-curve points total-wall-section-steps :alphaType alpha)))
        curves-from-cross-sections-fn (fn [cross-sectons linear-cond top-index]
                                        (vec (for [index (range (inc wall-cross-section-steps))
                                                   :let [points (mapv #(nth % index) cross-sectons)]] 
                                                   (if (and (= top-index index) linear-cond)
                                                     (linear-fn  points)
                                                     (catmull-rom-spline-curve points total-wall-section-steps :alphaType alpha)))))
        vertical-first-fn (fn []
                            (let [outer-wall-cross-section-points (mapv #(:points (:outer-wall-curve %)) cross-sections)
                                  inner-wall-cross-section-points  (reverse (mapv #(:points (:inner-wall-curve %)) cross-sections))
                                  outer-wall (curves-from-cross-sections-fn outer-wall-cross-section-points linear-outer-top 0)
                                  inner-wall (curves-from-cross-sections-fn inner-wall-cross-section-points linear-inner-top 0)]
                              {:outer-wall outer-wall :inner-wall inner-wall}))
        horizontal-first-fn (fn []
                              (let [outer-wall-curves-fn (fn [outer-wall-horizontal-curves-control-points]
                                                           (vec (for [index (range (count (nth outer-wall-horizontal-curves-control-points 0)))
                                                                 :let [points (mapv  #(nth % index) outer-wall-horizontal-curves-control-points)]] 
                                                               (curve-fn points (and (zero? index) linear-outer-top)))))
                                    inner-wall-curves-fn (fn [inner-wall-horizontal-curves-control-points]
                                                           (vec (for [index (range (count (nth inner-wall-horizontal-curves-control-points 0)))
                                                                 :let [points (mapv  #(nth % index) inner-wall-horizontal-curves-control-points)]]
                                                             (curve-fn points (and (= (dec (count (nth inner-wall-horizontal-curves-control-points 0))) index) linear-inner-top)))))]
                                (horizontal-first-outer-and-inner-walls wall-cross-section-parameters outer-wall-curves-fn inner-wall-curves-fn wall-cross-section-steps wall-section-steps total-wall-section-steps)))
        ;linear-segments (- (count outer-wall-cross-section-points) 3)
        ;linear-segment-steps (floor (/ wall-section-steps linear-segments))
        ;; outer-wall-linear-top (vec (apply concat (for [index (range 1 (inc linear-segments))
        ;;                             :let [points (bezier-linear (nth outer-wall-cross-section-top-points index)
        ;;                                                         (nth outer-wall-cross-section-top-points (inc index))
        ;;                                                         linear-segment-steps)]]
        ;;                         (do
        ;;                           (println "index top" index)
        ;;                           (if (< index  linear-segments)
        ;;                             (drop-last points)
        ;;                             points))
        ;;                         )))
        calculation-order (:calculation-order wall-section-parameter)
        walls (cond (= calculation-order :vertical-first) (vertical-first-fn)
                    (= calculation-order :horizontal-first) (horizontal-first-fn))
        {outer-wall :outer-wall
         inner-wall :inner-wall
         outer-floor-points :outer-floor-points
         inner-floor-points :inner-floor-points} walls
        combined-wall (vec (concat outer-wall inner-wall))] 
    (WallSection. cross-sections outer-wall inner-wall combined-wall outer-floor-points inner-floor-points)))


(comment (spit "things-low/horizontal-first-test.scad"
               (write-scad
                (include "../BOSL2/std.scad")
                key-holes
                (union (let [curve-paramater (catmull-rom-spline-parameters :linear-outer-top true :linear-inner-top true
                                                                            )
                             wall-section-parameter (wall-section-parameter
                                                      
                                                      (vec 
                                                                  
                                                            (apply concat
                                                                   (for [index (reverse (range 0 (inc lastcol)))]
                                                                     [(wall-cross-section-parameter (key-wall-position index 0 0 1 :tr) :outer-wall-parameters (outer-wall-vertical-bezier-parameters))
                                                                      (wall-cross-section-parameter (key-wall-position index 0 0 1 :tl) :outer-wall-parameters (outer-wall-vertical-bezier-parameters))])))
                                                     curve-paramater)
                             wall-cross-section-steps 30
                             wall-section-steps 30
                             wall-cross-section-parameters (:wall-cross-section-parameters wall-section-parameter)
                             steps-distrubution (:steps-distrubution wall-section-parameter)
                             curve-parameters (:curve-parameters wall-section-parameter)]
                         (vnf-polyhedron (wall-vnf (catmull-rom-spline-wall-section wall-section-parameter wall-cross-section-parameters curve-paramater
                                                                                     :per-segment wall-cross-section-steps wall-section-steps)
                                                   {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default})))))))

(defmethod create-wall-section [WallSectionParameter CatmullRomSplineParameters]
  [wall-section-parameter wall-cross-section-parameters curve-parameters
steps-distrubution  wall-cross-section-steps wall-section-steps]
  (catmull-rom-spline-wall-section wall-section-parameter wall-cross-section-parameters curve-parameters
steps-distrubution  wall-cross-section-steps wall-section-steps)
  )


(defn n-degree-bezier-curve-wall-section [wall-section-parameter wall-cross-section-parameters curve-parameters
steps-distrubution  wall-cross-section-steps wall-section-steps]
  
  (let [wall-cross-section-parameters (:wall-cross-section-parameters wall-section-parameter)
        cross-sections (mapv #(wall-cross-section % wall-cross-section-steps)
                             wall-cross-section-parameters)
        n (dec (count cross-sections))
        curve-fn (fn [points]  (n-degree-bezier-curve points wall-section-steps))
        linear-outer-top (:linear-outer-top curve-parameters)
        linear-inner-top (:linear-inner-top curve-parameters)
        curves-from-cross-sections-fn (fn [cross-sections]
                                        (for [index (range (inc wall-cross-section-steps))
                                              :let [points (mapv #(nth % index) cross-sections)]]
                                          (curve-fn points)))
        vertical-first-fn (fn []
                            (let [outer-wall-cross-section-points (mapv #(:points (:outer-wall-curve %)) cross-sections)
                                  inner-wall-cross-section-points  (reverse (mapv #(:points (:inner-wall-curve %)) cross-sections))
                                  outer-wall (curves-from-cross-sections-fn outer-wall-cross-section-points)
                                  inner-wall (curves-from-cross-sections-fn inner-wall-cross-section-points)]
                              {:outer-wall outer-wall :inner-wall inner-wall :outer-floor-points (peek outer-wall) :inner-floor-points (peek inner-wall)}))
        horizontal-first-fn (fn []
                              (let [outer-wall-curves-fn  (fn [outer-wall-horizontal-curves-control-points]
                                                            (for [index (range (count (nth outer-wall-horizontal-curves-control-points 0)))
                                                                  :let [points (mapv  #(nth % index) outer-wall-horizontal-curves-control-points)]]
                                                              (if (and (zero? index) linear-outer-top) (bezier-linear-spline points wall-section-steps)
                                                                  (curve-fn points))))
                                    inner-wall-curves-fn (fn [inner-wall-horizontal-curves-control-points]
                                                           (for [index (range (count (nth inner-wall-horizontal-curves-control-points 0)))
                                                                 :let [points  (mapv  #(nth % index) inner-wall-horizontal-curves-control-points)]]
                                                             (if (and (= (dec (count (nth inner-wall-horizontal-curves-control-points 0))) index) linear-inner-top)
                                                               (bezier-linear-spline points wall-section-steps) (curve-fn points))))]
                                (horizontal-first-outer-and-inner-walls wall-cross-section-parameters outer-wall-curves-fn inner-wall-curves-fn wall-cross-section-steps wall-section-steps wall-section-steps)))
        calculation-order (:calculation-order wall-section-parameter)
        walls (cond (= calculation-order :vertical-first) (vertical-first-fn)
                    (= calculation-order :horizontal-first) (horizontal-first-fn))
        {outer-wall :outer-wall
         inner-wall :inner-wall
         outer-floor-points :outer-floor-points
         inner-floor-points :inner-floor-points} walls
        combined-wall (vec (concat outer-wall inner-wall))]
    (WallSection. cross-sections outer-wall inner-wall combined-wall outer-floor-points inner-floor-points))
  )

(comment (spit "things-low/horizontal-first-test.scad"
               (write-scad
                (include "../BOSL2/std.scad")
                key-holes
                (union (let [curve-paramater (n-degree-bezier-curve-paramaters :linear-outer-top true :linear-inner-top true)
                             wall-section-parameter (wall-section-parameter
                                                     [(wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bl)) 
                                                      (wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :br))
                                                      ;(wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :bl)) 
                                                      ]
                                                     curve-paramater)
                             wall-cross-section-steps 60
                             wall-section-steps 60
                             wall-cross-section-parameters (:wall-cross-section-parameters wall-section-parameter)
                             steps-distrubution (:steps-distrubution wall-section-parameter)
                             curve-parameters (:curve-parameters wall-section-parameter)]
                         (vnf-polyhedron (wall-vnf (n-degree-bezier-curve-wall-section wall-section-parameter wall-cross-section-parameters curve-paramater
                                                                                     :per-segment wall-cross-section-steps wall-section-steps)
                                                   {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default})))))))

(defmethod create-wall-section [WallSectionParameter NDegreeBezierCurveParamaters]
  [wall-section-parameter wall-cross-section-parameters curve-parameters
steps-distrubution  wall-cross-section-steps wall-section-steps]
  (n-degree-bezier-curve-wall-section wall-section-parameter wall-cross-section-parameters curve-parameters
steps-distrubution  wall-cross-section-steps wall-section-steps))

(defn nurbs-wall-section [wall-section-parameter wall-cross-section-parameters curve-parameters
steps-distrubution  wall-cross-section-steps wall-section-steps]
  (let [cross-sections (mapv #(wall-cross-section % wall-cross-section-steps)
                             wall-cross-section-parameters)
        {degree :degree
         knot-vector :knot-vector
         weights :weights} curve-parameters
        segments (- (count cross-sections) degree)
        total-wall-section-steps (if (= steps-distrubution :per-segment) (* wall-section-steps segments)
                              wall-section-steps)
        curve-fn (fn [points] (if (nil? knot-vector)
                                (nurbs-with-calculated-knot-vector points degree weights total-wall-section-steps)
                               (nurbs points degree knot-vector weights total-wall-section-steps)))
        curves-from-cross-sections-fn (fn [cross-sections]
                                        (for [index (range (inc wall-cross-section-steps))
                                              :let [points (mapv #(nth % index) cross-sections)]]
                                          (curve-fn points)))
        linear-outer-top (:linear-outer-top curve-parameters)
        linear-inner-top (:linear-inner-top curve-parameters)
        vertical-first-fn (fn []
                            (let [outer-wall-cross-section-points (mapv #(:points (:outer-wall-curve %)) cross-sections)
                                  inner-wall-cross-section-points  (reverse (mapv #(:points (:inner-wall-curve %)) cross-sections))
                                  outer-wall (curves-from-cross-sections-fn outer-wall-cross-section-points)
                                  inner-wall (curves-from-cross-sections-fn inner-wall-cross-section-points)]
                              {:outer-wall outer-wall :inner-wall inner-wall :outer-floor-points (peek outer-wall) :inner-floor-points (peek inner-wall)}))
        horizontal-first-fn (fn []
                              (let [outer-wall-curves-fn (fn [outer-wall-horizontal-curves-control-points]
                                                           (vec (for [index (range (count (nth outer-wall-horizontal-curves-control-points 0)))
                                                                 :let [points (mapv  #(nth % index) outer-wall-horizontal-curves-control-points)]]
                                                             (if (and (zero? index) linear-outer-top) (bezier-linear-spline points total-wall-section-steps)
                                                                 (curve-fn points)))))
                                    inner-wall-curves-fn (fn [inner-wall-horizontal-curves-control-points]
                                                           (vec (for [index (range (count (nth inner-wall-horizontal-curves-control-points 0)))
                                                                 :let [points (mapv  #(nth % index) inner-wall-horizontal-curves-control-points)]]
                                                             (if (and (= (dec (count (nth inner-wall-horizontal-curves-control-points 0))) index) linear-inner-top) (bezier-linear-spline points total-wall-section-steps)
                                                                 (curve-fn points)))))]
                                (horizontal-first-outer-and-inner-walls wall-cross-section-parameters outer-wall-curves-fn inner-wall-curves-fn wall-cross-section-steps wall-section-steps total-wall-section-steps)))
        calculation-order (:calculation-order wall-section-parameter)
        walls (cond (= calculation-order :vertical-first) (vertical-first-fn)
                    (= calculation-order :horizontal-first) (horizontal-first-fn))
        {outer-wall :outer-wall
         inner-wall :inner-wall
         outer-floor-points :outer-floor-points
         inner-floor-points :inner-floor-points} walls
        combined-wall (vec (concat outer-wall inner-wall))] 
    (WallSection. cross-sections outer-wall inner-wall combined-wall outer-floor-points inner-floor-points)))

(defmethod create-wall-section [WallSectionParameter NurbsParameters]
[wall-section-parameter wall-cross-section-parameters curve-parameters
steps-distrubution  wall-cross-section-steps wall-section-steps]
  (nurbs-wall-section wall-section-parameter wall-cross-section-parameters curve-parameters
steps-distrubution  wall-cross-section-steps wall-section-steps)
  )

(comment (double (* 7 8)))

(comment (spit "things-low/horizontal-first-test.scad"
               (write-scad
                (include "../BOSL2/std.scad")
                key-holes
                (union (let [curve-paramater (nurbs-parameters 2 (vec (reverse [1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1  1]))
                                                               :knot-vector (mapv (partial * 10)[0 0 0 (/ 1 11) (/ 2 11) (/ 3 11) (/ 5 11) (/ 6 11) (/ 7 11)(/ 8 11) (/ 9.8 11)  (/ 9.9 11) (/ 11 11) (/ 11 11) (/ 11 11)])
                                                               :linear-outer-top false
                                                               :linear-inner-top false)
                             wall-section-parameter (wall-section-parameter
                                                     [(wall-cross-section-parameter (key-wall-position 1 2 1 0 :br :xy 3 ))
                                                      (wall-cross-section-parameter (key-wall-position 2 2 -1 -1 :bl  :xy 3 :slant :no-slant))
                                                      (wall-cross-section-parameter (key-wall-position 2 2 0 -1 :bm  :xy 3 :slant :no-slant))
                                                      (wall-cross-section-parameter (key-wall-position 2 2 -1 -1 :br :slant :no-slant :xy 4))
                                                      (wall-cross-section-parameter (key-wall-position 3 2 -1 0 :bl :offset [0.000001 0 0] :slant :no-slant :xy 4))
                                                      (wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :bl :slant :no-slant :xy 4.5))
                                                      (wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bl :offset [0 0.000001 0]))
                                                      (wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :br :slant :no-slant))
                                                      (wall-cross-section-parameter (key-wall-position lastcol 2 -1 0 :bl :slant :no-slant :offset [0.000001 0 0]))
                                                      (wall-cross-section-parameter (key-wall-position lastcol 2 -1 -1 :bl :slant :no-slant))
                                                      (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :bl :slant :no-slant :offset [0 0.000001 0] ))
                                                      (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :br))
                                                      ;(wall-cross-section-parameter (key-wall-position 3 2 -1 0 :bl))
                                                      ]
                                                     curve-paramater)
                             wall-cross-section-steps 30
                             wall-section-steps 30
                             wall-cross-section-parameters (:wall-cross-section-parameters wall-section-parameter)
                             steps-distrubution (:steps-distrubution wall-section-parameter)
                             curve-parameters (:curve-parameters wall-section-parameter)]
                         (vnf-polyhedron (wall-vnf (nurbs-wall-section wall-section-parameter wall-cross-section-parameters curve-paramater
                                                                                     :per-segment wall-cross-section-steps wall-section-steps)
                                                   {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default})))))))

(defmethod create-wall-section [WallSectionParameter CubicHermiteSplineSegmentParameter]
  [wall-section-parameter wall-cross-section-parameters curve-parameters
   steps-distrubution  wall-cross-section-steps wall-section-steps]
  (n-degree-bezier-curve-wall-section wall-section-parameter wall-cross-section-parameters curve-parameters
                                      steps-distrubution  wall-cross-section-steps wall-section-steps))

(defn cubic-hermite-spline-segment-parameter [wall-section-parameter wall-cross-section-parameters curve-parameters
                                              steps-distrubution  wall-cross-section-steps wall-section-steps]
  (let [{tension :tension
         linear-outer-top :linear-outer-top
         linear-inner-top :linear-inner-top} curve-parameters
        curve-fn (fn [p-zero p-one p-two p-three]
                   (let [p1t (mapv - p-one p-zero )
                         p2t (mapv - p-three p-two)]
                     (cubic-hermite-tension-spline-curve p-one p-two p1t p2t tension wall-section-steps)))
        curves-from-cross-sections-fn (fn [cross-sections]
                                        (for [index (range (inc wall-cross-section-steps))
                                              :let [points (mapv #(nth % index) cross-sections)]]
                                          (apply curve-fn points)))
        
        ]
   
  )
  
  )


(defn outer-wall-curve-nurbs [wall-control-points
                              steps
                              & {:keys [control-points-keywords weights degree knot-vector-calculation-style]
                                 :or {control-points-keywords wall-vertical-outer-nurbs-control-points-keywords weights [1 0.9  0.6 0.75 1] degree 3 knot-vector-calculation-style :centripetal}}]
  (nurbs-with-calculated-knot-vector (select-values wall-control-points control-points-keywords) degree weights steps :style knot-vector-calculation-style))

(defn outer-wall-curve-bezier-quintic [wall-control-points steps & {:keys [control-points-keywords] :or {control-points-keywords  outer-wall-curve-bezier-quintic-nurbs-keywords}}]

  (let [[p0 p1 p2 p3 p4 p5] (select-values wall-control-points control-points-keywords :throw-error-on-nil true)]
    (bezier-quintic p0 p1 p2 p3 p4 p5 steps)))

(comment (let [wall-control-points (wall-brace-polyhedron-points (partial key-place 0 0) 1 1 :tl :radians)]
           (outer-wall-curve-nurbs wall-control-points 30)))


(comment
  (select-keys {:a 1 :b 2 :c 3} [:a :c]))

(defmulti case-wall-vertical-control-points-inner )



(defn wall-brace-polyhedron-curve-points ([place dx dy post-position rad-or-deg steps] (wall-brace-polyhedron-curve-points place dx dy post-position rad-or-deg wall-xy-offset steps))
  ([place dx dy post-position rad-or-deg xy steps]
   (let [wall-brace-polyhedron-points-map (wall-brace-polyhedron-points place dx dy post-position rad-or-deg :xy xy)
         outer-points (nurbs-with-calculated-knot-vector
                       [(wall-brace-polyhedron-points-map :web-post-position-top)
                        (wall-brace-polyhedron-points-map :point-on-tangent-from-plate)
                       ; (wall-brace-polyhedron-points-map :wall-locate1-point)
                       ;(wall-brace-polyhedron-points-map :wall-locate-1-to-3-curve-for-polyhedron-control-point)
                        (wall-brace-polyhedron-points-map :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)
                        (wall-brace-polyhedron-points-map :wall-locate3-point)
                        (wall-brace-polyhedron-points-map :wall-locate3-point-floor)]
                       3
                       [1 0.9  0.6 0.75 1]
                       steps)
         inner-points (cubic-uniform-b-spline
                       [(wall-brace-polyhedron-points-map :opposite-web-post-position-bottom)
                       (wall-brace-polyhedron-points-map :web-post-position-bottom)
                       (wall-brace-polyhedron-points-map :wall-locate-2-top)
                       (wall-brace-polyhedron-points-map :wall-locate-2-bottom)
                       (wall-brace-polyhedron-points-map :wall-locate-2-bottom-floor)
                       (wall-brace-polyhedron-points-map :wall-locate-2-bottom-below-floor)]
                       steps)]
     {:outer-points outer-points
      :inner-points inner-points})))

()



(defn wall-brace-polyhedron-curves ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
                                    (wall-brace-polyhedron-curves place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps]
   (let [wall-brace-polyedron-curve-points1 (wall-brace-polyhedron-points place1 dx1 dy1 post-position-1 rad-or-deg1 :xy xy1)
         wall-brace-polyedron-curve-points2 (wall-brace-polyhedron-points place2 dx2 dy2 post-position-2 rad-or-deg2 :xy xy2)

         point-on-tangent-from-plate-curve (bezier-linear  (wall-brace-polyedron-curve-points1 :point-on-tangent-from-plate)  (wall-brace-polyedron-curve-points2 :point-on-tangent-from-plate) steps)
         web-post-top-curve (bezier-linear  (wall-brace-polyedron-curve-points1 :web-post-position-top)  (wall-brace-polyedron-curve-points2 :web-post-position-top) steps)
         wall-locate1-curve (bezier-linear  (wall-brace-polyedron-curve-points1 :wall-locate1-point)  (wall-brace-polyedron-curve-points2 :wall-locate1-point)  steps)
         wall-locate-1-to-3-curve-for-polyhedron-second-control-curve (bezier-linear  (wall-brace-polyedron-curve-points1 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)  (wall-brace-polyedron-curve-points2 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)  steps)
         wall-locate-1-to-3-curve-for-polyhedron-control-curve (bezier-linear  (wall-brace-polyedron-curve-points1 :wall-locate-1-to-3-curve-for-polyhedron-control-point)  (wall-brace-polyedron-curve-points2 :wall-locate-1-to-3-curve-for-polyhedron-control-point)  steps)
         wall-locate3-curve (bezier-linear  (wall-brace-polyedron-curve-points1 :wall-locate3-point)   (wall-brace-polyedron-curve-points2 :wall-locate3-point) steps) 
         wall-locate3-floor-curve (bezier-linear (wall-brace-polyedron-curve-points1 :wall-locate3-point-floor)  (wall-brace-polyedron-curve-points2 :wall-locate3-point-floor)  steps)
         wall-locate3-below-floor-curve (bezier-linear (wall-brace-polyedron-curve-points1 :wall-locate3-point-below-floor)  (wall-brace-polyedron-curve-points2 :wall-locate3-point-below-floor)  steps)
         wall-locate2-bottom-below-floor-curve (bezier-linear  (wall-brace-polyedron-curve-points2 :wall-locate-2-bottom-below-floor)  (wall-brace-polyedron-curve-points1 :wall-locate-2-bottom-below-floor) steps) 
         wall-locate2-bottom-floor-curve (bezier-linear  (wall-brace-polyedron-curve-points2 :wall-locate-2-bottom-floor)  (wall-brace-polyedron-curve-points1 :wall-locate-2-bottom-floor) steps)
         wall-locate2-bottom-curve (bezier-linear  (wall-brace-polyedron-curve-points2 :wall-locate-2-bottom)   (wall-brace-polyedron-curve-points1 :wall-locate-2-bottom) steps)
         wall-locate2-top-curve (bezier-linear   (wall-brace-polyedron-curve-points2 :wall-locate-2-top)  (wall-brace-polyedron-curve-points1 :wall-locate-2-top)   steps)
         web-post-bottom-curve (bezier-linear   (wall-brace-polyedron-curve-points2 :web-post-position-bottom)  (wall-brace-polyedron-curve-points1 :web-post-position-bottom) steps)
         opposite-web-post-bottom-curve (bezier-linear   (wall-brace-polyedron-curve-points2 :opposite-web-post-position-bottom)  (wall-brace-polyedron-curve-points1 :opposite-web-post-position-bottom) steps)]

     {:point-on-tangent-from-plate-curve point-on-tangent-from-plate-curve
      :web-post-top-curve web-post-top-curve
      :wall-locate1-curve wall-locate1-curve
      :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve wall-locate-1-to-3-curve-for-polyhedron-second-control-curve
      :wall-locate-1-to-3-curve-for-polyhedron-control-curve wall-locate-1-to-3-curve-for-polyhedron-control-curve
      :wall-locate3-curve wall-locate3-curve
      :wall-locate3-floor-curve wall-locate3-floor-curve
      :wall-locate3-below-floor-curve wall-locate3-below-floor-curve
      :wall-locate2-bottom-below-floor-curve wall-locate2-bottom-below-floor-curve
      :wall-locate2-bottom-floor-curve wall-locate2-bottom-floor-curve
      :wall-locate2-bottom-curve wall-locate2-bottom-curve
      :wall-locate2-top-curve wall-locate2-top-curve
      :web-post-bottom-curve web-post-bottom-curve
      :opposite-web-post-bottom-curve opposite-web-post-bottom-curve})))

(defn wall-brace-polyhedron-outer-floor-linear  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
   (wall-brace-polyhedron-outer-floor-linear place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps]
   (let [wall-brace-polyhedron-outer-floor-point1 (wall-brace-polyhedron-outer-floor-point place1 dx1 dy1 post-position-1 rad-or-deg1 xy1) 
         wall-brace-polyhedron-outer-floor-point2 (wall-brace-polyhedron-outer-floor-point place2 dx2 dy2 post-position-2 rad-or-deg2 xy2 )]
     (bezier-linear wall-brace-polyhedron-outer-floor-point1 wall-brace-polyhedron-outer-floor-point2 steps)
     )
   
   )
)

;; (deftype horizontal-curve-data [place-fn dx dy post-position rad-or-deg xy curve-type curve-data])
;; (defn case-wall-nurbs-spline [horizontal-curve-data-points steps]
;;   (let [ts (nurbs-with-calculated-knot-vector)])
;;   )

(defn wall-brace-polyhedron
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
   (wall-brace-polyhedron place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps]
   (wall-brace-polyhedron place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 false steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 extra-points-for-sides-and-top-and-bottom steps]
   (let [wall-brace-polyhedron-curves-map (wall-brace-polyhedron-curves place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps)
         wall-brace-polyedron-curve-points1 (wall-brace-polyhedron-curve-points place1 dx1 dy1 post-position-1 rad-or-deg1 xy1 steps)
wall-brace-polyedron-curve-points2 (wall-brace-polyhedron-curve-points place2 dx2 dy2 post-position-2 rad-or-deg2 xy2 steps)
         




         outer-points (into [] (apply concat
                                     (for [index (range 0 (inc steps))]
                                       (nurbs-with-calculated-knot-vector
                                        [(nth (wall-brace-polyhedron-curves-map :web-post-top-curve) index)
                                         (nth (wall-brace-polyhedron-curves-map :point-on-tangent-from-plate-curve) index)
                                          ;(nth (wall-brace-polyhedron-curves-map :wall-locate1-curve) index)
                                          ;(nth (wall-brace-polyhedron-curves-map :wall-locate-1-to-3-curve-for-polyhedron-control-curve) index)
                                         (nth (wall-brace-polyhedron-curves-map :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve) index)
                                         (nth (wall-brace-polyhedron-curves-map :wall-locate3-curve) index)
                                         (nth (wall-brace-polyhedron-curves-map :wall-locate3-floor-curve) index)
                                          ;(nth (wall-brace-polyhedron-curves-map :wall-locate3-below-floor-curve) index) 
                                         ]
                                        3
                                        [1 0.9  0.6 0.75 1]
                                        steps))))
        ;;  (into [] (apply concat
        ;;                               (for [index (range 0 (inc steps))]
        ;;                                 (nurbs-with-calculated-knot-vector
        ;;                                  [(nth (wall-brace-polyhedron-curves-map :web-post-top-curve) index)
        ;;                                   (nth (wall-brace-polyhedron-curves-map :point-on-tangent-from-plate-curve) index)
        ;;                                   ;(nth (wall-brace-polyhedron-curves-map :wall-locate1-curve) index)
        ;;                                   ;(nth (wall-brace-polyhedron-curves-map :wall-locate-1-to-3-curve-for-polyhedron-control-curve) index)
        ;;                                   (nth (wall-brace-polyhedron-curves-map :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve) index)
        ;;                                   (nth (wall-brace-polyhedron-curves-map :wall-locate3-curve) index)
        ;;                                   (nth (wall-brace-polyhedron-curves-map :wall-locate3-floor-curve) index)
        ;;                                   ;(nth (wall-brace-polyhedron-curves-map :wall-locate3-below-floor-curve) index) 
        ;;                                   ]
        ;;                                  3
        ;;                                  [1 0.9  0.6 0.75 1]
        ;;                                  steps)))) 
        ;;  (into [] (apply concat
        ;;                               (for [index (range 0 (inc steps))]
        ;;                                 (bezier-quintic
        ;;                                  (nth (wall-brace-polyhedron-curves-map :web-post-top-curve) index)
        ;;                                  (nth (wall-brace-polyhedron-curves-map :wall-locate1-curve) index)
        ;;                                  (nth (wall-brace-polyhedron-curves-map :wall-locate-1-to-3-curve-for-polyhedron-control-curve) index)
        ;;                                  (nth (wall-brace-polyhedron-curves-map :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve) index)
        ;;                                  (nth (wall-brace-polyhedron-curves-map :wall-locate3-curve) index)
        ;;                                  (nth (wall-brace-polyhedron-curves-map :wall-locate3-floor-curve) index)
        ;;                                  steps))))

         inner-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                               (bezier-cubic
                                                ;(nth (wall-brace-polyhedron-curves-map :opposite-web-post-bottom-curve) index)
                                                 (nth (wall-brace-polyhedron-curves-map :web-post-bottom-curve) index)
                                                (nth (wall-brace-polyhedron-curves-map :wall-locate2-top-curve) index)
                                                (nth (wall-brace-polyhedron-curves-map :wall-locate2-bottom-curve) index)
                                                (nth (wall-brace-polyhedron-curves-map :wall-locate2-bottom-floor-curve) index)
                                                 ;(nth (wall-brace-polyhedron-curves-map :wall-locate2-bottom-below-floor-curve) index) 
                                                steps))))
         smoother-wall-polyhedron (if extra-points-for-sides-and-top-and-bottom
                                    (generate-bezier-along-bezier-polyhedron-all-sides outer-points inner-points steps)
                                   (polyhedron (concat outer-points inner-points)
                                              (generate-bezier-along-bezier-polyhedron-faces
                                               outer-points inner-points
                                               steps)))]
         ;wall-polyhedron 
     smoother-wall-polyhedron)))

;; (defn wall-brace-polyhedron 
;;   ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
;;    (wall-brace-polyhedron place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset steps))
;;   ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps]
;;    (let [transform-1 (get-transform-fn rad-or-deg1 place1)
;;         transform-2 (get-transform-fn rad-or-deg2 place2)
;;         oled-post-outer-x-1 (if (pos? dx1) (/ oled-post-size 2) (/ oled-post-size -2))
;;         oled-post-outer-x-2 (if (pos? dx2) (/ oled-post-size 2) (/ oled-post-size -2))
;;         oled-post-outer-y-1 (if (pos? dy1) (/ oled-post-size 2) (/ oled-post-size -2))
;;         oled-post-outer-y-2 (if (pos? dy2) (/ oled-post-size 2) (/ oled-post-size -2))
;;         oled-post-inner-x-1 (if (pos? dx1) (/ oled-post-size -2) (/ oled-post-size 2))
;;         oled-post-inner-x-2 (if (pos? dx2) (/ oled-post-size -2) (/ oled-post-size 2))
;;         oled-post-inner-y-1 (if (pos? dy1) (/ oled-post-size -2) (/ oled-post-size 2))
;;         oled-post-inner-y-2 (if (pos? dy2) (/ oled-post-size -2) (/ oled-post-size 2))
;;         curve-post-x-1 (if (pos? dx1) (/ curve-post-size 2) (/ curve-post-size -2))
;;         curve-post-y-1 (if (pos? dy1) (/ curve-post-size 2) (/ curve-post-size -2))
;;         curve-post-x-2 (if (pos? dx2) (/ curve-post-size 2) (/ curve-post-size -2))
;;         curve-post-y-2 (if (pos? dy2) (/ curve-post-size 2) (/ curve-post-size -2))






;;         web-corner-translation-vector1 (get-single-plate-corner-position-vector post-position-1)
;;         web-corner-translation-vector2 (get-single-plate-corner-position-vector post-position-2)
;;         web-post-position-1-top (transform-1 (web-post-position-top web-corner-translation-vector1))
;;         web-post-position-1-bottom (transform-1 (web-post-position-bottom web-corner-translation-vector1))
;;         web-post-position-2-top (transform-2 (web-post-position-top web-corner-translation-vector2))
;;         web-post-position-2-bottom (transform-2 (web-post-position-bottom web-corner-translation-vector2))
;;         oled-corner-translation-vector1 (get-oled-corner-translation-vector post-position-1)
;;         oled-corner-translation-vector2  (get-oled-corner-translation-vector post-position-2)
;;         web-corner-translation-vector1 (get-single-plate-corner-position-vector post-position-1)
;;         wneb-corner-translation-vector2 (get-single-plate-corner-position-vector post-position-2)
;;         oled-post-position-1-top (oled-post-position-top oled-corner-translation-vector1)
;;         oled-post-position-1-bottom (oled-post-position-bottom oled-corner-translation-vector1)
;;         oled-post-position-2-top (oled-post-position-top oled-corner-translation-vector2)
;;         oled-post-position-2-bottom  (oled-post-position-bottom oled-corner-translation-vector2)
;;         curve-corner-translation-vector1 (get-curve-corner-translation-vector post-position-1)
;;         curve-corner-translation-vector2 (get-curve-corner-translation-vector post-position-2)

;;         curve-post-position-1-top (mapv + (curve-post-position-top curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))
;;         curve-post-position-1-middle (mapv + (curve-post-position-middle curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))
;;         curve-post-position-1-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))
;;         curve-post-position-2-top (mapv + (curve-post-position-top curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))
;;         curve-post-position-2-middle (mapv + (curve-post-position-middle curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))
;;         curve-post-position-2-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))
;;         wall-locate1-point1 (transform-1 (mapv + (wall-locate1 dx1 dy1) curve-post-position-1-top))
;;         wall-locate1-point2 (transform-2 (mapv + (wall-locate1 dx2 dy2) curve-post-position-2-top))

;;         wall-locate-1-to-3-curve-for-polyhedron-control-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx1 dy1) curve-post-position-1-middle (get-oled-post-outer-x-and-y-vector dx1 dy1))))
;;          wall-locate-1-to-3-curve-for-polyhedron-control-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx2 dy2) curve-post-position-2-middle (get-oled-post-outer-x-and-y-vector dx2 dy2))))

;;         wall-locate-1-to-3-curve-for-polyhedron-second-control-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx1 dy1) curve-post-position-1-middle (get-oled-post-outer-x-and-y-vector dx1 dy1))))
;; wall-locate-1-to-3-curve-for-polyhedron-second-control-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx2 dy2) curve-post-position-2-middle (get-oled-post-outer-x-and-y-vector dx2 dy2))))

;;         wall-locate3-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate3-for-polyhedron-point dx1 dy1 xy1) curve-post-position-1-bottom (get-oled-post-outer-x-and-y-vector dx1 dy1))))
;;         wall-locate3-point1-floor (assoc (vec wall-locate3-point1) 2 0)
;;         wall-locate3-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate3-for-polyhedron-point dx2 dy2 xy2) curve-post-position-2-bottom (get-oled-post-outer-x-and-y-vector dx2 dy2))))

;;         wall-locate3-point2-floor (assoc (vec wall-locate3-point2) 2 0)


;;         wall-locate-2-top1 (transform-1 (wall-locate2-for-polyhedron-point dx1 dy1 (mapv + oled-post-position-1-top (get-oled-post-inner-x-and-y-vector dx1 dy1)) xy1))
;;         wall-locate-2-top2 (transform-2 (wall-locate2-for-polyhedron-point dx1 dy1 (mapv + oled-post-position-2-top (get-oled-post-inner-x-and-y-vector dx2 dy2)) xy2))
;;         wall-locate-2-bottom1 (make-point-z-value-not-below-zero (transform-1 (wall-locate2-for-polyhedron-point dx1 dy1 (mapv + oled-post-position-1-bottom (get-oled-post-inner-x-and-y-vector dx1 dy1)) xy1)))
;;         wall-locate-2-bottom1-floor (assoc (vec wall-locate-2-bottom1) 2 0)
;;         wall-locate-2-bottom2 (make-point-z-value-not-below-zero (transform-2 (wall-locate2-for-polyhedron-point dx2 dy2 (mapv + oled-post-position-2-bottom (get-oled-post-inner-x-and-y-vector dx2 dy2)) xy2)))
;;         wall-locate-2-bottom2-floor (assoc (vec wall-locate-2-bottom2) 2 0)

;;         web-post-top-curve (bezier-linear  web-post-position-1-top  web-post-position-2-top  steps)
;;         wall-locate1-curve (bezier-linear  wall-locate1-point1  wall-locate1-point2  steps)
;;          wall-locate-1-to-3-curve-for-polyhedron-second-control-curve (bezier-linear  wall-locate-1-to-3-curve-for-polyhedron-second-control-point1  wall-locate-1-to-3-curve-for-polyhedron-second-control-point2  steps)
;;         wall-locate-1-to-3-curve-for-polyhedron-control-curve (bezier-linear  wall-locate-1-to-3-curve-for-polyhedron-control-point1  wall-locate-1-to-3-curve-for-polyhedron-control-point2  steps)
;;         wall-locate3-curve (bezier-linear  wall-locate3-point1   wall-locate3-point2 steps)
;;         wall-locate3-floor-curve (bezier-linear wall-locate3-point1-floor  wall-locate3-point2-floor  steps)

;;         wall-locate2-bottom-floor-curve (bezier-linear  wall-locate-2-bottom2-floor  wall-locate-2-bottom1-floor steps)
;;         wall-locate2-bottom-curve (bezier-linear  wall-locate-2-bottom2   wall-locate-2-bottom1 steps)
;;         wall-locate2-top-curve (bezier-linear   wall-locate-2-top2  wall-locate-2-top1   steps)
;;         web-post-bottom-curve (bezier-linear   web-post-position-2-bottom  web-post-position-1-bottom steps)

;;         outer-curve1 (bezier-cubic web-post-position-1-top  wall-locate1-point1 wall-locate3-point1 wall-locate3-point1-floor    steps)
;;         inner-curve1 (bezier-cubic   wall-locate-2-bottom1-floor wall-locate-2-bottom1 wall-locate-2-top1 web-post-position-1-bottom steps)
;;         outer-curve2 (bezier-cubic web-post-position-2-top  wall-locate1-point2  wall-locate3-point2 wall-locate3-point2-floor   steps)
;;         inner-curve2 (bezier-cubic   wall-locate-2-bottom2-floor wall-locate-2-bottom2 wall-locate-2-top2 web-post-position-2-bottom steps)
;;         wall-polyhedron (generate-polyhedron-from-points  outer-curve2 outer-curve1  inner-curve2 inner-curve1   steps)

;;         outer-points (into [] (apply concat
;;                                      (for [index (range 0 (inc steps))]
;;                                        (bezier-quintic
;;                                         (nth web-post-top-curve index)
;;                                         (nth wall-locate1-curve index)
;;                                         (nth wall-locate-1-to-3-curve-for-polyhedron-control-curve index)
;;                                         (nth wall-locate-1-to-3-curve-for-polyhedron-second-control-curve index) 
;;                                         (nth wall-locate3-curve index)
;;                                         (nth wall-locate3-floor-curve index)
;;                                         steps))))

;;         inner-points (into [] (apply concat (for [index (range 0 (inc steps))]
;;                                               (bezier-cubic
;;                                                (nth web-post-bottom-curve index)
;;                                                (nth wall-locate2-top-curve index)
;;                                                (nth wall-locate2-bottom-curve index)
;;                                                (nth wall-locate2-bottom-floor-curve index)
;;                                                steps))))
;;         smoother-wall-polyhedron (polyhedron (concat outer-points inner-points)
;;                                              (generate-bezier-along-bezier-polyhedron-faces
;;                                               outer-points inner-points
;;                                               steps))]
;;          ;wall-polyhedron 
;;     smoother-wall-polyhedron)))

(defn wall-brace-polyhedron-circular-points ([place dx dy post-position rad-or-deg] (wall-brace-polyhedron-circular-points place dx dy post-position rad-or-deg wall-xy-offset))
  ([place dx dy post-position rad-or-deg xy]
   (let [transform (get-transform-fn rad-or-deg place)
         web-corner-translation-vector (get-single-plate-corner-position-vector post-position)
         web-post-position-top (transform (get-web-post-position-top web-corner-translation-vector))
         web-post-position-bottom (transform (get-web-post-position-bottom web-corner-translation-vector))
         oled-corner-translation-vector (get-oled-corner-translation-vector post-position)
         oled-post-position-top (oled-post-position-top oled-corner-translation-vector)
         oled-post-position-bottom (oled-post-position-bottom oled-corner-translation-vector)
         curve-corner-translation-vector (get-curve-corner-translation-vector post-position)
         curve-post-position-top (mapv + (curve-post-position-top curve-corner-translation-vector) (get-curve-post-outer-x-and-y-vector dx dy))
         curve-post-position-middle (mapv + (curve-post-position-middle curve-corner-translation-vector) (get-curve-post-outer-x-and-y-vector dx dy))
         curve-post-position-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector) (get-curve-post-outer-x-and-y-vector dx dy))
         circular-z-factor (/ (nth web-post-position-top 2) circle-bezier-approx-a)
         circular-point-1 (transform (mapv + (wall-locate1-circular dx dy) curve-post-position-top))
         circular-point-2 (make-point-z-value-not-below-zero (transform (mapv + (wall-locate3-for-polyhedron-point-circular dx dy xy) curve-post-position-middle)))
         circular-point-3 (make-point-z-value-not-below-zero (transform (mapv + (wall-locate3-for-polyhedron-point-circular-2 dx dy xy) curve-post-position-middle)))
         circular-point-floor (assoc (vec (make-point-z-value-not-below-zero (transform (mapv + (wall-locate3-for-polyhedron-point-circular-2 dx dy xy) curve-post-position-bottom)))) 2 0)
         wall-locate1-point (transform (mapv + (wall-locate1 dx dy) curve-post-position-top))
         wall-locate-1-to-3-curve-for-polyhedron-control-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx dy) curve-post-position-middle (get-oled-post-outer-x-and-y-vector dx dy))))
         wall-locate-1-to-3-curve-for-polyhedron-second-control-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx dy) curve-post-position-middle (get-oled-post-outer-x-and-y-vector dx dy))))
         wall-locate3-point (make-point-z-value-not-below-zero (transform (mapv + (wall-locate3-for-polyhedron-point dx dy xy) curve-post-position-bottom (get-oled-post-outer-x-and-y-vector dx dy))))
         wall-locate3-point-floor (assoc (vec wall-locate3-point) 2 0)
         wall-locate-2-top (make-point-z-value-not-below-zero (transform (wall-locate2-for-polyhedron-point dx dy (mapv + oled-post-position-top (get-oled-post-inner-x-and-y-vector dx dy)) xy)))
         wall-locate-2-bottom (make-point-z-value-not-below-zero (transform (wall-locate2-for-polyhedron-point dx dy (mapv + oled-post-position-bottom (get-oled-post-inner-x-and-y-vector dx dy)) xy)))
         wall-locate-2-bottom-floor (assoc (vec wall-locate-2-bottom) 2 0)]

     {:web-post-position-top web-post-position-top 
      :circular-point-1 circular-point-1
      :circular-point-2 circular-point-2 
      :circular-point-3 circular-point-3
      :circular-point-floor circular-point-floor 
      :web-post-position-bottom web-post-position-bottom
      :wall-locate-2-top wall-locate-2-top
      :wall-locate-2-bottom wall-locate-2-bottom
      :wall-locate-2-bottom-floor wall-locate-2-bottom-floor})))

(defn wall-brace-polyhedron-circular-outer-floor-point ([place dx dy post-position rad-or-deg] (wall-brace-polyhedron-circular-outer-floor-point place dx dy post-position rad-or-deg wall-xy-offset))
  ([place dx dy post-position rad-or-deg xy]
   (let [transform (get-transform-fn rad-or-deg place)
         curve-corner-translation-vector (get-curve-corner-translation-vector post-position)
         curve-post-position-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector) (get-curve-post-outer-x-and-y-vector dx dy))
          circular-point-3 (make-point-z-value-not-below-zero (transform (mapv + (wall-locate3-for-polyhedron-point-circular-2 dx dy xy) curve-post-position-bottom)))
circular-point-floor (assoc (vec circular-point-3) 2 0)]
     circular-point-floor)))

(defn wall-brace-polyhedron-circular-curve-points ([place dx dy post-position rad-or-deg steps] (wall-brace-polyhedron-circular-curve-points place dx dy post-position rad-or-deg wall-xy-offset steps))
  ([place dx dy post-position rad-or-deg xy steps]
   (let [wall-brace-polyhedron-points-map (wall-brace-polyhedron-circular-points place dx dy post-position rad-or-deg xy)
         outer-points (bezier-quartic
                       (wall-brace-polyhedron-points-map :web-post-position-top) 
                       (wall-brace-polyhedron-points-map :circular-point-1)
                       (wall-brace-polyhedron-points-map :circular-point-2)
                       (wall-brace-polyhedron-points-map :circular-point-3)
                       (wall-brace-polyhedron-points-map :circular-point-floor) 
                       steps)
         inner-points (bezier-cubic
                       (wall-brace-polyhedron-points-map :web-post-position-bottom)
                       (wall-brace-polyhedron-points-map :wall-locate-2-top)
                       (wall-brace-polyhedron-points-map :wall-locate-2-bottom)
                       (wall-brace-polyhedron-points-map :wall-locate-2-bottom-floor)
                       steps)]
     {:outer-points outer-points
      :inner-points inner-points})))

(defn wall-brace-polyhedron-circular-curves ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
                                    (wall-brace-polyhedron-circular-curves place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps]
   (let [wall-brace-polyhedron-circular-points1 (wall-brace-polyhedron-circular-points place1 dx1 dy1 post-position-1 rad-or-deg1 xy1)
         wall-brace-polyhedron-circular-points2 (wall-brace-polyhedron-circular-points place2 dx2 dy2 post-position-2 rad-or-deg2 xy2)

         web-post-top-curve (bezier-linear  (wall-brace-polyhedron-circular-points1 :web-post-position-top)  (wall-brace-polyhedron-circular-points2 :web-post-position-top) steps)
         circular-point-1-curve (bezier-linear  (wall-brace-polyhedron-circular-points1 :circular-point-1)  (wall-brace-polyhedron-circular-points2 :circular-point-1)  steps)
         circular-point-2-curve (bezier-linear  (wall-brace-polyhedron-circular-points1 :circular-point-2)  (wall-brace-polyhedron-circular-points2 :circular-point-2)  steps)
         circular-point-3-curve (bezier-linear  (wall-brace-polyhedron-circular-points1 :circular-point-3)  (wall-brace-polyhedron-circular-points2 :circular-point-3)  steps)
         circular-point-floor (bezier-linear  (wall-brace-polyhedron-circular-points1 :circular-point-floor)   (wall-brace-polyhedron-circular-points2 :circular-point-floor) steps) 
         wall-locate2-bottom-floor-curve (bezier-linear  (wall-brace-polyhedron-circular-points2 :wall-locate-2-bottom-floor)  (wall-brace-polyhedron-circular-points1 :wall-locate-2-bottom-floor) steps)
         wall-locate2-bottom-curve (bezier-linear  (wall-brace-polyhedron-circular-points2 :wall-locate-2-bottom)   (wall-brace-polyhedron-circular-points1 :wall-locate-2-bottom) steps)
         wall-locate2-top-curve (bezier-linear   (wall-brace-polyhedron-circular-points2 :wall-locate-2-top)  (wall-brace-polyhedron-circular-points1 :wall-locate-2-top)   steps)
         web-post-bottom-curve (bezier-linear   (wall-brace-polyhedron-circular-points2 :web-post-position-bottom)  (wall-brace-polyhedron-circular-points1 :web-post-position-bottom) steps)]

     {:web-post-top-curve web-post-top-curve
      :circular-point-1-curve circular-point-1-curve
      :circular-point-2-curve circular-point-2-curve
      :circular-point-3-curve circular-point-3-curve
      :circular-point-floor circular-point-floor
      :wall-locate2-bottom-floor-curve wall-locate2-bottom-floor-curve
      :wall-locate2-bottom-curve wall-locate2-bottom-curve
      :wall-locate2-top-curve wall-locate2-top-curve
      :web-post-bottom-curve web-post-bottom-curve})))

(defn wall-brace-polyhedron-circular-outer-floor-linear  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
                                                 (wall-brace-polyhedron-circular-outer-floor-linear place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps]
   (let [wall-brace-polyhedron-circular-outer-floor-point1 (wall-brace-polyhedron-circular-outer-floor-point place1 dx1 dy1 post-position-1 rad-or-deg1 xy1)
         wall-brace-polyhedron-circular-outer-floor-point2 (wall-brace-polyhedron-circular-outer-floor-point place2 dx2 dy2 post-position-2 rad-or-deg2 xy2)]
     (bezier-linear wall-brace-polyhedron-circular-outer-floor-point1 wall-brace-polyhedron-circular-outer-floor-point2 steps))))

(defn wall-brace-polyhedron-circular
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
   (wall-brace-polyhedron-circular place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps]
   (let [wall-brace-polyhedron-circular-map (wall-brace-polyhedron-circular-curves place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 steps)




         outer-points (into [] (apply concat
                                      (for [index (range 0 (inc steps))]
                                        (bezier-quartic
                                         (nth (wall-brace-polyhedron-circular-map :web-post-top-curve) index)
                                         (nth (wall-brace-polyhedron-circular-map :circular-point-1-curve) index)
                                         (nth (wall-brace-polyhedron-circular-map :circular-point-2-curve) index)
                                         (nth (wall-brace-polyhedron-circular-map :circular-point-3-curve) index)
                                         (nth (wall-brace-polyhedron-circular-map :circular-point-floor) index) 
                                         steps))))

         inner-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                               (bezier-cubic
                                                (nth (wall-brace-polyhedron-circular-map :web-post-bottom-curve) index)
                                                (nth (wall-brace-polyhedron-circular-map :wall-locate2-top-curve) index)
                                                (nth (wall-brace-polyhedron-circular-map :wall-locate2-bottom-curve) index)
                                                (nth (wall-brace-polyhedron-circular-map :wall-locate2-bottom-floor-curve) index)
                                                steps))))
         smoother-wall-polyhedron (polyhedron (concat outer-points inner-points)
                                              (generate-bezier-along-bezier-polyhedron-faces
                                               outer-points inner-points
                                               steps))]
         ;wall-polyhedron 
     smoother-wall-polyhedron)))

(defn wall-brace-quadratic-polyhedron-circular-curves ([place1 dx1 dy1  post-position-1 rad-or-deg1
                                        place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                        place2 dx2 dy2  post-position-2 rad-or-deg2 steps]
                                       (wall-brace-quadratic-polyhedron-circular-curves
                                        place1 dx1 dy1  post-position-1 rad-or-deg1
                                        place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                        place2 dx2 dy2  post-position-2 rad-or-deg2
                                        wall-xy-offset wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1  post-position-1 rad-or-deg1
    place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
    place2 dx2 dy2  post-position-2 rad-or-deg2
    xy1 xy-mid1 xy2 steps]
   (let [wall-brace-polyhedron-circular-curve-points1 (wall-brace-polyhedron-circular-points place1 dx1 dy1  post-position-1 rad-or-deg1 xy1)
         wall-brace-polyhedron-circular-curve-points-mid1 (wall-brace-polyhedron-circular-points place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1 xy-mid1 )
         wall-brace-polyhedron-circular-curve-points2 (wall-brace-polyhedron-circular-points place2 dx2 dy2  post-position-2 rad-or-deg2 xy2)
     
     web-post-top-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points1 :web-post-position-top)
                                              (wall-brace-polyhedron-circular-curve-points-mid1 :web-post-position-top)
                                              (wall-brace-polyhedron-circular-curve-points2 :web-post-position-top)
                                              steps)


circular-point-1-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points1 :circular-point-1)
                                              (wall-brace-polyhedron-circular-curve-points-mid1 :circular-point-1)
                                              (wall-brace-polyhedron-circular-curve-points2 :circular-point-1)
                                              steps)
circular-point-2-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points1 :circular-point-2)
                                    (wall-brace-polyhedron-circular-curve-points-mid1 :circular-point-2)
                                    (wall-brace-polyhedron-circular-curve-points2 :circular-point-2)
                                    steps)
circular-point-3-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points1 :circular-point-3)
                                    (wall-brace-polyhedron-circular-curve-points-mid1 :circular-point-3)
                                    (wall-brace-polyhedron-circular-curve-points2 :circular-point-3)
                                    steps)
circular-point-floor-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points1 :circular-point-floor)
                                        (wall-brace-polyhedron-circular-curve-points-mid1 :circular-point-floor)
                                        (wall-brace-polyhedron-circular-curve-points2 :circular-point-floor)
                                        steps)

wall-locate-2-bottom-floor-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points2 :wall-locate-2-bottom-floor)
                                              (wall-brace-polyhedron-circular-curve-points-mid1 :wall-locate-2-bottom-floor)
                                              (wall-brace-polyhedron-circular-curve-points1 :wall-locate-2-bottom-floor)
                                              steps)
     wall-locate-2-bottom-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points2 :wall-locate-2-bottom)
                                             (wall-brace-polyhedron-circular-curve-points-mid1 :wall-locate-2-bottom)
                                             (wall-brace-polyhedron-circular-curve-points1 :wall-locate-2-bottom)
                                             steps)
wall-locate-2-top-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points2 :wall-locate-2-top)
                                     (wall-brace-polyhedron-circular-curve-points-mid1 :wall-locate-2-top)
                                     (wall-brace-polyhedron-circular-curve-points1 :wall-locate-2-top)
                                     steps)

web-post-position-bottom-curve (bezier-quadratic (wall-brace-polyhedron-circular-curve-points2 :web-post-position-bottom)
                                            (wall-brace-polyhedron-circular-curve-points-mid1 :web-post-position-bottom)
                                            (wall-brace-polyhedron-circular-curve-points1 :web-post-position-bottom)
                                            steps)
]
     {:web-post-top-curve web-post-top-curve
      :circular-point-1-curve circular-point-1-curve
      :circular-point-2-curve circular-point-2-curve
      :circular-point-3-curve circular-point-3-curve
      :circular-point-floor-curve circular-point-floor-curve
      :wall-locate-2-bottom-floor-curve wall-locate-2-bottom-floor-curve
      :wall-locate-2-bottom-curve wall-locate-2-bottom-curve
      :wall-locate-2-top-curve wall-locate-2-top-curve
      :web-post-position-bottom-curve web-post-position-bottom-curve}
     )
   ))
(defn wall-brace-quadratic-polyhedron-circular-floor-outer ([place1 dx1 dy1  post-position-1 rad-or-deg1
                                                             place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                                             place2 dx2 dy2  post-position-2 rad-or-deg2 steps]
                                                            (wall-brace-quadratic-polyhedron-circular-floor-outer
                                                             place1 dx1 dy1  post-position-1 rad-or-deg1
                                                             place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                                             place2 dx2 dy2  post-position-2 rad-or-deg2
                                                             wall-xy-offset wall-xy-offset wall-xy-offset steps) 
                                                            )
  ([place1 dx1 dy1  post-position-1 rad-or-deg1
   place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
   place2 dx2 dy2  post-position-2 rad-or-deg2
   xy1 xy-mid1 xy2 steps]
   (let [wall-brace-polyhedron-circular-curve-points1 (wall-brace-polyhedron-circular-points place1 dx1 dy1  post-position-1 rad-or-deg1 xy1)
         wall-brace-polyhedron-circular-curve-points-mid1 (wall-brace-polyhedron-circular-points place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1 xy-mid1)
         wall-brace-polyhedron-circular-curve-points2 (wall-brace-polyhedron-circular-points place2 dx2 dy2  post-position-2 rad-or-deg2 xy2)]
     (bezier-quadratic (wall-brace-polyhedron-circular-curve-points1 :circular-point-floor)
                      ( wall-brace-polyhedron-circular-curve-points-mid1 :circular-point-floor) 
                       (wall-brace-polyhedron-circular-curve-points2 :circular-point-floor) steps)
     
     
     )
   )
)

(defn wall-brace-quadratic-polyhedron-circular ([place1 dx1 dy1  post-position-1 rad-or-deg1
                                                 place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                                 place2 dx2 dy2  post-position-2 rad-or-deg2 steps]
                                                (wall-brace-quadratic-polyhedron-circular
                                                 place1 dx1 dy1  post-position-1 rad-or-deg1
                                                 place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                                 place2 dx2 dy2  post-position-2 rad-or-deg2
                                                 wall-xy-offset wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1  post-position-1 rad-or-deg1
    place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
    place2 dx2 dy2  post-position-2 rad-or-deg2
    xy1 xy-mid1 xy2 steps]
   (let [wall-brace-quadratic-polyhedron-circular-curves-map
         (wall-brace-quadratic-polyhedron-circular-curves place1 dx1 dy1  post-position-1 rad-or-deg1
    place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
    place2 dx2 dy2  post-position-2 rad-or-deg2
    xy1 xy-mid1 xy2 steps)
         
         outer-points (into [] (apply concat
                                      (for [index (range 0 (inc steps))]
                                        (bezier-quartic
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :web-post-top-curve ) index)
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :circular-point-1-curve) index)
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :circular-point-2-curve) index)
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :circular-point-3-curve) index)
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :circular-point-floor-curve) index)
                                     steps
                                        ))))
         inner-points (into [] (apply concat 
                                      (for [index (range 0 (inc steps))]
                                        (bezier-cubic
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :web-post-position-bottom-curve) index)
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :wall-locate-2-top-curve) index)
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :wall-locate-2-bottom-curve) index)
                                         (nth (wall-brace-quadratic-polyhedron-circular-curves-map :wall-locate-2-bottom-floor-curve) index) 
                                         steps
                                         ) 
                                        )))
         smoother-wall-polyhedron (polyhedron (concat outer-points inner-points)
                                              (generate-bezier-along-bezier-polyhedron-faces
                                               outer-points inner-points
                                               steps))
   ]
   smoother-wall-polyhedron
   )
                            
  ))

(defn wall-brace-catmull-rom-spline-horizontal-curves
  [outer-control-data-1 point-data-1 point-data-2 outer-control-data-2 steps & {:keys [alpha-type t1 t2 web-post-top-style] :or {alpha-type :centripetal t1 (/ 1 3) t2 (/ 2 3) web-post-top-style :linear}}]
  
  (let [data-to-wall-brace-polyhedron-points #(wall-brace-polyhedron-points (% :place) (% :dx) (% :dy) (% :post-position) (% :rad-or-deg) :xy (% :xy))
        outer-control-points-1 (data-to-wall-brace-polyhedron-points outer-control-data-1)
        points-1 (data-to-wall-brace-polyhedron-points point-data-1)
        points-2 (data-to-wall-brace-polyhedron-points point-data-2)
        outer-control-points-2 (data-to-wall-brace-polyhedron-points outer-control-data-2)
        web-post-top-curve (case web-post-top-style 
                             :linear (bezier-linear (points-1 :web-post-position-top) (points-2 :web-post-position-top) steps)
                             :curved (catmull-rom-spline-segment (outer-control-points-1 :web-post-position-top) (points-1 :web-post-position-top) (points-2 :web-post-position-top) (outer-control-points-2 :web-post-position-top) steps :alphaType alpha-type    )) 
        wall-locate1-curve (catmull-rom-spline-segment (outer-control-points-1 :wall-locate1-point) (points-1 :wall-locate1-point) (points-2 :wall-locate1-point) (outer-control-points-2 :wall-locate1-point) steps :alphaType alpha-type    )
       point-on-tangent-from-plate-curve (catmull-rom-spline-segment (outer-control-points-1 :point-on-tangent-from-plate) (points-1 :point-on-tangent-from-plate) (points-2 :point-on-tangent-from-plate) (outer-control-points-2 :point-on-tangent-from-plate) steps :alphaType alpha-type)
        wall-locate-1-to-3-curve-for-polyhedron-control-curve (catmull-rom-spline-segment (outer-control-points-1 :wall-locate-1-to-3-curve-for-polyhedron-control-point) (points-1 :wall-locate-1-to-3-curve-for-polyhedron-control-point) (points-2 :wall-locate-1-to-3-curve-for-polyhedron-control-point) (outer-control-points-2 :wall-locate-1-to-3-curve-for-polyhedron-control-point) steps :alphaType alpha-type    )
        wall-locate-1-to-3-curve-for-polyhedron-second-control-curve (catmull-rom-spline-segment (outer-control-points-1 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (points-1 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (points-2 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (outer-control-points-2 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) steps :alphaType alpha-type    )
        wall-locate3-curve  (catmull-rom-spline-segment (outer-control-points-1 :wall-locate3-point) (points-1 :wall-locate3-point) (points-2 :wall-locate3-point) (outer-control-points-2 :wall-locate3-point) steps :alphaType alpha-type    )
        wall-locate3-floor-curve (catmull-rom-spline-segment (outer-control-points-1 :wall-locate3-point-floor) (points-1 :wall-locate3-point-floor) (points-2 :wall-locate3-point-floor) (outer-control-points-2 :wall-locate3-point-floor) steps :alphaType alpha-type    ) 
        wall-locate2-bottom-curve (catmull-rom-spline-segment (outer-control-points-1 :wall-locate-2-bottom) (points-1 :wall-locate-2-bottom) (points-2 :wall-locate-2-bottom) (outer-control-points-2 :wall-locate-2-bottom) steps :alphaType alpha-type    )
        wall-locate2-bottom-floor-curve ;(map translate-to-floor wall-locate2-bottom-curve) 
        (catmull-rom-spline-segment (outer-control-points-1 :wall-locate-2-bottom-floor) (points-1 :wall-locate-2-bottom-floor) (points-2 :wall-locate-2-bottom-floor) (outer-control-points-2 :wall-locate-2-bottom-floor) steps :alphaType alpha-type    )
        wall-locate2-top-curve (catmull-rom-spline-segment (outer-control-points-1 :wall-locate-2-top) (points-1 :wall-locate-2-top) (points-2 :wall-locate-2-top) (outer-control-points-2 :wall-locate-2-top) steps :alphaType alpha-type    )
        web-post-bottom-curve (bezier-linear  (points-2 :web-post-position-bottom) (points-1 :web-post-position-bottom) steps)

        ]
        {:web-post-top-curve web-post-top-curve
         :point-on-tangent-from-plate-curve point-on-tangent-from-plate-curve
         :wall-locate1-curve wall-locate1-curve
         :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve wall-locate-1-to-3-curve-for-polyhedron-second-control-curve
         :wall-locate-1-to-3-curve-for-polyhedron-control-curve wall-locate-1-to-3-curve-for-polyhedron-control-curve
         :wall-locate3-curve wall-locate3-curve
         :wall-locate3-floor-curve wall-locate3-floor-curve
         :wall-locate2-bottom-floor-curve (reverse wall-locate2-bottom-floor-curve)
         :wall-locate2-bottom-curve (reverse wall-locate2-bottom-curve)
         :wall-locate2-top-curve (reverse wall-locate2-top-curve)
         :web-post-bottom-curve web-post-bottom-curve}
    )
  )


(defn wall-brace-catmull-rom-spline-points
  [outer-control-data-1 point-data-1 point-data-2 outer-control-data-2 steps & {:keys [alpha-type t1 t2 web-post-top-style] :or {alpha-type :centripetal t1 (/ 1 3) t2 (/ 2 3) web-post-top-style :linear}}]

  (let [wall-brace-catmull-rom-spline-horizontal-curves-map (wall-brace-catmull-rom-spline-horizontal-curves outer-control-data-1 point-data-1 point-data-2 outer-control-data-2 steps :alpha-type alpha-type :t1 t1 :t2 t2 :web-post-top-style web-post-top-style)
        outer-points (into [] (apply concat
                                     (for [index (range 0 (inc steps))]
                                       (nurbs-with-calculated-knot-vector
                                        [(nth (wall-brace-catmull-rom-spline-horizontal-curves-map :web-post-top-curve) index) 
                                        (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :point-on-tangent-from-plate-curve) index)
                                        ;(nth (wall-brace-catmull-rom-spline-horizontal-curves-map :wall-locate1-curve) index)
                                        ;(nth (wall-brace-catmull-rom-spline-horizontal-curves-map :wall-locate-1-to-3-curve-for-polyhedron-control-curve) index)
                                        (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve) index)
                                        (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :wall-locate3-curve) index)
                                        (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :wall-locate3-floor-curve) index)]
                                        3
                                        [1 0.9  0.6 0.75 1]
                                        steps))))

        inner-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                              (bezier-cubic
                                               (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :web-post-bottom-curve) index)
                                               (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :wall-locate2-top-curve) index)
                                               (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :wall-locate2-bottom-curve) index)
                                               (nth (wall-brace-catmull-rom-spline-horizontal-curves-map :wall-locate2-bottom-floor-curve) index)
                                               steps))))]
    {:outer-points outer-points
     :inner-points inner-points}                                           
    )
  )
(defn wall-brace-catmull-rom-spline 
  [outer-control-data-1 point-data-1 point-data-2 outer-control-data-2 steps & {:keys [alpha-type t1 t2 web-post-top-style extra-points-for-sides-and-top-and-bottom] :or {alpha-type :centripetal t1 (/ 1 3) t2 (/ 2 3) web-post-top-style :linear extra-points-for-sides-and-top-and-bottom false} }]
  (let [points (wall-brace-catmull-rom-spline-points outer-control-data-1 point-data-1 point-data-2 outer-control-data-2 steps :alpha-type alpha-type :t1 t1 :t2 t2 :web-post-top-style web-post-top-style)]
    (if extra-points-for-sides-and-top-and-bottom (generate-bezier-along-bezier-polyhedron-all-sides (points :outer-points) (points :inner-points) steps)
      (polyhedron (concat (points :outer-points) (points :inner-points))
              (generate-bezier-along-bezier-polyhedron-faces
               (points :outer-points) (points :inner-points)
               steps)))) 
  )

(defn wall-brace-catmull-rom-spline-floor 
  [outer-control-data-1 point-data-1 point-data-2 outer-control-data-2 steps & {:keys [alpha-type t1 t2 web-post-top-style] :or {alpha-type :centripetal t1 (/ 1 3) t2 (/ 2 3) web-post-top-style :linear}}]
  (let [points (wall-brace-catmull-rom-spline-horizontal-curves outer-control-data-1 point-data-1 point-data-2 outer-control-data-2 steps :alpha-type alpha-type :t1 t1 :t2 t2 :web-post-top-style web-post-top-style) 
        ]
    (points :wall-locate3-floor-curve)
    )
  )

(defn wall-brace-bezier-cubic-through-points-horizontal-curves [point-1-data point-2-data point-3-data point-4-data steps & {:keys [  t1 t2] :or { t1 (/ 1 3) t2 (/ 2 3)}}]
  (let [data-to-wall-brace-polyhedron-points #(wall-brace-polyhedron-points (% :place) (% :dx) (% :dy) (% :post-position) (% :rad-or-deg) :xy (% :xy))
        points-1 (data-to-wall-brace-polyhedron-points point-1-data)
        points-2 (data-to-wall-brace-polyhedron-points point-2-data)
        points-3 (data-to-wall-brace-polyhedron-points point-3-data)
        points-4 (data-to-wall-brace-polyhedron-points point-4-data)
        web-post-top-curve (bezier-cubic (points-1 :web-post-position-top) (points-2 :web-post-position-top) (points-3 :web-post-position-top) (points-4 :web-post-position-top) steps )
        ;;(case web-post-top-style
        ;;                      :linear (bezier-linear (points-1 :web-post-position-top) (points-2 :web-post-position-top) steps)
        ;;                      :curved )
        wall-locate1-curve (bezier-cubic-through-control-points (points-1 :wall-locate1-point) (points-2 :wall-locate1-point) (points-3 :wall-locate1-point) (points-4 :wall-locate1-point) steps  :t1 t1 :t2 t2)
        wall-locate-1-to-3-curve-for-polyhedron-control-curve (bezier-cubic-through-control-points (points-1 :wall-locate-1-to-3-curve-for-polyhedron-control-point) (points-2 :wall-locate-1-to-3-curve-for-polyhedron-control-point) (points-3 :wall-locate-1-to-3-curve-for-polyhedron-control-point) (points-4 :wall-locate-1-to-3-curve-for-polyhedron-control-point) steps  :t1 t1 :t2 t2)
        wall-locate-1-to-3-curve-for-polyhedron-second-control-curve (bezier-cubic-through-control-points (points-1 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (points-2 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (points-3 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) (points-4 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) steps  :t1 t1 :t2 t2)
        wall-locate3-curve  (bezier-cubic-through-control-points (points-1 :wall-locate3-point) (points-2 :wall-locate3-point) (points-3 :wall-locate3-point) (points-4 :wall-locate3-point) steps  :t1 t1 :t2 t2)
        wall-locate3-floor-curve (bezier-cubic-through-control-points (points-1 :wall-locate3-point-floor) (points-2 :wall-locate3-point-floor) (points-3 :wall-locate3-point-floor) (points-4 :wall-locate3-point-floor) steps  :t1 t1 :t2 t2)
        wall-locate2-bottom-curve (bezier-cubic-through-control-points (points-1 :wall-locate-2-bottom) (points-2 :wall-locate-2-bottom) (points-3 :wall-locate-2-bottom) (points-4 :wall-locate-2-bottom) steps  :t1 t1 :t2 t2)
        wall-locate2-bottom-floor-curve ;(map translate-to-floor wall-locate2-bottom-curve) 
        (bezier-cubic-through-control-points (points-1 :wall-locate-2-bottom-floor) (points-2 :wall-locate-2-bottom-floor) (points-3 :wall-locate-2-bottom-floor) (points-4 :wall-locate-2-bottom-floor) steps  :t1 t1 :t2 t2)
        wall-locate2-top-curve (bezier-cubic-through-control-points (points-1 :wall-locate-2-top) (points-2 :wall-locate-2-top) (points-3 :wall-locate-2-top) (points-4 :wall-locate-2-top) steps  :t1 t1 :t2 t2)
        web-post-bottom-curve (bezier-cubic   (points-1 :web-post-position-bottom) (points-2 :web-post-position-bottom) (points-3 :web-post-position-bottom)  (points-4 :web-post-position-bottom) steps)]
    {:web-post-top-curve web-post-top-curve
     :wall-locate1-curve wall-locate1-curve
     :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve wall-locate-1-to-3-curve-for-polyhedron-second-control-curve
     :wall-locate-1-to-3-curve-for-polyhedron-control-curve wall-locate-1-to-3-curve-for-polyhedron-control-curve
     :wall-locate3-curve wall-locate3-curve
     :wall-locate3-floor-curve wall-locate3-floor-curve
     :wall-locate2-bottom-floor-curve (reverse wall-locate2-bottom-floor-curve)
     :wall-locate2-bottom-curve (reverse wall-locate2-bottom-curve)
     :wall-locate2-top-curve (reverse wall-locate2-top-curve)
     :web-post-bottom-curve (reverse web-post-bottom-curve)}))

(defn wall-brace-bezier-cubic-through-points-points
  [point-1-data point-2-data point-3-data point-4-data steps & {:keys [ t1 t2] :or {t1 (/ 1 3) t2 (/ 2 3)}}]

  (let [wall-brace-bezier-cubic-through-points-horizontal-curves-map (wall-brace-bezier-cubic-through-points-horizontal-curves point-1-data point-2-data point-3-data point-4-data steps :t1 t1 :t2 t2)
        outer-points (into [] (apply concat
                                     (for [index (range 0 (inc steps))]
                                       (bezier-quintic
                                        (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :web-post-top-curve) index)
                                        (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :wall-locate1-curve) index)
                                        (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :wall-locate-1-to-3-curve-for-polyhedron-control-curve) index)
                                        (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve) index)
                                        (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :wall-locate3-curve) index)
                                        (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :wall-locate3-floor-curve) index)
                                        steps))))

        inner-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                              (bezier-cubic
                                               (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :web-post-bottom-curve) index)
                                               (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :wall-locate2-top-curve) index)
                                               (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :wall-locate2-bottom-curve) index)
                                               (nth (wall-brace-bezier-cubic-through-points-horizontal-curves-map :wall-locate2-bottom-floor-curve) index)
                                               steps))))]
    {:outer-points outer-points
     :inner-points inner-points}))

(defn wall-brace-bezier-cubic-through-points-polyhedron
  [point-1-data point-2-data point-3-data point-4-data steps & {:keys [ t1 t2] :or { t1 (/ 1 3) t2 (/ 2 3)}}]
  (let [points (wall-brace-bezier-cubic-through-points-points point-1-data point-2-data point-3-data point-4-data steps  :t1 t1 :t2 t2)]
    (polyhedron (concat (points :outer-points) (points :inner-points))
                (generate-bezier-along-bezier-polyhedron-faces
                 (points :outer-points) (points :inner-points)
                 steps))))

(defn wall-brace-bezier-cubic-through-points-floor
  [point-1-data point-2-data point-3-data point-4-data steps & {:keys [t1 t2] :or {t1 (/ 1 3) t2 (/ 2 3)}}]
  (let [points (wall-brace-bezier-cubic-through-points-horizontal-curves point-1-data point-2-data point-3-data point-4-data steps :t1 t1 :t2 t2)]
    (points :wall-locate3-floor-curve)))

(defn wall-brace-with-circular ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
   (wall-brace-with-circular place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset :circular :circular steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 curve-type1 curve-type2 steps]
   (let [curve1-fn (case curve-type1
                     :circular wall-brace-polyhedron-circular-curve-points
                     :standard wall-brace-polyhedron-curve-points)
         curve2-fn (case curve-type2
                     :circular wall-brace-polyhedron-circular-curve-points
                     :standard wall-brace-polyhedron-curve-points)
         curve-points1 (curve1-fn place1 dx1 dy1 post-position-1 rad-or-deg1 xy1 steps)
         curve-points2 (curve2-fn place2 dx2 dy2 post-position-2 rad-or-deg2 xy2 steps)]
     (generate-bezier-along-bezier-polyhedron-from-points-list-linear
      (curve-points2 :outer-points) (curve-points1 :outer-points)
     (reverse (curve-points2 :inner-points)) (reverse (curve-points1 :inner-points))
     steps
      )
     ) 
   )
  )

(defn wall-brace-with-circular-outer-floor-linear ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 steps]
                                (wall-brace-with-circular-outer-floor-linear place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 wall-xy-offset wall-xy-offset :circular :circular steps))
  ([place1 dx1 dy1 post-position-1 rad-or-deg1 place2 dx2 dy2 post-position-2 rad-or-deg2 xy1 xy2 curve-type1 curve-type2 steps]
   (let [curve1-fn (case curve-type1
                     :circular wall-brace-polyhedron-circular-points
                     :standard wall-brace-polyhedron-points)
         curve2-fn (case curve-type2
                     :circular wall-brace-polyhedron-circular-points
                     :standard wall-brace-polyhedron-points)
         floor-point-key1 (case curve-type1
                           :circular :circular-point-floor
                            :standard :wall-locate3-point-floor)
         floor-point-key2 (case curve-type2
                            :circular :circular-point-floor
                            :standard :wall-locate3-point-floor)                   
         curve-points1 (curve1-fn place1 dx1 dy1 post-position-1 rad-or-deg1 xy1 )
         curve-points2 (curve2-fn place2 dx2 dy2 post-position-2 rad-or-deg2 xy2 )] 
     (bezier-linear (curve-points1 floor-point-key1) (curve-points2 floor-point-key2) steps))))

(defn key-wall-brace-polyhedron [x1 y1 dx1 dy1 post-position1 x2 y2 dx2 dy2 post-position2 & 
                                 {:keys [steps xy1 xy2] :or {steps 60 xy1 wall-xy-offset xy2 wall-xy-offset}}] 

   (wall-brace-polyhedron (partial key-place x1 y1) dx1 dy1 post-position1 :radians
                         (partial key-place x2 y2) dx2 dy2 post-position2 :radians
                          xy1 xy2 steps))

(defn key-wall-brace-polyhedron-cicular [x1 y1 dx1 dy1 post-position1 x2 y2 dx2 dy2 post-position2 &
                                 {:keys [steps xy1 xy2] :or {steps 60 xy1 wall-xy-offset xy2 wall-xy-offset}}]

  (wall-brace-polyhedron-circular (partial key-place x1 y1) dx1 dy1 post-position1 :radians
                         (partial key-place x2 y2) dx2 dy2 post-position2 :radians
                         xy1 xy2 steps))

(defn key-wall-brace-polyhedron-with-circular [x1 y1 dx1 dy1 post-position1 x2 y2 dx2 dy2 post-position2 &
                                 {:keys [steps xy1 xy2 curve-type1 curve-type2] :or {steps 60 xy1 wall-xy-offset xy2 wall-xy-offset curve-type1 :circular curve-type2 :circular}}]

  (wall-brace-with-circular (partial key-place x1 y1) dx1 dy1 post-position1 :radians
                         (partial key-place x2 y2) dx2 dy2 post-position2 :radians
                         xy1 xy2 curve-type1 curve-type2 steps))

(defn key-wall-brace-polyhedron-outer-floor-linear [x1 y1 dx1 dy1 post-position1 x2 y2 dx2 dy2 post-position2 &
                                 {:keys [steps xy1 xy2] :or {steps 60 xy1 wall-xy-offset xy2 wall-xy-offset}}]

  (wall-brace-polyhedron-outer-floor-linear (partial key-place x1 y1) dx1 dy1 post-position1 :radians
                         (partial key-place x2 y2) dx2 dy2 post-position2 :radians
                         xy1 xy2 steps))

(defn key-wall-brace-polyhedron-with-circular-outer-floor-linear [x1 y1 dx1 dy1 post-position1 x2 y2 dx2 dy2 post-position2 &
                                               {:keys [steps xy1 xy2 curve-type1 curve-type2] :or {steps 60 xy1 wall-xy-offset xy2 wall-xy-offset curve-type1 :circular curve-type2 :circular}}]

  (wall-brace-with-circular-outer-floor-linear (partial key-place x1 y1) dx1 dy1 post-position1 :radians
                            (partial key-place x2 y2) dx2 dy2 post-position2 :radians
                            xy1 xy2 curve-type1 curve-type2 steps))

(defn key-wall-brace-polyhedron-circular-outer-floor-linear [x1 y1 dx1 dy1 post-position1 x2 y2 dx2 dy2 post-position2 &
                                                    {:keys [steps xy1 xy2] :or {steps 60 xy1 wall-xy-offset xy2 wall-xy-offset}}]

  (wall-brace-polyhedron-circular-outer-floor-linear (partial key-place x1 y1) dx1 dy1 post-position1 :radians
                                            (partial key-place x2 y2) dx2 dy2 post-position2 :radians
                                            xy1 xy2 steps))


(defn thumb-wall-brace-polyhedron 
  [place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2 & {:keys [xy1 xy2 steps] :or {xy1 wall-xy-offset xy2 wall-xy-offset steps 60}}] 
  
   (wall-brace-polyhedron place1 dx1 dy1 post-position1 :degrees
                          place2 dx2 dy2 post-position2 :degrees
                          xy1 xy2
                          steps))

(defn thumb-wall-brace-polyhedron-circular
  [place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2 & {:keys [xy1 xy2 steps] :or {xy1 wall-xy-offset xy2 wall-xy-offset steps 60}}]

  (wall-brace-polyhedron-circular place1 dx1 dy1 post-position1 :degrees
                         place2 dx2 dy2 post-position2 :degrees
                         xy1 xy2
                         steps))

(defn thumb-wall-brace-polyhedron-outer-floor-linear
  [place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2 & {:keys [xy1 xy2 steps] :or {xy1 wall-xy-offset xy2 wall-xy-offset steps 60}}]

  (wall-brace-polyhedron-outer-floor-linear place1 dx1 dy1 post-position1 :degrees
                         place2 dx2 dy2 post-position2 :degrees
                         xy1 xy2
                         steps))

(defn thumb-wall-brace-polyhedron-circular-outer-floor-linear
  [place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2 & {:keys [xy1 xy2 steps] :or {xy1 wall-xy-offset xy2 wall-xy-offset steps 60}}]

  (wall-brace-polyhedron-circular-outer-floor-linear place1 dx1 dy1 post-position1 :degrees
                                            place2 dx2 dy2 post-position2 :degrees
                                            xy1 xy2
                                            steps))

(defn thumb-wall-brace-polyhedron-with-circular
  [place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2 & 
   {:keys [steps xy1 xy2 curve-type1 curve-type2] :or {steps 60 xy1 wall-xy-offset xy2 wall-xy-offset curve-type1 :circular curve-type2 :circular}}]

  (wall-brace-with-circular place1 dx1 dy1 post-position1 :degrees
                         place2 dx2 dy2 post-position2 :degrees
                         xy1 xy2
                            curve-type1 curve-type2
                         steps))

(defn thumb-wall-brace-polyhedron-with-circular-outer-floor-linear
  [place1 dx1 dy1 post-position1 place2 dx2 dy2 post-position2 &
   {:keys [steps xy1 xy2 curve-type1 curve-type2] :or {steps 60 xy1 wall-xy-offset xy2 wall-xy-offset curve-type1 :circular curve-type2 :circular}}]

  (wall-brace-with-circular-outer-floor-linear place1 dx1 dy1 post-position1 :degrees
                            place2 dx2 dy2 post-position2 :degrees
                            xy1 xy2
                            curve-type1 curve-type2
                            steps))

(defn web-post-point-top [place post-position rad-or-deg &{:keys [offset] :or {offset [0 0 0]}}]
  (let [transform (get-transform-fn rad-or-deg place)
        web-corner-translation-vector (get-single-plate-corner-position-vector post-position)
web-post-point-top-coordinates (transform (mapv + (get-web-post-position-top web-corner-translation-vector) offset))
        ] 
    web-post-point-top-coordinates
    )
  )

(defn web-post-point-bottom [place post-position rad-or-deg & {:keys [offset] :or {offset [0 0 0]}}]
  (let [transform (get-transform-fn rad-or-deg place)
        web-corner-translation-vector (get-single-plate-corner-position-vector post-position) 
        web-post-position-1-bottom (transform (mapv + (get-web-post-position-bottom (mapv +  web-corner-translation-vector)) offset))]
    web-post-position-1-bottom
    )
  )

(defn main-body-web-post-point-top [col row post-position & {:keys [offset] :or {offset [0 0 0]}}]
  (web-post-point-top (partial key-place col row) post-position :radians :offset offset)
  )

(defn main-body-web-post-point-bottom [col row post-position & {:keys [offset] :or {offset [0 0 0]}}]
  (web-post-point-bottom (partial key-place col row) post-position :radians :offset offset))

(defn thumb-web-post-point-top [thumb-place post-position & {:keys [offset] :or {offset [0 0 0]}}]
  (web-post-point-top thumb-place post-position :degrees :offset offset)
  )

(defn thumb-web-post-point-bottom [thumb-place post-position & {:keys [offset] :or {offset [0 0 0]}}]
  (web-post-point-bottom thumb-place post-position :degrees :offset offset))


(defn web-post-point [place post-position rad-or-deg] 
    {:top (web-post-point-top place post-position rad-or-deg) 
     :bottom (web-post-point-bottom place post-position rad-or-deg)}
    
  )

(defn web-post-linear [place1 post-position-1 rad-or-deg1
                       place2 post-position-2 rad-or-deg2 steps
                       & {:keys [offset1 offset2] :or {offset1 [0 0 0] offset2 [0 0 0]}}]
  (let [transform-1 (get-transform-fn rad-or-deg1 place1)
        transform-2 (get-transform-fn rad-or-deg2 place2)
        web-corner-translation-vector1 (get-single-plate-corner-position-vector post-position-1)
        web-corner-translation-vector2 (get-single-plate-corner-position-vector post-position-2)
        web-post-position-1-top (transform-1 (get-web-post-position-top (mapv +  web-corner-translation-vector1 offset1)))
        web-post-position-1-bottom (transform-1 (get-web-post-position-bottom (mapv +  web-corner-translation-vector1 offset1)))


        web-post-position-2-top (transform-2 (get-web-post-position-top (mapv +  web-corner-translation-vector2 offset2)))
        web-post-position-2-bottom (transform-2 (get-web-post-position-bottom (mapv +  web-corner-translation-vector2 offset2)))
        web-post-top-curve (bezier-linear  web-post-position-1-top  web-post-position-2-top  steps)
        web-post-bottom-curve (bezier-linear   web-post-position-2-bottom  web-post-position-1-bottom steps)
        ;; web-post-curve (into [] (apply concat
        ;;                                (for [index (range 0 (inc steps))]
        ;;                                  (bezier-linear
        ;;                                   (nth web-post-top-curve index)
        ;;                                   (nth web-post-bottom-curve index)
        ;;                                   steps))))
        web-post-curve {:top web-post-top-curve :bottom web-post-bottom-curve}]
    web-post-curve))

(defn web-post-quadratic-curve [place1 post-position-1 rad-or-deg1
                                place-mid1 post-position-mid1 rad-or-degmid1
                                place2 post-position-2 rad-or-deg2 steps
                                & {:keys [offset1 offset-mid1 offset2] :or {offset1 [0 0 0] offset-mid1 [0 0 0] offset2 [0 0 0]}}]
  (let [transform-1 (get-transform-fn rad-or-deg1 place1)
        transform-mid1 (get-transform-fn rad-or-degmid1 place-mid1)
        transform-2 (get-transform-fn rad-or-deg2 place2)
        web-corner-translation-vector1 (get-single-plate-corner-position-vector post-position-1)
        web-corner-translation-vectormid1 (get-single-plate-corner-position-vector post-position-mid1)
        web-corner-translation-vector2 (get-single-plate-corner-position-vector post-position-2)
        web-post-position-1-top (transform-1 (get-web-post-position-top (mapv +  web-corner-translation-vector1 offset1)))
        web-post-position-1-bottom (transform-1 (get-web-post-position-bottom (mapv +  web-corner-translation-vector1 offset1)))

        web-post-position-mid1-top (transform-mid1 (get-web-post-position-top (mapv +  web-corner-translation-vectormid1 offset-mid1)))
        web-post-position-mid1-bottom (transform-mid1 (get-web-post-position-bottom (mapv +  web-corner-translation-vectormid1 offset-mid1)))

        web-post-position-2-top (transform-2 (get-web-post-position-top (mapv +  web-corner-translation-vector2 offset2)))
        web-post-position-2-bottom (transform-2 (get-web-post-position-bottom (mapv +  web-corner-translation-vector2 offset2)))
        web-post-top-curve (bezier-quadratic  web-post-position-1-top web-post-position-mid1-top web-post-position-2-top  steps)
        web-post-bottom-curve (bezier-quadratic   web-post-position-2-bottom web-post-position-mid1-bottom web-post-position-1-bottom steps)
        ;; web-post-curve (into [](apply concat
        ;;                         (for [index (range 0 (inc steps))]
        ;;                           (bezier-linear
        ;;                            (nth web-post-top-curve index)
        ;;                            (nth web-post-bottom-curve index)
        ;;                            steps
        ;;                            )

        ;;                           )
        ;;                         )) 
        web-post-curve {:top web-post-top-curve :bottom web-post-bottom-curve}]
    web-post-curve))
(defn wall-brace-quadratic-polyhedron-bottom ([place1 dx1 dy1  post-position-1 rad-or-deg1
                                        place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                        place2 dx2 dy2  post-position-2 rad-or-deg2 steps]
                                       (wall-brace-quadratic-polyhedron-bottom
                                        place1 dx1 dy1  post-position-1 rad-or-deg1
                                        place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                        place2 dx2 dy2  post-position-2 rad-or-deg2
                                        wall-xy-offset wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1  post-position-1 rad-or-deg1
    place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
    place2 dx2 dy2  post-position-2 rad-or-deg2 xy1 xy-mid1 xy2 steps]
   (let[ point1 (wall-brace-polyhedron-outer-floor-point place1 dx1 dy1  post-position-1 rad-or-deg1 xy1)
        point-mid1 (wall-brace-polyhedron-outer-floor-point place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1 xy-mid1)
        point2 (wall-brace-polyhedron-outer-floor-point  place2 dx2 dy2  post-position-2 rad-or-deg2 xy2)
        ]
    (bezier-quadratic point1 point-mid1 point2 steps)
    )
   
   )
  )
(defn wall-brace-quadratic-polyhedron ([place1 dx1 dy1  post-position-1 rad-or-deg1
                                        place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                        place2 dx2 dy2  post-position-2 rad-or-deg2 steps]
                                       (wall-brace-quadratic-polyhedron
                                        place1 dx1 dy1  post-position-1 rad-or-deg1
                                        place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                        place2 dx2 dy2  post-position-2 rad-or-deg2
                                        wall-xy-offset wall-xy-offset wall-xy-offset steps))
  ([place1 dx1 dy1  post-position-1 rad-or-deg1
    place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
    place2 dx2 dy2  post-position-2 rad-or-deg2 xy1 xy-mid1 xy2 steps]
   (wall-brace-quadratic-polyhedron
    place1 dx1 dy1  post-position-1 rad-or-deg1
    place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
    place2 dx2 dy2  post-position-2 rad-or-deg2
     xy1 xy-mid1 xy2 false steps))
  ([place1 dx1 dy1  post-position-1 rad-or-deg1
    place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
    place2 dx2 dy2  post-position-2 rad-or-deg2 xy1 xy-mid1 xy2 extra-points-for-sides-and-top-and-bottom steps]
    (let [control-points-1 (wall-brace-polyhedron-points place1 dx1 dy1  post-position-1 rad-or-deg1 :xy xy1)
          control-points-mid1 (wall-brace-polyhedron-points place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1 xy-mid1 )
          control-points-2 (wall-brace-polyhedron-points place2 dx2 dy2  post-position-2 rad-or-deg2 :xy xy2)
          ;;transform-1 (get-transform-fn rad-or-deg1 place1)
  ;;        transform-mid1 (get-transform-fn rad-or-degmid1 place-mid1)
  ;;        transform-2 (get-transform-fn rad-or-deg2 place2)

        ;;  web-corner-translation-vector1 (get-single-plate-corner-position-vector post-position-1)
        ;;  web-corner-translation-vectormid1 (get-single-plate-corner-position-vector post-position-mid1)
        ;;  web-corner-translation-vector2 (get-single-plate-corner-position-vector post-position-2)
        ;;  web-post-position-1-top (transform-1 (get-web-post-position-top (mapv +  web-corner-translation-vector1)))
        ;;  web-post-position-1-bottom (transform-1 (get-web-post-position-bottom (mapv +  web-corner-translation-vector1)))

        ;;  web-post-position-mid1-top (transform-mid1 (get-web-post-position-top (mapv +  web-corner-translation-vectormid1)))
        ;;  web-post-position-mid1-bottom (transform-mid1 (get-web-post-position-bottom (mapv +  web-corner-translation-vectormid1)))

        ;;  web-post-position-2-top (transform-2 (get-web-post-position-top (mapv +  web-corner-translation-vector2)))
        ;;  web-post-position-2-bottom (transform-2 (get-web-post-position-bottom (mapv +  web-corner-translation-vector2)))

        ;;  oled-corner-translation-vector1 (get-oled-corner-translation-vector post-position-1)
        ;;  oled-corner-translation-vectormid1 (get-oled-corner-translation-vector post-position-mid1)
        ;;  oled-corner-translation-vector2  (get-oled-corner-translation-vector post-position-2)

        ;;  oled-post-position-1-top (oled-post-position-top oled-corner-translation-vector1)
        ;;  oled-post-position-1-bottom (oled-post-position-bottom oled-corner-translation-vector1)

        ;;  oled-post-position-mid1-top (oled-post-position-top oled-corner-translation-vectormid1)
        ;;  oled-post-position-mid1-bottom (oled-post-position-bottom oled-corner-translation-vectormid1)

        ;;  oled-post-position-2-top (oled-post-position-top oled-corner-translation-vector2)
        ;;  oled-post-position-2-bottom  (oled-post-position-bottom oled-corner-translation-vector2)

        ;;  curve-corner-translation-vector1 (get-curve-corner-translation-vector post-position-1)
        ;;  curve-corner-translation-vectormid1 (get-curve-corner-translation-vector post-position-mid1)
        ;;  curve-corner-translation-vector2 (get-curve-corner-translation-vector post-position-2)

        ;;  curve-post-position-1-top (mapv + (curve-post-position-top curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))
        ;;  curve-post-position-1-middle (mapv + (curve-post-position-middle curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))
        ;;  curve-post-position-1-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector1) (get-curve-post-outer-x-and-y-vector dx1 dy1))

        ;;  curve-post-position-mid1-top (mapv + (curve-post-position-top curve-corner-translation-vectormid1) (get-curve-post-outer-x-and-y-vector dxmid1 dymid1))
        ;;  curve-post-position-mid1-middle (mapv + (curve-post-position-middle curve-corner-translation-vectormid1) (get-curve-post-outer-x-and-y-vector dxmid1 dymid1))
        ;;  curve-post-position-mid1-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vectormid1) (get-curve-post-outer-x-and-y-vector dxmid1 dymid1))

        ;;  curve-post-position-2-top (mapv + (curve-post-position-top curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))
        ;;  curve-post-position-2-middle (mapv + (curve-post-position-middle curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))
        ;;  curve-post-position-2-bottom (mapv + (curve-post-position-bottom curve-corner-translation-vector2) (get-curve-post-outer-x-and-y-vector dx2 dy2))

        ;;  wall-locate1-point1 (transform-1 (mapv + (wall-locate1 dx1 dy1) curve-post-position-1-top))
        ;;  wall-locate1-pointmid1 (transform-mid1 (mapv + (wall-locate1 dxmid1 dymid1) curve-post-position-mid1-top))
        ;;  wall-locate1-point2 (transform-2 (mapv + (wall-locate1 dx2 dy2) curve-post-position-2-top))

        ;;  wall-locate-1-to-3-curve-for-polyhedron-control-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx1 dy1) curve-post-position-1-middle (get-oled-post-outer-x-and-y-vector dx1 dy1))))
        ;;  wall-locate-1-to-3-curve-for-polyhedron-control-pointmid1 (make-point-z-value-not-below-zero (transform-mid1 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dxmid1 dymid1) curve-post-position-mid1-middle (get-oled-post-outer-x-and-y-vector dxmid1 dymid1))))
        ;;  wall-locate-1-to-3-curve-for-polyhedron-control-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-control-point dx2 dy2) curve-post-position-2-middle (get-oled-post-outer-x-and-y-vector dx2 dy2))))

        ;;  wall-locate-1-to-3-curve-for-polyhedron-second-control-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx1 dy1) curve-post-position-1-middle (get-oled-post-outer-x-and-y-vector dx1 dy1))))
        ;;  wall-locate-1-to-3-curve-for-polyhedron-second-control-pointmid1 (make-point-z-value-not-below-zero (transform-mid1 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dxmid1 dymid1) curve-post-position-1-middle (get-oled-post-outer-x-and-y-vector dxmid1 dymid1))))
        ;;  wall-locate-1-to-3-curve-for-polyhedron-second-control-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate-1-to-3-curve-for-polyhedron-second-control-point dx2 dy2) curve-post-position-2-middle (get-oled-post-outer-x-and-y-vector dx2 dy2))))

        ;;  wall-locate3-point1 (make-point-z-value-not-below-zero (transform-1 (mapv + (wall-locate3-for-polyhedron-point dx1 dy1 xy1) curve-post-position-1-bottom (get-oled-post-outer-x-and-y-vector dx1 dy1))))
        ;;  wall-locate3-point1-floor (assoc (vec wall-locate3-point1) 2 0)
        ;;  wall-locate3-pointmid1 (make-point-z-value-not-below-zero (transform-mid1 (mapv + (wall-locate3-for-polyhedron-point dxmid1 dymid1 xy-mid1) curve-post-position-mid1-bottom (get-oled-post-outer-x-and-y-vector dxmid1 dymid1))))
        ;;  wall-locate3-pointmid1-floor (assoc (vec wall-locate3-pointmid1) 2 0)
        ;;  wall-locate3-point2 (make-point-z-value-not-below-zero (transform-2 (mapv + (wall-locate3-for-polyhedron-point dx2 dy2 xy2) curve-post-position-2-bottom (get-oled-post-outer-x-and-y-vector dx2 dy2))))
        ;;  wall-locate3-point2-floor (assoc (vec wall-locate3-point2) 2 0)


        ;;  wall-locate-2-top1 (transform-1 (wall-locate2-for-polyhedron-point dx1 dy1 (mapv + oled-post-position-1-top (get-oled-post-inner-x-and-y-vector dx1 dy1)) xy1))
        ;;  wall-locate-2-topmid1 (transform-mid1 (wall-locate2-for-polyhedron-point dxmid1 dymid1 (mapv + oled-post-position-mid1-top (get-oled-post-inner-x-and-y-vector dxmid1 dymid1)) xy-mid1))
        ;;  wall-locate-2-top2 (transform-2 (wall-locate2-for-polyhedron-point dx2 dy2 (mapv + oled-post-position-2-top (get-oled-post-inner-x-and-y-vector dx2 dy2)) xy2))

        ;;  wall-locate-2-bottom1 (make-point-z-value-not-below-zero (transform-1 (wall-locate2-for-polyhedron-point dx1 dy1 (mapv + oled-post-position-1-bottom (get-oled-post-inner-x-and-y-vector dx1 dy1)) xy1)))
        ;;  wall-locate-2-bottom1-floor (assoc (vec wall-locate-2-bottom1) 2 0)
        ;;  wall-locate-2-bottommid1 (make-point-z-value-not-below-zero (transform-mid1 (wall-locate2-for-polyhedron-point dxmid1 dymid1 (mapv + oled-post-position-mid1-bottom (get-oled-post-inner-x-and-y-vector dxmid1 dymid1)) xy-mid1)))
        ;;  wall-locate-2-bottommid1-floor (assoc (vec wall-locate-2-bottommid1) 2 0)
        ;;  wall-locate-2-bottom2 (make-point-z-value-not-below-zero (transform-2 (wall-locate2-for-polyhedron-point dx2 dy2 (mapv + oled-post-position-2-bottom (get-oled-post-inner-x-and-y-vector dx2 dy2)) xy2)))
        ;;  wall-locate-2-bottom2-floor (assoc (vec wall-locate-2-bottom2) 2 0)

          web-post-top-curve (bezier-quadratic  (:web-post-position-top control-points-1) (:web-post-position-top control-points-mid1) (:web-post-position-top control-points-2)  steps)
          ;wall-locate1-curve (bezier-quadratic  wall-locate1-point1 wall-locate1-pointmid1 wall-locate1-point2  steps)
          point-on-tangent-from-plate-curve (bezier-quadratic (:point-on-tangent-from-plate control-points-1)  (:point-on-tangent-from-plate control-points-mid1)
                                                              (:point-on-tangent-from-plate control-points-2) steps)
          wall-locate-1-to-3-curve-for-polyhedron-second-control-curve (bezier-quadratic (:wall-locate-1-to-3-curve-for-polyhedron-second-control-point control-points-1)
                                                                                         (:wall-locate-1-to-3-curve-for-polyhedron-second-control-point control-points-mid1)
                                                                                         (:wall-locate-1-to-3-curve-for-polyhedron-second-control-point control-points-2) steps)
          wall-locate-1-to-3-curve-for-polyhedron-control-curve (bezier-quadratic (:wall-locate-1-to-3-curve-for-polyhedron-control-point control-points-1)
                                                                                  (:wall-locate-1-to-3-curve-for-polyhedron-control-point control-points-mid1)
                                                                                  (:wall-locate-1-to-3-curve-for-polyhedron-control-point control-points-2) steps)
          wall-locate3-curve (bezier-quadratic  (:wall-locate3-point control-points-1) (:wall-locate3-point control-points-mid1) (:wall-locate3-point control-points-2)  steps)
          wall-locate3-floor-curve (bezier-quadratic (:wall-locate3-point-floor control-points-1) (:wall-locate3-point-floor control-points-mid1) (:wall-locate3-point-floor control-points-2)  steps)

          wall-locate2-bottom-floor-curve (bezier-quadratic (:wall-locate-2-bottom-floor control-points-2) (:wall-locate-2-bottom-floor control-points-mid1) (:wall-locate-2-bottom-floor control-points-1) steps)
          wall-locate2-bottom-curve (bezier-quadratic  (:wall-locate-2-bottom control-points-2) (:wall-locate-2-bottom control-points-mid1) (:wall-locate-2-bottom control-points-1)steps)
          wall-locate2-top-curve (bezier-quadratic   (:wall-locate-2-top control-points-2) (:wall-locate-2-top control-points-mid1) (:wall-locate-2-top control-points-1)   steps)
          web-post-bottom-curve (bezier-quadratic   (:web-post-position-bottom control-points-2) (:web-post-position-bottom control-points-mid1) (:web-post-position-bottom control-points-1) steps)

         outer-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                               (nurbs-with-calculated-knot-vector
                                                [(nth web-post-top-curve index) 
                                                 (nth point-on-tangent-from-plate-curve index)
                                                 (nth wall-locate-1-to-3-curve-for-polyhedron-second-control-curve index)
                                                ;(nth wall-locate-1-to-3-curve-for-polyhedron-control-curve index)
                                                (nth wall-locate3-curve index)
                                                (nth wall-locate3-floor-curve index)]
                                                3
                                                [1 0.9  0.6 0.75 1]
                                                steps))))

         inner-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                               (bezier-cubic
                                                (nth web-post-bottom-curve index)
                                                (nth wall-locate2-top-curve index)
                                                (nth wall-locate2-bottom-curve index)
                                                (nth wall-locate2-bottom-floor-curve index)
                                                steps))))
         wall-brace-quadratic-polyhedron (if extra-points-for-sides-and-top-and-bottom (generate-bezier-along-bezier-polyhedron-all-sides outer-points inner-points steps) 
                                           (polyhedron (concat outer-points inner-points)
                                                     (generate-bezier-along-bezier-polyhedron-faces
                                                      outer-points inner-points
                                                      steps)))]
     wall-brace-quadratic-polyhedron)))

(defn wall-brace-cubic-polyhedron-floor-outer [{:keys [place1 dx1 dy1  post-position-1 rad-or-deg1
                                                 place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                                 place-mid2 dxmid2 dymid2  post-position-mid2 rad-or-degmid2
                                                 place2 dx2 dy2  post-position-2 rad-or-deg2 xy1 xymid1 xymid2 xy2
                                                  steps] :or {steps 60 xy1 wall-xy-offset xymid1 wall-xy-offset xymid2 wall-xy-offset xy2 wall-xy-offset}}]
  (let [wall-brace-polyedron-curve-points1 (wall-brace-polyhedron-points place1 dx1 dy1 post-position-1 rad-or-deg1 :xy xy1)
        wall-brace-polyedron-curve-points-mid1 (wall-brace-polyhedron-points place-mid1 dxmid1 dymid1 post-position-mid1 rad-or-degmid1 :xy xymid1) 
        wall-brace-polyedron-curve-points-mid2 (wall-brace-polyhedron-points place-mid2 dxmid2 dymid2 post-position-mid2 rad-or-degmid2 :xy xymid2)
        wall-brace-polyedron-curve-points2 (wall-brace-polyhedron-points place2 dx2 dy2 post-position-2 rad-or-deg2 :xy xy2)]
  (bezier-cubic (wall-brace-polyedron-curve-points1 :wall-locate3-point-floor)
                (wall-brace-polyedron-curve-points-mid1 :wall-locate3-point-floor)
                (wall-brace-polyedron-curve-points-mid2 :wall-locate3-point-floor)
                (wall-brace-polyedron-curve-points2 :wall-locate3-point-floor)  steps)
)
  )

(defn wall-brace-cubic-polyhedron-curves [{:keys [place1 dx1 dy1  post-position-1 rad-or-deg1
                                                 place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
                                                 place-mid2 dxmid2 dymid2  post-position-mid2 rad-or-degmid2
                                                 place2 dx2 dy2  post-position-2 rad-or-deg2 xy1 xymid1 xymid2 xy2
                                                  steps] :or {steps 60 xy1 wall-xy-offset xymid1 wall-xy-offset xymid2 wall-xy-offset xy2 wall-xy-offset}}]
  (let [wall-brace-polyedron-curve-points1 (wall-brace-polyhedron-points place1 dx1 dy1 post-position-1 rad-or-deg1 :xy xy1)
        wall-brace-polyedron-curve-points-mid1 (wall-brace-polyhedron-points place-mid1 dxmid1 dymid1 post-position-mid1 rad-or-degmid1 :xy xymid1) 
        wall-brace-polyedron-curve-points-mid2 (wall-brace-polyhedron-points place-mid2 dxmid2 dymid2 post-position-mid2 rad-or-degmid2 :xy xymid2)
        wall-brace-polyedron-curve-points2 (wall-brace-polyhedron-points place2 dx2 dy2 post-position-2 rad-or-deg2 :xy xy2)

        web-post-top-curve (bezier-cubic  (wall-brace-polyedron-curve-points1 :web-post-position-top) 
                                           (wall-brace-polyedron-curve-points-mid1 :web-post-position-top)
                                          (wall-brace-polyedron-curve-points-mid2 :web-post-position-top)
                                           (wall-brace-polyedron-curve-points2 :web-post-position-top)
                                           steps)
        
        wall-locate1-curve (bezier-cubic  (wall-brace-polyedron-curve-points1 :wall-locate1-point)
                                            (wall-brace-polyedron-curve-points-mid1 :wall-locate1-point)
                                            (wall-brace-polyedron-curve-points-mid2 :wall-locate1-point)
                                              (wall-brace-polyedron-curve-points2 :wall-locate1-point)
                                              steps)
        
        wall-locate-1-to-3-curve-for-polyhedron-second-control-curve (bezier-cubic  (wall-brace-polyedron-curve-points1 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)
                                                                                          (wall-brace-polyedron-curve-points-mid1 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)
                                                                                          (wall-brace-polyedron-curve-points-mid2 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)
                                                                                        (wall-brace-polyedron-curve-points2 :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)
                                                                                        steps)

        wall-locate-1-to-3-curve-for-polyhedron-control-curve (bezier-cubic  (wall-brace-polyedron-curve-points1 :wall-locate-1-to-3-curve-for-polyhedron-control-point)
                                                                               (wall-brace-polyedron-curve-points-mid1 :wall-locate-1-to-3-curve-for-polyhedron-control-point)
                                                                               (wall-brace-polyedron-curve-points-mid2 :wall-locate-1-to-3-curve-for-polyhedron-control-point)  
                                                                                 (wall-brace-polyedron-curve-points2 :wall-locate-1-to-3-curve-for-polyhedron-control-point)  steps)

        wall-locate3-curve (bezier-cubic  (wall-brace-polyedron-curve-points1 :wall-locate3-point)   
                                              (wall-brace-polyedron-curve-points-mid1 :wall-locate3-point)
                                              (wall-brace-polyedron-curve-points-mid2 :wall-locate3-point)
                                              (wall-brace-polyedron-curve-points2 :wall-locate3-point) steps)
        
        wall-locate3-floor-curve (bezier-cubic (wall-brace-polyedron-curve-points1 :wall-locate3-point-floor)
                                             (wall-brace-polyedron-curve-points-mid1 :wall-locate3-point-floor)
                                             (wall-brace-polyedron-curve-points-mid2 :wall-locate3-point-floor)
                                               (wall-brace-polyedron-curve-points2 :wall-locate3-point-floor)  steps)
        
        wall-locate2-bottom-floor-curve (bezier-cubic  (wall-brace-polyedron-curve-points2 :wall-locate-2-bottom-floor)
                                                     (wall-brace-polyedron-curve-points-mid2 :wall-locate-2-bottom-floor)
                                                     (wall-brace-polyedron-curve-points-mid1 :wall-locate-2-bottom-floor)
                                                       (wall-brace-polyedron-curve-points1 :wall-locate-2-bottom-floor) steps)
        
        wall-locate2-bottom-curve (bezier-cubic  (wall-brace-polyedron-curve-points2 :wall-locate-2-bottom)
                                              (wall-brace-polyedron-curve-points-mid2 :wall-locate-2-bottom)
                                              (wall-brace-polyedron-curve-points-mid1 :wall-locate-2-bottom)
                                                 (wall-brace-polyedron-curve-points1 :wall-locate-2-bottom) steps)
        
        wall-locate2-top-curve (bezier-cubic   (wall-brace-polyedron-curve-points2 :wall-locate-2-top)
                                              (wall-brace-polyedron-curve-points-mid2 :wall-locate-2-top)
                                              (wall-brace-polyedron-curve-points-mid1 :wall-locate-2-top)
                                               (wall-brace-polyedron-curve-points1 :wall-locate-2-top)   steps)
        
        web-post-bottom-curve (bezier-cubic   (wall-brace-polyedron-curve-points2 :web-post-position-bottom)
                                              (wall-brace-polyedron-curve-points-mid2 :web-post-position-bottom)
                                              (wall-brace-polyedron-curve-points-mid1 :web-post-position-bottom)
                                              (wall-brace-polyedron-curve-points1 :web-post-position-bottom) steps)
        ]

    {:web-post-top-curve web-post-top-curve
     :wall-locate1-curve wall-locate1-curve
     :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve wall-locate-1-to-3-curve-for-polyhedron-second-control-curve
     :wall-locate-1-to-3-curve-for-polyhedron-control-curve wall-locate-1-to-3-curve-for-polyhedron-control-curve
     :wall-locate3-curve wall-locate3-curve
     :wall-locate3-floor-curve wall-locate3-floor-curve
     :wall-locate2-bottom-floor-curve wall-locate2-bottom-floor-curve
     :wall-locate2-bottom-curve wall-locate2-bottom-curve
     :wall-locate2-top-curve wall-locate2-top-curve
     :web-post-bottom-curve web-post-bottom-curve})
  
  )
(defn wall-brace-cubic-polyhedron
  [{:keys [place1 dx1 dy1  post-position-1 rad-or-deg1
           place-mid1 dxmid1 dymid1  post-position-mid1 rad-or-degmid1
           place-mid2 dxmid2 dymid2  post-position-mid2 rad-or-degmid2
           place2 dx2 dy2  post-position-2 rad-or-deg2 xy1 xymid1 xymid2 xy2
           steps] :or {steps 60 xy1 wall-xy-offset xymid1 wall-xy-offset xymid2 wall-xy-offset xy2 wall-xy-offset}}] 
   (let [wall-brace-cubic-polyhedron-curves-map 
         (wall-brace-cubic-polyhedron-curves 
          {:place1 place1 :dx1 dx1 :dy1 dy1  :post-position-1 post-position-1 :rad-or-deg1 rad-or-deg1
:place-mid1 place-mid1 :dxmid1 dxmid1 :dymid1 dymid1  :post-position-mid1 post-position-mid1 :rad-or-degmid1 rad-or-degmid1
:place-mid2 place-mid2 :dxmid2 dxmid2 :dymid2 dymid2  :post-position-mid2 post-position-mid2 :rad-or-degmid2 rad-or-degmid2
:place2 place2 :dx2 dx2 :dy2 dy2  :post-position-2 post-position-2 :rad-or-deg2 rad-or-deg2 :xy1 xy1 :xymid1 xymid1 :xymid2 xymid2 :xy2 xy2
           :steps steps
   })
         outer-points (into [] (apply concat
                                      (for [index (range 0 (inc steps))]
                                        (bezier-quintic
                                         (nth (wall-brace-cubic-polyhedron-curves-map :web-post-top-curve) index)
                                         (nth (wall-brace-cubic-polyhedron-curves-map :wall-locate1-curve) index)
                                         (nth (wall-brace-cubic-polyhedron-curves-map :wall-locate-1-to-3-curve-for-polyhedron-control-curve) index)
                                         (nth (wall-brace-cubic-polyhedron-curves-map :wall-locate-1-to-3-curve-for-polyhedron-second-control-curve) index)
                                         (nth (wall-brace-cubic-polyhedron-curves-map :wall-locate3-curve) index)
                                         (nth (wall-brace-cubic-polyhedron-curves-map :wall-locate3-floor-curve) index)
                                         steps))))

         inner-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                               (bezier-cubic
                                                (nth (wall-brace-cubic-polyhedron-curves-map :web-post-bottom-curve) index)
                                                (nth (wall-brace-cubic-polyhedron-curves-map :wall-locate2-top-curve) index)
                                                (nth (wall-brace-cubic-polyhedron-curves-map :wall-locate2-bottom-curve) index)
                                                (nth (wall-brace-cubic-polyhedron-curves-map :wall-locate2-bottom-floor-curve) index)
                                                steps))))
         smoother-wall-polyhedron (polyhedron (concat outer-points inner-points)
                                              (generate-bezier-along-bezier-polyhedron-faces
                                               outer-points inner-points
                                               steps))]
         ;wall-polyhedron 
     smoother-wall-polyhedron))

     (defn generate-polyhedron-web-connecters [top-left-place top-left-post-position top-left-rad-or-deg
     top-right-place top-right-post-position top-right-rad-or-deg
      bottom-left-place bottom-left-post-position bottom-left-rad-or-deg
       bottom-right-place bottom-right-post-position bottom-right-rad-or-deg
        & {:keys [steps] :or {steps 60}}]
        (let [top-left-web-post (web-post-point top-left-place top-left-post-position top-left-rad-or-deg)
        top-right-web-post (web-post-point top-right-place top-right-post-position top-right-rad-or-deg)
      bottom-left-web-post (web-post-point bottom-left-place bottom-left-post-position bottom-left-rad-or-deg)
       bottom-right-web-post (web-post-point bottom-right-place bottom-right-post-position bottom-right-rad-or-deg)
       polyhedron-web-connecters (generate-bezier-along-bezier-polyhedron-from-points-linear 
        (top-left-web-post :top) (top-right-web-post :top)
        (bottom-left-web-post :top) (bottom-right-web-post :top)
        (top-right-web-post :bottom) (top-left-web-post :bottom) 
        (bottom-right-web-post :bottom) (bottom-left-web-post :bottom) 
        steps
        )
        ]
        polyhedron-web-connecters
        )
     )

     (comment  (vec (concat [[10 10][ 10] [10]] [[10] [30] [490]])))
     
     (defrecord WebConnectorCorner [place post-position rad-or-deg])
     (defn generate-polyhedron-web-connecters-curve [top-left-tangent-key-corner top-left-key-corner 
                                                     bottom-left-key-corner bottom-left-tangent-key-corner 
                                                     top-right-tangent-key-corner top-right-key-corner
                                                     bottom-right-key-corner bottom-right-tangent-key-corner 
                                               &{:keys [steps left-curve-type right-curve-type left-midpoint right-midpoint] 
                                                 :or {steps 10 left-curve-type [:catmull :centripetal] right-curve-type [:catmull :centripetal] left-midpoint false right-midpoint false}}]
       (let [key-corner-data-to-web-post-point-fn (fn [key-corner-data] (apply web-post-point (vals key-corner-data)))
             top-left-tangent-web-post (key-corner-data-to-web-post-point-fn top-left-tangent-key-corner)
             top-left-web-post (key-corner-data-to-web-post-point-fn top-left-key-corner)
             bottom-left-web-post (key-corner-data-to-web-post-point-fn bottom-left-key-corner)
             bottom-left-tangent-web-post (key-corner-data-to-web-post-point-fn bottom-left-tangent-key-corner)
             top-right-tangent-web-post (key-corner-data-to-web-post-point-fn top-right-tangent-key-corner)
             top-right-web-post (key-corner-data-to-web-post-point-fn top-right-key-corner) 
             bottom-right-web-post (key-corner-data-to-web-post-point-fn bottom-right-key-corner)
             bottom-right-tangent-web-post (key-corner-data-to-web-post-point-fn bottom-right-tangent-key-corner)
             curve-fn (fn [curve-type midpoint](fn [points end-z end-n top-or-bottom] (cond (= curve-type :global)(let [p (mapv #(top-or-bottom %) points)
                                                                                               p-mid (mapv + (nth p 1) (mapv #(/ % 2)(mapv - (nth p 0) (nth p 1))))
                                                                                               final-points (if midpoint [(nth p 0) p-mid (nth p 1)]
                                                                                                                p)](vec (global-curve-interp-with-end-unit-derivatives-curve final-points 3 (top-or-bottom end-z) (top-or-bottom end-n) steps)))
                                                                   (and (vector? curve-type)(= (nth curve-type 0) :catmull)) (let [p (mapv #(top-or-bottom %) points) 
                                                                                                 p-mid (mapv + (nth p 1) (mapv #(/ % 2) (mapv - (nth p 0) (nth p 1))))
                                                                                                 points-p [(top-or-bottom end-z) (nth p 0) p-mid (nth p 1) (top-or-bottom end-n)]
                                                                                                 ](vec (catmull-rom-spline-curve points-p steps :alphaType (nth curve-type 1))))
                                                                                            (= curve-type :linear) (let [p (mapv #(top-or-bottom %) points)
                                                                                                                         p-mid (mapv + (nth p 1) (mapv #(/ % 2) (mapv - (nth p 0) (nth p 1))))
                                                                                                                         final-points (if midpoint [(nth p 0) p-mid (nth p 1)]
                                                                                                                                          p)]
                                                                                                                     (bezier-linear-spline final-points steps)))))
             left-curve-fn (curve-fn  left-curve-type left-midpoint)
             right-curve-fn (curve-fn right-curve-type right-midpoint)
             left-top-curve (left-curve-fn [top-left-web-post bottom-left-web-post] top-left-tangent-web-post bottom-left-tangent-web-post :top)
             left-bottom-curve (left-curve-fn [top-left-web-post bottom-left-web-post] top-left-tangent-web-post bottom-left-tangent-web-post :bottom)
             right-top-curve (right-curve-fn [top-right-web-post bottom-right-web-post] top-right-tangent-web-post bottom-right-tangent-web-post :top)
             right-bottom-curve (right-curve-fn [top-right-web-post bottom-right-web-post] top-right-tangent-web-post bottom-right-tangent-web-post :bottom)
             top-face (vec (for [index (range 0 (inc steps))]
                             (bezier-linear
                              (nth left-top-curve index)
                              (nth right-top-curve index)
                              steps
                              )
                             )
                           )
             bottom-face (vec (for [index (range 0 (inc steps))]
                             (bezier-linear 
                              (nth right-bottom-curve index)
                              (nth left-bottom-curve index)
                              steps)))
             points-array (generate-array-for-vnf top-face bottom-face)] 
         points-array))

     (comment (let [col 0
                    row-above 0
                    row-below 1
                    p [[(partial key-place col row-above) :tl :radians]
               [(partial key-place col row-above) :bl :radians]
               [(partial key-place col row-below) :tl :radians]
                [(partial key-place col row-below) :bl :radians]]
                    ]
                (mapv #(apply web-post-point %) p)))
     (defn generate-polyhedron-row-column-crossroads-connecters [column-left column-right row-above row-below 
                                                                 &{:keys [steps curve-type] :or {steps 10 curve-type :global}}]
       (let [data-to-web-post-point-fn (fn [pos-data] (apply web-post-point pos-data))
             curve-fn (fn [top-or-bottom data] (let [points (mapv data-to-web-post-point-fn data)](cond (= curve-type :global) (global-curve-interp-with-end-unit-derivatives-curve (mapv #(top-or-bottom %) (subvec points 1 3)) 3 (top-or-bottom (first points)) (top-or-bottom (peek points)) steps)
                   (= curve-type :catmull) (catmull-rom-spline-curve (mapv #(top-or-bottom %) points) steps))))
             row-curve-fn (fn [points] (cond (= curve-type :global ) 
                                (global-curve-interp-with-end-unit-derivatives-curve  (subvec points 1 3) 3  (first points)  (peek points) steps)
                                             (= curve-type :catmull)
                                             (catmull-rom-spline-curve points steps :alphaType :chordal)))
             column-curve (fn [col start-pos] (fn [top-or-bottom] (curve-fn top-or-bottom (case start-pos
                                                :tl [[(partial key-place col row-below) :bl :radians]
                                                     [(partial key-place col row-below) :tl :radians]
                                                       [(partial key-place col row-above) :bl :radians] 
                                                     [(partial key-place col row-above) :tl :radians]   ]
                                                :tr [[(partial key-place col row-below) :br :radians] 
                                                     [(partial key-place col row-below) :tr :radians]
                                                      [(partial key-place col row-above) :br :radians]
                                                     [(partial key-place col row-above) :tr :radians]]) )))
             face-fn (fn [top-or-bottom reverse]
                       (let [left-tangent-col-curve ((column-curve column-left :tl) top-or-bottom)
                             left-col-curve ((column-curve column-left :tr) top-or-bottom)
                             right-col-curve ((column-curve column-right :tl) top-or-bottom)
                             right-tangent-col-curve ((column-curve column-left :tr) top-or-bottom)
                             lin1 (bezier-linear (top-or-bottom (web-post-point (partial key-place column-left row-below) :tl :radians))
                                                 (top-or-bottom (web-post-point (partial key-place column-left row-above) :bl :radians)) 
                                                 steps)
                             lin2 (bezier-linear (top-or-bottom (web-post-point (partial key-place column-right row-below) :tr :radians))
                                                 (top-or-bottom (web-post-point (partial key-place column-left row-above) :br :radians))
                                                 steps)]
                         (vec (for [index (range (inc steps))
                                    :let [points
                                          (if reverse [(nth left-tangent-col-curve index) 
                                                       (nth left-col-curve index)
                                                       (nth right-col-curve index)
                                                       (nth right-tangent-col-curve index)]
                                              [(nth right-tangent-col-curve index)
                                               (nth right-col-curve index)
                                               (nth left-col-curve index)
                                               (nth left-tangent-col-curve index)])]]
                           (row-curve-fn
                             points
                             )
                           ))
                         ))
             top-face (face-fn :top false)
             bottom-face (face-fn :bottom true)
             ;points-array (generate-array-for-vnf top-face bottom-face)
             ]
             
             (generate-array-for-vnf top-face bottom-face)
             )
       
       )
     
     (defn generate-all-polyhedron-row-column-crossroads-connecters [& {:keys [curve-type steps] :or {curve-type :global steps 20}}]
       (vec (for [row (range 0 2) column (range 0 lastcol)]
              (generate-polyhedron-row-column-crossroads-connecters column (inc column) row (inc row) :steps steps
                                                                    :curve-type curve-type)
              ))
       )
     (comment (let [test-fn (fn [yo] (fn [hi] (println yo hi)))]
                ((test-fn "yo") "hi")))
     (defn generate-polyhedron-main-body-connecters [&{:keys [curve-type steps] :or {curve-type :global steps 20}}]
       (let [rows (for [row (range 2) column (range (inc lastcol))]
                    (generate-polyhedron-web-connecters-curve 
                     (main-body-key-corner column row  :tl)
                     (main-body-key-corner column row  :bl)
                     (main-body-key-corner column (inc row)  :tl)
                     (main-body-key-corner column (inc row)  :bl)
                     (main-body-key-corner column row  :tr)
                     (main-body-key-corner column row  :br)
                     (main-body-key-corner column (inc row)  :tr)
                     (main-body-key-corner column (inc row)  :br)
                     :steps steps
                     ;:curve-type curve-type
                     ) 
                    )
             columns (for [column (range lastcol) row (range 2 -1 -1)
                           :let [left-curve-type (cond (= row 2) :linear 
                                                       (= row 1)[:catmull :chordal]
                                                       :else [:catmull :centripetal])
                                 right-curve-type (if (= row 2) [:catmull :chordal] [:catmull :centripetal] )
                                 left-midpoint (not= row 0)
                                 right-midpoint (some #(not= row %) [0 1]) 
                                 ]
                           :when (or (not= row 2) (not= column 1))
                           ]
                       (generate-polyhedron-web-connecters-curve
                        (main-body-key-corner column row :bl)
                        (main-body-key-corner column row :br)
                        (main-body-key-corner (inc column) row :bl)
                        (main-body-key-corner (inc column) row :br)
                        (main-body-key-corner column row :tl)
                        (main-body-key-corner column row :tr)
                        (main-body-key-corner (inc column) row :tl)
                        (main-body-key-corner (inc column) row :tr)
                        :steps steps
                        :left-curve-type left-curve-type
                        :right-curve-type right-curve-type
                        :left-midpoint left-midpoint
                        :right-midpoint right-midpoint
                        ) 
                       )
             crossroads (for [column (range lastcol) row (range 1 3)
                              :let [left-curve-type (if (= row 2) [:catmull :chordal] [:catmull :centripetal])
                                    right-curve-type (if (= row 2) [:catmull :chordal] [:catmull :centripetal])]]
                         (generate-polyhedron-web-connecters-curve
                          (main-body-key-corner column row  :tl)
                          (main-body-key-corner column row  :tr)
                          (main-body-key-corner (inc column) row  :tl)
                          (main-body-key-corner (inc column) row  :tr)
                          
                          (main-body-key-corner column (dec row)  :bl)
                          (main-body-key-corner column (dec  row) :br)
                          (main-body-key-corner (inc column) (dec row)  :bl)
                          (main-body-key-corner (inc column) (dec row)  :br)
                          
                          :steps steps
                          :left-curve-type left-curve-type
                          :right-curve-type right-curve-type
                          ;:curve-type curve-type
                          ) )
             ]
         (concat columns 
                 rows crossroads))
       )
     
     (defn generate-polyhedron-key-web-connecters [top-left-place top-left-post-position 
     top-right-place top-right-post-position 
      bottom-left-place bottom-left-post-position 
       bottom-right-place bottom-right-post-position & {:keys [steps] :or {steps 60}}]
      (generate-polyhedron-web-connecters
       top-left-place top-left-post-position :radians 
     top-right-place top-right-post-position :radians
      bottom-left-place bottom-left-post-position :radians
       bottom-right-place bottom-right-post-position :radians :steps steps)
       )

     (defn generate-polyhedron-thumb-web-connecters [top-left-place top-left-post-position 
     top-right-place top-right-post-position 
      bottom-left-place bottom-left-post-position 
       bottom-right-place bottom-right-post-position & {:keys [steps] :or {steps 60}}]
      (generate-polyhedron-web-connecters
       top-left-place top-left-post-position :degrees 
     top-right-place top-right-post-position :degrees
      bottom-left-place bottom-left-post-position :degrees
       bottom-right-place bottom-right-post-position :degrees :steps steps)
       )

     (defn points-fn [place dx dy post-position  rad-or-deg xy] {:place place :dx dx :dy dy :post-position post-position :rad-or-deg rad-or-deg :xy xy})
     (defn points-fn-rad [place dx dy post-position xy] (points-fn place dx dy post-position :radians xy))
     (defn points-fn-deg [place dx dy post-position  xy] (points-fn place dx dy post-position :degrees xy))

(defn get-wall-brace-polyhedron-fn [bottom-plate] (if (true? bottom-plate) wall-brace-polyhedron-outer-floor-linear wall-brace-polyhedron))
(defn get-wall-brace-polyhedron-circular-fn [bottom-plate] (if (true? bottom-plate) wall-brace-polyhedron-circular-outer-floor-linear wall-brace-polyhedron-circular))
(defn get-wall-brace-quadratic-fn [bottom-plate] (if (true? bottom-plate)  wall-brace-quadratic-polyhedron-bottom wall-brace-quadratic-polyhedron))
(defn get-wall-brace-with-circular-fn [bottom-plate] (if (true? bottom-plate)  wall-brace-with-circular-outer-floor-linear wall-brace-with-circular))
(defn get-wall-brace-cubic-fn [bottom-plate] (if (true? bottom-plate) wall-brace-cubic-polyhedron-floor-outer wall-brace-cubic-polyhedron))  
     (defn get-wall-brace-bezier-cubic-through-points-fn [bottom-plate] (if (true? bottom-plate) wall-brace-bezier-cubic-through-points-floor wall-brace-bezier-cubic-through-points-polyhedron))
(defn get-key-wall-brace-polyhedron-fn [bottom-plate] (if (true? bottom-plate) key-wall-brace-polyhedron-outer-floor-linear key-wall-brace-polyhedron))
(defn get-key-wall-brace-polyhedron-circular-fn [bottom-plate] (if (true? bottom-plate) key-wall-brace-polyhedron-circular-outer-floor-linear key-wall-brace-polyhedron-cicular))
(defn get-wall-brace-quadratic-polyhedron-circular-fn [bottom-plate] (if (true? bottom-plate) wall-brace-quadratic-polyhedron-circular-floor-outer wall-brace-quadratic-polyhedron-circular))
(defn get-key-wall-brace-polyhedron-with-circular-fn [bottom-plate] (if (true? bottom-plate) key-wall-brace-polyhedron-with-circular-outer-floor-linear key-wall-brace-polyhedron-with-circular))
(defn get-thumb-wall-brace-polyhedron-fn [bottom-plate] (if (true? bottom-plate) thumb-wall-brace-polyhedron-outer-floor-linear thumb-wall-brace-polyhedron))
(defn get-thumb-wall-brace-polyhedron-circular-fn [bottom-plate] (if (true? bottom-plate) thumb-wall-brace-polyhedron-circular-outer-floor-linear thumb-wall-brace-polyhedron-circular))
(defn get-thumb-wall-brace-polyhedron-with-circular [bottom-plate] (if (true? bottom-plate) thumb-wall-brace-polyhedron-with-circular-outer-floor-linear thumb-wall-brace-polyhedron-with-circular))
(defn get-wall-brace-catmull-rom-spline-fn [bottom-plate] (if (true? bottom-plate) wall-brace-catmull-rom-spline-floor wall-brace-catmull-rom-spline))
(defn get-collect-fn [bottom-plate] (if (true? bottom-plate) concat union))

(defn place-symbol [file {:keys [height place translation orientation-angle offset scale-x scale-y center z-rotation rotation] 
                          :or {offset [0 0 0]}}]
  (let [svg (call :import (format "file = \"%s\"" file) (format "center = %s" center))
        ]
    (->> svg 
         (scale [scale-x scale-y 1]) 
         (extrude-linear {:height height :center false})
         (rdz orientation-angle)
         rotation
         (rdz z-rotation)
         (translate (mapv + place offset translation))
         )
    )
  )

(defn place-2d-shape [shape {:keys [height place translation orientation-angle offset scale-x scale-y center z-rotation rotation]
                          :or {offset [0 0 0]}}]
  (let []
    (->> shape
         (scale [scale-x scale-y 1])
         (extrude-linear {:height height :center false})
         (rdz orientation-angle)
         rotation
         (rdz z-rotation)
         (translate (mapv + place offset translation)))))

(defn place-2d-shape-on-case-wall [shape {:keys [height place orientation-angle z-rotation offset scale-x scale-y center position wall-xy-offset]
                                       :or {height 1 center true  orientation-angle 0 z-rotation 0 wall-xy-offset wall-xy-offset offset [0 0 0]}}]
  (let [web-corner-translation-vector (get-web-corner-translation-vector position)
        rotation (case position
                   "tm" #(rdx -90 %)
                   "bm" #(rdx 90 %)
                   "lm" #(rdy -90 %)
                   "rm" #(rdy 90 %))
        z (/ (nth place 2) -2)
        translation-fn (case position
                         "tm" (fn [vector z-vector] [0 vector z-vector])
                         "bm" (fn [vector z-vector] [0 (- vector) z-vector])
                         "lm" (fn [vector z-vector] [(- vector) 0 z-vector])
                         "rm" (fn [vector z-vector] [vector 0 z-vector]))
        translation (mapv + web-corner-translation-vector (translation-fn (+ wall-thickness wall-xy-offset) z))]
    (place-2d-shape shape  {:height height :place place :translation translation
                        :orientation-angle orientation-angle
                        :scale-x scale-x :scale-y scale-y :center center
                        :z-rotation z-rotation :offset offset :rotation rotation})))

(defn place-symbol-on-case-wall [file {:keys [height place orientation-angle z-rotation offset scale-x scale-y center position wall-xy-offset] 
                                      :or {height 1 center true  orientation-angle 0 z-rotation 0 wall-xy-offset wall-xy-offset offset [0 0 0]}}]
  (let [web-corner-translation-vector (get-web-corner-translation-vector position)
        rotation (case position
                   "tm" #(rdx -90 %)
                   "bm" #(rdx 90 %)
                   "lm" #(rdy -90 %)
                   "rm" #(rdy 90 %)
                   )
        z (/ (nth place 2) -2)
        translation-fn (case position
                         "tm" (fn [vector z-vector] [0 vector z-vector])
                         "bm" (fn [vector z-vector] [0 (- vector) z-vector])
                         "lm" (fn [vector z-vector] [(- vector) 0 z-vector])
                         "rm" (fn [vector z-vector][vector 0 z-vector])
                         )
        translation (mapv + web-corner-translation-vector (translation-fn (+ wall-thickness wall-xy-offset) z))] 
    (place-symbol file {:height height :place place :translation translation 
                        :orientation-angle orientation-angle
                        :scale-x scale-x :scale-y scale-y :center center 
                        :z-rotation z-rotation :offset offset :rotation rotation})
    ) 
  )

 (defn place-symbol-on-thumb-wall [file {:keys [height place orientation-angle z-rotation offset scale-x scale-y center position wall-xy-offset rotation]
                                         :or {height 1 center true  orientation-angle 0 z-rotation 0 wall-xy-offset wall-xy-offset offset [0 0 0] rotation #(rdx 0 %)}}]
   (let [web-corner-translation-vector (get-web-corner-translation-vector position)
         thumb-rotation (case position
                    "tm" #(rdx -90 %)
                    "bm" #(rdx 90 %)
                    "lm" #(rdy -90 %)
                    "rm" #(rdy 90 %))
         z (/ (nth (transform-position place [0 0 0]) 2) -2)
         translation-fn (case position
                          "tm" (fn [vector z-vector] [vector 0 z-vector])
                          "bm" (fn [vector z-vector] [(- vector) 0 z-vector])
                          "lm" (fn [vector z-vector] [0 (- vector) z-vector])
                          "rm" (fn [vector z-vector] [0 vector z-vector]))
         translation (mapv + web-corner-translation-vector (translation-fn (+ wall-thickness wall-xy-offset) z))
         svg (call :import (format "file = \"%s\"" file) (format "center = %s" center))]
     (->> svg
          (scale [scale-x scale-y 1])
          (extrude-linear {:height height :center false})
          (rdz orientation-angle)
          thumb-rotation
          rotation
          (rdz z-rotation)
          (translate (mapv + offset translation))
          (place)
          )
     )
   )
 
  (defn place-2d-shape-on-thumb-wall [shape {:keys [height place orientation-angle z-rotation offset scale-x scale-y center position wall-xy-offset]
                                          :or {height 1 center true  orientation-angle 0 z-rotation 0 wall-xy-offset wall-xy-offset offset [0 0 0]}}]
    (let [web-corner-translation-vector (get-web-corner-translation-vector position)
          rotation (case position
                     "tm" #(rdx -90 %)
                     "bm" #(rdx 90 %)
                     "lm" #(rdy -90 %)
                     "rm" #(rdy 90 %))
          z (/ (nth (transform-position place [0 0 0]) 2) -2)
          translation-fn (case position
                           "tm" (fn [vector z-vector] [vector 0 z-vector])
                           "bm" (fn [vector z-vector] [(- vector) 0 z-vector])
                           "lm" (fn [vector z-vector] [0 (- vector) z-vector])
                           "rm" (fn [vector z-vector] [0 vector z-vector]))
          translation (mapv + web-corner-translation-vector (translation-fn (+ wall-thickness wall-xy-offset) z))
          ]
      (->> shape
           (scale [scale-x scale-y 1])
           (extrude-linear {:height height :center false})
           (rdz orientation-angle)
           rotation
           (rdz z-rotation)
           (translate (mapv + offset translation))
           (place))))

 (defn place-symbol-on-key-wall [file {:keys [height column row orientation-angle z-rotation offset scale-x scale-y center position wall-xy-offset]
                                          :or {height 1 center true column 0 row 0 orientation-angle 0 z-rotation 0 wall-xy-offset wall-xy-offset offset [0 0 0]}}]
   (let [place (key-position column row [0 0 0])]
     
     (place-symbol-on-case-wall file {:height height :place place
                         :orientation-angle orientation-angle
                         :scale-x scale-x :scale-y scale-y :center center
                         :z-rotation z-rotation :offset offset :wall-xy-offset wall-xy-offset
                                      :position position}))
   )
 

