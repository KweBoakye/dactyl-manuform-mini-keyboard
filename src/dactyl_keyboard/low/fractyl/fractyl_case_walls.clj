(ns dactyl-keyboard.low.fractyl.fractyl-case-walls
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [div magnitude mul]]
            [clojure.math :refer [ceil cos sqrt]]
            [dactyl-keyboard.des-caps :refer [des-scooped des-r2]]
            [dactyl-keyboard.lib.affine-transformations :refer :all]
            [dactyl-keyboard.lib.algebra :refer [find-point-on-line-using-x]]
            [dactyl-keyboard.lib.constants :refer [epsilon]]
            [dactyl-keyboard.lib.curvesandsplines.beziers :refer [n-degree-bezier-curve]]
            [dactyl-keyboard.lib.curvesandsplines.curve-fitting :refer [calculate-tangents-for-local-cubic-curve-interpolation-from-tangent
                                                                        global-curve-interp-with-calculated-first-derivatives
                                                                        global-curve-interp-with-calculated-first-derivatives-curve
                                                                        global-curve-interp-with-end-derivatives-calculated
                                                                        local-cubic-curve-interpolation-with-calculated-tangents
                                                                        local-cubic-curve-interpolation-with-tangents-curve]]
            [dactyl-keyboard.lib.curvesandsplines.linear-surface :refer [lofted-surface]]
            [dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline :refer [decompose-b-spline-curve-and-calculate-bezier-curves
                                                                               decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                                                                               get-function-for-u-k-values
                                                                               non-uniform-b-spline
                                                                               nurbs
                                                                               nurbs-with-calculated-knot-vector]]
            [dactyl-keyboard.lib.curvesandsplines.splines :refer [catmull-rom-spline-curve]]
            [dactyl-keyboard.lib.geometry :refer [deg2rad]]
            [dactyl-keyboard.lib.matrices :refer [rotate-matrix]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [include-bosl2 include-bosl2-joiners]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.joiners :refer [dovetail
                                                                         snap-pin]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vnf :refer :all]
            [dactyl-keyboard.lib.openscad.hull :refer [chained-hull-to-points]]
            [dactyl-keyboard.lib.transformations :refer [rdx rdy rdz rz]]
            [dactyl-keyboard.low.aviator-low :refer [aviator-assembly-diffs
                                                     aviator-assembly-polyhedron]]
            [dactyl-keyboard.low.case-low :refer [usb-jack-height
                                                  usb-jack-width]]
            [dactyl-keyboard.low.case-low-polyhedron-functions :refer :all]
            [dactyl-keyboard.low.fractyl.fractyl-key-plate-connectors :refer :all]
            [dactyl-keyboard.low.fractyl.fractyl-screw-inserts :refer :all]
            [dactyl-keyboard.low.fractyl.svg.svg-point :refer [svg-import]]
            [dactyl-keyboard.low.oled-low-placements :refer [screen-holder
                                                             screen-holder-cut
                                                             screen-holder-cut-viewport-cut
                                                             screen-holder-depth]]
            [dactyl-keyboard.low.placement-functions-low :refer :all]
            [dactyl-keyboard.low.screen-holder-placement-functions :refer [screen-holder-place-side]]
            [dactyl-keyboard.low.screen-holder-placement-points :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.low.thumbs-low :refer :all]
            [dactyl-keyboard.low.tps-65-placement-functions :refer :all]
            [dactyl-keyboard.low.tps-65-placement-points :refer :all]
            [dactyl-keyboard.low.vvybronics-vl91022-placement-functions :refer [vybronics-vl91022-place]]
            [dactyl-keyboard.low.web-connecters-low :refer :all]
            [dactyl-keyboard.MxLEDBitPCB-holder :refer [MxLEDBitPCB
                                                        MxLEDBitPCB-clearance-smaller
                                                        single-key-pcb-holder
                                                        single-key-pcb-holder-north-leg
                                                        single-key-pcb-holder-south-leg]]
            [dactyl-keyboard.RP2040-Plus :refer [rp2040-plus rp2040-plus-mount
                                                 rp2040-plus-mount-body-clearance
                                                 rp2040-plus-place]]
            [dactyl-keyboard.sk8707-06 :refer :all]
            [dactyl-keyboard.sk8707-51 :refer :all]
            [dactyl-keyboard.switch-hole :refer [mount-width plate-thickness
                                                 single-plate]]
            [dactyl-keyboard.tps-65 :refer :all]
            [dactyl-keyboard.utils :refer [plot-bezier-points]]
            [dactyl-keyboard.vybronics-vl91022 :refer [vybronics-vl91022-mount]]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))

(comment 
  (spit "things-low/bosl2-dovetail-test.scad"
        (write-scad
         (include "../BOSL2/joiners.scad")
         (include include-bosl2)
         (dovetail "female" 15 8 :slide 30))))
         
  
(def kailh-hotswap-mx
  (translate [0.75 -4.75 (- plate-thickness)] (import "../parts/Kailh Hotswap MX v22.stl")))

(def single-key-pcb-holder-on-main-body
  (apply union
         (for [column columns
               row rows
               :when (and (check-last-row-middle-and-fourth-keys-only column row)
                         (false? (and (= column 0) (= row 2))))]
           (key-place column row single-key-pcb-holder))))

(def single-key-pcb-holder-on-thumbs
  (union
   (thumb-1x-layout single-key-pcb-holder)))
   ;(thumb-15x-layout single-key-pcb-holder)
   

(def pcb-place
  (apply union
         (for [column columns
               row rows
               :when (check-last-row-middle-and-fourth-keys-only column row)]
           (key-place column row MxLEDBitPCB))))

(def pcb-place-thumbs 
  (union
   (thumb-1x-layout MxLEDBitPCB)
   (thumb-15x-layout MxLEDBitPCB)))


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
                   :linear-outer-top true :linear-inner-top true)) 
                  
                 wall-cross-section-steps wall-section-steps))
                ;{:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :alt}))
       

(defn usb-jack-place-new [shape &{:keys [column-pos row-pos slant wall-xy extra-z-rot] 
                                  :or {column-pos 0 row-pos 0 slant :no-slant wall-xy 5 extra-z-rot 0.0}}]
  (let [wall-position-1 (key-wall-position column-pos row-pos 0 1 :tm :xy wall-xy :slant slant)
        wall-position-2 (key-wall-position column-pos row-pos 0 1 :tr-tm :xy wall-xy :slant slant)
        position-1 (:wall-locate3-point-floor (calculate-control-points wall-position-1)) 
        position-2 (:wall-locate3-point-floor (calculate-control-points wall-position-2))
        position (div (mapv + (mul 0.6 position-1) (mul 0.4 position-2)) 1)]
        
    (->> (rdz (+ far-index-splay extra-z-rot) shape)
         ;(translate web-post-tm-translation-vector)
         ;(translate (rotate-around-z-in-degrees (+ far-index-splay extra-z-rot) [0 (nth far-index-post-splay-translation 0) 0]))
         (translate position)
         (translate [0 (+ (- wall-thickness plate-thickness) 0.5) 5]))))
         
    
  
  

(defn fractyl-usb-c-port [steps]
  (let [points-fn (fn [x-dis x-offset y-dis z-dis]
                    [[(- x-dis) y-dis 0]
                     [(- x-dis) y-dis z-dis]
                     [(+ (- x-dis) x-offset) y-dis z-dis]
                     [(- x-dis x-offset) y-dis z-dis]
                     [x-dis y-dis z-dis]
                     [x-dis y-dis 0]
                     [x-dis y-dis (- z-dis)]
                     [(- x-dis x-offset) y-dis (- z-dis)]
                     [(+ (- x-dis) x-offset) y-dis (- z-dis)]
                     [(- x-dis) y-dis (- z-dis)]
                     [(- x-dis) y-dis 0]])
        south-side-points (points-fn (/ usb-jack-width 2) (/ usb-jack-height 2) (- plate-thickness) (/ usb-jack-height 2))
        circ-weight  (/ (sqrt 2) 2)
        weights [1 circ-weight 1 1 circ-weight 1 circ-weight 1 1 circ-weight 1]
        knot-vector (let [denominator (dec (count south-side-points))]
                      (mul (dec denominator)
                           [0 0 0  (/ 2 denominator) (/ 2.5 denominator)
                            (/ 3 denominator)
                            (/ 5 denominator) (/ 5 denominator)
                            (/ 7 denominator) (/ 7.5 denominator) (/ 8 denominator)  1 1 1]))
        curve-fn (fn [points steps]
                   (nurbs points 2 knot-vector
                          weights steps))
        south-side-nurbs (curve-fn south-side-points steps)
        mid-nurbs (curve-fn (points-fn (/ usb-jack-width 2) (/ usb-jack-height 2) 0 (/ usb-jack-height 2)) steps)
        mid-to-mid-end-control (curve-fn (points-fn (/ usb-jack-width 2) (/ usb-jack-height 2) (/ plate-thickness 4) (/ usb-jack-height 2)) steps)
        mid-end-nurbs (curve-fn (points-fn (* usb-jack-width 0.6) (* usb-jack-height 0.6) (/ plate-thickness 4) (* (dec usb-jack-height)0.75)) steps)
        mid-end-to-end-control (curve-fn (points-fn (* (dec usb-jack-width) 0.7) (* usb-jack-height 0.7) (/ plate-thickness 4) (dec usb-jack-height)) steps)
        end-nurbs (curve-fn (points-fn (* (dec usb-jack-width) 0.7) (* usb-jack-height 0.7) (/ plate-thickness 2) (dec usb-jack-height)) steps)
        shape-weights [1 1 circ-weight 1 circ-weight 1]
        shape-knot-vector (let [denom (dec (count shape-weights))]
                            (mul (dec denom) [0 0 0 (/ 1 denom) (/ 3 denom) (/ 3 denom) 1 1 1]))
        shape  (vec (for [index (range (inc steps))]
                      (nurbs
                       [(nth south-side-nurbs index)
                        (nth mid-nurbs index)
                        (nth mid-to-mid-end-control index)
                        (nth mid-end-nurbs index)
                        (nth mid-end-to-end-control index)
                        (nth end-nurbs index)]
                       2
                       shape-knot-vector
                       shape-weights
                       steps)))]
       (vnf-polyhedron (vnf-vertex-array (rotate-matrix shape :reverse-new-column true) :caps true :col-wrap true :row-wrap false))))

(def fractyl-usb-c-port-2 (hull
                       (->> (binding [*fn* 36] (cylinder (* usb-jack-height 0.7) (* plate-thickness 1.5) :center false))
                            (rdx 90)
                            (translate [(- (* usb-jack-height 0.7) (* (dec usb-jack-width) 0.7)) 2 0]))
                       (->> (binding [*fn* 36] (cylinder (* usb-jack-height 0.7) (* plate-thickness 1.5) :center false))
                            (rdx 90)
                            (translate [(- (* (dec usb-jack-width) 0.7) (* usb-jack-height 0.7)) 2 0]))))
(comment (spit "things-low/usb-test.scad" 
               (write-scad
                (include include-bosl2) 
                (union
                  (fractyl-usb-c-port 30)
                 (-# (hull
                      (->> (binding [*fn* 36] (cylinder (* usb-jack-height 0.7) (* plate-thickness 1.5) :center false )   )
                           (rdx 90) 
                           (translate [(- (* usb-jack-height 0.7) (* (dec usb-jack-width) 0.7)) 2 0]))
                      (->> (binding [*fn* 36] (cylinder (* usb-jack-height 0.7) (* plate-thickness 1.5) :center false))
                           (rdx 90)
                           (translate [(- (* (dec usb-jack-width) 0.7) (* usb-jack-height 0.7) ) 2 0]))
                      ))
                 ))))
                  ;usb-jack-polyhedron
                   ;rp2040-plus
                  ;;  (->>(rdx -90 (import "../parts/usb connector type c female 6pin.stl"))
                  ;;   (translate [0 (/ rp2040-plus-length 2) (- (+ rp2040-plus-mount-height 0.4) (+ rp2040-plus-usb-connecter-height 
                  ;;                                                )
                  ;;                                             )]))
                  ;;  (->> (import "../parts/pico-r3.stl")
                  ;;       (rdx -90 )
                  ;;       (rdz 180)
                  ;;       (translate [(/ rp2040-plus-width 2) (/ rp2040-plus-length -2)  (- rp2040-plus-mount-height rp2040-plus-thickness)]))
                   

(defn alignment-peg-and-hole [&{:keys [radius length] :or {radius 0.4 length 1.6}}] 
    {:peg (binding [*fn* 8] (cylinder radius length))
     :hole (binding [*fn* 36] (cylinder radius length))})
  

(defn sk8707-51-place [shape]
  (if (and (vector? shape)  (number? (nth shape 0))) 
    (->> shape 
         (rotate-around-z-in-degrees 0)
         (rotate-around-x-in-degrees 5)
         (mapv + [0 0 0.75] (mapv + web-post-tl-translation-vector [-1.5 2.5 0]
                                  [0 0 (- (+ sk8707-51-stem-holder-height sk8707-51-pcb-thickness 0))]))
         (key-place 2 1 (partial mapv +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees))
    (->> shape
         (rdz 0)
         (rdx 5)
         (translate [0 0 0.75])
         (translate (mapv + web-post-tl-translation-vector [-1.5 2.5 0]
                          [0 0 (- (+ sk8707-51-stem-holder-height sk8707-51-pcb-thickness 0))]))
         (key-place 2 1))))

(comment 
  (cos (deg2rad 30)))
(defn back-wall-alignment-positions [type]
    (union
     (->> (alignment-peg-and-hole :radius 0.75)
       (type)
       (rdx 95)
                     ;(rdx -85)
                     ;(rdz -5)
       (translate (mapv + (:wall-locate3-point (calculate-control-points (key-wall-position lastcol 0 0 1 :tm)))
                        [0 -0.75 -7.1])))
     (->> (alignment-peg-and-hole)
          (type)
          (rdx 95)
                                  ;(rdx -85)
                                  ;(rdz -5)
          (translate (mapv + (:wall-locate3-point (calculate-control-points (key-wall-position lastcol 0 0 1 :tm)))
                           [0 -1.5 -2.5])))))
  

(comment (spit "things-low/back-wall-test.scad"
          (write-scad
           (include "../BOSL2/std.scad")
           (let [curve-points (vec (concat
                                    [(main-body-web-post-point-top lastcol 0  :tr)]
                                    (apply concat (for [col-index (range 0 ncols)
                                                        :let [col (- lastcol col-index)
                                                              end-position (if (= col lastcol) :tm :tr)
                                                              tr-slant (if (zero? col) :no-slant :parallel-by-d)
                                                              tl-slant (if (or (zero? col) (= col 1)) :no-slant :parallel-by-d)]]
                                                    (concat
                                                     [(main-body-web-post-point-top col 0  end-position)]
                                                     [(main-body-web-post-point-top col 0 :tl)])))
                                    [tps-65-bottom-right-outer]
                                    [tps-65-mid-right-outer]))
                 position-1 (:wall-locate-2-bottom-floor (calculate-control-points (tps-65-wall-position :tr :north-west)))
                 position-2 (:wall-locate-2-bottom-floor (calculate-control-points (key-wall-position 0 0 0 1 :tl  :slant :no-slant)))]
             (difference
              (union
              ;; (->>
              ;;  sk8707-06
              ;;  (rdx 5)
              ;;  (translate [0 0 -0.5]) 
              ;;  (rdz 0)
              ;;  (translate (mapv + web-post-tl-translation-vector [-1.5 2.5 0]
              ;;                   [0 0 (- (+ sk8707-06-stem-holder-height sk8707-06-pcb-height))]))
              ;;  (key-place 2 1))
               (sk8707-51-breakout-place (translate [0 0 4] sk8707-51-breakout))
               (-# (translate [0 10 -1](cube 20 20 2)))
               (sk8707-51-place (sk8707-51-mount))
               (difference (hull (sk8707-51-place (translate [0 0 (- 2)](sk8707-51-mount :sk8707-51-mount-thickness 0.1)))
                            (extrude-linear {:height 0.1 :center false } (project (sk8707-51-place (translate [0 0 (- 2)] (sk8707-51-mount :sk8707-51-mount-thickness 0.1))))))
                         (sk8707-51-place (sk8707-51-mount-cutout :sk8707-51-mount-thickness 10)))  
                      
               (plot-bezier-points  (sk8707-51-mount-shape (/ (+ sk8707-51-mounting-hole-diameter 4) 2) :func sk8707-51-place) (sphere 0.1))
              ;; (->>
              ;;  sk8707-51
              ;; (sk8707-51-place)
           
              ;;  )
          
               (key-place 1 0 (des-scooped 0))
               (key-place 1 1 (des-scooped 1))
               (key-place 2 0 (des-scooped 0))
               (key-place 2 1 (des-scooped 1))
               (let [items (union
                            (-# MxLEDBitPCB)
                            kailh-hotswap-mx
                            switch-model)]
                 (union (key-place 1 0 items)
                  (key-place 1 1 items)
                  (key-place 2 0 items)
                  (key-place 2 1 items)
                  (key-place 1 2 items)
                  (key-place 2 2 items)))
               (key-place 3 0 (des-scooped 0))
               (key-place 3 1 (des-scooped 1))
              ;; (-# (->> (svg-import "../svg/FUNTUNFUNEFU-DENKYEMFUNEFU.svg")
              ;;      (scale [0.07 0.07])
              ;;      (extrude-linear {:height 2 :scale 0.8})
               
              ;;      (rdz 180)
              ;;      (rdx -85)
              ;;      (rdz -5)
              ;;      (translate (mapv + (:wall-locate3-point (calculate-control-points (key-wall-position lastcol 0 0 1 :tm)))
              ;;                       [0 -0.5 -7.1]))))
        
          
               key-holes
              ;(translate  (cylinder 0.1 4))
              ;; (difference
               (vnf-polyhedron (wall-vnf (fractyl-back-wall 10 10) {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :alt}))
              ;;             (usb-jack-place-new (fractyl-usb-c-port 60) :extra-z-rot -1.5)
              ;;             (rp2040-plus-place rp2040-plus-mount-body-clearance :place-fn (fn [shape] (usb-jack-place-new shape :extra-z-rot -1.5))))
               (tps-65-place tps-65-mount-new)
               (rp2040-plus-place rp2040-plus-mount :place-fn (fn [shape] (usb-jack-place-new shape :extra-z-rot -1.5)))

               (rp2040-plus-place rp2040-plus :place-fn (fn [shape] (usb-jack-place-new shape :extra-z-rot -1.5)))
      ;;     (rp2040-plus-place (->> (import "../parts/pico-r3.stl")
      ;;                             (rdx -90)
      ;;                             (rdz 180)
      ;;                             (translate [(/ rp2040-plus-width 2) (/ rp2040-plus-length -2)  (- rp2040-plus-mount-height rp2040-plus-thickness)])) :place-fn (fn [shape] (usb-jack-place-new shape :extra-z-rot -1.5)))
               (-# (vnf-polyhedron (:fractyl-right-wall-vnf  (fractyl-right-wall 10 10))))
               (back-wall-alignment-positions :peg)))))))
          
              ;(back-wall-alignment-positions :hole)
          
       

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
     :start-segment 1 :end-segment 3 
     :corner-preservaton :preserve
     :use-cross false)
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
                      :magnitude-estimation-method magnitude-estimation-method)]
        
         
       (ThumbWallSectionForASingleThumbRowData. wall-section [(fn [steps](decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-params) (:P outer-params) 8 9 steps))
                                                              (fn [steps](decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-params) (:P outer-params) 14 15 steps))]
                                                [(fn [steps](decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-params) (:P inner-params) 8 9 steps))
                                                 (fn [steps](decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-params) (:P inner-params) 14 15 steps))])))

(defn thumb-wall-section [wall-cross-section-steps wall-section-steps]
  ;(thumb-wall-section-for-single-thumb-row-fn wall-cross-section-steps wall-section-steps)
  (union
   (vnf-polyhedron (wall-vnf (thumb-tr-br-to-middle-lm-local-cubic-curve-interpolation-wall-section-fn wall-cross-section-steps wall-section-steps)
                             {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))))

;;    (vnf-polyhedron (wall-vnf (thumb-wall-section-for-single-thumb-row-fn wall-cross-section-steps wall-section-steps)
;;                              {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse true :style :default}))
   
(comment (spit "things-low/thumb-section-test.scad"
          (write-scad
           (include "../BOSL2/std.scad")
           (union
            ;thumb-type
            (thumb-wall-section 30 30)))))

(comment (let [hi false]
           (false? hi)))
(defn front-wall-nurbs [wall-cross-section-steps wall-section-steps]
  (let [weights [1 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 1];(vec (reverse [1 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 (/ (sqrt 2) 2) 1 1]))
        knot-vector (let [denom 10]
                      (mapv (partial * (dec denom)) [0 0 0 (/ 1 denom) (/ 2 denom) (/ 3 denom) (/ 5 denom) (/ 6 denom) (/ 7 denom) (/ 7.5 denom) (/ 9.0 denom)  1 1 1]))
        
        curve-paramater (nurbs-parameters 2 weights
                                          :knot-vector knot-vector
                                          :linear-outer-top false
                                          :linear-inner-top false) 
        wall-section-parameter (wall-section-parameter
                                [
                                 (wall-cross-section-parameter (key-wall-position 1 2 1 0 :br :xy 3 :slant :no-slant))
                                 
                                 (wall-cross-section-parameter (key-wall-position 2 2 1 -1 :bl  :xy 3 :slant :no-slant))
                                 (wall-cross-section-parameter (key-wall-position 2 2 -1 -1 :br :slant :no-slant :xy 4))
                                 (wall-cross-section-parameter (key-wall-position 3 2 -1 0 :bl :offset [0.000001 0 0] :slant :no-slant :xy 4))
                                 (wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :bl :slant :no-slant :xy 4.5))
                                 (wall-cross-section-parameter (key-wall-position 3 2 0 -1 :bl :offset [0 0.000001 0]))
                                 (wall-cross-section-parameter (key-wall-position 3 2 -1 -1 :br :slant :no-slant))
                                 (wall-cross-section-parameter (key-wall-position lastcol 2 -1 0 :bl :slant :no-slant :offset [0.000001 0 0]))
                                 (wall-cross-section-parameter (key-wall-position lastcol 2 -1 -1 :bl :slant :no-slant))
                                 (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :bl :slant :no-slant :offset [0 0.000001 0]))
                                 (wall-cross-section-parameter (key-wall-position lastcol 2 0 -1 :bm :slant :no-slant))] 
                                 
                                curve-paramater)
        front-wall-nurbs-wall-section (wall-section wall-section-parameter wall-cross-section-steps wall-section-steps)
        wall-positions (mapv #(:wall-position %) front-wall-nurbs-wall-section)
        wall-cross-section-control-points (mapv #(:all-control-points %) (:wall-cross-sections front-wall-nurbs-wall-section))
        upper-points (mapv #(:web-post-position-top %) wall-cross-section-control-points)
        lower-points (vec (reverse (mapv #(:web-post-position-bottom %) wall-cross-section-control-points))) 
        wall-cross-section-parameters (:wall-cross-section-parameters wall-section-parameter)
        wall-top (mapv #(nth % 0) (:outer-wall front-wall-nurbs-wall-section))
        inner-wall-top (mapv #(peek %) (:inner-wall front-wall-nurbs-wall-section))
        steps-distrubution (:steps-distrubution wall-section-parameter)
        curve-parameters (:curve-parameters wall-section-parameter)
        pinky-bl-to-fourth-bl-outer-steps (* wall-section-steps 5)
        pinky-bl-to-fourth-bl-outer-curve-decomp (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                                                  2 knot-vector (vec (reverse upper-points)) weights 1 4 pinky-bl-to-fourth-bl-outer-steps)
        pinky-bl-to-fourth-bl-inner-curve-decomp (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                                                  2 knot-vector lower-points weights 1 4 (* wall-section-steps 5)) 
        fourth-bl-to-middle-outer-curve (subvec wall-top (* (nth knot-vector 8) wall-section-steps) (+ (* (nth knot-vector 10) wall-section-steps) 3))
        fourth-bl-to-middle-inner-curve (subvec inner-wall-top (* (nth knot-vector 8) wall-section-steps) (+ (* (nth knot-vector 10) wall-section-steps) 3))
        fourth-bl-to-middle-outer-steps (dec (count fourth-bl-to-middle-outer-curve))
        fourth-bl-to-middle-to-index-steps (* wall-section-steps 5)
        fourth-bl-to-middle-to-index-outer-curve-decomp (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                                                         2 knot-vector (vec (reverse upper-points)) weights 5 7 fourth-bl-to-middle-to-index-steps)
        fourth-bl-to-middle-to-index-inner-curve-decomp (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                                                         2 knot-vector lower-points weights 5 7 fourth-bl-to-middle-to-index-steps) 
        middle-to-index-outer-steps (* wall-section-steps 3)
        middle-to-index-outer-curve-decomp (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                                            2 knot-vector (vec (reverse upper-points)) weights 8 8 middle-to-index-outer-steps)
        middle-to-index-inner-curve-decomp (decompose-non-homogoneus-nurbs-curve-and-calculate-bezier-curves
                                            2 knot-vector lower-points weights 8 8 middle-to-index-outer-steps)] 
       

           
       {:front-wall-wall-section front-wall-nurbs-wall-section
        :chained-hull-shapes [(chained-hull-to-points (plot-bezier-points pinky-bl-to-fourth-bl-outer-curve-decomp (sphere epsilon)) (translate (main-body-web-post-point-top 3 cornerrow :br) (sphere epsilon))
                                                      (plot-bezier-points pinky-bl-to-fourth-bl-inner-curve-decomp (sphere epsilon)) (translate (main-body-web-post-point-bottom 3 cornerrow :br) (sphere epsilon))
                                                      pinky-bl-to-fourth-bl-outer-steps)
                              (chained-hull-to-points (plot-bezier-points fourth-bl-to-middle-outer-curve (sphere epsilon)) (translate (main-body-web-post-point-top 2 cornerrow :br) (sphere epsilon))
                                                      (plot-bezier-points fourth-bl-to-middle-inner-curve (sphere epsilon)) (translate (main-body-web-post-point-bottom 2 cornerrow :br) (sphere epsilon))
                                                      fourth-bl-to-middle-outer-steps)
                              (chained-hull-to-points (plot-bezier-points middle-to-index-outer-curve-decomp (sphere epsilon)) (translate (main-body-web-post-point-top 2 cornerrow :bl) (sphere epsilon))
                                                      (plot-bezier-points middle-to-index-inner-curve-decomp (sphere epsilon)) (translate (main-body-web-post-point-bottom 2 cornerrow :bl) (sphere epsilon))
                                                       middle-to-index-outer-steps)]}))
             
         


(spit "things-low/tps-to-screen-side-local-outer-curve.scad"
      (write-scad
       (include include-bosl2)
       (let [steps 20
             left-section-left-mid-wall-position  (tps-65-to-screen-wall-position :lm :west)
             left-section-bottom-left-mid-south-west-wall-position (tps-65-to-screen-wall-position :bl-lm :south-west)
             ;left-section-bottom-left-west-wall-position (tps-65-to-screen-wall-position :bl :west :offset [-0.0001 0 0])
             left-section-bottom-left-south-west-wall-position (tps-65-to-screen-wall-position :bl :south-west)
             left-section-bottom-left-south-wall-position (tps-65-to-screen-wall-position :bl :south :offset [0 -0.0001 0])
             left-section-left-mid-control-points  (calculate-control-points left-section-left-mid-wall-position)
             left-section-bottom-left-mid-south-west-control-points (calculate-control-points left-section-bottom-left-mid-south-west-wall-position) 
             left-section-bottom-left-south-west-control-points (calculate-control-points left-section-bottom-left-south-west-wall-position)
             left-section-bottom-left-south-control-points (calculate-control-points left-section-bottom-left-south-wall-position)
             left-section-left-mid-outer (tps-to-screen-side-local-outer-curve left-section-left-mid-control-points steps)
             left-section-bottom-left-south-west-outer (tps-to-screen-side-local-outer-curve left-section-bottom-left-south-west-control-points steps) 
             left-section-bottom-left-south-outer (tps-to-screen-side-local-outer-curve  left-section-bottom-left-south-control-points steps)
             left-section-bottom-left-mid-south-west-outer (tps-to-screen-side-local-outer-curve left-section-bottom-left-mid-south-west-control-points steps)]
         (union 
          (tps-65-place tps-65-mount-new)
          (screen-holder-place-side screen-holder)
          (plot-bezier-points left-section-left-mid-outer (sphere 1))
          (plot-bezier-points left-section-bottom-left-south-west-outer (sphere 1))
          (plot-bezier-points left-section-bottom-left-south-outer (sphere 1))
          (plot-bezier-points left-section-bottom-left-mid-south-west-outer (sphere 1))))))



(defrecord LeftSectionData [vnf-array outer-wall inner-wall outer-floor-points  inner-floor-points trackpad-to-main-body-data
                            thumb-bl-to-tl-outer-curve-fn thumb-tl-to-tr-outer-curve-fn
                            thumb-bl-to-tl-inner-curve-fn thumb-tl-to-tr-inner-curve-fn])
(defn left-section-data [steps &{:keys [screen-outer-curve-type] :or {screen-outer-curve-type :global}}]
  (let [wall-section-steps steps
        steps-times-2 (* steps 2)
        screen-outer-curve-fn (case screen-outer-curve-type
                                :global tps-to-screen-side-global-with-first-derivatives-outer-curve
                                :local tps-to-screen-side-local-outer-curve)
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
        thumb-tr-centre-control-points (calculate-control-points (thumb-wall-position thumb-tr-place -1 -1 :centre :xy 4  :slant :no-slant))
        thumb-tr-bl-control-points (calculate-control-points (thumb-wall-position thumb-tr-place -1 -1 :bl :xy 4  :slant :no-slant))
        thumb-tr-br-control-points (calculate-control-points (thumb-wall-position thumb-tr-place 1 0 :br :xy 4  :slant :no-slant))
        thumb-tr-rm-control-points (calculate-control-points (thumb-wall-position thumb-tr-place 1 0 :rm :xy 4  :slant :no-slant))
        thumb-tr-tr-control-points (calculate-control-points (thumb-wall-position thumb-tr-place 1 -1 :tr :xy 4 :slant :parallel-by-d-opposite))
        thumb-tr-tr-control-points-offset (calculate-control-points (thumb-wall-position thumb-tr-place 1 -1 :tr :xy 4 :slant :parallel-by-d-opposite :offset [-1 -1 0]))
        index-bottom-br (calculate-control-points (key-wall-position 1 2 1 0 :br :xy 4 :slant :no-slant))
        index-bottom-tr (calculate-control-points (key-wall-position 1 2 -1 1 :br :xy 4 :slant :no-slant))
        index-bottom-br-offset (calculate-control-points (key-wall-position 1 2 1 0 :br :xy 4 :slant :no-slant :offset [1 0 0]))
        index-tr (calculate-control-points (key-wall-position 1 2 1 0 :tr :xy 4 :slant :no-slant))
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
        left-section-bottom-left-mid-south-west-outer (screen-outer-curve-fn left-section-bottom-left-mid-south-west-control-points steps-times-2)

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
                                                                               steps-times-2
                                                                               :alpha-type :chordal)
        left-section-top-right-north-inner  (n-degree-bezier-curve (get-curve-control-points-by-key-words left-section-top-right-control-points inner-wall-curve-bezier-cubic-keywords) steps-times-2)
        left-section-top-mid-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words left-section-top-mid-control-points inner-wall-curve-bezier-cubic-keywords) steps-times-2)
        left-section-top-left-north-inner  (n-degree-bezier-curve (get-curve-control-points-by-key-words left-section-top-left-north-control-points inner-wall-curve-bezier-cubic-keywords) steps-times-2)
        left-section-top-left-west-inner  (n-degree-bezier-curve (get-curve-control-points-by-key-words left-section-top-left-west-control-points inner-wall-curve-bezier-quadratic-keywords) steps-times-2)
        left-section-top-left-mid-west-inner  (n-degree-bezier-curve (get-curve-control-points-by-key-words left-section-top-left-mid-control-points inner-wall-curve-bezier-cubic-keywords) steps-times-2)
        left-section-left-mid-outer (screen-outer-curve-fn left-section-left-mid-control-points steps-times-2)
        left-section-left-mid-inner (tps-to-screen-side-global-with-first-derivatives-inner-curve left-section-left-mid-control-points steps-times-2)
        left-section-bottom-left-mid-south-west-inner (tps-to-screen-side-global-with-first-derivatives-inner-curve left-section-bottom-left-mid-south-west-control-points steps-times-2)
        left-section-bottom-left-west-outer (catmull-rom-spline-curve (get-curve-control-points-by-key-words  left-section-bottom-left-west-control-points tps-65-to-screen-outer-wall-catmull-rom-spline-keywords) steps-times-2)
             ;left-section-bottom-left-west-inner (apply tps-to-screen-side-catmull (conj (vec (get-curve-control-points-by-key-words  left-section-bottom-left-west-control-points inner-wall-catmull-rom-spline-parameters)) steps-times-2))
        left-section-bottom-mid-south-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words left-section-bottom-mid-south-control-points inner-wall-curve-bezier-cubic-keywords) steps-times-2)
        left-section-bottom-right-south-inner-catmull (catmull-rom-spline-curve [;(:opposite-web-post-position-top left-section-bottom-right-south-control-points)
                                                                                 (mapv + [0 -8 10] (:web-post-position-bottom left-section-bottom-right-south-control-points))
                                                                                 ;(mapv + [0 0 10](:web-post-position-bottom left-section-bottom-right-south-control-points))
                                                                                 ;(mapv + [0 0 (- (/ web-thickness 2) (+ tps-65-y-modifier tps-65-depth tps-65-depth-tolerance))] (:web-post-position-bottom left-section-bottom-right-south-control-points)) 

                                                                                 (mapv + [0 0.75 (- (/ web-thickness 2) (+ tps-65-y-modifier tps-65-depth tps-65-depth-tolerance))] (find-point-on-line-using-x tps-65-top-left-inner tps-65-bottom-left-inner (+ (nth tps-65-bottom-left-inner 0) -1)))
                                                                                 (:web-post-position-bottom thumb-bl-tr-points)
                                                                                 (web-post-point-top thumb-bl-place :bl :degrees)]

                                                                                 ;(:wall-locate-2-bottom-floor thumb-bl-br-points)
                                                                                 
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
        left-section-bottom-left-south-west-outer (screen-outer-curve-fn left-section-bottom-left-south-west-control-points steps-times-2)
        left-section-bottom-left-south-west-inner (tps-to-screen-side-global-with-first-derivatives-inner-curve left-section-bottom-left-south-west-control-points steps-times-2)
        left-section-bottom-left-south-outer (screen-outer-curve-fn  left-section-bottom-left-south-control-points steps-times-2)
        left-section-bottom-left-south-inner (catmull-rom-spline-curve  (get-curve-control-points-by-key-words  left-section-bottom-left-south-control-points tps-65-to-screen-inner-wall-catmull-rom-spline-parameters) steps-times-2)
        thumb-bl-tl-points-outer  (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tl-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                     3 default-weights-for-vertical-nurbs steps-times-2)
        thumb-bl-tl-points-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tl-points inner-wall-curve-bezier-cubic-keywords) steps-times-2)


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
        end-outer-magnitude-estimation 22
      ;;   tt (non-uniform-b-spline (:P end-curve-params) 3 (:U end-curve-params) steps-times-2)
      ;;   tt-part (non-uniform-b-spline (:P end-curve-params) 3 (:U end-curve-params) steps-times-2
      ;;                                 :u-start (nth (:U end-curve-params) 7) :u-end (nth (:U end-curve-params) 9))
        end-curve-outer  (let [params (global-curve-interp-with-calculated-first-derivatives
                                       [(thumb-web-post-point-top thumb-tr-place :rm)
                                        (thumb-web-post-point-top thumb-tr-place :tr)
                                        (main-body-web-post-point-top 1 2 :br)]
                                       [(mapv - (thumb-web-post-point-top thumb-tr-place :rm) (thumb-web-post-point-top thumb-tr-place :br))
                                        (mapv - (thumb-web-post-point-top thumb-tr-place :tr) (thumb-web-post-point-top thumb-tr-place :rm))
                                        (mapv - (main-body-web-post-point-top 2 2 :bl) (main-body-web-post-point-top 1 2 :br))]
                                       2 
                                       :point-paramater-calculation-method :dynamic-centripetal
                                       :magnitude-estimation-method end-outer-magnitude-estimation)]
                         
                           (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U params) (:P params)
                                                                                 2 3 steps-times-2 
                                                                                 :reverse-curve true))
                                                                                 
                           
      ;;   (vec (reverse (local-cubic-curve-interpolation-with-tangents-curve [(:web-post-position-top thumb-tr-tr-control-points)
      ;;                                                                                       (:web-post-position-top index-bottom-br)]
      ;;                                                                                      (subvec  outer-end-curve-tangents 2 4)
      ;;                                                                                      steps-times-2)))
        end-tangents-curve-outer  
        (let [params (global-curve-interp-with-calculated-first-derivatives 
                                  [(:point-on-tangent-from-plate thumb-tr-rm-control-points)
                                   (:point-on-tangent-from-plate thumb-tr-tr-control-points)
                                   (:point-on-tangent-from-plate index-bottom-br)]
                                  [(mapv - (:point-on-tangent-from-plate thumb-tr-rm-control-points) (:point-on-tangent-from-plate thumb-tr-br-control-points))
                                   (mapv - (:point-on-tangent-from-plate thumb-tr-tr-control-points) (:point-on-tangent-from-plate thumb-tr-rm-control-points))
                                   (mapv - (:point-on-tangent-from-plate middle-bottom-bl) (:point-on-tangent-from-plate index-bottom-br))]
                                  2 
                                  :point-paramater-calculation-method :dynamic-centripetal
                                  :magnitude-estimation-method end-outer-magnitude-estimation)]
             (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U params) (:P params)
                                                                   2 3 steps-times-2 
                                                                   :reverse-curve true))
                                     
      ;;   (vec (reverse (local-cubic-curve-interpolation-with-tangents-curve [(:point-on-tangent-from-plate thumb-tr-tr-control-points)
      ;;                                                                                                (:point-on-tangent-from-plate index-bottom-br)]
      ;;                                                                                               (subvec  end-tangent-curve-tangents 2 4)
      ;;                                                                                               steps-times-2)))
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
        end-inner-magnitude-estimation 10
        end-inner-tangent-point :wall-locate-2-top
        end-inner-point-paramater-calculation-method :equal
        end-inner-fn (fn [point](let [degree 2
                                      params (global-curve-interp-with-end-derivatives-calculated
                                              [(point index-bottom-br)
                                               (point thumb-tr-tr-control-points)]
                                               ;(point thumb-tr-rm-control-points)
                                               ;(point thumb-tr-br-control-points)
                                               
                                              degree
                                              (mapv - (point index-bottom-br ) (point middle-bottom-bl))
                                              (mapv -  (point thumb-tr-rm-control-points) (point thumb-tr-tr-control-points))
                                              ;(mapv - (point thumb-tr-br-control-points) (point thumb-tr-rm-control-points) )
                                              ;(mapv - (point thumb-tr-br-control-points) (point thumb-tr-rm-control-points))
                                              :point-paramater-calculation-method :chordal
                                              :magnitude-estimation-method 1)]
                                              
                                  (decompose-b-spline-curve-and-calculate-bezier-curves degree (:U params) (:P params)
                                                                                        0 1 steps-times-2)))
         index-br-control-points  (calculate-control-points (key-wall-position 1 2 1 0 :br :xy 3 :slant :no-slant))
        index-br-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words index-br-control-points inner-wall-curve-bezier-cubic-keywords) steps) thumb-tr-tr-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-tr-tr-control-points inner-wall-curve-bezier-cubic-keywords) steps)
        thumb-tr-rm-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-tr-rm-control-points inner-wall-curve-bezier-cubic-keywords) steps)
        middle-bl-control-points  (calculate-control-points (key-wall-position 2 2 1 -1 :bl  :xy 3 :slant :no-slant))
        middle-bl-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words middle-bl-control-points inner-wall-curve-bezier-cubic-keywords)
                                               steps)
        thumb-tr-br-control-points (calculate-control-points (thumb-wall-position thumb-tr-place 1 0 :br :xy 4  :slant :no-slant))
        thumb-tr-br-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-tr-br-control-points inner-wall-curve-bezier-cubic-keywords) steps)
        index 0 
        inner-params  (global-curve-interp-with-calculated-first-derivatives
                       [(nth index-br-inner index)
                        (nth thumb-tr-tr-inner index)
                        (nth thumb-tr-rm-inner index)]
                       [(mapv -   (nth index-br-inner index) (nth middle-bl-inner index))
                        (mapv -  (nth thumb-tr-rm-inner index) (nth thumb-tr-tr-inner index))
                        (mapv -  (nth thumb-tr-br-inner index) (nth thumb-tr-rm-inner index))]
                       2
                       :point-paramater-calculation-method :equal
                                                                                                ;:knot-vector-calculation-method :equal
                       :magnitude-estimation-method 1)
                                  
                                  
        end-curve-inner (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-params) (:P inner-params) 0 1 steps-times-2)
        ;(end-inner-fn :web-post-position-bottom)
      ;;   (let [params (global-curve-interp-with-calculated-first-derivatives
      ;;                                 [(:web-post-position-bottom index-bottom-br)
      ;;                                  (:web-post-position-bottom thumb-tr-tr-control-points)
      ;;                                  (:web-post-position-bottom thumb-tr-centre-control-points)]
      ;;                                 [(mapv - (:web-post-position-bottom index-bottom-br) (:web-post-position-bottom index-tr))
      ;;                                  (mapv - (:web-post-position-bottom thumb-tr-centre-control-points) (:web-post-position-bottom thumb-tr-tr-control-points))
      ;;                                  (mapv - (:web-post-position-bottom thumb-tr-bl-control-points) (:web-post-position-bottom thumb-tr-centre-control-points))]
      ;;                                 2
      ;;                                 :point-paramater-calculation-method end-inner-point-paramater-calculation-method
      ;;                                 :magnitude-estimation-method end-inner-magnitude-estimation)]
      ;;                     (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U params) (:P params)
      ;;                                                                           0 1 steps-times-2)
      ;;                     )  
      ;;   (local-cubic-curve-interpolation-with-tangents-curve [(:web-post-position-bottom index-bottom-br)
      ;;                                                                              ;              (:web-post-position-bottom thumb-tr-tr-control-points)
      ;;                                                                         (web-post-point-bottom thumb-tr-place :tr :degrees
      ;;                                                                                                :offset [-1 -1 0])]
      ;;                                                                        (subvec  inner-end-curve-tangents 1 3)
      ;;                                                                        steps-times-2)
        end-tangents-inner-index steps
        end-tangents-inner-params  (global-curve-interp-with-calculated-first-derivatives
                                                 [(nth index-br-inner end-tangents-inner-index )
                                                  (nth thumb-tr-tr-inner end-tangents-inner-index)
                                                  (nth thumb-tr-rm-inner end-tangents-inner-index)]
                                                 [(mapv -   (nth index-br-inner end-tangents-inner-index) (nth middle-bl-inner end-tangents-inner-index))
                                                  (mapv -  (nth thumb-tr-rm-inner end-tangents-inner-index) (nth thumb-tr-tr-inner end-tangents-inner-index))
                                                  (mapv -  (nth thumb-tr-br-inner end-tangents-inner-index) (nth thumb-tr-rm-inner end-tangents-inner-index))]
                                                 2
                                                 :point-paramater-calculation-method :equal
                                                                                                                                  ;:knot-vector-calculation-method :equal
                                                 :magnitude-estimation-method 1)
        end-tangents-curve-inner (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U end-tangents-inner-params) (:P end-tangents-inner-params) 0 1 20)
     ;;    end-tangents-curve-inner  (local-cubic-curve-interpolation-with-tangents-curve [(:point-on-tangent-from-plate-bottom index-bottom-br)
     ;;                                                                                    (:point-on-tangent-from-plate-bottom thumb-tr-tr-control-points)]
     ;;                                                                                   (subvec  inner-tangent-end-curve-tangents 1 3)
     ;;                                                                                   steps-times-2)
        end-tangents-curve-inner2 (end-inner-fn :point-on-tangent-from-plate-bottom) 
      ;;   (let [params (global-curve-interp-with-calculated-first-derivatives
      ;;                                           [(end-inner-tangent-point index-bottom-br)
      ;;                                            (end-inner-tangent-point thumb-tr-tr-control-points)
      ;;                                            (end-inner-tangent-point thumb-tr-centre-control-points)]
      ;;                                           [(mapv - (end-inner-tangent-point index-bottom-br) (end-inner-tangent-point index-tr))
      ;;                                            (mapv - (end-inner-tangent-point thumb-tr-centre-control-points) (end-inner-tangent-point thumb-tr-tr-control-points))
      ;;                                            (mapv - (end-inner-tangent-point thumb-tr-bl-control-points) (end-inner-tangent-point thumb-tr-centre-control-points))]
      ;;                                           2
      ;;                                           :point-paramater-calculation-method end-inner-point-paramater-calculation-method
      ;;                                           :magnitude-estimation-method end-inner-magnitude-estimation)]
      ;;                               (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U params) (:P params)
      ;;                                                                                     0 1 steps-times-2))  
      ;;   (local-cubic-curve-interpolation-with-tangents-curve [(:wall-locate-2-top index-bottom-br)
      ;;                                                                                    (:wall-locate-2-top thumb-tr-tr-control-points)]
      ;;                                                                                   (subvec  inner-tangent-end-curve-tangents2 1 3)
      ;;                                                                                   steps-times-2)
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
                                           (mapv - (nth end-curve-outer index) (nth index-bottom-bl-to-thumb-tr-tl-catmull-outer index))
                                           ;(mapv - (nth end-tangents-curve-outer index) (nth end-curve-outer index))
                                           ])             outer-wall-f-point-paramater-calculation-method :chordal
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
        thumb-top-outer-points-fn (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-wall-top-outer-params)
                                                                                                    (:P outer-wall-top-outer-params)
                                                                                                    18 19 steps :reverse-curve true))
        thumb-top-inner-points-fn (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-wall-top-params)
                                                                                                (:P inner-wall-top-params)
                                                                                                10 11 steps :reverse-curve true))
        trackpad-to-main-body-data (trackpad-to-main-body steps :thumb-side-border-curve-fn-outer (partial outer-wall-trackpad-to-keys-gap-border-outer-fn)
                                                          :thumb-side-border-tangent-curve-fn-outer outer-wall-trackpad-to-keys-gap-border-tangent-outer-fn
                                                          :thumb-side-border-curve-fn-inner inner-wall-trackpad-to-keys-gap-border-outer-fn
                                                          :thumb-side-border-tangent-curve-fn-inner inner-wall-trackpad-to-keys-gap-border-tangent-outer-fn)
        vnf-array (wall-vnf-array outer-wall inner-wall
                                  {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :quincunx})]
          
    {:left-section-data (LeftSectionData. vnf-array outer-wall inner-wall  (filter #(zero? (nth % 2))(peek outer-wall)) (filter #(zero? (nth % 2)) (peek inner-wall))   trackpad-to-main-body-data
                                          outer-wall-trackpad-to-keys-gap-border-tangent-outer-fn thumb-tl-to-tr-outer-curve-fn
                                          inner-wall-trackpad-to-keys-gap-border-tangent-outer-fn thumb-tl-to-tr-inner-curve-fn)
     :thumb-outer-points-fn thumb-outer-points-fn
     :thumb-inner-points-fn thumb-inner-points-fn
     :thumb-top-outer-points-fn thumb-top-outer-points-fn
     :thumb-top-inner-points-fn thumb-top-inner-points-fn
     :inner-index-to-index-connector-outer-curve-fn (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-wall-top-outer-params)
                                                                        (:P outer-wall-top-outer-params)
                                                                        26 27 steps))
     :inner-index-to-index-connector-inner-curve-fn (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-wall-top-params)
                                                                                                     (:P inner-wall-top-params)
                                                                                                     2 3 steps :reverse-curve true))
     :outer-wall-parameters outer-wall-f-params
     :inner-wall-parameters inner-wall-f-params}))

(defn thumb-to-left-section [wall-cross-section-steps wall-section-steps left-section-to-thumb-outer-points-fn left-section-to-thumb-inner-points-fn
                             thumb-top-outer-points-fn thumb-top-inner-points-fn]
  (let [thumb-outer-points (left-section-to-thumb-outer-points-fn wall-cross-section-steps)
        thumb-inner-points (vec (reverse (left-section-to-thumb-inner-points-fn wall-cross-section-steps)))
        thumb-top-outer-points (thumb-top-outer-points-fn wall-cross-section-steps)
        thumb-top-inner-points (vec (reverse (thumb-top-inner-points-fn wall-cross-section-steps)))
        thumb-bl-tm-north-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tm :xy 3 :slant :no-slant))
        thumb-bl-tm-north-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tm-north-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                         3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-bl-tm-north-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 1 :tm :xy 4 :slant :no-slant))
        thumb-bl-tm-north-west-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tm-north-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                              3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-bl-tm-north-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tm-north-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-bl-tr-north-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tr :xy 3 :slant :no-slant))
        left-section-bottom-mid-south-wall-position (tps-65-wall-position :bm :south)
        left-secion-bottom-mid-south-control-points (calculate-control-points left-section-bottom-mid-south-wall-position)
        left-secion-bottom-mid-south-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words left-secion-bottom-mid-south-control-points wall-vertical-outer-nurbs-control-points-keywords) 3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-bl-tr-tm-north-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tr-tm :xy 4 :slant :no-slant))
        thumb-tl-tl-north-control-points (calculate-control-points (thumb-wall-position thumb-tl-place 0 1 :tl :xy 4))
        thumb-bl-tr-north-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tr :xy 4 :offset [0.00000001 0.0 0.0]  :slant :no-slant))
        thumb-bl-bl-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 0 :bl :xy 4 :slant :no-slant :offset [-0.000001 0.0 0.0]))
        thumb-bl-lm-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 0 :lm :xy 4 :slant :no-slant))
        thumb-bl-tl-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 0 :tl :xy 4 :offset [(- epsilon) 0 0] :slant :no-slant))
        thumb-bl-tl-north-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 1 :tl :xy 4 :slant :no-slant))
        thumb-bl-tl-north-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tl :xy 4 :offset [0 epsilon 0] :slant :no-slant))
        thumb-bl-bl-west-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-bl-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                        3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-bl-lm-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-lm-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                   3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-bl-tl-west-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tl-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                        3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-bl-tl-north-west-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tl-north-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                              3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-bl-tl-north-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tl-north-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                         3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-bl-tr-tm-north-west-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tr-tm-north-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                                 3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-tl-tl-north-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-tl-tl-north-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-bl-tr-tm-north-west-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tr-tm-north-west-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-bl-tr-north-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tr-north-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-bl-bl-west-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-bl-west-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-bl-lm-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-lm-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-bl-tl-west-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tl-west-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-bl-tl-north-west-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tl-north-west-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-bl-tl-north-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tl-north-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-bl-tm-north-west-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tm-north-west-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        section-steps (* wall-section-steps 6)
        outer-thumb-wall (vec (for [index (range (inc wall-cross-section-steps))]
                                (global-curve-interp-with-calculated-first-derivatives-curve
                                 [;(nth thumb-inner-points  index)
                                  (nth thumb-outer-points  index)
                                  (nth thumb-bl-tm-north-outer-curve  index)
                                  (nth  thumb-bl-tl-north-outer-curve index)
                                  (nth  thumb-bl-tl-west-outer-curve index)
                                  (nth  thumb-bl-lm-outer-curve index)]
                                 [;(mapv - (nth thumb-outer-points  index) (nth thumb-inner-points index))
                                  (mapv - (nth thumb-outer-points  index) (nth thumb-top-outer-points index))
                                       ;(mapv - (:wall-locate3-point-floor left-section-bottom-mid-south-control-points) (:wall-locate3-point left-section-bottom-mid-south-control-points))
                                  (mapv - (nth  thumb-bl-tm-north-outer-curve index) (nth thumb-outer-points  index))
                                  (mapv -  (nth  thumb-bl-tl-north-west-outer-curve index) (nth  thumb-bl-tm-north-outer-curve index))
                                  (mapv - (nth  thumb-bl-lm-outer-curve index) (nth  thumb-bl-tl-north-west-outer-curve index))
                                  (mapv - (nth thumb-bl-bl-west-outer-curve index) (nth  thumb-bl-lm-outer-curve index))]
                                 2
                                 section-steps
                                 :point-paramater-calculation-method :chordal
                                 :magnitude-estimation-method :farin-simple)))
        inner-thumb-wall (vec (for [index (range (inc wall-cross-section-steps))]
                                (global-curve-interp-with-calculated-first-derivatives-curve
                                 [(nth thumb-bl-lm-inner-curve index)
                                  (nth thumb-bl-tl-west-inner-curve index)
                                  (nth thumb-bl-tl-north-inner-curve index)
                                  (nth thumb-bl-tm-north-west-inner-curve index)
                                  (nth thumb-bl-tr-tm-north-west-inner-curve index)
                                  ;(nth thumb-bl-tr-north-inner-curve index)
                                  (nth thumb-inner-points index)]
                                  
                                 [(mapv - (nth thumb-bl-lm-inner-curve index) (nth thumb-bl-bl-west-inner-curve index))
                                  (mapv - (nth thumb-bl-tl-west-inner-curve index) (nth thumb-bl-lm-inner-curve index))
                                  (mapv - (nth thumb-bl-tm-north-west-inner-curve index) (nth thumb-bl-tl-north-west-inner-curve index))
                                  (mapv - (nth thumb-bl-tr-tm-north-west-inner-curve index) (nth thumb-bl-tm-north-west-inner-curve index))
                                  (mapv - (nth thumb-bl-tr-north-inner-curve index) (nth thumb-bl-tr-tm-north-west-inner-curve index))
                                  (mapv - (nth thumb-inner-points index) (nth thumb-bl-tr-north-inner-curve index))]
                                 2
                                 section-steps)))
        local-outer-thumb-wall (vec (for [index (range (inc wall-cross-section-steps))]
                                      (local-cubic-curve-interpolation-with-tangents-curve
                                       [;(nth thumb-inner-points  index)
                                        (nth thumb-outer-points  index)
                                        (nth thumb-bl-tm-north-outer-curve  index)
                                        (nth  thumb-bl-tl-north-outer-curve index)
                                        (nth  thumb-bl-tl-west-outer-curve index)
                                        (nth  thumb-bl-lm-outer-curve index)]
                                       [;(mapv - (nth thumb-outer-points  index) (nth thumb-inner-points index))
                                        (mapv - (nth thumb-outer-points  index) (nth thumb-top-outer-points index))
                                                                              ;(mapv - (:wall-locate3-point-floor left-section-bottom-mid-south-control-points) (:wall-locate3-point left-section-bottom-mid-south-control-points))
                                        (mapv - (nth  thumb-bl-tm-north-outer-curve index) (nth thumb-outer-points  index))
                                        (mapv -  (nth  thumb-bl-tl-north-west-outer-curve index) (nth  thumb-bl-tm-north-outer-curve index))
                                        (mapv - (nth  thumb-bl-lm-outer-curve index) (nth  thumb-bl-tl-north-west-outer-curve index))
                                        (mapv - (nth thumb-bl-bl-west-outer-curve index) (nth  thumb-bl-lm-outer-curve index))]
                                       (* section-steps 2))))]
    {:wall-vnf (walls-to-vnf [outer-thumb-wall inner-thumb-wall] :args {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}) 
               :outer-floor-points (peek local-outer-thumb-wall) :inner-thumb-points (peek inner-thumb-wall)}))
               
    

(defn thumb-to-left-section-2 [wall-cross-section-steps wall-section-steps  left-section-to-thumb-outer-points-fn left-section-to-thumb-inner-points-fn] 
  (let [thumb-outer-points (left-section-to-thumb-outer-points-fn wall-cross-section-steps)
        thumb-inner-points (vec (reverse (left-section-to-thumb-inner-points-fn wall-cross-section-steps)))
        thumb-bl-tm-north-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tm :xy 5 :slant :parallel-by-d-opposite))
        thumb-bl-tm-north-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tm-north-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                         3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-bl-tm-north-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 1 :tm :xy 4 :slant :no-slant))
        thumb-bl-tr-north-offset-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0.5 1 :tr :xy 5 :slant :no-slant :offset [0.5 0 0]))
        left-section-bottom-mid-south-wall-position (tps-65-wall-position :bm :south)
        left-section-bottom-mid-south-control-points (calculate-control-points left-section-bottom-mid-south-wall-position)
        thumb-bl-tr-tm-north-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tr-tm :xy 4 :slant :no-slant))
        thumb-bl-tr-tm-north-west-offest-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0.25 1 :tr-tm :xy 4 :slant :no-slant :offset [1 0 0]))
        thumb-tl-tl-north-control-points (calculate-control-points (thumb-wall-position thumb-tl-place 0 1 :tl :xy 4))
        thumb-bl-tr-north-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tr :xy 4 :offset [0.00000001 0.0 0.0]  :slant :no-slant))
        thumb-bl-bl-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 0 :bl :xy 4 :slant :no-slant :offset [-0.000001 0.0 0.0]))
        thumb-bl-lm-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 0 :lm :xy 4 :slant :no-slant))
        thumb-bl-tl-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 0 :tl :xy 4 :offset [(- epsilon) 0 0] :slant :no-slant))
        thumb-bl-tl-north-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 1 :tl :xy 4 :slant :no-slant))
        thumb-bl-tl-north-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tl :xy 4 :offset [0 epsilon 0] :slant :no-slant))
        thumb-bl-tr-north-offset-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0.5 1 :tr :xy 5 :slant :no-slant :offset [0.5 0 0]))
        left-section-bottom-mid-south-wall-position (tps-65-wall-position :bm :south)
        left-section-bottom-mid-south-control-points (calculate-control-points left-section-bottom-mid-south-wall-position)
        thumb-bl-tr-tm-north-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tr-tm :xy 4 :slant :no-slant))
        thumb-bl-tr-tm-north-west-offest-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0.25 1 :tr-tm :xy 4 :slant :no-slant :offset [1 0 0]))
        thumb-tl-tl-north-control-points (calculate-control-points (thumb-wall-position thumb-tl-place 0 1 :tl :xy 4))
        thumb-bl-tr-north-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tr :xy 4 :offset [0.00000001 0.0 0.0]  :slant :no-slant))
        thumb-bl-bl-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 0 :bl :xy 4 :slant :no-slant :offset [-0.000001 0.0 0.0]))
        thumb-bl-lm-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 0 :lm :xy 4 :slant :no-slant))
        thumb-bl-tl-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 0 :tl :xy 4 :offset [(- epsilon) 0 0] :slant :no-slant))
        thumb-bl-tl-north-west-control-points (calculate-control-points (thumb-wall-position thumb-bl-place -1 1 :tl :xy 4 :slant :no-slant))
        thumb-bl-tl-north-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tl :xy 4 :offset [0 epsilon 0] :slant :no-slant))
        thumb-bl-bl-west-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-bl-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                        3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-bl-lm-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-lm-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                   3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-bl-tl-west-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tl-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                        3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-bl-tl-north-west-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tl-north-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                              3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-bl-tl-north-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tl-north-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                         3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-bl-tr-north-offset-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tr-north-offset-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                                3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-bl-tr-tm-north-west-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tr-tm-north-west-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                                 3 default-weights-for-vertical-nurbs wall-cross-section-steps)

        {thumb-bl-tr-tm-north-west-offset-web-post-position-bottom :web-post-position-bottom
         thumb-bl-tr-tm-north-west-offset-wall-locate-2-top :wall-locate-2-top
         thumb-bl-tr-tm-north-west-offset-wall-locate-2-bottom :wall-locate-2-bottom
         thumb-bl-tr-tm-north-west-offset-wall-locate-2-bottom-floor :wall-locate-2-bottom-floor} thumb-bl-tr-tm-north-west-offest-control-points
        thumb-bl-tr-tm-north-west-offest-inner-curve (n-degree-bezier-curve [thumb-bl-tr-tm-north-west-offset-web-post-position-bottom
                                                                             (mapv + [0 0 -4] thumb-bl-tr-tm-north-west-offset-wall-locate-2-top)
                                                                             (mapv + [0.5 0.5 -6] thumb-bl-tr-tm-north-west-offset-wall-locate-2-bottom)
                                                                             (mapv + [-5 4 0] thumb-bl-tr-tm-north-west-offset-wall-locate-2-bottom-floor)] wall-cross-section-steps)
        thumb-bl-tr-north-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tr-north-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-bl-bl-west-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-bl-west-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-bl-lm-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-lm-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-bl-tl-west-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tl-west-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-bl-tl-north-west-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tl-north-west-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-bl-tl-north-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tl-north-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-bl-tm-north-west-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tm-north-west-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        total-wall-section-steps (* wall-section-steps  4)
        global-outer-thumb-wall-params (vec (for [index (range (inc wall-cross-section-steps))]
                                             (global-curve-interp-with-calculated-first-derivatives
                                              [;(nth thumb-bl-tr-tm-north-west-inner-curve index)
                                               (nth thumb-inner-points index)
                                               (nth thumb-outer-points  index)
                                               (nth thumb-bl-tm-north-outer-curve  index)
                                               (nth  thumb-bl-tl-north-outer-curve index)
                                               (nth  thumb-bl-tl-west-outer-curve index)
                                               (nth  thumb-bl-lm-outer-curve index)]
                                              [(mapv - (nth thumb-inner-points index) (nth thumb-bl-tr-tm-north-west-offest-inner-curve index))
                                               (mapv - (nth  thumb-bl-tm-north-outer-curve index) (nth thumb-outer-points  index))
                                               (mapv - (nth  thumb-bl-tl-north-outer-curve index) (nth thumb-bl-tm-north-outer-curve  index))
                                               (mapv - (nth  thumb-bl-tl-north-west-outer-curve index) (nth  thumb-bl-tm-north-outer-curve index))
                                               (mapv - (nth  thumb-bl-lm-outer-curve index) (nth  thumb-bl-tl-west-outer-curve index))
                                               (mapv - (nth thumb-bl-bl-west-outer-curve index) (nth  thumb-bl-lm-outer-curve index))]
                                              2))) 
        global-outer-thumb-wall (vec (for [index (range (inc wall-cross-section-steps))
                                           :let [param (nth global-outer-thumb-wall-params index)]]
                                       (non-uniform-b-spline (:P param) 2 (:U param) total-wall-section-steps))) 
                                       
      ;;   (vec (for [index (range (inc wall-cross-section-steps))]
      ;;          (global-curve-interp-with-calculated-first-derivatives-curve
      ;;           [;(nth thumb-bl-tr-tm-north-west-inner-curve index)
      ;;            (nth thumb-inner-points index)
      ;;            (nth thumb-outer-points  index)
      ;;            (nth thumb-bl-tm-north-outer-curve  index)
      ;;            (nth  thumb-bl-tl-north-outer-curve index)
      ;;            (nth  thumb-bl-tl-west-outer-curve index)
      ;;            (nth  thumb-bl-lm-outer-curve index)]
      ;;           [(mapv - (nth thumb-inner-points index) (nth thumb-bl-tr-tm-north-west-offest-inner-curve index))
      ;;            (mapv - (nth  thumb-bl-tm-north-outer-curve index) (nth thumb-outer-points  index))
      ;;            (mapv - (nth  thumb-bl-tl-north-outer-curve index) (nth thumb-bl-tm-north-outer-curve  index))
      ;;            (mapv - (nth  thumb-bl-tl-north-west-outer-curve index) (nth  thumb-bl-tm-north-outer-curve index))
      ;;            (mapv - (nth  thumb-bl-lm-outer-curve index) (nth  thumb-bl-tl-west-outer-curve index))
      ;;            (mapv - (nth thumb-bl-bl-west-outer-curve index) (nth  thumb-bl-lm-outer-curve index))]
      ;;           2
      ;;           total-wall-section-steps)))
        inner-thumb-wall (vec (for [index (range (inc wall-cross-section-steps))]
                                (global-curve-interp-with-calculated-first-derivatives-curve
                                 [(nth thumb-bl-lm-inner-curve index)
                                  (nth thumb-bl-tl-west-inner-curve index)
                                  (nth thumb-bl-tl-north-inner-curve index)
                                  (nth thumb-bl-tm-north-west-inner-curve index)
                                       ;(nth thumb-bl-tr-tm-north-west-inner-curve index)
                                  (nth thumb-bl-tr-tm-north-west-offest-inner-curve index)
                                  (nth thumb-inner-points index)]
                                       ;(nth thumb-inner-points index)
                                  
                                 [(mapv - (nth thumb-bl-lm-inner-curve index) (nth thumb-bl-bl-west-inner-curve index))
                                  (mapv - (nth thumb-bl-tl-west-inner-curve index) (nth thumb-bl-lm-inner-curve index))
                                  (mapv - (nth thumb-bl-tm-north-west-inner-curve index) (nth thumb-bl-tl-north-west-inner-curve index))
                                       ;(mapv - (nth thumb-bl-tr-tm-north-west-inner-curve index) (nth thumb-bl-tm-north-west-inner-curve index))
                                  (mapv - (nth thumb-bl-tr-tm-north-west-offest-inner-curve index) (nth thumb-bl-tm-north-west-inner-curve index))
                                  (mapv - (nth thumb-inner-points index) (nth thumb-bl-tr-tm-north-west-offest-inner-curve index))
                                  (mapv - (nth thumb-outer-points index) (nth thumb-inner-points index))]
                                 2
                                 total-wall-section-steps
                                 :point-paramater-calculation-method :dynamic-centripetal
                                 :magnitude-estimation-method :chord)))
                         outer-floor-points-params (nth global-outer-thumb-wall-params  wall-cross-section-steps)] 
       {:wall-vnf (wall-vnf-array global-outer-thumb-wall inner-thumb-wall :args {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default})
        :outer-floor-points (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-floor-points-params) (:P outer-floor-points-params) 2 9 total-wall-section-steps) :inner-floor-points (peek inner-thumb-wall)}))
                                 
  (defn thumb-tr-rm-to-index-br-to-left-section-fix [left-section-outer-wall-parameters left-section-inner-wall-parameters
                                                    thumb-tr-rm-to-index-br-outer-wall-parameters thumb-tr-rm-to-index-br-inner-wall-parameters
                                                    wall-cross-section-steps wall-section-steps]
    (let [thumb-tr-rm-to-index-br-outer-wall-points (-> (vec (for [index (range (inc (* wall-cross-section-steps 2)))
                                                                   :let [params (nth thumb-tr-rm-to-index-br-outer-wall-parameters index)]]
                                                               (decompose-b-spline-curve-and-calculate-bezier-curves
                                                                2 (:U params) (:P params)
                                                                2 3 (* wall-section-steps 2))))
                                                        (rotate-matrix :reverse-new-row true))
          thumb-tr-rm-to-index-br-inner-wall-points (do 
                                                      (println "thumb-tr-rm-to-index-br-outer-wall-points" (count thumb-tr-rm-to-index-br-outer-wall-points))
                                                      (-> (vec (for [index (range (inc (* wall-cross-section-steps 2)))
                                                                   :let [params (nth  thumb-tr-rm-to-index-br-inner-wall-parameters index)]]
                                                               (decompose-b-spline-curve-and-calculate-bezier-curves
                                                                2 (:U params) (:P params)
                                                                0 1 (* wall-section-steps 2))))
                                                        (rotate-matrix :reverse-new-row false :reverse-new-column true)))
           outer-merged-wall (do (println "thumb-tr-rm-to-index-br-outer-wall-points" (count thumb-tr-rm-to-index-br-outer-wall-points)
                                          (print (* wall-section-steps 2))) 
                               (vec (for [index (range (inc (* wall-section-steps 2)))
                                       :let [left-section-params (nth left-section-outer-wall-parameters index)
                                             left-section-points (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U left-section-params) (:P left-section-params) 29 29 wall-section-steps)
                                             thumb-points (do (println "index" index) 
                                                            (nth thumb-tr-rm-to-index-br-outer-wall-points  index))]]
                                   (vec (concat left-section-points thumb-points)))))
                           
          inner-merged-wall (vec (for [index (range (inc (* wall-section-steps 2)))
                                       :let [left-section-params (nth left-section-inner-wall-parameters index)
                                             left-section-points (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U left-section-params) (:P left-section-params) 0 0 wall-section-steps)
                                             thumb-points (nth thumb-tr-rm-to-index-br-inner-wall-points  index)]]
                                   (vec (concat   thumb-points  left-section-points))))]
                                   {:thumb-tr-rm-to-index-br-to-left-section-fix-vnf-array 
                                    (wall-vnf-array outer-merged-wall inner-merged-wall)}
                                   )
    )
  

(defn Ananse-Ntontan-alignment-positions [type]
   (mapv #(->> (type (alignment-peg-and-hole {:radius 0.8 :length 1.6}))
   ;;(cube 0.8 0.8 1.6)
              (rdx 90)
              (rdz 14)
              (translate (mapv + (div (mapv + (:wall-locate3-point (calculate-control-points (tps-65-wall-position :bm :south))) (:wall-locate3-point (calculate-control-points (tps-65-to-screen-wall-position :bl :south :offset [0 -0.0001 0])))) 2) [0 0.2 0]))
              (translate %))
        [[0 1 8] [4 2 -8] [-5 -1 -8]]))

(defn Onyakopon-Aniwa-alignment-positions [type]
  (mapv #(->> (type (alignment-peg-and-hole {:radius 0.6 :length 1.6})) 
          ;(cube 0.8 0.8 1.6)
              (rdy -88)
              (rdz  5)
              (translate (mapv + (div (mapv + (:wall-locate3-point (calculate-control-points (tps-65-wall-position :tl-lm :west)))
                                            (:wall-locate3-point (calculate-control-points (tps-65-wall-position :tl :west :offset [-0.00000001 0.0 0.0])))) 2)
                               [0 2.5 -14]))
              (translate %))
        [[1.8 -2 6] [0.4 7.6 0]]))

(comment (spit "things-low/left-test.scad"
          (write-scad
           (include "../BOSL2/std.scad")
           (let [steps 10
                 thumb-bl-tm-north-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tm :xy 3 :slant :no-slant))
                 thumb-bl-tm-north-outer-curve (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-bl-tm-north-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                                  3 default-weights-for-vertical-nurbs steps)
                 thumb-bl-tm-north-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tm-north-control-points inner-wall-curve-bezier-cubic-keywords) steps)
                 thumb-bl-tr-north-control-points (calculate-control-points (thumb-wall-position thumb-bl-place 0 1 :tr :xy 3 :slant :no-slant))
                 thumb-bl-tr-north-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tr-north-control-points  inner-wall-curve-bezier-cubic-keywords) steps)
                 left-section-bottom-mid-south-wall-position (tps-65-wall-position :bm :south)
                 left-section-bottom-mid-south-control-points (calculate-control-points left-section-bottom-mid-south-wall-position)
                 outer-curve (global-curve-interp-with-calculated-first-derivatives-curve
                              [(:web-post-position-top thumb-bl-tm-north-control-points)
                               (:wall-locate-1-to-3-curve-for-polyhedron-second-control-point thumb-bl-tm-north-control-points)
                               (:wall-locate3-point thumb-bl-tm-north-control-points)
                               (:wall-locate3-point-floor left-section-bottom-mid-south-control-points)]
                              [(mapv - (:web-post-position-top thumb-bl-tm-north-control-points) (:opposite-web-post-position-top thumb-bl-tm-north-control-points))
                               (mapv - (:wall-locate-1-to-3-curve-for-polyhedron-second-control-point thumb-bl-tm-north-control-points)  (:web-post-position-top thumb-bl-tm-north-control-points))
                               (mapv - (:wall-locate3-point thumb-bl-tm-north-control-points) (:wall-locate-1-to-3-curve-for-polyhedron-second-control-point thumb-bl-tm-north-control-points))
                               (mapv - (:wall-locate3-point left-section-bottom-mid-south-control-points) (:wall-locate3-point-floor left-section-bottom-mid-south-control-points))]
                              2
                              steps
                              :point-paramater-calculation-method :dynamic-centripetal)
                 inner-curve (global-curve-interp-with-calculated-first-derivatives-curve
                              [(:web-post-position-bottom thumb-bl-tm-north-control-points)
                               (:wall-locate-2-top thumb-bl-tm-north-control-points)
                               (:wall-locate-2-bottom thumb-bl-tm-north-control-points)
                               (:wall-locate-2-bottom-floor thumb-bl-tm-north-control-points)]
                              [(mapv - (:web-post-position-bottom thumb-bl-tm-north-control-points) (:opposite-web-post-position-bottom thumb-bl-tm-north-control-points))
                               (mapv - (:wall-locate-2-top thumb-bl-tm-north-control-points)  (:web-post-position-bottom thumb-bl-tm-north-control-points))
                               (mapv - (:wall-locate-2-bottom thumb-bl-tm-north-control-points) (:wall-locate-2-top thumb-bl-tm-north-control-points))
                               (mapv - (:wall-locate-2-bottom left-section-bottom-mid-south-control-points) (:wall-locate-2-bottom-floor thumb-bl-tm-north-control-points))]
                              2
                              steps
                              :point-paramater-calculation-method :dynamic-centripetal)

                 {left-section-data :left-section-data
                  thumb-outer-points-fn :thumb-outer-points-fn
                  thumb-inner-points-fn :thumb-inner-points-fn
                  outer-wall-parameters :outer-wall-parameters
                  inner-wall-parameters :inner-wall-parameters} (left-section-data steps :screen-outer-curve-type :local)
                 {left-section-vnf-array :vnf-array
                  outer-wall :outer-wall
                  inner-wall :inner-wall
                  trackpad-to-main-body-data :trackpad-to-main-body-data
                  thumb-bl-to-tl-outer-curve-fn :thumb-bl-to-tl-outer-curve-fn
                  thumb-tl-to-tr-outer-curve-fn :thumb-tl-to-tr-outer-curve-fn
                  thumb-bl-to-tl-inner-curve-fn :thumb-bl-to-tl-inner-curve-fn
                  thumb-tl-to-tr-inner-curve-fn :thumb-tl-to-tr-inner-curve-fn} left-section-data
                 thumb-outer-points (thumb-outer-points-fn steps)
                 thumb-inner-points (vec (reverse (thumb-inner-points-fn steps)))
                 thumb-bl-tr-north-inner-curve (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-bl-tr-north-control-points inner-wall-curve-bezier-cubic-keywords) steps)
                 left-section-bottom-mid-south-wall-position (tps-65-wall-position :bm :south)
                 left-section-bottom-mid-south-control-points (calculate-control-points left-section-bottom-mid-south-wall-position)
                 left-section-bottom-left-south-wall-position (tps-65-to-screen-wall-position :bl :south :offset [0 -0.0001 0])
                 left-section-bottom-left-south-control-points (calculate-control-points left-section-bottom-left-south-wall-position)
                 {thumb-bl-to-tl-vnf :thumb-bl-to-tl-vnf
                  thumb-tl-to-tr-vnf :thumb-tl-to-tr-vnf} (thumb-connecter-1-row steps :thumb-bl-to-tl-P-u-zero-outer (vec (reverse (thumb-bl-to-tl-outer-curve-fn steps)))
                                                                                 :thumb-tl-to-tr-P-u-zero-outer (vec (reverse (thumb-tl-to-tr-outer-curve-fn steps)))
                                                                                 :thumb-bl-to-tl-P-u-one-inner (thumb-bl-to-tl-inner-curve-fn steps) :thumb-tl-to-tr-P-u-one-inner (thumb-tl-to-tr-inner-curve-fn steps))
                 index-bottom-br (calculate-control-points (key-wall-position 1 2 1 0 :br :xy 4 :slant :no-slant))
                 thumb-tr-tr-control-points (calculate-control-points (thumb-wall-position thumb-tr-place 1 -1 :tr :xy 4 :slant :parallel-by-d-opposite))
                 thumb-tr-rm-control-points (calculate-control-points (thumb-wall-position thumb-tr-place 1 0 :rm :xy 4  :slant :no-slant))
                 middle-bottom-bl (calculate-control-points (key-wall-position 2 2 1 -1 :bl :xy 3 :slant :no-slant))
                 end-inner-fn (fn [point] (let [degree 2
                                                params (global-curve-interp-with-end-derivatives-calculated
                                                        [(point index-bottom-br)
                                                         (point thumb-tr-tr-control-points)]
                                                                                                               ;(point thumb-tr-rm-control-points)
                                                                                                               ;(point thumb-tr-br-control-points)
                                                        
                                                        degree
                                                        (mapv - (point index-bottom-br) (point middle-bottom-bl))
                                                        (mapv -  (point thumb-tr-rm-control-points) (point thumb-tr-tr-control-points))
                                                                                                              ;(mapv - (point thumb-tr-br-control-points) (point thumb-tr-rm-control-points) )
                                                                                                              ;(mapv - (point thumb-tr-br-control-points) (point thumb-tr-rm-control-points))
                                                        :point-paramater-calculation-method :chordal
                                                        :magnitude-estimation-method :chord)]
                                            (decompose-b-spline-curve-and-calculate-bezier-curves degree (:U params) (:P params)
                                                                                                  0 1 (* steps 2))))
                 end-curve-inner (end-inner-fn :web-post-position-bottom)
                 index-br-control-points  (calculate-control-points (key-wall-position 1 2 1 0 :br :xy 3 :slant :no-slant))
                 index-br-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words index-br-control-points inner-wall-curve-bezier-cubic-keywords) steps) thumb-tr-tr-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-tr-tr-control-points inner-wall-curve-bezier-cubic-keywords) steps)
                 thumb-tr-rm-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-tr-rm-control-points inner-wall-curve-bezier-cubic-keywords) steps)
                 middle-bl-control-points  (calculate-control-points (key-wall-position 2 2 1 -1 :bl  :xy 3 :slant :no-slant))
                 middle-bl-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words middle-bl-control-points inner-wall-curve-bezier-cubic-keywords)
                                                        steps)
                 thumb-tr-br-control-points (calculate-control-points (thumb-wall-position thumb-tr-place 1 0 :br :xy 4  :slant :no-slant))
                 thumb-tr-br-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-tr-br-control-points inner-wall-curve-bezier-cubic-keywords) steps)
                 index 0
                 inner-params  (global-curve-interp-with-calculated-first-derivatives
                                [(nth index-br-inner index)
                                 (nth thumb-tr-tr-inner index)
                                 (nth thumb-tr-rm-inner index)]
                                [(mapv -   (nth index-br-inner index) (nth middle-bl-inner index))
                                 (mapv -  (nth thumb-tr-rm-inner index) (nth thumb-tr-tr-inner index))
                                 (mapv -  (nth thumb-tr-br-inner index) (nth thumb-tr-rm-inner index))]
                                2 
                                :point-paramater-calculation-method :equal
                                                                                        ;:knot-vector-calculation-method :equal
                                :magnitude-estimation-method 1)
                 inner-curve (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-params) (:P inner-params) 0 1 20)
                 
                 end-tangents-inner-index steps
                 end-tangents-inner-params  (global-curve-interp-with-calculated-first-derivatives
                                             [(nth index-br-inner end-tangents-inner-index)
                                              (nth thumb-tr-tr-inner end-tangents-inner-index)
                                              (nth thumb-tr-rm-inner end-tangents-inner-index)]
                                             [(mapv -   (nth index-br-inner end-tangents-inner-index) (nth middle-bl-inner end-tangents-inner-index))
                                              (mapv -  (nth thumb-tr-rm-inner end-tangents-inner-index) (nth thumb-tr-tr-inner end-tangents-inner-index))
                                              (mapv -  (nth thumb-tr-br-inner end-tangents-inner-index) (nth thumb-tr-rm-inner end-tangents-inner-index))]
                                             2
                                             :point-paramater-calculation-method :equal
                                                                                                                                                   ;:knot-vector-calculation-method :equal
                                             :magnitude-estimation-method 1)
                 end-tangents-inner-tangent-curve (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U end-tangents-inner-params) (:P end-tangents-inner-params) 0 1 20)
                 thumb-tr-tr-outer (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-tr-tr-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                      3 default-weights-for-vertical-nurbs (* steps 2))
                 tt (global-curve-interp-with-calculated-first-derivatives-curve
                     [(thumb-web-post-point-top thumb-tr-place :rm)
                      (thumb-web-post-point-top thumb-tr-place :tr)
                      (main-body-web-post-point-top 1 2 :br)]
                     [(mapv - (thumb-web-post-point-top thumb-tr-place :rm) (thumb-web-post-point-top thumb-tr-place :br) )
                      (mapv - (thumb-web-post-point-top thumb-tr-place :tr) (thumb-web-post-point-top thumb-tr-place :rm))
                      (mapv - (main-body-web-post-point-top 2 2 :bl) (main-body-web-post-point-top 1 2 :br) )
                      ]
                     2
                     steps
                     :point-paramater-calculation-method :dynamic-centripetal
                     :magnitude-estimation-method 22 
                     )
                 outer-top (nth outer-wall-parameters 0)
                 outer-top-curve (decompose-b-spline-curve-and-calculate-bezier-curves 
                                  2 (:U outer-top) (:P outer-top) 29 29 20)
                 {thumb-tr-rm-to-index-br-vnf-array :vnf-array
                  outer-bottom-points :outer-bottom-points
                  inner-bottom-points :inner-bottom-points
                  thumb-tr-rm-to-index-br-outer-wall-parameters :outer-wall-parameters
                  thumb-tr-rm-to-index-br-inner-wall-parameters :inner-wall-parameters} (thumb-tr-rm-to-index-br (* steps 2) steps)
                 left-section-index (count outer-wall-parameters)
                 thumb-tr-rm-to-index-br-index (count thumb-tr-rm-to-index-br-outer-wall-parameters)
               thumb-tr-rm-to-index-br-outer-wall-points (-> (vec (for [index (range (inc (* steps 2)))
                                                                    :let [params (nth thumb-tr-rm-to-index-br-outer-wall-parameters index)]]
                                                                (decompose-b-spline-curve-and-calculate-bezier-curves
                                                                 2 (:U params) (:P params)
                                                                 2 3 20)))
                                                              (rotate-matrix :reverse-new-row true))
                 thumb-tr-rm-to-index-br-inner-wall-points (-> (vec (for [index (range (inc (* steps 2)))
                                                                          :let [params (nth  thumb-tr-rm-to-index-br-inner-wall-parameters index)]]
                                                                      (decompose-b-spline-curve-and-calculate-bezier-curves
                                                                       2 (:U params) (:P params)
                                                                       0 1 20)))
                                                               (rotate-matrix :reverse-new-row false :reverse-new-column true))
                 ;thumb-tr-rm-to-index-br-outer-wall-parameters 
                 ;(rotate-matrix thumb-tr-rm-to-index-br-outer-wall-parameters) 
                   outer-merged-wall (vec (for [index (range (inc (* steps 2)))
                                              :let [left-section-params (nth outer-wall-parameters index)
                                                    left-section-points (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U left-section-params) (:P left-section-params) 29 29 steps)
                                                    thumb-points (nth thumb-tr-rm-to-index-br-outer-wall-points  index)
                                                    ]]
                                            (vec (concat left-section-points thumb-points))
                                          ))
                 ;inner-wall-parameters-matrix-rotated (rotat) 
                     inner-merged-wall (vec (for [index (range (inc (* steps 2)))
                                              :let [left-section-params (nth inner-wall-parameters index)
                                                    left-section-points (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U left-section-params) (:P left-section-params) 0 0 steps)
                                                    thumb-points (nth thumb-tr-rm-to-index-br-inner-wall-points  index)]]
                                          (vec (concat   thumb-points  left-section-points))))
                 
                 ]
                                                                            
         
             (println "(:p inner-params)"(:p inner-params))
             (println "left-section-params" (count outer-wall-parameters))
             (println "thumb-tr-rm-to-index-br-outer-wall-parameters" (count thumb-tr-rm-to-index-br-outer-wall-parameters))
           
             (union
              ;(plot-bezier-points outer-top-curve (sphere 0.5))
              
              (-# (difference (aviator-assembly-polyhedron)
                          aviator-assembly-diffs))
              thumb-type
              ;(vnf-polyhedron (vnf-vertex-array outer-merged-wall (vnf-vertex-array-args :caps false :col-wrap false)))
              ;(vnf-polyhedron (vnf-vertex-array inner-merged-wall (vnf-vertex-array-args :caps false :col-wrap false)))
              (vnf-polyhedron (wall-vnf-array outer-merged-wall inner-merged-wall ))
              ;(vnf-polyhedron (vnf-vertex-array thumb-tr-rm-to-index-br-inner-wall-points (vnf-vertex-array-args :caps false :col-wrap false)))
              ;(translate (nth thumb-tr-rm-to-index-br-inner-wall-points 0) (sphere 0.5))
              ;(plot-bezier-points (nth (rotate-matrix thumb-tr-rm-to-index-br-outer-wall-points :reverse-new-row true) 0) (sphere 0.5))
              ;(translate (nth (nth (rotate-matrix thumb-tr-rm-to-index-br-outer-wall-points :reverse-new-row true) 0) 0) (sphere 0.5))
      ;;     (translate (mapv + (div (mapv + (:wall-locate3-point left-section-bottom-mid-south-control-points)(:wall-locate3-point left-section-bottom-left-south-control-points)) 2) [0 -0.5 0]) 
      ;;                (rdz 20 (rdx 90 (extrude-svg ananse 2 :scale-data [0.8 0.8] :top-face-scale 0.9))))
              ;(plot-bezier-points inner-curve  (sphere 0.5))
              ;(color [1 0 0 1] (plot-bezier-points index-br-inner (sphere 0.5)))
              (->> (svg-import "../svg/Ananse-Ntontan.svg") 
           
                   ;(translate [(* (/ -3 2) 25 ) (* (/ -3 2) 23.3 )])
                   (scale [0.3 0.3 1])
                   (extrude-linear {:height 2 :scale 0.8})
                   (rdx 90)
                   (rdz 14)
                   (translate (mapv + (div (mapv + (:wall-locate3-point left-section-bottom-mid-south-control-points) (:wall-locate3-point left-section-bottom-left-south-control-points)) 2) [0 0.2 0]))
                   (-#))
               (mapv #(->> (:hole (alignment-peg-and-hole {:radius 0.8 :length 2}))
                  ;;(cube 0.8 0.8 1.6)
                           (rdx 90)
                           (rdz 0)
                           (translate (:wall-locate3-point (calculate-control-points (tps-65-wall-position :tm :north))))
                           (translate %))
                     [[-8 -2 -2] [-8 -2 -24]])
              (Ananse-Ntontan-alignment-positions :peg)
          
              ;; (->> (cube 0.8 0.8 1.6)
              ;;      (rdx 90)
              ;;      (rdz 14)
              ;;      (translate (mapv + (div (mapv + (:wall-locate3-point left-section-bottom-mid-south-control-points) (:wall-locate3-point left-section-bottom-left-south-control-points)) 2) [0 0.2 0]))
              ;;      (translate [4 2 -8]))
              ;; (->> (cube 0.8 0.8 1.6)
              ;;      (rdx 90)
              ;;      (rdz 14)
              ;;      (translate (mapv + (div (mapv + (:wall-locate3-point left-section-bottom-mid-south-control-points) (:wall-locate3-point left-section-bottom-left-south-control-points)) 2) [0 0.2 0]))
              ;;      (translate [-5 0 -8]))
              (->>
               (svg-import "../svg/Onyakopon_Aniwa.svg")
               (scale [0.03 0.03 1])
               (extrude-linear {:height 3 :scale 0.8})
               ;(translate [0 0 -1])
               (rdz -10)
               (rdy -88)
               (rdz 5)
               (translate (mapv + (div (mapv + (:wall-locate3-point (calculate-control-points (tps-65-wall-position :tl-lm :west)))
                                             (:wall-locate3-point (calculate-control-points (tps-65-wall-position :tl :west :offset [-0.00000001 0.0 0.0])))) 2)
                                [0 2.5 -14]))
                      
               (-#))
           
              (Onyakopon-Aniwa-alignment-positions :peg)
              ;; (->>
              ;;  (svg-import "../svg/nkyinkyim_log_spiral.svg")
              ;;  (mirror [1 0 0])
              ;;  (scale [0.2 0.2 1])
              ;;  (extrude-linear {:height 3 :scale 0.8})
              ;;  (translate [0 0 -1])
              ;;  (rdz -90)
              ;;  (rdy -88)
              ;;  (rdz 5)
              ;;  (translate (mapv + (div (mapv + (:wall-locate3-point (calculate-control-points (tps-65-wall-position :tl-lm :west)))
              ;;                                (:wall-locate3-point (calculate-control-points (tps-65-wall-position :tl :west :offset [-0.00000001 0.0 0.0])))) 2)
              ;;                   [0 2.5 -30]))
              ;;  )
              ;; (->> (svg-import "../svg/Odenkyem.svg")

              ;;      ;(translate [(* (/ -3 2) 25 ) (* (/ -3 2) 23.3 )])
              ;;      (scale [0.1 0.1 1])
              ;;      (extrude-linear {:height 2 :scale 0.8})
              ;;      (rdx 80)
              ;;      (rdz -80)
              ;;      (translate (mapv + (div (mapv + (:wall-locate3-point (calculate-control-points (tps-65-wall-position :tl-lm :west)) )
              ;;                               (:wall-locate3-point (calculate-control-points (tps-65-wall-position :tl :west :offset [-0.00000001 0.0 0.0])))) 2) [0 0.0 -8])))
          
              ;(plot-bezier-points thumb-inner-points (sphere 0.5))
             ; (plot-bezier-points thumb-bl-tr-tm-north-west-offest-inner-curve (sphere 0.5))
      
      ;;      (vnf-polyhedron (wall-vnf-array thumb-to-l (reverse thumb-to-l-inner-2) 
      ;;                                    :args {:caps false :cap1 false :cap2 false :col-wrap false :row-wrap false :reverse false :style :default}))
              ;(vnf-polyhedron (vnf-vertex-array (reverse thumb-to-l) :caps false :col-wrap false))
              ;(vnf-polyhedron (vnf-vertex-array thumb-to-l-inner-2 :caps false :col-wrap false))
              ;(vnf-polyhedron (vnf-vertex-array thumb-to-l-inner :caps false :col-wrap false))
              ;(translate (nth (nth (rotate-matrix thumb-to-l :reverse-new-row true) 0) 0) (cube 1 1 1))
              ;(plot-bezier-points (nth (rotate-matrix thumb-to-l :reverse-new-row true) 0) (cube 0.5 0.5 0.5))
              ;(color [1 0 1 1](plot-bezier-points (peek (reverse thumb-to-l-inner-2) ) (cube 0.5 0.5 0.5)))
              ;(thumb-tr-place MxLEDBitPCB)
              ;(key-place 1 2 des-r2)
              ;(thumb-tr-place single-key-pcb-holder)
      ;;     (vnf-polyhedron (wall-vnf-array global-outer-thumb-wall inner-thumb-wall :args {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default}))
              ;(vnf-polyhedron (:wall-vnf(thumb-to-left-section-2 steps steps thumb-outer-points-fn thumb-inner-points-fn)))
              ;(vnf-polyhedron (vnf-vertex-array global-outer-thumb-wall :caps false :col-wrap false))
              ;(translate (nth (thumb-outer-points-fn steps) 0) (sphere 2))
              ;key-holes
      ;;     (plot-bezier-points (thumb-inner-points-fn steps) (sphere 0.5) )
      ;;     (plot-bezier-points outer-curve (sphere 0.5))
      ;;     (plot-bezier-points thumb-bl-tm-north-inner-curve (sphere 0.5))
      ;;     (plot-bezier-points thumb-bl-tr-north-inner-curve (sphere 0.5))
              ;(color [1 0 0 1] (plot-bezier-points thumb-bl-tm-north-outer-curve (sphere 0.5)))
              ;(translate (nth (thumb-inner-points-fn steps) 0) (sphere 2))
      ;;     (tps-65-place (difference  tps-65-mount-new
      ;;                                tps-65
      ;;                                tps-65-mount-cutout
      ;;                                (translate [0 0 (+ 0 1)] tps-65-mount-main-cutout)))
             ;(vybronics-vl91022-place vybronics-vl91022-mount)
              ;(vnf-polyhedron  (thumb-to-left-section steps thumb-outer-points-fn thumb-inner-points-fn)) 
              ;(vnf-polyhedron thumb-bl-to-tl-vnf)
              ;(vnf-polyhedron thumb-tl-to-tr-vnf)
              ;(vnf-polyhedron (:trackpad-to-main-body-vnf trackpad-to-main-body-data))
              ;(screen-holder-place-side ST7789-240x240)
          ;;  (difference 
          ;;   (union 
          ;;   (-# (vnf-polyhedron left-section-vnf-array))
      
          ;;               ;(screen-holder-place-side screen-holder)
          ;;  ;                  aviator-assembly-polyhedron
          ;;                    ))
              ;; (difference 
              ;;   (thumb-tr-place (intersection MxLEDBitPCB-clearance-smaller
              ;;                                 single-key-pcb-holder-north-leg))
              ;;  (thumb-tr-place single-plate))
          ;;     (-# (vnf-polyhedron (vnf-vertex-array outer-wall (vnf-vertex-array-args :caps false :col-wrap false))))
          ;;     (vnf-polyhedron (vnf-vertex-array inner-wall (vnf-vertex-array-args :caps false :col-wrap false)))
          ;;     (thumb-tr-place MxLEDBitPCB-clearance-smaller)
              ;(key-place 0 2 (rdz 0 des-r2))
              ;(key-place 1 2 (rdz 0 des-r2))
              )))))
      
      ;; (thumb-tr-place MxLEDBitPCB-clearance-smaller)                 
      ;;                  ) 
                ;          aviator-assembly-diffs
                          ;(screen-holder-place-side screen-holder-cut)
                          ;(screen-holder-place-side (translate [0 0 screen-holder-depth] screen-holder-cut-viewport-cut ))
                 ;         )
          

(defrecord FractylRightWallData [fractyl-right-wall-vnf bottom-outer bottom-inner key-gap-outer-curve-fn-coll 
                                  key-gap-inner-curve-fn-coll])
(defn fractyl-right-wall [wall-cross-section-steps wall-section-steps]
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
       pinky-row-2-br-east (wall-cross-section-parameter (key-wall-position lastcol 2 1 0 :br  :slant :no-slant))
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
       pinky-row-0-br-east (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :br)
                                                         :inner-wall-parameters (inner-wall-vertical-bezier-parameters :control-point-keywords inner-wall-curve-bezier-quadratic-keywords))
       pinky-row-0-rm-east (wall-cross-section-parameter (key-wall-position lastcol 0 1 0 :rm))
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
                                       pinky-row-0-tm-north]
                                     
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
                 (wall-cross-section-tangent-parameter pinky-row-0-tl-north pinky-row-0-tm-north)]
                
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
                       pinky-row-0-tm-north-wall-position]
                      
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
                                        [pinky-row-0-tl-north-wall-position pinky-row-0-tm-north-wall-position]]
                
       outer-coll (global-curve-interp-with-calculated-first-derivatives
                   (mapv #(:web-post-position-top (calculate-control-points %)) wall-positions)
                   (mapv #(mapv - (:web-post-position-top (calculate-control-points (nth % 0))) 
                                (:web-post-position-top (calculate-control-points(nth % 1))))
                         wall-position-tangents-position)
                   2 
                   :point-paramater-calculation-method point-paramater-calculation-method
                   :magnitude-estimation-method magnitude-estimation-method)
                  
       inner-coll (global-curve-interp-with-calculated-first-derivatives
                   (mapv #(:web-post-position-bottom (calculate-control-points %)) wall-positions)
                   (mapv #(mapv - (:web-post-position-bottom (calculate-control-points (nth % 0)))
                                (:web-post-position-bottom (calculate-control-points (nth % 1))))
                         wall-position-tangents-position)
                   2
                   top-cap-steps)]
      
      (FractylRightWallData. (vnf-join [(wall-vnf wall-section {:caps false :cap1 false :cap2 true :col-wrap true :row-wrap false :reverse false :style :concave})
                                        (vnf-vertex-array top-cap :caps false :col-wrap false)])
                             (peek (:outer-wall wall-section)) (peek (:inner-wall wall-section))
                             [(fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-coll) (:P outer-coll) 6 7 steps))
                              (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U outer-coll) (:P outer-coll) 12 13 steps))] 
                             [(fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-coll) (:P inner-coll) 6 7 steps))
                              (fn [steps] (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U inner-coll) (:P inner-coll) 12 13 steps))])))
                                           
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
                      
  

(def right-wall-nkynkyim 
  (let 
   [scale-factor 0.1
    face-scale-factor 0.9
    z-height (/ (* 69.213 scale-factor) 2)
    width 1.6
    row-2-rot (+ (:z (apply-key-geometry-rotation-values 4 2)) (deg2rad 7))
    row-1-rot (+ (:z (apply-key-geometry-rotation-values 4 2)) (deg2rad 0.19))
    row-1-to-0-rot (+ (:z (apply-key-geometry-rotation-values 4 2)) (deg2rad -3.61))
    row-0-rot (+ (:z (apply-key-geometry-rotation-values 4 2)) (deg2rad -6.34))
    place-nkyinkyim-spiral (fn [wall-pos & {:keys [offset  y-rot z-rot level flip extrude-scale x-rot x-extra-offset z-extra-offset orientation]
                                            :or {offset [0 0 0] y-rot 90 z-rot 186 level 0
                                                 extrude-scale face-scale-factor flip false
                                                 x-rot 0 x-extra-offset 0 z-extra-offset 0 orientation 90}}]
                             (->>
                              (cond->>
                               (svg-import "../svg/nkyinkyim_log_spiral.svg")
                               flip (mirror [1 0 0]))
                              (scale [scale-factor scale-factor 1])
                              (extrude-linear {:height 1.5 :scale extrude-scale})
                                             ;(translate [0 0 -0.5])
                                                       ;(translate [0 0 -1])
                              (rdz orientation)
                              (rdy y-rot)
                              (rdx x-rot)
                              (rz z-rot)

                              (translate
                               (assoc (mapv + (:wall-locate3-point (calculate-control-points wall-pos))
                                            (rotate-around-z z-rot offset)) 2
                                      z-height))
                              (translate (rotate-around-y-in-degrees (- y-rot 90) (rotate-around-z z-rot [0 0 (- (+ (* z-height level 2 face-scale-factor)) (* level width 0.88))])))
                              (translate (rotate-around-y-in-degrees (- y-rot 90) (rotate-around-z z-rot [x-extra-offset 0 z-extra-offset])))))
    x-offset -0.135
    r2-br [(key-wall-position 4 2 1 0 :br) :z-rot row-2-rot :offset [0 1.7 0] :y-rot 86]
    r2-rm [(key-wall-position 4 2 1 0 :rm) :z-rot row-2-rot :offset [x-offset 0 0] :y-rot 86]
    r2-tr [(key-wall-position 4 2 1 0 :tr) :z-rot row-2-rot :offset [x-offset -1.70 0] :y-rot 86]
    r2-r1 [(key-wall-position 4 1 1 0 :br) :z-rot row-2-rot :offset [(+ -0.6 x-offset) -6.3 0] :y-rot 84]
    r1-br [(key-wall-position 4 1 1 0 :br) :z-rot row-2-rot :offset [(+ -0.6 x-offset) -0.5 0] :y-rot 78]
    r1-br-rm [(key-wall-position 4 1 1 0 :br-rm) :z-rot row-1-rot :offset [(+ -0.80 x-offset) 1 0] :y-rot 78]
    r1-tr-rm [(key-wall-position 4 1 1 0 :tr-rm) :z-rot row-1-rot  :offset [(+ -0.80 x-offset) -1.8 0] :y-rot 78]
    r1-tr [(key-wall-position 4 1 1 0 :tr) :z-rot row-1-rot  :offset [(+ -0.80 x-offset) -0.3 0] :y-rot 78]
    r1-r0 [(key-wall-position 4 0 1 0 :br) :z-rot row-1-to-0-rot  :offset [(+ -0.80 x-offset) -5.1 0] :y-rot 81]
    r0-br [(key-wall-position 4 0 1 0 :br) :z-rot row-0-rot  :offset [(+ -0.666 x-offset) 0.7 0] :y-rot 84]
    r0-rm [(key-wall-position 4 0 1 0 :rm) :z-rot (+ (:z (apply-key-geometry-rotation-values 4 2)) (deg2rad -10))  :offset [(+ -0.33 x-offset) -1.3 0] :y-rot 84]
    r0-tr [(key-wall-position 4 0 1 0 :tr) :z-rot (+ (:z (apply-key-geometry-rotation-values 4 2)) (deg2rad -10))
           :offset [(+  0.17 x-offset) -3.25 0] :y-rot 84]
    spiral-list [r2-br r2-rm r2-tr r2-r1 r1-br r1-br-rm r1-tr-rm r1-tr r1-r0 r0-br]] 
   (union
    (apply place-nkyinkyim-spiral (concat r2-br [:level 2 :flip false :orientation -45]))
    (apply place-nkyinkyim-spiral (concat r2-br [:level 2 :flip false :orientation -45 :x-extra-offset -1.5 :extrude-scale 1]))
    (apply place-nkyinkyim-spiral (concat r2-br [:level 2 :flip false :orientation -45 :x-extra-offset -3 :extrude-scale 1]))
    (apply place-nkyinkyim-spiral (concat r2-br [:level 1  :flip true :orientation -90]))
    (apply place-nkyinkyim-spiral (concat r2-br [:level 1  :flip true :orientation -90]))
                              ;(apply place-nkyinkyim-spiral (conj r0-tr [:level 2]))
    (apply place-nkyinkyim-spiral (concat r0-rm [:level 1 :z-extra-offset 0.1]))
    (apply place-nkyinkyim-spiral (concat r0-rm [:level 1 :z-extra-offset 0.1 :x-extra-offset -1.5 :extrude-scale 1]))
    (apply place-nkyinkyim-spiral (concat r0-rm [:flip true :orientation -90]))
    (apply place-nkyinkyim-spiral (concat r0-tr [:orientation -90]))
                              ;(apply place-nkyinkyim-spiral (concat r0-tr [:level 1 :flip true]))
    (apply place-nkyinkyim-spiral (concat r0-tr [:level 1 :flip true :z-extra-offset 0.1]))
    (apply place-nkyinkyim-spiral (concat r0-tr [:level 1 :flip true :z-extra-offset 0.1 :extrude-scale 1 :x-extra-offset -1.5]))
    (map #(apply place-nkyinkyim-spiral %) spiral-list)
    (map #(apply place-nkyinkyim-spiral %)
         (mapv #(concat % [:level 1 :flip true]) [r2-rm]))
    (map #(apply place-nkyinkyim-spiral %)
         (mapv #(concat % [:x-extra-offset -1.5 :extrude-scale 1])
               [r2-tr r2-r1 r1-br r1-br-rm r1-tr-rm r1-tr r1-r0 r0-br]))
    (map #(apply place-nkyinkyim-spiral %)
         (mapv #(concat % [:x-extra-offset -1.5 :extrude-scale 1 :level 1 :flip true])
               [r2-rm])))))
                  
  

(def right-side-symbol-floor-diff
  (-# (translate  (mapv + (:wall-locate3-point-floor (calculate-control-points (key-wall-position 4 2 1 0 :tr)))
                        [0 20 -2])
                  (rdz pinky-splay  (cube 10 100 4)))))
  

(def right-wall-nkynkyim-smaller
  (let
   [spiral-width 79.280
    scale-factor 0.067
    face-scale-factor 0.9
    spiral-scaled-width (* spiral-width scale-factor)
    z-height (/ (* 69.213 scale-factor) 2)
    stroke-width 14.765
    scaled-stroke-width (* stroke-width scale-factor)
    place-nkyinkyim-spiral (fn [wall-pos & {:keys [offset x-rot z-rot y-rot level wall-elevation flip extrude-scale]
                                            :or {offset [0 0 0] x-rot 90 z-rot 0 y-rot 0
                                                 level 0 wall-elevation 0 flip  false extrude-scale face-scale-factor}}]
                             (->>
                              (cond->> (svg-import "../svg/nkyinkyim_log_spiral.svg")
                                flip  (mirror [1 0 0]))
                                                                                        ;(mirror [1 0 0])
                              (scale [scale-factor scale-factor 1])
                              (extrude-linear {:height 1.5 :scale extrude-scale})
  
  
  
                                                                                        ;(translate [0 0 -1])
                              (rdz 0)
                              (rdx x-rot)
                              (rdy y-rot)
                              (rdz z-rot)
                              (translate
                               (assoc (mapv + (:wall-locate3-point (calculate-control-points wall-pos))
                                            (rotate-around-z-in-degrees z-rot offset)) 2
                                      (- (+ z-height (* z-height level 2 face-scale-factor)) (* level scaled-stroke-width 0.95))))))
    nkyinkyim-chain (fn [starting-wall-pos  & {:keys [end-wall-pos chain-length offset x-rot z-rot y-rot
                                                      level wall-elevation flip extrude-scale]
                                               :or {offset [0 0 0] x-rot 90 z-rot 0 y-rot 0
                                                    level 0 wall-elevation 0 flip  false extrude-scale face-scale-factor}}]
                      (let
                       [num-of-spirals (if chain-length chain-length
                                           (if end-wall-pos (/ (+ (magnitude (mapv - (:web-post-position-top (calculate-control-points starting-wall-pos))
                                                                                   (:web-post-position-top (calculate-control-points end-wall-pos))))
                                                                  (/ spiral-scaled-width 2))
                                                               spiral-scaled-width)
                                               (ceil (/ mount-width spiral-scaled-width))))
                        initial-shape (place-nkyinkyim-spiral starting-wall-pos :offset offset :x-rot x-rot
                                                              :z-rot z-rot :y-rot y-rot
                                                              :level level :wall-elevation wall-elevation
                                                              :flip flip :extrude-scale extrude-scale)]
                       (apply union (for [i (range num-of-spirals)
                                          :let [translation (rotate-around-z-in-degrees z-rot [(- (* i spiral-scaled-width) (* i stroke-width scale-factor 1.45)) 0 0])]]
                                      (do (println "translation" translation)
                                          (translate  translation initial-shape))))))
    x-offset -1
    y-offset-bl 0.23
    x-rot-bl 90]
   (union
  
    (nkyinkyim-chain (key-wall-position lastcol cornerrow 1 0 :br :xy 5)
                             ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                     :chain-length 4
                     :z-rot 90;(- 90 pinky-splay)
                     :offset [0 0.5 0]
                     :level 0 :flip false :x-rot x-rot-bl)
    (nkyinkyim-chain (key-wall-position lastcol cornerrow 1 0 :br :xy 5)
                                    ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                     :chain-length 3
                     :z-rot 90;(- 90 pinky-splay)
                     :offset [0 0.5 0]
                     :level 1 :flip true :x-rot x-rot-bl)
    (nkyinkyim-chain (key-wall-position lastcol cornerrow 1 0 :br :xy 5)
                                           ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                     :chain-length 2
                     :z-rot 90;(- 90 pinky-splay)
                     :offset [0 0.5 0]
                     :level 2 :flip false :x-rot x-rot-bl)
    (nkyinkyim-chain (key-wall-position lastcol cornerrow 1 0 :br :xy 5)
                                                  ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                     :chain-length 1
                     :z-rot 90;(- 90 pinky-splay)
                     :offset [0 0.5 0]
                     :level 3 :flip true :x-rot x-rot-bl)
    (nkyinkyim-chain (key-wall-position lastcol cornerrow 1 0 :tr :xy 5)
                                    ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                     :chain-length 1
                     :z-rot 90;(- 90 pinky-splay)
                     :offset [0.44 0.61 0]
                     :level 0 :flip false :x-rot (- x-rot-bl 4))
    (nkyinkyim-chain (key-wall-position lastcol 2 1 0 :tr :xy 5)
                                       ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                     :chain-length 2
                     :z-rot (+ 90 (/ pinky-splay 2))
                     :offset [4.3 0.72 0]
                     :level 0 :flip false :x-rot (- x-rot-bl 6))
    (nkyinkyim-chain (key-wall-position lastcol 1 1 0 :br :xy 5)
                                              ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                     :chain-length 2
                     :z-rot (+ 90 -5)
                     :offset [1.65 0.75 0]
                     :level 0 :flip false :x-rot (- x-rot-bl 8))
    (nkyinkyim-chain (key-wall-position lastcol 1 1 0 :rm :xy 5)
                                                     ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                     :chain-length 3
                     :z-rot (+ 90 pinky-splay)
                     :offset [0.825 1. 0]
                     :level 0 :flip false :x-rot (- x-rot-bl 10))
    (nkyinkyim-chain (key-wall-position lastcol 1 1 0 :tr :xy 5)
                                                           ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                     :chain-length 2
                     :z-rot (+ 90 (* pinky-splay 1.5))
                     :offset [3.85 1.05 0]
                     :level 0 :flip false :x-rot (- x-rot-bl 7))
    (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :br :xy 5)
                                                                  ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                     :chain-length 1
                     :z-rot (+ 90 (* pinky-splay 2))
                     :offset [1.05 0.81 0]
                     :level 0 :flip false :x-rot (- x-rot-bl 4))
    (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :br-rm :xy 5)
                                                                         ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                     :chain-length 1
                     :z-rot (+ 90 (* pinky-splay 2))
                     :offset [1.05 0.7 0]
                     :level 0 :flip false :x-rot (- x-rot-bl 1))
       ;; (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :br-rm :xy 5)
       ;;                                                                         ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
       ;;                  :chain-length 1
       ;;                  :z-rot (+ 90 (* pinky-splay 2))
       ;;                  :offset [1.05 0.7 0]
       ;;                  :level 1 :flip true :x-rot (- x-rot-bl 1))
    (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :rm :xy 5)
                                                                               ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                     :chain-length 1
                     :z-rot (+ 90 (* pinky-splay 2.5))
                     :offset [1.05 0.55 0]
                     :level 0 :flip false :x-rot (+ x-rot-bl 2))
    (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :rm :xy 5)
                                                                                      ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                     :chain-length 1
                     :z-rot (+ 90 (* pinky-splay 2.5))
                     :offset [1.05 0.55 0]
                     :level 1 :flip true :x-rot (+ x-rot-bl 2))
       ;;  (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :rm :xy 5)
       ;;                                                                                ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
       ;;                   :chain-length 1
       ;;                   :z-rot (+ 90 (* pinky-splay 2.5))
       ;;                   :offset [1.05 0.55 0]
       ;;                   :level 2 :flip false :x-rot (+ x-rot-bl 2))
    (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :tr-rm :xy 5)
                                                                                      ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                     :chain-length 1
                     :z-rot (+ 90 (* pinky-splay 2.25))
                     :offset [1.05 0.22 0]
                     :level 0 :flip false :x-rot (+ x-rot-bl 0))
    (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :tr-rm :xy 5)
                                                                                             ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                     :chain-length 1
                     :z-rot (+ 90 (* pinky-splay 2.25))
                     :offset [1.05 0.22 0]
                     :level 1 :flip true :x-rot (+ x-rot-bl 0))
    (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :tr-rm :xy 5)
                                                                                            ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                     :chain-length 1
                     :z-rot (+ 90 (* pinky-splay 2.25))
                     :offset [1.05 0.22 0]
                     :level 2 :flip false :x-rot (+ x-rot-bl 0)))))
      
     
  

(comment 
  (spit "things-low/alignment-peg-and-hole-test.scad"
        (write-scad
         (:peg (alignment-peg-and-hole)))))
(defn right-side-alignment-positions [type]
  (union
   (->> (type (alignment-peg-and-hole :radius 0.3)) 
    ;(cube 0.25 0.25 1.5)
        (binding [*fn* 36])
        (rdy  90)
        (translate [-0.8 0 0.5])
        (translate (mapv + (:wall-locate3-point (calculate-control-points (key-wall-position lastcol cornerrow 1 0 :br :xy 5))))))
                        ;(rotate-around-z-in-degrees 0 offset)
                         
   (->> (type (alignment-peg-and-hole :radius 0.3))
        (binding [*fn* 36])
        (rdy  90)
        (translate [-0.8 0 -6])
        (translate (mapv + (:wall-locate3-point (calculate-control-points (key-wall-position lastcol cornerrow 1 0 :br :xy 5))))))
                               ;(rotate-around-z-in-degrees 0 offset)
                         
   (->> (type (alignment-peg-and-hole :radius 0.3))
        (binding [*fn* 36])
        (rdy  90)
        (rdz -10)
        (translate [-1.2 -0.7 3])
        (translate (mapv + (:wall-locate3-point (calculate-control-points (key-wall-position lastcol 0 1 0 :tr-rm :xy 5))))))
                               ;(rotate-around-z-in-degrees 0 offset)
                         
   (->> (type (alignment-peg-and-hole :radius 0.3))
        (binding [*fn* 36])
        (rdy  90)
        (rdz -20)
        (translate [-0.6 -0.7 -4])
        (translate (mapv + (:wall-locate3-point (calculate-control-points (key-wall-position lastcol 0 1 0 :tr-rm :xy 5))))))
                                      ;(rotate-around-z-in-degrees 0 offset)
                         
   (->> (type (alignment-peg-and-hole :radius 0.3))
        (binding [*fn* 36])
        (rdy  90)
              ; (rdz -20)
        (translate [-1.7 -0.9 2.4])
        (translate (mapv + (:wall-locate3-point (calculate-control-points (key-wall-position lastcol 1 1 0 :rm :xy 5))))))))
                                             ;(rotate-around-z-in-degrees 0 offset)
                         
   

(comment
  (spit
   "things-low/fractyl-right-wall-test.scad"
   (write-scad
    (include include-bosl2)
    (include include-bosl2-joiners)
    (let
     [spiral-width 79.280
      scale-factor 0.067
      face-scale-factor 0.9
      spiral-scaled-width (* spiral-width scale-factor)
      z-height (/ (* 69.213 scale-factor) 2)
      stroke-width 14.765
      scaled-stroke-width (* stroke-width scale-factor)
      place-nkyinkyim-spiral (fn [wall-pos & {:keys [offset x-rot z-rot y-rot level wall-elevation flip extrude-scale]
                                              :or {offset [0 0 0] x-rot 90 z-rot 0 y-rot 0
                                                   level 0 wall-elevation 0 flip  false extrude-scale face-scale-factor}}]
                               (->>
                                (cond->> (svg-import "../svg/nkyinkyim_log_spiral.svg")
                                  flip  (mirror [1 0 0]))
                                                                                      ;(mirror [1 0 0])
                                (scale [scale-factor scale-factor 1])
                                (extrude-linear {:height 1.5 :scale extrude-scale})
      
      
      
                                                                                      ;(translate [0 0 -1])
                                (rdz 0)
                                (rdx x-rot)
                                (rdy y-rot)
                                (rdz z-rot)
                                (translate
                                 (assoc (mapv + (:wall-locate3-point (calculate-control-points wall-pos))
                                              (rotate-around-z-in-degrees z-rot offset)) 2
                                        (- (+ z-height (* z-height level 2 face-scale-factor)) (* level scaled-stroke-width 0.95))))))
      nkyinkyim-chain (fn [starting-wall-pos  & {:keys [end-wall-pos chain-length offset x-rot z-rot y-rot
                                                        level wall-elevation flip extrude-scale]
                                                 :or {offset [0 0 0] x-rot 90 z-rot 0 y-rot 0
                                                      level 0 wall-elevation 0 flip  false extrude-scale face-scale-factor}}]
                        (let
                         [num-of-spirals (if chain-length chain-length
                                             (if end-wall-pos (/ (+ (magnitude (mapv - (:web-post-position-top (calculate-control-points starting-wall-pos))
                                                                                     (:web-post-position-top (calculate-control-points end-wall-pos))))
                                                                    (/ spiral-scaled-width 2))
                                                                 spiral-scaled-width)
                                                 (ceil (/ mount-width spiral-scaled-width))))
                          initial-shape (place-nkyinkyim-spiral starting-wall-pos :offset offset :x-rot x-rot
                                                                :z-rot z-rot :y-rot y-rot
                                                                :level level :wall-elevation wall-elevation
                                                                :flip flip :extrude-scale extrude-scale)]
                         (apply union (for [i (range num-of-spirals)
                                            :let [translation (rotate-around-z-in-degrees z-rot [(- (* i spiral-scaled-width) (* i stroke-width scale-factor 1.45)) 0 0])]]
                                        (do (println "translation" translation)
                                            (translate  translation initial-shape))))))
      x-offset -1
      y-offset-bl 0.23
      x-rot-bl 90]
     (union 
      (right-side-alignment-positions :peg)
      (-#(union
       
          (translate [0 -2.5 0](rdz 90 (snap-pin "tiny" :l 1 :d 0.625 :snap 0.0125 :nub-depth 0.225 :thickness 0.07 :preload 0.025)))
          (rdz 90 (snap-pin "tiny" :l 2 :d 1.25 :snap 0.125 :nub-depth 0.45 :thickness 0.3 :preload 0.05))
          (translate [0 2.5 0](rdz 90 (snap-pin "tiny")))
       
          (nkyinkyim-chain (key-wall-position lastcol cornerrow 1 0 :br :xy 5)
                               ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                           :chain-length 4
                           :z-rot 90;(- 90 pinky-splay)
                           :offset [0 0.5 0]
                           :level 0 :flip false :x-rot x-rot-bl)
          (nkyinkyim-chain (key-wall-position lastcol cornerrow 1 0 :br :xy 5)
                                      ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                           :chain-length 3
                           :z-rot 90;(- 90 pinky-splay)
                           :offset [0 0.5 0]
                           :level 1 :flip true :x-rot x-rot-bl)
          (nkyinkyim-chain (key-wall-position lastcol cornerrow 1 0 :br :xy 5)
                                             ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                           :chain-length 2
                           :z-rot 90;(- 90 pinky-splay)
                           :offset [0 0.5 0]
                           :level 2 :flip false :x-rot x-rot-bl)
          (nkyinkyim-chain (key-wall-position lastcol cornerrow 1 0 :br :xy 5)
                                                    ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                           :chain-length 1
                           :z-rot 90;(- 90 pinky-splay)
                           :offset [0 0.5 0]
                           :level 3 :flip true :x-rot x-rot-bl)
          (nkyinkyim-chain (key-wall-position lastcol cornerrow 1 0 :tr :xy 5)
                                      ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                           :chain-length 1
                           :z-rot 90;(- 90 pinky-splay)
                           :offset [0.44 0.61 0]
                           :level 0 :flip false :x-rot (- x-rot-bl 4))
          (nkyinkyim-chain (key-wall-position lastcol 2 1 0 :tr :xy 5)
                                          ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                           :chain-length 2
                           :z-rot (+ 90 (/ pinky-splay 2))
                           :offset [4.3 0.72 0]
                           :level 0 :flip false :x-rot (- x-rot-bl 6))
          (nkyinkyim-chain (key-wall-position lastcol 1 1 0 :br :xy 5)
                                                ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                           :chain-length 2
                           :z-rot (+ 90 -5)
                           :offset [1.65 0.75 0]
                           :level 0 :flip false :x-rot (- x-rot-bl 8))
          (nkyinkyim-chain (key-wall-position lastcol 1 1 0 :rm :xy 5)
                                                       ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                           :chain-length 3
                           :z-rot (+ 90 pinky-splay)
                           :offset [0.825 1. 0]
                           :level 0 :flip false :x-rot (- x-rot-bl 10))
          (nkyinkyim-chain (key-wall-position lastcol 1 1 0 :tr :xy 5)
                                                              ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                           :chain-length 2
                           :z-rot (+ 90 (* pinky-splay 1.5))
                           :offset [3.85 1.05 0]
                           :level 0 :flip false :x-rot (- x-rot-bl 7))
          (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :br :xy 5)
                                                                    ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                           :chain-length 1
                           :z-rot (+ 90 (* pinky-splay 2))
                           :offset [1.05 0.81 0]
                           :level 0 :flip false :x-rot (- x-rot-bl 4))
          (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :br-rm :xy 5)
                                                                           ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                           :chain-length 1
                           :z-rot (+ 90 (* pinky-splay 2))
                           :offset [1.05 0.7 0]
                           :level 0 :flip false :x-rot (- x-rot-bl 1))
      ;; (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :br-rm :xy 5)
      ;;                                                                         ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
      ;;                  :chain-length 1
      ;;                  :z-rot (+ 90 (* pinky-splay 2))
      ;;                  :offset [1.05 0.7 0]
      ;;                  :level 1 :flip true :x-rot (- x-rot-bl 1))
          (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :rm :xy 5)
                                                                                  ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                           :chain-length 1
                           :z-rot (+ 90 (* pinky-splay 2.5))
                           :offset [1.05 0.55 0]
                           :level 0 :flip false :x-rot (+ x-rot-bl 2))
          (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :rm :xy 5)
                                                                                        ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                           :chain-length 1
                           :z-rot (+ 90 (* pinky-splay 2.5))
                           :offset [1.05 0.55 0]
                           :level 1 :flip true :x-rot (+ x-rot-bl 2))
      ;;  (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :rm :xy 5)
      ;;                                                                                ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
      ;;                   :chain-length 1
      ;;                   :z-rot (+ 90 (* pinky-splay 2.5))
      ;;                   :offset [1.05 0.55 0]
      ;;                   :level 2 :flip false :x-rot (+ x-rot-bl 2))
          (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :tr-rm :xy 5)
                                                                                        ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                           :chain-length 1
                           :z-rot (+ 90 (* pinky-splay 2.25))
                           :offset [1.05 0.22 0]
                           :level 0 :flip false :x-rot (+ x-rot-bl 0))
          (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :tr-rm :xy 5)
                                                                                               ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                           :chain-length 1
                           :z-rot (+ 90 (* pinky-splay 2.25))
                           :offset [1.05 0.22 0]
                           :level 1 :flip true :x-rot (+ x-rot-bl 0))
          (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :tr-rm :xy 5)
                                                                                               ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
                           :chain-length 1
                           :z-rot (+ 90 (* pinky-splay 2.25))
                           :offset [1.05 0.22 0]
                           :level 2 :flip false :x-rot (+ x-rot-bl 0))
      ;;  (nkyinkyim-chain (key-wall-position lastcol 0 1 0 :tr-rm :xy 5)
      ;;                                                                                              ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
      ;;                   :chain-length 1
      ;;                   :z-rot (+ 90 (* pinky-splay 2.25))
      ;;                   :offset [1.05 0.22 0]
      ;;                   :level 3 :flip true :x-rot (+ x-rot-bl 0))
       
           ;;  (nkyinkyim-chain (key-wall-position lastcol 2 1 0 :tr :xy 5)
      ;;                           ;;:end-wall-pos (key-wall-position lastcol 1 1 0 :tr :xy 5)
      ;;                   :chain-length 6
      ;;                   :z-rot (+ 90 pinky-splay)
      ;;                   :offset [4.4 0.5 0]
      ;;                   :level 0 :flip false :x-rot x-rot-bl)
           ;;right-wall-nkynkyim-smaller
          (-# (vnf-polyhedron (:fractyl-right-wall-vnf  (fractyl-right-wall 5 5))))
           ;(front-wall-nurbs 10 30)

          (vnf-polyhedron (wall-vnf (fractyl-back-wall 5 5) default-vnf-vertex-array-args))
          key-holes)))))))
       
       
       
       
      ;;  (map #(apply place-nkyinkyim-spiral %)
      ;;       (mapv #(concat % [:x-extra-offset -3 :extrude-scale 1])
      ;;             [r2-tr r2-r1 r1-br r1-br-rm r1-tr-rm r1-tr r1-r0 r0-br]))
       
       
       
  
(comment (mapv + (:wall-locate3-point (calculate-control-points (key-wall-position 4 2 1 0 :tr))) [0 1 3.8]))
(comment (mapv + (:wall-locate3-point (calculate-control-points (key-wall-position 4 2 1 0 :rm))) [0 0 -0.7]))
(comment (- 5.44421158845456 5.025555231351336))
(comment (spit "things-low/horizontal-first-test.scad"
               (write-scad
                (include "../BOSL2/std.scad")
                (let [{front-wall-wall-section :front-wall-wall-section 
                       chained-hull-shapes :chained-hull-shapes} (front-wall-nurbs 10 10)
                      points [[15.43255710595963 -32.58500738133914 16.491736329784636 0.9023689270621825] [12.607902973218858 -35.90827997504199 19.396617814784115 1] [10.761518350412612 -30.64963444868728 16.556049038228004 0.8535533905932737]]] 
                  (println "bm"(main-body-web-post-point-top 2 2 :bm))
                  (union key-holes
                         ;(mapv #(plot-bezier-points % (sphere 1)) points)
                         
                         chained-hull-shapes
                        ;;  (color [1 0 0 1] (plot-bezier-points (nth upper-points 0) (sphere 0.5)))
                        ;;  (color [0 1 0 1] (plot-bezier-points (nth upper-points 1) (sphere 0.5)))
                        ;;  (color [0 0 1 1] (plot-bezier-points (nth upper-points 2) (sphere 0.5)))
                        ;;  (color [1 0 0 1] (plot-bezier-points (nth lower-points 0) (sphere 0.5)))
                        ;;  (color [0 1 0 1] (plot-bezier-points (nth lower-points 1) (sphere 0.5)))
                        ;;  (color [0 0 1 1] (plot-bezier-points (nth lower-points 2) (sphere 0.5)))

                         (->> (svg-import "../svg/FUNTUNFUNEFU-DENKYEMFUNEFU.svg")
                              (scale [0.08 0.08])
                              (extrude-linear {:height 2 :scale 0.8})
                              (rdx 80)
                              (rdz -5)
                              (translate (mapv + (:wall-locate3-point (calculate-control-points (key-wall-position lastcol 2 0 -1 :bm :slant :no-slant)))
                                               [0 0.5 -7.1])))
                         (-# (vnf-polyhedron (wall-vnf front-wall-wall-section default-vnf-vertex-array-args))))))))
                       ;(-# (vnf-polyhedron (:fractyl-right-wall-vnf (fractyl-right-wall 10 10))))
                         
(defn thumb-tr-rm-to-index-br [wall-cross-section-steps wall-section-steps]
  (let [middle-bl-control-points  (calculate-control-points (key-wall-position 2 2 1 -1 :bl  :xy 3 :slant :no-slant))
        index-br-control-points  (calculate-control-points (key-wall-position 1 2 1 0 :br :xy 3 :slant :no-slant))
        thumb-tr-tr-control-points  (calculate-control-points (thumb-wall-position thumb-tr-place 1 -1 :tr :xy 4 :slant :parallel-by-d-opposite))
        thumb-tr-rm-control-points  (calculate-control-points (thumb-wall-position thumb-tr-place 1 0 :rm :xy 4  :slant :no-slant))
        thumb-tr-br-control-points (calculate-control-points (thumb-wall-position thumb-tr-place 1 0 :br :xy 4  :slant :no-slant))
        thumb-tr-br-south-east-control-points (calculate-control-points (thumb-wall-position thumb-tr-place 1 -1 :br :xy 4  :slant :no-slant :offset [0.0 -0.000001 0.0]))
        middle-bl-outer (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words middle-bl-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                           3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-tr-tr-outer (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-tr-tr-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                             3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-tr-rm-outer (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-tr-rm-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                             3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-tr-br-outer (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-tr-br-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                             3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        thumb-tr-br-south-east-outer (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words thumb-tr-br-south-east-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                                        3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        index-br-outer (nurbs-with-calculated-knot-vector (get-curve-control-points-by-key-words index-br-control-points wall-vertical-outer-nurbs-control-points-keywords)
                                                          3 default-weights-for-vertical-nurbs wall-cross-section-steps)
        middle-bl-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words middle-bl-control-points inner-wall-curve-bezier-cubic-keywords)
                                               wall-cross-section-steps)
        thumb-tr-tr-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-tr-tr-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-tr-rm-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-tr-rm-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-tr-br-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words thumb-tr-br-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        thumb-tr-br-south-east-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words  thumb-tr-br-south-east-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        index-br-inner (n-degree-bezier-curve (get-curve-control-points-by-key-words index-br-control-points inner-wall-curve-bezier-cubic-keywords) wall-cross-section-steps)
        total-wall-section-steps (* wall-section-steps 3)
        outer-wall-parameters (vec (for [index (range (inc wall-cross-section-steps))]
                                     (global-curve-interp-with-calculated-first-derivatives
                                      [(nth thumb-tr-rm-outer index)
                                       (nth thumb-tr-tr-outer index)
                                       (nth index-br-outer index)]
                                      [(mapv - (nth thumb-tr-rm-outer index) (nth thumb-tr-br-outer index))
                                       (mapv - (nth thumb-tr-tr-outer index) (nth thumb-tr-rm-outer index))
                                       (mapv -  (nth middle-bl-outer index) (nth index-br-outer index))]
                                      2 
                                      :point-paramater-calculation-method :dynamic-centripetal
                                                      ;:knot-vector-calculation-method :equal
                                      :magnitude-estimation-method 22)))
        outer-wall (vec (for [index (range (inc wall-cross-section-steps))
                              :let [params (nth outer-wall-parameters index)]]
                          (non-uniform-b-spline (:P params) 2 (:U params) total-wall-section-steps)))
        inner-wall-parameters (vec (for [index (range (inc wall-cross-section-steps))]
                          (global-curve-interp-with-calculated-first-derivatives
                           [(nth index-br-inner index)
                            (nth thumb-tr-tr-inner index)
                            (nth thumb-tr-rm-inner index)]
                           [(mapv -   (nth index-br-inner index) (nth middle-bl-inner index))
                            (mapv -  (nth thumb-tr-rm-inner index) (nth thumb-tr-tr-inner index))
                            (mapv -  (nth thumb-tr-br-inner index) (nth thumb-tr-rm-inner index))]
                           2 
                           :point-paramater-calculation-method :equal
                                ;:knot-vector-calculation-method :equal
                           :magnitude-estimation-method 1)))
        inner-wall (vec (for [index (range (inc wall-cross-section-steps))
                              :let [params (nth inner-wall-parameters index)]]
                          (non-uniform-b-spline (:P params) 2 (:U params) total-wall-section-steps)))]
       {:vnf-array (wall-vnf-array outer-wall inner-wall :args (vnf-vertex-array-args :style :quincunx))
        :outer-bottom-points (peek outer-wall)
        :inner-bottom-points (peek inner-wall)
        :outer-wall-parameters outer-wall-parameters
        :inner-wall-parameters inner-wall-parameters}))
                             
  

(comment (:web-post-position-top (calculate-control-points (thumb-wall-position thumb-tr-place 0 -1 :bm :xy 5 :slant :no-slant))))
(comment (/ 27.963244322374585 3))
(comment (/ 1 (/ 69.213 (/ 9.321081440791529 2))))
(comment (/ 69.213 8.30556))
(comment (/ 1 8.3))
(comment (magnitude [-11.947636456829152 -13.041140524357047 1.498378096282643]))

(defn thumb-alignment-positions [type]
 (union (->>
         (type (alignment-peg-and-hole))
         (rdx 90)
         (rdz (+ (thumb-place-convex-z-rotation :bl) 15))
         (translate (rotate-around-z-in-degrees (+ (thumb-place-convex-z-rotation :bl) 15) [-2 1.5 -6]))
         (translate (:wall-locate3-point (calculate-control-points (thumb-wall-position thumb-bl-place 0 -1 :bl-bm :xy 5)))))

  (->>
   (alignment-peg-and-hole)
   type
   (rdx 90)
   (rdz (+ (thumb-place-convex-z-rotation :tr) 15))
   (translate (->> (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                   (calculate-control-points)
                   (:wall-locate3-point)))
   (translate (rotate-around-z-in-degrees (+ (thumb-place-convex-z-rotation :tr) 15) [0 0.5 -13.5])))
  (->> (alignment-peg-and-hole)
       (type)
       (rdx 84)
       (rdz -41)
       (translate (rotate-around-z-in-degrees -41 [-3 0.5 0]))
       (translate (:wall-locate3-point (calculate-control-points (thumb-wall-position thumb-bl-place -1 0 :lm :xy 4 :slant :no-slant))))
       (translate [0 0 -5]))
  (->> (alignment-peg-and-hole)
       (type)
       (rdx 84)
       (rdz -41)
       (translate (rotate-around-z-in-degrees -41 [3 0.5 0]))
       (translate (:wall-locate3-point (calculate-control-points (thumb-wall-position thumb-bl-place -1 0 :lm :xy 4 :slant :no-slant))))
       (translate [0 0 -5]))))
 ;;       (translate (transform-position thumb-bl-place [0 0 0]) (rdz 50 (cube 8 8 4)))
        

(comment (spit "things-low/thumb-test.scad"
               (write-scad
                (include include-bosl2)
                (let [steps 10

                      wall-cross-section-steps 10
                      wall-section-steps  10
                      {vnf-array :vnf-array
                       outer-bottom-points :outer-bottom-points
                       inner-bottom-points :inner-bottom-points} (thumb-tr-rm-to-index-br steps steps)
                      middle-bl (wall-cross-section-parameter (key-wall-position 2 2 1 -1 :bl  :xy 3 :slant :no-slant))
                      index-br (wall-cross-section-parameter (key-wall-position 1 2 1 0 :br :xy 3 :slant :no-slant))
                      thumb-tr-tr (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 -0.8 :tr :xy 4 :slant :parallel-by-d-opposite))
                      thumb-tr-rm (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :rm :xy 4  :slant :no-slant))
                      thumb-tr-br (wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 0 :br :xy 4  :slant :no-slant))
                      end-curve-outer  (let [params (global-curve-interp-with-calculated-first-derivatives
                                                     [(thumb-web-post-point-top thumb-tr-place :rm)
                                                      (thumb-web-post-point-top thumb-tr-place :tr)
                                                      (main-body-web-post-point-top 1 2 :br)]
                                                     [(mapv - (thumb-web-post-point-top thumb-tr-place :rm) (thumb-web-post-point-top thumb-tr-place :br))
                                                      (mapv - (thumb-web-post-point-top thumb-tr-place :tr) (thumb-web-post-point-top thumb-tr-place :rm))
                                                      (mapv - (main-body-web-post-point-top 2 2 :bl) (main-body-web-post-point-top 1 2 :br))]
                                                     2
                                                     :point-paramater-calculation-method :dynamic-centripetal
                                                     :magnitude-estimation-method :farin-simple)]
                                         (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U params) (:P params)
                                                                                               2 3 (* steps 2)
                                                                                               :reverse-curve true))
                      {thumb-single-row-wall-section :wall-section
                       outer-key-gap-fn-coll :outer-key-gap-fn-coll
                       inner-key-gap-fn-coll :inner-key-gap-fn-coll} (thumb-wall-section-for-single-thumb-row-fn wall-cross-section-steps wall-section-steps)
                      {left-section-data :left-section-data
                       thumb-outer-points-fn :thumb-outer-points-fn
                       thumb-inner-points-fn :thumb-inner-points-fn
                       inner-index-to-index-connector-outer-curve-fn :inner-index-to-index-connector-outer-curve-fn
                       inner-index-to-index-connector-inner-curve-fn :inner-index-to-index-connector-inner-curve-fn} (left-section-data steps :screen-outer-curve-type :local)
                      {left-section-vnf-array :vnf-array
                       trackpad-to-main-body-data :trackpad-to-main-body-data
                       thumb-bl-to-tl-outer-curve-fn :thumb-bl-to-tl-outer-curve-fn
                       thumb-tl-to-tr-outer-curve-fn :thumb-tl-to-tr-outer-curve-fn
                       thumb-bl-to-tl-inner-curve-fn :thumb-bl-to-tl-inner-curve-fn
                       thumb-tl-to-tr-inner-curve-fn :thumb-tl-to-tr-inner-curve-fn} left-section-data
                      height 9.321081440791529
                      spiral-width 79.280
                      scale-factor 0.067
                      face-scale-factor 0.9
                      spiral-scaled-width (* spiral-width scale-factor)
                      z-height (/ (* 69.213 scale-factor) 2)
                      stroke-width 14.765
                      scaled-stroke-width (* stroke-width scale-factor)
                      width 1.6
                      place-nkyinkyim-spiral (fn [wall-pos & {:keys [offset x-rot z-rot y-rot level wall-elevation flip extrude-scale]
                                                              :or {offset [0 0 0] x-rot 90 z-rot 0 y-rot 0
                                                                   level 0 wall-elevation 0 flip  false extrude-scale face-scale-factor}}]
                                               (->>
                                                (cond->> (svg-import "../svg/nkyinkyim_log_spiral.svg")
                                                  flip  (mirror [1 0 0]))
                                                                         ;(mirror [1 0 0])
                                                (scale [scale-factor scale-factor 1])
                                                (extrude-linear {:height 1.5 :scale extrude-scale})



                                                                         ;(translate [0 0 -1])
                                                (rdz 0)
                                                (rdx x-rot)
                                                (rdy y-rot)
                                                (rdz z-rot)
                                                (translate
                                                 (assoc (mapv + (:wall-locate3-point (calculate-control-points wall-pos))
                                                              (rotate-around-z-in-degrees z-rot offset)) 2
                                                        (- (+ z-height (* z-height level 2 face-scale-factor)) (* level scaled-stroke-width 0.95))))))
                      nkyinkyim-chain (fn [starting-wall-pos  & {:keys [end-wall-pos chain-length offset x-rot z-rot y-rot
                                                                        level wall-elevation flip extrude-scale]
                                                                 :or {offset [0 0 0] x-rot 90 z-rot 0 y-rot 0
                                                                      level 0 wall-elevation 0 flip  false extrude-scale face-scale-factor}}]
                                        (let
                                         [num-of-spirals (if chain-length chain-length
                                                             (if end-wall-pos (/ (+ (magnitude (mapv - (:web-post-position-top (calculate-control-points starting-wall-pos))
                                                                                                     (:web-post-position-top (calculate-control-points end-wall-pos))))
                                                                                    (/ spiral-scaled-width 2))
                                                                                 spiral-scaled-width)
                                                                 (ceil (/ mount-width spiral-scaled-width))))
                                          initial-shape (place-nkyinkyim-spiral starting-wall-pos :offset offset :x-rot x-rot
                                                                                :z-rot z-rot :y-rot y-rot
                                                                                :level level :wall-elevation wall-elevation
                                                                                :flip flip :extrude-scale extrude-scale)]
                                         (apply union (for [i (range num-of-spirals)
                                                            :let [translation (rotate-around-z-in-degrees z-rot [(- (* i spiral-scaled-width) (* i stroke-width scale-factor 1.45)) 0 0])]]
                                                        (do (println "translation" translation)
                                                            (translate  translation initial-shape))))))
                      x-offset -1
                      y-offset-bl 0.23
                      x-rot-bl 90
                      two-level-spiral (fn [spiral-config]
                                         (union
                                          (spiral-config)
                                          (apply spiral-config [:flip true :level 1])))
                    
                      three-level-spiral (fn [spiral-config]
                                           (union
                                            (spiral-config)
                                            (apply spiral-config [:flip true :level 1])
                                            (apply spiral-config [:y-rot 180 :level 2])))
                      end-curve-outer  (let [params (global-curve-interp-with-calculated-first-derivatives
                                                     [(thumb-web-post-point-top thumb-tr-place :rm)
                                                      (thumb-web-post-point-top thumb-tr-place :tr)
                                                      (main-body-web-post-point-top 1 2 :br)]
                                                     [(mapv - (thumb-web-post-point-top thumb-tr-place :rm) (thumb-web-post-point-top thumb-tr-place :br))
                                                      (mapv - (thumb-web-post-point-top thumb-tr-place :tr) (thumb-web-post-point-top thumb-tr-place :rm))
                                                      (mapv - (main-body-web-post-point-top 2 2 :bl) (main-body-web-post-point-top 1 2 :br))]
                                                     2
                                                     :point-paramater-calculation-method :dynamic-centripetal
                                                     :magnitude-estimation-method 30)]
                      
                                         (decompose-b-spline-curve-and-calculate-bezier-curves 2 (:U params) (:P params)
                                                                                               2 3 (* steps 2)
                                                                                               :reverse-curve true))
                      ]
                  (println "z-height" (* z-height 2))
                  (println (/ mount-width spiral-scaled-width))
                  (println spiral-scaled-width)
                  (union
                   
                   ;(plot-bezier-points end-curve-outer (sphere 0.5))
                   ;(plot-bezier-points curve (sphere 0.5))
                   ;(color [1 0 0 1](plot-bezier-points end-curve-outer (sphere 0.5)))
                   ;(color [1 0 0 1](plot-bezier-points curve-2 (sphere 0.5)))
                   ;(-# nkyinkyim-on-thumb)
                   ;(thumb-alignment-positions :peg)
                  ;(key-place 1 2 des-r2)
                   ;(key-place 1 2 switch-model)
                              
                   ;;  (nkyinkyim-chain (thumb-wall-position thumb-bl-place 0 -1 :bl-bm :xy 5)
                  ;;                   :end-wall-pos (thumb-wall-position thumb-bl-place 0 -1 :br :xy 5)
                  ;;                          :z-rot (+ (thumb-place-convex-z-rotation :bl) 15)
                  ;;                          :offset [(+ -0.6 x-offset) (+ 0.6 y-offset-bl) 0]
                  ;;                          :level 0 :flip false :x-rot x-rot-bl)
                  ;;  (place-nkyinkyim-spiral (thumb-wall-position thumb-bl-place 0 -1 :br :xy 5)
                  ;;                   :end-wall-pos (thumb-wall-position thumb-tl-place 0 -1 :bl :xy 5 :slant :no-slant)
                  ;;                   :z-rot (+  (thumb-place-convex-z-rotation :tl) 22.5)
                  ;;                   :offset [(* scaled-stroke-width 0.8) 0.025 0]
                  ;;                   :level 0 :flip false :x-rot x-rot-bl)
                  ;;  (nkyinkyim-chain (thumb-wall-position thumb-tl-place 0 -1 :bl :xy 5 :slant :no-slant)
                  ;;                   :chain-length 6
                  ;;           :offset [(+ 0.25 x-offset) 0.6 0] :z-rot (+  (thumb-place-convex-z-rotation :tl) 15) :wall-elevation 0)
                  ;;  (place-nkyinkyim-spiral (thumb-wall-position thumb-tr-place 0 -1 :bl :xy 5 :slant :no-slant)
                                    
                  ;;                   :offset [-0.25 -0.2 0] :z-rot 20) 
                  ;;  (nkyinkyim-chain (thumb-wall-position thumb-tr-place 0 -1 :bl-bm :xy 5 :slant :no-slant)
                  ;;                   :end-wall-pos (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                  ;;           :offset [(+ -0.1 x-offset scaled-stroke-width) 0.05 0] :z-rot 10)
                   
                  ;;  (nkyinkyim-chain (thumb-wall-position thumb-bl-place 0 -1 :bl-bm :xy 5)
                  ;;                   :end-wall-pos (thumb-wall-position thumb-bl-place 0 -1 :br :xy 5)
                  ;;                   :z-rot (+ (thumb-place-convex-z-rotation :bl) 15)
                  ;;                   :offset [(+ -0.6 x-offset) (+ 0.6 y-offset-bl) 0]
                  ;;                   :level 0 :flip true :x-rot x-rot-bl :level 1)
                  ;;  (place-nkyinkyim-spiral (thumb-wall-position thumb-bl-place 0 -1 :br :xy 5)
                  ;;                          :end-wall-pos (thumb-wall-position thumb-tl-place 0 -1 :bl :xy 5 :slant :no-slant)
                  ;;                          :z-rot (+  (thumb-place-convex-z-rotation :tl) 22.5)
                  ;;                          :offset [(* scaled-stroke-width 0.8) 0.025 0]
                  ;;                          :level 0 :flip false :x-rot x-rot-bl
                  ;;                          :level 1 :flip true)
                  ;;  (nkyinkyim-chain (thumb-wall-position thumb-tl-place 0 -1 :bl :xy 5 :slant :no-slant)
                  ;;                   :chain-length 6
                  ;;                   :offset [(+ 0.25 x-offset) 0.6 0] :z-rot (+  (thumb-place-convex-z-rotation :tl) 15) 
                  ;;                   :level 1 :flip true) 
                  ;;  (place-nkyinkyim-spiral (thumb-wall-position thumb-tr-place 0 -1 :bl :xy 5 :slant :no-slant)
                   
                  ;;                          :offset [-0.25 -0.2 0] :z-rot 20
                  ;;                          :level 1 :flip true)
                  ;;  (nkyinkyim-chain (thumb-wall-position thumb-tr-place 0 -1 :bl-bm :xy 5 :slant :no-slant)
                  ;;                   :end-wall-pos (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                  ;;                   :offset [(+ -0.1 x-offset scaled-stroke-width) 0.05 0] :z-rot 10
                  ;;                   :level 1 :flip true)
                  ;;  (translate [0 0 (* scaled-stroke-width 0.05)](place-nkyinkyim-spiral (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                  ;;                   :end-wall-pos (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                  ;;                   :offset [(+ -0.1 x-offset scaled-stroke-width) 0.068 0] :z-rot 10
                  ;;                   :level 2 :y-rot 180))
                  ;;  (translate [0 0 (* scaled-stroke-width 0.05)] 
                  ;;             (place-nkyinkyim-spiral (thumb-wall-position thumb-tr-place 0 -1 :br-bm :xy 5 :slant :no-slant) 
                  ;;                          :offset [(+ -0.1 x-offset scaled-stroke-width) 0.062 0] :z-rot 10
                  ;;                          :level 2 :y-rot 180 :flip true))
                  ;;  (translate [0 0 (* scaled-stroke-width 0.1)] 
                  ;;             (place-nkyinkyim-spiral
                  ;;              (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                  ;;              :end-wall-pos (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                  ;;              :offset [(+ -0.1 x-offset scaled-stroke-width) 0.068 0] :z-rot 10
                  ;;              :level 3 :y-rot 0 :flip true))
                  ;;  (translate [0 0 (* scaled-stroke-width 0.1)]
                  ;;             (place-nkyinkyim-spiral (thumb-wall-position thumb-tr-place 0 -1 :br-bm :xy 5 :slant :no-slant)
                  ;;                                     :offset [(+ -0.1 x-offset scaled-stroke-width) 0.062 0] :z-rot 10
                  ;;                                     :level 3 ))
                  ;;  (translate [0 0 (* scaled-stroke-width 0.05)] (place-nkyinkyim-spiral (thumb-wall-position thumb-tr-place 0 -1 :bm :xy 5 :slant :no-slant)
                  ;;                                                                        :end-wall-pos (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                  ;;                                                                        :offset [(+ -0.1 x-offset scaled-stroke-width) 0.054 0] :z-rot 10
                  ;;                                                                        :level 2 :y-rot 180 :flip true))
                  ;;  (translate [0 0 (* scaled-stroke-width 0.1)]
                  ;;             (place-nkyinkyim-spiral
                  ;;              (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                  ;;              :end-wall-pos (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                  ;;              :offset [(+ -0.1 x-offset scaled-stroke-width) 0.068 0] :z-rot 10
                  ;;              :level 4 :y-rot 180 :flip true))
                   
                  ;;  pcb-place-thumbs
                   ;thumb-type
                   ;key-holes
                   (vnf-polyhedron (wall-vnf thumb-single-row-wall-section {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse true :style :quincunx}))
                  ;;  (key-place 1 2 des-r2)
                   (vnf-polyhedron vnf-array)
                   ;(plot-bezier-points outer-bottom-points (sphere 0.5))
                   
                   ;single-key-pcb-holder-on-thumbs
                    (vnf-polyhedron  (:wall-vnf (thumb-to-left-section-2 wall-cross-section-steps wall-section-steps thumb-outer-points-fn thumb-inner-points-fn)))
                  
                     
                  ;;  (->> (svg-import "../svg/Gye_Nyame.svg")
                  ;;       (scale [0.115 0.115])
                  ;;       (extrude-linear {:height 2 :scale 0.8})
                  ;;       (rdx 88)
                  ;;       (rdz 10) 
                  ;;       (translate (:wall-locate3-point (calculate-control-points (thumb-wall-position thumb-tr-place 0 -1 :bm :xy 5 :slant :no-slant))))
                  ;;       (translate (rotate-around-z-in-degrees 10 [-1 0 0]))
                  ;;       (translate [0 0 -8])
                  ;;       )
               ;;     (-# (->> (svg-import "../svg/dwenninem.svg")
               ;;          (scale [0.15 0.15])
               ;;          (extrude-linear {:height 2 :scale 0.8})
               ;;          (rdx 84)
               ;;          (rdz -41)
               ;;          (translate (:wall-locate3-point (calculate-control-points (thumb-wall-position thumb-bl-place -1 0 :lm :xy 4 :slant :no-slant))))
               ;;          (translate [0 0 -5])))
                   )))))
                        
                   
                   
                   ;(vnf-polyhedron (wall-vnf-array outer-wall inner-wall-2 ))
                   ;(vnf-polyhedron (vnf-vertex-array outer-wall :caps false :col-wrap false))
                   ;(vnf-polyhedron (vnf-vertex-array inner-wall-2 :caps false :col-wrap false))
                  ;;  (vnf-polyhedron
                  ;;   (wall-vnf
                  ;;    (wall-section
                  ;;     (wall-section-parameter
                  ;;      [;(wall-cross-section-parameter (thumb-wall-position thumb-tr-place 1 -1 :tr :xy 3 :slant :parallel-by-d-opposite :offset [-0.000001 -0.000001 0]))
                  ;;       thumb-tr-rm
                  ;;       thumb-tr-tr
                  ;;       index-br]
                  ;;      (global-curve-interp-with-first-derivatives-parameters
                  ;;       [(wall-cross-section-tangent-parameter thumb-tr-rm thumb-tr-br)
                  ;;        (wall-cross-section-tangent-parameter thumb-tr-tr thumb-tr-br)
                  ;;        (wall-cross-section-tangent-parameter middle-bl index-br)]
                  ;;       2
                  ;;       :point-paramater-calculation-method :circular
                  ;;       ;:knot-vector-calculation-method :equal
                  ;;       :magnitude-estimation-method :chord
                  ;;       :linear-inner-top true)
                  ;;      :calculation-order :vertical-first)
                  ;;     steps steps)
                  ;;    default-vnf-vertex-array-args))
                   

(def nkyinkyim-on-thumb 
  (let 
   [spiral-width 79.280
    scale-factor 0.067
    face-scale-factor 0.9
    spiral-scaled-width (* spiral-width scale-factor)
    z-height (/ (* 69.213 scale-factor) 2)
    stroke-width 14.765
    scaled-stroke-width (* stroke-width scale-factor)
    width 1.6
    place-nkyinkyim-spiral (fn [wall-pos & {:keys [offset x-rot z-rot y-rot level wall-elevation flip extrude-scale]
                                            :or {offset [0 0 0] x-rot 90 z-rot 0 y-rot 0
                                                 level 0 wall-elevation 0 flip  false extrude-scale face-scale-factor}}]
                             (->>
                              (cond->> (svg-import "../svg/nkyinkyim_log_spiral.svg")
                                flip  (mirror [1 0 0]))
                                                                             ;(mirror [1 0 0])
                              (scale [scale-factor scale-factor 1])
                              (extrude-linear {:height 1.5 :scale extrude-scale})
   
   
   
                                                                             ;(translate [0 0 -1])
                              (rdz 0)
                              (rdx x-rot)
                              (rdy y-rot)
                              (rdz z-rot)
                              (translate
                               (assoc (mapv + (:wall-locate3-point (calculate-control-points wall-pos))
                                            (rotate-around-z-in-degrees z-rot offset)) 2
                                      (- (+ z-height (* z-height level 2 face-scale-factor)) (* level scaled-stroke-width 0.95))))))
    nkyinkyim-chain (fn [starting-wall-pos  & {:keys [end-wall-pos chain-length offset x-rot z-rot y-rot
                                                      level wall-elevation flip extrude-scale]
                                               :or {offset [0 0 0] x-rot 90 z-rot 0 y-rot 0
                                                    level 0 wall-elevation 0 flip  false extrude-scale face-scale-factor}}]
                      (let
                       [num-of-spirals (if chain-length chain-length
                                           (if end-wall-pos (/ (+ (magnitude (mapv - (:web-post-position-top (calculate-control-points starting-wall-pos))
                                                                                   (:web-post-position-top (calculate-control-points end-wall-pos))))
                                                                  (/ spiral-scaled-width 2))
                                                               spiral-scaled-width)
                                               (ceil (/ mount-width spiral-scaled-width))))
                        initial-shape (place-nkyinkyim-spiral starting-wall-pos :offset offset :x-rot x-rot
                                                              :z-rot z-rot :y-rot y-rot
                                                              :level level :wall-elevation wall-elevation
                                                              :flip flip :extrude-scale extrude-scale)]
                       (apply union (for [i (range num-of-spirals)
                                          :let [translation (rotate-around-z-in-degrees z-rot [(- (* i spiral-scaled-width) (* i stroke-width scale-factor 1.45)) 0 0])]]
                                      (do (println "translation" translation)
                                          (translate  translation initial-shape))))))
    x-offset -1
    y-offset-bl 0.23
    x-rot-bl 90
     two-level-spiral (fn [spiral-config]
                       (union
                        (spiral-config)
                        (apply spiral-config [:flip true :level 1])))
    three-level-spiral (fn [spiral-config]
                         (union
                          (spiral-config)
                          (apply spiral-config [:flip true :level 1])
                          (apply spiral-config [:y-rot 180 :level 2])))]
                       
   (union
    (nkyinkyim-chain (thumb-wall-position thumb-bl-place 0 -1 :bl-bm :xy 5)
                     :end-wall-pos (thumb-wall-position thumb-bl-place 0 -1 :br :xy 5)
                     :z-rot (+ (thumb-place-convex-z-rotation :bl) 15)
                     :offset [(+ -0.6 x-offset) (+ 0.6 y-offset-bl) 0]
                     :level 0 :flip false :x-rot x-rot-bl)
    (place-nkyinkyim-spiral (thumb-wall-position thumb-bl-place 0 -1 :br :xy 5)
                            :end-wall-pos (thumb-wall-position thumb-tl-place 0 -1 :bl :xy 5 :slant :no-slant)
                            :z-rot (+  (thumb-place-convex-z-rotation :tl) 22.5)
                            :offset [(* scaled-stroke-width 0.8) 0.025 0]
                            :level 0 :flip false :x-rot x-rot-bl)
    (nkyinkyim-chain (thumb-wall-position thumb-tl-place 0 -1 :bl :xy 5 :slant :no-slant)
                     :chain-length 6
                     :offset [(+ 0.25 x-offset) 0.6 0] :z-rot (+  (thumb-place-convex-z-rotation :tl) 15) :wall-elevation 0)
    (place-nkyinkyim-spiral (thumb-wall-position thumb-tr-place 0 -1 :bl :xy 5 :slant :no-slant)
    
                            :offset [-0.25 -0.2 0] :z-rot 20)
    (nkyinkyim-chain (thumb-wall-position thumb-tr-place 0 -1 :bl-bm :xy 5 :slant :no-slant)
                     :end-wall-pos (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                     :offset [(+ -0.1 x-offset scaled-stroke-width) 0.05 0] :z-rot 10)
    
    (nkyinkyim-chain (thumb-wall-position thumb-bl-place 0 -1 :bl-bm :xy 5)
                     :end-wall-pos (thumb-wall-position thumb-bl-place 0 -1 :br :xy 5)
                     :z-rot (+ (thumb-place-convex-z-rotation :bl) 15)
                     :offset [(+ -0.6 x-offset) (+ 0.6 y-offset-bl) 0]
                     :level 0 :flip true :x-rot x-rot-bl :level 1)
    (place-nkyinkyim-spiral (thumb-wall-position thumb-bl-place 0 -1 :br :xy 5)
                            :end-wall-pos (thumb-wall-position thumb-tl-place 0 -1 :bl :xy 5 :slant :no-slant)
                            :z-rot (+  (thumb-place-convex-z-rotation :tl) 22.5)
                            :offset [(* scaled-stroke-width 0.8) 0.025 0]
                            :level 0 :flip false :x-rot x-rot-bl
                            :level 1 :flip true)
    (nkyinkyim-chain (thumb-wall-position thumb-tl-place 0 -1 :bl :xy 5 :slant :no-slant)
                     :chain-length 6
                     :offset [(+ 0.25 x-offset) 0.6 0] :z-rot (+  (thumb-place-convex-z-rotation :tl) 15)
                     :level 1 :flip true)
    (place-nkyinkyim-spiral (thumb-wall-position thumb-tr-place 0 -1 :bl :xy 5 :slant :no-slant)
    
                            :offset [-0.25 -0.2 0] :z-rot 20
                            :level 1 :flip true)
    (nkyinkyim-chain (thumb-wall-position thumb-tr-place 0 -1 :bl-bm :xy 5 :slant :no-slant)
                     :end-wall-pos (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                     :offset [(+ -0.1 x-offset scaled-stroke-width) 0.05 0] :z-rot 10
                     :level 1 :flip true)
    (translate [0 0 (* scaled-stroke-width 0.05)] (place-nkyinkyim-spiral (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                                                                          :end-wall-pos (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                                                                          :offset [(+ -0.1 x-offset scaled-stroke-width) 0.068 0] :z-rot 10
                                                                          :level 2 :y-rot 180))
    (translate [0 0 (* scaled-stroke-width 0.05)]
               (place-nkyinkyim-spiral (thumb-wall-position thumb-tr-place 0 -1 :br-bm :xy 5 :slant :no-slant)
                                       :offset [(+ -0.1 x-offset scaled-stroke-width) 0.062 0] :z-rot 10
                                       :level 2 :y-rot 180 :flip true))
    (translate [0 0 (* scaled-stroke-width 0.1)]
               (place-nkyinkyim-spiral
                (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                :end-wall-pos (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                :offset [(+ -0.1 x-offset scaled-stroke-width) 0.068 0] :z-rot 10
                :level 3 :y-rot 0 :flip true))
    (translate [0 0 (* scaled-stroke-width 0.1)]
               (place-nkyinkyim-spiral (thumb-wall-position thumb-tr-place 0 -1 :br-bm :xy 5 :slant :no-slant)
                                       :offset [(+ -0.1 x-offset scaled-stroke-width) 0.062 0] :z-rot 10
                                       :level 3))
    (translate [0 0 (* scaled-stroke-width 0.05)] (place-nkyinkyim-spiral (thumb-wall-position thumb-tr-place 0 -1 :bm :xy 5 :slant :no-slant)
                                                                          :end-wall-pos (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                                                                          :offset [(+ -0.1 x-offset scaled-stroke-width) 0.054 0] :z-rot 10
                                                                          :level 2 :y-rot 180 :flip true))
    (translate [0 0 (* scaled-stroke-width 0.1)]
               (place-nkyinkyim-spiral
                (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                :end-wall-pos (thumb-wall-position thumb-tr-place 0 -1 :br :xy 5 :slant :no-slant)
                :offset [(+ -0.1 x-offset scaled-stroke-width) 0.068 0] :z-rot 10
                :level 4 :y-rot 180 :flip true)))))    
   
  

(comment (apply (partial map + [0 1 2] [4 1 2]) [[1 1 1]]))
(defn front-wall-alignment-positions [type]
  (union
   (->> (type (alignment-peg-and-hole :radius 1))

        (rdx 80)

        (translate (mapv + (:wall-locate3-point (calculate-control-points (key-wall-position lastcol 2 0 -1 :bm :slant :no-slant)))
                         [0 1 -7.1])))
   (->> (type (alignment-peg-and-hole :radius 0.6))

        (rdx 80)

        (translate (mapv + (:wall-locate3-point (calculate-control-points (key-wall-position lastcol 2 0 -1 :bm :slant :no-slant)))
                         [0 1.5 -2.1])))))
(def alignment-positions
  [Ananse-Ntontan-alignment-positions
   Onyakopon-Aniwa-alignment-positions
   right-side-alignment-positions 
   thumb-alignment-positions
   back-wall-alignment-positions
   front-wall-alignment-positions])
  
  
(defn case-symbols [&{:keys [side] :or {side :right}}] 
  (let [mirror-fn (fn [shape] (if (= side :left) (mirror [1 0 0] shape)
                                  shape))]
    (union (union
  ;;  (->> (svg-import "../svg/Gye_Nyame.svg")
  ;;       (scale [0.115 0.115])
  ;;       (extrude-linear {:height 2 :scale 0.8})
  ;;       (mirror-fn)
  ;;       (rdx 88)
  ;;       (rdz 10)
  ;;       (translate (:wall-locate3-point (calculate-control-points (thumb-wall-position thumb-tr-place 0 -1 :bm :xy 5 :slant :no-slant))))
  ;;       (translate (rotate-around-z-in-degrees 10 [-1 0 0]))
  ;;       (translate [0 0 -8]))
     (->> (svg-import "../svg/dwenninem.svg")
          (scale [0.15 0.15])
          (extrude-linear {:height 2 :scale 0.8})
          (mirror-fn)
          (rdx 84)
          (rdz -41)
          (translate (:wall-locate3-point (calculate-control-points (thumb-wall-position thumb-bl-place -1 0 :lm :xy 4 :slant :no-slant))))
          (translate [0 0 -5]))
     (->> (svg-import "../svg/Ananse-Ntontan.svg")

                 ;(translate [(* (/ -3 2) 25 ) (* (/ -3 2) 23.3 )])
          (scale [0.3 0.3 1])
          (extrude-linear {:height 2 :scale 0.8})
          (mirror-fn)
          (rdx 90)
          (rdz 14)
          (translate (mapv + (div (mapv + (:wall-locate3-point (calculate-control-points (tps-65-wall-position :bm :south)))
                                        (:wall-locate3-point (calculate-control-points (tps-65-to-screen-wall-position :bl :south :offset [0 -0.0001 0]))))
                                  2) [0 0.2 0])))
     (->> (svg-import "../svg/FUNTUNFUNEFU-DENKYEMFUNEFU.svg")
          (scale [0.08 0.08])
          (extrude-linear {:height 2 :scale 0.8})
          (mirror-fn)
          (rdx 80)
          (rdz -5)
          (translate (mapv + (:wall-locate3-point (calculate-control-points (key-wall-position lastcol 2 0 -1 :bm :slant :no-slant)))
                           [0 0.5 -7.1])))
     (->> (svg-import "../svg/FUNTUNFUNEFU-DENKYEMFUNEFU.svg")
          (scale [0.07 0.07])
          (extrude-linear {:height 2 :scale 0.8})
          (mirror-fn)
          (rdz 180)
          (rdx -85)
          (rdz -5)
          (translate (mapv + (:wall-locate3-point (calculate-control-points (key-wall-position lastcol 0 0 1 :tm)))
                           [0 -0.5 -7.1])))
     nkyinkyim-on-thumb
     (difference right-wall-nkynkyim-smaller
                 right-side-symbol-floor-diff)
     (->>
      (svg-import "../svg/Onyakopon_Aniwa.svg")
      (scale [0.03 0.03 1])
      (extrude-linear {:height 3 :scale 0.8})
                ;(translate [0 0 -1])
      (rdz -10)
      (rdy -88)
      (rdz 5)
      (translate (mapv + (div (mapv + (:wall-locate3-point (calculate-control-points (tps-65-wall-position :tl-lm :west)))
                                    (:wall-locate3-point (calculate-control-points (tps-65-wall-position :tl :west :offset [-0.00000001 0.0 0.0])))) 2)
                       [0 2.5 -14]))))
                       
                    ;;    (->> (mapv #(% :peg) alignment-positions)
                    ;;         (mapv #(mirror-fn %)))
                       )
                       ))
   



(comment
  (spit "things-low/fractyl-front-wall-test.scad"
        (write-scad
         (include include-bosl2)
         (let [{front-wall-wall-section :front-wall-wall-section
                chained-hull-shapes :chained-hull-shapes} (front-wall-nurbs 10 10)
               {fractyl-right-wall-vnf :fractyl-right-wall-vnf
                key-gap-outer-curve-fn-coll :key-gap-outer-curve-fn-coll
                key-gap-inner-curve-fn-coll :key-gap-inner-curve-fn-coll} (fractyl-right-wall 10 10)]
           (union
          
            (-#(vnf-polyhedron (wall-vnf front-wall-wall-section default-vnf-vertex-array-args)))
            (-# (vnf-polyhedron fractyl-right-wall-vnf))
            chained-hull-shapes
            (->> (svg-import "../svg/FUNTUNFUNEFU-DENKYEMFUNEFU.svg")
                (scale [0.08 0.08])
                (extrude-linear {:height 2 :scale 0.8}) 
                (rdx 80)
                (rdz -5)
                (translate (mapv + (:wall-locate3-point (calculate-control-points (key-wall-position lastcol 2 0 -1 :bm :slant :no-slant)))
                                 [0 0.5 -7.1]))
                -#)
            (front-wall-alignment-positions :peg))))))
          

(comment (spit "things-low/fractyl-case-walls-test.scad"
          (write-scad
           (include include-bosl2)
           (let [steps 10
             
                 wall-cross-section-steps 10
                 wall-section-steps  10
                 {thumb-single-row-wall-section :wall-section 
                  outer-key-gap-fn-coll :outer-key-gap-fn-coll 
                  inner-key-gap-fn-coll :inner-key-gap-fn-coll} (thumb-wall-section-for-single-thumb-row-fn wall-cross-section-steps wall-section-steps)
                 fractyl-back-wall-wall-secton (fractyl-back-wall wall-cross-section-steps wall-section-steps)
             
                 {front-wall-wall-section :front-wall-wall-section 
                  chained-hull-shapes :chained-hull-shapes}(front-wall-nurbs wall-cross-section-steps wall-section-steps)
                 {left-section-data :left-section-data
                  thumb-outer-points-fn :thumb-outer-points-fn
                  thumb-inner-points-fn :thumb-inner-points-fn
                  inner-index-to-index-connector-outer-curve-fn :inner-index-to-index-connector-outer-curve-fn
                  inner-index-to-index-connector-inner-curve-fn :inner-index-to-index-connector-inner-curve-fn} (left-section-data steps :screen-outer-curve-type :local)
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
                 {thumb-tr-rm-to-index-br-vnf-array :vnf-array
                  thumb-tr-rm-to-index-br-outer-bottom-points :outer-bottom-points
                  thumb-tr-rm-to-index-br-inner-bottom-points :inner-bottom-points} (thumb-tr-rm-to-index-br (* wall-cross-section-steps 2) wall-section-steps)
                 {thumb-bl-to-tl-vnf :thumb-bl-to-tl-vnf
                  thumb-tl-to-tr-vnf :thumb-tl-to-tr-vnf} (thumb-connecter-1-row steps :thumb-bl-to-tl-P-u-zero-outer (vec (reverse (thumb-bl-to-tl-outer-curve-fn steps)))
                                                                                 :thumb-tl-to-tr-P-u-zero-outer (vec (reverse (thumb-tl-to-tr-outer-curve-fn steps)))
                                                                                 :thumb-bl-to-tl-P-u-one-inner (thumb-bl-to-tl-inner-curve-fn steps) 
                                                                                 :thumb-tl-to-tr-P-u-one-inner (thumb-tl-to-tr-inner-curve-fn steps)
                                                                                 :thumb-bl-to-tl-P-u-one-outer ((nth outer-key-gap-fn-coll 1) steps)
                                                                                 :thumb-tl-to-tr-P-u-one-outer ((nth outer-key-gap-fn-coll 0) steps))]
                                                                                 ;:thumb-bl-to-tl-P-u-zero-inner ((nth inner-key-gap-fn-coll 1) steps)
                                                                                 ;:thumb-tl-to-tr-P-u-zero-inner ((nth inner-key-gap-fn-coll 0) steps)
                                                                              
             (union
              fractyl-screw-insert-outers
              (case-symbols)
              ;(let [height 55](-# (translate [-30 -20 (/ height 2)] (cube 190 140 height))))
              chained-hull-shapes 
              (screen-holder-place-side screen-holder) 
              (fractyl-switch-plate steps
                                     inner-index-to-index-connector-outer-curve-fn inner-index-to-index-connector-inner-curve-fn
                                     left-side-key-gap-outer-curve-fn-coll left-side-key-gap-inner-curve-fn-coll
                                     key-gap-outer-curve-fn-coll key-gap-inner-curve-fn-coll)
              thumb-type
       
          
              key-holes
              (vnf-polyhedron thumb-tr-rm-to-index-br-vnf-array)
              (vnf-polyhedron trackpad-to-main-body-vnf)
              (-# (vnf-polyhedron fractyl-right-wall-vnf))
              (vnf-polyhedron (wall-vnf thumb-single-row-wall-section {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse true :style :default}))
              (vnf-polyhedron  (:wall-vnf (thumb-to-left-section-2 wall-cross-section-steps wall-section-steps thumb-outer-points-fn thumb-inner-points-fn)))
              (vnf-polyhedron left-section-vnf-array)
              (vnf-polyhedron thumb-bl-to-tl-vnf)
              (vnf-polyhedron thumb-tl-to-tr-vnf)

              (-# (vnf-polyhedron (wall-vnf fractyl-back-wall-wall-secton default-vnf-vertex-array-args)))
              (vnf-polyhedron (wall-vnf front-wall-wall-section default-vnf-vertex-array-args)))))))
       

(comment (spit "things-low/fractyl-thumb-test-redo.scad"
               (write-scad
                (include include-bosl2)
                (let [steps 10 
                      wall-cross-section-steps 10 
                      wall-section-steps 10
                      {left-section-data :left-section-data
                       thumb-outer-points-fn :thumb-outer-points-fn
                       thumb-top-outer-points-fn :thumb-top-outer-points-fn
                       thumb-inner-points-fn :thumb-inner-points-fn 
                       thumb-top-inner-points-fn :thumb-top-inner-points-fn 
                       inner-index-to-index-connector-outer-curve-fn :inner-index-to-index-connector-outer-curve-fn
                       inner-index-to-index-connector-inner-curve-fn :inner-index-to-index-connector-inner-curve-fn} (left-section-data steps :screen-outer-curve-type :local)
                      {left-section-vnf-array :vnf-array
                       trackpad-to-main-body-data :trackpad-to-main-body-data
                       thumb-bl-to-tl-outer-curve-fn :thumb-bl-to-tl-outer-curve-fn
                       thumb-tl-to-tr-outer-curve-fn :thumb-tl-to-tr-outer-curve-fn
                       thumb-bl-to-tl-inner-curve-fn :thumb-bl-to-tl-inner-curve-fn
                       thumb-tl-to-tr-inner-curve-fn :thumb-tl-to-tr-inner-curve-fn} left-section-data]
                  (union
                   (plot-bezier-points (thumb-top-outer-points-fn steps) (sphere 0.5))
                   (vnf-polyhedron  (:wall-vnf (thumb-to-left-section steps steps thumb-outer-points-fn thumb-inner-points-fn thumb-top-outer-points-fn  thumb-top-inner-points-fn)))
                   (vnf-polyhedron left-section-vnf-array))))))
                  
                

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
                                                                             degree)]]
                            ;segments-steps
                                                                         
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
              (fractyl-right-wall wall-cross-section-steps wall-section-steps))))))
          
         
       


(defn fractyl-body [wall-cross-section-steps
                    wall-section-steps &{:keys [steps side show-aviator-assembly show-aviator-assembly-diffs trackpoint-cutout] 
                                         :or {steps wall-section-steps
                                              side :right
                                              show-aviator-assembly true
                                              show-aviator-assembly-diffs true
                                              trackpoint-cutout false}}]
  (let [{thumb-single-row-wall-section :wall-section
         outer-key-gap-fn-coll :outer-key-gap-fn-coll
         inner-key-gap-fn-coll :inner-key-gap-fn-coll} (thumb-wall-section-for-single-thumb-row-fn wall-cross-section-steps wall-section-steps)
        fractyl-back-wall-wall-section (fractyl-back-wall wall-cross-section-steps wall-section-steps)

        {front-wall-wall-section :front-wall-wall-section
         chained-hull-shapes :chained-hull-shapes} (front-wall-nurbs wall-cross-section-steps wall-section-steps)
        {left-section-data :left-section-data
         thumb-outer-points-fn :thumb-outer-points-fn
         thumb-inner-points-fn :thumb-inner-points-fn
         thumb-top-outer-points-fn :thumb-top-outer-points-fn
         thumb-top-inner-points-fn :thumb-top-inner-points-fn
         inner-index-to-index-connector-outer-curve-fn :inner-index-to-index-connector-outer-curve-fn
         inner-index-to-index-connector-inner-curve-fn :inner-index-to-index-connector-inner-curve-fn
         left-section-outer-wall-parameters :outer-wall-parameters
         left-section-inner-wall-parameters :inner-wall-parameters
         } (left-section-data steps :screen-outer-curve-type :local)
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
        {thumb-tr-rm-to-index-br-vnf-array :vnf-array
         thumb-tr-rm-to-index-br-outer-bottom-points :outer-bottom-points
         thumb-tr-rm-to-index-br-inner-bottom-points :inner-bottom-points
         thumb-tr-rm-to-index-br-outer-wall-parameters :outer-wall-parameters
         thumb-tr-rm-to-index-br-inner-wall-parameters :inner-wall-parameters} (thumb-tr-rm-to-index-br (* wall-cross-section-steps 2) wall-section-steps)
        {thumb-bl-to-tl-vnf :thumb-bl-to-tl-vnf
         thumb-tl-to-tr-vnf :thumb-tl-to-tr-vnf} (thumb-connecter-1-row steps :thumb-bl-to-tl-P-u-zero-outer (vec (reverse (thumb-bl-to-tl-outer-curve-fn steps)))
                                                                        :thumb-tl-to-tr-P-u-zero-outer (vec (reverse (thumb-tl-to-tr-outer-curve-fn steps)))
                                                                        :thumb-bl-to-tl-P-u-one-inner (thumb-bl-to-tl-inner-curve-fn steps)
                                                                        :thumb-tl-to-tr-P-u-one-inner (thumb-tl-to-tr-inner-curve-fn steps)
                                                                        :thumb-bl-to-tl-P-u-one-outer ((nth outer-key-gap-fn-coll 1) steps)
                                                                        :thumb-tl-to-tr-P-u-one-outer ((nth outer-key-gap-fn-coll 0) steps))
                                                                             ;:thumb-bl-to-tl-P-u-zero-inner ((nth inner-key-gap-fn-coll 1) steps)
                                                                             ;:thumb-tl-to-tr-P-u-zero-inner ((nth inner-key-gap-fn-coll 0) steps)
                                                                        
        {thumb-tr-rm-to-index-br-to-left-section-fix-vnf-array :thumb-tr-rm-to-index-br-to-left-section-fix-vnf-array} (thumb-tr-rm-to-index-br-to-left-section-fix left-section-outer-wall-parameters left-section-inner-wall-parameters thumb-tr-rm-to-index-br-outer-wall-parameters thumb-tr-rm-to-index-br-inner-wall-parameters
        wall-cross-section-steps wall-section-steps                                                                                                                                   )
                                                                       switch-plate (fractyl-switch-plate steps
                                           inner-index-to-index-connector-outer-curve-fn inner-index-to-index-connector-inner-curve-fn
                                           left-side-key-gap-outer-curve-fn-coll left-side-key-gap-inner-curve-fn-coll
                                           key-gap-outer-curve-fn-coll key-gap-inner-curve-fn-coll)]
   (cond->> (difference (union
                         
                         
                         fractyl-screw-insert-outers
                         ;(case-symbols :side side)
          ;(let [height 55](-# (translate [-30 -20 (/ height 2)] (cube 190 140 height))))
                         chained-hull-shapes
                         
                         (tps-65-place (difference  tps-65-mount-new
                                                    tps-65
                                                     tps-65-mount-cutout
                                                    (translate [0 0 -1]tps-65-mount-cutout)
                                                    (translate [0 0 (+ 0 1)] tps-65-mount-main-cutout)))

                         (if trackpoint-cutout 
                           (difference
                            switch-plate
                            (sk8707-51-place sk8707-51-stem-cutout))
                           switch-plate)
                           
                         thumb-type
                         (vybronics-vl91022-place vybronics-vl91022-mount)
                         key-holes
                         (difference 
                          (vnf-polyhedron thumb-tr-rm-to-index-br-vnf-array)
                          ;(thumb-tr-place MxLEDBitPCB-clearance-smaller)
                          )
                         (vnf-polyhedron trackpad-to-main-body-vnf)
                         (vnf-polyhedron fractyl-right-wall-vnf)
                         (difference
                          (vnf-polyhedron (wall-vnf thumb-single-row-wall-section {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse true :style :default}))
                          (thumb-bl-place MxLEDBitPCB-clearance-smaller)
                          (thumb-tl-place MxLEDBitPCB-clearance-smaller)
                          ;(thumb-tr-place MxLEDBitPCB-clearance-smaller)
                          (key-place 1 2 MxLEDBitPCB-clearance-smaller))
                         (difference
                          (vnf-polyhedron  (:wall-vnf (thumb-to-left-section wall-cross-section-steps wall-section-steps thumb-outer-points-fn thumb-inner-points-fn thumb-top-outer-points-fn  thumb-top-inner-points-fn)))
                          (thumb-bl-place MxLEDBitPCB-clearance-smaller))
                         single-key-pcb-holder-on-main-body
                         single-key-pcb-holder-on-thumbs
                         (vnf-polyhedron thumb-bl-to-tl-vnf)
                         (vnf-polyhedron thumb-tl-to-tr-vnf)
             ;(key-place 2 2 MxLEDBitPCB)
             ;(key-place 0 2 MxLEDBitPCB)
                         (difference
                          (thumb-tr-place (intersection MxLEDBitPCB-clearance-smaller
                                                        single-key-pcb-holder-north-leg))
                          (thumb-tr-place single-plate))
                         (key-place 0 2 single-key-pcb-holder-north-leg)
                         (key-place 0 2 (intersection single-key-pcb-holder-south-leg
                                                      MxLEDBitPCB-clearance-smaller))
                         (difference
                          (union
                           (vnf-polyhedron left-section-vnf-array)
                           (screen-holder-place-side screen-holder)
                           (vnf-polyhedron thumb-tr-rm-to-index-br-to-left-section-fix-vnf-array)
                        ;(screen-holder-place-side screen-holder)
                           (cond show-aviator-assembly (aviator-assembly-polyhedron :side side)))
                          ;(thumb-tr-place MxLEDBitPCB-clearance-smaller)
                          (key-place 0 2 MxLEDBitPCB-clearance-smaller)
                          (cond show-aviator-assembly-diffs aviator-assembly-diffs)
                          (screen-holder-place-side screen-holder-cut)
                          (screen-holder-place-side (translate [0 0 screen-holder-depth] screen-holder-cut-viewport-cut)))
                         
                         (difference (vnf-polyhedron (wall-vnf fractyl-back-wall-wall-section default-vnf-vertex-array-args))
                                     (usb-jack-place-new fractyl-usb-c-port-2 :extra-z-rot -1.5)
                                     (rp2040-plus-place rp2040-plus-mount-body-clearance :place-fn (fn [shape] (usb-jack-place-new shape :extra-z-rot -1.5)))
                                     (apply union
                                            (for [col (range ncols)]
                                              (key-place col 0 MxLEDBitPCB-clearance-smaller))))
                         (difference (vnf-polyhedron (wall-vnf front-wall-wall-section default-vnf-vertex-array-args))
                                     (key-place 2 2 MxLEDBitPCB-clearance-smaller)
                                     (key-place 3 2 MxLEDBitPCB-clearance-smaller)
                                     (key-place 4 2 MxLEDBitPCB-clearance-smaller)))
                        (mapv #(% :hole) alignment-positions)
                        
                        fractyl-screw-insert-holes)
     (= side :left) (mirror [1 0 0])))) 
  

(comment (spit "things-low/fractyl-case-right.scad"
               (write-scad
                (include include-bosl2)
                (fractyl-body 80 80 :show-aviator-assembly true :trackpoint-cutout true))))
                

(comment (spit "things-low/fractyl-symbols-right.scad"
               (write-scad
                (include include-bosl2)
                (union(difference 
                 (case-symbols :side :right)
                 (fractyl-body 10 10 :show-aviator-assembly false :trackpoint-cutout true))
                  (->> (mapv #(% :peg) alignment-positions)
                      ;(mapv #(mirror-fn %))
                       )
                 ))))

(comment (spit "things-low/fractyl-case-left.scad"
               (write-scad
                (include include-bosl2)
               (union
                (fractyl-body 80 80 :side :left :show-aviator-assembly true)
                (translate [200 0 0](cube 10 10  10))))))

(comment (spit "things-low/fractyl-symbols-left.scad"
               (write-scad
                (include include-bosl2)
                (union (difference
                        (case-symbols :side :left)
                        (fractyl-body 10 10 :show-aviator-assembly false :trackpoint-cutout false))
                       (->> (mapv #(% :peg) alignment-positions)
                      ;(mapv #(mirror [1 0 0] %))
                            )))))

(comment (spit "things-low/fractyl-tps-65-mount.scad"
               (write-scad
                (include include-bosl2)
                (difference  tps-65-mount-new
                             tps-65
                             tps-65-mount-cutout
                             (translate [0 0 -1] tps-65-mount-cutout)
                             (translate [0 0 (+ 0 1)] tps-65-mount-main-cutout))
                )))

