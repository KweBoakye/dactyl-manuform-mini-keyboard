 (ns dactyl-keyboard.low.aviator-low
   (:refer-clojure :exclude [use import])
   (:require [clojure.core.matrix :refer [add mmul]]
             [dactyl-keyboard.lib.affine-transformations :refer [rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees]]
             [dactyl-keyboard.lib.curvesandsplines.beziers :refer [bezier-cubic
                                                                   bezier-linear bezier-quadratic bezier-quartic]]
             [dactyl-keyboard.low.tps-65-placement-functions :refer [tps-65-translate-and-place-at-position tps-65-z-rotation]]
             [dactyl-keyboard.lib.geometry :refer [deg2rad]]
             [dactyl-keyboard.lib.openscad.hull :refer [join-beziers]]
             [dactyl-keyboard.lib.openscad.polyhedrons :refer [bezier-along-bezier-polyhedron-generate-front-or-back-faces generate-bezier-to-point-polyhedron]]
             [dactyl-keyboard.lib.transformations :refer [rdx rdy rdz]]
             [dactyl-keyboard.lib.vectors :refer [add-third-dimension
                                                  vec-to-matrix]]
             [dactyl-keyboard.low.case-low-functions :refer :all]
             [dactyl-keyboard.low.placement-functions-low :refer :all] ;[dactyl-keyboard.low.case-low :refer :all]
             [dactyl-keyboard.low.shape-parameters-low :refer :all] ; 
             [dactyl-keyboard.metal-tactile-button :refer :all]
             [dactyl-keyboard.switch-hole :refer :all]
             [dactyl-keyboard.tps-65 :refer :all]
             [dactyl-keyboard.utils :refer :all]
             [scad-clj.model :refer :all]
             [scad-clj.scad :refer :all]))

; code adapted from https://gist.github.com/jamiehs/de163e7d469e4fb4220e504b58613806
(def aviator-includes (map include ["../BOSL2/transforms.scad" , "../BOSL2/std.scad"] ))
(def aviator-start (map + [-42 -6.5  20] (key-position 0 0 (map + (wall-locate3 0 1) [0 (/ mount-height  2) 0]))))
(def aviator-position [(first aviator-start) (second aviator-start) 12])
(def aviator-diameter 16.2)
(def aviator-recess-start (map + [-40 -12  20] (key-position 0 0 (map + (wall-locate3 0 1) [0 (/ mount-height  2) 0]))))
(def aviator-recess-position [(first aviator-recess-start) (second aviator-recess-start) 12])
(def aviator-recess-diameter 17.6)
(def aviator-plug-connecter-ring-diameter 19)
(def aviator-plug-connecter-ring-recess-diameter (+ aviator-plug-connecter-ring-diameter 4))
(def aviator-plug-connecter-length 35.5)
(def aviator-socket-connecter-ring-diameter 21.5)
(def aviator-locking-ring-length 7.5)
(def aviator-neck-width 3)
;(def aviator-neck-supoort-width metal-tactile-button-neck-radius)
(def metal-tactile-button-hole-diameter (+ metal-tactile-button-neck-diameter 0.5))
(def metal-tactile-button-hole-radius (/ metal-tactile-button-hole-diameter 2))
(def aviator-neck-support-width (- aviator-neck-width 1))
(def aviator-neck-support-height (+ metal-tactile-button-distance-from-top-of-ball-to-top-of-neck metal-tactile-button-neck-height))
(def aviator-neck-bezier-width aviator-neck-width)
(def aviator-assembly-left-or-right-translation (- (/ aviator-diameter 2) 5))
(def font-name "Eurostile Extended Black")
(def font-size 1.8)
(def text-depth 1)

(def aviator-neck-height 4)
(def aviator-offset [0 (/ aviator-neck-height 2) 0])
(defn aviator-place-shape ([shape] (aviator-place-shape translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape](->> shape
       (rotate-z-fn 90)
   (rotate-y-fn -92)
    
       ;(rdz tps-65-z-rotation)
       ;(rdy 90)
       (tps-65-translate-and-place-at-position [(+ (- (/ tps-65-mount-width 2) tps-65-corner-radius ) wall-thickness wall-xy-offset (* aviator-neck-height 1.25) )
                                                -1 
                                                (- (+ aviator-plug-connecter-ring-diameter (/ aviator-neck-width 2)))]
                                              translate-fn rotate-x-fn rotate-y-fn rotate-z-fn )
       ;(rotate (deg2rad 90) [1 0 0])
       ;(rdz left-section-rotation-angle)
 
   
   ;(translate position)
   
       )))
(defn aviator-place-points [position points]
 (map add
  (mmul (multmatrix-z-rot tps-65-z-rotation)
   (mmul (multmatrix-z-rot -90) points)
   )
    (multmatrix-translate position))
   
 ;(mmul
  ;     (multmatrix-translate position)
   ;    (multmatrix-z-rot tps-65-z-rotation)
    ;   (multmatrix-z-rot -90) 
;       )
)
(defn aviator-place [ diameter]
  
  (aviator-place-shape  (cylinder (/  diameter 2) plate-thickness)
   
   )
  ;(translate position
   ;          (rotate (deg2rad 90) [1 0 0]
                   ;(rotate (deg2rad 45) [0 1 0]
    ;                 (translate [6 0 -10]
     ;                           (rdz tps-65-z-rotation
      ;                           (cylinder (/  diameter 2) 20)))))
  )
;(def aviator-hole (translate aviator-position
;                             (rotate (deg2rad 90) [1 0 0]
;                                     ;(rotate (deg2rad 45) [0 1 0]
;                                     (translate [3 0 0]
;                                                (cylinder (/  aviator-diameter 2) 20)))))

(def aviator-hole (aviator-place-shape    (rdx 180 (binding [*fn* 36] (cylinder (/  aviator-diameter 2) (+ wall-xy-offset wall-thickness 2) :center true)))))
(def aviator-recess-hole (translate aviator-recess-position (aviator-place-shape   (rdx 180 (binding [*fn* 36] (cylinder (/ aviator-recess-diameter 2) (+ wall-xy-offset wall-thickness) :center true))))))
(def cutout-for-metal-tactile-button (metal-tactile-button-main-body-cutout (+ wall-xy-offset wall-thickness) ))
(def aviator-male-connecter-clearence-test (translate [0 10 0] (aviator-place  aviator-plug-connecter-ring-diameter)))
(def aviator-female-connecter-clearence-test (aviator-place  aviator-socket-connecter-ring-diameter))
(def aviator-fillet (->> 
                    
                    
                     (difference  (fillet-circle 9)
                                  (translate [ 0 0 9](cube 20 20 6)) 
                                 (cube 40 40 2) )
                      
                     (rdx 180)
                     (aviator-place-shape )
                    
                         ))
;; (def aviator-neck
;;   (let [main-cyl (aviator-place-shape aviator-position (cylinder 9.25 12))
;;         sub-cyl (aviator-place-shape aviator-position (cylinder (/ aviator-diameter 2) 12.2))
;;         sub-rec (aviator-place-shape aviator-position (cube 20 20 10))
;;         ]
    
;;     (difference
;;      main-cyl
;;      sub-cyl
;;      (translate [0 5 0] sub-rec)
;;      ))
 
 ;  )
(def aviator-neck-bezier-points (bezier-curve [[aviator-neck-bezier-width 0] [0.5 0] [0 0.5] [0 aviator-neck-height]] 0.5))
(def aviator-neck-bezier-points-with-origin (into [](concat aviator-neck-bezier-points [[0 0]])))
(def aviator-neck-bezier
  (difference
   (polygon [[(- aviator-neck-bezier-width 0.01) 0] [0 0] [0 (- aviator-neck-height 0.01)]] false :convexity 10)
   (polygon aviator-neck-bezier-points false :convexity 10)
   )
  )


(def aviator-neck-shape
  (let [radius (/ aviator-plug-connecter-ring-diameter 2) square-width aviator-neck-width circ-rad (/ square-width 2)]
     (binding [*fn* 36] (extrude-rotate {:convexity 10}
                                        (union
                                         (translate [radius aviator-neck-height 0] (square circ-rad circ-rad :center false))
                                         (translate [radius 0 0] (square square-width aviator-neck-height :center false))
                                         (translate [(+ radius square-width) 0 0] aviator-neck-bezier)
                                         (translate [(+ radius  circ-rad) aviator-neck-height 0]  (difference
                                                                                                   (binding [*fn* 36] (circle circ-rad))
                                                                                                   (mirror [-1 0 0] (square circ-rad aviator-neck-height :center false))))
                                         )))

     )
  )
(defn aviator-neck-place ([shape] (aviator-neck-place translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape](->> shape
   (rotate-x-fn 180) 
(aviator-place-shape translate-fn rotate-x-fn rotate-y-fn rotate-z-fn )))
  ) 

(def aviator-assembly-neck-shape
  (->>
   (cylinder (/ aviator-neck-width 2) aviator-neck-height)
   (translate [(/ aviator-plug-connecter-ring-diameter 2) 0 0 ])
   )
  )

(def aviator-neck
  (aviator-neck-place aviator-neck-shape))

;(defn aviator-neck-place-points [points]
;(aviator-place-points aviator-position 
;                      (let [x-rot 
;                            (multmatrix-x-rot 180)] 
;   (map (fn [point](mmul 
;                    x-rot
;                    point
;                    )) points
;        ))
;)
;  ) 

(defn aviator-neck-place-points [points]
      (->>
       (mmul (multmatrix-x-rot 180) points)
          (aviator-place-points aviator-position)
          
       ) 
)

(def aviator-neck-support-bezier-points (bezier-curve [[metal-tactile-button-neck-diameter 0] [0.5 0] [0 0.5] [0 aviator-neck-support-height]] 0.5))
(def aviator-neck-support-bezier-points-with-origin (into [] (concat aviator-neck-support-bezier-points [[0 0]])))
(def aviator-neck-support-bezier
  (difference
   (polygon   [[(- metal-tactile-button-neck-diameter 0.01) 0] [0 0] [0 (- aviator-neck-support-height 0.01)]] false :convexity 10)
   (polygon  aviator-neck-support-bezier-points false :convexity 10)))

(def aviator-neck-support-shape
  (let [radius (/ (+ metal-tactile-button-neck-diameter 0.5) 2) square-width aviator-neck-support-width circ-rad (/ square-width 2)] 
     (binding [*fn* 36]
       (extrude-rotate {:convexity 10}
                       (union
                        (translate [radius aviator-neck-support-height 0] (square circ-rad circ-rad :center false))
                         (translate [radius 0 0] (square square-width aviator-neck-support-height :center false))
                        (translate [(+ radius square-width)  0 0] aviator-neck-support-bezier)
                       
                        
                        (translate [(+ radius circ-rad) aviator-neck-support-height 0] (difference
                                                                                      (binding [*fn* 36] (circle circ-rad))
                                                                                      (mirror [-1 0 0] (square circ-rad aviator-neck-support-height :center false)))))))
     )
  )

(defn aviator-neck-support-place ([dx shape] (aviator-neck-support-place dx 0 0 0 shape))
([dx x y z shape ] (aviator-neck-support-place dx x y z translate rdx rdy rdz shape)) 
  ([dx x y z translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (let [x-trans-modifier 4 x-trans (cond (neg? dx) (- dx x-trans-modifier) :else (+ dx x-trans-modifier))] 
   (->> shape
   (rotate-x-fn 180)
(translate-fn [(+ (- aviator-plug-connecter-ring-diameter metal-tactile-button-hole-radius) y) (+ x-trans x)  z])
(aviator-place-shape translate-fn rotate-x-fn rotate-y-fn rotate-z-fn )
   )))
  )
(defn aviator-neck-support [dx]
  (aviator-neck-support-place dx aviator-neck-support-shape)
  )

(def aviator-neck-support-left
  (aviator-neck-support  (- aviator-assembly-left-or-right-translation))
  )

(def aviator-neck-support-right
  (aviator-neck-support   aviator-assembly-left-or-right-translation)
                        )
(def aviator-assembly-neck
 (->>
 (translate [(+ (/ aviator-plug-connecter-ring-diameter 2) (/ aviator-neck-width 1)) 0 0] (cylinder (/ aviator-neck-width 2) aviator-neck-height :center false))
 (rdz -90)

 )
  )

(defn aviator-assembly-neck-bezier [side]
  (let [x-trans (+ (/ aviator-plug-connecter-ring-diameter 2) (/ aviator-neck-width 1))]
    (mapv (fn [point]
      (->>
    (translate point (sphere 0.01))                                                                          
   (translate [x-trans 0 0] )
   (rdx 90)
   (rdz (case side "left" 100 "right" -100))
   (aviator-neck-place)
   ))
          (mapv add-third-dimension aviator-neck-bezier-points-with-origin)
         )
    )
  )

(defn transform-2d-to-matrix [points]
  (->> points
   (map add-third-dimension)
   (map vec-to-matrix)
   )
  )

;(defn aviator-assembly-neck-bezier [side]
;  (mapv (fn [point](->> 
;                   
;   (mapv add point (multmatrix-translate [(+ (/ aviator-male-connecter-ring-diameter 2) (/ aviator-neck-width 1)) 0 0]))
;    ;(mmul (multmatrix-x-rot 90))
;    ;(mmul (multmatrix-z-rot (case side "left" 120 "right" -120)))
;    ;(aviator-neck-place-points) 
;   ))
;       (transform-2d-to-matrix aviator-neck-bezier-points-with-origin)
;  )
;)
;
;(defn aviator-assembly-neck-bezier [side]
;  (let [trans (multmatrix-translate [(+ (/ aviator-male-connecter-ring-diameter 2) (/ aviator-neck-width 1)) 0 0])
;        x-rot (multmatrix-x-rot 90)
;        z-rot (multmatrix-z-rot (case side "left" 120 "right" -120))
;        ]
;    
;    (->> (map (fn [point]
;                (map * 
;                      
;                      (mmul
;                       ; (aviator-neck-place-points)
;                       (multmatrix-translate aviator-position)
;                       (multmatrix-x-rot 180)
;                       (multmatrix-z-rot tps-65-z-rotation)
;                       (multmatrix-z-rot -90)
;                       z-rot
;                       x-rot
;                       trans)
;                    point    )
;       )
;          (transform-2d-to-matrix aviator-neck-bezier-points-with-origin))
;     ;(map (fn [point] [(nth point) (second point ) (nth point 2)]))
;         ) )
;  )

(def panel-switch-diameter 12)
(def panel-switch-radius (/ panel-switch-diameter 2))

(defn aviator-assembly-neck-support-bezier [side]
  (let [x-trans (+ metal-tactile-button-hole-radius aviator-neck-support-width) half-d aviator-assembly-left-or-right-translation]
    (mapv 
     (fn [point](->>
                 (translate point (sphere 0.01))
   (translate [x-trans 0 0] )
   (rdx 90)
   (rdz (case side "left" 90 "right" -90 ("top-left" "top-right") 0))
   (aviator-neck-support-place (case side ("left" "top-left") (- half-d) ("right" "top-right") half-d) )
   
   ))
     (map add-third-dimension aviator-neck-support-bezier-points-with-origin)
     ) 
    )
  )
(def aviator-assembly-neck-support
  (->>
    (binding [*fn* 36] (cylinder (/ (- aviator-neck-width 1) 2) (- aviator-neck-height 2) :center false) )
   (translate [(+ (/ 12 2) (/ (- aviator-neck-width 1) 1))  0 0])
   (rdz -90)
   (rdx 180)
   (translate [aviator-diameter (+  (/ aviator-diameter 2) 2) 0])
(aviator-place-shape )
   )
  )

(def aviator-assembly-neck-bezier-left
  (aviator-assembly-neck-bezier "left")
  )

(def aviator-assembly-neck-support-bezier-left
  (aviator-assembly-neck-support-bezier "left")
  )

(def aviator-assembly-neck-bezier-right
  (aviator-assembly-neck-bezier "right"))

(def aviator-assembly-neck-support-bezier-right
  (aviator-assembly-neck-support-bezier "right"))

;; (defn aviator-neck-poly-shape [diameter-aviator diameter-tactile height-aviator height-tactile steps] 
;;   (let [radius-aviator (/ aviator-plug-connecter-ring-diameter 2)
;;         square-width-aviator aviator-neck-width
;;         circ-rad (/ square-width-aviator 2)
;;         radius-tactile (/ (+ metal-tactile-button-neck-radius 0.5) 2)
;;         square-width-tactile aviator-neck-support-width
;;         place #(aviator-place-shape (partial map +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees [0 0 0] %)
;;         place-tactile (fn [dx shape](aviator-neck-support-place dx 0 0 0 (partial map +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees  shape))
;;         place-points #(mapv place %)
;;         place-points-tactile #(mapv (partial place-tactile %1) %2)
;;         bezier-circle-quadrant #(bezier-quadratic (nth % 0) (nth % 1) (nth % 2) steps)
;;         aviator-bottom-left-quad (place-points [[(-(+ radius-aviator square-width-aviator diameter-aviator)) 0 height-aviator] 
;;                                   [(- (+ radius-aviator square-width-aviator diameter-aviator)) (- (+ radius-aviator square-width-aviator diameter-aviator)) height-aviator]
;;                                   [0 (- (+ radius-aviator square-width-aviator diameter-aviator)) height-aviator]])
;;         aviator-bottom-right-quad (place-points [[0 (+ radius-aviator square-width-aviator diameter-aviator) height-aviator]
;;                                                 [(- (+ radius-aviator square-width-aviator diameter-aviator)) (+ radius-aviator square-width-aviator diameter-aviator) height-aviator]
;;                                                 [(- (+ radius-aviator square-width-aviator diameter-aviator)) 0 height-aviator]]) 
;;         aviator-top-left-bezier (place-points-tactile (- aviator-assembly-left-or-right-translation) [[0 (+ radius-tactile square-width-tactile diameter-tactile) height-tactile] 
;;                                                                                                       [(+ radius-tactile square-width-tactile diameter-tactile) (+ radius-tactile square-width-tactile diameter-tactile) height-tactile]
;;                                                                                                       [(+ radius-tactile square-width-tactile diameter-tactile) 0 height-tactile]
;;                                                                                                       ])
;;         aviator-top-right-bezier (place-points-tactile (+ aviator-assembly-left-or-right-translation) [[(+ radius-tactile square-width-tactile diameter-tactile) 0 height-tactile]
;;                                                [(+ radius-tactile square-width-tactile diameter-tactile) (-  (+ radius-tactile square-width-tactile diameter-tactile)) height-tactile]
;;                                                [0 (-  (+ radius-tactile square-width-tactile diameter-tactile)) height-tactile]])
;;         aviator-bottom-left-quad-bezier-points (bezier-circle-quadrant aviator-bottom-left-quad)
;;         aviator-bottom-right-quad-bezier-points (bezier-circle-quadrant aviator-bottom-right-quad)
;;         aviator-top-left-bezier-points (bezier-circle-quadrant aviator-top-left-bezier)
;;         aviator-top-right-bezier-points (bezier-circle-quadrant aviator-top-right-bezier)
;;         left-linear-points (bezier-linear (nth aviator-bottom-left-quad 2) (nth aviator-top-left-bezier 0) steps)
;;         top-linear-points (bezier-linear (nth aviator-top-left-bezier 2) (nth aviator-top-right-bezier 0) steps)
;;         right-linear-points (bezier-linear (nth aviator-top-right-bezier 2) (nth aviator-bottom-right-quad 0)  steps) 
;;         ] 
;;       (concat
;;       (drop-last aviator-bottom-left-quad-bezier-points)  
;;       (drop-last left-linear-points) 
;;        (drop-last aviator-top-left-bezier-points)
;;        (drop-last top-linear-points)
;;       (drop-last aviator-top-right-bezier-points)
;;        (drop-last right-linear-points)
;;         aviator-bottom-right-quad-bezier-points 
;;       ) 
;;     ;;  (plot-bezier-points aviator-bottom-left-quad-bezier-points (sphere 1))
;;     ;;  (plot-bezier-points aviator-bottom-right-quad-bezier-points (sphere 1))
;;     ;;   (plot-bezier-points aviator-top-left-bezier-points (sphere 1)) 
;;     ;;  (plot-bezier-points aviator-top-right-bezier-points (sphere 1))
;;     ;;  (plot-bezier-points left-linear-points (sphere 1))
;;     ;;  (plot-bezier-points right-linear-points (sphere 1))
;;     ;;  (plot-bezier-points top-linear-points (sphere 1))
     
;;     ) 
  
;;   )

(defn aviator-neck-poly-shape [diameter-aviator diameter-tactile height-aviator height-tactile steps]
  (let [ 
        radius-aviator (/ aviator-plug-connecter-ring-diameter 2)
        square-width-aviator aviator-neck-width
        circ-rad (/ square-width-aviator 2)
        radius-tactile (/ (+ metal-tactile-button-neck-diameter 0.5) 2)
        square-width-tactile aviator-neck-support-width
        place #(aviator-place-shape (partial map +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees  %)
        place-tactile (fn [dx shape] (aviator-neck-support-place dx 0 0 0 (partial map +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees  shape))
        place-points #(mapv place %)
        place-points-tactile #(mapv (partial place-tactile %1) %2)
        bezier-circle-quadrant #(bezier-cubic (nth % 0) (nth % 1) (nth % 2) (nth % 3) steps)
        aviator-bottom-left-quad (place-points [[(* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-a) 0 height-aviator]
                                                [(* (- (+ radius-aviator square-width-aviator diameter-aviator )) circle-bezier-approx-c) (* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-b) height-aviator]
                                                [(* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-b) (* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-c) height-aviator]
                                                [0 (* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-a) height-aviator]])
        aviator-bottom-right-quad (place-points [[0 (* (+ radius-aviator square-width-aviator diameter-aviator ) circle-bezier-approx-a) height-aviator]
                                                 [(* (- (+ radius-aviator square-width-aviator diameter-aviator )) circle-bezier-approx-b) (* (+ radius-aviator square-width-aviator diameter-aviator ) circle-bezier-approx-c) height-aviator]
                                                 [(* (- (+ radius-aviator square-width-aviator diameter-aviator )) circle-bezier-approx-c) (* (+ radius-aviator square-width-aviator diameter-aviator) circle-bezier-approx-b) height-aviator]
                                                 [(* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-a) 0 height-aviator]])
        aviator-top-left-bezier (place-points-tactile (- aviator-assembly-left-or-right-translation) [[0 (* (+ radius-tactile square-width-tactile diameter-tactile) circle-bezier-approx-a) height-tactile]
                                                                                                      [(* (+ radius-tactile square-width-tactile diameter-tactile) circle-bezier-approx-b) (* (+ radius-tactile square-width-tactile diameter-tactile ) circle-bezier-approx-c) height-tactile]
                                                                                                      [(* (+ radius-tactile square-width-tactile diameter-tactile ) circle-bezier-approx-c) (* (+ radius-tactile square-width-tactile diameter-tactile) circle-bezier-approx-b) height-tactile]
                                                                                                      [(* (+ radius-tactile square-width-tactile diameter-tactile ) circle-bezier-approx-a) 0 height-tactile]])
        aviator-top-right-bezier (place-points-tactile (+ aviator-assembly-left-or-right-translation) [[(* (+ radius-tactile square-width-tactile diameter-tactile ) 1) 0 height-tactile]
                                                                                                       [(* (+ radius-tactile square-width-tactile diameter-tactile ) circle-bezier-approx-c) (* (-  (+ radius-tactile square-width-tactile diameter-tactile)) circle-bezier-approx-b) height-tactile]
                                                                                                       [(* (+ radius-tactile square-width-tactile diameter-tactile) circle-bezier-approx-b) (* (-  (+ radius-tactile square-width-tactile diameter-tactile )) circle-bezier-approx-c) height-tactile]
                                                                                                       [0 (* (-  (+ radius-tactile square-width-tactile diameter-tactile)) circle-bezier-approx-a) height-tactile]])
        aviator-bottom-left-quad-bezier-points (bezier-circle-quadrant aviator-bottom-left-quad)
        aviator-bottom-right-quad-bezier-points (bezier-circle-quadrant aviator-bottom-right-quad)
        aviator-top-left-bezier-points (bezier-circle-quadrant aviator-top-left-bezier)
        aviator-top-right-bezier-points (bezier-circle-quadrant aviator-top-right-bezier)
        left-linear-points (bezier-linear (nth aviator-bottom-left-quad 3) (nth aviator-top-left-bezier 0) steps)
        top-linear-points (bezier-linear (nth aviator-top-left-bezier 3) (nth aviator-top-right-bezier 0) steps)
        right-linear-points (bezier-linear (nth aviator-top-right-bezier 3) (nth aviator-bottom-right-quad 0)  steps)]
    (concat
     (drop-last aviator-bottom-left-quad-bezier-points)
     (drop-last left-linear-points)
     (drop-last aviator-top-left-bezier-points)
     (drop-last top-linear-points)
     (drop-last aviator-top-right-bezier-points)
     (drop-last right-linear-points)
     aviator-bottom-right-quad-bezier-points)
    ;;  (plot-bezier-points aviator-bottom-left-quad-bezier-points (sphere 1))
    ;;  (plot-bezier-points aviator-bottom-right-quad-bezier-points (sphere 1))
    ;;   (plot-bezier-points aviator-top-left-bezier-points (sphere 1)) 
    ;;  (plot-bezier-points aviator-top-right-bezier-points (sphere 1))
    ;;  (plot-bezier-points left-linear-points (sphere 1))
    ;;  (plot-bezier-points right-linear-points (sphere 1))
    ;;  (plot-bezier-points top-linear-points (sphere 1))
    ))

(defn aviator-mount-shape [diameter-aviator height-aviator steps]
  (let [
        radius-aviator (/ aviator-plug-connecter-ring-diameter 2)
square-width-aviator aviator-neck-width
        place #(aviator-place-shape (partial map +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees  %)
        place-points #(mapv place %)
        bezier-circle-quadrant #(bezier-cubic (nth % 0) (nth % 1) (nth % 2) (nth % 3) steps)
        aviator-bottom-left-quad (place-points [[(* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-a) 0 height-aviator]
                                                [(* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-c) (* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-b) height-aviator]
                                                [(* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-b) (* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-c) height-aviator]
                                                [0 (* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-a) height-aviator]])
aviator-bottom-right-quad (place-points [[0 (* (+ radius-aviator square-width-aviator diameter-aviator) circle-bezier-approx-a) height-aviator]
                                         [(* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-b) (* (+ radius-aviator square-width-aviator diameter-aviator) circle-bezier-approx-c) height-aviator]
                                         [(* (- (+ radius-aviator square-width-aviator diameter-aviator )) circle-bezier-approx-c) (* (+ radius-aviator square-width-aviator diameter-aviator) circle-bezier-approx-b) height-aviator]
                                         [(* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-a) 0 height-aviator]])
        aviator-top-left-quad (place-points [[0 (* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-a) height-aviator]
                                                [(*  (+ radius-aviator square-width-aviator diameter-aviator) circle-bezier-approx-b) (* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-c) height-aviator] 
                                             [(* (+ radius-aviator square-width-aviator diameter-aviator) circle-bezier-approx-c) (* (- (+ radius-aviator square-width-aviator diameter-aviator)) circle-bezier-approx-b) height-aviator]
                                             [(*  (+ radius-aviator square-width-aviator diameter-aviator) circle-bezier-approx-a) 0 height-aviator]
                                             ])
aviator-top-right-quad (place-points [[(*  (+ radius-aviator square-width-aviator diameter-aviator) circle-bezier-approx-a) 0 height-aviator] 
                                         [(* (+ radius-aviator square-width-aviator diameter-aviator ) circle-bezier-approx-c) (* (+ radius-aviator square-width-aviator diameter-aviator) circle-bezier-approx-b) height-aviator]
                                         [(*  (+ radius-aviator square-width-aviator diameter-aviator) circle-bezier-approx-b) (* (+ radius-aviator square-width-aviator diameter-aviator) circle-bezier-approx-c) height-aviator]
                                      [0 (* (+ radius-aviator square-width-aviator diameter-aviator) circle-bezier-approx-a) height-aviator]])
  aviator-bottom-left-quad-points (bezier-circle-quadrant aviator-bottom-left-quad)
        aviator-bottom-right-quad-points (bezier-circle-quadrant aviator-bottom-right-quad)
aviator-top-left-quad-points (bezier-circle-quadrant aviator-top-left-quad)
aviator-top-right-quad-points (bezier-circle-quadrant aviator-top-right-quad)
        ]
  (concat 
   (drop-last aviator-bottom-left-quad-points)
   (drop-last aviator-top-left-quad-points)
   (drop-last aviator-top-right-quad-points)
    aviator-bottom-right-quad-points 
   )
  )
  )

(defn tactile-mount-shape [diameter-tactile height-tactile dx steps]
  (let [radius-tactile (/ (+ metal-tactile-button-neck-diameter 0.5) 2)
        square-width-tactile aviator-neck-support-width
        place (fn [shape] (aviator-neck-support-place dx 0 0 0 (partial map +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees  shape))
        place-points #(mapv place %)
        bezier-circle-quadrant #(bezier-cubic (nth % 0) (nth % 1) (nth % 2) (nth % 3) steps)
        tactile-bottom-left-quad (place-points [[(* (- (+ radius-tactile square-width-tactile diameter-tactile)) circle-bezier-approx-a) 0 height-tactile]
                                                [(* (- (+ radius-tactile square-width-tactile diameter-tactile)) circle-bezier-approx-c) (* (- (+ radius-tactile square-width-tactile diameter-tactile)) circle-bezier-approx-b) height-tactile]
                                                [(* (- (+ radius-tactile square-width-tactile diameter-tactile)) circle-bezier-approx-b) (* (- (+ radius-tactile square-width-tactile diameter-tactile)) circle-bezier-approx-c) height-tactile]
                                                [0 (* (- (+ radius-tactile square-width-tactile diameter-tactile)) circle-bezier-approx-a) height-tactile]])
        tactile-bottom-right-quad (place-points [[0 (* (+ radius-tactile square-width-tactile diameter-tactile) circle-bezier-approx-a) height-tactile]
                                                 [(* (- (+ radius-tactile square-width-tactile diameter-tactile)) circle-bezier-approx-b) (* (+ radius-tactile square-width-tactile diameter-tactile) circle-bezier-approx-c) height-tactile]
                                                 [(* (- (+ radius-tactile square-width-tactile diameter-tactile)) circle-bezier-approx-c) (* (+ radius-tactile square-width-tactile diameter-tactile) circle-bezier-approx-b) height-tactile]
                                                 [(* (- (+ radius-tactile square-width-tactile diameter-tactile)) circle-bezier-approx-a) 0 height-tactile]])
        tactile-top-left-quad (place-points [[0 (* (- (+ radius-tactile square-width-tactile diameter-tactile)) circle-bezier-approx-a) height-tactile]
                                             [(*  (+ radius-tactile square-width-tactile diameter-tactile) circle-bezier-approx-b) (* (- (+ radius-tactile square-width-tactile diameter-tactile)) circle-bezier-approx-c) height-tactile]
                                             [(* (+ radius-tactile square-width-tactile diameter-tactile) circle-bezier-approx-c) (* (- (+ radius-tactile square-width-tactile diameter-tactile)) circle-bezier-approx-b) height-tactile]
                                             [(*  (+ radius-tactile square-width-tactile diameter-tactile) circle-bezier-approx-a) 0 height-tactile]])
        tactile-top-right-quad (place-points [[(*  (+ radius-tactile square-width-tactile diameter-tactile) circle-bezier-approx-a) 0 height-tactile]
                                              [(* (+ radius-tactile square-width-tactile diameter-tactile) circle-bezier-approx-c) (* (+ radius-tactile square-width-tactile diameter-tactile) circle-bezier-approx-b) height-tactile]
                                              [(*  (+ radius-tactile square-width-tactile diameter-tactile) circle-bezier-approx-b) (* (+ radius-tactile square-width-tactile diameter-tactile) circle-bezier-approx-c) height-tactile]
                                              [0 (* (+ radius-tactile square-width-tactile diameter-tactile) circle-bezier-approx-a) height-tactile]])
        tactile-bottom-left-quad-points (bezier-circle-quadrant tactile-bottom-left-quad)
        tactile-bottom-right-quad-points (bezier-circle-quadrant tactile-bottom-right-quad)
        tactile-top-left-quad-points (bezier-circle-quadrant tactile-top-left-quad)
        tactile-top-right-quad-points (bezier-circle-quadrant tactile-top-right-quad)]
    (concat
     (drop-last tactile-bottom-left-quad-points)
     (drop-last tactile-top-left-quad-points)
     (drop-last tactile-top-right-quad-points)
     tactile-bottom-right-quad-points)))

(defn mount-polyhedron [mount-outside-lower mount-outside-upper mount-outside-control mount-outside-curve-end mount-outside-upper-inside
                        mount-outside-lower-inside steps]
  (let [  mount-sides-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                                     (bezier-linear
                                                      (nth mount-outside-lower index)
                                                      (nth mount-outside-upper index)
                                                      steps))))
        mount-curve-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                 (bezier-quadratic
                                  (nth mount-outside-upper index)
                                  (nth mount-outside-control index)
                                  (nth mount-outside-curve-end index) 
                                  steps))))
        mount-top-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                                            (bezier-linear
                                                              (nth mount-outside-curve-end index)
                                                            (nth mount-outside-upper-inside index) 
                                                             steps))))
        mount-inside-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                                          (bezier-linear
                                                           (nth mount-outside-upper-inside index)
                                                           (nth mount-outside-lower-inside index) 
                                                           steps))))
        mount-bottom-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                                             (bezier-linear
                                                              (nth mount-outside-lower-inside index)
                                                              (nth mount-outside-lower index) 
                                                              steps))))
        mount-curve-points-start-index (count mount-sides-points)
        mount-top-points-start-index  (+ mount-curve-points-start-index (count mount-curve-points))
        mount-inside-points-start-index (+ mount-top-points-start-index (count mount-top-points))
        mount-bottom-points-start-index (+ mount-inside-points-start-index (count mount-inside-points))
        mount-sides-faces (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps) 
        mount-curve-faces (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps mount-curve-points-start-index)
        mount-top-faces (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps mount-top-points-start-index)
        mount-inside-faces (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps mount-inside-points-start-index)
        mount-bottom-faces (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps mount-bottom-points-start-index)
        mount-polyhedron-points (concat mount-sides-points mount-curve-points mount-top-points mount-inside-points mount-bottom-points) 
        mount-polyhedron-faces (into [] (concat mount-sides-faces mount-curve-faces
                                                        mount-top-faces mount-inside-faces
                                                        mount-bottom-faces
                                                        ))
        mount-polyhedron (polyhedron mount-polyhedron-points mount-polyhedron-faces )]
        mount-polyhedron)
  )
(defn mount-polyhedron-tactile [mount-outside-lower mount-outside-upper mount-outside-control mount-outside-curve-end mount-outside-upper-inside
                        mount-outside-lower-inside steps]
  (let [mount-sides-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                                    (bezier-linear
                                                     (nth mount-outside-upper index)
                                                      (nth mount-outside-lower index)
                                                     steps))))
        mount-curve-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                                    (bezier-quadratic 
                                                     (nth mount-outside-curve-end index)
                                                     (nth mount-outside-control index) 
                                                     (nth mount-outside-upper index)
                                                     steps))))
        mount-top-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                                  (bezier-linear
                                                    (nth mount-outside-upper-inside index)
                                                   (nth mount-outside-curve-end index)
                                                   steps))))
        mount-inside-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                                     (bezier-linear
                                                       (nth mount-outside-lower-inside index)
                                                      (nth mount-outside-upper-inside index)
                                                      steps))))
        mount-bottom-points (into [] (apply concat (for [index (range 0 (inc steps))]
                                                     (bezier-linear
                                                       (nth mount-outside-lower index)
                                                      (nth mount-outside-lower-inside index)
                                                      steps))))
        mount-curve-points-start-index (count mount-sides-points)
        mount-top-points-start-index  (+ mount-curve-points-start-index (count mount-curve-points))
        mount-inside-points-start-index (+ mount-top-points-start-index (count mount-top-points))
        mount-bottom-points-start-index (+ mount-inside-points-start-index (count mount-inside-points))
        mount-sides-faces (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps)
        mount-curve-faces (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps mount-curve-points-start-index)
        mount-top-faces (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps mount-top-points-start-index)
        mount-inside-faces (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps mount-inside-points-start-index)
        mount-bottom-faces (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps mount-bottom-points-start-index)
        mount-polyhedron-points (concat mount-sides-points mount-curve-points mount-top-points mount-inside-points mount-bottom-points)
        mount-polyhedron-faces (into [] (concat mount-sides-faces mount-curve-faces
                                                mount-top-faces mount-inside-faces
                                                mount-bottom-faces))
        mount-polyhedron (polyhedron mount-polyhedron-points mount-polyhedron-faces)]
    mount-polyhedron))


(def aviator-neck-poly 
  (let [steps-for-seven 140
        steps-for-four 80
        neck-bezier-outside (aviator-neck-poly-shape aviator-neck-width aviator-neck-support-width 0 0 (/ steps-for-seven 7))
        neck-bezier-outside-back (map #(mapv + [0 (- wall-thickness) 0] %)(aviator-neck-poly-shape aviator-neck-width aviator-neck-support-width 0 0 (/ steps-for-seven 7)))
        neck-control (aviator-neck-poly-shape 0 0  0 0 (/ steps-for-seven 7))
        neck-upper-control (aviator-neck-poly-shape (/ aviator-neck-width 2) (/ aviator-neck-support-width 2) (- aviator-neck-height) aviator-neck-support-height (/ steps-for-seven 7))
        neck-upper-control-2 (aviator-neck-poly-shape (/ aviator-neck-width 2) (/ aviator-neck-support-width 2) (- (+ aviator-neck-height (/ aviator-neck-width 2))) (+ aviator-neck-support-height (/ aviator-neck-support-width 2)) (/ steps-for-seven 7))
        neck-upper (aviator-neck-poly-shape (/ aviator-neck-width -2) (/ aviator-neck-support-width -2) (- (+ aviator-neck-height (/ aviator-neck-width 2))) (+ aviator-neck-support-height (/ aviator-neck-support-width 2)) (/ steps-for-seven 7))
        bezier-polyhedron-side-points
        (into [] (apply concat (for [index (range 0 (inc steps-for-seven))]
                                 (bezier-quartic
                                  (nth neck-bezier-outside index)
                                  (nth neck-control index)
                                  (nth neck-upper-control index)
                                  (nth neck-upper-control-2 index)
                                  (nth neck-upper index)
                                  steps-for-seven))))
        bezier-back-polyhedron-side-points (into [] (apply concat 
                                                           (for [index (range 0 (inc steps-for-seven))]
                                                             (bezier-linear
                                                              (nth neck-bezier-outside-back index)
                                                              (nth neck-bezier-outside index)
                                                              steps-for-seven
                                                              )
                                                             )))
        lower-back-centre (transform-position (partial aviator-place-shape) [0 0 wall-thickness])
        lower-centre (transform-position (partial aviator-place-shape) [0 0 0])
        upper-centre (transform-position (partial aviator-place-shape) [0 0 (- (+ aviator-neck-height (/ aviator-neck-width 2)))])
        lower-centre-index (count bezier-polyhedron-side-points)
        upper-centre-index  (inc lower-centre-index)
        lower-back-centre-index (count bezier-polyhedron-side-points)
        lower-centre-index-for-back (inc lower-back-centre-index) 
        bottom-faces (for [index (range 0 (inc steps-for-seven))]
                       [(* (inc steps-for-seven) index) lower-centre-index (* (inc steps-for-seven) (inc index))])
        top-faces (for [index (range 0 steps-for-seven)]
                    [(+ steps-for-seven (* (inc steps-for-seven) index))  (+ (*  (inc steps-for-seven) (inc index)) steps-for-seven) upper-centre-index])
        bezier-polyhedron-side-faces (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps-for-seven) (inc steps-for-seven) steps-for-seven)
        bezier-polyhedron-points (concat bezier-polyhedron-side-points [lower-centre upper-centre])
        bezier-back-polyhedron-points (concat bezier-back-polyhedron-side-points [lower-back-centre lower-centre])
        bezier-polyhedron-faces (into [] (concat bezier-polyhedron-side-faces bottom-faces top-faces))
        bezier-back-polyhedron-faces (into [] (concat bezier-polyhedron-side-faces bottom-faces  top-faces))
        bezier-polyhedron (polyhedron bezier-polyhedron-points  bezier-polyhedron-faces)
        bezier-back-polyhedron (polyhedron bezier-back-polyhedron-points bezier-back-polyhedron-faces)
        neck-bezier-inside (aviator-neck-poly-shape (- aviator-neck-width) (- aviator-neck-support-width) 0 0 (/ steps-for-seven 7))
        neck-bezier-upper-inside (aviator-neck-poly-shape (- aviator-neck-width) (- aviator-neck-support-width) (- (+ aviator-neck-height (/ aviator-neck-width 2))) (+ aviator-neck-support-height (/ aviator-neck-support-width 2)) (/ steps-for-seven 7))
        aviator-mount-outside-lower (aviator-mount-shape 0 0 (/ steps-for-four 4))
        aviator-mount-outside-upper (aviator-mount-shape 0 (- aviator-neck-height) (/ steps-for-four 4))
        aviator-mount-outside-control (aviator-mount-shape 0 (- (+ aviator-neck-height (/ aviator-neck-width 2))) (/ steps-for-four 4))
        aviator-mount-outside-curve-end (aviator-mount-shape (/ aviator-neck-width -2) (- (+ aviator-neck-height (/ aviator-neck-width 2))) (/ steps-for-four 4))
        aviator-mount-outside-upper-inside (aviator-mount-shape (- aviator-neck-width) (- (+ aviator-neck-height (/ aviator-neck-width 2))) (/ steps-for-four 4))
        aviator-mount-outside-lower-inside (aviator-mount-shape (- aviator-neck-width) 0 (/ steps-for-four 4))
        aviator-mount-polyhedron (mount-polyhedron aviator-mount-outside-lower aviator-mount-outside-upper aviator-mount-outside-control aviator-mount-outside-curve-end aviator-mount-outside-upper-inside aviator-mount-outside-lower-inside steps-for-four)
        tactile-mount-left-outside-lower (tactile-mount-shape 0 0 (- aviator-assembly-left-or-right-translation) (/ steps-for-four 4)) 
tactile-mount-left-outside-upper   (tactile-mount-shape 0 (+ aviator-neck-support-height) (- aviator-assembly-left-or-right-translation) (/ steps-for-four 4))
tactile-mount-left-outside-control   (tactile-mount-shape 0 (+ (+ aviator-neck-support-height (/ aviator-neck-support-width 2))) (- aviator-assembly-left-or-right-translation) (/ steps-for-four 4))
tactile-mount-left-outside-curve-end   (tactile-mount-shape (/ aviator-neck-support-width -2) (+ (+ aviator-neck-support-height (/ aviator-neck-support-width 2))) (- aviator-assembly-left-or-right-translation) (/ steps-for-four 4))
tactile-mount-left-outside-upper-inside  (tactile-mount-shape (- aviator-neck-support-width) (+ (+ aviator-neck-support-height (/ aviator-neck-support-width 2)))(- aviator-assembly-left-or-right-translation)  (/ steps-for-four 4))
tactile-mount-left-outside-lower-inside  (tactile-mount-shape (- aviator-neck-support-width) 0 (- aviator-assembly-left-or-right-translation) (/ steps-for-four 4))
tactile-mount-left-polyhedron (mount-polyhedron-tactile tactile-mount-left-outside-lower tactile-mount-left-outside-upper tactile-mount-left-outside-control tactile-mount-left-outside-curve-end tactile-mount-left-outside-upper-inside tactile-mount-left-outside-lower-inside steps-for-four)
        tactile-mount-right-outside-lower (tactile-mount-shape 0 0 (+ aviator-assembly-left-or-right-translation) (/ steps-for-four 4)) 
tactile-mount-right-outside-upper   (tactile-mount-shape 0 (+ aviator-neck-support-height) (+ aviator-assembly-left-or-right-translation) (/ steps-for-four 4))
tactile-mount-right-outside-control   (tactile-mount-shape 0 (+ (+ aviator-neck-support-height (/ aviator-neck-support-width 2))) (+ aviator-assembly-left-or-right-translation) (/ steps-for-four 4))
tactile-mount-right-outside-curve-end   (tactile-mount-shape (/ aviator-neck-support-width -2) (+ (+ aviator-neck-support-height (/ aviator-neck-support-width 2))) (+ aviator-assembly-left-or-right-translation) (/ steps-for-four 4))
tactile-mount-right-outside-upper-inside  (tactile-mount-shape (- aviator-neck-support-width) (+ (+ aviator-neck-support-height (/ aviator-neck-support-width 2)))(+ aviator-assembly-left-or-right-translation)  (/ steps-for-four 4))
tactile-mount-right-outside-lower-inside  (tactile-mount-shape (- aviator-neck-support-width) 0 (+ aviator-assembly-left-or-right-translation) (/ steps-for-four 4))
tactile-mount-right-polyhedron (mount-polyhedron-tactile tactile-mount-right-outside-lower tactile-mount-right-outside-upper tactile-mount-right-outside-control tactile-mount-right-outside-curve-end tactile-mount-right-outside-upper-inside tactile-mount-right-outside-lower-inside steps-for-four)
        ]
  
    (union 
     bezier-back-polyhedron
     aviator-mount-polyhedron
     ;tactile-mount-left-polyhedron
     ;tactile-mount-right-polyhedron
     bezier-polyhedron)
    )
  )

(def aviator-assembly-back 
  (let [steps 12
        place #(aviator-place-shape (partial map +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees %)
        shape-x (+ aviator-neck-width (/ aviator-diameter 2) 2)
        shape-y (+ aviator-plug-connecter-ring-diameter metal-tactile-button-hole-diameter (* aviator-neck-width 5) )
        shape-z (+ (* wall-thickness 1.5))
        x-trans (- (* aviator-neck-width 2)  2)
        y-trans  (- (+ (/ aviator-diameter 2) aviator-neck-width -2))
        z-trans (/ (* wall-thickness 1.5) 2)
        translation [x-trans y-trans z-trans]
        place-and-translate #(place (mapv + translation (rotate-around-z-in-degrees 90  %)))
        upper-top-left-corner (place-and-translate [(/ shape-x -2) (- (/ shape-y 2) 1) (/ shape-z -2)])
        upper-top-right-corner (place-and-translate [(/ shape-x -2) (+ (/ shape-y -2) 1) (/ shape-z -2)])
        upper-bottom-left-corner  (place-and-translate [(/ shape-x 2) (/ shape-y 2) (/ shape-z -2)])
        upper-bottom-right-corner (place-and-translate [(/ shape-x 2) (/ shape-y -2) (/ shape-z -2)])
        lower-top-left-corner (place-and-translate [(+ (/ shape-x -2) 4) (/ shape-y 2) (/ shape-z 2)])
        lower-top-right-corner (place-and-translate [(+ (/ shape-x -2) 4) (/ shape-y -2) (/ shape-z 2)])
        lower-bottom-left-corner (place-and-translate [(/ shape-x 2) (/ shape-y 2) (/ shape-z 2)])
        lower-bottom-right-corner (place-and-translate [(/ shape-x 2) (/ shape-y -2) (/ shape-z 2)])
        upper-top-left-corner-control-point (place-and-translate [(- (/ shape-x -2) 1)  (* shape-y 0.375) (/ shape-z -2)])
        upper-top-right-corner-control-point (place-and-translate [(- (/ shape-x -2) 1) (* shape-y -0.375) (/ shape-z -2)])
        base-points [upper-top-left-corner upper-top-right-corner upper-bottom-left-corner upper-bottom-right-corner lower-top-left-corner lower-top-right-corner lower-bottom-left-corner lower-bottom-right-corner]
        base-faces [[0 1 3] [0 3 2]  [5 4 6] [5 6 7] [0 4 5] [0 5 1] [2 3 7] [2 7 6] [4 0 2] [4 2 6] [1 5 7] [1 7 3]]
        lower-top-left-corner-curve-point (place-and-translate [(- (/ shape-x -2) 1) (/ shape-y 4) (* shape-z 1.75)])
        lower-top-right-corner-curve-point (place-and-translate [(- (/ shape-x -2) 1) (/ shape-y -4) (* shape-z 1.75)])
        upper-left-edge-points (bezier-linear upper-top-left-corner upper-bottom-left-corner steps)
        upper-right-edge-points (bezier-linear upper-top-right-corner upper-bottom-right-corner steps)
        lower-left-edge-points (bezier-linear lower-top-left-corner lower-bottom-left-corner steps)
        lower-right-edge-points (bezier-linear lower-top-right-corner lower-bottom-left-corner steps)
        base-polyhedron (polyhedron base-points base-faces)
        left-curve (bezier-quadratic upper-top-left-corner upper-top-left-corner-control-point lower-top-left-corner-curve-point steps)
        right-curve (bezier-quadratic upper-top-right-corner upper-top-right-corner-control-point lower-top-right-corner-curve-point steps)
        curve-polyhedron (generate-bezier-to-point-polyhedron left-curve lower-top-left-corner right-curve lower-top-right-corner)
        test [(place-and-translate [(/ shape-x 2) (/ shape-y 2) (/ shape-z -2)]) 
              (place-and-translate [(/ shape-x -2) (/ shape-y 2) (/ shape-z -2)])
             (place-and-translate [(- (/ shape-x -2) 1) (/ shape-y 2) (* shape-z 1.75)]) ]
        ]
    (union
     (plot-bezier-points (bezier-cubic upper-bottom-left-corner upper-top-left-corner upper-top-left-corner-control-point lower-top-left-corner-curve-point steps) (sphere 0.1)) 
     (plot-bezier-points (bezier-cubic upper-bottom-right-corner upper-top-right-corner upper-top-right-corner-control-point lower-top-right-corner-curve-point steps) (sphere 0.1))
     (-# base-polyhedron) 
     (-# curve-polyhedron) 
     )
    ) 
  )



(def ss [aviator-assembly-neck-bezier-left aviator-assembly-neck-support-bezier-left])
(def aviator-assembly
   (union
   (difference
    (hull
     (aviator-neck-place (binding [*fn* 36] (cylinder (+ (/ aviator-plug-connecter-ring-diameter 2) aviator-neck-width) (+ aviator-neck-height ) :center false)))
     (aviator-neck-support-place aviator-assembly-left-or-right-translation (binding [*fn* 36] (cylinder (+ metal-tactile-button-hole-radius aviator-neck-support-width) (+ aviator-neck-support-height ) :center false)))
     (aviator-neck-support-place (- aviator-assembly-left-or-right-translation) (binding [*fn* 36] (cylinder (+ metal-tactile-button-hole-radius aviator-neck-support-width) (+ aviator-neck-support-height ) :center false)))
     )
     (union
      (aviator-neck-place (binding [*fn* 36] (cylinder (+ (/ aviator-plug-connecter-ring-diameter 2) aviator-neck-width) (+ 1 (+ aviator-neck-height (/ aviator-neck-width 2))) :center false)))
(aviator-neck-support-place aviator-assembly-left-or-right-translation (binding [*fn* 36] (cylinder (+ metal-tactile-button-hole-radius aviator-neck-support-width) (+ (+ aviator-neck-support-height (/ (- aviator-neck-width 1) 2)) 4) :center false)))
(aviator-neck-support-place (- aviator-assembly-left-or-right-translation) (binding [*fn* 36] (cylinder (+ metal-tactile-button-hole-radius aviator-neck-support-width) (+ (+ aviator-neck-support-height (/ (- aviator-neck-width 1) 2)) 4) :center false)))
      )
    )
    aviator-assembly-back
    
   ; (call-module "bezier_polyhedron"  (union aviator-assembly-neck-bezier-left aviator-assembly-neck-support-bezier-left))   
  ;; (-# (->> 
  ;;  (cube (+ aviator-neck-width (/ aviator-diameter 2) 2 ) 
  ;;   (+ aviator-plug-connecter-ring-diameter metal-tactile-button-hole-diameter (* aviator-neck-width 6))
  ;;        (+ (* wall-thickness 1.5) ))
  ;;  (rdz 90)
  ;;  (translate [(- (* aviator-neck-width 2)  1) 
  ;;              (- (+ (/ aviator-diameter 2) aviator-neck-width))
  ;;              (/ (* wall-thickness 1.5) 2)]) 
  ;;  (aviator-place-shape [0 0 0])
  ;;  ))
      
    ;(-# (polygon aviator-assembly-neck-bezier-left false :convexity 10))
     ;(call-module "move"  [0, 0, 0]  [0, 0, 0])
    (join-beziers  aviator-assembly-neck-bezier-left aviator-assembly-neck-support-bezier-left)
    (join-beziers  aviator-assembly-neck-bezier-right aviator-assembly-neck-support-bezier-right)
     (join-beziers  (aviator-assembly-neck-support-bezier "top-left") (aviator-assembly-neck-support-bezier "top-right"))
    
   
    ;
    ; (-# (aviator-assembly-neck-support-bezier "right"))
    aviator-neck
aviator-neck-support-left
aviator-neck-support-right
    )
)

(def aviator-assembly-diffs
  (union
   aviator-hole 
(aviator-neck-support-place aviator-assembly-left-or-right-translation  0 0 (- (/ (+ wall-xy-offset wall-thickness) 2) 0.4) cutout-for-metal-tactile-button)
   (aviator-neck-support-place aviator-assembly-left-or-right-translation  0 0 (/ (+ metal-tactile-button-main-body-height 1) -2) (cube  metal-tactile-button-main-body-width metal-tactile-button-main-body-length  (+ metal-tactile-button-main-body-height 1)))
   ;(aviator-neck-support-place aviator-assembly-left-or-right-translation  0 0 (* 3 (/ metal-tactile-button-ball-diameter -4)) (binding [*fn* 36](sphere (/ metal-tactile-button-ball-diameter 2))))
(aviator-neck-support-place (- aviator-assembly-left-or-right-translation) 0 0 (- (/ (+ wall-xy-offset wall-thickness) 2) 0.4) cutout-for-metal-tactile-button)
   (aviator-neck-support-place (- aviator-assembly-left-or-right-translation)  0 0 (/ (+ metal-tactile-button-main-body-height 1) -2) (cube  metal-tactile-button-main-body-width metal-tactile-button-main-body-length  (+ metal-tactile-button-main-body-height 1)))
   (aviator-neck-support-place aviator-assembly-left-or-right-translation 0 0 0 (binding [*fn* 36] (cylinder metal-tactile-button-ball-diameter (+ aviator-neck-height 2) :center false)))
(aviator-neck-support-place (- aviator-assembly-left-or-right-translation) 0 0 0 (binding [*fn* 36] (cylinder metal-tactile-button-ball-diameter  (+ aviator-neck-height 2) :center false)))
(aviator-place-shape (binding [*fn* 36] (translate [0 0 -10] (cylinder (/ aviator-plug-connecter-ring-diameter 2) 10 :center false)))) 
    (aviator-place-shape (translate [0 0 2.3 ](binding [*fn* 6] (cylinder (/ aviator-plug-connecter-ring-recess-diameter 2) 2 :center false))))
   )
  )

(defn aviator-assembly-polyhedron [&{:keys [side] :or {side :right}}]
  (let [mirror-fn #(if (= side :left) (mirror [1 0 0] %) %)](difference
   aviator-neck-poly
   (->>
    (text "RESET" :font font-name :size font-size :halign "center")
    (mirror-fn)
    (extrude-linear {:height 1 :center false})
    (translate [0 (+ metal-tactile-button-hole-radius font-size 0.5) (+ aviator-neck-height 0.5)])
    (aviator-neck-support-place aviator-assembly-left-or-right-translation)
    )
   (->>
    (text "BOOT" :font font-name :size font-size :halign "center")
    (mirror-fn)
    (extrude-linear {:height 1 :center false})
    (translate [0 (-(+ metal-tactile-button-hole-radius font-size 1.5)) (+ aviator-neck-height 0.5)])
    (aviator-neck-support-place (- aviator-assembly-left-or-right-translation)))
    
   ;aviator-assembly-back
   )
  )
   
  )

(spit "things-low/aviator-assembly-polyhedron-test.scad"
      (write-scad
       (difference (aviator-assembly-polyhedron)
                   aviator-assembly-diffs)
       ))

(def resetswitch-start (map + [0 -3  20] (key-position 0 0 (map + (wall-locate3 0 1) [0 (/ mount-height  2) 0]))))
(def resetswitch-position [(first resetswitch-start) (second resetswitch-start) 12])
(def resetswitch-diameter 8.8)

(def reset-hole
  (union
  ; (->> (union (binding [*fn* 36] (cylinder [(/ resetswitch-diameter 2) (/ resetswitch-diameter 2)] 10)))
  ;      (rotate (/ π 2) [1 0 0])
  ;      (translate [(+ 4  (first aviator-start)) (- (second aviator-start) 1) (/ (+ 44 aviator-diameter 0) 2)]))
  ;    ; thinner wall
   (->> (union (binding [*fn* 36] (cylinder [(/ resetswitch-diameter 2) (/ resetswitch-diameter 2)] 20))) ;; depth here matters; has been eyeballed
        (rotate (/ Math/PI 2) [1 0 0])
        (translate [(+ 32 (first resetswitch-start)) (- (second resetswitch-start) 2) (/ (+ 8 aviator-diameter 0) 2)]))))

(def resetswitch-hole (translate aviator-position
                                 (rotate (deg2rad 90) [1 0 0]
                                     ;(rotate (deg2rad 45) [0 1 0]
                                         (translate [4 0 0]
                                                    (cylinder (/  resetswitch-diameter 2) 20)))))