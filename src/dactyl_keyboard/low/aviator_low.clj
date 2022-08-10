 (ns dactyl-keyboard.low.aviator-low
   (:refer-clojure :exclude [use import])
   (:require [clojure.core.matrix :refer [array matrix mmul inner-product transform add]]
             [scad-clj.scad :refer :all]
             [scad-clj.model :refer :all]
             [dactyl-keyboard.utils :refer :all]
             [dactyl-keyboard.switch-hole :refer :all]
             [dactyl-keyboard.low.placement-functions-low :refer :all]
             [dactyl-keyboard.low.case-low :refer :all]
             [dactyl-keyboard.low.shape-parameters-low :refer :all]
             [unicode-math.core :refer :all]
              ;[uncomplicate.neanderthal.native :refer :all]
              [uncomplicate.neanderthal.core :refer [mv]]
             ))

; code adapted from https://gist.github.com/jamiehs/de163e7d469e4fb4220e504b58613806
(def aviator-includes (map include ["../BOSL2/transforms.scad" , "../BOSL2/std.scad"] ))
(def aviator-start (map + [-38 -11  20] (key-position 0 0 (map + (wall-locate3 0 1) [0 (/ mount-height  2) 0]))))
(def aviator-position [(first aviator-start) (second aviator-start) 12])
(def aviator-diameter 16.2)
(def aviator-recess-start (map + [-40 -12  20] (key-position 0 0 (map + (wall-locate3 0 1) [0 (/ mount-height  2) 0]))))
(def aviator-recess-position [(first aviator-recess-start) (second aviator-recess-start) 12])
(def aviator-recess-diameter 17.6)
(def aviator-male-connecter-ring-diameter 18.3)
(def aviator-male-connecter-length 35.5)
(def aviator-female-connecter-ring-diameter 21.5)
(def aviator-locking-ring-length 7.5)
(def aviator-neck-width 3)
(def aviator-neck-bezier-width aviator-neck-width)

(def aviator-neck-height 8.6)
(def aviator-offset [0 (/ aviator-neck-height 2) 0])
(defn aviator-place-shape [position shape]
  (->> shape
   (rdy -90)
       (rdz tps-65-z-rotation)
       ;(rotate (deg2rad 90) [1 0 0])
       ;(rdz left-section-rotation-angle)
 
   
   (translate position)
   
       ))
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
(defn aviator-place [position diameter]
  
  (aviator-place-shape position (cylinder (/  diameter 2) plate-thickness)
   
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

(def aviator-hole (translate  aviator-position (aviator-place-shape  [0  0 0]  (rdx 180 (binding [*fn* 36] (cylinder (/  aviator-female-connecter-ring-diameter 2) (+ wall-xy-offset wall-thickness) :center true))))))
(def aviator-recess-hole (translate aviator-recess-position (aviator-place-shape  [0  0 0] (rdx 180 (binding [*fn* 36] (cylinder (/ aviator-recess-diameter 2) (+ wall-xy-offset wall-thickness) :center true))))))
(def aviator-male-connecter-clearence-test (translate [0 10 0] (aviator-place aviator-position aviator-male-connecter-ring-diameter)))
(def aviator-female-connecter-clearence-test (aviator-place aviator-position aviator-female-connecter-ring-diameter))
(def aviator-fillet (->> 
                    
                    
                     (difference  (fillet-circle 9)
                                  (translate [ 0 0 9](cube 20 20 6)) 
                                 (cube 40 40 2) )
                      
                     (rdx 180)
                     (aviator-place-shape aviator-position)
                    
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
  (let [radius (/ aviator-male-connecter-ring-diameter 2) square-width aviator-neck-width circ-rad (/ square-width 2)]
     (binding [*fn* 36] (extrude-rotate {:convexity 10}
                                        (union
                                         (translate [radius aviator-neck-height 0] (square circ-rad circ-rad :center false))
                                         (translate [radius 0 0] (square square-width aviator-neck-height :center false))
                                         (translate [(+ radius square-width) 0 0] aviator-neck-bezier)
                                         (translate [(+ radius  circ-rad) aviator-neck-height 0]  (difference
                                                                                                   (binding [*fn* 36] (circle circ-rad))
                                                                                                   (mirror [-1 0 0] (square circ-rad aviator-neck-height :center false)))))))

     )
  )
(defn aviator-neck-place [shape]
  (->> shape
   (rdx 180) 
(aviator-place-shape aviator-position))
  ) 

(def aviator-assembly-neck-shape
  (->>
   (cylinder (/ aviator-neck-width 2) aviator-neck-height)
   (translate [(/ aviator-male-connecter-ring-diameter 2) 0 0 ])
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

(def aviator-neck-support-bezier-points (bezier-curve [[(- aviator-neck-width 1) 0] [0.5 0] [0 0.5] [0 (- aviator-neck-height 2)]] 0.5))
(def aviator-neck-support-bezier-points-with-origin (into [] (concat aviator-neck-support-bezier-points [[0 0]])))
(def aviator-neck-support-bezier
  (difference
   (polygon   [[(- (- aviator-neck-width 1) 0.01) 0] [0 0] [0 (- aviator-neck-height 2 0.01)]] false :convexity 10)
   (polygon  aviator-neck-support-bezier-points false :convexity 10)))

(def aviator-neck-support-shape
  (let [radius (/ 12 2) square-width (- aviator-neck-width 1) circ-rad (/ square-width 2)] 
     (binding [*fn* 36]
       (extrude-rotate {:convexity 10}
                       (union
                        (translate [radius (- aviator-neck-height 2) 0] (square circ-rad circ-rad :center false))
                         (translate [radius 0 0] (square square-width (- aviator-neck-height 2) :center false))
                        (translate [(+ radius square-width)  0 0] aviator-neck-support-bezier)
                       
                        
                        (translate [(+ radius circ-rad) (- aviator-neck-height 2) 0] (difference
                                                                                      (binding [*fn* 36] (circle circ-rad))
                                                                                      (mirror [-1 0 0] (square circ-rad (- aviator-neck-height 2) :center false)))))))
     )
  )

(defn aviator-neck-support-place [dx shape]
  (let [x-trans-modifier 2 x-trans (cond (neg? dx) (- dx x-trans-modifier) :else (+ dx x-trans-modifier))] 
   (->> shape
   (rdx 180)
(translate [aviator-male-connecter-ring-diameter x-trans  0])
(aviator-place-shape aviator-position)
   ))
  )
(defn aviator-neck-support [dx]
  (aviator-neck-support-place dx aviator-neck-support-shape)
  )

(def aviator-neck-support-left
  (aviator-neck-support  (- (/ aviator-diameter 2)))
  )

(def aviator-neck-support-right
  (aviator-neck-support   (/ aviator-diameter 2))
                        )
(def aviator-assembly-neck
 (->>
 (translate [(+ (/ aviator-male-connecter-ring-diameter 2) (/ aviator-neck-width 1)) 0 0] (cylinder (/ aviator-neck-width 2) aviator-neck-height :center false))
 (rdz -90)

 )
  )

(defn aviator-assembly-neck-bezier [side]
  (let [x-trans (+ (/ aviator-male-connecter-ring-diameter 2) (/ aviator-neck-width 1))]
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
(def aviator-neck-support-width (- aviator-neck-width 1))
(defn aviator-assembly-neck-support-bezier [side]
  (let [x-trans (+ panel-switch-radius aviator-neck-support-width) half-d (/ aviator-diameter 2)]
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
(aviator-place-shape aviator-position)
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



(def ss [aviator-assembly-neck-bezier-left aviator-assembly-neck-support-bezier-left])
(def aviator-assembly
   (union
   (difference
    (hull
     (aviator-neck-place (binding [*fn* 36] (cylinder (+ (/ aviator-male-connecter-ring-diameter 2) aviator-neck-width) (+ aviator-neck-height ) :center false)))
     (aviator-neck-support-place (/ aviator-diameter 2) (binding [*fn* 36] (cylinder (+ panel-switch-radius aviator-neck-support-width) (+ (- aviator-neck-height 2) ) :center false)))
     (aviator-neck-support-place (- (/ aviator-diameter 2)) (binding [*fn* 36] (cylinder (+ panel-switch-radius aviator-neck-support-width) (+ (- aviator-neck-height 2) ) :center false)))
     )
     (union
      (aviator-neck-place (binding [*fn* 36] (cylinder (+ (/ aviator-male-connecter-ring-diameter 2) aviator-neck-width) (+ 1 (+ aviator-neck-height (/ aviator-neck-width 2))) :center false)))
(aviator-neck-support-place (/ aviator-diameter 2) (binding [*fn* 36] (cylinder (+ panel-switch-radius aviator-neck-support-width) (+ (+ (- aviator-neck-height 2) (/ (- aviator-neck-width 1) 2)) 4) :center false)))
(aviator-neck-support-place (- (/ aviator-diameter 2)) (binding [*fn* 36] (cylinder (+ panel-switch-radius aviator-neck-support-width) (+ (+ (- aviator-neck-height 2) (/ (- aviator-neck-width 1) 2)) 4) :center false)))
      )
    )
   ; (call-module "bezier_polyhedron"  (union aviator-assembly-neck-bezier-left aviator-assembly-neck-support-bezier-left))   

      
    ;(-# (polygon aviator-assembly-neck-bezier-left false :convexity 10))
     ;(call-module "move"  [0, 0, 0]  [0, 0, 0])
    (-#(join-beziers  aviator-assembly-neck-bezier-left aviator-assembly-neck-support-bezier-left))
    (join-beziers  aviator-assembly-neck-bezier-right aviator-assembly-neck-support-bezier-right)
    (-# (join-beziers  (aviator-assembly-neck-support-bezier "top-left") (aviator-assembly-neck-support-bezier "top-right")))
    
   
    ;
    ; (-# (aviator-assembly-neck-support-bezier "right"))
    aviator-neck
aviator-neck-support-left
aviator-neck-support-right
    )
)

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
        (rotate (/ π 2) [1 0 0])
        (translate [(+ 32 (first resetswitch-start)) (- (second resetswitch-start) 2) (/ (+ 8 aviator-diameter 0) 2)]))))

(def resetswitch-hole (translate aviator-position
                                 (rotate (deg2rad 90) [1 0 0]
                                     ;(rotate (deg2rad 45) [0 1 0]
                                         (translate [4 0 0]
                                                    (cylinder (/  resetswitch-diameter 2) 20)))))