(ns dactyl-keyboard.low.case-low
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.sa-keycaps :refer :all]
            [dactyl-keyboard.low.placement-functions-low :refer :all]
            [dactyl-keyboard.low.web-connecters-low :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.low.thumbs-low :refer :all]
            [dactyl-keyboard.low.oled-low-placements :refer :all]
            [dactyl-keyboard.oled :refer :all]
             [dactyl-keyboard.tps-65 :refer :all]
            [dactyl-keyboard.EVQWGD001 :refer :all]
            ))



;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 10})
       (translate [0 0 (- (/ height 2) 10)])))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

(def left-wall-x-offset 5) ; original 10
(def left-wall-z-offset 4) ; original 3
(def left-wall-x-offset-oled 
  (cond (= nrows 5) -16
        (= nrows 4 ) -16))

(def left-section-rotation-angle oled-mount-rotation-z-old)
(defn left-key-position [row direction]
  (map - (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0]) [left-wall-x-offset 0 left-wall-z-offset]))

(defn left-key-place [row direction shape]
  (translate (left-key-position row direction) shape))

(defn left-wall-plate-position [xdir ydir]
  (->>
   (add-vec
    [left-wall-x-offset-oled 0 (- left-wall-z-offset 2)]
    (key-position 0 (cond (= nrows 5) 0 (= nrows 4) -1) [0 0 0])
    [(* mount-width -0.5) (* mount-width 0.5) 0]
    [(* oled-holder-width -0.5) (* oled-holder-height -0.5) 0]
    [(* xdir oled-holder-width 0.5) (* ydir oled-holder-height 0.5) 0]
    [-3 7 -7])))

(defn left-wall-plate-position-variable [xdir ydir]
  (->>
   (add-vec
    [left-wall-x-offset-oled 0 (- left-wall-z-offset 2)]
    (key-position 0 0 [0 0 0])
    [(* mount-width -0.5) (* mount-width 0.5) 0]
    [(* screen-holder-width -0.5) (* screen-holder-height -0.5) 0]
    [(* xdir screen-holder-width 0.5) (* ydir screen-holder-height 0.5) 0]
    [-3 7 -7])))


(defn left-wall-plate-place [xdir ydir shape]
  (->> shape
       (translate (left-wall-plate-position xdir ydir))
       (rotate oled-mount-rotation-x-old [1 0 0])
       (rotate oled-mount-rotation-z-old [0 0 1])))

(defn left-wall-plate-place-variable [xdir ydir shape]
  (->> shape
       (translate (left-wall-plate-position-variable xdir ydir))
       (rotate oled-mount-rotation-x-old [1 0 0])
       (rotate oled-mount-rotation-z-old [0 0 1])))

(defn wall-locate0 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) 0])
(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) -1])
(defn wall-locate2 [dx dy] [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset])
(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness)) (* dy (+ wall-xy-offset wall-thickness)) wall-z-offset])

(defn wall-locate22-xy [dx dy xy] [(* dx (/ xy 2)) (* dy (/ xy 2)) wall-z-offset])
(defn wall-locate2-xy [dx dy xy] [(* dx xy) (* dy xy) wall-z-offset])
(defn wall-locate2.5-xy [dx dy xy] [(* dx (+ xy wall-thickness)) (* dy (+ xy wall-thickness)) wall-z-offset])

(defn wall-locate3-xy [dx dy xy] [(* dx (+ xy wall-thickness)) (* dy (+ xy wall-thickness)) wall-z-offset])

(defn wall-locate3-xy-3d [dx dy dz xy] [(* dx (+ xy wall-thickness)) (* dy (+ xy wall-thickness)) (wall-z-offset)])


(def fillet-s (->>
               (binding [*fn* 36] (sphere 2))
               (translate [ 0 0 plate-thickness])))

(defn check-post [post]
  (cond 
    (identical? post oled-post-tr) web-post-tr
(identical? post oled-post-tl) web-post-tl
(identical? post oled-post-bl) web-post-bl
(identical? post oled-post-br)   web-post-br
    :else post 
    )
  )

(defn convert-to-curve-post [post]
  (cond
    (or (identical? post oled-post-tr) (identical? post web-post-tr)) curve-post-tr
    (or (identical? post oled-post-tl) (identical? post web-post-tl)) curve-post-tl
    (or (identical? post oled-post-bl) (identical? post web-post-bl)) curve-post-bl
    (or (identical? post oled-post-br) (identical? post oled-post-br))   curve-post-br
    :else curve-post))

(defn convert-to-sphere-post [post]
  (cond
    (or (identical? post oled-post-tr) (identical? post web-post-tr)) sphere-post-tr
    (or (identical? post oled-post-tl) (identical? post web-post-tl)) sphere-post-tl
    (or (identical? post oled-post-bl) (identical? post web-post-bl)) sphere-post-bl
    (or (identical? post oled-post-br) (identical? post oled-post-br))   sphere-post-br
    :else sphere-post))

(defn fillet-about-point [dx dy steps]
  (let [w1 (wall-locate1 dx dy)
        point1 [(first w1) (first w1) (+ (last w1) 2)]
        w2  (wall-locate3 dx dy)
        point2 [(* (first w2) 0.98) (* (second w2) 0.98) (+ (last w2) 0.0)]]
   
   (bezier-quadratic point1 [(* dx wall-xy-offset) (* dy wall-xy-offset) 0] 
                 ;[(* dx (+ wall-xy-offset wall-thickness (/ oled-post-size 1))) (* dy (+ wall-xy-offset wall-thickness (/ oled-post-size 1))) (+ wall-z-offset (/ oled-holder-thickness 1))]
                 point2 steps))
  )

(defn fillet-about-point-2 [dx dy steps]
  (let [w1 [(* dx (+ wall-xy-offset wall-thickness 
                     ;(- (/ oled-post-size 2))
                     )) (* dy (+ wall-xy-offset wall-thickness 
                                 ;(- (/ oled-post-size 2))
                                 )) (+ wall-z-offset 0.0)]
        point1 [(* (first w1) 0.98) (* (second w1) 0.98) (+ (last w1) 0.0)]
        w2 (wall-locate3 dx dy)
        point2 [(* (first w2) 0.98) (* (second w2) 0.98) (+ (last w2) 0.5)]]

    (bezier-quadratic ;point1 
        point1
                     ; [(* dx (+ wall-xy-offset wall-thickness oled-post-size)) (* dy (+ wall-xy-offset wall-thickness oled-post-size)) 0]
                      [(* dx (+ wall-xy-offset wall-thickness (+ (/ oled-post-size 2)))) (* dy (+ wall-xy-offset wall-thickness (+ (/ oled-post-size 2)))) (- wall-z-offset 1) ]
                     [(* 0.98 (* dx (+ wall-xy-offset wall-thickness (+ (/ oled-post-size 2))))) (* 0.98 (* dy (+ wall-xy-offset wall-thickness (+ (/ oled-post-size 2))))) (+ 0.5 (- wall-z-offset (/ oled-holder-thickness  1)))]
                       steps)))

(defn fillet-about-point-xy [dx dy  xy steps]
  (let [w1 (wall-locate1 dx dy)
        point1 [(first w1) (first w1) (+ (last w1) 2)]
        w2 (wall-locate3-xy dx dy xy)
        point2 [(* (first w2) 0.98) (* (second w2) 0.98) (+ (last w2) 0.0)]]

    (bezier-quadratic point1 [(* dx xy) (* dy xy) 0] point2 steps)))

(defn fillet-about-point-xy-2 [dx dy xy steps]
  (let [w1 [(* dx (+ xy wall-thickness)) (* dy (+ xy wall-thickness)) (+ wall-z-offset 0.5)]
        point1 [(* (first w1) 0.98) (* (second w1) 0.98) (+ (last w1) 0.0)] 
        ]
    (bezier-quadratic 
     point1
     [(* dx (+ xy wall-thickness (+ (/ oled-post-size 2)))) (* dy (+ xy wall-thickness (+ (/ oled-post-size 2)))) (- wall-z-offset 1)]
     [(* 0.98 (* dx (+ xy wall-thickness (+ (/ oled-post-size 2))))) (* 0.98 (* dy (+ xy wall-thickness (+ (/ oled-post-size 2))))) (+ 0.5 (- wall-z-offset (/ oled-holder-thickness  1)))]
     steps
     )
    )
  )

(defn bezier-points-for-oled-post [dx dy ]
  (let [shift-factor (fn [point](cond 
                 (> point 0) (/ oled-post-size 2)
                  (< point 0) (- (/ oled-post-size 2))
                  :else point
                  )) 
  ] 
    [ [0 0 (/ oled-holder-thickness 2)] [(shift-factor dx) (shift-factor dy) (/ oled-holder-thickness 2)]  [0 0 (- (/ oled-holder-thickness 2))]]

  )
)

(defn get-oled-corner-translation-vector [oled-post-type]
(cond
  (identical? oled-post-type oled-post-tr) oled-post-tr-translation-vector
  (identical? oled-post-type oled-post-tl) oled-post-tl-translation-vector
  (identical? oled-post-type oled-post-bl) oled-post-bl-translation-vector
  (identical? oled-post-type oled-post-br) oled-post-br-translation-vector
   :else [0 0 0] 
  )
  )

(defn fillet-about-point-lower [ points vector w3 oled-post-type place]
  
    (->>
     
     (mapv (fn [point]
                       (->> (translate point (convert-to-curve-post oled-post-type))
                            (translate oled-translation-vector)
                            (translate vector)
                            (translate w3)
                            (place ))
        ) points
                     )))
  


(defn plot-and-translate-bezier-points [place bezier-points shape]
  (mapv (fn [point]
          (->> 
           (translate point shape)
           (place)
           )
          )
        bezier-points
        )
  )

(defn wall-bezier-1 [place dx dy post]
  (plot-and-translate-bezier-points (partial place)  (fillet-about-point dx dy 20)  (convert-to-curve-post post))
  )

(defn wall-bezier-2 [place dx dy post]
  (plot-and-translate-bezier-points (partial place)  (fillet-about-point-2 dx dy 20)  (convert-to-curve-post post)))

(defn wall-bezier-1-xy [place dx dy xy post]
  (plot-and-translate-bezier-points (partial place)  (fillet-about-point-xy dx dy xy 20)  (convert-to-curve-post post)))

(defn wall-bezier-2-xy [place dx dy xy post]
  (plot-and-translate-bezier-points (partial place)  (fillet-about-point-xy-2 dx dy xy 20)  (convert-to-curve-post post)))
; dx1, dy1, dx2, dy2 = direction of the wall. '1' for front, '-1' for back, '0' for 'not in this direction'.
; place1, place2 = function that places an object at a location, typically refers to the center of a key position.
; post1, post2 = the shape that should be rendered
(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  "If you want to change the wall, use this.
   place1 means the location at the keyboard, marked by key-place or thumb-xx-place
   dx1 means the movement from place1 in x coordinate, multiplied by wall-xy-locate.
   dy1 means the movement from place1 in y coordinate, multiplied by wall-xy-locate.
   post1 means the position this wall attached to place1.
         xxxxx-br means bottom right of the place1.
         xxxxx-bl means bottom left of the place1.
         xxxxx-tr means top right of the place1.
         xxxxx-tl means top left of the place1.
   place2 means the location at the keyboard, marked by key-place or thumb-xx-place
   dx2 means the movement from place2 in x coordinate, multiplied by wall-xy-locate.
   dy2 means the movement from place2 in y coordinate, multiplied by wall-xy-locate.
   post2 means the position this wall attached to place2.
         xxxxx-br means bottom right of the place2.
         xxxxx-bl means bottom left of the place2.
         xxxxx-tr means top right of the place2.
         xxxxx-tl means top left of the place2.
   How does it work?
   Given the following wall
       a ==\\ b
            \\
           c \\ d
             | |
             | |
             | |
             | |
           e | | f
   In this function a: usually the wall of a switch hole.
                    b: the result of hull and translation from wall-locate1
                    c: the result of hull and translation from wall-locate2
                    d: the result of hull and translation from wall-locate3
                    e: the result of bottom-hull translation from wall-locate2
                    f: the result of bottom-hull translation from wall-locate3"
  (let [
        ] 
   
   (union
    
   
      
    ( hull
    ;(hull a b)
     ;(hull b c)
    ; (hull c d)
     
 
 (plot-and-translate-bezier-points (partial place1)  (fillet-about-point dx1 dy1 20)  (convert-to-curve-post post1))
              
     
;    (-# (plot-and-translate-bezier-points (partial place1)  (fillet-about-point-2 dx1 dy1 20)  (convert-to-curve-post post1)))
;(-# (plot-and-translate-bezier-points (partial place2) (fillet-about-point-2 dx2 dy2 20) (convert-to-curve-post post2)))
   (place1 (check-post post1))
   ; 
    (place1 (translate (wall-locate1 dx1 dy1)  post1))
    (place1 (translate (wall-locate2 dx1 dy1) post1))
  ;   (-# (plot-and-translate-bezier-points (partial place1)  (fillet-about-point-3 dx1 dy1 20)  (convert-to-curve-post post1)))
    ;(place1 (translate (wall-locate3 dx1 dy1)  post1))
      
   ; 
    (place2 (check-post post2))
     (plot-and-translate-bezier-points (partial place2) (fillet-about-point dx2 dy2 20) (convert-to-curve-post post2))
   ;  
   ;(place2 (translate (wall-locate1 dx2 dy2)  post2))
   (place2 (translate (wall-locate2 dx2 dy2) post2))
 
 ;(-# (plot-and-translate-bezier-points (partial place2)  (fillet-about-point-3 dx2 dy2 20)  (convert-to-curve-post post2)))  
 ;(place2 (translate (wall-locate3 dx2 dy2)  post1))
 )
    
(bottom-hull
 (place1 (translate (wall-locate2 dx1 dy1) post1))
 (plot-and-translate-bezier-points (partial place1)  (fillet-about-point-2 dx1 dy1 10)  (convert-to-curve-post post1))
 ;(place1 (translate (wall-locate3 dx1 dy1) post1))
  (place2 (translate (wall-locate2 dx2 dy2) post2))
 (plot-and-translate-bezier-points (partial place2) (fillet-about-point-2 dx2 dy2 10) (convert-to-curve-post post2))
 ;(place2 (translate (wall-locate3 dx2 dy2) post2))
 )
   )))


  

(defn wall-brace-xy 
  ( [place1 dx1 dy1 post1 place2 dx2 dy2 post2 xy1 xy2]
   (wall-brace-xy place1 dx1 dy1 post1 place2 dx2 dy2 post2 xy1 xy2 true))
  ( [place1 dx1 dy1 post1 place2 dx2 dy2 post2 xy1 xy2 bottom](union
   (hull
    
   (place1 (check-post post1))
    
    (place1 (translate (wall-locate1 dx1 dy1) post1))
    (place1 (translate (wall-locate2-xy dx1 dy1 xy1) post1)) 
    (plot-and-translate-bezier-points (partial place1)  (fillet-about-point-xy dx1 dy1 xy1 20)  (convert-to-curve-post post1))
    ;(place1 (translate (wall-locate3-xy dx1 dy1 xy1) post1))
    ;  )
    ;(for [i (range 0 1 0.1)
    ;      :let [dxv2 (* dx2 i)
    ;            dyv2 (* dy2 i)]]
    ;( union
    (place2 (check-post post2))
    (place2 (translate (wall-locate1 dx2 dy2) post2))
    (place2 (translate (wall-locate2-xy dx2 dy2 xy2) post2))
    (plot-and-translate-bezier-points (partial place2) (fillet-about-point-xy dx2 dy2 xy2 20) (convert-to-curve-post post2))
    ;(place2 (translate (wall-locate3-xy dx2 dy2 xy2) post2))
    )
    
   
   
 
   (if bottom
     (bottom-hull
      (place1 (translate (wall-locate2-xy dx1 dy1 xy1) post1))
    ;(place1 (translate (wall-locate3-xy dx1 dy1 xy1) post1))
      (plot-and-translate-bezier-points (partial place1) (fillet-about-point-xy-2 dx1 dy1 xy1 20) (convert-to-curve-post post1))
      (place2 (translate (wall-locate2-xy dx2 dy2 xy2) post2))
      (plot-and-translate-bezier-points (partial place2) (fillet-about-point-xy-2 dx2 dy2 xy2 20) (convert-to-curve-post post2))
    ;(place2 (translate (wall-locate3-xy dx2 dy2 xy2) post2))
      )
     (hull 
      (plot-and-translate-bezier-points (partial place1) (fillet-about-point-xy-2 dx1 dy1 xy1 20) (convert-to-curve-post post1))
      (plot-and-translate-bezier-points (partial place2) (fillet-about-point-xy-2 dx2 dy2 xy2 20) (convert-to-curve-post post2))
      )

     )
   )))

(defn wall-brace-xy-half-top [place dx dy post xy]
 
 (union 
  (place (check-post post))

  (place (translate (wall-locate1 dx dy) post))
  (place (translate (wall-locate2-xy dx dy xy) post))
  (plot-and-translate-bezier-points (partial place)  (fillet-about-point-xy dx dy xy 20)  (convert-to-curve-post post)))
)
(defn wall-brace-xy-half-bottom [place dx dy post xy] 
   (bottom-hull
    (place (translate (wall-locate2 dx dy) post))
    (plot-and-translate-bezier-points (partial place)  (fillet-about-point-xy-2 dx dy xy 10)  (convert-to-curve-post post))))

(defn wall-brace-xy-half [place dx dy post xy]
  (union
(wall-brace-xy-half-top place dx dy post xy)
   (wall-brace-xy-half-bottom place dx dy post xy)
   ))

(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2))

(defn key-wall-brace-xy [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2 xy1 xy2]
  (wall-brace-xy (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2 xy1 xy2))



(defn left-wall-plate-place-reverse [xdir ydir shape]
  (->> shape
       (translate (left-wall-plate-position xdir ydir))
       (rotate oled-mount-rotation-x-old [1 0 0])
       (rotate oled-mount-rotation-z-old [0 0 1])))

(def tps-65-z-position
 (cond
   (= nrows 5) 12  
  (= nrows 4 ) 12
 ))
  (def tps-65-z-rotation 85)
(def tps-65-x-rotation -10)
(def tps-65-y-rotation 7.5)
 (defn tps-65-place [shape]
   (->> shape
        (rotate (deg2rad tps-65-z-rotation) [0 0 1])
        (rotate (deg2rad tps-65-x-rotation) [1 0 0])
        
        (rotate (deg2rad tps-65-y-rotation) [0 1 0])
       ;(rdy -7.5)
        (left-wall-plate-place 0 (+ left-wall-y-modifier -3))
        (translate [0 0 tps-65-z-position])))



(defn div-by-2 [num]
  (/ num 2))

(defn col-avg [col1 col2]
  (->>
   (map + col1 col2)
   (map div-by-2)
   )
  )


(defn EVQWGD001-place [shape]
  (->> shape
       ;(rdz 90)
       ;(rdy -20)
       (translate [0 0 (- (+ EVQWGD001-plastic-height plate-thickness))])
       (rdx 80)
       (rdz -57)
       (rdy 25)
      ;(rdz 20)
       ;(rdy 45)
       (translate [(- tps-65-mount-corner-radius (/ tps-65-width 2)) 0 0])
       ;(translate [(- tps-65-mount-corner-radius-with-offset) tps-65-mount-corner-radius-with-offset 0])
       ;(rdz -90)
       ;(rdy -90)
       (rdz (/ (+ tps-65-z-rotation 35) 2))
       ;(rdz tps-65-z-rotation)
       (rdx (/ (+ tps-65-x-rotation 6) 2))
       (rdy (/ (+ tps-65-y-rotation -32) 2))
       ;(rdz (/ (+ tps-65-z-rotation 35) 2))
       ;(rdx (/ (+ tps-65-x-rotation 6) 2))
       ;(rdy (/ (+ tps-65-y-rotation -32) 2))
       (translate (col-avg (left-wall-plate-position 0 (+ left-wall-y-modifier -3)) thumborigin))
      (translate (col-avg br-minithumb-loc [0 0 tps-65-z-position]) )
       (translate [-10 10 2])
       )
  )

(defn EVQWGD001-translate-and-place [x y z shape]
  (->> shape
       (translate [x y z])
       (EVQWGD001-place)
       )
  )
(defn EVQWGD001-translate-and-place-at-position [position shape]
  (->> shape
       (translate position)
       (EVQWGD001-place)))

(defn tps-65-translate-and-place [ x y z shape]
  (->> shape 
       (translate [x y z])
       (tps-65-place)     
       )
  )
 
 (defn tps-65-translate-and-place-at-position [position shape]
   (->> shape
        (translate position)
        (tps-65-place)))



(defn tps-65-translate-and-place-with-radius [position radius-compensation-x radius-compensation-y shape]
  (->> shape
       (translate position)
       ;plate-thickness
       (translate [(tps-radius-compensation-adjust radius-compensation-x) (tps-radius-compensation-adjust radius-compensation-y) (- plate-thickness)])
       (tps-65-place)
       ))

(defn tps-65-translate-and-place-with-radius-xyz [x y z radius-compensation-x radius-compensation-y shape]
  (->> shape
       (translate [x y z])
       ;plate-thickness
       (translate [(tps-radius-compensation-adjust radius-compensation-x) (tps-radius-compensation-adjust radius-compensation-y) (- plate-thickness)])
       (tps-65-place)))

(defn screen-holder-rotate [shape]
  (->> shape
       (rdx (cond 
              (= nrows 5) -5
              (= nrows 4) -15))
(rdy 7.5)
(rdz screen-rotation-angle)      
       ))
 
(defn screen-holder-place [shape]
  (->> shape
       (screen-holder-rotate)
       (left-wall-plate-place 0 (+ left-wall-y-modifier 0.5))
       (translate screen-holder-position)
       (translate [0 0 (cond (= nrows 5) 0 (= nrows 4) -4)])))

(defn screen-holder-place-x-y [xdir ydir shape]
  (->> shape
       (translate [(* xdir oled-holder-width 0.5) (* ydir oled-holder-height 0.5) (/ screen-holder-depth 2)])
       (screen-holder-rotate)
       (translate [(- (* xdir oled-holder-width 0.5)) (- (* ydir oled-holder-height 0.5)) 0])
       (left-wall-plate-place xdir ydir)

       (translate screen-holder-position)
       ))

(defn screen-holder-translate-and-place [x y z shape]
  (->> shape
       (translate [x y z])
       (screen-holder-place))
  )

(def screen-holder-rotate-side-y -70)
(defn screen-holder-rotate-side [shape]
  (->> shape
       (rdz -80)
      (rdy screen-holder-rotate-side-y)
      (rdx -10) 
  )
)
(defn screen-holder-place-side [shape]
  (->> shape
       (screen-holder-rotate-side)
       (translate [-6 0 0])
       ;(rdz 5)
       (left-wall-plate-place -2 -3)
       (translate [0 0 (- 2.5 keyboard-z-offset 3.5)])))

(defn screen-holder-translate-and-place-side [x y z shape]
  (->> shape 
       (translate [x y z])
       (screen-holder-place-side)
       ))

(def right-wall
  (let [tr (if (true? pinky-15u) wide-post-tr oled-post-tr)
        br (if (true? pinky-15u) wide-post-br oled-post-br)]
    (union (key-wall-brace lastcol 0 0 1 tr lastcol 0 1 0 tr)
           (for [y (range 0 lastrow)] (key-wall-brace lastcol y 1 0 tr lastcol y 1 0 br))
           (for [y (range 1 lastrow)] (key-wall-brace lastcol (dec y) 1 0 br lastcol y 1 0 tr))
            (key-wall-brace lastcol cornerrow 0 -1 br lastcol cornerrow 1 0 br)
           )))

(def back-wall 
   (union(for [x (range 0 ncols)] (key-wall-brace x 0 0 1 oled-post-tl x       0 0 1 oled-post-tr))
(for [x (range 1 ncols)] (key-wall-brace x 0 0 1 oled-post-tl (dec x) 0 0 1 oled-post-tr ))
  ))

(def left-wall-x-furthest -1.5)
(def left-wall-y-furthest 1.75)

(def left-wall-4-rows
  ; left wall
   (union

    (when (= screen-holder-mount-position "screen-holder-mount-top")(wall-brace-xy (partial key-place 0 0) 0 1 oled-post-tl  (partial screen-holder-translate-and-place (/ screen-holder-height 2) (/ screen-holder-width 2)  0) 0 1 oled-post wall-xy-offset wall-xy-offset-thin))
    (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial key-place 0 0) 0 1 oled-post-tl (partial key-place 0 0) -1 1 oled-post-tl  wall-xy-offset wall-xy-offset))
    (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial key-place 0 0) -1 1 oled-post-tl (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset)) 1 1 oled-post  wall-xy-offset wall-xy-offset))
    (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset)) 1 1 oled-post (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 1 0 oled-post wall-xy-offset wall-xy-offset ))
    ;(-# (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 1 0 oled-post (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 0 oled-post wall-xy-offset wall-xy-offset)))

     (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 1 0 oled-post (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 oled-post wall-xy-offset wall-xy-offset))
     ;(when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 oled-post (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2) 0) 1 -1 oled-post wall-xy-offset wall-xy-offset-thin))
   
    ;(-# (when (= screen-holder-mount-position "screen-holder-mount-side")  (wall-brace-xy (partial screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2) 0) -1 -1 oled-post (partial screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2) 0) 0 -1 oled-post wall-xy-offset wall-xy-offset)))
   ;(when (= screen-holder-mount-position "screen-holder-mount-side")  (wall-brace-xy (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2) 0)  1 -1 oled-post (partial screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2) 0) -1 -1 oled-post wall-xy-offset wall-xy-offset))
    ; (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2) 0) 0 -1 oled-post  (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset) -1 0  oled-post wall-xy-offset wall-xy-offset-thin))
    (when (= screen-holder-mount-position "screen-holder-mount-side") (hull (bottom-hull ( screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2) 0 oled-post))  (wall-brace-xy-half-top (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left) 0 -1 oled-post wall-xy-offset)))
    (when (= screen-holder-mount-position "screen-holder-mount-side") (hull (bottom-hull (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2) 0 oled-post))  (wall-brace-xy-half-bottom (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left) 0 -1 oled-post wall-xy-offset)))
    ;(when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset) -1 0 oled-post   (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left) 0 -1 oled-post wall-xy-offset-thin wall-xy-offset))
    ;(when (= screen-holder-mount-position  "screen-holder-mount-top") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  (- tps-65-mount-corner-radius) ) 0 1 oled-post  (partial screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0) 0 0 oled-post wall-xy-offset-thin wall-xy-offset-thin))
    (when (= screen-holder-mount-position  "screen-holder-mount-top") (wall-brace-xy (partial screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0) 0 1 oled-post  (partial screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0) 0 1 oled-post wall-xy-offset-thin wall-xy-offset-thin))
    ;(wall-brace-xy (partial screen-holder-place-x-y  1.5   (+ left-wall-y-modifier left-wall-y-furthest -0.25)) 0 1 oled-post  (partial screen-holder-place-x-y left-wall-x-furthest    (+ left-wall-y-modifier left-wall-y-furthest -0.5)) 0 1 oled-post wall-xy-offset-thin wall-xy-offset-thin)
    (when (= screen-holder-mount-position  "screen-holder-mount-top") (wall-brace-xy (partial screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0) 0 1 oled-post  (partial screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0) 0 1 oled-post wall-xy-offset-thin wall-xy-offset-thin))
    ;(color [0 1 0 1](wall-brace-xy (partial screen-holder-place-x-y left-wall-x-furthest (+ left-wall-y-modifier left-wall-y-furthest -0.5)) 0 1 oled-post  (partial screen-holder-place-x-y left-wall-x-furthest    (+ left-wall-y-modifier left-wall-y-furthest -0.5)) -1 0 oled-post wall-xy-offset-thin wall-xy-offset-thin))
    ( when (= screen-holder-mount-position "screen-holder-mount-top") (color [0 1 0 1] (wall-brace-xy (partial screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0) 0 1 oled-post  (partial screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2) 0) -1 0 oled-post wall-xy-offset-thin wall-xy-offset-thin)))
    (when (= screen-holder-mount-position "screen-holder-mount-top")(color [1 0 0 1](wall-brace-xy (partial screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0) -1 0 oled-post  (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset   tps-65-mount-corner-radius-with-offset ) 1 -1 oled-post wall-xy-offset-thin wall-xy-offset-thin)))
    (when (= screen-holder-mount-position "screen-holder-mount-top") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset ) 1 -1 oled-post  (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset ) tps-65-mount-corner-radius-with-offset ) 0 0  oled-post wall-xy-offset-thin wall-xy-offset))
    ;(wall-brace (partial  translate (left-wall-plate-position left-wall-x-furthest (+ left-wall-y-modifier  -5.5))) 0 0  oled-post  (partial thumb-bl-place) -1 -1  thumb-post-tl)
    (wall-brace-xy   (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left) 0 -1 oled-post (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right) 0 -1 oled-post wall-xy-offset wall-xy-offset) 
    (wall-brace-xy (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right) 0 -1 oled-post (partial thumb-bl-place) -1 0 oled-post-tl wall-xy-offset wall-xy-offset)
    ;(wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset ) tps-65-mount-corner-radius-with-offset ) 0 0  oled-post  (partial thumb-bl-place) -1 0 oled-post-tl wall-xy-offset-thin wall-xy-offset)
    ;(wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position  0 tps-65-mount-corner-radius-with-offset) 0 -1  oled-post  (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset) tps-65-mount-corner-radius-with-offset) 0 -1  oled-post wall-xy-offset-thin wall-xy-offset-thin)
    ;(when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 oled-post (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2) 0) 1 -1 oled-post wall-xy-offset-thin wall-xy-offset-thin false))
   ;(when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 oled-post (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2) 0) 1 -1 oled-post wall-xy-offset wall-xy-offset false))
  ;;  (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy  (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2) 0) 1 -1 oled-post (partial screen-holder-translate-and-place-side (/  screen-holder-height 2) (/ (+ screen-holder-width) 2) 0) 0 0 oled-post wall-xy-offset-thin wall-xy-offset-thin false))
  ;;   (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy  (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2) 0) 1 -1 oled-post (partial screen-holder-translate-and-place-side (/  screen-holder-height 2) (/ (- screen-holder-width) 2) 0) 0 0 oled-post wall-xy-offset-thin wall-xy-offset-thin false))
  ;;  (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy  (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2) 0) 1 -1 oled-post (partial screen-holder-translate-and-place-side (/  (- screen-holder-height) 2) (/ (- screen-holder-width) 2) 0) 0 0 oled-post wall-xy-offset-thin wall-xy-offset-thin false))
  ; (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 oled-post (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset) -1 0  oled-post wall-xy-offset-thin wall-xy-offset-thin false))
   ; (-# (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset ) tps-65-mount-corner-radius-with-offset ) -1 0  oled-post  (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left) 0 -2 oled-post wall-xy-offset-thin wall-xy-offset))
   ; (wall-brace-xy (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left) 0 -2 oled-post (partial thumb-bl-place) -1 0 oled-post-tl wall-xy-offset wall-xy-offset)
;(wall-brace (partial key-place 0 0) 0 1 oled-post-tl (partial left-key-place 0 1) 0 1 oled-post)
;(wall-brace (partial left-key-place 0 1) 0 1 oled-post (partial left-key-place 0 1) -1 0 oled-post)
    ))

(def left-wall-5-rows
  ; left wall
  (union
;    (for [y (range 0 lastrow)] (union (wall-brace (partial left-key-place y 1)       -1 0 web-post (partial left-key-place y -1) -1 0 web-post)
;                                     (hull (key-place 0 y web-post-tl)
;                                           (key-place 0 y web-post-bl)
;                                           (left-key-place y  1 web-post)
;                                           (left-key-place y -1 web-post))))
;
;
;(for [y (range 1 lastrow)] (union (wall-brace (partial left-key-place (dec y) -1) -1 0 web-post (partial left-key-place y  1) -1 0 web-post)
;                                  (hull (key-place 0 y       web-post-tl)
;                                        (key-place 0 (dec y) web-post-bl)
;                                        (left-key-place y        1 web-post)
;                                        (left-key-place (dec y) -1 web-post))))
   (wall-brace-xy (partial key-place 0 0) 0 1 web-post-tl  (partial left-wall-plate-place 1 2.5) 0 1 oled-post wall-xy-offset wall-xy-offset-thin)
   (wall-brace-xy  (partial left-wall-plate-place 1 1.5) 0 1 oled-post  (partial left-wall-plate-place -2.5 1.5) 0 1 oled-post wall-xy-offset-thin wall-xy-offset-thin)
   (wall-brace-xy  (partial left-wall-plate-place -2.5 1.5) 0 1 oled-post  (partial left-wall-plate-place -2.5 1.5) -1 0 oled-post wall-xy-offset-thin wall-xy-offset-thin)
   (wall-brace-xy  (partial left-wall-plate-place -2.5 1.5) -1 0 oled-post  (partial left-wall-plate-place -2.5 -1) -1 -1 oled-post wall-xy-offset-thin wall-xy-offset-thin)
   (wall-brace-xy (partial left-wall-plate-place -2.5 -1) -1 -1 oled-post  (partial left-wall-plate-place -2.5 -5) 0 0  web-post-br wall-xy-offset-thin wall-xy-offset)
   (wall-brace-xy (partial left-wall-plate-place -2.5 -5) 0 0  web-post-br  (partial thumb-bl-place) -1 0  thumb-post-tl wall-xy-offset-thin wall-xy-offset)
;(wall-brace (partial key-place 0 0) 0 1 web-post-tl (partial left-key-place 0 1) 0 1 web-post)
;(wall-brace (partial left-key-place 0 1) 0 1 web-post (partial left-key-place 0 1) -1 0 web-post)
   ))

(when (= nrows 5)
 (def left-wall left-wall-4-rows) 
  )

(when (= nrows 4)
  (def left-wall left-wall-4-rows))

    
(def front-wall
  (union 
    

 
   ;(key-wall-brace )
    
   (cond (> ncols 5)
(union 
 (for [x (range 4 ncols)] (key-wall-brace x cornerrow 0 -1 oled-post-bl x       cornerrow 0 -1 oled-post-br)) ; TODO fix extra wall
(for [x (range 5 ncols)] (key-wall-brace x cornerrow 0 -1 oled-post-bl (dec x) cornerrow 0 -1 oled-post-br))
 (key-wall-brace 3 lastrow 0.5 -1 oled-post-br 4 cornerrow 0.5 -1 oled-post-bl)
 (key-wall-brace 3 lastrow   0 -1 oled-post-bl 3 lastrow 0.5 -1 oled-post-br)
 )
     :else
 (union
  (key-wall-brace 3 lastrow   0 -1 oled-post-bl 3 lastrow 0 -1 oled-post-br)
  (key-wall-brace 3 lastrow 0 -1 oled-post-br 4 cornerrow 0 -1 oled-post-br)
(hull 
 (key-place lastcol cornerrow web-post-bl)
 (key-place lastcol cornerrow web-post-br)
 (key-place (dec lastcol) lastrow web-post-br)
 )
  )    
         ))
  )

(def thumb-walls
  (union 
    (wall-brace thumb-mr-place  0 -1 oled-post-br thumb-tr-place  0 -1 oled-post-br)
(wall-brace thumb-mr-place  0 -1 oled-post-br thumb-mr-place  0 -1 oled-post-bl)
(wall-brace thumb-br-place  0 -1 oled-post-br thumb-br-place  0 -1 oled-post-bl)
;(wall-brace thumb-bl-place  0  1 oled-post-tr thumb-bl-place  0  1 oled-post-tl)
(wall-brace thumb-br-place -1  0 oled-post-tl thumb-br-place -1  0 oled-post-bl)
(wall-brace thumb-bl-place -1  0 oled-post-tl thumb-bl-place -1  0 oled-post-bl)
   ))

(def thumb-corners
  (union 
     (wall-brace thumb-br-place -1  0 oled-post-bl thumb-br-place  0 -1 oled-post-bl)
;(wall-brace thumb-bl-place -1  0 oled-post-tl thumb-bl-place  0  1 oled-post-tl)
   ))

(def thumb-tweeners
    (union 
     (wall-brace thumb-mr-place  0 -1 oled-post-bl thumb-br-place  0 -1 oled-post-br)
(wall-brace thumb-bl-place -1  0 oled-post-bl thumb-br-place -1  0 oled-post-tl)
(wall-brace thumb-tr-place  0 -1 oled-post-br (partial key-place 3 lastrow)  0 -1 oled-post-bl)
     )
  )





(def left-side 
  (triangle-hulls
   
   (triangle-hulls
    (screen-holder-place (translate [(/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0] web-post))
                    ;(left-wall-plate-place left-wall-x-furthest  (+ left-wall-y-modifier  -1) web-post)
    (screen-holder-place (translate [(/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0] web-post))
    (left-wall-plate-place left-wall-x-furthest    (+ left-wall-y-modifier left-wall-y-furthest -0.5) web-post)
                        ; (tps-65-place (translate tps-65-mount-corner-cylinder-top-right-position oled-post))

    (translate [0 5 0]  (tps-65-place (translate tps-65-mount-corner-cylinder-top-right-position tps-65-mount-post)))
    (screen-holder-place (translate [(/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0] web-post))
    (left-wall-plate-place left-wall-x-furthest    (+ left-wall-y-modifier left-wall-y-furthest -0.5) web-post))
   
   (triangle-hulls

    (tps-65-place (translate tps-65-mount-corner-cylinder-top-right-position (rdz -90 tps-65-mount-corner-quarter-cylinder)))
                    ;(translate [0 0 0] (screen-holder-place (translate [(/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2) 0] web-post)))
    (left-wall-plate-place left-wall-x-furthest    (+ left-wall-y-modifier left-wall-y-furthest -0.5) web-post)
    (left-wall-plate-place left-wall-x-furthest  (+ left-wall-y-modifier  -1) web-post)

                  ; (translate [0 1 0] (screen-holder-place (translate [(/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2) screen-holder-depth] web-post)))
                  ; (tps-65-place (translate tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-cylinder))
                  ; (translate [0 0.5 0] (screen-holder-place (translate [(/ (- screen-holder-height) 2) (/ screen-holder-width 2)  (- screen-holder-depth)] web-post)))

   ; (translate [0 2 0] (left-wall-plate-place left-wall-x-furthest    (+ left-wall-y-modifier left-wall-y-furthest -0.5) oled-post))
    )
   
   
      (tps-65-place (translate tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-cylinder))
   (left-wall-plate-place left-wall-x-furthest  (+ left-wall-y-modifier  -1) web-post)
   (tps-65-place (translate tps-65-mount-corner-cylinder-top-left-position tps-65-mount-corner-cylinder))

   ( translate (left-wall-plate-position left-wall-x-furthest (+ left-wall-y-modifier  -5.5)) web-post)
   (left-wall-plate-place left-wall-x-furthest  (+ left-wall-y-modifier  -1) web-post)
   (tps-65-place (translate tps-65-mount-corner-cylinder-top-left-position tps-65-mount-corner-cylinder))
   
   )
  )


(def left-side-new
  (union 
   (triangle-hulls
   (  screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ screen-holder-width 2)  0 oled-post)
   (screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post) 
      (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset   tps-65-mount-corner-radius-with-offset oled-post)
    
          (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset   tps-65-mount-corner-radius-with-offset oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
    (screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)

    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset   tps-65-mount-corner-radius-with-offset oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius)  tps-65-mount-corner-radius oled-post)

    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset   tps-65-mount-corner-radius-with-offset oled-post)
    (color [1 0 0 1](tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset)  tps-65-mount-corner-radius oled-post))
    (color [0 1 0 1](tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset)  tps-65-mount-corner-radius-with-offset oled-post))


    )
   )
)
  

(def thumb-side-normal
  
  (triangle-hulls
    (translate (left-wall-plate-position left-wall-x-furthest (+ left-wall-y-modifier  -5.5)) oled-post)
   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset)  tps-65-mount-corner-radius-with-offset oled-post)
   (thumb-bl-place thumb-post-tl)

   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset)  tps-65-mount-corner-radius-with-offset oled-post)
(thumb-bl-place thumb-post-tl)
   (thumb-bl-place thumb-post-tr)

   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset)  tps-65-mount-corner-radius-with-offset oled-post)
   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset)  (- tps-65-mount-corner-radius-with-offset) oled-post)
   (thumb-bl-place thumb-post-tl)

   (thumb-bl-place thumb-post-tr)
(thumb-tl-place thumb-post-tl)
   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset)  (- tps-65-mount-corner-radius-with-offset) oled-post)

   )
  )

(def thumb-side-EVQWGD001-mount
  (union
   (hull 
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
    (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left oled-post)
    (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/ (- screen-holder-width) 2) 0 oled-post)
    )
 (triangle-hulls (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
(EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left oled-post)
(EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-left oled-post))
   
;;    (-# (triangle-hulls (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
;; (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left oled-post)
;; (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left oled-post)))
   
   (triangle-hulls (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
(EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post))
   
   
   (triangle-hulls (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
                   (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-left oled-post)
                   (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post))
   
   (triangle-hulls (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
                       (thumb-bl-place web-post-tr)
                   (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post))
   
   (hull
    ;(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
    
    (thumb-bl-place web-post-tr)
    (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right oled-post)
    (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post)
    )
   
;;    (-# (hull
;;      (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-left oled-post)
;; (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post)
;; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
;;     ))
   (triangle-hulls
     

  
   
  ;(EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right oled-post)
  ; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset)  (- tps-65-mount-corner-radius-with-offset) oled-post)
  ; (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post)

 ; (thumb-bl-place thumb-post-tr)
  (thumb-tl-place web-post-tl)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
    (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post)

    (thumb-bl-place web-post-tr)
   (thumb-tl-place web-post-tl)
   (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post)

   (thumb-bl-place web-post-tr)
   (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post)
   (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right oled-post)

   (thumb-bl-place web-post-tr)
    (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right oled-post)
 ( thumb-bl-place web-post-tl)
    )
   
;;    (-# (triangle-hulls
;;     (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left oled-post)
;; (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right oled-post)
;; (thumb-bl-place web-post-tl)))
   
  (triangle-hulls
     (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
(thumb-bl-place web-post-tr)
(thumb-tl-place web-post-tl)
    )
   )
)
  
  (def thumb-side
    (if EVQWGD001-mount thumb-side-EVQWGD001-mount thumb-side-normal)
    )

(def thumb-side-to-trackpad-mount 
(triangle-hulls
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-mod)  tps-65-mount-corner-radius oled-post)

   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-mod)  tps-65-mount-corner-radius oled-post)
   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius)  (- tps-65-mount-corner-radius) oled-post)
   )
  
  
  )


(def row-at-top-of-trackpad
  (cond
    (= nrows 5) 1
    (= nrows 4) 0))

(def right-side
  (union
 ;  (triangle-hulls
 ;     (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset) (- tps-65-mount-corner-radius-with-offset) oled-post)
 ; (thumb-bl-place thumb-post-tl)
 ;  (key-place 0 cornerrow web-post-bl)

;   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset) (- tps-65-mount-corner-radius-with-offset) oled-post)
;(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
;(key-place 0 cornerrow web-post-bl)
   
   

   
  ; )
;   (for [x (range 0 (+ cornerrow 1))]
;     (triangle-hulls
;      (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
;      (key-place 0 x web-post-bl)
;      (key-place 0 x web-post-tl)
;      
;       
;      
;      ))
;   
;   (for [x (range 0 cornerrow)]
;     ( triangle-hulls
;      
;      (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
;(key-place 0 x web-post-bl)
;(key-place 0 (+ x 1) web-post-tl))
;
;     )
   (triangle-hulls

(when (= screen-holder-mount-position "screen-holder-mount-top") 
  (key-place 0 0 web-post-tl)
    (screen-holder-translate-and-place (/  screen-holder-height 2) (/ screen-holder-width 2)  0 oled-post)
    (screen-holder-translate-and-place (/  screen-holder-height 2) (/ (- screen-holder-width) 2)  0 oled-post)
    
    (key-place 0 0 web-post-tl)
    (key-place 0 0 web-post-bl)
    (screen-holder-translate-and-place (/  screen-holder-height 2) (/ screen-holder-width 2)  0 oled-post)

     (key-place 0 0 web-post-bl)
(screen-holder-translate-and-place (/  screen-holder-height 2) (/ screen-holder-width 2)  0 oled-post)
    (screen-holder-translate-and-place (/  screen-holder-height 2) (/ (- screen-holder-width) 2)  0 oled-post)
     
(key-place 0 0 web-post-bl)
    (key-place 0 1 web-post-tl)
    (screen-holder-translate-and-place (/  screen-holder-height 2) (/ (- screen-holder-width) 2)  0 oled-post))

   
    
 ;   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
 ;  (key-place 0 0 web-post-tl)
 ; (translate [0 0 -1] (left-wall-plate-place  1.5   (+ left-wall-y-modifier left-wall-y-furthest -0.25) oled-post))

 ;  (left-wall-plate-place  1.5   (+ left-wall-y-modifier left-wall-y-furthest -0.25) oled-post)
 ;  (translate [0 0 0] (screen-holder-place (translate [(/ screen-holder-height 2) (/  screen-holder-width 2) 0] web-post)))
 ;   (translate [0 0 0] (screen-holder-place (translate [(/ screen-holder-height 2)  (/  (- screen-holder-width) 2) 0] web-post)))
    
;     (left-wall-plate-place  1.5   (+ left-wall-y-modifier left-wall-y-furthest -0.25) oled-post)
;(translate [0 5 0]  (tps-65-place (translate tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-post)))
;(translate [0 0 0] (screen-holder-place (translate [(/ screen-holder-height 2)  (/  (- screen-holder-width) 2) 0] web-post)))
   ;  (translate [0 0 0] (screen-holder-place (translate [(/ screen-holder-height 2) (/  screen-holder-width 2) 0] web-post)))
   ;(translate [0 0 0] (screen-holder-place (translate [(/ screen-holder-height 2)  (/  (- screen-holder-width) 2) 0] web-post)))
   ;  (tps-65-place (translate tps-65-mount-corner-cylinder-bottom-right-position web-post))
    )
       
   (triangle-hulls
    (thumb-tl-place web-post-tl)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
    (key-place 0 cornerrow web-post-bl)

    (key-place 0 cornerrow web-post-bl)
    (key-place 0 cornerrow web-post-tl)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)

    (key-place 0 (- cornerrow 1) web-post-bl)
    (key-place 0 cornerrow web-post-tl)
    (tps-65-translate-and-place-with-radius-xyz  0 (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)

    (key-place 0 cornerrow web-post-tl)
    (tps-65-translate-and-place-with-radius-xyz  0 (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)

    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius)  (- tps-65-mount-corner-radius) oled-post)
    (tps-65-translate-and-place-with-radius-xyz  0 (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)

    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius)  (- tps-65-mount-corner-radius) oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post))


 ( triangle-hulls
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius (- tps-65-mount-corner-radius) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius)  (- tps-65-mount-corner-radius) oled-post)
 

  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius (- tps-65-mount-corner-radius) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius)  (- tps-65-mount-corner-radius) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)


 
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius (- tps-65-mount-corner-radius) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius)  (- tps-65-mount-corner-radius) oled-post)
  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
 
 (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
 (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
  (tps-65-translate-and-place-with-radius-xyz  0 (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)
  )
  
;    (tps-65-translate-and-place-with-radius-xyz  0 (- (- (/ tps-65-length 2) tps-65-corner-radius)) 0 0 (- tps-65-mount-corner-radius) oled-post)
;(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset)  (- tps-65-mount-corner-radius-with-offset) oled-post)
;    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset)  (- tps-65-mount-corner-radius-with-offset) oled-post)

(triangle-hulls
   (key-place 0 (- cornerrow 1) web-post-bl)
   (key-place 0 (- cornerrow 1) web-post-tl)
   (tps-65-translate-and-place-with-radius-xyz  0 (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)

    (key-place 0 (- cornerrow 1) web-post-tl)
    (tps-65-translate-and-place-with-radius-xyz  0 (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)
    (tps-65-translate-and-place-with-radius-xyz  (/ tps-65-width 3) (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)

    (key-place 0 (- cornerrow 1) web-post-tl)
    (key-place 0 row-at-top-of-trackpad web-post-bl)
    (tps-65-translate-and-place-with-radius-xyz  (/ tps-65-width 3) (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)

    (key-place 0 row-at-top-of-trackpad web-post-bl)
    (tps-65-translate-and-place-with-radius-xyz  (/ tps-65-width 3) (- (- (/ tps-65-length 2) tps-65-mount-corner-radius)) 0 0 (- tps-65-mount-corner-radius-with-offset) oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)

    (key-place 0 row-at-top-of-trackpad web-post-bl)
    (key-place 0 row-at-top-of-trackpad web-post-tl)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
    
    
    ;(key-place 0 row-at-top-of-trackpad web-post-tl)
    ;(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius (- tps-65-mount-corner-radius) oled-post)
    ;(screen-holder-translate-and-place (/ screen-holder-height 2) (- (/ screen-holder-width 2)) 0 oled-post)
    )
   
   ))

 (def tps-65-mount-cyl
   (binding [*fn* 100] (cylinder tps-65-mount-corner-radius 2 :center true))
   )
 
 
(def between-screen-and-trackpad-top
  (triangle-hulls
        (screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
(screen-holder-translate-and-place (/ screen-holder-height 2) (/ (- screen-holder-width) 2)  0 oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius (- tps-65-mount-corner-radius) oled-post)


(screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
(screen-holder-translate-and-place (/ screen-holder-height 2) (/ (- screen-holder-width) 2)  0 oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)

   ;(translate [0 0 0] (screen-holder-place (translate [0 (/  (- screen-holder-width) 2) 0] web-post)))
(screen-holder-translate-and-place 0 (/  (- screen-holder-width) 2) 0 oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius (- tps-65-mount-corner-radius) oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)

(screen-holder-translate-and-place (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
(screen-holder-translate-and-place (/ screen-holder-height 2) (/ (- screen-holder-width) 2)  0 oled-post)
(tps-65-translate-and-place-with-radius-xyz (- (/ tps-65-width 2) tps-65-corner-radius) 0 0 tps-65-mount-corner-radius 0 oled-post)
)
  )
 
 (defn bottom-of-screen-bezier [place]
   
   (mapv (fn [point]
          (->>
           (translate point curve-post)
           (rdx 10)
           (rdy  screen-holder-rotate-side-y)
           (place))
   )
           (fillet-about-point-xy-2 0 -1 wall-xy-offset-thin 20)
   )
   
 )
 (def between-screen-and-trackpad-side 
  (union
   (hull
    (wall-bezier-1-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 wall-xy-offset oled-post)
    (wall-bezier-1-xy (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0) 0 1 wall-xy-offset-thin oled-post))
   (hull
    (wall-bezier-1-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 wall-xy-offset oled-post)
    ;(wall-bezier-1-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset) 0 1 wall-xy-offset-thin oled-post)
    ( screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
    (wall-bezier-1-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 wall-xy-offset oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
    )

   (hull
    (wall-bezier-2-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 wall-xy-offset oled-post)

    (wall-bezier-1-xy (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0) 0 1 wall-xy-offset-thin oled-post)
    ( screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
    (bottom 0.001 (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post))

    (bottom 0.001 (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset (translate (wall-locate3 0 1) oled-post))))

   (hull
    (wall-bezier-1-xy (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0) 0 1 wall-xy-offset-thin oled-post)
    ;(wall-bezier-1-xy (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0) 0 1 wall-xy-offset-thin oled-post)
    (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
    (screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post))

   (hull
    (wall-bezier-2-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 wall-xy-offset oled-post)
    (wall-bezier-1-xy (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0) 0 1 wall-xy-offset-thin oled-post)
    ( screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
    )

   (bottom-hull 
    
    (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
    (screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
;    ( screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 curve-post)
;( screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  oled-post-size  curve-post)
;    (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 curve-post)
;(screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  oled-post-size  oled-)
    )

;          (triangle-hulls
;    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
;(screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
;(screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/  screen-holder-width 2)  0  oled-post))

;;  (triangle-hulls
  ;; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
;;  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
;;  (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post))

  ;(triangle-hulls
  ; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)  
  ; (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/  screen-holder-width 2)  0 oled-post)
  ; (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post))
   (triangle-hulls
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
    (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post))

   (triangle-hulls
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)

    (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post)
    (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2)  0 oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)

    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
    (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)

    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius)  tps-65-mount-corner-radius oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post))
   (triangle-hulls
    (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post)
    (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2)  0 oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post))) 
    


;(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius)  tps-65-mount-corner-radius oled-post)
;(screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post)
;(screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/  screen-holder-width 2)  0 oled-post)
    
  ; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius)  tps-65-mount-corner-radius oled-post) 
  ;  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
  ; (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/  screen-holder-width 2)  0 oled-post)
    
;    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius)  tps-65-mount-corner-radius oled-post)
;    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset)  tps-65-mount-corner-radius-with-offset oled-post)
;(screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post)
    
    )

(def top-side
  (union  
   (triangle-hulls
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius (- tps-65-mount-corner-radius) oled-post) 

     (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post)
    
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset tps-65-mount-corner-radius-with-offset oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius (- tps-65-mount-corner-radius) oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
    )
   
   (triangle-hulls
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset tps-65-mount-corner-radius-with-offset oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset) oled-post) 
(key-place 0 0 web-post-tl)
    )
   
   )
  )
(def between-screen-and-trackpad 
    
     (cond
  (= screen-holder-mount-position "screen-holder-mount-top") between-screen-and-trackpad-top
  
 (= screen-holder-mount-position "screen-holder-mount-side") between-screen-and-trackpad-side
   
    
   ))
   


(def left-section
  (union
   (when (= screen-holder-mount-position "screen-holder-mount-top")(screen-holder-place screen-holder))
   (when (= screen-holder-mount-position "screen-holder-mount-side") (screen-holder-place-side screen-holder))
   ;left-side
   (when (= screen-holder-mount-position "screen-holder-mount-top") ( color [0 1 0]left-side-new))
    thumb-side
   thumb-side-to-trackpad-mount
   right-side
    between-screen-and-trackpad
   top-side
   
  

    ))
(def default-thumb-wall 
  (union 
    ; thumb walls
   thumb-walls
   ; thumb corners
thumb-corners
   ; thumb tweeners
 thumb-tweeners
   ))
(def case-walls
  (union
   right-wall
   left-section
   ; back wall
  back-wall

   left-wall
  front-wall
   
  
   
   ))

(def cf-thumb-wall
  (union
   ; thumb walls
   (wall-brace cfthumb-mr-place  0 -1 web-post-br cfthumb-tr-place  0 -1 web-post-br)
   (wall-brace cfthumb-mr-place  0 -1 web-post-br cfthumb-mr-place  0 -1.15 web-post-bl)
   (wall-brace cfthumb-br-place  0 -1 web-post-br cfthumb-br-place  0 -1 web-post-bl)
   (wall-brace cfthumb-bl-place -0.3  1 thumb-post-tr cfthumb-bl-place  0  1 thumb-post-tl)
   (wall-brace cfthumb-br-place -1  0 web-post-tl cfthumb-br-place -1  0 web-post-bl)
   (wall-brace cfthumb-bl-place -1  0 thumb-post-tl cfthumb-bl-place -1  0 web-post-bl)
   ; cfthumb corners
   (wall-brace cfthumb-br-place -1  0 web-post-bl cfthumb-br-place  0 -1 web-post-bl)
   (wall-brace cfthumb-bl-place -1  0 thumb-post-tl cfthumb-bl-place  0  1 thumb-post-tl)
   ; cfthumb tweeners
   (wall-brace cfthumb-mr-place  0 -1.15 web-post-bl cfthumb-br-place  0 -1 web-post-br)
   (wall-brace cfthumb-bl-place -1  0 web-post-bl cfthumb-br-place -1  0 web-post-tl)
   (wall-brace cfthumb-tr-place  0 -1 web-post-br (partial key-place (+ innercol-offset 3) lastrow)  0 -1 web-post-bl)
   ; clunky bit on the top left cfthumb connection  (normal connectors don't work well)
   (bottom-hull
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (cfthumb-bl-place (translate (wall-locate2 -0.3 1) thumb-post-tr))
    (cfthumb-bl-place (translate (wall-locate3 -0.3 1) thumb-post-tr)))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (cfthumb-bl-place (translate (wall-locate2 -0.3 1) thumb-post-tr))
    (cfthumb-bl-place (translate (wall-locate3 -0.3 1) thumb-post-tr))
    (cfthumb-ml-place thumb-post-tl))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 web-post)
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (cfthumb-ml-place thumb-post-tl))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 web-post)
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
    (key-place 0 (- cornerrow innercol-offset) web-post-bl)
    (cfthumb-ml-place thumb-post-tl))
   (hull
    (cfthumb-bl-place thumb-post-tr)
    (cfthumb-bl-place (translate (wall-locate1 -0.3 1) thumb-post-tr))
    (cfthumb-bl-place (translate (wall-locate2 -0.3 1) thumb-post-tr))
    (cfthumb-bl-place (translate (wall-locate3 -0.3 1) thumb-post-tr))
    (cfthumb-ml-place thumb-post-tl))
   ; connectors below the inner column to the thumb & second column
   (if inner-column
     (union
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 0 (dec cornerrow) web-post-br)
       (key-place 0 cornerrow web-post-tr))
      (hull
       (key-place 0 cornerrow web-post-tr)
       (key-place 1 cornerrow web-post-tl)
       (key-place 1 cornerrow web-post-bl))
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 0 cornerrow web-post-tr)
       (key-place 1 cornerrow web-post-bl))
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 1 cornerrow web-post-bl)
       (cfthumb-ml-place thumb-post-tl))))))

(def inner-column-bottom-section
  (color [0.3 0.4 0.8 1] (union
                          (hull
                           (key-place 0 (dec cornerrow) web-post-bl)
                           (key-place 0 (dec cornerrow) web-post-br)
                           (key-place 0 cornerrow web-post-tr))

                          (hull
                           (key-place 0 (dec cornerrow) web-post-bl)
                           (key-place 0 cornerrow (translate (wall-locate1 0 0) web-post-tl))
                           (key-place 0 cornerrow web-post-tr))

                          (hull
                           (key-place 0 cornerrow web-post-tr)
                           (key-place 1 cornerrow web-post-tl)
                           (key-place 1 cornerrow web-post-bl))
                          (hull
                           (key-place 0 cornerrow (translate (wall-locate1 0 0) web-post-tl))
                           (key-place 0 cornerrow web-post-tr)
                           (key-place 0 cornerrow web-post-br))
     ;removed so it doesnt clash with the trackball
                            ; (if (or trackball-enabled joystick-enabled) nil (hull
                            ;                                                  (key-place 1 lastrow web-post-tl)
                            ;                                                  (key-place 1 cornerrow web-post-bl)
                            ;                                                  (minithumb-tl-place minithumb-post-tl)))

                          (triangle-hulls
                           (key-place 0  cornerrow (translate (wall-locate1 0 0) web-post-tl))
                           (key-place 0 cornerrow web-post-br)
                           (key-place 0 cornerrow (translate (wall-locate3 0 0) web-post-bl)))

                          (triangle-hulls
                           (key-place 0  cornerrow web-post-tr)
                           (key-place 1 cornerrow web-post-bl)
                           (key-place 0 cornerrow web-post-br)))))

;switching walls depending on thumb-style used
(def thumb-wall-type
  (case thumb-style
    "default" default-thumb-wall
    "cf" cf-thumb-wall
    "trackpad" default-thumb-wall
  )
)

(def usb-holder-ref (key-position 0 0 (map - (wall-locate2  0  -1) [0 (/ mount-height 2) 0])))

(def usb-holder-position (map + [17 19.3 0] [(first usb-holder-ref) (second usb-holder-ref) 2]))
(def usb-holder-cube   (cube 15 12 2))
(def usb-holder-space  (translate (map + usb-holder-position [0 (* -1 wall-thickness) 1]) usb-holder-cube))
(def usb-holder-holder (translate usb-holder-position (cube 19 12 4)))
(def usb-jack-height 4)
(def usb-jack-width 9.5)
(def usb-jack-cylinder 
  (->>
   (binding [*fn* 36](cylinder (/ usb-jack-height 2) (+ plate-thickness 1) :center true))
   (rdx 90)
   (translate [0 1 0]) 
   
   ))

(def usb-c-torus 
  (->>
   ;(binding [*fn* 36] (circle 1))
  ;(translate [(- (/ usb-jack-height 2) 1) 0 0 ])
   ;(extrude-rotate {:convexity 10})
   (fillet-circle (/ usb-jack-height 2))
   (rdx 90)
   (translate [0 plate-thickness 0])
   )
  )

(defn usb-jack-place [shape]  (translate (map + usb-holder-position [-12 6.5 3]) shape))
;(def usb-jack (usb-jack-place (cube 9.5 plate-thickness 3.1)))
(def usb-jack 
  (->>
   (union
     (hull
      (translate [(- (/ usb-jack-height 2) (/ usb-jack-width 2)) 0 0] usb-jack-cylinder)
      (translate [(- (/ usb-jack-width 2) (/ usb-jack-height 2) ) 0 0] usb-jack-cylinder)
      
      )
    (union
     (translate [(- (/ usb-jack-height 2) (/ usb-jack-width 2)) 0 0] usb-c-torus)
     (translate [(* (- (/ usb-jack-height 2) (/ usb-jack-width 2) ) 0.75) 0 0] usb-c-torus)
     (translate [(/ (- (/ usb-jack-height 2) (/ usb-jack-width 2) )2) 0 0] usb-c-torus)
     (translate [(/ (- (/ usb-jack-height 2) (/ usb-jack-width 2) ) 4) 0 0] usb-c-torus)
     usb-c-torus
      (translate [(/ (- (/ usb-jack-width 2) (/ usb-jack-height 2)) 4) 0 0] usb-c-torus)
     (translate [(/ (- (/ usb-jack-width 2) (/ usb-jack-height 2)) 2) 0 0] usb-c-torus)
     (translate [(* (- (/ usb-jack-width 2) (/ usb-jack-height 2)) 0.75) 0 0] usb-c-torus)
    (translate [(- (/ usb-jack-width 2) (/ usb-jack-height 2)) 0 0] usb-c-torus))
      
     )
   (translate [0 -2 0])
   (usb-jack-place )
   ))

(def pro-micro-position (map + (key-position 0 1 (wall-locate3 -1 0)) [-6 2 -15]))
(def pro-micro-space-size [4 10 12]) ; z has no wall;
(def pro-micro-wall-thickness 2)
(def pro-micro-holder-size [(+ pro-micro-wall-thickness (first pro-micro-space-size)) (+ pro-micro-wall-thickness (second pro-micro-space-size)) (last pro-micro-space-size)])
(def pro-micro-space
  (->> (cube (first pro-micro-space-size) (second pro-micro-space-size) (last pro-micro-space-size))
       (translate [(- (first pro-micro-position) (/ pro-micro-wall-thickness 2)) (- (second pro-micro-position) (/ pro-micro-wall-thickness 2)) (last pro-micro-position)])))
(def pro-micro-holder
  (difference
   (->> (cube (first pro-micro-holder-size) (second pro-micro-holder-size) (last pro-micro-holder-size))
        (translate [(first pro-micro-position) (second pro-micro-position) (last pro-micro-position)]))
   pro-micro-space))

(def trrs-holder-size [6.2 10 2]) ; trrs jack PJ-320A
(def trrs-holder-hole-size [6.2 10 6]) ; trrs jack PJ-320A
(def trrs-holder-position  (map + usb-holder-position [-13.6 0 0]))
(def trrs-holder-thickness 2)
(def trrs-holder-thickness-2x (* 2 trrs-holder-thickness))
(def trrs-holder
  (union
   (->> (cube (+ (first trrs-holder-size) trrs-holder-thickness-2x) (+ trrs-holder-thickness (second trrs-holder-size)) (+ (last trrs-holder-size) trrs-holder-thickness))
        (translate [(first trrs-holder-position) (second trrs-holder-position) (/ (+ (last trrs-holder-size) trrs-holder-thickness) 2)]))))
(def trrs-holder-hole
  (union

  ; circle trrs hole
   (->>
    (->> (binding [*fn* 30] (cylinder 2.55 20))) ; 5mm trrs jack
    (rotate (deg2rad  90) [1 0 0])
    (translate [(first trrs-holder-position) (+ (second trrs-holder-position) (/ (+ (second trrs-holder-size) trrs-holder-thickness) 2)) (+ 3 (/ (+ (last trrs-holder-size) trrs-holder-thickness) 2))])) ;1.5 padding

  ; rectangular trrs holder
   (->> (apply cube trrs-holder-hole-size) (translate [(first trrs-holder-position) (+ (/ trrs-holder-thickness -2) (second trrs-holder-position)) (+ (/ (last trrs-holder-hole-size) 2) trrs-holder-thickness)]))))

(defn screw-insert-shape [bottom-radius top-radius height]
  (union
   (->> (binding [*fn* 30]
          (cylinder [bottom-radius top-radius] height)))
   (translate [0 0 (/ height 2)] (->> (binding [*fn* 30] (sphere top-radius))))))

(defn screw-insert [column row bottom-radius top-radius height offset]
  (let [shift-right   (= column lastcol)
        shift-left    (= column 0)
        shift-up      (and (not (or shift-right shift-left)) (= row 0))
        shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
        position      (if shift-up     (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
                          (if shift-down  (key-position column row (map - (wall-locate2  0 -1) [0 (/ mount-height 2) 0]))
                              (if shift-left (map + (left-key-position row 0) (wall-locate3 -1 0))
                                  (key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0])))))]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate (map + offset [(first position) (second position) (/ height 2)])))))

(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union 
         (screw-insert lastcol 0         bottom-radius top-radius height [-152 9 0])
;(screw-insert 0 lastrow   bottom-radius top-radius height [-50 7 0])
   (screw-insert 0 0         bottom-radius top-radius height [9 6 0])
         (screw-insert 0 lastrow   bottom-radius top-radius height [-17 -7 0])
    (screw-insert 2 0  bottom-radius top-radius height [0 -1 0])
(screw-insert 1 lastrow         bottom-radius top-radius height [0 -6 0])
          (screw-insert lastcol lastrow  bottom-radius top-radius height [-5 13 0])
          (screw-insert lastcol 0         bottom-radius top-radius height [-3 6 0])
       
   
         ))

; Hole Depth Y: 4.4
(def screw-insert-height 4)

; Hole Diameter C: 4.1-4.4
(def screw-insert-bottom-radius (/ 4.4 2))
(def screw-insert-top-radius (/ 4.4 2))
(def screw-insert-holes  (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))

; Wall Thickness W:\t1.65
(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1.5)))
(def screw-insert-screw-holes  (screw-insert-all-shapes 1.7 1.7 350))

(def pinky-connectors
  (apply union
         (concat
          ;; Row connections
          (for [row (range 0 lastrow)]
            (triangle-hulls
             (key-place lastcol row web-post-tr)
             (key-place lastcol row wide-post-tr)
             (key-place lastcol row web-post-br)
             (key-place lastcol row wide-post-br)))

          ;; Column connections
          (for [row (range 0 cornerrow)]
            (triangle-hulls
             (key-place lastcol row web-post-br)
             (key-place lastcol row wide-post-br)
             (key-place lastcol (inc row) web-post-tr)
             (key-place lastcol (inc row) wide-post-tr)))
          ;;
          )))

(def pinky-walls
  (union
   (key-wall-brace lastcol cornerrow 0 -1 web-post-br lastcol cornerrow 0 -1 wide-post-br)
   (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 0 1 wide-post-tr)))