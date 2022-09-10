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

 
 (defn bottom-translate [points-list]
   ;(println points-list)
   (for [points points-list]
     (for [p points]
       (assoc p 2 (- oled-holder-thickness))
           ;(println p)
           ; [(first p) (second p) 0]
       ))
;)                
   )

(def left-wall-x-offset 5) ; original 10
(def left-wall-z-offset 4) ; original 3
(def left-wall-x-offset-oled 
  (cond (= nrows 5) -10
        (= nrows 4 ) -10))

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

(defn left-wall-plate-place-rotate ([shape] (left-wall-plate-place-rotate rdx rdz shape))
  ([rotate-x-fn rotate-z-fn shape]
   (->> shape
   (rotate-x-fn oled-mount-rotation-x-old)
(rotate-z-fn oled-mount-rotation-z-old)))) 

(defn left-wall-plate-place ([xdir ydir shape ] (left-wall-plate-place xdir ydir translate rdx rdz shape))
  ([xdir ydir translate-fn rotate-x-fn rotate-z-fn shape](->> shape 
       (translate-fn (left-wall-plate-position xdir ydir))
       (rotate-x-fn oled-mount-rotation-x-old )
       (rotate-z-fn oled-mount-rotation-z-old ))))

(defn left-wall-plate-place-variable [xdir ydir shape]
  (->> shape
       (translate (left-wall-plate-position-variable xdir ydir))
       (rotate oled-mount-rotation-x-old [1 0 0])
       (rotate oled-mount-rotation-z-old [0 0 1])))

(defn wall-locate0 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) 0])
(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) -1])
(defn wall-locate2 [dx dy] [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset])
(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness)) (* dy (+ wall-xy-offset wall-thickness)) wall-z-offset])
(defn wall-locate3-for-polyhedron-point [dx dy]
[
 (* 0.98 (* dx (+ wall-xy-offset wall-thickness (+ (/ oled-post-size 2)))))
 (* 0.98 (* dy (+ wall-xy-offset wall-thickness (+ (/ oled-post-size 2)))))
 (+ 0.5 (- wall-z-offset (/ oled-holder-thickness  1)))
 ]
  )

(defn wall-locate22-xy [dx dy xy] [(* dx (/ xy 2)) (* dy (/ xy 2)) wall-z-offset])
(defn wall-locate2-xy [dx dy xy] [(* dx xy) (* dy xy) wall-z-offset])
(defn wall-locate2.5-xy [dx dy xy] [(* dx (+ xy wall-thickness)) (* dy (+ xy wall-thickness)) wall-z-offset])

(defn wall-locate3-xy [dx dy xy] [(* dx (+ xy wall-thickness)) (* dy (+ xy wall-thickness)) wall-z-offset])

(defn wall-locate3-xy-for-polyhedron-point [dx dy xy] 
  [(* 0.98 (* dx (+ xy wall-thickness (+ (/ oled-post-size 2)))))
   (* 0.98 (* dy (+ xy wall-thickness (+ (/ oled-post-size 2)))))
   (+ 0.5 (- wall-z-offset (/ oled-holder-thickness  1)))])

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
    (identical? post oled-post-bm)   web-post-bm
    :else post 
    )
  )

(defn convert-to-curve-post [post]
  (cond
    (or (identical? post oled-post-tr) (identical? post web-post-tr)) curve-post-tr
    (or (identical? post oled-post-tl) (identical? post web-post-tl)) curve-post-tl
    (or (identical? post oled-post-bl) (identical? post web-post-bl)) curve-post-bl
    (or (identical? post oled-post-br) (identical? post web-post-br))   curve-post-br
    (or (identical? post oled-post-bm) (identical? post web-post-bm))   curve-post-bm
    :else curve-post))

(defn convert-to-sphere-post [post]
  (cond
    (or (identical? post oled-post-tr) (identical? post web-post-tr)) sphere-post-tr
    (or (identical? post oled-post-tl) (identical? post web-post-tl)) sphere-post-tl
    (or (identical? post oled-post-bl) (identical? post web-post-bl)) sphere-post-bl
    (or (identical? post oled-post-br) (identical? post web-post-br))   sphere-post-br
    :else sphere-post))

(defn fillet-about-point ([dx dy steps] (fillet-about-point dx dy steps [0 0 0]))
  ([dx dy steps orig-point] (let [w1 (wall-locate1 dx dy)
         point1 [(* (first w1) 0.98) (* (second w1) 0.98) (+ (last w1) 0.0)]
        w2  (wall-locate3 dx dy)
        point2 [(* (first w2) 0.98) (* (second w2) 0.98) (+ (last w2) 0.0)]]
   
   (bezier-quadratic (mapv + point1 orig-point) 
                     (mapv + [(* dx wall-xy-offset) (* dy wall-xy-offset) 0] orig-point) 
                 ;[(* dx (+ wall-xy-offset wall-thickness (/ oled-post-size 1))) (* dy (+ wall-xy-offset wall-thickness (/ oled-post-size 1))) (+ wall-z-offset (/ oled-holder-thickness 1))]
                 (mapv + point2 orig-point) 
                     steps))
  )
  )




(defn fillet-about-point-2 ([dx dy steps] (fillet-about-point dx dy steps [0 0 0]) )
  ([dx dy steps orig-point] (let [w1 [(* dx (+ wall-xy-offset wall-thickness 
                     ;(- (/ oled-post-size 2))
                     )) (* dy (+ wall-xy-offset wall-thickness 
                                 ;(- (/ oled-post-size 2))
                                 )) (+ wall-z-offset 0.0)]
        point1 [(* (first w1) 0.98) (* (second w1) 0.98) (+ (last w1) 0.0)]
        w2 (wall-locate3 dx dy)
        point2 [(* (first w2) 0.98) (* (second w2) 0.98) (+ (last w2) 0.5)]]

    (bezier-quadratic ;point1 
        (mapv + point1 orig-point)
                     ; [(* dx (+ wall-xy-offset wall-thickness oled-post-size)) (* dy (+ wall-xy-offset wall-thickness oled-post-size)) 0]
                      (mapv + [(* dx (+ wall-xy-offset wall-thickness (+ (/ oled-post-size 2)))) (* dy (+ wall-xy-offset wall-thickness (+ (/ oled-post-size 2)))) (- wall-z-offset 1) ] orig-point)
                     (mapv + [(* 0.98 (* dx (+ wall-xy-offset wall-thickness (+ (/ oled-post-size 2))))) (* 0.98 (* dy (+ wall-xy-offset wall-thickness (+ (/ oled-post-size 2))))) (+ 0.5 (- wall-z-offset (/ oled-holder-thickness  1)))] orig-point)
                       steps))))


(defn fillet-about-point-xy [dx dy  xy steps]
  (let [w1 (wall-locate1 dx dy)
        point1 [(* (first w1) 0.98) (* (second w1) 0.98) (+ (last w1) 0.0)]
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
(defn get-web-corner-translation-vector [position]
  (case position
    "tr" web-post-tr-translation-vector
    "tl" web-post-tl-translation-vector
    "bl" web-post-bl-translation-vector
    "br" web-post-br-translation-vector
    "bm" web-post-bm-translation-vector
     [0 0 0]))

(defn get-oled-corner-translation-vector [position]
(case position
  "tr" oled-post-tr-translation-vector
  "tl" oled-post-tl-translation-vector
  "bl" oled-post-bl-translation-vector
  "br" oled-post-br-translation-vector
  "bm" oled-post-bm-translation-vector
    [0 0 0] 
  )
  )

(defn get-curve-corner-translation-vector [position]
  (case position
    "tr" curve-post-tr-translation-vector
    "tl" curve-post-tl-translation-vector
    "bl" curve-post-bl-translation-vector
    "br" curve-post-br-translation-vector
    "bm" curve-post-bm-translation-vector
    [0 0 0])
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
  


(defn plot-and-translate-bezier-points ([place bezier-points shape] (plot-and-translate-bezier-points place bezier-points shape [0 0 0]))
  ([place bezier-points shape translation-modifier] (plot-and-translate-bezier-points place bezier-points translate shape translation-modifier))
  ([place bezier-points translate-fn shape translation-modifier]
   (mapv (fn [point]
          (->> 
           (translate-fn point shape)
           (place)
           (translate-fn translation-modifier)
           )
          )
        bezier-points
        ))
  )

(defn plot-bezier-points [bezier-points shape]
  (mapv (fn [point]
          (translate point shape)
          )
bezier-points
        )
  )


(defn plot-and-translate-bezier-points-with-rotation [place bezier-points shape rotation-fn]
  (mapv (fn [point]
          (->>
           (translate point (rotation-fn shape))
           (place)))
        bezier-points))

(defn plot-bezier-points-with-rotation [bezier-points shape rotation-fn]
  (mapv (fn [point]
          (translate point (rotation-fn shape)))
        bezier-points))

(defn plot-bezier-points-variable-rotation [bezier-points rotation1 rotation2 rotate-x-fn rotate-y-fn rotate-z-fn variable-rotation-fn shape]
  (let [rot1-x (rotation1 :x)
        rot1-y (rotation1 :y)
        rot1-z (rotation1 :z) 
        rot2-x (rotation2 :x)
rot2-y (rotation2 :y)
rot2-z (rotation2 :z)
        
        rotation-variable-x (-  rot1-x rot2-x)
                           rotation-variable-y (- rot1-y rot2-y)
                           rotation-variable-z (- rot1-z rot2-z)
        number-of-slices (count bezier-points)
        x-rot-slice (/ rotation-variable-x number-of-slices)
       y-rot-slice (/ rotation-variable-y number-of-slices)
       z-rot-slice (/ rotation-variable-z number-of-slices) 
        ] 
    (for [index (range 0 number-of-slices)
          ] 
      (translate (nth bezier-points index) (variable-rotation-fn rot1-x rot1-y rot1-z x-rot-slice y-rot-slice z-rot-slice rotate-x-fn rotate-y-fn rotate-z-fn index shape))
      )
 ))

(defn wall-bezier-1 [place dx dy post]
  (hull
   (plot-and-translate-bezier-points (partial place)  (fillet-about-point dx dy 20)  (convert-to-curve-post post)))
  (place (check-post post))
  )

(defn wall-bezier-2 [place dx dy post]
  (plot-and-translate-bezier-points (partial place)  (fillet-about-point-2 dx dy 20)  (convert-to-curve-post post))) 

(defn wall-bezier-1-xy [place dx dy xy post]
  (hull 
   (plot-and-translate-bezier-points (partial place)  (fillet-about-point-xy dx dy xy 20)  (convert-to-curve-post post))
   (place (check-post post))
   ))

(defn wall-bezier-2-xy [place dx dy xy post]
  (hull
   (plot-and-translate-bezier-points (partial place)  (fillet-about-point-xy-2 dx dy xy 20)  (convert-to-curve-post post))
   (place (translate (wall-locate2-xy dx dy xy) post))
   ))
; dx1, dy1, dx2, dy2 = direction of the wall. '1' for front, '-1' for back, '0' for 'not in this direction'.
; place1, place2 = function that places an object at a location, typically refers to the center of a key position.
; post1, post2 = the shape that should be rendered
(defn wall-brace 
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
  ([place1 dx1 dy1 post1 place2 dx2 dy2 post2] (wall-brace place1 dx1 dy1 post1 place2 dx2 dy2 post2 (partial mapv +) (partial mapv +) (partial rdx 0) (partial rdx 0)))
 
 ([place1 dx1 dy1 post1 place2 dx2 dy2 post2 transform-fn1 transform-fn2 rotation-fn1 rotation-fn2 ] 
  (let [oled-translation-vector-z (last oled-translation-vector)
        bottom-pos1   (+ (* oled-holder-thickness 2) oled-translation-vector-z (last (transform-fn1 (wall-locate2 dx1 dy1))))
        bottom-pos2 (+ (* oled-holder-thickness 2) oled-translation-vector-z (last (transform-fn2 (wall-locate2 dx2 dy2))))
        lower-curve1  (plot-and-translate-bezier-points (partial place1)  (fillet-about-point-2 dx1 dy1 20)  (convert-to-curve-post post1))
        lower-curve2 (plot-and-translate-bezier-points (partial place2)  (fillet-about-point-2 dx2 dy2 20)  (convert-to-curve-post post2))
        last-curve-sphere1 (last lower-curve1)
        last-curve-sphere2 (last lower-curve2)
        ]  
   
   (union



;;     ( hull
;;     ;(hull a b)
;;      ;(hull b c)
;;     ; (hull c d)

    (hull
    (plot-and-translate-bezier-points (partial place1)  (fillet-about-point dx1 dy1 20)  (convert-to-curve-post post1))


;    (-# (plot-and-translate-bezier-points (partial place1)  (fillet-about-point-2 dx1 dy1 20)  (convert-to-curve-post post1)))
;(-# (plot-and-translate-bezier-points (partial place2) (fillet-about-point-2 dx2 dy2 20) (convert-to-curve-post post2)))
    (place1 (check-post post1))
   ; 
    (place1 (translate (wall-locate1 dx1 dy1)  post1))
    ;(place1 (translate (wall-locate2 dx1 dy1) post1))
  ;   (-# (plot-and-translate-bezier-points (partial place1)  (fillet-about-point-3 dx1 dy1 20)  (convert-to-curve-post post1)))
    ;(place1 (translate (wall-locate3 dx1 dy1)  post1))

   ; 
    (place2 (check-post post2))
    (plot-and-translate-bezier-points (partial place2) (fillet-about-point dx2 dy2 20) (convert-to-curve-post post2))
   ;  
    (place2 (translate (wall-locate1 dx2 dy2)  post2))
   ;(place2 (translate (wall-locate2 dx2 dy2) post2))

 ;(-# (plot-and-translate-bezier-points (partial place2)  (fillet-about-point-3 dx2 dy2 20)  (convert-to-curve-post post2)))  
 ;(place2 (translate (wall-locate3 dx2 dy2)  post1))
    )
;; (bottom-hull
;; ; (translate (transform-fn1 (wall-locate2 dx1 dy1))  (rotation-fn1 post1))
;;  ;(place1 (translate (wall-locate2 dx1 dy1) post1))
;; (plot-and-translate-bezier-points (partial place1)  (fillet-about-point-2 dx1 dy1 20)  (convert-to-curve-post post1))
;;  ;(plot-and-translate-bezier-points-with-rotation (partial place1)  (fillet-about-point-2 dx1 dy1 20)  (convert-to-curve-post post1) rotation-func1)
;;  ;(place1 (translate (wall-locate3 dx1 dy1) post1))
;;  ;(place2 (translate (wall-locate2 dx2 dy2) post2))
;;  (plot-and-translate-bezier-points (partial place2)  (fillet-about-point-2 dx2 dy2 20)  (convert-to-curve-post post2))
;;  ;(translate (transform-fn2 (wall-locate2 dx2 dy2))   (rotation-fn2 post2))
;;  ;(plot-and-translate-bezier-points-with-rotation (partial place2) (fillet-about-point-2 dx2 dy2 20) (convert-to-curve-post post2) rotation-func2)
;;  ;(place2 (translate (wall-locate3 dx2 dy2) post2))
;;  )    
(hull
; (translate (transform-fn1 (wall-locate2 dx1 dy1))  (rotation-fn1 post1))
  
 
lower-curve1 
lower-curve2
 ;   (plot-and-translate-bezier-points (partial place1)  (fillet-about-point-2 dx1 dy1 20)  (convert-to-curve-post post1) [0 0 (- bottom-pos1)])
 
 ;(place1 (translate (wall-locate3 dx1 dy1) post1))
  ;(place2 (translate (wall-locate2 dx2 dy2) (check-post post2)))
 
 ;   (translate [0 0 (-  bottom-pos2)](place2 (translate (wall-locate2 dx2 dy2) (check-post post2))))
 
 ;(translate (transform-fn2 (wall-locate2 dx2 dy2))   (rotation-fn2 post2))
    
  ;(plot-and-translate-bezier-points (partial place2)  (fillet-about-point-2 dx2 dy2 20)  (convert-to-curve-post post2) [0 0 (- bottom-pos2)])
 ;(place2 (translate (wall-locate3 dx2 dy2) post2))
 )
(hull
(place1 (translate (wall-locate2 dx1 dy1) post1))
 (translate [0 0 (- bottom-pos1)] (place1 (translate (wall-locate2 dx1 dy1) post1)))  
 last-curve-sphere1
 (translate [0 0 (- bottom-pos1)] last-curve-sphere1)

 last-curve-sphere2
(translate [0 0 (- bottom-pos2)] last-curve-sphere2)
 
(place2 (translate (wall-locate2 dx2 dy2) post2))

   (translate [0 0 (-  bottom-pos2)](place2 (translate (wall-locate2 dx2 dy2) post2)))

 

 )
   ))))

(defn wall-brace-half [place dx dy post]
  (plot-and-translate-bezier-points (partial place)  (fillet-about-point dx dy 20)  (convert-to-curve-post post))
  (place (check-post post))
  (place (translate (wall-locate1 dx dy)  post))
(place (translate (wall-locate2 dx dy) post))
  
  (bottom-hull
   (place (translate (wall-locate2 dx dy) post))
(plot-and-translate-bezier-points (partial place)  (fillet-about-point-2 dx dy 0)  (convert-to-curve-post post))
   )
  )


  

(defn wall-brace-xy 
  ( [place1 dx1 dy1 post1 place2 dx2 dy2 post2 xy1 xy2]
   (wall-brace-xy place1 dx1 dy1 post1 place2 dx2 dy2 post2 xy1 xy2 true))
  ( [place1 dx1 dy1 post1 place2 dx2 dy2 post2 xy1 xy2 bottom](union
   (hull
    
   (place1 (check-post post1))
    
    (place1 (translate (wall-locate1 dx1 dy1) post1))
    ;(place1 (translate (wall-locate2-xy dx1 dy1 xy1) post1)) 
    (plot-and-translate-bezier-points (partial place1)  (fillet-about-point-xy dx1 dy1 xy1 20)  (convert-to-curve-post post1))
    ;(place1 (translate (wall-locate3-xy dx1 dy1 xy1) post1))
    ;  )
    ;(for [i (range 0 1 0.1)
    ;      :let [dxv2 (* dx2 i)
    ;            dyv2 (* dy2 i)]]
    ;( union
    (place2 (check-post post2))
    (place2 (translate (wall-locate1 dx2 dy2) post2))
    ;(place2 (translate (wall-locate2-xy dx2 dy2 xy2) post2))
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
              (partial key-place x2 y2) dx2 dy2 post2
              (partial key-position x1 y1)
              (partial key-position x2 y2)
              (partial apply-key-geometry-rotation x1 y1) 
              (partial apply-key-geometry-rotation x2 y2)))

(defn key-wall-brace-xy [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2 xy1 xy2]
  (wall-brace-xy (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2 xy1 xy2))

(defn get-thumb-translate-fn [thumb-place]
  (partial transform-position thumb-place))

(defn get-thumb-rotate-fn [thumb-place]
  (cond
    (= (name thumb-place) "thumb-tl-place") (partial rotate-position thumb-tl-rotate)
    (= (name thumb-place) "thumb-tr-place") (partial rotate-position thumb-tr-rotate)
    (= (name thumb-place) "thumb-mr-place") (partial rotate-position thumb-mr-rotate)
    (= (name thumb-place) "thumb-bl-place") (partial rotate-position thumb-bl-rotate)
    (= (name thumb-place) "thumb-br-place") (partial rotate-position thumb-br-rotate)
    ))
(defn thumb-wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2 rotate-fn1 rotate-fn2]
  (let [translate-fn1 (get-thumb-translate-fn place1)
        translate-fn2 (get-thumb-translate-fn place2)]

    (wall-brace
     place1 dx1 dy1 post1
     place2 dx2 dy2 post2
     translate-fn1 translate-fn2
     rotate-fn1 rotate-fn2)))



(defn left-wall-plate-place-reverse [xdir ydir shape]
  (->> shape
       (translate (left-wall-plate-position xdir ydir))
       (rotate oled-mount-rotation-x-old [1 0 0])
       (rotate oled-mount-rotation-z-old [0 0 1])))

(def tps-65-z-position
 (cond
   (= nrows 5) 10  
  (= nrows 4 ) 10
 ))
  (def tps-65-z-rotation (+ 90 far-index-splay))
(def tps-65-x-rotation -20)
(def tps-65-y-rotation 0)

(defn tps-65-rotate ([shape] (tps-65-rotate  rdx rdy rdz shape))
  ([ rotate-x-fn rotate-y-fn rotate-z-fn shape] 
   (->> shape (rotate-z-fn tps-65-z-rotation) 
        (rotate-x-fn tps-65-x-rotation) 
        (rotate-y-fn tps-65-y-rotation)  
        (left-wall-plate-place-rotate  rotate-x-fn rotate-z-fn) 
        )))
 (defn tps-65-place ([shape] (tps-65-place translate rdx rdy rdz shape))
   ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape](->> shape
        (rotate-z-fn tps-65-z-rotation) 
        (rotate-x-fn tps-65-x-rotation)
        
        (rotate-y-fn tps-65-y-rotation)
       ;(rdy -7.5)
        (left-wall-plate-place 0 (+ left-wall-y-modifier -3) translate-fn rotate-x-fn rotate-z-fn)
        (translate-fn [0 2 tps-65-z-position]))))



(defn div-by-2 [num]
  (/ num 2))

(defn col-avg [col1 col2]
  (->>
   (map + col1 col2)
   (map div-by-2)
   )
  )






(defn tps-65-translate-and-place [ x y z shape]
  (->> shape 
       (translate [x y z])
       (tps-65-place)     
       )
  )
 
 (defn tps-65-translate-and-place-at-position ([position shape] (tps-65-translate-and-place-at-position position translate rdx rdy rdz shape))
   ([position translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape](->> shape
        (translate-fn position)
        (tps-65-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))



(defn tps-65-translate-and-place-with-radius 
  ([position radius-compensation-x radius-compensation-y shape]
   (tps-65-translate-and-place-with-radius position radius-compensation-x radius-compensation-y translate rdx rdy rdz shape))
  ([position radius-compensation-x radius-compensation-y translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
       (translate-fn position)
       ;plate-thickness
       (translate-fn [(tps-radius-compensation-adjust radius-compensation-x) (tps-radius-compensation-adjust radius-compensation-y) (- plate-thickness)])
       (tps-65-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn)
       )))

(defn tps-65-translate-and-place-with-radius-xyz 
  ([x y z radius-compensation-x radius-compensation-y shape]
  ( tps-65-translate-and-place-with-radius-xyz x y z radius-compensation-x radius-compensation-y translate rdx rdy rdz shape))
  ( [x y z radius-compensation-x radius-compensation-y translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
       (translate-fn [x y z])
       ;plate-thickness
       (translate-fn [(tps-radius-compensation-adjust radius-compensation-x) (tps-radius-compensation-adjust radius-compensation-y) (- plate-thickness)])
       (tps-65-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn)
       )))

(defn EVQWGD001-place ([shape] (EVQWGD001-place translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape](->> shape
       ;(rdz 90)
       ;(rdy -20)
      ;;  (translate [0 0 (- (+ EVQWGD001-plastic-height plate-thickness))])
      ;;  (rdx 80)
      ;;  (rdz -57)
      ;;  (rdy 25)
      ;; ;(rdz 20)
      ;;  ;(rdy 45)
      ;;  (translate [(- tps-65-mount-corner-radius (/ tps-65-width 2)) 0 0])
      ;;  ;(translate [(- tps-65-mount-corner-radius-with-offset) tps-65-mount-corner-radius-with-offset 0])
      ;;  ;(rdz -90)
      ;;  ;(rdy -90)
      ;;  (rdz (/ (+ tps-65-z-rotation 35) 2))
      ;;  ;(rdz tps-65-z-rotation)
      ;;  (rdx (/ (+ tps-65-x-rotation 6) 2))
      ;;  (rdy (/ (+ tps-65-y-rotation -32) 2))
      ;;  ;(rdz (/ (+ tps-65-z-rotation 35) 2))
      ;;  ;(rdx (/ (+ tps-65-x-rotation 6) 2))
      ;;  ;(rdy (/ (+ tps-65-y-rotation -32) 2))
      ;;  (translate (col-avg (left-wall-plate-position 0 (+ left-wall-y-modifier -3)) thumborigin))
      ;; (translate (col-avg br-minithumb-loc [0 0 tps-65-z-position]) )
      ;;  (translate [-10 10 2])
       (rotate-z-fn (- tps-65-z-rotation))
       (rotate-y-fn -70) 
       (rotate-x-fn 20)
       (tps-65-translate-and-place-with-radius [(- tps-65-corner-radius (/ tps-65-width 2)),
                                                0,
                                                0] (- tps-65-mount-corner-radius-with-offset)  0
                                               translate-fn rotate-x-fn rotate-y-fn rotate-z-fn 
                                               )
       (translate-fn [(- (/ EVQWGD001-mount-width 2)) (- (/ EVQWGD001-height 2)) (- EVQWGD001-mount-length) ])
       ))
  
  )

(defn EVQWGD001-translate-and-place ([x y z shape] (EVQWGD001-translate-and-place x y z translate rdx rdy rdz shape))
  ([x y z translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
       (translate-fn [x y z])
       (EVQWGD001-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))

(defn EVQWGD001-translate-and-place-at-position ([position shape] (EVQWGD001-translate-and-place-at-position position translate rdx rdy rdz shape))
  ( [position translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
       (translate-fn position)
       (EVQWGD001-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))

(defn screen-holder-rotate [shape]
  (->> shape
       (rdx (cond 
              (= nrows 5) -5
              (= nrows 4) -5))
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

(def screen-holder-rotate-side-y -80)
(defn screen-holder-rotate-side 
  ([shape] (screen-holder-rotate-side rdz rdy rdz shape))
  ([rotate-x-fn rotate-y-fn rotate-z-fn shape](->> shape
       (rotate-z-fn -90)
      (rotate-y-fn screen-holder-rotate-side-y)
      (rotate-x-fn -17.5) 
       (rotate-z-fn 20)
  ))
)
(defn screen-holder-place-side ([shape] (screen-holder-place-side translate rdx rdy rdz shape) )
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape](->> shape
       (screen-holder-rotate-side rotate-x-fn rotate-y-fn rotate-z-fn)
       (translate-fn [0 -20 8])
       ;(rdz 5)
       (left-wall-plate-place -2 -3 translate-fn rotate-x-fn rotate-z-fn)
       (translate-fn [0 0 (- 2.5 keyboard-z-offset 3.5)]))))

(defn screen-holder-translate-and-place-side ([x y z shape] (screen-holder-translate-and-place-side x y z translate rdx rdy rdz shape ))
  ([x y z translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape] (->> shape 
       (translate-fn [x y z])
       (screen-holder-place-side translate-fn rotate-x-fn rotate-y-fn rotate-z-fn)
       )))

(defn wall-corner-points [dx1 dy1 dxmid dymid dx2 dy2 translation-mod  dz]
  
         
         (bezier-quadratic
    [(* dx1 translation-mod)  (* dy1 translation-mod) dz]
    [(* dxmid translation-mod) (* dymid translation-mod) dz] 
    [(* dx2 translation-mod) (* dy2 translation-mod) dz]
   20
   ) )

(defn wall-corner-points-bottom [dx1 dy1 dxmid dymid dx2 dy2 translation-mod  dz]


  (bezier-quadratic
   (assoc [(* dx1 translation-mod)  (* dy1 translation-mod) dz] 2 (- oled-holder-thickness))
   (assoc [(* dxmid translation-mod) (* dymid translation-mod) dz] 2 (- oled-holder-thickness))
   (assoc [(* dx2 translation-mod) (* dy2 translation-mod) dz] 2 (- oled-holder-thickness))
   20))

;; (defn curved-corner ([dx1 dy1 dxmid dymid dx2 dy2 place post] (curved-corner dx1 dy1 dxmid dymid dx2 dy2 place post 20))
;;   ([dx1 dy1 dxmid dymid dx2 dy2 place post steps](let [
;;         f1pts1 (fillet-about-point dx1 dy1 20)
;;         f1ptsmid (fillet-about-point dxmid dymid 20)
;;         f1pts2 (fillet-about-point dx2 dy2 20)
;;         bezier-fn (fn [index]
;;                     (bezier-quadratic
;;                      (nth f1pts1 index)
;;                      (nth f1ptsmid index)
;;                      (nth f1pts2 index)
;;                      steps))]
;;     (union
;;      (hull
;;       (plot-and-translate-bezier-points place (wall-corner-points dx1 dy1 dxmid dymid dx2 dy2 wall-thickness  -1) post)
;;        (for [index (range 0 20)]
;;         (plot-and-translate-bezier-points place (bezier-fn index) (convert-to-curve-post post)))
;;       (place (check-post post)))
;;      (bottom-hull
;;       (plot-and-translate-bezier-points place (wall-corner-points dx1 dy1 dxmid dymid dx2 dy2 wall-xy-offset  wall-z-offset) post)

;;       (for [index (range 0 20)
;;             :let [pts1 (fillet-about-point-2 dx1 dy1 20)
;;                   ptsmid (fillet-about-point-2 dxmid dymid 20)
;;                   pts2 (fillet-about-point-2 dx2 dy2 20)]]
;;         (plot-and-translate-bezier-points place
;;                                           (bezier-quadratic
;;                                            (nth pts1 index)
;;                                            (nth ptsmid index)
;;                                            (nth pts2 index)
;;                                            steps)  (convert-to-curve-post post)
;;                                           )))))))

 (defn wall-corner-points-convex [dx1 dy1 dxmid dymid dx2 dy2 translation-mod join-point  dz]


   (bezier-quadratic
    [(* dx1 translation-mod)  (* dy1 translation-mod) dz]
    [(* dxmid translation-mod) (* dymid translation-mod) dz]
    [(* dx2 translation-mod) (* dy2 translation-mod) dz]
    20))

(defn curved-corner ([dx1 dy1 dxmid dymid dx2 dy2 place post] (curved-corner dx1 dy1 dxmid dymid dx2 dy2 place post 20 :radians))
  ([dx1 dy1 dxmid dymid dx2 dy2 place post steps rad-or-deg] (let [f1pts1 (fillet-about-point dx1 dy1 20)
                                                        f1ptsmid (fillet-about-point dxmid dymid 20)
                                                        f1pts2 (fillet-about-point dx2 dy2 20)
                                                        bezier-fn (fn [index]
                                                                    (bezier-quadratic
                                                                     (nth f1pts1 index)
                                                                     (nth f1ptsmid index)
                                                                     (nth f1pts2 index)
                                                                     steps))
                                                        upper-curves  (for [index (range 0 20)]
                                                                        (plot-and-translate-bezier-points place (bezier-fn index) (convert-to-curve-post post)))
                                                        lower-curves (for [index (range 0 20)
                                                                           :let [pts1 (fillet-about-point-2 dx1 dy1 20)
                                                                                 ptsmid (fillet-about-point-2 dxmid dymid 20)
                                                                                 pts2 (fillet-about-point-2 dx2 dy2 20)]]
                                                                       (plot-and-translate-bezier-points place
                                                                                                         (bezier-quadratic
                                                                                                          (nth pts1 index)
                                                                                                          (nth ptsmid index)
                                                                                                          (nth pts2 index)
                                                                                                          steps)  (convert-to-curve-post post)))
                                                        get-last-spheres (fn [curves]
                                                                           (map (fn [curve]
                                                                                  (last curve))
                                                                                curves))
                                                        lower-curves-last (last lower-curves)
                                                        transform (case rad-or-deg
                                                                    :radians (partial transform-position-radians)
                                                                    :degrees (partial transform-position)
                                                                   )
                                                        z-position (last (transform place [0 0 (- oled-holder-thickness wall-z-offset)]))
                                                        ]
                                                    (union
                                                     (hull
                                                      (plot-and-translate-bezier-points place (wall-corner-points dx1 dy1 dxmid dymid dx2 dy2 wall-thickness  -1) post)
                                                     upper-curves
                                                      (place  (check-post post)))
                                                     (hull
                                                      lower-curves
                                                      )
                                                     (hull
                                                      (-# lower-curves-last)
                                                      (translate [0 0 (- z-position)] lower-curves-last)
                                                      (plot-and-translate-bezier-points place (wall-corner-points dx1 dy1 dxmid dymid dx2 dy2 wall-xy-offset  wall-z-offset) post)
                                                      (plot-and-translate-bezier-points place (wall-corner-points-bottom dx1 dy1 dxmid dymid dx2 dy2 wall-xy-offset  wall-z-offset) post)
                                                      )))))
 
 (defn wall-corner-quadratic ([ dx1 dy1 place1 post-position-1 dxmid dymid place-mid  post-position-mid dx2 dy2 place2 post-position-2 translation-mod  dz] 
                              (wall-corner-quadratic dx1 dy1 place1 post-position-1 dxmid dymid place-mid  post-position-mid dx2 dy2 place2 post-position-2 translation-mod  dz 20))

   ([dx1 dy1 place1 post-position-1 dxmid dymid place-mid  post-position-mid dx2 dy2 place2 post-position-2 translation-mod  dz steps]
    (bezier-quadratic
    (place1 (mapv + post-position-1 [(* dx1 translation-mod)  (* dy1 translation-mod) dz]))
    (place-mid (mapv + post-position-mid [(* dxmid translation-mod) (* dymid translation-mod) dz]))
    (place2 (mapv + post-position-2 [(* dx2 translation-mod) (* dy2 translation-mod) dz]))
    steps)))
 
 (defn wall-corner-quadratic-bottom ([dx1 dy1 place1 post-position-1 dxmid dymid place-mid  post-position-mid dx2 dy2 place2 post-position-2 translation-mod  dz]
                              (wall-corner-quadratic-bottom dx1 dy1 place1 post-position-1 dxmid dymid place-mid  post-position-mid dx2 dy2 place2 post-position-2 translation-mod  dz 20))

   ([dx1 dy1 place1 post-position-1 dxmid dymid place-mid  post-position-mid dx2 dy2 place2 post-position-2 translation-mod  dz steps]
    (bezier-quadratic
     (assoc (vec (place1 (mapv + post-position-1 [(* dx1 translation-mod)  (* dy1 translation-mod) dz]))) 2 (- oled-holder-thickness))
     (assoc (vec (place-mid (mapv + post-position-mid [(* dxmid translation-mod) (* dymid translation-mod) dz]))) 2 (- oled-holder-thickness))
     (assoc (vec (place2 (mapv + post-position-2 [(* dx2 translation-mod) (* dy2 translation-mod) dz]))) 2 (- oled-holder-thickness))
     steps)))
 
 (defn wall-corner-cubic ([dx1 dy1 place1 post-position-1 dxmid1 dymid1 place-mid1 post-position-mid1 dxmid2 dymid2 place-mid2 post-position-mid2  dx2 dy2 place2 post-position-2 translation-mod  dz]
                          (wall-corner-cubic dx1 dy1 place1 post-position-1 dxmid1 dymid1 place-mid1 post-position-mid1 dxmid2 dymid2 place-mid2 post-position-mid2  dx2 dy2 place2 post-position-2 translation-mod  dz))
([dx1 dy1 place1 post-position-1 dxmid1 dymid1 place-mid1 post-position-mid1 dxmid2 dymid2 place-mid2 post-position-mid2  dx2 dy2 place2 post-position-2 translation-mod  dz steps]
   (bezier-cubic
    (place1 (mapv + post-position-1 [(* dx1 translation-mod)  (* dy1 translation-mod) dz]))
    (place-mid1 (mapv + post-position-mid1 [(* dxmid1 translation-mod) (* dymid1 translation-mod) dz]))
    (place-mid2 (mapv + post-position-mid2 [(* dxmid2 translation-mod) (* dymid2 translation-mod) dz]))
    (place2 (mapv + post-position-2 [(* dx2 translation-mod) (* dy2 translation-mod) dz]))
    steps)))
 
 (defn wall-corner-cubic-bottom ([dx1 dy1 place1 post-position-1 dxmid1 dymid1 place-mid1 post-position-mid1 dxmid2 dymid2 place-mid2 post-position-mid2  dx2 dy2 place2 post-position-2 translation-mod  dz]
                          (wall-corner-cubic-bottom dx1 dy1 place1 post-position-1 dxmid1 dymid1 place-mid1 post-position-mid1 dxmid2 dymid2 place-mid2 post-position-mid2  dx2 dy2 place2 post-position-2 translation-mod  dz))
   ([dx1 dy1 place1 post-position-1 dxmid1 dymid1 place-mid1 post-position-mid1 dxmid2 dymid2 place-mid2 post-position-mid2  dx2 dy2 place2 post-position-2 translation-mod  dz steps]
    (bezier-cubic
     (assoc (vec (place1 (mapv + post-position-1 [(* dx1 translation-mod)  (* dy1 translation-mod) dz]))) 2 (- oled-holder-thickness))
     (assoc (vec(place-mid1 (mapv + post-position-mid1 [(* dxmid1 translation-mod) (* dymid1 translation-mod) dz])))  2 (- oled-holder-thickness))
     (assoc (vec(place-mid2 (mapv + post-position-mid2 [(* dxmid2 translation-mod) (* dymid2 translation-mod) dz]))) 2 (- oled-holder-thickness))
     (assoc (vec(place2 (mapv + post-position-2 [(* dx2 translation-mod) (* dy2 translation-mod) dz])))  2 (- oled-holder-thickness))
     steps)))

  (defn wall-corner-quartic 
    [{:keys [dx1 dy1 place1 post-position-1 
             dxmid1 dymid1 place-mid1 post-position-mid1
             dxmid2 dymid2 place-mid2 post-position-mid2 
            dxmid3 dymid3 place-mid3 post-position-mid3  
             dx2 dy2 place2 post-position-2 translation-mod  
             dz steps] :or {steps 20}}]
     (bezier-quartic
      (place1 (mapv + post-position-1 [(* dx1 translation-mod)  (* dy1 translation-mod) dz]))
      (place-mid1 (mapv + post-position-mid1 [(* dxmid1 translation-mod) (* dymid1 translation-mod) dz]))
      (place-mid2 (mapv + post-position-mid2 [(* dxmid2 translation-mod) (* dymid2 translation-mod) dz]))
      (place-mid3 (mapv + post-position-mid3 [(* dxmid3 translation-mod) (* dymid3 translation-mod) dz]))
      (place2 (mapv + post-position-2 [(* dx2 translation-mod) (* dy2 translation-mod) dz]))
      steps))

(defn wall-corner-quartic-bottom [{:keys [dx1 dy1 place1 post-position-1 
                                          dxmid1 dymid1 place-mid1 post-position-mid1
                                          dxmid2 dymid2 place-mid2 post-position-mid2
                                          dxmid3 dymid3 place-mid3 post-position-mid3  
                                          dx2 dy2 place2 post-position-2 
                                          translation-mod  dz steps] :or {steps 20}}]
   (bezier-quartic
    (assoc (vec (place1 (mapv + post-position-1 [(* dx1 translation-mod)  (* dy1 translation-mod) dz]))) 2 (- oled-holder-thickness))
    (assoc (vec (place-mid1 (mapv + post-position-mid1 [(* dxmid1 translation-mod) (* dymid1 translation-mod) dz])))  2 (- oled-holder-thickness))
    (assoc (vec (place-mid2 (mapv + post-position-mid2 [(* dxmid2 translation-mod) (* dymid2 translation-mod) dz]))) 2 (- oled-holder-thickness))
    (assoc (vec (place-mid3 (mapv + post-position-mid3 [(* dxmid3 translation-mod) (* dymid3 translation-mod) dz]))) 2 (- oled-holder-thickness)) 
    (assoc (vec (place2 (mapv + post-position-2 [(* dx2 translation-mod) (* dy2 translation-mod) dz])))  2 (- oled-holder-thickness))
    steps))

 
;;  (defn curved-corner-3-points ([dx1 dy1 place1 post-position-1 dxmid dymid place-mid post-position-mid  dx2 dy2 place2 post-position-2 post] (curved-corner-3-points dx1 dy1 place1 post-position-1 dxmid dymid place-mid post-position-mid  dx2 dy2 place2 post-position-2 post 20))
;;    ([dx1 dy1 place1 post-position-1 dxmid dymid place-mid post-position-mid  dx2 dy2 place2 post-position-2 post steps] 
;;     (let 
;;      [f1pts1 (fillet-about-point dx1 dy1 20)
;;                                                          f1ptsmid (fillet-about-point dxmid dymid 20)
;;                                                          f1pts2 (fillet-about-point dx2 dy2 20)
;;                                                          bezier-fn (fn [index]
;;                                                                      (bezier-quadratic
;;                                                                     (place1 (map + post-position-1 (nth f1pts1 index)))
;;                                                                     (place-mid (map + post-position-mid     (nth f1ptsmid index)))
;;                                                                     (place2  (map + post-position-2 (nth f1pts2 index)))
;;                                                                       steps))]
;;                                                      (union
;;                                                       (union
;;                                                        (chained-hull (plot-bezier-points  (wall-corner-3-points dx1 dy1 place1 post-position-1 dxmid dymid place-mid post-position-mid  dx2 dy2 place2 post-position-2 wall-thickness  -1) post))
;;                                                         (for [index (range 0 20)]
;;                                                          (chained-hull (plot-bezier-points (bezier-fn index) (convert-to-curve-post post)))))
;;                                                        ;(translate (place1 post-position-1) (check-post post))
;;                                                        ;(translate (place2 post-position-2) (check-post post))
;;                                                        )
;;                                                       (union
;;                                                        (chained-hull-with-function (partial bottom-hull) (plot-bezier-points  (wall-corner-3-points dx1 dy1 place1 post-position-1 dxmid dymid place-mid post-position-mid  dx2 dy2 place2 post-position-2 wall-xy-offset  wall-z-offset) post))

;;                                                          (for [index (range 0 20)
;;                                                              :let [pts1 (fillet-about-point-2 dx1 dy1 20)
;;                                                                    ptsmid (fillet-about-point-2 dxmid dymid 20)
;;                                                                    pts2 (fillet-about-point-2 dx2 dy2 20)
;;                                                              ]]
;;                                                          (chained-hull-with-function (partial bottom-hull) (plot-bezier-points 
;;                                                                                            (bezier-quadratic
;;                                                                                             (place1(map + post-position-1  (nth pts1 index)))
;;                                                                                            (place-mid (map + post-position-mid   (nth ptsmid index)))
;;                                                                                            (place2 (map + post-position-2  (nth pts2 index)))
;;                                                                                             steps)  (convert-to-curve-post post))))))))
 
 
  ;; (defn curved-corner-quadratic ([dx1 dy1 place1 post-position-1 dxmid dymid place-mid post-position-mid  dx2 dy2 place2 post-position-2 post] (curved-corner-quadratic dx1 dy1 place1 post-position-1 dxmid dymid place-mid post-position-mid  dx2 dy2 place2 post-position-2 post 20))
  ;;   ([dx1 dy1 place1 post-position-1 dxmid dymid place-mid post-position-mid  dx2 dy2 place2 post-position-2 post steps]
  ;;    (let
  ;;     [place-position1 (partial transform-position place1)
  ;;      place-position-mid (partial transform-position place-mid)
  ;;      place-position2 (partial transform-position place2)
  ;;      f1pts1 (fillet-about-point dx1 dy1 20  )
  ;;      f1ptsmid (fillet-about-point dxmid dymid 20 )
  ;;      f1pts2 (fillet-about-point dx2 dy2 20 )
  ;;      bezier-fn (fn [index]
  ;;                  (bezier-quadratic
  ;;                   (place-position1  (mapv + (get-oled-corner-translation-vector post-position-1) (nth f1pts1 index)))
  ;;                   (place-position-mid    (mapv + (get-oled-corner-translation-vector post-position-mid)  (nth f1ptsmid index)))
  ;;                   (place-position2  (mapv + (get-oled-corner-translation-vector post-position-2) (nth f1pts2 index)))
  ;;                   steps))
  ;;      upper-curves (rearrange-nested-list 20 20 (for [index (range 0 20)] (bezier-fn index)))
  ;;      pts1 (fillet-about-point-2 dx1 dy1 20 )
  ;;      ptsmid (fillet-about-point-2 dxmid dymid 20 )
  ;;      pts2 (fillet-about-point-2 dx2 dy2 20 )
  ;;      bezier-fn2 (fn [index] (bezier-quadratic
  ;;                              (place-position1   (mapv + (get-oled-corner-translation-vector post-position-1) (nth pts1 index)))
  ;;                              (place-position-mid (mapv + (get-oled-corner-translation-vector post-position-mid)   (nth ptsmid index)))
  ;;                              (place-position2 (mapv + (get-oled-corner-translation-vector post-position-2) (nth pts2 index)))
  ;;                              steps))
  ;;      lower-curves(rearrange-nested-list steps steps (for [index (range 0 steps)] (bezier-fn2 index)))
  ;;      ]
  ;;      (union
        
  ;;       (chained-hull-for-three-lists
  ;;          (plot-bezier-points  (wall-corner-3-points dx1 dy1 place-position1 (get-oled-corner-translation-vector post-position-1) dxmid dymid  place-position-mid (get-oled-corner-translation-vector post-position-mid)  dx2 dy2  place-position2 (get-oled-corner-translation-vector post-position-2) wall-thickness  -1) post)
  ;;        (plot-bezier-points  (wall-corner-3-points dx1 dy1 place-position1 (get-web-corner-translation-vector post-position-1) dxmid dymid  place-position-mid (get-web-corner-translation-vector post-position-mid)  dx2 dy2  place-position2 (get-web-corner-translation-vector post-position-2) 1 0) web-post)
  ;;          ;(chained-hull 
  ;;          (for [index (range 0 steps)] (plot-bezier-points (nth upper-curves index) (convert-to-curve-post post)))
  ;;       ;(place1 (translate post-position-1 post))
  ;;       ;(place-mid (translate post-position-mid post))
  ;;       ;(place2 (translate post-position-mid post)) 
         
  ;;        steps
  ;;        )
  ;;                                                      ;(translate (place-position1 post-position-1) (check-post post))
  ;;                                                      ;(translate (place-position2 post-position-2) (check-post post))
        
  ;;      (chained-hull-for-two-lists
  ;;         (plot-bezier-points  (bottom-translate (wall-corner-3-points dx1 dy1 place-position1 (get-oled-corner-translation-vector post-position-1) dxmid dymid  place-position-mid (get-oled-corner-translation-vector post-position-mid)  dx2 dy2  place-position2 (get-oled-corner-translation-vector post-position-2) wall-xy-offset  wall-z-offset)) post)

  ;;        (for [index (range 0 steps)]
  ;;          (plot-bezier-points (bottom-translate (nth lower-curves index ))(convert-to-curve-post post))) 
  ;;       steps
  ;;       )
  ;;       )))
  ;; )
 
 (defn curved-corner-quadratic
   [{:keys [dx1 dy1 place1 rotate1 post-position-1
            dxmid1 dymid1 place-mid1 rotatemid1 post-position-mid1 
            dx2 dy2 place2 rotate2 post-position-2
            rotation-values1 rotation-values2 rotate-x-fn rotate-y-fn rotate-z-fn variable-rotation-fn post steps] :or {steps 20}}]
   (let
    [rotated-d1 (rotate1 [dx1 dy1 0])
     rotated-dmid1 (rotatemid1 [dxmid1 dymid1 0]) 
     rotated-d2 (rotate2 [dx2 dy2 0])

     rotated-dx1 (first rotated-d1)
     rotated-dxmid1 (first rotated-dmid1) 
     rotated-dx2 (first rotated-d2)
     rotated-dy1 (second rotated-d1)
     rotated-dymid1 (second rotated-dmid1) 
     rotated-dy2 (second rotated-d2)

     f1pts1 (fillet-about-point dx1 dy1 20)
     f1ptsmid1 (fillet-about-point dxmid1 dymid1 20) 
     f1pts2 (fillet-about-point dx2 dy2 20)
     bezier-fn (fn [index]
                 (bezier-quadratic
                  (place1  (mapv + (get-curve-corner-translation-vector post-position-1) (nth f1pts1 index)))
                  (place-mid1    (mapv + (get-curve-corner-translation-vector post-position-mid1)  (nth f1ptsmid1 index))) 
                  (place2  (mapv + (get-curve-corner-translation-vector post-position-2) (nth f1pts2 index)))
                  steps))
     upper-curves (rearrange-nested-list steps 20 (for [index (range 0 20)] (bezier-fn index)))
     pts1 (fillet-about-point-2 dx1 dy1 20)
     ptsmid1 (fillet-about-point-2 dxmid1 dymid1 20) 
     pts2 (fillet-about-point-2 dx2 dy2 20)
     bezier-fn2 (fn [index] (bezier-quadratic
                             (place1   (mapv + (get-curve-corner-translation-vector post-position-1) (nth pts1 index)))
                             (place-mid1 (mapv + (get-curve-corner-translation-vector post-position-mid1)   (nth ptsmid1 index))) 
                             (place2 (mapv + (get-curve-corner-translation-vector post-position-2) (nth pts2 index)))
                             steps))
     lower-curves (rearrange-nested-list steps 20 (for [index (range 0 20)] (bezier-fn2 index)))
     wall-locate-2-cubic (wall-corner-quadratic dx1 dy1 place1 (get-oled-corner-translation-vector post-position-1)
                                            dxmid1 dymid1  place-mid1 (get-oled-corner-translation-vector post-position-mid1) 
                                            dx2 dy2  place2 (get-oled-corner-translation-vector post-position-2) wall-xy-offset  wall-z-offset steps)
     wall-locate-2-cubic-bottom (wall-corner-quadratic-bottom dx1 dy1 place1 (get-oled-corner-translation-vector post-position-1)
                                                          dxmid1 dymid1  place-mid1 (get-oled-corner-translation-vector post-position-mid1) 
                                                          dx2 dy2  place2 (get-oled-corner-translation-vector post-position-2) wall-xy-offset  (/ oled-holder-thickness 2) steps)
     plot-points-with-rotation (fn [bezier-points shape] (plot-bezier-points-variable-rotation bezier-points rotation-values1 rotation-values2 rotate-x-fn rotate-y-fn rotate-z-fn variable-rotation-fn shape))]
     (union

      (chained-hull-for-two-lists
       ;(plot-bezier-points  (wall-corner-cubic dx1 dy1 place1 (get-oled-corner-translation-vector post-position-1) 
       ;                                        dxmid1 dymid1  place-mid1 (get-oled-corner-translation-vector post-position-mid1) 
       ;                                        dx1mid2 dymid2  place-mid2 (get-oled-corner-translation-vector post-position-mid2)  
       ;                                        dx2 dy2  place2 (get-oled-corner-translation-vector post-position-2) 
       ;                                        wall-thickness  -1 steps)
       ;                                        post)

       (plot-points-with-rotation   (wall-corner-quadratic dx1 dy1 place1 (get-web-corner-translation-vector post-position-1)
                                                       dxmid1 dymid1  place-mid1 (get-web-corner-translation-vector post-position-mid1) 
                                                       dx2 dy2  place2 (get-web-corner-translation-vector post-position-2)
                                                       0 0 steps)
                                    web-post)
           ;(chained-hull 
       (for [index (range 0 steps)] (plot-points-with-rotation  (nth upper-curves index) (convert-to-curve-post post)))
        ;(place1 (translate post-position-1 post))
        ;(place-mid (translate post-position-mid post))
        ;(place2 (translate post-position-mid post)) 

       steps)
                                                       ;(translate (place1 post-position-1) (check-post post))
                                                       ;(translate (place2 post-position-2) (check-post post))

      (chained-hull-for-four-lists
       (plot-points-with-rotation   wall-locate-2-cubic
                                    post)
       (plot-points-with-rotation  wall-locate-2-cubic-bottom
                                   post)

       (for [index (range 0 steps)]
         (plot-points-with-rotation (nth  lower-curves index) (convert-to-curve-post post)))
       (for [index (range 0 steps)]
         (plot-points-with-rotation
          (nth (bottom-translate (vec lower-curves)) index)
          (convert-to-curve-post post)))
       steps))))
 
 (defn curved-corner-cubic 
   [{:keys [dx1 dy1 place1 rotate1 post-position-1 
            dxmid1 dymid1 place-mid1 rotatemid1 post-position-mid1 
            dxmid2 dymid2 place-mid2 rotatemid2 post-position-mid2  
            dx2 dy2 place2 rotate2 post-position-2 
            rotation-values1 rotation-values2 rotate-x-fn rotate-y-fn rotate-z-fn variable-rotation-fn post steps] :or {steps 20} }] 
    (let
     [rotated-d1 (rotate1 [dx1 dy1 0])
      rotated-dmid1 (rotatemid1 [dxmid1 dymid1 0])
      rotated-dmid2 (rotatemid2 [dxmid2 dymid2 0])
      rotated-d2 (rotate2 [dx2 dy2 0])
      
      rotated-dx1 (first rotated-d1)
      rotated-dxmid1 (first rotated-dmid1)
      rotated-dx1mid2 (first rotated-dmid2)
      rotated-dx2 (first rotated-d2)
      rotated-dy1 (second rotated-d1)
      rotated-dymid1 (second rotated-dmid1)
      rotated-dymid2 (second rotated-dmid2)
      rotated-dy2 (second rotated-d2)
     
      f1pts1 (fillet-about-point dx1 dy1 20)
      f1ptsmid1 (fillet-about-point dxmid1 dymid1 20)
      f1ptsmid2 (fillet-about-point dxmid2 dymid2 20)
      f1pts2 (fillet-about-point dx2 dy2 20)
      bezier-fn (fn [index]
                  (bezier-cubic
                   (place1  (mapv + (get-curve-corner-translation-vector post-position-1) (nth f1pts1 index)))
                   (place-mid1    (mapv + (get-curve-corner-translation-vector post-position-mid1)  (nth f1ptsmid1 index)))
                   (place-mid2    (mapv + (get-curve-corner-translation-vector post-position-mid2)  (nth f1ptsmid2 index)))
                   (place2  (mapv + (get-curve-corner-translation-vector post-position-2) (nth f1pts2 index)))
                   steps))
      upper-curves (rearrange-nested-list steps 20 (for [index (range 0 20)] (bezier-fn index)))
      pts1 (fillet-about-point-2 dx1 dy1 20)
      ptsmid1 (fillet-about-point-2 dxmid1 dymid1 20)
      ptsmid2 (fillet-about-point-2 dxmid2 dymid2 20)
      pts2 (fillet-about-point-2 dx2 dy2 20)
      bezier-fn2 (fn [index] (bezier-cubic
                              (place1   (mapv + (get-curve-corner-translation-vector post-position-1) (nth pts1 index)))
                              (place-mid1 (mapv + (get-curve-corner-translation-vector post-position-mid1)   (nth ptsmid1 index)))
                              (place-mid2 (mapv + (get-curve-corner-translation-vector post-position-mid2)   (nth ptsmid2 index)))
                              (place2 (mapv + (get-curve-corner-translation-vector post-position-2) (nth pts2 index)))
                              steps))
      lower-curves (rearrange-nested-list steps 20 (for [index (range 0 20)] (bezier-fn2 index)))
      wall-locate-2-cubic (wall-corner-cubic dx1 dy1 place1 (get-oled-corner-translation-vector post-position-1)
                                             dxmid1 dymid1  place-mid1 (get-oled-corner-translation-vector post-position-mid1)
                                             dxmid2 dymid2  place-mid2 (get-oled-corner-translation-vector post-position-mid2)
                                             dx2 dy2  place2 (get-oled-corner-translation-vector post-position-2) wall-xy-offset  wall-z-offset steps)
      wall-locate-2-cubic-bottom (wall-corner-cubic-bottom dx1 dy1 place1 (get-oled-corner-translation-vector post-position-1)
                                                           dxmid1 dymid1  place-mid1 (get-oled-corner-translation-vector post-position-mid1)
                                                           dxmid2 dymid2  place-mid2 (get-oled-corner-translation-vector post-position-mid2)
                                                           dx2 dy2  place2 (get-oled-corner-translation-vector post-position-2) wall-xy-offset  (/ oled-holder-thickness 2) steps)
      plot-points-with-rotation (fn [bezier-points shape] (plot-bezier-points-variable-rotation bezier-points rotation-values1 rotation-values2 rotate-x-fn rotate-y-fn rotate-z-fn variable-rotation-fn shape))
      ]
      (union

       (chained-hull-for-two-lists
       ;(plot-bezier-points  (wall-corner-cubic dx1 dy1 place1 (get-oled-corner-translation-vector post-position-1) 
       ;                                        dxmid1 dymid1  place-mid1 (get-oled-corner-translation-vector post-position-mid1) 
       ;                                        dx1mid2 dymid2  place-mid2 (get-oled-corner-translation-vector post-position-mid2)  
       ;                                        dx2 dy2  place2 (get-oled-corner-translation-vector post-position-2) 
       ;                                        wall-thickness  -1 steps)
       ;                                        post)
        
        (plot-points-with-rotation   (wall-corner-cubic dx1 dy1 place1 (get-web-corner-translation-vector post-position-1) 
                                                dxmid1 dymid1  place-mid1 (get-web-corner-translation-vector post-position-mid1) 
                                                dxmid2 dymid2  place-mid2 (get-web-corner-translation-vector post-position-mid2) 
                                                dx2 dy2  place2 (get-web-corner-translation-vector post-position-2) 
                                                0 0 steps)
                             web-post)
           ;(chained-hull 
        (for [index (range 0 steps)] (plot-points-with-rotation  (nth upper-curves index) (convert-to-curve-post post)))
        ;(place1 (translate post-position-1 post))
        ;(place-mid (translate post-position-mid post))
        ;(place2 (translate post-position-mid post)) 

        steps)
                                                       ;(translate (place1 post-position-1) (check-post post))
                                                       ;(translate (place2 post-position-2) (check-post post))

       (chained-hull-for-four-lists 
        (plot-points-with-rotation   wall-locate-2-cubic
                                               post)
       (plot-points-with-rotation  wall-locate-2-cubic-bottom 
                            post)

        (for [index (range 0 steps)]
          (plot-points-with-rotation (nth  lower-curves index) (convert-to-curve-post post)) 
          )
       (for [index (range 0 steps)]
         (plot-points-with-rotation 
          (nth (bottom-translate (vec lower-curves)) index)
          (convert-to-curve-post post))
         )
        steps
        ))))
 
 (defn curved-corner-quartic
   [{:keys [target1-dx target1-dy target1-place target1-post target1-rad-or-deg
            dx1 dy1 place1 rotate1 post-position-1
            dxmid1 dymid1 place-mid1 rotatemid1 post-position-mid1
            dxmid2 dymid2 place-mid2 rotatemid2 post-position-mid2
            dxmid3 dymid3 place-mid3 post-position-mid3
            dx2 dy2 place2 rotate2 post-position-2
            target2-dx target2-dy target2-place target2-post target2-rad-or-deg
            rotation-values1 rotation-values2 rotate-x-fn rotate-y-fn rotate-z-fn variable-rotation-fn post steps] :or {steps 20}}]
   (let
    [rotated-d1 (rotate1 [dx1 dy1 0])
     rotated-dmid1 (rotatemid1 [dxmid1 dymid1 0])
     rotated-dmid2 (rotatemid2 [dxmid2 dymid2 0])
     rotated-d2 (rotate2 [dx2 dy2 0])
     place1-bottom-z (last (place1 [0 0 (- oled-holder-thickness wall-z-offset)]))
     place2-bottom-z (last (place2 [0 0 (- oled-holder-thickness wall-z-offset)]))
     bottom-z (if (> place1-bottom-z place2-bottom-z)  place1-bottom-z  place2-bottom-z)
     target1-bottom-z (last (case target1-rad-or-deg
                              :radians (transform-position-radians target1-place [0 0 (- oled-holder-thickness wall-z-offset)])
                              :degrees (transform-position  target1-place [0 0 (- oled-holder-thickness wall-z-offset)])))
     target2-bottom-z (last (case target2-rad-or-deg
                              :radians (transform-position-radians target2-place [0 0 (- oled-holder-thickness wall-z-offset)])
                              :degrees (transform-position  target2-place [0 0 (- oled-holder-thickness wall-z-offset)])))

     rotated-dx1 (first rotated-d1)
     rotated-dxmid1 (first rotated-dmid1)
     rotated-dx1mid2 (first rotated-dmid2)
     rotated-dx2 (first rotated-d2)
     rotated-dy1 (second rotated-d1)
     rotated-dymid1 (second rotated-dmid1)
     rotated-dymid2 (second rotated-dmid2)
     rotated-dy2 (second rotated-d2)
     plot-points-with-rotation (fn [bezier-points shape] (plot-bezier-points-variable-rotation bezier-points rotation-values1 rotation-values2 rotate-x-fn rotate-y-fn rotate-z-fn variable-rotation-fn shape))
     f1pts1 (fillet-about-point dx1 dy1 20)
     f1ptsmid1 (fillet-about-point dxmid1 dymid1 20)
     f1ptsmid2 (fillet-about-point dxmid2 dymid2 20)
     f1ptsmid3 (fillet-about-point dxmid3 dymid3 20)
     f1pts2 (fillet-about-point dx2 dy2 20)
     bezier-fn (fn [index]
                 (bezier-quartic
                  (place1  (mapv + (get-curve-corner-translation-vector post-position-1) (nth f1pts1 index)))
                  (place-mid1    (mapv + (get-curve-corner-translation-vector post-position-mid1)  (nth f1ptsmid1 index)))
                  (place-mid2    (mapv + (get-curve-corner-translation-vector post-position-mid2)  (nth f1ptsmid2 index)))
                  (place-mid3    (mapv + (get-curve-corner-translation-vector post-position-mid3)  (nth f1ptsmid3 index)))
                  (place2  (mapv + (get-curve-corner-translation-vector post-position-2) (nth f1pts2 index)))
                  steps))
     upper-curves (rearrange-nested-list steps 20 (for [index (range 0 20)] (bezier-fn index)))
     pts1 (fillet-about-point-2 dx1 dy1 20)
     ptsmid1 (fillet-about-point-2 dxmid1 dymid1 20)
     ptsmid2 (fillet-about-point-2 dxmid2 dymid2 20)
     ptsmid3 (fillet-about-point-2 dxmid3 dymid3 20)
     pts2 (fillet-about-point-2 dx2 dy2 20)
     bezier-fn2 (fn [index] (bezier-quartic
                             (place1   (mapv + (get-curve-corner-translation-vector post-position-1) (nth pts1 index)))
                             (place-mid1 (mapv + (get-curve-corner-translation-vector post-position-mid1)   (nth ptsmid1 index)))
                             (place-mid2 (mapv + (get-curve-corner-translation-vector post-position-mid2)   (nth ptsmid2 index)))
                             (place-mid3 (mapv + (get-curve-corner-translation-vector post-position-mid3)   (nth ptsmid3 index)))
                             (place2 (mapv + (get-curve-corner-translation-vector post-position-2) (nth pts2 index)))
                             steps))
     lower-curves-unrearranged (for [index (range 0 20)] (bezier-fn2 index))
     lower-curves-unrearranged-last-spheres (last lower-curves-unrearranged)
     lower-curves (rearrange-nested-list steps 20 lower-curves-unrearranged )
     lower-curves-plotted (for [index (range 0 steps)]
                            (plot-points-with-rotation (nth  lower-curves index) (convert-to-curve-post post)))
     lower-curves-plotted-last (map (fn [points] (last points) ) lower-curves-plotted)
     target1-lower-curve (plot-and-translate-bezier-points target1-place (fillet-about-point-2 target1-dx target1-dy 20) (convert-to-curve-post target1-post)) 
     target1-lower-curve-last-sphere (last target1-lower-curve)
     target2-lower-curve (plot-and-translate-bezier-points target2-place (fillet-about-point-2 target2-dx target2-dy 20) (convert-to-curve-post target2-post))
     target2-lower-curve-last-sphere (last target2-lower-curve)
     wall-locate-2-quartic (wall-corner-quartic {:dx1 dx1 :dy1 dy1 :place1 place1 :post-position-1 (get-oled-corner-translation-vector post-position-1)
                                                 :dxmid1 dxmid1 :dymid1 dymid1  :place-mid1 place-mid1 :post-position-mid1 (get-oled-corner-translation-vector post-position-mid1)
                                                 :dxmid2 dxmid2 :dymid2 dymid2  :place-mid2 place-mid2 :post-position-mid2 (get-oled-corner-translation-vector post-position-mid2)
                                                 :dxmid3 dxmid3 :dymid3 dymid3  :place-mid3 place-mid3 :post-position-mid3 (get-oled-corner-translation-vector post-position-mid3)
                                                 :dx2 dx2 :dy2 dy2  :place2 place2 :post-position-2 (get-oled-corner-translation-vector post-position-2)
                                                 :translation-mod wall-xy-offset :dz wall-z-offset :steps steps})
     wall-locate-2-quartic-bottom (wall-corner-quartic-bottom {:dx1 dx1 :dy1 dy1 :place1 place1 :post-position-1 (get-oled-corner-translation-vector post-position-1)
                                                               :dxmid1 dxmid1 :dymid1 dymid1  :place-mid1 place-mid1 :post-position-mid1 (get-oled-corner-translation-vector post-position-mid1)
                                                               :dxmid2 dxmid2 :dymid2 dymid2  :place-mid2 place-mid2 :post-position-mid2 (get-oled-corner-translation-vector post-position-mid2)
                                                               :dxmid3 dxmid3 :dymid3 dymid3  :place-mid3 place-mid3 :post-position-mid3 (get-oled-corner-translation-vector post-position-mid3)
                                                               :dx2 dx2 :dy2 dy2  :place2 place2 :post-position-2 (get-oled-corner-translation-vector post-position-2)
                                                               :translation-mod wall-xy-offset  :dz wall-z-offset :steps steps})
     
     tops-and-tails (fn [list-to-be-modified to-prepend to-append]
                      (concat (conj list-to-be-modified to-prepend) (list to-append)))] 
     (union

       (chained-hull-for-two-lists
       ;(plot-bezier-points  (wall-corner-cubic dx1 dy1 place1 (get-oled-corner-translation-vector post-position-1) 
       ;                                        dxmid1 dymid1  place-mid1 (get-oled-corner-translation-vector post-position-mid1) 
       ;                                        dx1mid2 dymid2  place-mid2 (get-oled-corner-translation-vector post-position-mid2)  
       ;                                        dx2 dy2  place2 (get-oled-corner-translation-vector post-position-2) 
       ;                                        wall-thickness  -1 steps)
       ;                                        post)get-web-corner-translation-vector

       (tops-and-tails 
        (plot-points-with-rotation   (wall-corner-quartic  {:dx1 dx1 :dy1 dy1 :place1 place1 :post-position-1 (get-web-corner-translation-vector post-position-1)
:dxmid1 dxmid1 :dymid1 dymid1  :place-mid1 place-mid1 :post-position-mid1 (get-web-corner-translation-vector post-position-mid1)
:dxmid2 dxmid2 :dymid2 dymid2  :place-mid2 place-mid2 :post-position-mid2 (get-web-corner-translation-vector post-position-mid2)
:dxmid3 dxmid3 :dymid3 dymid3  :place-mid3 place-mid3 :post-position-mid3 (get-web-corner-translation-vector post-position-mid3)
:dx2 dx2 :dy2 dy2  :place2 place2 :post-position-2 (get-web-corner-translation-vector post-position-2)
                                                       :translation-mod 0 :dz 0 :steps steps})
                                    web-post)
        (target1-place (check-post target1-post))
        (target2-place (check-post target2-post))
        )
           ;(chained-hull 
       (tops-and-tails
             (for [index (range 0 steps)] (plot-points-with-rotation  (nth upper-curves index) (convert-to-curve-post post)))
        (plot-and-translate-bezier-points target1-place  (fillet-about-point target1-dx target1-dy 20)  (convert-to-curve-post target1-post))
        (plot-and-translate-bezier-points target2-place  (fillet-about-point target2-dx target2-dy 20)  (convert-to-curve-post target2-post))
        )
        ;(place1 (translate post-position-1 post))
        ;(place-mid (translate post-position-mid post))
        ;(place2 (translate post-position-mid post)) 

       (+ steps 2))
                                                       ;(translate (place1 post-position-1) (check-post post))
                                                       ;(translate (place2 post-position-2) (check-post post))
     (chained-hull
      (tops-and-tails (for [index (range 0 steps)]
                        (plot-points-with-rotation (nth  lower-curves index) (convert-to-curve-post post)))
                      target1-lower-curve
                      target2-lower-curve)
      )
      (chained-hull-for-four-lists
       (tops-and-tails (plot-points-with-rotation   wall-locate-2-quartic
                                    post)
             (target1-place (translate (wall-locate2 target1-dx target1-dy) target1-post))
             (target2-place (translate (wall-locate2 target2-dx target2-dy) target2-post))
             )
       (tops-and-tails (plot-points-with-rotation   wall-locate-2-quartic-bottom
                                                    post)
                       (translate [0 0 (- target1-bottom-z)](target1-place (translate (wall-locate2 target1-dx target1-dy) target1-post)))
                       (translate [0 0 (- target2-bottom-z)] (target2-place (translate (wall-locate2 target2-dx target2-dy) target2-post))))
       

       (tops-and-tails
         lower-curves-plotted-last
        target1-lower-curve-last-sphere
        target2-lower-curve-last-sphere
        )
       (tops-and-tails
         (map (fn [point] (translate [0 0 (- bottom-z)] point)) lower-curves-plotted-last)
        (translate [0 0 (- target1-bottom-z )]target1-lower-curve-last-sphere)
        (translate [0 0 (- target2-bottom-z)] target2-lower-curve-last-sphere))
       (+ steps 2)
       )
        ;; (-# (translate [0 0 (- target1-bottom-z)](target1-place (translate (wall-locate2 target1-dx target1-dy) target1-post))))
        ;; (-# (plot-points-with-rotation   wall-locate-2-quartic-bottom
        ;;                                              post))
       )))

;;  (defn curved-corner-cubic-polyhedron [{:keys [dx1 dy1 place1 rotate1 post-position-1 dxmid1 dymid1 place-mid1 rotatemid1 post-position-mid1 dxmid2 dymid2 place-mid2 rotatemid2 post-position-mid2  dx2 dy2 place2 rotate2 post-position-2 post steps] :or {steps 20}}]
;;    (let
;;      [fillet-point-count 20
;;       rotated-d1 (rotate1 [dx1 dy1 0])
;;       rotated-dmid1 (rotatemid1 [dxmid1 dymid1 0])
;;       rotated-dmid2 (rotatemid2 [dxmid2 dymid2 0])
;;       rotated-d2 (rotate2 [dx2 dy2 0])

;;       rotated-dx1 (first rotated-d1)
;;       rotated-dxmid1 (first rotated-dmid1)
;;       rotated-dx1mid2 (first rotated-dmid2)
;;       rotated-dx2 (first rotated-d2)
;;       rotated-dy1 (second rotated-d1)
;;       rotated-dymid1 (second rotated-dmid1)
;;       rotated-dymid2 (second rotated-dmid2)
;;       rotated-dy2 (second rotated-d2)
;;       place-position1 (partial transform-position place1)
;;       place-position-mid1 (partial transform-position place-mid1)
;;       place-position-mid2 (partial transform-position place-mid2)
;;       place-position2 (partial transform-position place2)
;;       f1pts1 (fillet-about-point dx1 dy1 20)
;;       f1ptsmid1 (fillet-about-point dxmid1 dymid1 20)
;;       f1ptsmid2 (fillet-about-point dxmid2 dymid2 20)
;;       f1pts2 (fillet-about-point dx2 dy2 20)
;;       bezier-fn (fn [index]
;;                   (bezier-cubic
;;                    (place-position1  (map + (get-curve-corner-translation-vector post-position-1) (nth f1pts1 index)))
;;                    (place-position-mid1    (map + (get-curve-corner-translation-vector post-position-mid1)  (nth f1ptsmid1 index)))
;;                    (place-position-mid2    (map + (get-curve-corner-translation-vector post-position-mid2)  (nth f1ptsmid2 index)))
;;                    (place-position2  (map + (get-curve-corner-translation-vector post-position-2) (nth f1pts2 index)))
;;                    steps))
;;       upper-curves (rearrange-nested-list steps 20 (for [index (range 0 20)] (bezier-fn index)))
;;       pts1 (fillet-about-point-2 dx1 dy1 20)
;;       ptsmid1 (fillet-about-point-2 dxmid1 dymid1 20)
;;       ptsmid2 (fillet-about-point-2 dxmid2 dymid2 20)
;;       pts2 (fillet-about-point-2 dx2 rotated-dy2 20)
;;       bezier-fn2 (fn [index] (bezier-cubic
;;                               (place-position1   (map + (get-curve-corner-translation-vector post-position-1) (nth pts1 index)))
;;                               (place-position-mid1 (map + (get-curve-corner-translation-vector post-position-mid1)   (nth ptsmid1 index)))
;;                               (place-position-mid2 (map + (get-curve-corner-translation-vector post-position-mid2)   (nth ptsmid2 index)))
;;                               (place-position2 (map + (get-curve-corner-translation-vector post-position-2) (nth pts2 index)))
;;                               steps))
;;       lower-curves (rearrange-nested-list steps 20 (for [index (range 0 20)] (bezier-fn2 index)))
;;       upper-curves-flat-list (flatten upper-curves)
;;       ]
      
;;       (polyhedron [upper-curves-flat-list], [(for [i (range 0 (- 1(count upper-curves))) 
;;                                                    k (range 0 (- fillet-point-count 1))
;;                                                    :let] 
;;     z                                  (nth upper-curves-flat-list (* i 20))
;;                                       )])
;;       )
;;    )
 
;;  (defn curved-corner-cubic ([dx1 dy1 place1 post-position-1 dxmid1 dymid1 place-mid1 post-position-mid1 dxmid2 dymid2 place-mid2 post-position-mid2  dx2 dy2 place2 post-position-2 post] (curved-corner-cubic dx1 dy1 place1 post-position-1 dxmid1 dymid1 place-mid1 post-position-mid1 dxmid2 dymid2 place-mid2 post-position-mid2  dx2 dy2 place2 post-position-2 post 20))
;;    ([dx1 dy1 place1 post-position-1 dxmid1 dymid1 place-mid1 post-position-mid1 dxmid2 dymid2 place-mid2 post-position-mid2  dx2 dy2 place2 post-position-2 post steps]
;;     (let
;;      [place-position1 (partial transform-position place1)
;;       place-position-mid1 (partial transform-position place-mid1)
;;       place-position-mid2 (partial transform-position place-mid2)
;;       place-position2 (partial transform-position place2) 
;;       f1pts1 (fillet-about-point dx1 dy1 20)
;;       f1ptsmid1 (fillet-about-point dxmid1 dymid1 20)
;;       f1ptsmid2 (fillet-about-point dxmid2 dymid2 20)
;;       f1pts2 (fillet-about-point dx2 dy2 20)
;;       bezier-fn (fn [index]
;;                   (bezier-cubic
;;                    (place-position1 (map + post-position-1 (nth f1pts1 index)))
;;                    (place-position-mid1 (map + post-position-mid1     (nth f1ptsmid1 index)))
;;                    (place-position-mid2 (map + post-position-mid2     (nth f1ptsmid2 index)))
;;                    (place-position2  (map + post-position-2 (nth f1pts2 index)))
;;                    steps))]
;;       (union
;;        (union
;;         ;(plot-bezier-points  (wall-corner-cubic dx1 dy1 place-position1 post-position-1 dxmid1 dymid1 place-position-mid1 post-position-mid1 dxmid2 dymid2 place-position-mid2 post-position-mid2  dx2 dy2 place-position2 post-position-2 wall-thickness  -1) post)
;;         (for [index (range 0 20)]
;;           (plot-bezier-points (bezier-fn index) (convert-to-curve-post post))) 
;;         ;steps
;;         )
;;                                                        ;(translate (place-position1 post-position-1) (check-post post))
;;                                                        ;(translate (place-position2 post-position-2) (check-post post))

;;        (union
;;         (plot-bezier-points  (wall-corner-cubic dx1 dy1 place-position1 post-position-1 dxmid1 dymid1 place-position-mid1 post-position-mid1 dxmid2 dymid2 place-position-mid2 post-position-mid2  dx2 dy2 place-position2 post-position-2 wall-xy-offset  wall-z-offset) post)

;;         (for [index (range 0 20)
;;               :let [pts1 (fillet-about-point-2 dx1 dy1 20)
;;                     ptsmid1 (fillet-about-point-2 dxmid1 dymid1 20)
;;                     ptsmid2 (fillet-about-point-2 dxmid2 dymid2 20)
;;                     pts2 (fillet-about-point-2 dx2 dy2 20)]]
;;           (plot-bezier-points
;;            (bezier-cubic
;;             (place-position1 (map + post-position-1  (nth pts1 index)))
;;             (place-position-mid1 (map + post-position-mid1   (nth ptsmid1 index)))
;;             (place-position-mid1 (map + post-position-mid1   (nth ptsmid2 index)))
;;             (place-position2 (map + post-position-2  (nth pts2 index)))
;;             steps)  (convert-to-curve-post post))) 
;;        ; steps
;;         )
;;        ))))
 
 (defn curved-corner-xy ([dx1 dy1 dxmid dymid dx2 dy2 place post xy] (curved-corner-xy dx1 dy1 dxmid dymid dx2 dy2 place post xy 20))
   ([dx1 dy1 dxmid dymid dx2 dy2 place post xy steps] (let [f1pts1 (fillet-about-point-xy dx1 dy1 xy 20)
                                                         f1ptsmid (fillet-about-point-xy dxmid dymid xy 20)
                                                         f1pts2 (fillet-about-point-xy dx2 dy2 xy 20)
                                                         bezier-fn (fn [index]
                                                                     (bezier-quadratic
                                                                      (nth f1pts1 index)
                                                                      (nth f1ptsmid index)
                                                                      (nth f1pts2 index)
                                                                      steps))]
                                                     (union
                                                      (hull
                                                       (plot-and-translate-bezier-points place (wall-corner-points dx1 dy1 dxmid dymid dx2 dy2 wall-thickness  -1) post)
                                                       (for [index (range 0 20)]
                                                         (plot-and-translate-bezier-points place (bezier-fn index) (convert-to-curve-post post)))
                                                       (place (check-post post)))
                                                      (bottom-hull
                                                       (plot-and-translate-bezier-points place (wall-corner-points dx1 dy1 dxmid dymid dx2 dy2 xy  wall-z-offset) post)

                                                       (for [index (range 0 20)
                                                             :let [pts1 (fillet-about-point-xy-2 dx1 dy1 xy 20)
                                                                   ptsmid (fillet-about-point-xy-2 dxmid dymid xy 20)
                                                                   pts2 (fillet-about-point-xy-2 dx2 dy2 xy 20)]]
                                                         (plot-and-translate-bezier-points place
                                                                                           (bezier-quadratic
                                                                                            (nth pts1 index)
                                                                                            (nth ptsmid index)
                                                                                            (nth pts2 index)
                                                                                            steps)  (convert-to-curve-post post))))))))
 (def top-right-corner 
   (curved-corner 0 1 1 1 1 0 (partial key-place lastcol 0) oled-post-tr)

   )

 (def bottom-right-corner
   (curved-corner 0 -1 1 -1 1 0 (partial key-place lastcol cornerrow) oled-post-br)
   )

(def right-wall
  (let [tr (if (true? pinky-15u) wide-post-tr oled-post-tr)
        br (if (true? pinky-15u) wide-post-br oled-post-br)]
    (union 
     ;(key-wall-brace lastcol 0 0 1 tr lastcol 0 1 0 tr)
top-right-corner      
     (for [y (range 0 lastrow)] (key-wall-brace lastcol y 1 0 tr lastcol y 1 0 br))
           (for [y (range 1 lastrow)] (key-wall-brace lastcol (dec y) 1 0 br lastcol y 1 0 tr))
            ;(key-wall-brace lastcol cornerrow 0 -1 br lastcol cornerrow 1 0 br)
     bottom-right-corner      
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
   ; (-# (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial key-place 0 0) 0 1 oled-post-tl (partial key-place 0 0) -1 1 oled-post-tl  wall-xy-offset wall-xy-offset)))
   (curved-corner 0 1 -1 1 -1 0 (partial key-place 0 0) oled-post-tl)
    (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial key-place 0 0) -1 0 oled-post-tl (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset)) 1 1 oled-post  wall-xy-offset wall-xy-offset))
    (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-radius-with-offset (- tps-65-mount-corner-radius-with-offset)) 1 1 oled-post (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 1 0 oled-post wall-xy-offset wall-xy-offset ))
    ;(-# (when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 1 0 oled-post (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 0 oled-post wall-xy-offset wall-xy-offset)))

    (curved-corner-xy 1 0 1 1 0 1 (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) oled-post wall-xy-offset)
     ;(when (= screen-holder-mount-position "screen-holder-mount-side") (wall-brace-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 1 0 oled-post (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 oled-post wall-xy-offset wall-xy-offset))
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


(defn apply-key-geometry-rotation-variable [rot1-x rot1-y rot1-z x-rot-slice y-rot-slice z-rot-slice rotate-x-fn rotate-y-fn rotate-z-fn index shape] 
  (->> shape
         (rotate-x-fn (+ rot1-x (* x-rot-slice index)))
       (rotate-z-fn (+ rot1-z (* z-rot-slice index)))  
       (rotate-y-fn (+ rot1-y (* y-rot-slice  index)))
        
       )
  )
  

(defn between-ring-and-pinky-front-wall [{:keys [target1-dx target1-dy target1-place target1-post target1-rad-or-deg
                                                 dx1 dy1 place1 rotate1 post-position-1
                                           dxmid1 dymid1 place-mid1 rotatemid1 post-position-mid1
                                           dxmid2 dymid2 place-mid2 rotatemid2 post-position-mid2
                                           dxmid3 dymid3 place-mid3 rotatemid3 post-position-mid3
                                                 dx2 dy2 place2 rotate2 post-position-2
                                         target2-dx target2-dy target2-place target2-post target2-rad-or-deg
                                                 rotation-values1 rotation-values2 rotate-x-fn rotate-y-fn rotate-z-fn variable-rotation-fn
                                           post steps] :or {steps 20}}]
  (let [
       curved-wall (curved-corner-quartic
         {:target1-dx target1-dx :target1-dy target1-dy :target1-place target1-place :target1-post target1-post :target1-rad-or-deg target1-rad-or-deg
          :dx1 dx1 :dy1 dy1 :place1 place1 :rotate1 rotate1 :post-position-1 post-position-1
          :dxmid1 dxmid1 :dymid1 dymid1 :place-mid1 place-mid1 :rotatemid1 rotatemid1 :post-position-mid1 post-position-mid1
          :dxmid2 dxmid2 :dymid2 dymid2 :place-mid2 place-mid2 :rotatemid2 rotatemid2 :post-position-mid2 post-position-mid2
          :dxmid3 dxmid3 :dymid3 dymid3 :place-mid3 place-mid3 :rotatemid3 rotatemid3 :post-position-mid3 post-position-mid3
          :dx2 dx2 :dy2 dy2 :place2 place2 :rotate2 rotate2 :post-position-2 post-position-2
          :target2-dx target2-dx :target2-dy target2-dy :target2-place target2-place :target2-post target2-post :target2-rad-or-deg target2-rad-or-deg
          :rotation-values1 rotation-values1 :rotation-values2 rotation-values2  :rotate-x-fn rotate-x-fn  :rotate-y-fn rotate-y-fn :rotate-z-fn rotate-z-fn :variable-rotation-fn variable-rotation-fn
          :post post :steps steps})
  ]
    (union
     curved-wall
     )
    
    )

  )

(defn between-middle-and-thumb-front-wall [{:keys [dx1 dy1 place1 rotate1 post-position-1
                                                 dxmid1 dymid1 place-mid1 rotatemid1 post-position-mid1
                                                 dxmid2 dymid2 place-mid2 rotatemid2 post-position-mid2
                                                 dx2 dy2 place2 rotate2 post-position-2 
                                                 rotation-values1 rotation-values2 
                                                 rotate-x-fn rotate-y-fn rotate-z-fn variable-rotation-fn post steps] :or {steps 20}}]
  (let [curved-wall (curved-corner-cubic
                     {:dx1 dx1 :dy1 dy1 :place1 place1 :rotate1 rotate1 :post-position-1 post-position-1
                      :dxmid1 dxmid1 :dymid1 dymid1 :place-mid1 place-mid1 :rotatemid1 rotatemid1 :post-position-mid1 post-position-mid1
                      :dxmid2 dxmid2 :dymid2 dymid2 :place-mid2 place-mid2 :rotatemid2 rotatemid2 :post-position-mid2 post-position-mid2
                      :dx2 dx2 :dy2 dy2 :place2 place2 :rotate2 rotate2 :post-position-2 post-position-2 
                      :rotation-values1 rotation-values1 :rotation-values2 rotation-values2 :rotate-x-fn rotate-x-fn  :rotate-y-fn rotate-y-fn :rotate-z-fn rotate-z-fn :variable-rotation-fn variable-rotation-fn
                      
                      :post post :steps steps})]
    (union
     curved-wall)))


    
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
  (-# (key-place 3 lastrow oled-post-br))
  (curved-corner 0 -1 1 -1 1 0 (partial key-place 4 cornerrow) oled-post-br)
  ;(-# (translate (key-position  3 lastrow [0 0 0]) (apply-key-geometry-rotation 3 lastrow (translate oled-post-br-translation-vector oled-post))))
  ;(curved-corner -1 0 -1 -1 0 -1 (partial key-place 3 lastrow) oled-post-bl)
 ;(curved-corner 0 -1 1 -1 1 0 (partial key-place 3 lastrow) oled-post-br)
  (key-wall-brace 3 lastrow 0 -1 oled-post-bl 3 lastrow 0 -1 oled-post-br)
 ; (key-wall-brace 4 cornerrow 0 -1 oled-post-br 4 cornerrow 0 -1 oled-post-bm)
  ;(key-wall-brace 4 cornerrow 0.5 -1 oled-post-bl 4 cornerrow 0 -1 oled-post-br )
  (between-ring-and-pinky-front-wall 
   {:target1-dx 0 :target1-dy -1 :target1-place (partial key-place 4 cornerrow) :target1-post oled-post-br :target1-rad-or-deg :radians
    :dx1 0  :dy1 -0.9 :place1 (partial key-position 4 cornerrow) :rotate1 (partial apply-key-geometry-rotation 4 cornerrow)   :post-position-1 "br"
    :dxmid1 0 :dymid1 -0.7 :place-mid1 (partial key-position 4 cornerrow) :rotatemid1 (partial apply-key-geometry-rotation 4 cornerrow) :post-position-mid1 "bm" 
    ; :dxmid1 1  :dymid1 0 :place-mid1  (partial key-position 3 lastrow) :rotatemid1 (partial apply-key-geometry-rotation 3 lastrow) :post-position-mid1 "br"
     :dxmid2 1  :dymid2 -0.7 :place-mid2  (partial key-position 4 cornerrow) :rotatemid2 (partial apply-key-geometry-rotation 4 cornerrow) :post-position-mid2 "bl"
    :dxmid3 0.7  :dymid3 -0.7 :place-mid3  (partial key-position 3 lastrow) :rotatemid3 (partial apply-key-geometry-rotation 3 lastrow) :post-position-mid3 "br"
    :dx2 0.4 :dy2 -0.7 :place2  (partial key-position 3 lastrow) :rotate2 (partial apply-key-geometry-rotation 3 lastrow)   :post-position-2 "br"
    :target2-dx 0 :target2-dy -1 :target2-place (partial key-place 3 lastrow) :target2-post oled-post-br :target2-rad-or-deg :radians
    :rotation-values1  (apply-key-geometry-rotation-values 4 cornerrow) :rotation-values2 (apply-key-geometry-rotation-values 3 lastrow)  
    :rotate-x-fn rx  :rotate-y-fn ry :rotate-z-fn rz :variable-rotation-fn (partial apply-key-geometry-rotation-variable)
    :post oled-post :steps 20})
  
  ;; (curved-corner-quadratic
  ;;  {:dx1 0  :dy1 -1 :place1 (partial key-position 3 lastrow) :rotate1 (partial apply-key-geometry-rotation 3 lastrow)   :post-position-1 "br"
  ;;   :dxmid1 1 :dymid1 -1 :place-mid1 (partial key-position 3 lastrow) :rotatemid1 (partial apply-key-geometry-rotation 3 lastrow) :post-position-mid1 "br"
  ;;   ; :dxmid1 1  :dymid1 0 :place-mid1  (partial key-position 3 lastrow) :rotatemid1 (partial apply-key-geometry-rotation 3 lastrow) :post-position-mid1 "br"
  ;;   ; :dxmid2 1  :dymid2 -1 :place-mid2  (partial key-position 3 lastrow) :rotatemid2 (partial apply-key-geometry-rotation 3 lastrow) :post-position-mid2 "br"
  ;;   :dx2 -1 :dy2 0 :place2  (partial key-position 3 lastrow) :rotate2 (partial apply-key-geometry-rotation 3 lastrow)   :post-position-2 "br"
  ;;   :rotation-values1   (apply-key-geometry-rotation-values 3 lastrow):rotation-values2 (apply-key-geometry-rotation-values 3 lastrow)
  ;;   :rotate-x-fn rx  :rotate-y-fn ry :rotate-z-fn rz :variable-rotation-fn (partial apply-key-geometry-rotation-variable)
  ;;   :post oled-post :steps 20})
  
  (between-middle-and-thumb-front-wall
   {:dx1 0 :dy1 -1 :place1 (partial key-position 2 lastrow) :rotate1 (partial apply-key-geometry-rotation 2 lastrow)   :post-position-1 "bm"
    :dxmid1 -1 :dymid1 -1 :place-mid1 (partial key-position 2 lastrow) :rotatemid1 (partial apply-key-geometry-rotation 2 lastrow) :post-position-mid1  "br"
    :dxmid2 -1  :dymid2 0 :place-mid2  (partial key-position 3 lastrow) :rotatemid2 (partial apply-key-geometry-rotation 3 lastrow) :post-position-mid2 "bl"
    :dx2 0 :dy2 -1.1 :place2  (partial key-position 3 lastrow) :rotate2 (partial apply-key-geometry-rotation 3 lastrow)   :post-position-2 "bl"
    :rotation-values1  (apply-key-geometry-rotation-values 2 lastrow) :rotation-values2 (apply-key-geometry-rotation-values 3 lastrow)
    :rotate-x-fn rx  :rotate-y-fn ry :rotate-z-fn rz :variable-rotation-fn (partial apply-key-geometry-rotation-variable)
    :post oled-post :steps 20})
  
;;  (key-wall-brace 3 lastrow   0 -1 oled-post-bl 3 lastrow 0 -1 oled-post-br)
;;  (key-wall-brace 3 lastrow 0 -1 oled-post-br 4 cornerrow 0 -1 oled-post-br)
;;(hull 
;; (key-place lastcol cornerrow web-post-bl)
;; (key-place lastcol cornerrow web-post-br)
;; (key-place (dec lastcol) lastrow web-post-br)
;; )
  )    
         ))
  )

(defn thumb-place-rotation-variable [rot1-x rot1-y rot1-z x-rot-slice y-rot-slice z-rot-slice rotate-x-fn rotate-y-fn rotate-z-fn index shape]
  (->> shape
       (rotate-x-fn (+ rot1-x (* x-rot-slice (inc index))))
       (rotate-z-fn (+ rot1-z (* z-rot-slice (inc index))))
       (rotate-y-fn (+ rot1-y (* y-rot-slice (inc index))))))

(defn between-thumb-and-front-wall [{:keys [dx1 dy1 place1 rotate1 post-position-1 
                                            dxmid1 dymid1 place-mid1 rotatemid1 post-position-mid1 
                                            dxmid2 dymid2 place-mid2 rotatemid2 post-position-mid2  
                                            dx2 dy2 place2 rotate2 post-position-2
                                           rotation-values1 rotation-values2 rotate-x-fn rotate-y-fn rotate-z-fn variable-rotation-fn
                                            post steps] :or {steps 20}}]
  
   (let [web-post-curved-list (plot-bezier-points-variable-rotation   (wall-corner-cubic dx1 dy1 place1 (get-web-corner-translation-vector post-position-1)
                                                                                         dxmid1 dymid1 place-mid1 (get-web-corner-translation-vector post-position-mid1)
                                                                                         dxmid2 dymid2 place-mid2 (get-web-corner-translation-vector post-position-mid2)
                                                                                         dx2 dy2  place2 (get-web-corner-translation-vector post-position-2)
                                                                                         0 0 steps)
                                                                      web-post rotation-values1 rotation-values2 thumb-place-rotation-variable) 
         curved-wall (curved-corner-cubic
                      {:dx1 dx1 :dy1 dy1 :place1 place1 :rotate1 rotate1 :post-position-1 post-position-1
                       :dxmid1 dxmid1 :dymid1 dymid1 :place-mid1 place-mid1 :rotatemid1 rotatemid1 :post-position-mid1 post-position-mid1
                       :dxmid2 dxmid2 :dymid2 dymid2 :place-mid2 place-mid2 :rotatemid2 rotatemid2 :post-position-mid2 post-position-mid2
                       :dx2 dx2 :dy2 dy2 :place2 place2 :rotate2 rotate2 :post-position-2 post-position-2
                       :rotation-values1 rotation-values1 :rotation-values2 rotation-values2 :rotate-x-fn rotate-x-fn  :rotate-y-fn rotate-y-fn :rotate-z-fn rotate-z-fn :variable-rotation-fn variable-rotation-fn
                       :post post :steps steps})
         web-post-curved-list-last (last web-post-curved-list)
         fill-gap-to-thumb-tr (hull 
                               web-post-curved-list-last
                               (thumb-mr-place web-post-tr)
                               (thumb-tr-place web-post-br)
                               )
         ]
     (union
    curved-wall
    
    (for [index (range 0 (- (count web-post-curved-list) 1))]
           (hull
            (nth web-post-curved-list index)
            (nth web-post-curved-list (inc index))
            (thumb-mr-place web-post-tr) 
            )
          )
      fill-gap-to-thumb-tr
    ))
  )

(def thumb-walls
  (union 
   
   ;(curved-corner 1 1 1 1 1 0 (partial thumb-mr-place) oled-post-br)
   ;(curved-corner 1 0 1 1 1 1 (partial thumb-mr-place) oled-post-br)
   ;(curved-corner 1 1 1 2 2 2  (partial thumb-mr-place ) oled-post-br)
   ;(-# (curved-corner -1 -1 -1 -1 0 -1 (partial thumb-tr-place) oled-post-br))
     ;(translate (rotate-around-z-in-degrees 25 (rotate-around-y-in-degrees -23(rotate-around-x-in-degrees 10 [0 0 0])))
   
                ;; (between-thumb-and-front-wall 
                ;;  {:dx1 1 :dy1 0 :place1 (partial transform-position thumb-mr-place ) :rotate1 (partial rotate-position thumb-mr-rotate) :post-position-1 "br"
                ;;  :dxmid1 1 :dymid1 -1 :place-mid1 (partial transform-position thumb-mr-place) :rotatemid1 (partial rotate-position thumb-mr-rotate) :post-position-mid1 "tr" 
                ;;  :dxmid2 -0.5 :dymid2 -1 :place-mid2 (partial transform-position thumb-tr-place) :rotatemid2 (partial rotate-position thumb-tr-rotate) :post-position-mid2 "br"  
                ;;  :dx2 0 :dy2 -1 :place2 (partial transform-position thumb-tr-place) :rotate2 (partial rotate-position thumb-tr-rotate) :post-position-2 "br" 
                ;;   :rotation-values1 thumb-mr-rotation-values :rotation-values2 thumb-tr-rotation-values :rotate-x-fn  (partial rdx) :rotate-y-fn (partial rdy) :rotate-z-fn  (partial rdz)
                ;;   :variable-rotation-fn (partial thumb-place-rotation-variable)
                ;;   :post oled-post :steps 20} 
                ;;   )
                
                
   ;(curved-corner -1 -1 -1 -2 -1 -2  (partial thumb-tr-place) oled-post-br)
   
   ;(wall-brace thumb-mr-place 1 0 oled-post-br thumb-tr-place -1 -2  oled-post-br)
    
   ;(wall-brace thumb-mr-place  0 -1 oled-post-br thumb-tr-place  0 -1 oled-post-br)
    
   (translate (thumb-tr-position web-post-bl-translation-vector))
(thumb-wall-brace thumb-mr-place  0 -1 oled-post-br thumb-mr-place  0 -1 oled-post-bl thumb-mr-rotate thumb-mr-rotate)
(thumb-wall-brace thumb-br-place  0 -1 oled-post-br thumb-br-place  0 -1 oled-post-bl thumb-br-rotate thumb-br-rotate)
;(wall-brace thumb-bl-place  0  1 oled-post-tr thumb-bl-place  0  1 oled-post-tl)
(thumb-wall-brace thumb-br-place -1  0 oled-post-tl thumb-br-place -1  0 oled-post-bl thumb-br-rotate thumb-br-rotate)
(thumb-wall-brace thumb-bl-place -1  0 oled-post-tl thumb-bl-place -1  0 oled-post-bl thumb-bl-rotate thumb-bl-rotate)
   ))

(def thumb-corners
  (union
  (curved-corner -1 0 -1 -1 0 -1 (partial thumb-br-place) oled-post-bl)
  (curved-corner 0 -1 1 -1 1 0 (partial thumb-mr-place) oled-post-br) 
     ;(-# (wall-brace thumb-br-place -1  0 oled-post-bl thumb-br-place  0 -1 oled-post-bl))
;(wall-brace thumb-bl-place -1  0 oled-post-tl thumb-bl-place  0  1 oled-post-tl)
   ))

(def thumb-tweeners
     (union 
    (thumb-wall-brace thumb-mr-place  0 -1 oled-post-bl thumb-br-place  0 -1 oled-post-br thumb-mr-rotate thumb-br-rotate)
(thumb-wall-brace thumb-bl-place -1  0 oled-post-bl thumb-br-place -1  0 oled-post-tl  thumb-bl-rotate thumb-br-rotate)
;(wall-brace thumb-tr-place  0 -1 oled-post-br (partial key-place 3 lastrow)  0 -1 oled-post-bl (get-thumb-translate-fn thumb-tr-place) (partial key-position  3 lastrow) thumb-tr-rotate (partial apply-key-geometry-rotation 3 lastrow))
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
  ;(hull 
  ; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
  ; (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left oled-post)
  ; (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/ (- screen-holder-width) 2) 0 oled-post)
  ; )
 (triangle-hulls 
  ;(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
  (screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
(EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left oled-post)
(EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-left oled-post))
   
   (hull
    (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-left oled-post)
    (screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
    )
   
    (hull
     (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left oled-post)
(screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
     (screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
     )
   
;;    (-# (triangle-hulls (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
;; (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left oled-post)
;; (EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left oled-post)))
   
   (triangle-hulls (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
(EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right oled-post))
   
   
   (triangle-hulls 
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod) tps-65-mount-corner-radius-with-offset oled-post)
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

(def between-thumb-cornerrow-and-trackpad
  (let [thumb-position (thumb-tl-place (partial map +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees web-post-tl-translation-vector)
        trackpad-corner (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) (partial map +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees [0 0 0])
        corner-key (key-position 0 cornerrow web-post-bl-translation-vector)
        trackpad-to-thumb-bezier-modifier-initial-point (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius)  (- tps-65-mount-corner-radius-with-offset-mod-neg) (partial map +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees [0 0 0])
        trackpad-to-thumb-bezier-modifier [(first trackpad-to-thumb-bezier-modifier-initial-point) (second trackpad-to-thumb-bezier-modifier-initial-point) (last thumb-position)]
        trackpad-to-thumb-bezier-points (bezier-quadratic trackpad-corner trackpad-to-thumb-bezier-modifier thumb-position 20)
        corner-key-to-thumb-bezier-modifier [(first corner-key) (second corner-key) (last thumb-position)]
        corner-key-to-thumb-bezier-points (bezier-quadratic corner-key corner-key-to-thumb-bezier-modifier thumb-position 20)
        ]

    (hull
     (plot-bezier-points trackpad-to-thumb-bezier-points curve-post)
     (plot-bezier-points corner-key-to-thumb-bezier-points curve-post)
     (thumb-tl-place web-post-tl)
 (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
 (key-place 0 cornerrow web-post-bl)
     ;(translate thumb-position (->> oled-post
     ;                               (rdx  10)
     ;                               (rdy -23)
     ;                               (rdz  25)))
     ;(translate trackpad-corner oled-post)
     ;(translate corner-key web-post)
    )
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

   (-# between-thumb-cornerrow-and-trackpad)
;; (-# (hull 
;;  (thumb-tl-place web-post-tl)
;; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg) oled-post)
;; (key-place 0 cornerrow web-post-bl)
;;  )  ) 
   (triangle-hulls
    

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
    ;( screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
   ; (wall-bezier-1-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 wall-xy-offset oled-post)
(tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
    )
    (hull
    (screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
    (screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
    )

  ;;  (-# (hull 
  ;;  ; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
  ;;    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
  ;;   (screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
  ;;   (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
  ;;   ))

   (hull
    (bottom-hull 
     (wall-bezier-2-xy (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset) 0 1 wall-xy-offset oled-post)
     (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset (translate (wall-locate3-xy 0 1 wall-xy-offset) oled-post)))
   
    (wall-bezier-1-xy (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0) 0 1 wall-xy-offset-thin oled-post)
    ( screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post)
    (bottom 0.001 (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post))

    ;(bottom 0.001 (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset (translate (wall-locate3-xy 0 1 wall-xy-offset) oled-post)))
    )

   (hull
    (wall-bezier-1-xy (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0) 0 1 wall-xy-offset-thin oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
    (tps-65-translate-and-place-with-radius [0 (- (/ tps-65-length 2) tps-65-corner-radius) 0] 0  tps-65-mount-corner-radius-with-offset oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
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
  
   ; (-# (triangle-hulls
  ;  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
  ;  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
  ;  (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post)))
;
;  (hull 
;   (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
;screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post)
;tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post))
;  
   (triangle-hulls
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)

  ;  (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post)
  ;  (screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2)  0 oled-post)
  ;  (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)

    

    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius)  tps-65-mount-corner-radius oled-post)
    (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius  tps-65-mount-corner-radius oled-post))
   ;(triangle-hulls
    ;(screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  screen-holder-width 2)  0 oled-post)
    ;(screen-holder-translate-and-place-side (/ screen-holder-height 2) (/  (- screen-holder-width) 2)  0 oled-post)
   ; (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset oled-post))
   ) 
    


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
   
;; (def back-wall-to-screen 
;;   (let)
;;   )

(def left-section
  (union
   (when (= screen-holder-mount-position "screen-holder-mount-top")(screen-holder-place screen-holder))
   (when (= screen-holder-mount-position "screen-holder-mount-side") (screen-holder-place-side screen-holder))
   ;left-side
   (when (= screen-holder-mount-position "screen-holder-mount-top") ( color [0 1 0] left-side-new))
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

(defn usb-jack-place [shape]  
  (->> shape
      
       (rdz far-index-splay) 
      (translate (rotate-around-z far-index-splay [-0.5 (first far-index-post-splay-translation) 0])) ;x and y axes get reversed by rotate-around-z 
   (translate (map + usb-holder-position [-12 6.5 3]) )
       
       
   ))
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