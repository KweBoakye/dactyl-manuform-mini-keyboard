(ns dactyl-keyboard.low.case-low-functions
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            
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
            [dactyl-keyboard.vybronics-vl91022 :refer :all]
            ))

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

(def left-section-rotation-angle oled-mount-rotation-z-old)
(defn left-key-position [row direction]
  (map - (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0]) [left-wall-x-offset 0 left-wall-z-offset]))

(defn left-key-place [row direction shape]
  (translate (left-key-position row direction) shape))

(def left-wall-plate-key-position
  (cond
    (= nrows 5) 0
    (= nrows 4) -1
    (= nrows 3) -1))

(defn left-wall-plate-position [xdir ydir]
  (->>
   (add-vec
    [left-wall-x-offset-oled 0 (- left-wall-z-offset 2)]
    (key-position 0 left-wall-plate-key-position [0 0 0])
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

(defn left-wall-plate-place ([xdir ydir shape] (left-wall-plate-place xdir ydir translate rdx rdz shape))
  ([xdir ydir translate-fn rotate-x-fn rotate-z-fn shape] (->> shape
                                                               (translate-fn (left-wall-plate-position xdir ydir))
                                                               (rotate-x-fn oled-mount-rotation-x-old)
                                                               (rotate-z-fn oled-mount-rotation-z-old))))

(defn left-wall-plate-place-variable [xdir ydir shape]
  (->> shape
       (translate (left-wall-plate-position-variable xdir ydir))
       (rotate oled-mount-rotation-x-old [1 0 0])
       (rotate oled-mount-rotation-z-old [0 0 1])))

(defn wall-locate0 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) 0])
(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) -1])
(defn wall-locate2 ([dx dy] (wall-locate2 dx dy [0 0 0])) ([dx dy orig-point] (mapv + orig-point [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset])))
(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness)) (* dy (+ wall-xy-offset wall-thickness)) wall-z-offset])


(defn wall-locate22-xy [dx dy xy] [(* dx (/ xy 2)) (* dy (/ xy 2)) wall-z-offset])
(defn wall-locate2-xy [dx dy xy] [(* dx xy) (* dy xy) wall-z-offset])
(defn wall-locate2.5-xy [dx dy xy] [(* dx (+ xy wall-thickness)) (* dy (+ xy wall-thickness)) wall-z-offset])

(defn wall-locate3-xy [dx dy xy] [(* dx (+ xy wall-thickness)) (* dy (+ xy wall-thickness)) wall-z-offset])



(defn wall-locate3-xy-3d [dx dy dz xy] [(* dx (+ xy wall-thickness)) (* dy (+ xy wall-thickness)) (wall-z-offset)])


(def fillet-s (->>
               (binding [*fn* 36] (sphere 2))
               (translate [0 0 plate-thickness])))

(defn check-post [post]
  (cond
    (identical? post oled-post-tr) web-post-tr
    (identical? post oled-post-tl) web-post-tl
    (identical? post oled-post-bl) web-post-bl
    (identical? post oled-post-br)   web-post-br
    (identical? post oled-post-bm)   web-post-bm
    :else post))

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
                                                steps))))




(defn fillet-about-point-2 ([dx dy steps] (fillet-about-point dx dy steps [0 0 0]))
  ([dx dy steps orig-point] (let [w1 [(* dx (+ wall-xy-offset wall-thickness
                     ;(- (/ oled-post-size 2))
                                               ))(* dy (+ wall-xy-offset wall-thickness
                                 ;(- (/ oled-post-size 2))
                                                          ))(+ wall-z-offset 0.0)]
                                  point1 [(* (first w1) 0.98) (* (second w1) 0.98) (+ (last w1) 0.0)]
                                  w2 (wall-locate3 dx dy)
                                  point2 [(* (first w2) 0.98) (* (second w2) 0.98) (+ (last w2) 0.5)]]

                              (bezier-quadratic ;point1 
                               (mapv + point1 orig-point)
                     ; [(* dx (+ wall-xy-offset wall-thickness oled-post-size)) (* dy (+ wall-xy-offset wall-thickness oled-post-size)) 0]
                               (mapv + [(* dx (+ wall-xy-offset wall-thickness (+ (/ oled-post-size 2)))) (* dy (+ wall-xy-offset wall-thickness (+ (/ oled-post-size 2)))) (- wall-z-offset 1)] orig-point)
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
        point1 [(* (first w1) 0.98) (* (second w1) 0.98) (+ (last w1) 0.0)]]
    (bezier-quadratic
     point1
     [(* dx (+ xy wall-thickness (+ (/ oled-post-size 2)))) (* dy (+ xy wall-thickness (+ (/ oled-post-size 2)))) (- wall-z-offset 1)]
     [(* 0.98 (* dx (+ xy wall-thickness (+ (/ oled-post-size 2))))) (* 0.98 (* dy (+ xy wall-thickness (+ (/ oled-post-size 2))))) (+ 0.5 (- wall-z-offset (/ oled-holder-thickness  1)))]
     steps)))

(defn bezier-points-for-oled-post [dx dy]
  (let [shift-factor (fn [point] (cond
                                   (> point 0) (/ oled-post-size 2)
                                   (< point 0) (- (/ oled-post-size 2))
                                   :else point))]
    [[0 0 (/ oled-holder-thickness 2)] [(shift-factor dx) (shift-factor dy) (/ oled-holder-thickness 2)]  [0 0 (- (/ oled-holder-thickness 2))]]))




(defn fillet-about-point-lower [points vector w3 oled-post-type place]

  (->>

   (mapv (fn [point]
           (->> (translate point (convert-to-curve-post oled-post-type))
                (translate oled-translation-vector)
                (translate vector)
                (translate w3)
                (place))) points)))



(defn plot-and-translate-bezier-points ([place bezier-points shape] (plot-and-translate-bezier-points place bezier-points shape [0 0 0]))
  ([place bezier-points shape translation-modifier] (plot-and-translate-bezier-points place bezier-points translate shape translation-modifier))
  ([place bezier-points translate-fn shape translation-modifier]
   (mapv (fn [point]
           (->>
            (translate-fn point shape)
            (place)
            (translate-fn translation-modifier)))
         bezier-points)))




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
        z-rot-slice (/ rotation-variable-z number-of-slices)]
    (for [index (range 0 number-of-slices)]
      (translate (nth bezier-points index) (variable-rotation-fn rot1-x rot1-y rot1-z x-rot-slice y-rot-slice z-rot-slice rotate-x-fn rotate-y-fn rotate-z-fn index shape)))))

(defn wall-bezier-1 [place dx dy post]
  (hull
   (plot-and-translate-bezier-points (partial place)  (fillet-about-point dx dy 20)  (convert-to-curve-post post)))
  (place (check-post post)))

(defn wall-bezier-2 [place dx dy post]
  (plot-and-translate-bezier-points (partial place)  (fillet-about-point-2 dx dy 20)  (convert-to-curve-post post)))

(defn wall-bezier-1-xy [place dx dy xy post]
  (hull
   (plot-and-translate-bezier-points (partial place)  (fillet-about-point-xy dx dy xy 20)  (convert-to-curve-post post))
   (place (check-post post))))

(defn wall-bezier-2-xy [place dx dy xy post]
  (hull
   (plot-and-translate-bezier-points (partial place)  (fillet-about-point-xy-2 dx dy xy 20)  (convert-to-curve-post post))
   (place (translate (wall-locate2-xy dx dy xy) post))))
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

  ([place1 dx1 dy1 post1 place2 dx2 dy2 post2 transform-fn1 transform-fn2 rotation-fn1 rotation-fn2]
   (let [oled-translation-vector-z (last oled-translation-vector)
         bottom-pos1   (+ (* oled-holder-thickness 2) oled-translation-vector-z (last (transform-fn1 (wall-locate2 dx1 dy1))))
         bottom-pos2 (+ (* oled-holder-thickness 2) oled-translation-vector-z (last (transform-fn2 (wall-locate2 dx2 dy2))))
         lower-curve1  (plot-and-translate-bezier-points (partial place1)  (fillet-about-point-2 dx1 dy1 20)  (convert-to-curve-post post1))
         lower-curve2 (plot-and-translate-bezier-points (partial place2)  (fillet-about-point-2 dx2 dy2 20)  (convert-to-curve-post post2))
         last-curve-sphere1 (last lower-curve1)
         last-curve-sphere2 (last lower-curve2)]

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

       (translate [0 0 (-  bottom-pos2)] (place2 (translate (wall-locate2 dx2 dy2) post2))))))))










(defn wall-brace-half [place dx dy post]
  (plot-and-translate-bezier-points (partial place)  (fillet-about-point dx dy 20)  (convert-to-curve-post post))
  (place (check-post post))
  (place (translate (wall-locate1 dx dy)  post))
  (place (translate (wall-locate2 dx dy) post))

  (bottom-hull
   (place (translate (wall-locate2 dx dy) post))
   (plot-and-translate-bezier-points (partial place)  (fillet-about-point-2 dx dy 0)  (convert-to-curve-post post))))




(defn wall-brace-xy
  ([place1 dx1 dy1 post1 place2 dx2 dy2 post2 xy1 xy2]
   (wall-brace-xy place1 dx1 dy1 post1 place2 dx2 dy2 post2 xy1 xy2 true))
  ([place1 dx1 dy1 post1 place2 dx2 dy2 post2 xy1 xy2 bottom] (union
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
                                                                  (plot-and-translate-bezier-points (partial place2) (fillet-about-point-xy-2 dx2 dy2 xy2 20) (convert-to-curve-post post2)))))))

(defn wall-brace-xy-half-top [place dx dy post xy]

  (union
   (place (check-post post))

   (place (translate (wall-locate1 dx dy) post))
   (place (translate (wall-locate2-xy dx dy xy) post))
   (plot-and-translate-bezier-points (partial place)  (fillet-about-point-xy dx dy xy 20)  (convert-to-curve-post post))))
(defn wall-brace-xy-half-bottom [place dx dy post xy]
  (bottom-hull
   (place (translate (wall-locate2 dx dy) post))
   (plot-and-translate-bezier-points (partial place)  (fillet-about-point-xy-2 dx dy xy 10)  (convert-to-curve-post post))))

(defn wall-brace-xy-half [place dx dy post xy]
  (union
   (wall-brace-xy-half-top place dx dy post xy)
   (wall-brace-xy-half-bottom place dx dy post xy)))

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
    (= (name thumb-place) "thumb-br-place") (partial rotate-position thumb-br-rotate)))
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

(def tps-65-z-position 10)
(def tps-65-z-rotation (+ 90 far-index-splay))
(def tps-65-x-rotation -20)
(def tps-65-y-rotation 0)

(defn tps-65-rotate ([shape] (tps-65-rotate  rdx rdy rdz shape))
  ([rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape (rotate-z-fn tps-65-z-rotation)
        (rotate-x-fn tps-65-x-rotation)
        (rotate-y-fn tps-65-y-rotation)
        (left-wall-plate-place-rotate  rotate-x-fn rotate-z-fn))))
(defn tps-65-place ([shape] (tps-65-place translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape] (->> shape
                                                                 (rotate-z-fn tps-65-z-rotation)
                                                                 (rotate-x-fn tps-65-x-rotation)

                                                                 (rotate-y-fn tps-65-y-rotation)
       ;(rdy -7.5)
                                                                 (left-wall-plate-place 0 (+ left-wall-y-modifier -3) translate-fn rotate-x-fn rotate-z-fn)
                                                                 (translate-fn [0 3 tps-65-z-position]))))



(defn div-by-2 [num]
  (/ num 2))

(defn col-avg [col1 col2]
  (->>
   (map + col1 col2)
   (map div-by-2)))






(defn tps-65-translate-and-place [x y z shape]
  (->> shape
       (translate [x y z])
       (tps-65-place)))

(defn tps-65-translate-and-place-at-position ([position shape] (tps-65-translate-and-place-at-position position translate rdx rdy rdz shape))
  ([position translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape] (->> shape
                                                                          (translate-fn position)
                                                                          (tps-65-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))

(defn tps-65-translate-and-place-at-position-with-offset ([position offset shape] (tps-65-translate-and-place-at-position-with-offset position offset translate rdx rdy rdz shape))
  ([position offset translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape] (->> shape 
                                                                          (translate-fn offset)       
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
        (tps-65-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))

(defn tps-65-translate-and-place-with-radius-xyz
  ([x y z radius-compensation-x radius-compensation-y shape]
   (tps-65-translate-and-place-with-radius-xyz x y z radius-compensation-x radius-compensation-y translate rdx rdy rdz shape))
  ([x y z radius-compensation-x radius-compensation-y translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (translate-fn [x y z])
       ;plate-thickness
        (translate-fn [(tps-radius-compensation-adjust radius-compensation-x) (tps-radius-compensation-adjust radius-compensation-y) (- plate-thickness)])
        (tps-65-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))

(defn EVQWGD001-place ([shape] (EVQWGD001-place translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape] (->> shape
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
                                                                 (rotate-y-fn -20)
                                                                 (rotate-x-fn -20)
                                                                 (tps-65-translate-and-place-with-radius [(- tps-65-corner-radius (/ tps-65-mount-width 2)),
                                                                                                          0,
                                                                                                          0] (- tps-65-mount-corner-radius-with-offset)  0
                                                                                                         translate-fn rotate-x-fn rotate-y-fn rotate-z-fn)
                                                                 (translate-fn [(- (/ EVQWGD001-mount-width 2)) (- (/ EVQWGD001-height 2)) (- (/ EVQWGD001-mount-length 2))]))))

(defn EVQWGD001-translate-and-place ([x y z shape] (EVQWGD001-translate-and-place x y z translate rdx rdy rdz shape))
  ([x y z translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (translate-fn [x y z])
        (EVQWGD001-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))

(defn EVQWGD001-translate-and-place-at-position ([position shape] (EVQWGD001-translate-and-place-at-position position translate rdx rdy rdz shape))
  ([position translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (translate-fn position)
        (EVQWGD001-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))

(defn EVQWGD001-translate-and-place-at-position-with-offset ([position offset shape] (EVQWGD001-translate-and-place-at-position-with-offset position offset translate rdx rdy rdz shape))
  ([position offset translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (translate-fn offset)
        (translate-fn position)
        (EVQWGD001-place translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))

(defn screen-holder-rotate [shape]
  (->> shape
       (rdx -5)
       (rdy 7.5)
       (rdz screen-rotation-angle)))

(defn screen-holder-place [shape]
  (->> shape
       (screen-holder-rotate)
       (left-wall-plate-place 0 (+ left-wall-y-modifier 0.5))
       (translate screen-holder-position)
       (translate [0 0 (cond (= nrows 5) 0 (= nrows 4) -4 (= nrows 3) -4)])))

(defn screen-holder-place-x-y [xdir ydir shape]
  (->> shape
       (translate [(* xdir oled-holder-width 0.5) (* ydir oled-holder-height 0.5) (/ screen-holder-depth 2)])
       (screen-holder-rotate)
       (translate [(- (* xdir oled-holder-width 0.5)) (- (* ydir oled-holder-height 0.5)) 0])
       (left-wall-plate-place xdir ydir)

       (translate screen-holder-position)))

(defn screen-holder-translate-and-place [x y z shape]
  (->> shape
       (translate [x y z])
       (screen-holder-place)))

(def screen-holder-rotate-side-y -70)
(defn screen-holder-rotate-side
  ([shape] (screen-holder-rotate-side rdz rdy rdz shape))
  ([rotate-x-fn rotate-y-fn rotate-z-fn shape] (->> shape
                                                    (rotate-z-fn -90)
                                                    (rotate-y-fn screen-holder-rotate-side-y)
                                                    (rotate-x-fn -18)
                                                    (rotate-z-fn 20))))
(defn screen-holder-place-side ([shape] (screen-holder-place-side translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape] (->> shape
                                                                 (screen-holder-rotate-side rotate-x-fn rotate-y-fn rotate-z-fn)
                                                                 (translate-fn [0 -20 8])
       ;(rdz 5)
                                                                 (left-wall-plate-place -2 -3 translate-fn rotate-x-fn rotate-z-fn)
                                                                 (translate-fn [0 0 (- 2.5 keyboard-z-offset 3)]))))

(defn screen-holder-translate-and-place-side ([x y z shape] (screen-holder-translate-and-place-side x y z translate rdx rdy rdz shape))
  ([x y z translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape] (->> shape
                                                                       (translate-fn [x y z])
                                                                       (screen-holder-place-side translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))
(defn screen-holder-translate-and-place-side-with-offset ([x y z offset shape] (screen-holder-translate-and-place-side-with-offset x y z offset translate rdx rdy rdz shape))
  ([x y z offset translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape] (->> shape 
                                                                              (translate-fn offset)
                                                                              (translate-fn [x y z]) 
                                                                              (screen-holder-place-side translate-fn rotate-x-fn rotate-y-fn rotate-z-fn))))

(defn vybronics-vl91022-place ([shape] (vybronics-vl91022-place translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (rotate-z-fn -90)
        (rotate-y-fn 180)
        (tps-65-translate-and-place-at-position [(/ vybronics-vl91022-y-axis 2)
                                                 0
                                                 (- (+ (* tps-65-depth 2) tps-65-depth-tolerance tps-65-overlay-thickness))] translate-fn rotate-x-fn rotate-y-fn rotate-z-fn)
    ) 
   ) 
  )

(defn wall-corner-points [dx1 dy1 dxmid dymid dx2 dy2 translation-mod  dz]


  (bezier-quadratic
   [(* dx1 translation-mod)  (* dy1 translation-mod) dz]
   [(* dxmid translation-mod) (* dymid translation-mod) dz]
   [(* dx2 translation-mod) (* dy2 translation-mod) dz]
   20))

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
                                                                               :degrees (partial transform-position))
                                                                   z-position (last (transform place [0 0 (- oled-holder-thickness wall-z-offset)]))]
                                                               (union
                                                                (hull
                                                                 (plot-and-translate-bezier-points place (wall-corner-points dx1 dy1 dxmid dymid dx2 dy2 wall-thickness  -1) post)
                                                                 upper-curves
                                                                 (place  (check-post post)))
                                                                (hull
                                                                 lower-curves)
                                                                (hull
                                                                 (-# lower-curves-last)
                                                                 (translate [0 0 (- z-position)] lower-curves-last)
                                                                 (plot-and-translate-bezier-points place (wall-corner-points dx1 dy1 dxmid dymid dx2 dy2 wall-xy-offset  wall-z-offset) post)
                                                                 (plot-and-translate-bezier-points place (wall-corner-points-bottom dx1 dy1 dxmid dymid dx2 dy2 wall-xy-offset  wall-z-offset) post))))))

(defn wall-corner-quadratic ([dx1 dy1 place1 post-position-1 dxmid dymid place-mid  post-position-mid dx2 dy2 place2 post-position-2 translation-mod  dz]
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
    (assoc (vec (place-mid1 (mapv + post-position-mid1 [(* dxmid1 translation-mod) (* dymid1 translation-mod) dz])))  2 (- oled-holder-thickness))
    (assoc (vec (place-mid2 (mapv + post-position-mid2 [(* dxmid2 translation-mod) (* dymid2 translation-mod) dz]))) 2 (- oled-holder-thickness))
    (assoc (vec (place2 (mapv + post-position-2 [(* dx2 translation-mod) (* dy2 translation-mod) dz])))  2 (- oled-holder-thickness))
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
           rotation-values1 rotation-values2 rotate-x-fn rotate-y-fn rotate-z-fn variable-rotation-fn post steps] :or {steps 15}}]
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
           rotation-values1 rotation-values2 rotate-x-fn rotate-y-fn rotate-z-fn variable-rotation-fn post steps] :or {steps 36}}]
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
    plot-points-with-rotation (fn [bezier-points shape] (plot-bezier-points-variable-rotation bezier-points rotation-values1 rotation-values2 rotate-x-fn rotate-y-fn rotate-z-fn variable-rotation-fn shape))]
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
        (plot-points-with-rotation (nth  lower-curves index) (convert-to-curve-post post)))
      (for [index (range 0 steps)]
        (plot-points-with-rotation
         (nth (bottom-translate (vec lower-curves)) index)
         (convert-to-curve-post post)))
      steps))))

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
    lower-curves (rearrange-nested-list steps 20 lower-curves-unrearranged)
    lower-curves-plotted (for [index (range 0 steps)]
                           (plot-points-with-rotation (nth  lower-curves index) (convert-to-curve-post post)))
    lower-curves-plotted-last (map (fn [points] (last points)) lower-curves-plotted)
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
       (target2-place (check-post target2-post)))
           ;(chained-hull 
      (tops-and-tails
       (for [index (range 0 steps)] (plot-points-with-rotation  (nth upper-curves index) (convert-to-curve-post post)))
       (plot-and-translate-bezier-points target1-place  (fillet-about-point target1-dx target1-dy 20)  (convert-to-curve-post target1-post))
       (plot-and-translate-bezier-points target2-place  (fillet-about-point target2-dx target2-dy 20)  (convert-to-curve-post target2-post)))
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
                      target2-lower-curve))
     (chained-hull-for-four-lists
      (tops-and-tails (plot-points-with-rotation   wall-locate-2-quartic
                                                   post)
                      (target1-place (translate (wall-locate2 target1-dx target1-dy) target1-post))
                      (target2-place (translate (wall-locate2 target2-dx target2-dy) target2-post)))
      (tops-and-tails (plot-points-with-rotation   wall-locate-2-quartic-bottom
                                                   post)
                      (translate [0 0 (- target1-bottom-z)] (target1-place (translate (wall-locate2 target1-dx target1-dy) target1-post)))
                      (translate [0 0 (- target2-bottom-z)] (target2-place (translate (wall-locate2 target2-dx target2-dy) target2-post))))


      (tops-and-tails
       lower-curves-plotted-last
       target1-lower-curve-last-sphere
       target2-lower-curve-last-sphere)
      (tops-and-tails
       (map (fn [point] (translate [0 0 (- bottom-z)] point)) lower-curves-plotted-last)
       (translate [0 0 (- target1-bottom-z)] target1-lower-curve-last-sphere)
       (translate [0 0 (- target2-bottom-z)] target2-lower-curve-last-sphere))
      (+ steps 2))
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
