(ns dactyl-keyboard.low.thumbs-low
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
            [dactyl-keyboard.cirque-circle-trackpad :refer :all]
            [dactyl-keyboard.lib.transformations :refer [rd rdx rdy rdz]]
            [dactyl-keyboard.lib.affine-transformations :refer [rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees]]
            [dactyl-keyboard.lib.geometry :refer [deg2rad radius-of-chord]]
            [dactyl-keyboard.lib.openscad.polyhedrons :refer [generate-bezier-quadratic-polyhedron-from-points-and-control-vectors]]
            ))


;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

(def thumborigin
  (map + (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
       thumb-offsets))

(def thumb-offsets-convex [-14 -42 13.5])
(def thumborigin-convex
  (map + (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
       thumb-offsets))


(def trackball-middle-translate [-6.5 6 -0.5])
(def minithumb-tip-offset [-35 -16 -6.5])
(def minithumb-tip-origin (map + thumborigin minithumb-tip-offset))


(def larger-plate-half
  (let [plate-height (/ (- sa-double-length mount-height) 3)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))]
    (union top-plate (mirror [0 0 0] top-plate))))

(defn thumb-place-convex-old ([column row shape]
                          (thumb-place-convex-old column row translate rdx rdy rdz  shape))
  ([column row translate-fn rotate-x-fn  rotate-y-fn rotate-z-fn  shape]
   (let [cap-top-height (+ plate-thickness sa-profile-key-height)

         row-radius (+ (/ (/ (+ mount-height extra-height) 2)
                          (Math/sin (/ (deg2rad thumb-alpha) 2)))
                       cap-top-height)


         column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                             (Math/sin (/ (deg2rad thumb-beta) 2)))
                          cap-top-height)
         extra-translation (if (and (= column 1) (= row 1))  [0  (/ mount-height 4) 0] [0 0 0])
         thumb-fan-radius-old 70
         thumb-well-radius-old 85
         thumb-fan-angle-old 17.5
         #_(+ (/ (/ (+ pillar-width 5) 2)
                 (Math/sin (/ (deg2rad beta) 2)))
              cap-top-height)]
     (->> shape
         ;(rotate-z-fn -45) 
         ;(rotate-z-fn  -90)
          (rotate-x-fn 4.5)
          (rotate-y-fn (/ 180 -14))
          (rotate-z-fn 7.5)


          (translate-fn [0 0   (- row-radius)])

          (rotate-y-fn (* thumb-alpha  row))

          (translate-fn [0 0 (+ row-radius)])

          (translate-fn [0 (+ thumb-fan-radius-old) 0])
          (rotate-z-fn (* thumb-fan-angle-old    row)); (* (inc row) 7))  

          (translate-fn [0 (- thumb-fan-radius-old) 0])
          (translate-fn (rotate-around-z-in-degrees (* 1 (* thumb-fan-angle-old  row))
                                                    (mapv + [(+ (* (/ (+ mount-width) 1)  row) (/  extra-width 2) (* (/ (+ mount-width extra-width) 2)  column))
                                                             (* (* (/ (+ mount-height extra-height) -4) (if (zero? row) 0 1)) column);(* (/ (+ mount-height extra-height) 2) row)
                                                             (+ (- (* plate-thickness row)) (- (* plate-thickness column)))] extra-translation)))
        ;;  (translate-fn (rotate-around-z-in-degrees (* 1 (* thumb-fan-angle  row))
        ;;                                            [(* (/ (+ mount-width extra-width) 2)  column)
        ;;                                           (* (* (/ (+ mount-height extra-height) -4) (if (zero? row) 0 1)) column)
        ;;                                             0]))
        ;;  (translate-fn (rotate-around-z-in-degrees (* 1 (* thumb-fan-angle  row)) extra-translation))



          (translate-fn [0 0 (- column-radius)])
          (rotate-x-fn (* (- thumb-beta) column))
;(rotate-x-fn fan-tilt)
          (translate-fn [0 0 (+ column-radius)])
          (translate-fn [0 0 (- thumb-well-radius-old)])

          (rotate-y-fn  thumb-well-angle)
          (translate-fn [0 0 thumb-well-radius-old])




           (rotate-x-fn 7.5)
           (rotate-y-fn -20)
           (rotate-z-fn 2.5)
        
           (translate-fn thumborigin-convex)
           (translate-fn [0 (/ (+ mount-height extra-height) -2)
                          0
                          ])
 
          ))))


(defn thumb-place-convex ([column row shape] 
                          (thumb-place-convex column row translate rdx rdy rdz  shape))
  ([column row translate-fn rotate-x-fn  rotate-y-fn rotate-z-fn  shape]
   (let [cap-top-height (+ plate-thickness sa-profile-key-height)

          row-radius (+ (/ (/ (+ mount-height extra-height) 2)
                           (Math/sin (/ (deg2rad thumb-alpha) 2)))
                        cap-top-height)


column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                    (Math/sin (/ (deg2rad thumb-beta) 2)))
                 cap-top-height)
         thumb-fan-radius (fn [column](+ (radius-of-chord (+ mount-width ) (deg2rad thumb-fan-angle)) 
                                         (+ 
                                          ;(* (+ mount-height extra-height) column)
                                          (/ (+ mount-height extra-height) 2))))
         thumb-well-radius (- (radius-of-chord (+ mount-width) (deg2rad (+ thumb-well-angle ))) (+ cap-top-height))
         max-thumb-rows 3
         extra-translation (if (and (= column 1) (= row 1))  [0  (/ mount-height 4) 0] [0 0 0])
         #_(+ (/ (/ (+ pillar-width 5) 2)
                 (Math/sin (/ (deg2rad beta) 2)))
              cap-top-height)]
    (->> shape 
          
         (translate-fn [0 (+ (thumb-fan-radius column)) 0])

(rotate-z-fn (* thumb-fan-angle (dec row)))
         (translate-fn [0 0 (- (* (abs (dec row)) (/ plate-thickness 1)))])

(translate-fn [0 (- (thumb-fan-radius column)) 0])
         (translate-fn [0
                        0 (- cap-top-height)])

(rotate-y-fn (* (+  20 ) (dec row)))
(translate-fn [0 0 cap-top-height])
         (translate-fn [0 0 (- column-radius)])
(rotate-x-fn (* (- thumb-beta) column))
(translate-fn [0 0 column-radius])
         
         (rotate-x-fn 7.5)
           (rotate-y-fn (/ 180 -14))
           (rotate-z-fn 30)
        
          (translate-fn [(- (+ (* (/ max-thumb-rows 2) (+ mount-width extra-width)) (/ mount-width 2) )) (/ mount-height -2) (/ cap-top-height -2)]) 
        (translate-fn thumborigin-convex)
           (translate-fn [extra-width (/ (+ mount-height extra-height) -2)
                            (* column (- plate-thickness))
                           ]) 

         ))))


(def thumb-tr-standard-rotation-values {:x 14 :y -15 :z 10})
(def thumb-tr-convex-rotation-values {:x 14 :y -15 :z 10})
(def thumb-tr-rotation-values (case thumb-curvature-type
                                :thumb-curvature-standard thumb-tr-standard-rotation-values
                                :thumb-curvature-convex thumb-tr-convex-rotation-values))

(defn thumb-tr-rotate ([shape] (thumb-tr-rotate rdx rdy rdz shape))
  ([ rotate-x-fn rotate-y-fn rotate-z-fn shape]
  (->> shape  
       (rotate-x-fn  (thumb-tr-rotation-values :x))
   (rotate-y-fn (thumb-tr-rotation-values :y))
        (rotate-z-fn  (thumb-tr-rotation-values :z)) )
  )
)
(def thumb-tr-translation-values [-15 -10  5])

(defn thumb-tr-place-standard ([shape] (thumb-tr-place-standard translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
         (thumb-tr-rotate rotate-x-fn rotate-y-fn rotate-z-fn) 
        (translate-fn thumborigin)
       (translate-fn [-15 -10 5])))) ; original 1.5u  (translate [-12 -16 3])

(def thumb-tr-place (case thumb-curvature-type
                      :thumb-curvature-standard thumb-tr-place-standard
                      :thumb-curvature-convex (partial thumb-place-convex 0 0)))
(defn thumb-tr-position [position] (thumb-tr-place (partial map +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees position))
(def tl-minithumb-loc (map + minithumb-tip-offset (if cirque-TM040040-mount-thumb trackball-middle-translate [0 0 0])))

(def thumb-tl-standard-rotation-values {:x 10 :y -23 :z 25})
(def thumb-tl-convex-rotation-values {:x (+ (thumb-tr-convex-rotation-values :x) -4) 
                                      :y (+ (thumb-tr-convex-rotation-values :y) 2.5)
                                      :z (+ (thumb-tr-convex-rotation-values :z) 15)})
(def thumb-tl-rotation-values (case thumb-curvature-type
                                :thumb-curvature-standard thumb-tl-standard-rotation-values
                                :thumb-curvature-convex thumb-tl-convex-rotation-values))

(defn thumb-tl-rotate ([shape] (thumb-tl-rotate  rdx rdy rdz shape))
  ([ rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (rotate-x-fn  (thumb-tl-rotation-values :x)) 
        (rotate-y-fn (thumb-tl-rotation-values :y)) 
        (rotate-z-fn  (thumb-tl-rotation-values :z)) ; original 10
   )))

(def thumb-tl-standard-translation-values [-35 -16 -2])
(def thumb-tl-convex-translation-values
  (map + thumb-tr-translation-values  
       (apply thumb-tl-rotate (rotation-transformations [(- (+ mount-width (* 2 extra-width))) (* extra-height 2) 0]) )))
(def thumb-tl-translation-values (case thumb-curvature-type
                                   :thumb-curvature-standard thumb-tl-standard-translation-values
                                   :thumb-curvature-convex thumb-tl-convex-translation-values))

(defn thumb-tl-place-standard ([shape] (thumb-tl-place-standard translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (thumb-tl-rotate rotate-x-fn rotate-y-fn rotate-z-fn)
       (translate-fn thumborigin)
       (translate-fn thumb-tl-translation-values)))) ; original 1.5u (translate [-32 -15 -2])))

(def thumb-tl-place (case thumb-curvature-type
                       :thumb-curvature-standard  thumb-tl-place-standard
                       :thumb-curvature-convex  (partial thumb-place-convex 0 1)))

(def mr-minithumb-loc (map + [-23.5 -36.5 -2] (if cirque-TM040040-mount-thumb trackball-middle-translate [0 0 0])))


(def thumb-mr-standard-rotation-values {:x 10 :y -23 :z 25})
(def thumb-mr-convex-rotation-values {:x (+ (thumb-tr-convex-rotation-values :x) -4)
                                      :y (+ (thumb-tr-convex-rotation-values :y) 2.5)
                                      :z (+ (thumb-tr-convex-rotation-values :z) 10)})
(def thumb-mr-rotation-values (case thumb-curvature-type
                                :thumb-curvature-standard  thumb-mr-standard-rotation-values
                                :thumb-curvature-convex  thumb-mr-convex-rotation-values))

(defn thumb-mr-rotate ([shape] (thumb-mr-rotate rdx rdy rdz shape))
  ([rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (rotate-x-fn  (thumb-mr-rotation-values :x))
        (rotate-y-fn (thumb-mr-rotation-values :y))
        (rotate-z-fn  (thumb-mr-rotation-values :z)))))

(def thumb-mr-standard-translation-values [-23 -34 -6])
(def thumb-mr-convex-translation-values (map + thumb-tr-translation-values
                                             (apply thumb-mr-rotate (rotation-transformations [(- (+ mount-width (* 1.5 extra-width))) 
                                                                                               (+ (- mount-height) (* extra-height -2)) 0]))))

(def thumb-mr-translation-values (case thumb-curvature-type 
                                            :thumb-curvature-standard thumb-mr-standard-translation-values
                                            :thumb-curvature-convex thumb-mr-convex-translation-values))


(defn thumb-mr-rotate-reverse ([shape] (thumb-mr-rotate rdx rdy rdz shape))
  ([rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (rotate-x-fn  -10)
        (rotate-y-fn 23)
        (rotate-z-fn  -25))))


(defn thumb-mr-place-standard ([shape] (thumb-mr-place-standard translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
       (thumb-mr-rotate rotate-x-fn rotate-y-fn rotate-z-fn)
       (translate-fn thumborigin)
       (translate-fn thumb-mr-translation-values))))

(def thumb-mr-place (case thumb-curvature-type
                      :thumb-curvature-standard thumb-mr-place-standard
                      :thumb-curvature-convex (partial thumb-place-convex 1 1)))
(defn thumb-mr-position [position] (thumb-mr-place (partial map +) rotate-around-x-in-degrees rotate-around-y-in-degrees rotate-around-z-in-degrees position))
(def br-minithumb-loc (map + [-39 -43 -16] (if (= thumb-curvature-type :thumb-curvature-convex) [-4.5 2.5 -3] [0 0 0]))) 

(def thumb-br-standard-rotation-values {:x 6 :y -34 :z 35})
(def thumb-br-convex-rotation-values {:x (+ (thumb-tr-convex-rotation-values :x) -8)
                                      :y (+ (thumb-tr-convex-rotation-values :y) 5)
                                      :z (+ (thumb-tr-convex-rotation-values :z) 20)})
(def thumb-br-rotation-values (case thumb-curvature-type
                                :thumb-curvature-standard  thumb-br-standard-rotation-values
                                :thumb-curvature-convex  thumb-br-convex-rotation-values))



(defn thumb-br-rotate-standard ([shape] (thumb-br-rotate-standard rdx rdy rdz shape))
  ([rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (rotate-x-fn  6)
        (rotate-y-fn -34)
        (rotate-z-fn  35))))
(defn thumb-br-place-standard  ([shape] (thumb-br-place-standard translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
  (->> shape
       (rotate-x-fn   6) 
        (rotate-y-fn -34) 
        (rotate-z-fn  35) 
       (translate-fn thumborigin)
       (translate-fn br-minithumb-loc))))

(defn thumb-br-rotate-convex ([shape] (thumb-br-rotate-convex rdx rdy rdz shape))
  ([rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (rotate-x-fn  (thumb-br-convex-rotation-values :x))
        (rotate-y-fn (thumb-br-convex-rotation-values :y))
        (rotate-z-fn  (thumb-br-convex-rotation-values :z)))))

(def thumb-br-convex-translation-values
  (map + thumb-tr-translation-values 
       (apply thumb-br-rotate-convex 
              (rotation-transformations [(-  (+ (* 2 mount-width) (* 3 extra-width))) (+  (- mount-height) ) 0]))))

(defn thumb-br-place-convex  ([shape] (thumb-br-place-convex translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (thumb-br-rotate-convex rotate-x-fn rotate-y-fn rotate-z-fn)
        (translate-fn thumborigin)
        (translate-fn thumb-br-convex-translation-values))))

(def thumb-br-rotate (case thumb-curvature-type
                       :thumb-curvature-standard thumb-br-rotate-standard
                       :thumb-curvature-convex thumb-br-rotate-convex))
(def thumb-br-place (case thumb-curvature-type
                      :thumb-curvature-standard thumb-br-place-standard
                      :thumb-curvature-convex (partial thumb-place-convex 1 2)))

(def bl-minithumb-loc (map + [-51 -25 -11.5] (if cirque-TM040040-mount-thumb [0 0 0] [0 0 0])))
(def bl-minithumb-loc-convex (map + [-52.5 -25.5 -11.5]  (if cirque-TM040040-mount-thumb [0 0 0] [0 0 0])))
;; (def bl-minithumb-loc (case thumb-curvature-type
;;                        :thumb-curvature-standard thumb-bl-rotate-standard
;;                        :thumb-curvature-convex thumb-bl-rotate-convex))

(defn thumb-bl-rotate-standard ([shape] (thumb-bl-rotate-standard rdx rdy rdz shape))
  ([rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (rotate-x-fn  6)
        (rotate-y-fn -32)
        (rotate-z-fn  35))))
(defn thumb-bl-place-standard  ([shape] (thumb-bl-place-standard translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
  (->> shape
        (thumb-bl-rotate-standard rotate-x-fn rotate-y-fn rotate-z-fn)
       (translate-fn thumborigin)
       (translate-fn bl-minithumb-loc)))) ;        (translate [-51 -25 -12])))

(def thumb-bl-rotate-convex-values 
  {:x (+ (thumb-tr-convex-rotation-values :x) -8)
   :y (+ (thumb-tr-convex-rotation-values :y) 5)
   :z (+ (thumb-tr-convex-rotation-values :z) 20)}
  )

(defn thumb-bl-rotate-convex ([shape] (thumb-bl-rotate-convex rdx rdy rdz shape))
  ([rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (rotate-x-fn  (thumb-bl-rotate-convex-values :x))
        (rotate-y-fn (thumb-bl-rotate-convex-values :y))
        (rotate-z-fn  (thumb-bl-rotate-convex-values :z)))))

(def thumb-bl-rotate (case thumb-curvature-type
                       :thumb-curvature-standard thumb-bl-rotate-standard
                       :thumb-curvature-convex thumb-bl-rotate-convex))

(def thumb-bl-convex-translation-values 
  (map + thumb-tr-translation-values
       (apply thumb-bl-rotate (rotation-transformations [(-  (+ (* 2 mount-width) (* 3 extra-width)))  (* extra-height 3) 0]))))
(defn thumb-bl-place-convex ([shape] (thumb-bl-place-convex translate rdx rdy rdz shape))
  ([translate-fn rotate-x-fn rotate-y-fn rotate-z-fn shape]
   (->> shape
        (thumb-bl-rotate-convex rotate-x-fn rotate-y-fn rotate-z-fn)
        (translate-fn thumborigin)
        (translate-fn thumb-bl-convex-translation-values))))


(def thumb-bl-place (case thumb-curvature-type
                             :thumb-curvature-standard thumb-bl-place-standard
                             :thumb-curvature-convex (partial thumb-place-convex 0 2)))
;defn thumb-b1-place-multmatrix [shape]
; (multmatrix 
;   (mmul
;    
;    
;     
;     
;     (multmatrix-translate thumborigin)
;     (multmatrix-translate bl-minithumb-loc)
;    (multmatrix-z-rot 35)
;    (multmatrix-y-rot -32)
;    (multmatrix-x-rot 6)
;    )
;       shape))


(defn thumb-1x-layout [shape]
  (union
   (thumb-mr-place shape)
   (thumb-br-place shape)
   (thumb-tl-place shape)
   (thumb-bl-place shape)))

(defn thumb-15x-layout [shape]
  (union
   (thumb-tr-place shape)))

(def larger-plate
  (let [plate-height (- (/ (- sa-double-length mount-height) 3) 0.5)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))]
    (union top-plate (mirror [0 1 0] top-plate))))

(def thumbcaps
  (union
   (thumb-1x-layout (sa-cap 1))
   (thumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1)))))

(def dsa-thumbcaps 
 (union
  (thumb-1x-layout dsa-cap)
  (thumb-15x-layout (rotate (/ π 2) [0 0 1] dsa-cap))))

(def thumbcaps-fill
  (union
   (thumb-1x-layout keyhole-fill)
   (thumb-15x-layout (rotate (/ π 2) [0 0 1] keyhole-fill))))

(def thumb
  (union
   (thumb-1x-layout single-plate)
   (thumb-15x-layout single-plate)
  ; (thumb-15x-layout larger-plate)
   ))

(def thumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
(def thumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
(def thumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def thumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post))

(def thumb-connectors-polyhedron
  (let [steps 20
        first-colmun-third-row-web-post-bl-top (key-position 0 2 (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness 2)]))
        first-colmun-third-row-web-post-bl-bottom (key-position 0 2 (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness -2)]))
        first-colmun-third-row-web-post-br-top (key-position 0 2 (mapv + web-post-translation-vector web-post-br-translation-vector [0 0 (/ web-thickness 2)]))
        first-colmun-third-row-web-post-br-bottom (key-position 0 2 (mapv + web-post-translation-vector web-post-br-translation-vector [0 0 (/ web-thickness -2)]))
        second-colmun-third-row-web-post-bl-top (key-position 1 2 (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness 2)]))
        second-colmun-third-row-web-post-bl-bottom (key-position 1 2 (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness -2)])) 
        second-colmun-third-row-web-post-br-top (key-position 1 2 (mapv + web-post-translation-vector web-post-br-translation-vector [0 0 (/ web-thickness 2)]))
second-colmun-third-row-web-post-br-bottom (key-position 1 2 (mapv + web-post-translation-vector web-post-br-translation-vector [0 0 (/ web-thickness -2)]))
        thumb-tl-tr-web-post-top (transform-position (partial thumb-tl-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-tl-tr-web-post-bottom (transform-position (partial thumb-tl-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        thumb-tr-tl-web-post-top (transform-position (partial thumb-tr-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-tr-tl-web-post-bottom (transform-position (partial thumb-tr-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        thumb-tr-tr-web-post-top (transform-position (partial thumb-tr-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
thumb-tr-tr-web-post-bottom (transform-position (partial thumb-tr-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        
        first-colmun-third-to-thumb-tl-and-tr (generate-bezier-quadratic-polyhedron-from-points-and-control-vectors
                                              first-colmun-third-row-web-post-br-top thumb-tr-tl-web-post-top
                                             first-colmun-third-row-web-post-bl-top thumb-tl-tr-web-post-top 
                                             first-colmun-third-row-web-post-br-bottom  thumb-tr-tl-web-post-bottom
                                             first-colmun-third-row-web-post-bl-bottom thumb-tl-tr-web-post-bottom
                                             steps
                                             {:outside-upper-control-point-vector [0 0 -1] :outside-lower-control-point-vector [0 0 0]
                                              :inside-upper-control-point-vector [0 0 0] :inside-lower-control-point-vector [0 0 0]})
        first-colmun-third-row-br-and-second-column-third-row-br-to-thumb-tr (generate-bezier-quadratic-polyhedron-from-points-and-control-vectors
                                               
                                                                              second-colmun-third-row-web-post-br-top thumb-tr-tr-web-post-top 
                                                                              first-colmun-third-row-web-post-br-top thumb-tr-tl-web-post-top 
                                                                              second-colmun-third-row-web-post-br-bottom thumb-tr-tr-web-post-bottom
                                                                              first-colmun-third-row-web-post-br-bottom  thumb-tr-tl-web-post-bottom
                                               
                                               steps
                                               {:outside-upper-control-point-vector [0 0 -1] :outside-lower-control-point-vector [0 0 -1]
                                                :inside-upper-control-point-vector [0 0 0] :inside-lower-control-point-vector [0 0 0]})
        
        ] 
    (union
     first-colmun-third-to-thumb-tl-and-tr
     first-colmun-third-row-br-and-second-column-third-row-br-to-thumb-tr
     )
    )
  )

(def thumb-connectors
  (union
  ; thumb-connectors-polyhedron
   (triangle-hulls    ; top two
    (thumb-tl-place web-post-tr)
    (thumb-tl-place web-post-br)
    (thumb-tr-place web-post-tl)
    (thumb-tr-place web-post-bl))
   (triangle-hulls    ; bottom two
    (thumb-br-place web-post-tr)
    (thumb-br-place web-post-br)
    (thumb-mr-place web-post-tl)
    (thumb-mr-place web-post-bl))
  ; (triangle-hulls
  ;  (thumb-mr-place web-post-tr)
  ;  (thumb-mr-place web-post-br)
  ;  (thumb-tr-place thumb-post-br))
   (triangle-hulls    ; between top row and bottom row
    (thumb-br-place web-post-tl)
    (thumb-bl-place web-post-bl)
    (thumb-br-place web-post-tr)
    (thumb-bl-place web-post-br)
    (thumb-mr-place web-post-tl)
    (thumb-tl-place web-post-bl)
    (thumb-mr-place web-post-tr)
    (thumb-tl-place web-post-br)
    (thumb-tr-place web-post-bl)
    (thumb-mr-place web-post-tr)
    (thumb-tr-place web-post-br))
   (triangle-hulls    ; top two to the middle two, starting on the left
    (thumb-tl-place web-post-tl)
    (thumb-bl-place web-post-tr)
    (thumb-tl-place web-post-bl)
    (thumb-bl-place web-post-br)
    (thumb-mr-place web-post-tr)
    (thumb-tl-place web-post-bl)
    (thumb-tl-place web-post-br)
    (thumb-mr-place web-post-tr))
;;    (triangle-hulls
;;     (thumb-tl-place web-post-tl)
;; (key-place 0 cornerrow web-post-bl)
;; (thumb-tl-place web-post-tr)
;;     )
; ( -#(triangle-hulls
;   (key-place 0 cornerrow web-post-br)
;   (thumb-tr-place web-post-tr)
;(key-place 1 cornerrow web-post-br)
;   ))
   

   
  ;;  (hull
  ;;   (key-place 0 cornerrow web-post-bl)
  ;;   (key-place 0 cornerrow web-post-br)
  ;;   (thumb-tr-place web-post-tl)
  ;;   )

;;  (hull
;;   (key-place 0 cornerrow web-post-bl)
;; (thumb-tr-place web-post-tl)
;;  (thumb-tl-place web-post-tr)
;;  )


   
   
  
;;    (triangle-hulls

;;     (thumb-tr-place (translate [0 1 0] web-post-tr))
;; (thumb-tr-place  web-post-tr)
;; (thumb-tr-place  web-post-tl)
;;    )
;;    (triangle-hulls

;;     (thumb-tr-place (translate [0 1 0] web-post-tr))
;;     (thumb-tr-place  web-post-tl)
;;     (key-place 0 cornerrow web-post-br))
;;    (triangle-hulls
   
;;     (thumb-tr-place (translate [ 0 1 0]web-post-tr))
;;     (key-place 0 cornerrow web-post-br)
;;     (key-place 1 cornerrow web-post-br)
    

;;     (thumb-tr-place (translate [0 1 0] web-post-tr))
;; (thumb-tr-place  web-post-tr)
;; (key-place 1 cornerrow web-post-br)
;;     )
   
;;    (triangle-hulls

;;     (thumb-tr-place (translate [0 1 0] web-post-tr))
;; (key-place 2 lastrow web-post-bl)
;; (key-place 1 cornerrow web-post-br)
;;    )

;;    (triangle-hulls

;;     (thumb-tr-place (translate [0 1 0] web-post-tr))
;;     (key-place 2 lastrow web-post-bl)
;;     (thumb-tr-place  web-post-tr))

   
  ; (triangle-hulls
  ;  (key-place 0 cornerrow web-post-br)
  ;  (key-place 1 cornerrow web-post-bl)
  ;  (thumb-tr-place web-post-tr)
  ;  )
  ;  (triangle-hulls
  ;   (key-place 1 cornerrow web-post-br)
  ;   (key-place 1 cornerrow web-post-bl)
  ;   (thumb-tr-place web-post-tr))
   (when (or (= last-row-style :last-row-middle-and-fourth-keys-only) (= last-row-style :all-columns))
(hull
 (key-place 2 lastrow web-post-bl)
 (thumb-tr-place web-post-tr)
 (thumb-tr-place web-post-br))
     
     (hull
 (thumb-tr-place web-post-br)
 (key-place 2 lastrow web-post-bl)
 (key-place 3 lastrow web-post-bl))

(hull
 (key-place 2 lastrow web-post-br)
 (key-place 2 lastrow web-post-bl)
 (key-place 3 lastrow web-post-bl))
  
     (triangle-hulls    ; top two to the main keyboard, starting on the left


    ;(thumb-tr-place web-post-tl)
    ;(key-place 1 cornerrow web-post-bl)




    (key-place 2 lastrow web-post-br)
    (key-place 3 lastrow web-post-bl)
    (key-place 2 lastrow web-post-tr)
    (key-place 3 lastrow web-post-tl)
    (key-place 3 cornerrow web-post-bl)
    (key-place 3 lastrow web-post-tr)
    (key-place 3 cornerrow web-post-br)
    (key-place 4 cornerrow web-post-bl))
(triangle-hulls

 (key-place 1 cornerrow web-post-bl)
 (key-place 0 cornerrow web-post-br)
 (key-place 1 cornerrow web-post-br))
     
     (triangle-hulls
    (key-place 1 cornerrow web-post-br)
    (key-place 2 lastrow web-post-tl)
    (key-place 2 lastrow web-post-bl)
    )
   (triangle-hulls
    (key-place 1 cornerrow web-post-br)
    (key-place 2 lastrow web-post-tl)
    (key-place 2 cornerrow web-post-bl)
    (key-place 2 lastrow web-post-tr)
    (key-place 2 cornerrow web-post-br)
    (key-place 3 cornerrow web-post-bl))
   (triangle-hulls
    (key-place 3 lastrow web-post-tr)
    (key-place 3 lastrow web-post-br)
    (key-place 3 lastrow web-post-tr)
    (key-place 4 cornerrow web-post-bl)))))

;;;;;;;;

(def tbjs-key-diameter 70)
(def tbjs-translation-offset [0 0 2])
(def tbjs-rotation-offset [0 0 2])
 (def tbjs-uwidth 1.2) ; size for inner key near trackball
 (def tbjs-uheight 1.2) ;   size for inner key near trackball
(def post-adj-tbjs 0)

(def tbjs-key-translation-offsets [[0.0, 0.0, -8]
                                   [0.0, 0.0, -8]
                                   [0.0, 0.0, -8]
                                   [0.0, 0.0, -8]])

(def tbjs-key-rotation-offsets [[0.0, 0.0, 0.0]
                                [0.0, 0.0, 0.0]
                                [0.0, 0.0, 0.0]
                                [0.0, 0.0, 0.0]])


 ;Changes size based on key diameter around ball, shifting off of the top left cluster key.
(def shift  [(+ (/ (* -0.9 tbjs-key-diameter) 27 -42)) (+ (/ (* -0.1 tbjs-key-diameter) 2) 3 -25) 5])

(def tests
  (let [pos (into [] thumborigin) i 0]
    (assoc pos i
           (+ (get pos i)
              (get shift i)
              (get tbjs-translation-offset i)))))

(defn lll [index item ]
  (let [pos (into [] thumborigin)] 
    (+ item
     (get pos index) 
    (get shift index)
    (get tbjs-translation-offset index)))
  )
(def tbjs-thumb-position-translation
  (let [pos (into [] thumborigin) iter  (into [] (range (count pos)))] 
    tests 
    (map-indexed lll [0 0 0]
      )))

(defn tbjs-thumb-position-rotation-calculator [index item]
   (let [rot [10 -15 5]] 
      (+ item (get rot index) (get tbjs-rotation-offset index))
  ))
(def tbjs-thumb-position-rotation
 (map-indexed tbjs-thumb-position-rotation-calculator [0 0 0] )
  )

(defn tbjs-place [shape]
  (->> shape
       (rotate tbjs-thumb-position-rotation)
       (translate tbjs-thumb-position-translation)
                  ))

(defn tbjs-thumb-place [index rot1 rot2 shape]
  (let [t-off (get tbjs-key-translation-offsets index)]
    (->> shape
         (rotate [0 0 rot1])
         (rotate (get tbjs-key-rotation-offsets index))
         (translate [(get t-off 0)
           (+ (get t-off 1) (/ tbjs-key-diameter 2))
           (get t-off 2)])
         (rotate [0 0 rot2])
          (tbjs-place)
         )
  )
)

(defn tbjs-thumb-tl-place [shape] 
  (tbjs-thumb-place 0 0 -80 shape)
  )

(defn tbjs-thumb-mr-place [shape]
  (tbjs-thumb-place 1 0 -130 shape)
  )

(defn tbjs-thumb-br-place [shape]
(tbjs-thumb-place 2 180 -180 shape)
  )

(defn tbjs-thumb-bl-place [shape]
  (tbjs-thumb-place 3 180 -230 shape)
  )

(defn tbjs-thumb-1x-layout [shape]
  (union
   (tbjs-thumb-tl-place shape)
   (tbjs-thumb-mr-place shape)
   (tbjs-thumb-br-place shape)
   (tbjs-thumb-bl-place shape)
   ))

(def tbjs-thumbcaps
   (tbjs-thumb-1x-layout (sa-cap 1))
  )

(def tbjs-thumbcaps-fill
  (tbjs-thumb-1x-layout keyhole-fill)
  )

(def tbjs-thumb
  (union (tbjs-thumb-1x-layout single-plate)))

(defn  adjustable-plate-size [Usize]
    (/ (- (* Usize sa-length) mount-height)  2))

(def tbjs-thumb-post-tr
  (translate [(- (+(/ mount-width  2) (adjustable-plate-size tbjs-uwidth)) post-adj-tbjs)
              (+ (/ mount-height 2) (adjustable-plate-size tbjs-uheight) (- post-adj-tbjs))
              0]
             web-post)
  )

(def tbjs-thumb-post-tl
  (translate [(+ (- (- (/ mount-width  2)) (adjustable-plate-size tbjs-uwidth)) post-adj-tbjs)
              (+ (- (/ mount-height 2)) (adjustable-plate-size tbjs-uheight) (- post-adj-tbjs))
              0]
             web-post))

(def tbjs-thumb-post-bl
  (translate [(+ (- (- (/ mount-width  2)) (adjustable-plate-size tbjs-uwidth)) post-adj-tbjs)
              (+ (/ mount-height 2) (adjustable-plate-size tbjs-uheight) (- post-adj-tbjs))
              0]
             web-post))

(def tbjs-thumb-post-br
  (translate [(- (+ (/ mount-width  2) (adjustable-plate-size tbjs-uwidth)) post-adj-tbjs)
              (+ (/ mount-height 2) (adjustable-plate-size tbjs-uheight) (- post-adj-tbjs))
              0]
             web-post))
(def tbjs-post-radius (+ (/ cirque-circle-trackpad-TM040040-curved-overlay-diameter 2) cirque-circle-trackpad-TM040040-curved-overlay-gap))
(def tbjs-post-radius-adjusted (- tbjs-post-radius post-adj-tbjs))

(defn tbjs-post-create[xadj yadj]
  (translate [(* xadj tbjs-post-radius-adjusted) (* yadj tbjs-post-radius-adjusted) 0] web-post)
  )


(def tbjs-post-r
 (tbjs-post-create 1 0)
  )

(def tbjs-post-tr
  (tbjs-post-create 0.5 0.866)
  )

(def tbjs-post-tl
  (tbjs-post-create -0.5 0.866))

(def tbjs-post-l
  (tbjs-post-create -1 0))

(def tbjs-post-bl
  (tbjs-post-create -0.5 -0.866))

(def tbjs-post-br
  (tbjs-post-create 0.5 -0.866))



(def tbjs-thumb-connectors
  (union 
   ( triangle-hulls
    (tbjs-place tbjs-post-l)
    (tbjs-thumb-bl-place tbjs-thumb-post-tl)
    (tbjs-place tbjs-post-bl)
    (tbjs-thumb-bl-place tbjs-thumb-post-tr)
    (tbjs-thumb-br-place tbjs-thumb-post-bl)
    (tbjs-place tbjs-post-bl)
    (tbjs-thumb-br-place tbjs-thumb-post-tr)
    (tbjs-place tbjs-thumb-post-br)
    (tbjs-thumb-br-place tbjs-thumb-post-tr)
    (tbjs-place tbjs-post-br)
    (tbjs-thumb-mr-place tbjs-thumb-post-br)
    (tbjs-place tbjs-post-r)
    (tbjs-thumb-mr-place tbjs-thumb-post-bl)
    (tbjs-thumb-tl-place tbjs-thumb-post-br)
    (tbjs-place tbjs-post-r)
    (tbjs-thumb-tl-place tbjs-thumb-post-bl)
    (tbjs-place tbjs-post-tr)
    (key-place 0 cornerrow web-post-bl )
    (tbjs-place tbjs-post-tl)
  )

   ;bottom left
  (triangle-hulls
   (tbjs-thumb-bl-place tbjs-thumb-post-tr)
    (tbjs-thumb-br-place tbjs-thumb-post-tl)
    (tbjs-thumb-bl-place tbjs-thumb-post-br)
    (tbjs-thumb-br-place tbjs-thumb-post-bl)
   )
  
   ;bottom right
   (triangle-hulls
    (tbjs-thumb-br-place tbjs-thumb-post-tr)
    (tbjs-thumb-mr-place tbjs-thumb-post-br)
    (tbjs-thumb-br-place tbjs-thumb-post-br)
    (tbjs-thumb-mr-place tbjs-thumb-post-tr))
  
   ;bottom right
  (triangle-hulls
   (tbjs-thumb-mr-place tbjs-thumb-post-bl)
   (tbjs-thumb-tl-place tbjs-thumb-post-br)
   (tbjs-thumb-mr-place tbjs-thumb-post-tl)
   (tbjs-thumb-tl-place tbjs-thumb-post-tr))

  )
  )

;;;;;;;;;;;;;;;;
;; cf Thumb ;;
;;;;;;;;;;;;;;;;

(defn cfthumb-tl-place [shape]
  (->> shape
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad -24) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-13 -9.8 4])))
(defn cfthumb-tr-place [shape]
  (->> shape
       (rotate (deg2rad  6) [1 0 0])
       (rotate (deg2rad -24) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-7.5 -29.5 0])))
(defn cfthumb-ml-place [shape]
  (->> shape
       (rotate (deg2rad  8) [1 0 0])
       (rotate (deg2rad -31) [0 1 0])
       (rotate (deg2rad  14) [0 0 1])
       (translate thumborigin)
       (translate [-30.5 -17 -6])))
(defn cfthumb-mr-place [shape]
  (->> shape
       (rotate (deg2rad  4) [1 0 0])
       (rotate (deg2rad -31) [0 1 0])
       (rotate (deg2rad  14) [0 0 1])
       (translate thumborigin)
       (translate [-22.2 -41 -10.3])))
(defn cfthumb-br-place [shape]
  (->> shape
       (rotate (deg2rad   2) [1 0 0])
       (rotate (deg2rad -37) [0 1 0])
       (rotate (deg2rad  18) [0 0 1])
       (translate thumborigin)
       (translate [-37 -46.4 -22])))
(defn cfthumb-bl-place [shape]
  (->> shape
       (rotate (deg2rad   6) [1 0 0])
       (rotate (deg2rad -37) [0 1 0])
       (rotate (deg2rad  18) [0 0 1])
       (translate thumborigin)
       (translate [-47 -23 -19])))

(defn cfthumb-1x-layout [shape]
  (union
   (cfthumb-tr-place (rotate (/ π 2) [0 0 0] shape))
   (cfthumb-mr-place shape)
   (cfthumb-br-place shape)
   (cfthumb-tl-place (rotate (/ π 2) [0 0 0] shape))))

(defn cfthumb-15x-layout [shape]
  (union
   (cfthumb-bl-place shape)
   (cfthumb-ml-place shape)))

(def cfthumbcaps
  (union
   (cfthumb-1x-layout (sa-cap 1))
   (cfthumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1.5)))))

(def cfthumbcaps-fill
  (union
   (cfthumb-1x-layout keyhole-fill)
   (cfthumb-15x-layout (rotate (/ π 2) [0 0 1] keyhole-fill))))

(def cfthumb
  (union
   (cfthumb-1x-layout single-plate)
   (cfthumb-15x-layout larger-plate-half)
   (cfthumb-15x-layout single-plate)))

(def cfthumb-connectors
  (union
   (triangle-hulls    ; top two
    (cfthumb-tl-place web-post-tl)
    (cfthumb-tl-place web-post-bl)
    (cfthumb-ml-place thumb-post-tr)
    (cfthumb-ml-place web-post-br))
   (triangle-hulls
    (cfthumb-ml-place thumb-post-tl)
    (cfthumb-ml-place web-post-bl)
    (cfthumb-bl-place thumb-post-tr)
    (cfthumb-bl-place web-post-br))
   (triangle-hulls    ; bottom two
    (cfthumb-br-place web-post-tr)
    (cfthumb-br-place web-post-br)
    (cfthumb-mr-place web-post-tl)
    (cfthumb-mr-place web-post-bl))
   (triangle-hulls
    (cfthumb-mr-place web-post-tr)
    (cfthumb-mr-place web-post-br)
    (cfthumb-tr-place web-post-tl)
    (cfthumb-tr-place web-post-bl))
   (triangle-hulls
    (cfthumb-tr-place web-post-br)
    (cfthumb-tr-place web-post-bl)
    (cfthumb-mr-place web-post-br))
   (triangle-hulls    ; between top row and bottom row
    (cfthumb-br-place web-post-tl)
    (cfthumb-bl-place web-post-bl)
    (cfthumb-br-place web-post-tr)
    (cfthumb-bl-place web-post-br)
    (cfthumb-mr-place web-post-tl)
    (cfthumb-ml-place web-post-bl)
    (cfthumb-mr-place web-post-tr)
    (cfthumb-ml-place web-post-br)
    (cfthumb-tr-place web-post-tl)
    (cfthumb-tl-place web-post-bl)
    (cfthumb-tr-place web-post-tr)
    (cfthumb-tl-place web-post-br))
   (triangle-hulls    ; top two to the main keyboard, starting on the left
    (cfthumb-ml-place thumb-post-tl)
    (key-place (+ innercol-offset 0) cornerrow web-post-bl)
    (cfthumb-ml-place thumb-post-tr)
    (key-place (+ innercol-offset 0) cornerrow web-post-br)
    (cfthumb-tl-place web-post-tl)
    (key-place (+ innercol-offset 1) cornerrow web-post-bl)
    (cfthumb-tl-place web-post-tr)
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (cfthumb-tl-place web-post-tr)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (cfthumb-tl-place web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-bl)
    (cfthumb-tl-place web-post-br)
    (cfthumb-tr-place web-post-tr))
   (triangle-hulls
    (key-place (+ innercol-offset 3) lastrow web-post-tr)
    (key-place (+ innercol-offset 3) cornerrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-tl)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl))
   (triangle-hulls
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 2) lastrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-tl)
    (key-place (+ innercol-offset 3) lastrow web-post-bl))
   (triangle-hulls
    (cfthumb-tr-place web-post-br)
    (cfthumb-tr-place web-post-tr)
    (key-place (+ innercol-offset 3) lastrow web-post-bl))
   (triangle-hulls
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) cornerrow web-post-bl)
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 2) cornerrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-tl)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl))
   (if extra-row
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) lastrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl)))
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))))))


;;;;;;;;;
(defn trackpad-thumb-tr-place [shape]
  (->> shape
       (rotate (deg2rad  14) [1 0 0])
       (rotate (deg2rad -15) [0 1 0])
       (rotate (deg2rad  10) [0 0 1]) ; original 10
       (translate thumborigin)
       (translate [-15 -10 5])))

(defn trackpad-thumb-tr-place [shape]
  (->> shape
       (rd 7 -6 11)
       (translate thumborigin)
       (translate [-17.2 -7 5])))

(defn trackpad-thumb-1x-layout [shape]
  (union
   (trackpad-thumb-tr-place shape)
    (thumb-mr-place shape)
   (thumb-br-place shape)
  ))

(def trackpad-thumb
  (union
   (trackpad-thumb-1x-layout single-plate)
  
  ; (thumb-15x-layout larger-plate)
   ))


(when ( = thumb-style "default")
 (def thumb-type thumb)
 (def thumb-connector-type thumb-connectors)
 (def thumbcaps-type thumbcaps)
 (def thumbcaps-fill-type thumbcaps-fill))

(when (= thumb-style "orbyl")
  (def thumb-type tbjs-thumb)
  (def thumb-connector-type tbjs-thumb-connectors)
  (def thumbcaps-type tbjs-thumbcaps)
  (def thumbcaps-fill-type tbjs-thumbcaps-fill)
  )

(when (= thumb-style "cf")
  (def thumb-type cfthumb)
  (def thumb-connector-type cfthumb-connectors)
  (def thumbcaps-type cfthumbcaps)
  (def thumbcaps-fill-type cfthumbcaps-fill))

(when (= thumb-style "trackpad")
  (def thumb-type trackpad-thumb)
  (def thumb-connector-type thumb)
  (def thumbcaps-type thumbcaps)
  (def thumbcaps-fill-type thumbcaps-fill)
  )