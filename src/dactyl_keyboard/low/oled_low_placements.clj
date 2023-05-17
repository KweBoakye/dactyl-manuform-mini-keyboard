(ns dactyl-keyboard.low.oled-low-placements
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.oled :refer :all]
            [dactyl-keyboard.low.web-connecters-low :refer :all]
             [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.lib.geometry :refer [deg2rad]]
            ))

(def screen-holder-position [2, -2, 0])


(when (= oled-type "ST7789-240x240-154")
  (def screen-holder-cut (rotate (deg2rad 0) [1 0 0] (rotate (deg2rad 90) [0 0 1] ST7789-240x240-154-holder-cut))) 
  (def screen-holder-cut-screen-cut  (rotate (deg2rad 0) [1 0 0] (rotate (deg2rad 90) [0 0 1] ST7789-240x240-154-screen-cut)))
  (def screen-holder-cut-viewport-cut  (rotate (deg2rad 0) [1 0 0] (rotate (deg2rad 90) [0 0 1] ST7789-240x240-154-viewport-cut)))
  (def screen-holder (rotate (deg2rad 0) [1 0 0] (rotate (deg2rad 90) [0 0 1] ST7789-240x240-154-holder)))
  (def screen-holder-width ST7789-240x240-154-holder-width)
  (def screen-holder-height ST7789-240x240-154-holder-height)
  (def screen-holder-depth ST7789-240x240-154-holder-thickness)
  )

(def oled-post-size 1)
(def oled-translation-vector [0 0 (+ (/ oled-holder-thickness -2) plate-thickness)])

(def oled-post (->> (web-post-shape-with-size oled-holder-thickness oled-post-size 36)
                    (translate oled-translation-vector )))

(def oled-post-adj (/ oled-post-size 2))
(def oled-post-tr-translation-vector  [(- (/ mount-width 1.95)  oled-post-adj) (- (/ mount-height 1.95) oled-post-adj) 0] )
(def oled-post-tl-translation-vector  [(+ (/ mount-width -1.95) oled-post-adj) (- (/ mount-height 1.95) oled-post-adj) 0] )
(def oled-post-bl-translation-vector  [(+ (/ mount-width -1.95) oled-post-adj) (+ (/ mount-height -1.95) oled-post-adj) 0])
(def oled-post-br-translation-vector  [(- (/ mount-width 1.95)  oled-post-adj) (+ (/ mount-height -1.95) oled-post-adj) 0])
(def oled-post-tm-translation-vector  [0 (- (/ mount-height 1.95) oled-post-adj) 0])
(def oled-post-bm-translation-vector  [0 (+ (/ mount-height -1.95) oled-post-adj) 0])
(def oled-post-rm-translation-vector  [(- (/ mount-width 1.95)  oled-post-adj) 0 0])
(def oled-post-lm-translation-vector  [(+ (/ mount-width -1.95)  oled-post-adj) 0 0])
(def oled-post-tr-tm-translation-vector [(/ (- (/ mount-width 1.95)  oled-post-adj) 2) (- (/ mount-height 1.95) oled-post-adj) 0])
(def oled-post-tr-rm-translation-vector [(- (/ mount-width 1.95)  oled-post-adj) (/ (- (/ mount-height 1.95) oled-post-adj) 2) 0])
(def oled-post-tl-tm-translation-vector [(/ (+ (/ mount-width -1.95) oled-post-adj) 2) (- (/ mount-height 1.95) oled-post-adj) 0])
(def oled-post-bl-lm-translation-vector [(+ (/ mount-width -1.95) oled-post-adj) (/ (+ (/ mount-height -1.95) oled-post-adj)  2) 0])
(def oled-post-br-rm-translation-vector [(- (/ mount-width 1.95)  oled-post-adj) (/ (+ (/ mount-height -1.95) oled-post-adj)) 0])
(def oled-post-tl-lm-translation-vector [(+ (/ mount-width -1.95) oled-post-adj) (/ (- (/ mount-height 1.95) oled-post-adj) 2) 0])
(def oled-post-bl-bm-translation-vector [(/ (+ (/ mount-width -1.95) oled-post-adj) 2) (+ (/ mount-height -1.95) oled-post-adj) 0])
(def oled-post-br-bm-translation-vector [(/ (- (/ mount-width 1.95)  oled-post-adj) 2 )(+ (/ mount-height -1.95) oled-post-adj) 0])
  
(def oled-post-tr (translate oled-post-tr-translation-vector oled-post))
(def oled-post-tl (translate oled-post-tl-translation-vector oled-post))
(def oled-post-bl (translate oled-post-bl-translation-vector  oled-post))
(def oled-post-br (translate oled-post-br-translation-vector  oled-post))
(def oled-post-bm (translate oled-post-bm-translation-vector  oled-post))
(def oled-post-tr-tm (translate oled-post-tr-tm-translation-vector oled-post))
(def oled-post-tr-rm (translate oled-post-tr-rm-translation-vector oled-post))
(def oled-post-tl-tm (translate oled-post-tl-tm-translation-vector oled-post))
(def oled-post-bl-lm (translate oled-post-bl-lm-translation-vector oled-post))
(def oled-post-br-rm (translate oled-post-br-rm-translation-vector oled-post))
(def oled-post-tl-lm (translate oled-post-tl-lm-translation-vector oled-post))
(def oled-post-bl-bm (translate oled-post-bl-bm-translation-vector oled-post))
(def oled-post-br-bm (translate oled-post-br-bm-translation-vector oled-post))

(def curve-post-size (/ oled-post-size 2))
(def curve-post-translation-vector [0 0 (+ (/ oled-post-size -2)  plate-thickness)])
(def curve-post (->> (binding [*fn* sphere-fn-value](sphere curve-post-size))
                    (translate curve-post-translation-vector)))

(def curve-post-adj (/ oled-post-size 2))
(def curve-post-tr-translation-vector  [(- (/ mount-width 1.95)  curve-post-adj) (- (/ mount-height 1.95) curve-post-adj) 0] )
(def curve-post-tl-translation-vector  [(+ (/ mount-width -1.95) curve-post-adj) (- (/ mount-height 1.95) curve-post-adj) 0] )
(def curve-post-bl-translation-vector  [(+ (/ mount-width -1.95) curve-post-adj) (+ (/ mount-height -1.95) curve-post-adj) 0] )
(def curve-post-br-translation-vector  [(- (/ mount-width 1.95)  curve-post-adj) (+ (/ mount-height -1.95) curve-post-adj) 0] )
(def curve-post-tm-translation-vector  [0 (- (/ mount-height 1.95) curve-post-adj) 0])
(def curve-post-bm-translation-vector  [0 (+ (/ mount-height -1.95) curve-post-adj) 0])
(def curve-post-rm-translation-vector  [(- (/ mount-width 1.95)  curve-post-adj) 0 0])
(def curve-post-lm-translation-vector  [(+ (/ mount-width -1.95)  curve-post-adj) 0 0])
(def curve-post-tr-tm-translation-vector [(/ (- (/ mount-width 1.95)  curve-post-adj) 2) (- (/ mount-height 1.95) curve-post-adj) 0])
(def curve-post-tr-rm-translation-vector [(- (/ mount-width 1.95)  curve-post-adj) (/ (- (/ mount-height 1.95) curve-post-adj) 2) 0])
(def curve-post-tl-tm-translation-vector [(/ (+ (/ mount-width -1.95) curve-post-adj) 2) (- (/ mount-height 1.95) curve-post-adj) 0])
(def curve-post-bl-lm-translation-vector [(+ (/ mount-width -1.95) curve-post-adj) (/ (+ (/ mount-height -1.95) curve-post-adj)  2) 0])
(def curve-post-br-rm-translation-vector [(- (/ mount-width 1.95)  curve-post-adj) (/ (+ (/ mount-height -1.95) curve-post-adj)) 0])
(def curve-post-tl-lm-translation-vector [(+ (/ mount-width -1.95) curve-post-adj) (/ (- (/ mount-height 1.95) curve-post-adj) 2) 0])
(def curve-post-bl-bm-translation-vector [(/ (+ (/ mount-width -1.95) curve-post-adj) 2) (+ (/ mount-height -1.95) curve-post-adj) 0])
(def curve-post-br-bm-translation-vector [(/ (- (/ mount-width 1.95)  curve-post-adj) 2) (+ (/ mount-height -1.95) curve-post-adj) 0])

(def curve-post-tr (translate curve-post-tr-translation-vector curve-post))
(def curve-post-tl (translate curve-post-tl-translation-vector curve-post))
(def curve-post-bl (translate curve-post-bl-translation-vector  curve-post))
(def curve-post-br (translate curve-post-br-translation-vector  curve-post))
(def curve-post-bm (translate curve-post-bm-translation-vector  curve-post))

(def curve-post-tr-tm (translate curve-post-tr-tm-translation-vector curve-post))
(def curve-post-tr-rm (translate curve-post-tr-rm-translation-vector curve-post))
(def curve-post-tl-tm (translate curve-post-tl-tm-translation-vector curve-post))
(def curve-post-bl-lm (translate curve-post-bl-lm-translation-vector curve-post))
(def curve-post-br-rm (translate curve-post-br-rm-translation-vector curve-post))
(def curve-post-tl-lm (translate curve-post-tl-lm-translation-vector curve-post))
(def curve-post-bl-bm (translate curve-post-bl-bm-translation-vector curve-post))
(def curve-post-br-bm (translate curve-post-br-bm-translation-vector curve-post))

(def sphere-post (->> (sphere (/ oled-post-size 2))
                     (translate [0 0 (+ (/ oled-holder-thickness -2) plate-thickness)])))

(def sphere-post-adj (/ oled-post-size 2))
(def sphere-post-tr (translate [(- (/ mount-width 1.95)  sphere-post-adj) (- (/ mount-height 1.95) sphere-post-adj) 0] sphere-post))
(def sphere-post-tl (translate [(+ (/ mount-width -1.95) sphere-post-adj) (- (/ mount-height 1.95) sphere-post-adj) 0] sphere-post))
(def sphere-post-bl (translate [(+ (/ mount-width -1.95) sphere-post-adj) (+ (/ mount-height -1.95) sphere-post-adj) 0] sphere-post))
(def sphere-post-br (translate [(- (/ mount-width 1.95)  sphere-post-adj) (+ (/ mount-height -1.95) sphere-post-adj) 0] sphere-post))

(def round-thumb-post (->> (web-post-shape-with-size web-thickness oled-post-size 36)
                    (translate [0 0 (+ (/ web-thickness -2) plate-thickness)])))
(def round-thumb-post-tr (translate [(- (/ mount-width 1.95)  oled-post-adj) (- (+ (/ mount-height 1.95) (/ oled-post-size 2)) oled-post-adj) 0] round-thumb-post))
(def round-thumb-post-tl (translate [(+ (/ mount-width -1.95) oled-post-adj) (- (+ (/ mount-height 1.95) (/ oled-post-size 2)) oled-post-adj) 0] round-thumb-post))
(def round-thumb-post-bl (translate [(+ (/ mount-width -1.95) oled-post-adj) (+ (/ mount-height -1.95) oled-post-adj) 0] round-thumb-post))
(def round-thumb-post-br (translate [(- (/ mount-width 1.95)  oled-post-adj) (+ (/ mount-height -1.95) oled-post-adj) 0] round-thumb-post))

(defn oled-post-position-top [corner-translation-vector] (mapv + [0 0 (/ oled-holder-thickness 2)] oled-translation-vector corner-translation-vector))
(defn oled-post-position-midle [corner-translation-vector ] (mapv + [0 0 0] oled-translation-vector corner-translation-vector))
(defn oled-post-position-bottom [corner-translation-vector] (mapv + [0 0 (/ oled-holder-thickness -2)] oled-translation-vector corner-translation-vector))
(defn curve-post-position-top [corner-translation-vector] (mapv + [0 0 (/ web-thickness 2)] curve-post-translation-vector corner-translation-vector))
(defn curve-post-position-middle [corner-translation-vector] (mapv + [0 0 0] curve-post-translation-vector corner-translation-vector))
(defn curve-post-position-bottom [corner-translation-vector] (mapv + [0 0 (/ web-thickness -2)] curve-post-translation-vector corner-translation-vector))

(defn get-oled-corner-translation-vector [position]
  (case (if-position-is-string-return-keyword position)
    :tr oled-post-tr-translation-vector
    :tl oled-post-tl-translation-vector
    :bl oled-post-bl-translation-vector
    :br oled-post-br-translation-vector
    :tm oled-post-tm-translation-vector
    :bm oled-post-bm-translation-vector
    :rm oled-post-rm-translation-vector
    :lm oled-post-lm-translation-vector
    :tr-tm oled-post-tr-tm-translation-vector
    :tr-rm oled-post-tr-rm-translation-vector
    :tl-tm oled-post-tl-tm-translation-vector
    :bl-lm oled-post-bl-lm-translation-vector
    :br-rm oled-post-br-rm-translation-vector
    :tl-lm oled-post-tl-lm-translation-vector
    :bl-bm oled-post-bl-bm-translation-vector
    :br-bm oled-post-br-bm-translation-vector
    [0 0 0]))

(defn get-curve-corner-translation-vector [position]
  (case (if-position-is-string-return-keyword position)
    :tr curve-post-tr-translation-vector
    :tl curve-post-tl-translation-vector
    :bl curve-post-bl-translation-vector
    :br curve-post-br-translation-vector
    :tm curve-post-tm-translation-vector
    :bm curve-post-bm-translation-vector
    :rm curve-post-rm-translation-vector
    :lm curve-post-lm-translation-vector
     :tr-tm curve-post-tr-tm-translation-vector
:tr-rm curve-post-tr-rm-translation-vector
:tl-tm curve-post-tl-tm-translation-vector
:bl-lm curve-post-bl-lm-translation-vector
:br-rm curve-post-br-rm-translation-vector
:tl-lm curve-post-tl-lm-translation-vector
:bl-bm curve-post-bl-bm-translation-vector
:br-bm curve-post-br-bm-translation-vector
    [0 0 0]))

(defn get-oled-post-outer-x-and-y-vector [dx dy]
  (let [x (cond (pos? dx) (/ oled-post-size 2)
                (neg? dx) (/ oled-post-size -2)
                :else 0.0)
        y (cond (pos? dy) (/ oled-post-size 2)
                (neg? dy) (/ oled-post-size -2)
                 :else 0.0)]
    [x y 0.0]))

(defn get-oled-post-inner-x-and-y-vector [dx dy]
  (let [x (cond (pos? dx)  (/ oled-post-size -2)
              (neg? dx) (/ oled-post-size 2)
                :else 0.0)
        y (cond (pos? dy) (/ oled-post-size -2)
                (neg? dy)(/ oled-post-size 2)
                :else 0.0)]
    [x y 0.0]))

(comment (pos? 0))
(defn get-curve-post-outer-x-and-y-vector [dx dy]
  (let [x (cond (pos? dx) (/ curve-post-size 2)
                (neg? dx)(/ curve-post-size -2)
                :else 0.0)
        y (cond (pos? dy) (/ curve-post-size 2)
                (neg? dy ) (/ curve-post-size -2)
                :else 0.0)]
    [x y 0.0]))