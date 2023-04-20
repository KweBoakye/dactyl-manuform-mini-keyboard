(ns dactyl-keyboard.low.screen-holder-placement-points
(:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.low.oled-low-placements :refer [screen-holder-depth
                                                             screen-holder-height screen-holder-width]]
            [dactyl-keyboard.low.placement-functions-low :refer [transform-position]]
            [dactyl-keyboard.low.screen-holder-placement-functions :refer :all]
            [dactyl-keyboard.low.web-connecters-low :refer [post-size
                                                            web-thickness]]
            [dactyl-keyboard.oled :refer [oled-holder-thickness]])
  )

(def screen-holder-top-left  (partial screen-holder-translate-and-place-side-with-offset (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 (mapv + [(/ post-size 2) (/ post-size -2) 0] [0 0 (/ oled-holder-thickness 2)])))

(def screen-holder-top-right (partial screen-holder-translate-and-place-side-with-offset (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2) 0 (mapv + [(/ post-size -2) (/ post-size -2) 0] [0 0 (/ oled-holder-thickness 2)])))

(def screen-holder-top-left-outside-point (transform-position
                                           (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                           (mapv + [(/ post-size 2) (/ post-size -2) 0] [0 0 (/ oled-holder-thickness 2)])))

(def screen-holder-top-right-outside-point (transform-position
                                            (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                            (mapv + [(/ post-size -2) (/ post-size -2) 0] [0 0 (/ oled-holder-thickness 2)])))
(def screen-holder-top-right-outside-point-alt (transform-position
                                            (partial screen-holder-translate-and-place-side (+ (/ (+ screen-holder-height) 2) (/ screen-holder-depth 4)) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                            (mapv + [(/ post-size -2) (/ post-size -2) 0] [0 0 (/ oled-holder-thickness 2)])))



(def screen-holder-bottom-right-outside-point (transform-position
                                               (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                               (mapv + [(/ post-size -2) (/ post-size 2) 0] [0 0 (/ oled-holder-thickness 2)])))
(def screen-holder-bottom-right-outside-point-alt (transform-position
                                               (partial screen-holder-translate-and-place-side (+ (/ (+ screen-holder-height) 2) (/ screen-holder-depth 4)) (/ (- screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                               (mapv + [(/ post-size -2) (/ post-size 2) 0] [0 0 (/ oled-holder-thickness 2)])))
(def screen-holder-bottom-right-outside-floor-point (assoc (vec screen-holder-bottom-right-outside-point) 2 0))
(def screen-holder-bottom-right-outside-floor-point-alt (assoc (vec screen-holder-bottom-right-outside-point-alt ) 2 0))


(def screen-holder-top-left-inside-point (transform-position
                                          (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                          (mapv + [(/ post-size 2) (/ post-size -2) 0] [0 0 (/ oled-holder-thickness 2)])))


(def screen-holder-top-left-inside-point-translated (transform-position
                                                     (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                                     (mapv + [(/ post-size 2) (/ post-size -2) 0] [0 (/ web-thickness 2) (/ oled-holder-thickness 1)])))
(def screen-holder-bottom-left-outside-point (transform-position
                                              (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                              (mapv + [(/ post-size 2) (/ post-size 2) 0] [0 0 (/ oled-holder-thickness 2)])))
(def screen-holder-bottom-left-outside-floor-point (assoc (vec screen-holder-bottom-left-outside-point) 2 0))
(def screen-holder-bottom-left-inside-point (transform-position
                                             (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                             (mapv + [(/ post-size 2) (/ post-size 2) 0] [0 0 (/ oled-holder-thickness 2)])))

(def screen-holder-top-right-inside-point (transform-position
                                           (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                           (mapv +  [(/ post-size -2) (/ post-size -2) (/ oled-holder-thickness 2)])))

(def screen-holder-bottom-right-inside-point (transform-position
                                              (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                              (mapv + [(/ post-size -2) (/ post-size 2) 0] [0 0 (/ oled-holder-thickness 2)])))
(def screen-holder-bottom-right-inside-point-alt (transform-position
                                              (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  (* (- (/ screen-holder-depth 2)) 1.5))
                                              (mapv + [(/ post-size -2) (/ post-size 2) 0] [0 0 (/ oled-holder-thickness 2)])))


(def screen-holder-bottom-left-inside-floor-point (assoc (vec screen-holder-bottom-left-inside-point) 2 0.0))
(def screen-holder-bottom-right-inside-floor-point (assoc (vec screen-holder-bottom-right-inside-point) 2 0.0))

(def screen-holder-top-mid-outside-point (transform-position
                                          (partial screen-holder-translate-and-place-side 0 (/ (+ screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                          (mapv + [0 (/ post-size -2) 0] [0 0 (/ oled-holder-thickness 2)])))

(def screen-holder-top-mid-inside-point (transform-position
                                         (partial screen-holder-translate-and-place-side 0 (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                         (mapv + [0 (/ post-size -2) 0] [0 0 (/ oled-holder-thickness 2)])))

(def screen-holder-bottom-mid-outside-point (transform-position
                                          (partial screen-holder-translate-and-place-side 0 (/ (- screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                          (mapv + [0 (/ post-size 2) 0] [0 0 (/ oled-holder-thickness 2)])))

(def screen-holder-bottom-mid-inside-point (transform-position
                                         (partial screen-holder-translate-and-place-side 0 (/ (- screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                         (mapv + [0 (/ post-size 2) 0] [0 0 (/ oled-holder-thickness 2)])))

(def screen-holder-bottom-mid-outside-floor-point (assoc (vec screen-holder-bottom-mid-outside-point) 2 0.0))
(def screen-holder-bottom-mid-inside-floor-point (assoc (vec screen-holder-bottom-mid-inside-point) 2 0.0))