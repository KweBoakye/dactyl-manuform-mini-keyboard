(ns dactyl-keyboard.low.case-low-polyhedron
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
            [dactyl-keyboard.low.case-low-functions :refer :all]
            [dactyl-keyboard.low.case-low :refer :all]
            [dactyl-keyboard.low.case-low-polyhedron-functions :refer :all] 
            [dactyl-keyboard.low.thumbs-low :refer :all]
            [dactyl-keyboard.low.oled-low-placements :refer :all]
            [dactyl-keyboard.oled :refer :all]
            [dactyl-keyboard.tps-65 :refer :all]
            [dactyl-keyboard.EVQWGD001 :refer :all]))

(def tps-65-top-right (partial tps-65-translate-and-place-at-position-with-offset tps-65-mount-corner-cylinder-top-right-position [(+ tps-65-corner-radius 0.05 (/ post-size 2))  (+ tps-65-corner-radius 0.05 (/ post-size 2)) (+ (* (+ tps-65-depth) -2) 0.05)]))


(def tps-65-bottom-right (partial tps-65-translate-and-place-at-position-with-offset tps-65-mount-corner-cylinder-bottom-right-position [(+ tps-65-corner-radius 0.05 (/ post-size 2))  (- (+ tps-65-corner-radius 0.05 (/ post-size 2))) (+ (* (+ tps-65-depth) -2) 0.05)]))

(def screen-holder-top-left-outside-point (transform-position
                                      (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                      (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)])))

(def screen-holder-top-right-outside-point (transform-position
                                       (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                       (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)])))

(def tps-65-top-left-outer    (transform-position
                          (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-left-position)
                          [(- (+ tps-65-corner-radius 0.05))  (+ tps-65-corner-radius 0.05) 0]))



 (def screen-holder-top-left-inside-point (transform-position
                                      (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                      (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)])))


(def screen-holder-top-left-inside-point-translated (transform-position
                                                (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                                (mapv + [0 0 0] [0 (/ web-thickness 2) (/ oled-holder-thickness 1)])))
(def screen-holder-bottom-left-outside-point (transform-position
                                         (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                         (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)])))
(def screen-holder-bottom-left-outside-floor-point (assoc (vec screen-holder-bottom-left-outside-point) 2 0))
(def screen-holder-bottom-left-inside-point (transform-position
                                        (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                        (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)])))

 (def screen-holder-top-right-inside-point (transform-position
                                       (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                       (mapv +  [0 0 (/ oled-holder-thickness 2)])))
 
 (def tps-65-top-left-inner    (transform-position
                           (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-left-position)
                           [(- (+ tps-65-corner-radius 0.05))  (+ tps-65-corner-radius 0.05) (- (/ web-thickness 2))]))
(def tps-65-top-left-control-point-outer (transform-position
                                          (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-left-position)
                                          [(- (+ tps-65-corner-radius 0.05))  (+ tps-65-corner-radius 0.05) (/ web-thickness)]))
(def  screen-holder-top-right-inside-point-translated (transform-position
                                                       (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                                       (mapv +  [0 (/ web-thickness 2) (/ oled-holder-thickness 1)])))
(def screen-holder-outside-point-with-same-y-as-tps-65-top-left-upper (find-point-on-line-using-y  screen-holder-top-left-outside-point screen-holder-top-right-outside-point  (nth tps-65-top-left-outer 1)))

(def EVQWGD001-mount-bottom-right-outside (transform-position
                                           (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right)
                                           (mapv + [(/ oled-post-size 1) (/ oled-post-size -1) (- (/ EVQWGD001-mount-y-modifier 1))] [0 0 (/  EVQWGD001-mount-height 2)])))
(def screen-holder-inside-point-with-same-y-as-tps-65-top-left-inner (find-point-on-line-using-y   screen-holder-top-right-inside-point screen-holder-top-left-inside-point  (nth tps-65-top-left-inner 1)))
 (def EVQWGD001-mount-bottom-right-outside-floor (translate-to-floor EVQWGD001-mount-bottom-right-outside))
(def screen-holder-top-right-inside-point-translated-with-same-y-as-tps-65-top-left-inner (find-point-on-line-using-y  screen-holder-top-left-inside-point-translated screen-holder-top-right-inside-point-translated  (nth tps-65-top-left-inner 1)))
(def tps-65-top-left-inner-with-same-z-as-screen-holder-top-right-inside-point-translated-with-same-y-as-tps-65-top-left-inner (assoc tps-65-top-left-inner 2 (nth screen-holder-inside-point-with-same-y-as-tps-65-top-left-inner 2)))
(def screen-holder-bottom-right-outside-point (transform-position
                                               (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                               (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)])))
 (def screen-holder-bottom-right-outside-floor-point (assoc (vec screen-holder-bottom-right-outside-point) 2 0))
 
 (defn back-left-wall-to-screen [steps &{:keys [bottom-plate] :or {bottom-plate false}}]
  (let [half-steps1 (if (even? steps) (/ steps 2) (/ (inc steps) 2))
        half-steps2 (if (even? steps) (/ steps 2) (/ (dec steps) 2))

        tps-65-top-right-web-post-outside-point (transform-position
                                                 tps-65-top-right
                                                 [0 0 (/ web-thickness 2)])
        
        screen-holder-bottom-left-inside-floor-point (assoc (vec screen-holder-bottom-left-inside-point) 2 0)



        tps-65-top-right-web-post-inside-point (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset [0 0 (- (/ web-thickness))])
        outside-vertical-curve-origin [0 0 (/ curve-post-size 2)]
        tps-65-top-right-wall-locate3-outside-point (transform-position
                                                     tps-65-top-right
                                                     (mapv + (wall-locate3-for-polyhedron-point  0 1) [(/ curve-post-size 2) (/ curve-post-size 1) 0]
                                                           curve-post-translation-vector))
        tps-65-top-right-wall-locate3-outside-control-point (transform-position
                                                             tps-65-top-right
                                                             (mapv + (wall-locate3-for-polyhedron-point 0 1) [0 (/ curve-post-size 1)  0] curve-post-translation-vector))
        tps-65-top-right-wall-locate2-inside-point (transform-position
                                                    tps-65-top-right
                                                    (mapv + (wall-locate2-for-polyhedron-point 0 1) [(/ curve-post-size -1) curve-post-size  0] oled-translation-vector))
        tps-65-top-right-wall-locate2-inside-point-translated (transform-position
                                                               tps-65-top-right
                                                               (mapv + (wall-locate2-for-polyhedron-point 0 1) [0 0 (/ oled-holder-thickness 2)] [(/ curve-post-size -1) curve-post-size  0] oled-translation-vector))
        tps-65-top-right-wall-locate2-inside-control-point (transform-position
                                                            tps-65-top-right
                                                            (mapv + (wall-locate2-for-polyhedron-point 0 1) [(/ curve-post-size -1) curve-post-size 0] oled-translation-vector))
        tps-65-top-right-wall-locate2-inside-control-point-translated (transform-position
                                                                       tps-65-top-right
                                                                       (mapv + (wall-locate2-for-polyhedron-point 0 1) [0 0 (/ oled-holder-thickness 2)] [(/ curve-post-size -1) curve-post-size 0] oled-translation-vector))
        tps-65-top-right-inside-level-with-screen-bottom-left (assoc (vec tps-65-top-right-wall-locate2-inside-point) 2 (last screen-holder-bottom-left-inside-point))
        tps-65-top-right-inside-level-with-screen-bottom-left-control-point (assoc (vec tps-65-top-right-wall-locate2-inside-control-point) 2 (last screen-holder-bottom-left-inside-point))
        tps-65-top-right-level-with-screen-bottom-left (assoc (vec tps-65-top-right-wall-locate3-outside-point) 2 (last screen-holder-bottom-left-outside-point))
        tps-65-top-right-level-with-screen-bottom-left-control-point (assoc (vec tps-65-top-right-wall-locate3-outside-control-point) 2 (last screen-holder-bottom-left-outside-point))
        tps-65-top-right-floor-bottom-left (assoc (vec tps-65-top-right-wall-locate3-outside-point) 2 0)
        tps-65-top-right-floor-bottom-left-control-point (assoc (vec tps-65-top-right-wall-locate3-outside-control-point) 2 0)
        tps-65-top-right-inside-bottom-point  (assoc (vec tps-65-top-right-wall-locate2-inside-point) 2 0)
        tps-65-top-right-inside-bottom-control-point (assoc (vec tps-65-top-right-wall-locate2-inside-control-point) 2 0)
        top-bezier-points (bezier-quadratic tps-65-top-right-wall-locate3-outside-point tps-65-top-right-wall-locate3-outside-control-point screen-holder-top-left-outside-point steps)
        mid-bezier-points (bezier-quadratic tps-65-top-right-level-with-screen-bottom-left tps-65-top-right-level-with-screen-bottom-left-control-point screen-holder-bottom-left-outside-point steps)
        bottom-bezier-points (bezier-quadratic tps-65-top-right-floor-bottom-left tps-65-top-right-floor-bottom-left-control-point screen-holder-bottom-left-outside-floor-point steps)
        top-inside-bezier-points (bezier-quadratic  screen-holder-top-left-inside-point tps-65-top-right-wall-locate2-inside-control-point  tps-65-top-right-wall-locate2-inside-point steps)
        mid-inside-bezier-points (bezier-quadratic screen-holder-bottom-left-inside-point tps-65-top-right-inside-level-with-screen-bottom-left-control-point tps-65-top-right-inside-level-with-screen-bottom-left steps)
        bottom-inside-bezier-points (bezier-quadratic screen-holder-bottom-left-inside-floor-point  tps-65-top-right-inside-bottom-control-point  tps-65-top-right-inside-bottom-point steps)
        wall-curve-points  (into [] (concat top-bezier-points mid-bezier-points bottom-bezier-points top-inside-bezier-points mid-inside-bezier-points bottom-inside-bezier-points))
        top-bezier-points-size (count top-bezier-points)
        top-bezier-points-end (- top-bezier-points-size 1)
        mid-bezier-points-size (count mid-bezier-points)
        mid-bezier-points-start top-bezier-points-size
        mid-bezier-points-end (+ mid-bezier-points-start (dec mid-bezier-points-size))
        bottom-bezier-points-size (count bottom-bezier-points)
        bottom-bezier-points-start (inc mid-bezier-points-end)
        bottom-bezier-points-end (+ bottom-bezier-points-start (dec bottom-bezier-points-size))
        top-inside-bezier-points-start (inc bottom-bezier-points-end)
        top-inside-bezier-points-end (+ top-inside-bezier-points-start  steps)
        mid-inside-bezier-points-start (inc top-inside-bezier-points-end)
        mid-inside-bezier-points-end (+ mid-inside-bezier-points-start  steps)
        bottom-inside-bezier-points-start (inc mid-inside-bezier-points-end)
        bottom-inside-bezier-points-end (+ bottom-inside-bezier-points-start steps)


 ;top inside to mid inside

        wall-curve-faces (into [] (concat

                                   (for [index (range top-bezier-points-end)]
                                     [index (inc index) (+ (inc index) (inc steps))])
                                   (for [index (range mid-bezier-points-start mid-bezier-points-end)]
                                     [(- index mid-bezier-points-size) (inc index) index])
                                   (for [index (range mid-bezier-points-start  mid-bezier-points-end)]
                                     [index (inc index) (+ (inc index) (inc steps))])
                                   (for [index (range bottom-bezier-points-start bottom-bezier-points-end)]
                                     [index (- index bottom-bezier-points-size) (inc index)])
                                   (for [index (range top-inside-bezier-points-start top-inside-bezier-points-end)]
                                     [index (inc index) (+ (inc index) (inc steps))])
                                   (for [index (range mid-inside-bezier-points-start mid-inside-bezier-points-end)]
                                     [(- index (inc steps)) (inc index) index])
                                   (for [index (range mid-inside-bezier-points-start  mid-inside-bezier-points-end)]
                                     [index (inc index) (+ (inc index) (inc steps))])
                                   (for [index (range bottom-inside-bezier-points-start bottom-inside-bezier-points-end)]
                                     [index (- index (inc steps)) (inc index)])
                                   [[top-bezier-points-end mid-inside-bezier-points-start mid-bezier-points-end]
                                    [top-bezier-points-end top-inside-bezier-points-start mid-inside-bezier-points-start]
                                    [mid-bezier-points-end bottom-inside-bezier-points-start bottom-bezier-points-end]
                                    [mid-inside-bezier-points-start bottom-inside-bezier-points-start mid-bezier-points-end]]

                                   [[top-inside-bezier-points-end mid-bezier-points-start mid-inside-bezier-points-end]
                                    [top-inside-bezier-points-end 0 mid-bezier-points-start]
;[top-inside-bezier-points-end 0 mid-inside-bezier-points-start]
                                    [mid-inside-bezier-points-end mid-bezier-points-start bottom-bezier-points-start]
                                    [mid-inside-bezier-points-end bottom-bezier-points-start bottom-inside-bezier-points-end]]

                                   (for [index (range 0 top-bezier-points-end)]
                                     [(inc index) index (- top-inside-bezier-points-end index)])
                                   (for [index (range 1  (inc top-bezier-points-end))]
                                     [(- top-inside-bezier-points-end (dec index))  (- top-inside-bezier-points-end  index) index])

                                   (for [index (range bottom-bezier-points-start bottom-bezier-points-end)]
                                     [index (inc index)  (- bottom-inside-bezier-points-end (-  index bottom-bezier-points-start))])
                                   (for [index (range bottom-bezier-points-start   bottom-bezier-points-end)]
                                     [(- bottom-inside-bezier-points-end  (- index bottom-bezier-points-start)) (inc index) (- bottom-inside-bezier-points-end (inc (- index bottom-bezier-points-start)))])))
        wall-curve (polyhedron wall-curve-points, wall-curve-faces)

        tps-65-top-middle-outer    (transform-position
                                    (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-centre-position)
                                    [0  (+ tps-65-corner-radius 0.05) 0])

        tps-65-top-middle-inner    (transform-position
                                    (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-centre-position)
                                    [0  (+ tps-65-corner-radius 0.05) (/ web-thickness -2)])
        tps-65-top-right-outer    (transform-position
                                   (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-right-position)
                                   [(+ tps-65-corner-radius 0.05)  (+ tps-65-corner-radius 0.05) 0])
        tps-65-top-right-to-top-left-outer (bezier-linear  tps-65-top-right-outer tps-65-top-left-outer  (* steps 2))

        tps-65-top-right-control-point-outer (transform-position
                                              (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position  tps-65-mount-corner-radius-with-offset-mod  tps-65-mount-corner-radius-with-offset)
                                              (mapv + oled-translation-vector [(/ oled-post-size -2) (/ oled-post-size 2) (/ oled-holder-thickness 2)]))
        tps-65-top-middle-control-point-outer (transform-position
                                               (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-centre-position  0  tps-65-mount-corner-radius-with-offset)
                                               (mapv + oled-translation-vector [0 (/ oled-post-size 2) (/ web-thickness 2)]))
        tps-65-top-middle-control-point-inner (transform-position
                                               (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-centre-position  0  tps-65-mount-corner-radius-with-offset)
                                               (mapv + oled-translation-vector [0 (/ oled-post-size 2) (/ web-thickness -2)]))
        tps-65-screen-side-upper-control-points (bezier-linear tps-65-top-right-control-point-outer tps-65-top-left-control-point-outer  (* steps 2))
        tps-65-top-right-wall-to-screen-holder-top-right-points-outer (concat top-bezier-points (reverse (drop 1 (bezier-linear  screen-holder-top-right-outside-point screen-holder-top-left-outside-point steps))))


        tps-65-top-right-inner    (transform-position
                                   (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-right-position)
                                   [(+ tps-65-corner-radius 0.05)  (+ tps-65-corner-radius 0.05) (/ web-thickness -2)])
        tps-65-top-right-to-top-left-inner (bezier-linear tps-65-top-left-inner tps-65-top-right-inner  (inc (* steps 2)))
        tps-65-top-left-control-point-inner (mapv + tps-65-top-left-inner [0 0 (/ web-thickness 2)])

        tps-65-top-right-control-point-inner (transform-position
                                              (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset-mod  tps-65-mount-corner-radius-with-offset)
                                              (mapv + oled-translation-vector [(/ oled-post-size -2) (/ oled-post-size 2) (/ oled-holder-thickness -2)]))


        tps-65-top-middle-control-point-inner-flat (find-point-on-line-using-x tps-65-top-middle-inner screen-holder-top-left-inside-point (nth tps-65-top-middle-control-point-inner 0))
        tps-65-screen-side-inner-control-points (bezier-linear tps-65-top-left-control-point-inner tps-65-top-right-control-point-inner (inc (* steps 2)))
        tps-65-top-right-wall-to-screen-holder-top-right-points-inner (concat top-inside-bezier-points (bezier-linear screen-holder-top-left-inside-point screen-holder-top-right-inside-point steps))
        tps-65-top-right-points (wall-brace-polyhedron-points tps-65-top-right 0 1 "centre" :degrees)
        tps-65-top-right-curve-points (wall-brace-polyhedron-curve-points tps-65-top-right 0 1 "centre" :degrees steps)
        tps-65-top-right-to-screen-holder-outer (into [] (apply concat
                                                                (for [index (range 0 (inc steps))
                                                                      :let [control-vector [4 -4 0]
                                                                            tps-65-top-right-curves-outer (tps-65-top-right-curve-points :outer-points)
                                                                            control-curves (bezier-quintic
                                                                                            (calculate-point-between-points (tps-65-top-right-points :web-post-position-top) tps-65-top-middle-outer [0 0 0])
                                                                                            (calculate-point-between-points (tps-65-top-right-points :wall-locate1-point) tps-65-top-middle-control-point-outer control-vector)
                                                                                            (calculate-point-between-points (tps-65-top-right-points :wall-locate-1-to-3-curve-for-polyhedron-control-point) screen-holder-top-left-inside-point-translated control-vector)
                                                                                            (calculate-point-between-points (tps-65-top-right-points :wall-locate-1-to-3-curve-for-polyhedron-second-control-point) screen-holder-top-left-outside-point control-vector)
                                                                                            (calculate-point-between-points (tps-65-top-right-points :wall-locate3-point) screen-holder-top-left-outside-point control-vector)
                                                                                            (calculate-point-between-points (tps-65-top-right-points :wall-locate3-point-floor) screen-holder-bottom-left-outside-floor-point [0 0 0])
                                                                                            steps)
                                                                            mid-curves (concat (drop-last (bezier-cubic tps-65-top-middle-outer
                                                                                                                        tps-65-top-middle-control-point-outer
                                                                                                                        screen-holder-top-left-inside-point-translated
                                                                                                                        screen-holder-top-left-outside-point
                                                                                                                        half-steps1))
                                                                                               (bezier-quadratic
                                                                                                screen-holder-top-left-outside-point
                                                                                                screen-holder-bottom-left-outside-point
                                                                                                screen-holder-bottom-left-outside-floor-point
                                                                                                half-steps2))]]
                                                                  (bezier-quadratic  (nth mid-curves index) (nth control-curves index) (nth tps-65-top-right-curves-outer index) steps))))

        tps-65-top-right-to-screen-holder-inner (into [] (apply concat
                                                                (for [index (range 0 (inc steps))
                                                                      :let [tps-65-top-right-curves-inner (tps-65-top-right-curve-points :inner-points)
                                                                            control-vector [4 -4 0]
                                                                            control-points (bezier-cubic
                                                                                            (calculate-point-between-points (tps-65-top-right-points :web-post-position-bottom) tps-65-top-middle-inner [0 0 0])
                                                                                            (calculate-point-between-points (tps-65-top-right-points :wall-locate-2-top) tps-65-top-middle-control-point-inner-flat control-vector)
                                                                                            (calculate-point-between-points (tps-65-top-right-points :wall-locate-2-bottom) screen-holder-top-left-inside-point control-vector)
                                                                                            (calculate-point-between-points (tps-65-top-right-points :wall-locate-2-bottom-floor) screen-holder-bottom-left-inside-floor-point [0 0 0])
                                                                                            steps)
                                                                            mid-curves (concat
                                                                                        (drop-last (bezier-quadratic
                                                                                                    tps-65-top-middle-inner
                                                                                                    tps-65-top-middle-control-point-inner-flat
                                                                                                    screen-holder-top-left-inside-point
                                                                                                    half-steps1))
                                                                                        (bezier-quadratic
                                                                                         screen-holder-top-left-inside-point
                                                                                         screen-holder-bottom-left-inside-point
                                                                                         screen-holder-bottom-left-inside-floor-point
                                                                                         half-steps2))]]
                                                                  (bezier-quadratic   (nth tps-65-top-right-curves-inner index) (nth control-points index) (nth mid-curves index)   steps))))
        tps-65-screen-side-bezier-along-bezier-point-outer (into []
                                                                 (apply concat

                                                                        (for [index (range 0 (inc steps))
                                                                              :let [top-points (bezier-linear tps-65-top-right-outer tps-65-top-middle-outer steps)
                                                                                    control-points-1 (bezier-linear tps-65-top-right-control-point-outer tps-65-top-middle-control-point-outer steps)
                                                                                    control-points-2 (bezier-quadratic tps-65-top-right-wall-locate2-inside-point-translated
                                                                                                                       tps-65-top-right-wall-locate2-inside-control-point-translated
                                                                                                                       screen-holder-top-left-inside-point-translated
                                                                                                                       steps)]]
                                                                          (bezier-cubic
                                                                           (nth top-points  index)
                                                                           (nth control-points-1  index)
                                                                           (nth control-points-2  index)
                                                                           (nth top-bezier-points index)
                                                                           steps))))

        tps-65-screen-side-bezier-along-bezier-point-inner (into []
                                                                 (apply concat
                                                                        (for [index (range 0 (inc steps))
                                                                              :let [top-points (bezier-linear tps-65-top-middle-inner tps-65-top-right-inner  steps)
                                                                                    control-points (bezier-linear tps-65-top-middle-control-point-inner-flat tps-65-top-right-control-point-inner  steps)]]
                                                                          (bezier-quadratic
                                                                           (nth top-points index)
                                                                           (nth control-points index)
                                                                           (nth top-inside-bezier-points index)
                                                                           steps))))

        tps-65-screen-side-bezier-along-bezier-middle-to-right-outer-points (into []
                                                                                  (apply concat
                                                                                         (for [index (range 0 (inc steps))
                                                                                               :let [tps-65-points (bezier-linear  tps-65-top-middle-outer tps-65-top-left-outer steps)
                                                                                                     tps-65-control-points (bezier-linear  tps-65-top-middle-control-point-outer tps-65-top-left-control-point-outer steps)
                                                                                                     screen-holder-control-points (bezier-linear screen-holder-top-left-inside-point-translated
                                                                                                                                                 screen-holder-top-right-inside-point-translated-with-same-y-as-tps-65-top-left-inner
                                                                                                                                                 steps)
                                                                                                     screen-holder-points (bezier-linear  screen-holder-top-left-outside-point screen-holder-outside-point-with-same-y-as-tps-65-top-left-upper steps)]]
                                                                                           (bezier-cubic
                                                                                            (nth tps-65-points index)
                                                                                            (nth tps-65-control-points index)
                                                                                            (nth screen-holder-control-points index)
                                                                                            (nth screen-holder-points index)
                                                                                            steps))))

        tps-65-screen-side-bezier-along-bezier-middle-to-screen-holder-right-inner-points (into []
                                                                                                (apply concat
                                                                                                       (for [index (range 0 (inc steps))
                                                                                                             :let [tps-65-points (bezier-linear tps-65-top-left-inner-with-same-z-as-screen-holder-top-right-inside-point-translated-with-same-y-as-tps-65-top-left-inner
                                                                                                                                                tps-65-top-middle-inner   steps)
                                                                                                     ;control-points (bezier-linear tps-65-top-left-control-point-inner tps-65-top-middle-control-point-inner   steps)
                                                                                                                   screen-holder-inner-points (bezier-linear screen-holder-inside-point-with-same-y-as-tps-65-top-left-inner screen-holder-top-left-inside-point steps)]]
                                                                                                         (bezier-linear
                                                                                                          (nth tps-65-points index)
                                                                                            ;(nth control-points index)
                                                                                                          (nth screen-holder-inner-points index)
                                                                                                          steps))))
        tps-65-screen-side-bezier-along-bezier-polyhedron (polyhedron (concat tps-65-screen-side-bezier-along-bezier-point-outer tps-65-screen-side-bezier-along-bezier-point-inner)
                                                                      (generate-bezier-along-bezier-polyhedron-faces
                                                                       tps-65-screen-side-bezier-along-bezier-point-outer
                                                                       tps-65-screen-side-bezier-along-bezier-point-inner steps))
        tps-65-screen-side-bezier-along-bezier-middle-to-screen-holder-right-polyhedron (polyhedron (concat tps-65-screen-side-bezier-along-bezier-middle-to-right-outer-points tps-65-screen-side-bezier-along-bezier-middle-to-screen-holder-right-inner-points)
                                                                                                    (generate-bezier-along-bezier-polyhedron-faces
                                                                                                     tps-65-screen-side-bezier-along-bezier-middle-to-right-outer-points
                                                                                                     tps-65-screen-side-bezier-along-bezier-middle-to-screen-holder-right-inner-points
                                                                                                     steps))
        front-points-count (count tps-65-top-right-to-screen-holder-outer)
        back-points-count (count tps-65-top-right-to-screen-holder-inner)
        front-points-start 0
        front-points-end (dec front-points-count)
        back-points-start (inc front-points-end)
        back-points-end (+ back-points-start back-points-count)
        ;; f1pts1 (fillet-about-point dx1 dy1 20)
        ;; f1ptsmid1 (fillet-about-point dxmid1 dymid1 20)
        ;; f1ptsmid2 (fillet-about-point dxmid2 dymid2 20)
        ;; f1pts2 (fillet-about-point dx2 dy2 20)
        ;; bezier-fn (fn [index]
        ;;             (bezier-cubic
        ;;              (place1  (mapv + (get-curve-corner-translation-vector post-position-1) (nth f1pts1 index)))
        ;;              (place-mid1    (mapv + (get-curve-corner-translation-vector post-position-mid1)  (nth f1ptsmid1 index)))
        ;;              (place-mid2    (mapv + (get-curve-corner-translation-vector post-position-mid2)  (nth f1ptsmid2 index)))
        ;;              (place2  (mapv + (get-curve-corner-translation-vector post-position-2) (nth f1pts2 index)))
        ;;              steps))
        ;; top-upper-curve-points (for [index (range 0 20)]  (bezier-fn index))
        ;; top-upper-curve-points-flattend (apply concat top-upper-curve-points)
        ;; top-upper-curve-faces (into []
        ;;                             (for [index (range 0 (- (count top-upper-curve-points-flattend) steps 1))]
        ;;                               [index (inc index) (+ (inc index) steps) (+ index steps)]))
        ;; top-upper-curve-polyhedron (polyhedron top-upper-curve-points-flattend top-upper-curve-faces)
        tps-65-top-right-to-screen-holder-outer-floor-points (reverse (filter #(= (nth % 2) 0.0) tps-65-top-right-to-screen-holder-outer))
        polyhedrons (union
                     tps-65-screen-side-bezier-along-bezier-middle-to-screen-holder-right-polyhedron
                     (polyhedron  (concat tps-65-top-right-to-screen-holder-outer  tps-65-top-right-to-screen-holder-inner)
                                  (into [] (concat
                                            (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps)
                                            (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps back-points-start)
                                            (bezier-along-bezier-polyhedron-generate-side-reverse (- front-points-end  steps) back-points-end steps)
                                            (bezier-along-bezier-polyhedron-generate-side-reverse-2 (+ 0  steps) back-points-start  steps)
                                            (bezier-along-bezier-polyhedron-generate-side-reverse-3  back-points-start (+ 0  steps)  steps (inc steps))
                                            (bezier-along-bezier-polyhedron-generate-side-reverse-3  front-points-start (+ back-points-start   steps)   steps  (inc steps))
                   ;(bezier-along-bezier-polyhedron-generate-side  front-points-start  back-points-start steps)
                                            ))))
        ] 
     (if (true? bottom-plate) tps-65-top-right-to-screen-holder-outer-floor-points polyhedrons)
    ;(println top-bezier-points) 
    
     ;tps-65-screen-side-bezier-along-bezier-polyhedron
     ;tps-65-screen-side-bezier-along-bezier-middle-to-screen-holder-right-polyhedron
     ;wall-curve
     ))

;; (def screen-holder-bottom-right-outside-floor-point-to-screen-holder-bottom-left-outside-floor-point
;;   screen-holder-bottom-right-outside-floor-point
;;   screen-holder-bottom-left-outside-floor-point
;;   )

(def under-screen
  (let
   [steps 20
    screen-holder-bottom-left-outside-point (transform-position
                                             (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                             (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
    screen-holder-bottom-left-inside-point (transform-position
                                            (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                            (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
    screen-holder-bottom-right-outside-point (transform-position
                                              (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                              (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
    screen-holder-bottom-right-inside-point (transform-position
                                             (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                             (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
    screen-holder-bottom-left-outside-floor-point (assoc (vec screen-holder-bottom-left-outside-point) 2 0)


    screen-holder-bottom-left-inside-floor-point (assoc (vec screen-holder-bottom-left-inside-point) 2 0)


    screen-holder-bottom-right-outside-floor-point (assoc (vec screen-holder-bottom-right-outside-point) 2 0)


    screen-holder-bottom-right-inside-floor-point (assoc (vec screen-holder-bottom-right-inside-point) 2 0)

    points [screen-holder-bottom-left-outside-point
            screen-holder-bottom-right-outside-point
            screen-holder-bottom-left-outside-floor-point
            screen-holder-bottom-right-outside-floor-point
            screen-holder-bottom-left-inside-point
            screen-holder-bottom-right-inside-point
            screen-holder-bottom-left-inside-floor-point
            screen-holder-bottom-right-inside-floor-point]

    faces [[0 3 2] [0 1 3]
           [4 7 5] [4 6 7]
           [4 5 1] [4 1 0]
           [2 7 6] [2 3 7]
           [4 2 6]  [4 0 2]
           [1 7 3] [1 5 7]]
   ;under-screen-polyhedron (polyhedron points faces)
    ]
    (polyhedron points faces))) 

(def screen-to-EVQWGD001
  (let [steps 72
        screen-holder-top-right-outside-point (transform-position
                                               (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                               (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
        screen-holder-top-right-inside-point (transform-position
                                              (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                              (mapv +  [0 0 (/ oled-holder-thickness 2)]))
        screen-holder-bottom-right-outside-point (transform-position
                                                  (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                                  (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))



        screen-holder-bottom-right-inside-point (transform-position
                                                 (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                                 (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))

        screen-holder-bottom-right-outside-floor-point (assoc (vec screen-holder-bottom-right-outside-point) 2 0)
        screen-holder-bottom-right-inside-floor-point (assoc (vec screen-holder-bottom-right-inside-point) 2 0)
        EVQWGD001-mount-top-left-outside (transform-position
                                          (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-left)
                                          (mapv + [(/ oled-post-size -1) (/ oled-post-size 1) (- (/ EVQWGD001-mount-y-modifier 1))] [0 0 (/ EVQWGD001-mount-height 2)]))
        EVQWGD001-mount-top-left-inside (transform-position
                                         (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-left)
                                         (mapv + [(/ oled-post-size -1) (/ oled-post-size 1) (- (/ EVQWGD001-mount-y-modifier 1))] [0 0 (/  (- plate-thickness) 2)]))
        EVQWGD001-mount-bottom-left-outside (transform-position
                                             (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left)
                                             (mapv + [(/ oled-post-size -1) (/ oled-post-size -1) (- (/ EVQWGD001-mount-y-modifier 1))] [0 0 (+  (/ EVQWGD001-mount-height 2))]))
        EVQWGD001-mount-bottom-left-inside (transform-position
                                            (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left)
                                            (mapv + [(/ oled-post-size -1) (/ oled-post-size -1) (- (/ EVQWGD001-mount-y-modifier 1))] [0 0 0]))
        EVQWGD001-mount-bottom-right-outside (transform-position
                                              (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right)
                                              (mapv + [(/ oled-post-size 1) (/ oled-post-size -1) (- (/ EVQWGD001-mount-y-modifier 1))] [0 0 (/  EVQWGD001-mount-height 2)]))
        EVQWGD001-mount-bottom-right-inside (transform-position
                                             (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right)
                                             (mapv + [(/ oled-post-size 1) (/ oled-post-size -1) (- (/ EVQWGD001-mount-y-modifier 1))] [0 0 (/  (- plate-thickness) 2)]))

        EVQWGD001-mount-top-right-outside (transform-position
                                           (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right)
                                           (mapv + [(/ oled-post-size 1) (/ oled-post-size 1) (- (/ EVQWGD001-mount-y-modifier 1))] [0 0 (/  EVQWGD001-mount-height 2)]))
        EVQWGD001-mount-top-right-inside (transform-position
                                          (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right)
                                          (mapv + [(/ oled-post-size 1) (/ oled-post-size 1) (- (/ EVQWGD001-mount-y-modifier 1))]  [0 0 (/  (- plate-thickness) 2)]))

        EVQWGD001-mount-bottom-right-outside-floor (translate-to-floor EVQWGD001-mount-bottom-right-outside)
        EVQWGD001-mount-bottom-right-inside-floor (translate-to-floor EVQWGD001-mount-bottom-right-inside)
        thumb-bl-tl-outside (transform-position
                             (partial thumb-bl-place) (mapv + (wall-locate3-for-polyhedron-point -1 0) curve-post-tl-translation-vector curve-post-translation-vector [(/ curve-post-size 2) (/ curve-post-size 2) (- oled-holder-thickness curve-post-size)]))
        thumb-bl-tl-inside (transform-position
                            (partial thumb-bl-place) (mapv + (wall-locate2 -1 0) oled-post-tl-translation-vector oled-translation-vector [(/ oled-post-size 2) (/ oled-post-size 2)  0]))
        thumb-bl-tl-outside-floor (assoc (vec thumb-bl-tl-outside) 2 0)
        thumb-bl-tl-inside-floor (assoc (vec thumb-bl-tl-inside) 2 0)
        thumb-bl-tl-web-post-bottom (transform-position (partial thumb-bl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        thumb-bl-tr-web-post-top (transform-position (partial thumb-bl-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-bl-tr-web-post-bottom (transform-position (partial thumb-bl-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        thumb-tl-tl-web-post-top (transform-position (partial thumb-tl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-tl-tl-web-post-bottom (transform-position (partial thumb-tl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        ;tps-65-bottom-edge
        trackpad-thumb-side-points-left (bezier-linear
                                         (transform-position
                                          (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius)  tps-65-mount-corner-radius)
                                          (mapv + oled-translation-vector [(/ oled-post-size 2) (/ oled-post-size 2) (/ oled-holder-thickness 2)]))

                                         (transform-position
                                          (partial tps-65-translate-and-place-with-radius [(- tps-65-corner-radius (/ tps-65-width 2))
                                                                                           (/ (- (/ tps-65-length 2) tps-65-corner-radius) 2)
                                                                                           0] (- tps-65-mount-corner-radius)  0)
                                          (mapv + oled-translation-vector [(/ oled-post-size 2) 0 (/ oled-holder-thickness 2)]))
                                         steps)

        trackpad-thumb-side-points-left-lower (bezier-linear


                                               (transform-position
                                                (partial tps-65-translate-and-place-with-radius [(- tps-65-corner-radius (/ tps-65-width 2))
                                                                                                 (/ (- (/ tps-65-length 2) tps-65-corner-radius) 2)
                                                                                                 0] (- tps-65-mount-corner-radius)  0)
                                                (mapv + oled-translation-vector [(/ oled-post-size 2) 0 (/ oled-holder-thickness -2)]))
                                               (transform-position
                                                (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius)  tps-65-mount-corner-radius)
                                                (mapv + oled-translation-vector [(/ oled-post-size 2) (/ oled-post-size 2) (/ oled-holder-thickness -2)]))
                                               steps)
        trackpad-thumb-side-points-right (bezier-linear
                                          (nth trackpad-thumb-side-points-left  steps)
                                          ;; (transform-position
                                          ;;  (partial tps-65-translate-and-place-with-radius [(- tps-65-corner-radius (/ tps-65-width 2))
                                          ;;                                                   (/ (- (/ tps-65-length 2) tps-65-corner-radius) 2)
                                          ;;                                                   0] (- tps-65-mount-corner-radius)  0)
                                          ;;  (mapv + oled-translation-vector [(/ oled-post-size -2) 0 (/ oled-holder-thickness 2)]))

                                          (transform-position
                                           (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius)  (- tps-65-mount-corner-radius))
                                           (mapv + oled-translation-vector [0.4 (/ oled-post-size -2) (/ oled-holder-thickness 2)]))
                                          steps)
        trackpad-thumb-side-points-right-lower (bezier-linear
                                                (transform-position
                                                 (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius)  (- tps-65-mount-corner-radius))
                                                 (mapv + oled-translation-vector [0.4 (/ oled-post-size -2) (/ oled-holder-thickness -2)]))

                                                (transform-position
                                                 (partial tps-65-translate-and-place-with-radius [(- tps-65-corner-radius (/ tps-65-width 2))
                                                                                                  (/ (- (/ tps-65-length 2) tps-65-corner-radius) 2)
                                                                                                  0] (- tps-65-mount-corner-radius)  0)
                                                 (mapv + oled-translation-vector [(/ oled-post-size 2) 0 (/ oled-holder-thickness -2)]))
                                                steps)


        upper-outside-control-point (mapv + [0 -2 0] (mapv + EVQWGD001-mount-top-left-outside (mapv  (fn [point] (/ point 2)) (mapv - screen-holder-top-right-outside-point EVQWGD001-mount-top-left-outside))))
        upper-inside-control-point (mapv + [0 -2 0] (mapv + EVQWGD001-mount-top-left-inside (mapv  (fn [point] (/ point 2)) (mapv - screen-holder-top-right-inside-point EVQWGD001-mount-top-left-inside))))
        mid-inside-control-point (mapv + EVQWGD001-mount-bottom-left-inside [0 -2 0] (mapv (fn [point] (/ point 2)) (mapv - screen-holder-bottom-right-inside-point EVQWGD001-mount-bottom-left-inside)))
        mid-outside-control-point (mapv + EVQWGD001-mount-bottom-left-outside [0 -2 0] (mapv (fn [point] (/ point 2)) (mapv - screen-holder-bottom-right-outside-point EVQWGD001-mount-bottom-left-outside)))
        bottom-outside-control-point (mapv + EVQWGD001-mount-bottom-right-outside [0 -2 0] (mapv (fn [point] (/ point 2)) (mapv - screen-holder-bottom-right-outside-floor-point EVQWGD001-mount-bottom-right-outside)))
        bottom-inside-control-point (mapv + EVQWGD001-mount-bottom-right-inside [0 -2 0] (mapv (fn [point] (/ point 2)) (mapv - screen-holder-bottom-right-inside-floor-point EVQWGD001-mount-bottom-right-inside)))
        screen-holder-bottom-right-outside-floor-to-EVQWGD001-mount-bottom-right-floor-outside-control (calculate-point-between-points screen-holder-bottom-right-outside-floor-point EVQWGD001-mount-bottom-right-inside-floor [0 -2 0])
        screen-holder-bottom-right-inside-floor-to-EVQWGD001-mount-bottom-right-floor-inside-control (calculate-point-between-points screen-holder-bottom-right-inside-floor-point EVQWGD001-mount-bottom-right-inside-floor [0 -2 0])
        EVQWGD001-mount-bottom-right-outside-to-thumb-bl-tl-outside-control-point (calculate-point-between-points EVQWGD001-mount-bottom-right-outside thumb-bl-tl-outside [0 -2 0])
        EVQWGD001-mount-bottom-right-inside-to-thumb-bl-tl-inside-control-point (calculate-point-between-points EVQWGD001-mount-bottom-right-inside thumb-bl-tl-inside [0 -2 0])
        EVQWGD001-mount-bottom-right-outside-floor-to-thumb-bl-tl-outside-floor-control-point (calculate-point-between-points EVQWGD001-mount-bottom-right-outside-floor thumb-bl-tl-outside-floor [0 -2 0])
        EVQWGD001-mount-bottom-right-inside-floor-to-thumb-bl-tl-inside-floor-control-point (calculate-point-between-points EVQWGD001-mount-bottom-right-inside-floor thumb-bl-tl-inside-floor [0 -2 0])
        EVQWGD001-mount-top-right-outside-to-thumb-bl-tr-web-post-top-control-point (calculate-point-between-points EVQWGD001-mount-top-right-outside thumb-bl-tr-web-post-top [0 1 0])
        EVQWGD001-mount-top-right-inside-to-thumb-bl-tr-web-post-bottom-control-point (calculate-point-between-points EVQWGD001-mount-top-right-inside thumb-bl-tr-web-post-bottom [0 1 0])
        EVQWGD001-mount-points (concat (bezier-quadratic screen-holder-top-right-outside-point upper-outside-control-point EVQWGD001-mount-top-left-outside (/ steps 2))
                                       (bezier-quadratic EVQWGD001-mount-top-left-outside (calculate-point-between-points EVQWGD001-mount-top-left-outside
                                                                                                                          thumb-tl-tl-web-post-top [0 1 0]) thumb-tl-tl-web-post-top (/ steps 2)))


        thumb-side-trackpad-to-EVQWGD001-linear-control-points-left (bezier-linear
                                                                     (transform-position
                                                                      (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset)
                                                                      (mapv + oled-translation-vector [(/ oled-post-size -2) (/ oled-post-size -2) (/ oled-holder-thickness 2)]))
                                                                     (transform-position
                                                                      (partial tps-65-translate-and-place-with-radius [(- tps-65-corner-radius (/ tps-65-width 2))
                                                                                                                       (/ (- (/ tps-65-length 2) tps-65-corner-radius) 2)
                                                                                                                       0] (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset)
                                                                      (mapv + oled-translation-vector [(/ oled-post-size -2) (/ oled-post-size -2) (/ oled-holder-thickness 2)]))
                                                                     steps)
        thumb-side-trackpad-to-EVQWGD001-linear-control-points-right (bezier-linear
                                                                      (transform-position
                                                                       (partial tps-65-translate-and-place-with-radius [(- tps-65-corner-radius (/ tps-65-width 2))
                                                                                                                        (/ (- (/ tps-65-length 2) tps-65-corner-radius) 2)
                                                                                                                        0] (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset)
                                                                       (mapv + oled-translation-vector [(/ oled-post-size -2) (/ oled-post-size -2) (/ oled-holder-thickness 2)]))
                                                                      (transform-position
                                                                       (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-bottom-left-position (- tps-65-mount-corner-radius-with-offset-mod-neg)  (- tps-65-mount-corner-radius-with-offset-mod-neg))
                                                                       (mapv + oled-translation-vector [(/ oled-post-size 2) (/ oled-post-size -2) (/ oled-holder-thickness 2)]))
                                                                      steps)

        trackpad-thumb-side-to-screen-holder-top-right-to-EVQWGD001-mount-top-left-to-thumb-tl-tl-front-faces (into [] (concat  (for [index-outer (range 0 steps) index-inner (range 0   steps)]
                                                                                                                                  [(+ (* index-outer (inc steps)) index-inner) (+ (* (inc index-outer) (inc steps)) (inc index-inner)) (+ (* index-outer (inc steps)) (inc index-inner))])
                                                                                                                                (for [index-outer (range 0  steps) index-inner (range 0   steps)]
                                                                                                                                  [(+ (* index-outer (inc steps)) index-inner) (+ (* (inc index-outer) (inc steps)) index-inner) (+ (* (inc index-outer) (inc steps)) (inc index-inner))])))


        thumb-bl-tl-curve (map #(transform-position
                                 (partial thumb-bl-place) %)  (fillet-about-point -1 0 steps (mapv +  curve-post-tl-translation-vector curve-post-translation-vector [(/ curve-post-size -2) (/ curve-post-size 2) (+ curve-post-size)])))
        EVQWGD001-mount-bottom-right-outside-to-thumb-bl-outside (bezier-quadratic  EVQWGD001-mount-bottom-right-outside (calculate-point-between-points EVQWGD001-mount-bottom-right-outside thumb-bl-tl-outside [0 1 0]) thumb-bl-tl-outside  steps)
        EVQWGD001-mount-bottom-right-outside-to-thumb-bl-web-post-tl (bezier-linear
                                                                      EVQWGD001-mount-bottom-right-outside
                                                                      (transform-position (partial thumb-bl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
                                                                      steps)
        EVQWGD001-mount-bottom-right-outside-to-thumb-bl-to-top-of-tl-curve (bezier-linear
                                                                             EVQWGD001-mount-bottom-right-outside
                                                                             (transform-position
                                                                              (partial thumb-bl-place)  (nth (fillet-about-point -1 0 steps (mapv +  curve-post-tl-translation-vector curve-post-translation-vector [(/ curve-post-size -2) (/ curve-post-size 2) (+ curve-post-size)])) 0))
                                                                             steps)
        EVQWGD001-mount-top-right-outside-to-thumb-bl-tr-web-post-top (bezier-quadratic
                                                                       EVQWGD001-mount-top-right-outside EVQWGD001-mount-top-right-outside-to-thumb-bl-tr-web-post-top-control-point thumb-bl-tr-web-post-top steps)



        screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-points (bezier-quadratic screen-holder-top-right-outside-point upper-outside-control-point EVQWGD001-mount-top-left-outside steps)
        screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-points (bezier-quadratic EVQWGD001-mount-top-left-inside upper-inside-control-point screen-holder-top-right-outside-point  steps)
        screen-holder-bottom-right-outside-to-EVQWGD001-mount-bottom-left-outside-points (bezier-quadratic screen-holder-bottom-right-outside-point mid-outside-control-point EVQWGD001-mount-bottom-left-outside steps)
        screen-holder-bottom-right-inside-to-EVQWGD001-mount-bottom-left-inside-points (bezier-quadratic  EVQWGD001-mount-bottom-left-inside mid-inside-control-point screen-holder-bottom-right-inside-point steps)
        screen-holder-bottom-right-outside-floor-to-EVQWGD001-mount-bottom-right-outside-points (bezier-quadratic screen-holder-bottom-right-outside-floor-point   bottom-outside-control-point EVQWGD001-mount-bottom-right-outside steps)
        screen-holder-bottom-right-inside-floor-to-EVQWGD001-mount-bottom-right-inside-points (bezier-quadratic EVQWGD001-mount-bottom-right-inside bottom-inside-control-point screen-holder-bottom-right-inside-floor-point  steps)
        EVQWGD001-mount-top-left-outside-to-thumb-tl-tl-web-post-top (bezier-quadratic EVQWGD001-mount-top-left-outside
                                                                                       (calculate-point-between-points thumb-tl-tl-web-post-top EVQWGD001-mount-top-left-outside  [0 1 0])
                                                                                       thumb-tl-tl-web-post-top
                                                                                       steps)
        thumb-tl-tl-web-post-bottom-to-EVQWGD001-mount-top-left-inside (bezier-quadratic
                                                                        thumb-tl-tl-web-post-bottom
                                                                        (calculate-point-between-points EVQWGD001-mount-top-left-inside thumb-tl-tl-web-post-bottom [0 1 0])
                                                                        EVQWGD001-mount-top-left-inside
                                                                        steps)
        screen-holder-bottom-right-inside-floor-to-EVQWGD001-mount-bottom-right-floor-inside-points (bezier-quadratic EVQWGD001-mount-bottom-right-inside-floor bottom-inside-control-point screen-holder-bottom-right-inside-floor-point  steps)

        thumb-side-trackpad-to-EVQWGD001-control-points-left (for [index (range 0 (+ steps 1))]
                                                               (calculate-point-between-points (nth trackpad-thumb-side-points-left index) (nth screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-points index) [0 0 1]))
        thumb-side-trackpad-to-EVQWGD001-control-points-left-lower (for [index (range 0 (+ steps 1))]
                                                                     (calculate-point-between-points (nth trackpad-thumb-side-points-left-lower index) (nth screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-points index) [0 0 1]))
        thumb-side-trackpad-to-EVQWGD001-control-points-right (for [index (range 0 (+ steps 1))]
                                                                (calculate-point-between-points (nth trackpad-thumb-side-points-right index) (nth EVQWGD001-mount-top-left-outside-to-thumb-tl-tl-web-post-top index) [0 0 1]))
        thumb-side-trackpad-to-EVQWGD001-control-points-right-lower (for [index (range 0 (+ steps 1))]
                                                                      (calculate-point-between-points (nth trackpad-thumb-side-points-right-lower index) (nth thumb-tl-tl-web-post-bottom-to-EVQWGD001-mount-top-left-inside index) [0 0 1]))


        trackpad-thumb-side-left-to-screen-holder-top-right-to-EVQWGD001-mount-top-left-points (into [] (apply concat (for [index (range 0 (+ steps 1))]
                                                                                                                        (bezier-cubic
                                                                                                                         (nth trackpad-thumb-side-points-left index)
                                                                                                                         (nth thumb-side-trackpad-to-EVQWGD001-linear-control-points-left index)
                                                                                                                         (nth thumb-side-trackpad-to-EVQWGD001-control-points-left index)
                                                                                                                         (nth screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-points index)
                                                                                                                         steps))))

        trackpad-thumb-side-left-to-EVQWGD001-mount-top-left-to-screen-holder-top-right-to-points (into [] (apply concat (for [index (range 0 (+ steps 1))
                                                                                                                               :let [trackpad-thumb-side-points-left-reverse (reverse trackpad-thumb-side-points-left)
                                                                                                                                     control-points-reverse (reverse thumb-side-trackpad-to-EVQWGD001-control-points-left)]]
                                                                                                                           (bezier-quadratic
                                                                                                                            (nth trackpad-thumb-side-points-left-lower
                                                                                                                                 index)
                                                                                                                            (nth  thumb-side-trackpad-to-EVQWGD001-control-points-left-lower index)
                                                                                                                            (nth screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-points index)
                                                                                                                            steps))))

        trackpad-thumb-side-to-EVQWGD001-mount-top-left-to-thumb-tl-tl-steps (into [] (apply concat (for [index (range 0 (+  steps 1))]
                                                                                                      (bezier-cubic
                                                                                                       (nth trackpad-thumb-side-points-right index)
                                                                                                       (nth thumb-side-trackpad-to-EVQWGD001-linear-control-points-right index)
                                                                                                       (nth thumb-side-trackpad-to-EVQWGD001-control-points-right index)
                                                                                                       (nth EVQWGD001-mount-top-left-outside-to-thumb-tl-tl-web-post-top index)
                                                                                                       steps))))

        trackpad-thumb-side-to-thumb-tl-tl-bottom-to-EVQWGD001-mount-top-left-points (into [] (apply concat (for [index (range 0 (+  steps 1))
                                                                                                                  :let [trackpad-thumb-side-points-right-reverse (reverse trackpad-thumb-side-points-right)
                                                                                                                        thumb-side-trackpad-to-EVQWGD001-control-points-right-reverse
                                                                                                                        (reverse thumb-side-trackpad-to-EVQWGD001-control-points-right)]]
                                                                                                              (bezier-quadratic
                                                                                                               (nth trackpad-thumb-side-points-right-lower index)
                                                                                                               (nth thumb-side-trackpad-to-EVQWGD001-control-points-right-lower index)
                                                                                                               (nth  thumb-tl-tl-web-post-bottom-to-EVQWGD001-mount-top-left-inside  index)
                                                                                                               steps))))


        ;;          EVQWGD001-mount-bottom-right-outside-to-thumb-bl-tl-outside-points (bezier-quadratic EVQWGD001-mount-bottom-right-outside )
;;  EVQWGD001-mount-bottom-right-inside-to-thumb-bl-tl-inside-points (bezier-quadratic)
;;  EVQWGD001-mount-bottom-right-outside-floor-to-thumb-bl-tl-outside-floor-points (bezier-quadratic)
;;  EVQWGD001-mount-bottom-right-inside-floor-to-thumb-bl-tl-inside-floor-points (bezier-quadratic)
        upper-points-start 0
        upper-points-end (dec (count screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-points))
        mid-points-size (count screen-holder-bottom-right-outside-to-EVQWGD001-mount-bottom-left-outside-points)
        mid-points-start (inc upper-points-end)
        mid-points-end (dec (+ mid-points-start (count screen-holder-bottom-right-outside-to-EVQWGD001-mount-bottom-left-outside-points)))
        bottom-outside-points-start (inc mid-points-end)
        bottom-outside-points-end (dec (+ bottom-outside-points-start (count screen-holder-bottom-right-outside-floor-to-EVQWGD001-mount-bottom-right-outside-points)))
        upper-wall-curve-points (into [] (concat screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-points screen-holder-bottom-right-outside-to-EVQWGD001-mount-bottom-left-outside-points screen-holder-bottom-right-outside-floor-to-EVQWGD001-mount-bottom-right-outside-points))
        screen-holder-top-and-bottom-to-EVQWGD001-mount-top-and-bottom-left-polyhedron (generate-polyhedron-from-points
                                                                                        screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-points
                                                                                        screen-holder-bottom-right-outside-to-EVQWGD001-mount-bottom-left-outside-points
                                                                                        screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-points
                                                                                        screen-holder-bottom-right-inside-to-EVQWGD001-mount-bottom-left-inside-points
                                                                                        steps)
        screen-holder-bottom-and-floor-point-to-EVQWGD001-bottom-left-and-right-polyhedron (generate-polyhedron-from-points
                                                                                            screen-holder-bottom-right-outside-to-EVQWGD001-mount-bottom-left-outside-points
                                                                                            screen-holder-bottom-right-outside-floor-to-EVQWGD001-mount-bottom-right-outside-points
                                                                                            screen-holder-bottom-right-inside-to-EVQWGD001-mount-bottom-left-inside-points
                                                                                            screen-holder-bottom-right-inside-floor-to-EVQWGD001-mount-bottom-right-inside-points
                                                                                            steps)
        screen-holder-bottom-and-floor-point-to-EVQWGD001-bottom-right-and-floor-polyhedron (generate-bezier-quadratic-polyhedron-from-points
                                                                                             screen-holder-bottom-right-outside-point EVQWGD001-mount-bottom-right-outside screen-holder-bottom-right-outside-floor-point EVQWGD001-mount-bottom-right-outside-floor
                                                                                             screen-holder-bottom-right-inside-point EVQWGD001-mount-bottom-right-inside screen-holder-bottom-right-inside-floor-point EVQWGD001-mount-bottom-right-inside-floor
                                                                                             steps)
        EVQWGD001-bottom-right-and-floor-thumb-bl-tl-and-floor-polyhedron (generate-bezier-quadratic-polyhedron-from-points-and-control-vectors
                                                                           EVQWGD001-mount-bottom-right-outside thumb-bl-tl-outside
                                                                           EVQWGD001-mount-bottom-right-outside-floor thumb-bl-tl-outside-floor
                                                                           EVQWGD001-mount-bottom-right-inside thumb-bl-tl-inside
                                                                           EVQWGD001-mount-bottom-right-inside-floor thumb-bl-tl-inside-floor
                                                                           steps
                                                                           {:outside-upper-control-point-vector [0 1 0] :outside-lower-control-point-vector [0 1 0]
                                                                            :inside-upper-control-point-vector [0 1 0] :inside-lower-control-point-vector [0 1 0]})
        EVQWGD001-mount-bottom-right-to-thumb-bl-polyhedron (generate-polyhedron-from-points

                                                             thumb-bl-tl-curve
                                                             EVQWGD001-mount-bottom-right-outside-to-thumb-bl-outside

                                                             (bezier-linear
                                                              thumb-bl-tl-inside
                                                              thumb-bl-tl-web-post-bottom
                                                              steps)
                                                             (bezier-quadratic thumb-bl-tl-inside (calculate-point-between-points EVQWGD001-mount-bottom-right-inside thumb-bl-tl-inside  [0 1 0])   EVQWGD001-mount-bottom-right-inside steps)



                                                             steps)

        EVQWGD001-mount-bottom-right-to-thumb-bl-web-post-tl-polyhedron (generate-polyhedron-from-points
                                                                         EVQWGD001-mount-bottom-right-outside-to-thumb-bl-web-post-tl
                                                                         EVQWGD001-mount-bottom-right-outside-to-thumb-bl-to-top-of-tl-curve

                                                                         (bezier-linear

                                                                          thumb-bl-tl-web-post-bottom
                                                                          EVQWGD001-mount-bottom-right-inside
                                                                          steps)
                                                                         (bezier-linear
                                                                          thumb-bl-tl-web-post-bottom
                                                                          EVQWGD001-mount-bottom-right-inside
                                                                          steps)
                                                                         steps)

        EVQWGD001-mount-left-right-to-thumb-bl-web-post-tr-polyhedron (generate-polyhedron-from-points
                                                                       EVQWGD001-mount-top-right-outside-to-thumb-bl-tr-web-post-top
                                                                       EVQWGD001-mount-bottom-right-outside-to-thumb-bl-web-post-tl
                                                                       (bezier-quadratic
                                                                        thumb-bl-tr-web-post-bottom EVQWGD001-mount-top-right-inside-to-thumb-bl-tr-web-post-bottom-control-point EVQWGD001-mount-top-right-inside  steps)
                                                                       (bezier-linear

                                                                        thumb-bl-tl-web-post-bottom
                                                                        EVQWGD001-mount-bottom-right-inside
                                                                        steps)
                                                                       steps)
        EVQWGD001-mount-top-left-and-top-right-to-thumb-bl-web-post-tr-and-thumb-tl-web-post-tl-polyhedron (generate-bezier-quadratic-polyhedron-from-points-and-control-vectors
                                                                                                            EVQWGD001-mount-top-left-outside
                                                                                                            thumb-tl-tl-web-post-top
                                                                                                            EVQWGD001-mount-top-right-outside
                                                                                                            thumb-bl-tr-web-post-top
                                                                                                            EVQWGD001-mount-top-left-inside
                                                                                                            thumb-tl-tl-web-post-bottom
                                                                                                            EVQWGD001-mount-top-right-inside
                                                                                                            thumb-bl-tr-web-post-bottom
                                                                                                            steps
                                                                                                            {:outside-upper-control-point-vector [0 1 0] :outside-lower-control-point-vector [0 1 0]
                                                                                                             :inside-upper-control-point-vector [0 1 0] :inside-lower-control-point-vector [0 1 0]})
        trackpad-thumb-side-to-screen-holder-top-right-to-EVQWGD001-mount-top-left-polyhedron (polyhedron
                                                                                               (concat trackpad-thumb-side-left-to-screen-holder-top-right-to-EVQWGD001-mount-top-left-points
                                                                                                       trackpad-thumb-side-left-to-EVQWGD001-mount-top-left-to-screen-holder-top-right-to-points)
                                                                                               (generate-bezier-along-bezier-polyhedron-faces trackpad-thumb-side-left-to-screen-holder-top-right-to-EVQWGD001-mount-top-left-points
                                                                                                                                              trackpad-thumb-side-left-to-EVQWGD001-mount-top-left-to-screen-holder-top-right-to-points
                                                                                                                                              steps))
        trackpad-thumb-side-to-EVQWGD001-mount-top-left-to-thumb-tl-tl-polyhedron (polyhedron
                                                                                   (concat trackpad-thumb-side-to-EVQWGD001-mount-top-left-to-thumb-tl-tl-steps trackpad-thumb-side-to-thumb-tl-tl-bottom-to-EVQWGD001-mount-top-left-points)
                                                                                   (generate-bezier-along-bezier-polyhedron-faces trackpad-thumb-side-to-EVQWGD001-mount-top-left-to-thumb-tl-tl-steps
                                                                                                                                  trackpad-thumb-side-to-thumb-tl-tl-bottom-to-EVQWGD001-mount-top-left-points
                                                                                                                                  steps))
        ;; EVQWGD001-mount-bottom-right-and-floor-to-thumb-bl-tl-and-floor-polyhedron (generate-bezier-polyhedron)
        wall-curve-faces (fn [outside-upper-start outside-upper-end outside-lower-start outside-lower-end inner-upper-start inner-upper-end inner-lower-start inner-lower-end steps] (into [] (concat

                                                                                                                                                                                               (for [index (range outside-upper-start outside-upper-end)]
                                                                                                                                                                                                 [index (inc index) (+ (inc index) (inc steps))])
                                                                                                                                                                                               (for [index (range outside-lower-start outside-lower-end)]
                                                                                                                                                                                                 [(- index (inc steps)) (inc index) index])

                                  ;;  (for [index (range mid-points-start mid-points-end)]
                                  ;;    [index (inc index) (+ (inc index) mid-points-size)]
                                  ;;    )
                                  ;;  (for [index (range bottom-outside-points-start bottom-outside-points-end)]
                                  ;;    [(- index (inc steps)) (inc index) index ])
                                                                                                                                                                                               )))
        upper-wall-curve (polyhedron upper-wall-curve-points wall-curve-faces)]
    ;(println  (count trackpad-thumb-side-to-screen-holder-top-right-to-EVQWGD001-mount-top-left-to-thumb-tl-tl-points))

    (union
     screen-holder-top-and-bottom-to-EVQWGD001-mount-top-and-bottom-left-polyhedron
     screen-holder-bottom-and-floor-point-to-EVQWGD001-bottom-left-and-right-polyhedron
     screen-holder-bottom-and-floor-point-to-EVQWGD001-bottom-right-and-floor-polyhedron
     EVQWGD001-bottom-right-and-floor-thumb-bl-tl-and-floor-polyhedron
     ;wall-curve
     EVQWGD001-mount-bottom-right-to-thumb-bl-polyhedron
     EVQWGD001-mount-bottom-right-to-thumb-bl-web-post-tl-polyhedron
     EVQWGD001-mount-left-right-to-thumb-bl-web-post-tr-polyhedron
     EVQWGD001-mount-top-left-and-top-right-to-thumb-bl-web-post-tr-and-thumb-tl-web-post-tl-polyhedron
     trackpad-thumb-side-to-screen-holder-top-right-to-EVQWGD001-mount-top-left-polyhedron
     trackpad-thumb-side-to-EVQWGD001-mount-top-left-to-thumb-tl-tl-polyhedron

     ;thumb-side-to-trackpad-mount
     )))

(defn left-section-front-polyhedron [steps]
  (let [screen-holder-top-right-outside-point (transform-position
                                               (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                               (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
        screen-holder-top-right-inside-point (transform-position
                                              (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                              (mapv +  [0 0 (/ oled-holder-thickness 2)]))
        

        screen-holder-bottom-right-inside-point (transform-position
                                                 (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                                 (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))

        
        screen-holder-bottom-right-inside-floor-point (assoc (vec screen-holder-bottom-right-inside-point) 2 0)
        tps-65-mount-top-left-upper (transform-position
                                     (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-left-position)
                                     [(- (+ tps-65-corner-radius 0.05))  (+ tps-65-corner-radius 0.05) 0])

        tps-65-mount-top-left-upper-control-point (transform-position
                                                   (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-left-position)
                                                   [(- (+ (* tps-65-mount-corner-radius-with-offset 2) 0.05))  (+ (* tps-65-mount-corner-radius-with-offset 2) 0.05) 0])
        tps-65-mount-top-left-lower (transform-position
                                     (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-left-position)
                                     [(- (+ tps-65-corner-radius 0.05))  (+ tps-65-corner-radius 0.05) (- (/ web-thickness 2))])
        tps-65-mount-bottom-left-upper (transform-position
                                        (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-bottom-left-position)
                                        [(- (+ tps-65-corner-radius 0.05))  (- (+ tps-65-corner-radius 0.05)) 0])
        tps-65-mount-bottom-left-upper-control-pont (transform-position
                                                     (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-bottom-left-position)
                                                     [(- (+ (* tps-65-mount-corner-radius-with-offset 2) 0.05))  (- (+ (* tps-65-mount-corner-radius-with-offset 2) 0.05)) 0])
        tps-65-mount-bottom-left-lower (transform-position
                                        (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-bottom-left-position)
                                        [(- (+ tps-65-corner-radius 0.05))  (- (+ tps-65-corner-radius 0.05)) (- (/ web-thickness 2))])
        tps-65-mount-top-right-upper    (transform-position
                                         (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-right-position)
                                         [(+ tps-65-corner-radius 0.05)  (+ tps-65-corner-radius 0.05) 0])
        tps-65-mount-top-right-lower    (transform-position
                                         (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-right-position)
                                         [(+ tps-65-corner-radius 0.05)  (+ tps-65-corner-radius 0.05) (/ web-thickness -2)])

        EVQWGD001-mount-top-left-outside (transform-position
                                          (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-left)
                                          (mapv + [(/ oled-post-size -1) (/ oled-post-size 1) (- (/ EVQWGD001-mount-y-modifier 1))] [0 0 (/ EVQWGD001-mount-height 2)]))
        EVQWGD001-mount-top-left-inside (transform-position
                                         (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-left)
                                         (mapv + [(/ oled-post-size -1) (/ oled-post-size 1) (- (/ EVQWGD001-mount-y-modifier 1))] [0 0 (+  (/ EVQWGD001-mount-height 4))]))
        EVQWGD001-mount-bottom-left-outside (transform-position
                                             (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left)
                                             (mapv + [(/ oled-post-size -1) (/ oled-post-size -1) (- (/ EVQWGD001-mount-y-modifier 1))] [0 0 (+  (/ EVQWGD001-mount-height 2))]))
        EVQWGD001-mount-bottom-left-inside (transform-position
                                            (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left)
                                            (mapv + [(/ oled-post-size -1) (/ oled-post-size -1) (- (/ EVQWGD001-mount-y-modifier 1))] [0 0 (+  (/ EVQWGD001-mount-height 4))]))
        
        EVQWGD001-mount-bottom-right-inside (transform-position
                                             (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right)
                                             (mapv + [(/ oled-post-size 1) (/ oled-post-size -1) (- (/ EVQWGD001-mount-y-modifier 1))] [0 0 0]))

        EVQWGD001-mount-top-right-outside (transform-position
                                           (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right)
                                           (mapv + [(/ oled-post-size 1) (/ oled-post-size 1) (- (/ EVQWGD001-mount-y-modifier 1))] [0 0 (/  EVQWGD001-mount-height 2)]))
        EVQWGD001-mount-top-right-inside (transform-position
                                          (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right)
                                          (mapv + [(/ oled-post-size 1) (/ oled-post-size 1) (- (/ EVQWGD001-mount-y-modifier 1))]  [0 0 0]))
        EVQWGD001-mount-left-control-point-scale-factor (/ 1 20)
        EVQWGD001-mount-right-control-point-scale-factor (/ 1 20)
        EVQWGD001-mount-top-left-outside-control-point (mapv + (mapv #(* % EVQWGD001-mount-left-control-point-scale-factor) (mapv -  EVQWGD001-mount-top-left-outside EVQWGD001-mount-top-right-outside)) EVQWGD001-mount-top-left-outside)
        EVQWGD001-mount-bottom-left-outside-control-point (mapv + (mapv #(* % EVQWGD001-mount-left-control-point-scale-factor) (mapv -  EVQWGD001-mount-bottom-left-outside EVQWGD001-mount-bottom-right-outside)) EVQWGD001-mount-bottom-left-outside) 
        EVQWGD001-mount-bottom-right-inside-floor (translate-to-floor EVQWGD001-mount-bottom-right-inside)
        EVQWGD001-mount-bottom-mid-outside (transform-position
                                            (partial EVQWGD001-translate-and-place-at-position [0 (- (/ EVQWGD001-mount-length 2)) 1])
                                            (mapv + [0 (/ oled-post-size -1) (- (/ EVQWGD001-mount-y-modifier 1))] [0 0 (/  EVQWGD001-mount-height 2)]))
        EVQWGD001-mount-bottom-mid-outside-floor (assoc (vec EVQWGD001-mount-bottom-mid-outside) 2 0)
        EVQWGD001-mount-bottom-mid-outside-floor-control-point (mapv + [0 -1 0] EVQWGD001-mount-bottom-mid-outside-floor)
        thumb-bl-tl (wall-brace-polyhedron-curve-points thumb-bl-place -1 0 "tl" :degrees steps)
        thumb-bl-tl-outside (thumb-bl-tl :outer-points)
        thumb-bl-tl-inside (thumb-bl-tl :inner-points)
        thumb-bl-tl-wall-locate3-floor (last thumb-bl-tl-outside)
        thumb-bl-tl-outside-floor (assoc (vec thumb-bl-tl-outside) 2 0)
        thumb-bl-tl-inside-floor (assoc (vec thumb-bl-tl-inside) 2 0)
        thumb-bl-tl-web-post-top (transform-position (partial thumb-bl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-bl-tl-web-post-bottom (transform-position (partial thumb-bl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        thumb-bl-tr-web-post-top (transform-position (partial thumb-bl-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-bl-tr-web-post-bottom (transform-position (partial thumb-bl-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        thumb-tl-tl-web-post-top (transform-position (partial thumb-tl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-tl-tl-web-post-bottom (transform-position (partial thumb-tl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        tps-65-mount-top-left-upper-with-same-x-as-EVQWGD001-mount-top-left-outside (find-point-on-line-using-x tps-65-mount-top-left-upper tps-65-mount-bottom-left-upper (nth EVQWGD001-mount-top-left-outside 0))
        tps-65-mount-top-left-lower-with-same-x-as-EVQWGD001-mount-top-left-inside (find-point-on-line-using-x tps-65-mount-top-left-lower tps-65-mount-bottom-left-lower (nth EVQWGD001-mount-top-left-inside 0))
        tps-65-to-EVQWGD001-mount-top-left-outside-control-point (calculate-point-between-points tps-65-mount-top-left-upper-with-same-x-as-EVQWGD001-mount-top-left-outside EVQWGD001-mount-top-left-outside [0 -4 2])
        tps-65-to-EVQWGD001-mount-top-left-outside (bezier-quadratic EVQWGD001-mount-top-left-outside  tps-65-to-EVQWGD001-mount-top-left-outside-control-point tps-65-mount-top-left-upper-with-same-x-as-EVQWGD001-mount-top-left-outside steps)
        tps-65-to-EVQWGD001-mount-top-right-outside (bezier-quadratic  EVQWGD001-mount-top-right-outside (calculate-point-between-points tps-65-mount-bottom-left-upper EVQWGD001-mount-top-right-outside [0 -4 2]) tps-65-mount-bottom-left-upper  steps)
        tps-65-to-EVQWGD001-mount-top-left-inside-control-point (calculate-point-between-points tps-65-mount-top-left-lower-with-same-x-as-EVQWGD001-mount-top-left-inside EVQWGD001-mount-top-left-inside [0 -4 2])
        tps-65-to-EVQWGD001-mount-top-left-inside (bezier-quadratic tps-65-mount-top-left-lower-with-same-x-as-EVQWGD001-mount-top-left-inside tps-65-to-EVQWGD001-mount-top-left-inside-control-point EVQWGD001-mount-top-left-inside steps)
        tps-65-to-EVQWGD001-mount-top-right-inside (bezier-quadratic  tps-65-mount-bottom-left-lower (calculate-point-between-points tps-65-mount-bottom-left-lower EVQWGD001-mount-top-right-inside [0 -4 2]) EVQWGD001-mount-top-right-inside  steps)
        EVQWGD001-mount-top-right-outside-to-thumb-bl-tr-web-post-top (bezier-quadratic EVQWGD001-mount-top-right-outside (calculate-point-between-points EVQWGD001-mount-top-right-outside thumb-bl-tr-web-post-top [0 1 0]) thumb-bl-tr-web-post-top  steps)
        EVQWGD001-mount-top-right-inside-to-thumb-bl-tr-web-post-bottom (bezier-quadratic thumb-bl-tr-web-post-bottom  (calculate-point-between-points EVQWGD001-mount-top-right-inside thumb-bl-tr-web-post-bottom [0 1 0]) EVQWGD001-mount-top-right-inside  steps)
        screen-holder-bottom-right-outside-floor-point-to-EVQWGD001-mount-bottom-left-outside (bezier-quadratic   screen-holder-bottom-right-outside-floor-point (calculate-point-between-points screen-holder-bottom-right-outside-floor-point EVQWGD001-mount-bottom-left-outside [0 -2 0])  EVQWGD001-mount-bottom-left-outside steps)
        screen-holder-bottom-right-inside-floor-point-to-EVQWGD001-mount-bottom-left-inside (bezier-quadratic   EVQWGD001-mount-bottom-left-inside (calculate-point-between-points screen-holder-bottom-right-inside-floor-point EVQWGD001-mount-bottom-left-inside [0 -2 0]) screen-holder-bottom-right-inside-floor-point steps)

        screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point (calculate-point-between-points screen-holder-top-right-outside-point EVQWGD001-mount-top-left-outside [0 -2 0])
        screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside (bezier-quadratic screen-holder-top-right-outside-point screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point  EVQWGD001-mount-top-left-outside steps)
        screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-control-point (calculate-point-between-points screen-holder-top-right-inside-point EVQWGD001-mount-top-left-inside [0 -2 0])
        screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside (bezier-quadratic  EVQWGD001-mount-top-left-inside screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-control-point screen-holder-top-right-inside-point  steps)
        EVQWGD001-mount-bottom-right-outside-to-floor (bezier-quadratic    EVQWGD001-mount-bottom-right-outside-floor (calculate-point-between-points EVQWGD001-mount-bottom-right-outside  EVQWGD001-mount-bottom-right-outside-floor [0 -2 0]) EVQWGD001-mount-bottom-right-outside steps)
        EVQWGD001-mount-bottom-right-inside-to-floor (bezier-quadratic    EVQWGD001-mount-bottom-right-inside (calculate-point-between-points EVQWGD001-mount-bottom-right-inside  EVQWGD001-mount-bottom-right-inside-floor [0 -2 0]) EVQWGD001-mount-bottom-right-inside-floor  steps)
        tps-65-mount-position-with-same-x-as-screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point (find-point-on-line-using-x tps-65-mount-top-left-upper  tps-65-mount-bottom-left-upper  (nth screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point 0))
        tps-65-to-EVQWGD001-mount-top-left-outside-control-point-with-same-x-as-screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point (assoc tps-65-to-EVQWGD001-mount-top-left-outside-control-point 0 (nth screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point 0))
        screen-holder-outer-position-with-same-y-as-tps-65-to-EVQWGD001-mount-top-left-outside-control-point (find-point-on-line-using-y screen-holder-top-right-outside-point screen-holder-outside-point-with-same-y-as-tps-65-top-left-upper (nth tps-65-to-EVQWGD001-mount-top-left-outside-control-point 1))

        tps-65-mount-position-with-same-x-as-screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-control-point (find-point-on-line-using-x tps-65-mount-top-left-lower  tps-65-mount-bottom-left-lower  (nth screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-control-point 0))
        tps-65-to-EVQWGD001-mount-top-left-inside-control-point-with-same-x-as-screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-control-point (assoc tps-65-to-EVQWGD001-mount-top-left-inside-control-point 0 (nth screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-control-point 0))
        screen-holder-inner-position-with-same-y-as-tps-65-to-EVQWGD001-mount-top-left-inside-control-point (find-point-on-line-using-y screen-holder-top-right-inside-point screen-holder-inside-point-with-same-y-as-tps-65-top-left-inner (nth tps-65-to-EVQWGD001-mount-top-left-inside-control-point 1))
        tps-65-upper-to-EVQWGD001-mount-outside (into [] (apply concat
                                                                (for [index (range 0 (inc steps))]
                                                                  (bezier-linear
                                                                   (nth tps-65-to-EVQWGD001-mount-top-left-outside index)
                                                                   (nth tps-65-to-EVQWGD001-mount-top-right-outside index)
                                                                   steps))))
        tps-65-lower-to-EVQWGD001-mount-inside (into [] (apply concat
                                                               (for [index (range 0 (inc steps))]
                                                                 (bezier-linear
                                                                  (nth tps-65-to-EVQWGD001-mount-top-left-inside index)
                                                                  (nth tps-65-to-EVQWGD001-mount-top-right-inside index)
                                                                  steps))))
        tps-65-to-EVQWGD001-mount (into [] (concat tps-65-upper-to-EVQWGD001-mount-outside tps-65-lower-to-EVQWGD001-mount-inside))
        tps-65-to-EVQWGD001-mount-polyhedron (polyhedron tps-65-to-EVQWGD001-mount (generate-bezier-along-bezier-polyhedron-faces tps-65-upper-to-EVQWGD001-mount-outside tps-65-lower-to-EVQWGD001-mount-inside steps))
        EVQWGD001-mount-top-right-outside-to-thumb-bl-tr-web-post-top-quadratic (bezier-quadratic EVQWGD001-mount-top-right-outside (calculate-point-between-points EVQWGD001-mount-top-right-outside thumb-bl-tr-web-post-top [0 1 0]) thumb-bl-tr-web-post-top steps)
        EVQWGD001-mount-top-right-outside-to-thumb-bl-tr-to-tps-65-outside (into [] (apply concat
                                                                                           (for [index (range 0 (inc steps))]
                                                                                             (bezier-linear
                                                                                              (nth tps-65-to-EVQWGD001-mount-top-right-outside index)
                                                                                              (nth EVQWGD001-mount-top-right-outside-to-thumb-bl-tr-web-post-top index)
                                                                                              steps))))
        EVQWGD001-mount-top-right-inside-to-thumb-bl-tr-to-tps-65-inside (into [] (apply concat
                                                                                         (for [index (range 0 (inc steps))]
                                                                                           (bezier-linear
                                                                                            (nth tps-65-to-EVQWGD001-mount-top-right-inside index)
                                                                                            (nth EVQWGD001-mount-top-right-inside-to-thumb-bl-tr-web-post-bottom index)
                                                                                            steps))))
        VQWGD001-mount-top-right-to-thumb-bl-tr-to-tps-65-polyhedron (polyhedron (into [] (concat EVQWGD001-mount-top-right-outside-to-thumb-bl-tr-to-tps-65-outside EVQWGD001-mount-top-right-inside-to-thumb-bl-tr-to-tps-65-inside))
                                                                                 (generate-bezier-along-bezier-polyhedron-faces EVQWGD001-mount-top-right-outside-to-thumb-bl-tr-to-tps-65-outside EVQWGD001-mount-top-right-inside-to-thumb-bl-tr-to-tps-65-inside steps))
        tps-65-mount-top-left-to-bottom-left-web-post-top (bezier-linear tps-65-mount-top-left-upper tps-65-mount-bottom-left-upper (* steps 3))
        tps-65-mount-top-left-to-bottom-left-web-post-top-control-points (bezier-quadratic screen-holder-top-right-outside-point tps-65-mount-top-left-upper-control-point tps-65-mount-bottom-left-upper-control-pont (* steps 3))
        screen-holder-top-right-outside-point-cubic-to-EVQWGD001-mount-top-left-outside-linear-to-EVQWGD001-mount-top-right-outside-quadratic-to-thumb-bl-tr-points
        (concat (drop-last (bezier-cubic screen-holder-top-right-outside-point (calculate-point-between-points screen-holder-top-right-outside-point EVQWGD001-mount-top-left-outside [0 -2 0]) EVQWGD001-mount-top-left-outside-control-point EVQWGD001-mount-top-left-outside steps))
                (drop-last (bezier-linear EVQWGD001-mount-top-left-outside EVQWGD001-mount-top-right-outside steps))
                (bezier-quadratic EVQWGD001-mount-top-right-outside (calculate-point-between-points EVQWGD001-mount-top-right-outside thumb-bl-tr-web-post-top [0 1 0]) thumb-bl-tr-web-post-top steps))
        screen-holder-bottom-right-outside-point-cubic-to-EVQWGD001-mount-bottom-left-outside-linear-to-EVQWGD001-mount-bottom-right-outside-quadratic-to-thumb-bl-tl-points
        (concat (drop-last (bezier-cubic screen-holder-bottom-right-outside-point (calculate-point-between-points screen-holder-bottom-right-outside-point EVQWGD001-mount-bottom-left-outside [0 -2 0]) EVQWGD001-mount-bottom-left-outside-control-point EVQWGD001-mount-bottom-left-outside steps))
                (drop-last (bezier-linear EVQWGD001-mount-bottom-left-outside EVQWGD001-mount-bottom-right-outside steps))
                (bezier-quadratic EVQWGD001-mount-bottom-right-outside (calculate-point-between-points EVQWGD001-mount-bottom-right-outside thumb-bl-tr-web-post-top [0 1 0]) thumb-bl-tl-web-post-top steps))
        screen-holder-bottom-right-floor-to-thumb-bl-tl-wall-locate3-floor
        (concat  (drop-last (bezier-quadratic screen-holder-bottom-right-outside-floor-point (calculate-point-between-points screen-holder-bottom-right-outside-floor-point EVQWGD001-mount-bottom-mid-outside-floor-control-point [0 -1 0]) EVQWGD001-mount-bottom-mid-outside-floor-control-point steps))
                 (drop-last (bezier-quadratic EVQWGD001-mount-bottom-mid-outside-floor-control-point (calculate-point-between-points EVQWGD001-mount-bottom-mid-outside-floor-control-point thumb-bl-tl-wall-locate3-floor [0 -1 0]) thumb-bl-tl-wall-locate3-floor steps))
                 (reverse thumb-bl-tl-outside))
        outer-faces-start (concat
                           (drop-last (bezier-quadratic tps-65-mount-top-left-upper tps-65-mount-top-left-upper-control-point screen-holder-top-right-outside-point steps))
                           (drop-last (bezier-linear screen-holder-top-right-outside-point screen-holder-bottom-right-outside-point steps))
                           (bezier-linear screen-holder-bottom-right-outside-point screen-holder-bottom-right-outside-floor-point steps))

        outer-faces-nth (fn [index]
                          (concat (drop-last (bezier-quadratic
                                              (nth tps-65-mount-top-left-to-bottom-left-web-post-top index)
                                              (nth tps-65-mount-top-left-to-bottom-left-web-post-top-control-points index)
                                              (nth screen-holder-top-right-outside-point-cubic-to-EVQWGD001-mount-top-left-outside-linear-to-EVQWGD001-mount-top-right-outside-quadratic-to-thumb-bl-tr-points index)
                                              steps))
                                  (drop-last
                                   (bezier-linear
                                    (nth screen-holder-top-right-outside-point-cubic-to-EVQWGD001-mount-top-left-outside-linear-to-EVQWGD001-mount-top-right-outside-quadratic-to-thumb-bl-tr-points index)
                                    (nth screen-holder-bottom-right-outside-point-cubic-to-EVQWGD001-mount-bottom-left-outside-linear-to-EVQWGD001-mount-bottom-right-outside-quadratic-to-thumb-bl-tl-points index)
                                    steps))
                                  (bezier-linear
                                   (nth screen-holder-bottom-right-outside-point-cubic-to-EVQWGD001-mount-bottom-left-outside-linear-to-EVQWGD001-mount-bottom-right-outside-quadratic-to-thumb-bl-tl-points index)
                                   (nth screen-holder-bottom-right-floor-to-thumb-bl-tl-wall-locate3-floor index)
                                   steps)))
        screen-holder-pos-with-same-y-as-tps-65-left-top-curve (bezier-cubic 
                                                                screen-holder-outside-point-with-same-y-as-tps-65-top-left-upper
                                                                screen-holder-top-right-inside-point-translated-with-same-y-as-tps-65-top-left-inner
                                                                tps-65-top-left-control-point-outer
                                                                tps-65-top-left-outer
                                                                steps)


        outer-faces-fn (fn [index]
                         (bezier-quartic
                          (nth tps-65-mount-top-left-to-bottom-left-web-post-top index)
                          (nth tps-65-mount-top-left-to-bottom-left-web-post-top-control-points index)
                          (nth screen-holder-top-right-outside-point-cubic-to-EVQWGD001-mount-top-left-outside-linear-to-EVQWGD001-mount-top-right-outside-quadratic-to-thumb-bl-tr-points index)
                          (nth screen-holder-bottom-right-outside-point-cubic-to-EVQWGD001-mount-bottom-left-outside-linear-to-EVQWGD001-mount-bottom-right-outside-quadratic-to-thumb-bl-tl-points index)
                          (nth screen-holder-bottom-right-floor-to-thumb-bl-tl-wall-locate3-floor index)
                          (* steps 3)))
        outer-faces (into [] (apply concat
                                    ;outer-faces-start
                                    (for [index (range 0 (inc (* steps 3)))]
                                      (outer-faces-nth index))))
        screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point-translated-z-pos (mapv + screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point [0 0 (* oled-holder-thickness 2)])
        top-left-corner-surface-outer-points-bottom #(bezier-quadratic screen-holder-top-right-outside-point  (mapv + screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point [0 0 (* oled-holder-thickness 1)]) EVQWGD001-mount-top-left-outside %)
        top-left-corner-surface-outer-points (into [] (apply concat (for [index (range 0 (inc (* steps 2)))
                                                                           :let [top-points (concat (drop-last screen-holder-pos-with-same-y-as-tps-65-left-top-curve) 
                                                                                                   (bezier-quadratic tps-65-mount-top-left-upper tps-65-mount-position-with-same-x-as-screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point tps-65-mount-top-left-upper-with-same-x-as-EVQWGD001-mount-top-left-outside steps))
                                                                                 control-points (bezier-quadratic screen-holder-outer-position-with-same-y-as-tps-65-to-EVQWGD001-mount-top-left-outside-control-point 
                                                                                                 (mapv + tps-65-to-EVQWGD001-mount-top-left-outside-control-point-with-same-x-as-screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point [0 0 0])
                                                                                                                  tps-65-to-EVQWGD001-mount-top-left-outside-control-point (* steps 2))
                                                                                 control-points-2 (bezier-quadratic screen-holder-top-right-outside-point screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point-translated-z-pos EVQWGD001-mount-top-left-outside (* steps 2))
                                                                                 bottom-points (top-left-corner-surface-outer-points-bottom (* steps 2))
                                                                                 ]]
                                                                      (bezier-quadratic
                                                                       (nth top-points index)
                                                                      (nth control-points index)
                                                                       ;(nth control-points-2 index)
                                                                      (nth bottom-points index)
                                                                      (* steps 2) 
                                                                       )
                                                                      )))
        top-left-corner-surface-inner-points (into [] (apply concat (for [index (range 0 (inc (* steps 2)))
                                                                          :let [top-points (bezier-quadratic tps-65-mount-top-left-lower-with-same-x-as-EVQWGD001-mount-top-left-inside
                                                                                                          tps-65-top-left-inner-with-same-z-as-screen-holder-top-right-inside-point-translated-with-same-y-as-tps-65-top-left-inner
                                                                                                         screen-holder-inside-point-with-same-y-as-tps-65-top-left-inner
                                                                                                          (* steps 2))
                                                                                control-points (bezier-quadratic  tps-65-to-EVQWGD001-mount-top-left-inside-control-point 
                                                                                                                  tps-65-to-EVQWGD001-mount-top-left-inside-control-point-with-same-x-as-screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-control-point
                                                                                                                  screen-holder-inner-position-with-same-y-as-tps-65-to-EVQWGD001-mount-top-left-inside-control-point
                                                                                                                  (* steps 2)
                                                                                                                  )
                                                                                lower-points (bezier-quadratic  EVQWGD001-mount-top-left-inside screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-control-point screen-holder-top-right-inside-point  (* steps 2)) ]]
                                                                      (bezier-quadratic (nth top-points index) (nth control-points index) (nth lower-points index) (* steps 2))
                                                                      )))
        below-top-left-corner-surface-outer-points (into [] (apply concat (for [index (range 0 (inc steps))
                                                                                :let [top-points (top-left-corner-surface-outer-points-bottom steps)
                                                                                      bottom-points screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside
                                                                                      ]]
                                                                            (bezier-linear (nth top-points index) (nth bottom-points index) steps)
                                                                            ))) 
        below-top-left-corner-surface-inner-points (into [] (apply concat (for [index (range 0 (inc steps))
                                                                          :let [top-points screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside 
                                                                                bottom-points screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside]]
                                                                      (bezier-linear 
                                                                       (nth top-points index)
                                                                       (nth bottom-points index)
                                                                       steps
                                                                       ))))
        top-left-corner-surface-polyhedron (generate-bezier-along-bezier-polyhedron top-left-corner-surface-outer-points top-left-corner-surface-inner-points (* steps 2) )
        below-top-left-corner-surface-polyhedron (generate-bezier-along-bezier-polyhedron below-top-left-corner-surface-outer-points below-top-left-corner-surface-inner-points steps ) 
        EVQWGD001-mount-bottom-right-outside-floor-thumb-bl-tl-wall-locate3-floor (bezier-linear EVQWGD001-mount-bottom-right-outside-floor thumb-bl-tl-wall-locate3-floor steps)
screen-holder-bottom-right-outside-floor-point-to-EVQWGD001-mount-bottom-right-outside-floor (bezier-linear screen-holder-bottom-right-outside-floor-point EVQWGD001-mount-bottom-right-outside-floor  steps)
        ]
  
  (union
  ;;  (plot-bezier-points tps-65-mount-top-left-to-bottom-left-web-post-top (sphere 0.1))
  ;;  (plot-bezier-points tps-65-mount-top-left-to-bottom-left-web-post-top-control-points (sphere 0.1))
  ;;  (plot-bezier-points screen-holder-top-right-outside-point-cubic-to-EVQWGD001-mount-top-left-outside-linear-to-EVQWGD001-mount-top-right-outside-quadratic-to-thumb-bl-tr-points (sphere 0.1))
  ;;  (plot-bezier-points screen-holder-bottom-right-outside-point-cubic-to-EVQWGD001-mount-bottom-left-outside-linear-to-EVQWGD001-mount-bottom-right-outside-quadratic-to-thumb-bl-tl-points (sphere 0.1))
  ;;  (plot-bezier-points screen-holder-bottom-right-floor-to-thumb-bl-tl-wall-locate3-floor (sphere 0.1))
   ;(polyhedron outer-faces (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc (* steps 3)) (inc (* steps 3)) (* steps 3)))

  ;;  (wall-brace-polyhedron (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-left) 0 -1 "centre" :degrees
  ;;                         (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left) 0 0 "centre" :degrees
  ;;                         steps)


  ;;  (wall-brace-polyhedron (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left) 0 -2 "centre" :degrees
  ;;                         (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right) 1 -2 "centre" :degrees
  ;;                         steps)
  ;;  (wall-brace-polyhedron (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-right) 1 -2 "centre" :degrees
  ;;                         thumb-bl-place -1 0 "tl" :degrees
  ;;                         steps)
;;    (generate-bezier-quadratic-polyhedron-from-points-and-control-vectors 
;;     EVQWGD001-mount-top-right-outside thumb-bl-tr-web-post-top
;;     EVQWGD001-mount-bottom-right-outside thumb-bl-tl-web-post-top
;;     EVQWGD001-mount-top-right-inside thumb-bl-tr-web-post-bottom
;; EVQWGD001-mount-bottom-right-inside thumb-bl-tl-web-post-bottom
;;     steps
;;     {:outside-upper-control-point-vector [0 1 0] :outside-lower-control-point-vector [0 0 0] :inside-upper-control-point-vector [0 1 0] :inside-lower-control-point-vector [0 0 0]}
;;     )
   (generate-bezier-along-bezier-polyhedron-from-points-list-linear
    (bezier-quadratic EVQWGD001-mount-top-right-outside (calculate-point-between-points EVQWGD001-mount-top-right-outside thumb-bl-tr-web-post-top [0 1 0]) thumb-bl-tr-web-post-top steps)
    (bezier-linear EVQWGD001-mount-bottom-right-outside thumb-bl-tl-web-post-top steps)
    (bezier-quadratic  thumb-bl-tr-web-post-bottom (calculate-point-between-points EVQWGD001-mount-top-right-inside thumb-bl-tr-web-post-bottom [0 1 0])  EVQWGD001-mount-top-right-inside steps)
    (bezier-linear  thumb-bl-tl-web-post-bottom EVQWGD001-mount-bottom-right-inside steps)
    steps)
   (generate-bezier-along-bezier-polyhedron-from-points-list-linear
    screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside screen-holder-bottom-right-outside-floor-point-to-EVQWGD001-mount-bottom-left-outside
    screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside  screen-holder-bottom-right-inside-floor-point-to-EVQWGD001-mount-bottom-left-inside
    steps)

   (generate-bezier-along-bezier-polyhedron-from-points-list-linear 
     screen-holder-bottom-right-outside-floor-point-to-EVQWGD001-mount-bottom-left-outside EVQWGD001-mount-bottom-right-outside-to-floor
   screen-holder-bottom-right-inside-floor-point-to-EVQWGD001-mount-bottom-left-inside EVQWGD001-mount-bottom-right-inside-to-floor    
    
    steps)
   (generate-bezier-along-bezier-polyhedron-from-points-list-linear
    EVQWGD001-mount-bottom-right-outside-to-floor(reverse thumb-bl-tl-outside)
      EVQWGD001-mount-bottom-right-inside-to-floor thumb-bl-tl-inside
    steps
    )
   tps-65-to-EVQWGD001-mount-polyhedron
   VQWGD001-mount-top-right-to-thumb-bl-tr-to-tps-65-polyhedron
   (generate-bezier-along-bezier-polyhedron-from-points-linear
    thumb-bl-tr-web-post-top tps-65-mount-bottom-left-upper
    thumb-tl-tl-web-post-top tps-65-mount-bottom-left-upper
     tps-65-mount-bottom-left-lower thumb-bl-tr-web-post-bottom
    tps-65-mount-bottom-left-lower thumb-tl-tl-web-post-bottom 
    steps)
   top-left-corner-surface-polyhedron
   below-top-left-corner-surface-polyhedron
  ;;  (generate-bezier-along-bezier-polyhedron-from-points-list-linear
  ;;   (bezier-quadratic EVQWGD001-mount-top-left-outside (calculate-point-between-points screen-holder-top-right-inside-point EVQWGD001-mount-top-left-outside [0 -2 0]) screen-holder-top-right-inside-point  steps) tps-65-to-EVQWGD001-mount-top-left-outside
  ;;  (bezier-quadratic EVQWGD001-mount-top-left-inside  (calculate-point-between-points screen-holder-top-right-inside-point EVQWGD001-mount-top-left-inside [0 -2 0]) screen-holder-top-right-inside-point steps ) tps-65-to-EVQWGD001-mount-top-left-inside
  ;;   steps
  ;;   )
  ;;  (wall-brace-polyhedron (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-top-right) 0 2 "centre" :degrees
  ;;                         thumb-bl-place -1 0 "tl" :degrees
  ;;                         steps)
   ;(plot-bezier-points screen-holder-top-right-outside-point-cubic-to-EVQWGD001-mount-top-left-outside-linear-to-EVQWGD001-mount-top-right-outside-quadratic-to-thumb-bl-tr-points (sphere ))
;;   (-#(translate EVQWGD001-mount-top-left-outside-control-point (sphere 0.1)))
;;   (translate EVQWGD001-mount-top-left-outside (sphere 0.1))
;; (translate EVQWGD001-mount-top-right-outside (sphere 0.1))
   )
  )
  )
  (defn left-section-front-polyhedron-bottom [steps]
  (let 
   [thumb-bl-tl-wall-locate3-floor (wall-brace-polyhedron-outer-floor-point thumb-bl-place -1 0 "tl" :degrees )
    
    EVQWGD001-mount-bottom-right-outside-floor-thumb-bl-tl-wall-locate3-floor (bezier-linear EVQWGD001-mount-bottom-right-outside-floor thumb-bl-tl-wall-locate3-floor steps)
screen-holder-bottom-right-outside-floor-point-to-EVQWGD001-mount-bottom-right-outside-floor (bezier-linear screen-holder-bottom-right-outside-floor-point EVQWGD001-mount-bottom-right-outside-floor  steps)]
    (concat screen-holder-bottom-right-outside-floor-point-to-EVQWGD001-mount-bottom-right-outside-floor EVQWGD001-mount-bottom-right-outside-floor-thumb-bl-tl-wall-locate3-floor)
    
    )
    )
  
  (defn pinky-to-fourth-br-bl [steps]
     (let [pinky-to-fourth-web-post-curve
           (web-post-quadratic-curve (partial key-place lastcol cornerrow)  "bl" :radians
                                     (partial key-place 3 cornerrow) "br" :radians
                                     (partial key-place 3 cornerrow) "bl" :radians
                                     steps)
           fourth-br-top (transform-position-radians (partial key-place 3 cornerrow) (web-post-position-top (mapv +  (get-single-plate-corner-position-vector "br"))))
           fourth-br-bottom (transform-position-radians (partial key-place 3 cornerrow) (web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "br"))))
           fourth-bl-to-br (web-post-linear
                            (partial key-place lastcol cornerrow) "bl" :radians
                            (partial key-place 3 cornerrow) "br" :radians
                            steps)
           fourth-br-to-pinky-bl (web-post-linear
                                  (partial key-place lastcol cornerrow) "bl" :radians
                                  (partial key-place 3 cornerrow) "br" :radians
                                  18)
           outer-top-start 0 
           outer-top-end steps
           outer-bottom-start (inc outer-top-end) 
           outer-bottom-end (+ outer-bottom-start steps)
           inner-top-start (inc outer-bottom-end)
           inner-top-end (+ inner-top-start steps)
           inner-bottom-start (inc inner-top-end)
           inner-bottom-end (+ inner-bottom-start steps)
           top-layer (into [] (apply concat (for [index (range 0 (inc steps))
                                                  :let [pinky-to-fourth-web-post-curve-top
                                                        (pinky-to-fourth-web-post-curve :top)
                                                        fourth-bl-to-br-top ( fourth-bl-to-br :top)]]
                                              (bezier-linear
                                               (nth pinky-to-fourth-web-post-curve-top index)
                                               (nth fourth-bl-to-br-top index)
                                               steps))))
           bottom-layer (into [] (apply concat (for [index (range 0 (inc steps))
                                                     :let [pinky-to-fourth-web-post-curve-bottom
                                                           (pinky-to-fourth-web-post-curve :top)
                                                           fourth-bl-to-br-bottom (fourth-bl-to-br :bottom)]]
                                                 (bezier-linear
                                                  (nth pinky-to-fourth-web-post-curve-bottom index)
                                                  (nth fourth-bl-to-br-bottom index)
                                                  steps))))]
  ;; (generate-polyhedron-from-points 

  ;;   (nth pinky-to-fourth-web-post-curve 0) (reverse (nth pinky-to-fourth-web-post-curve 1))
  ;;   (nth fourth-bl-to-br 0) (reverse (nth fourth-bl-to-br 1)) 
  ;;   steps
  ;;  )
  ;; (polyhedron (concat top-layer bottom-layer) 
  ;;             (generate-bezier-along-bezier-polyhedron-faces 
  ;;              top-layer bottom-layer steps))
       (generate-bezier-to-point-polyhedron
        (reverse (pinky-to-fourth-web-post-curve :top))
        fourth-br-top
        (pinky-to-fourth-web-post-curve :bottom)
        fourth-br-bottom)))

(defn middle-bm-to-fourth-bl-to-middle-br [steps]
  (let [middle-bm-to-fourth-bl
        (web-post-quadratic-curve
         (partial key-place 3 cornerrow)  "bl" :radians
         (partial key-place 2 cornerrow)  "br" :radians
         (partial key-place 2 cornerrow) "bm" :radians
         steps
         :offset1 [0 post-size 0]  :offset2 [post-size 0 0]
         )
        
        middle-bm-to-fourth-bl-top (middle-bm-to-fourth-bl :top)
        middle-bm-to-fourth-bl-bottom (middle-bm-to-fourth-bl :bottom)
        middle-br-top (transform-position-radians (partial key-place 2 cornerrow) (web-post-position-top (mapv +  (get-single-plate-corner-position-vector "br"))))
        middle-br-bottom (transform-position-radians (partial key-place 2 cornerrow) (web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "br"))))

        top-faces (into [] (apply concat
                                  (for [index (range 0 (inc steps))]
                                    (bezier-linear
                                     (nth middle-bm-to-fourth-bl-top index)
                                     middle-br-top
                                     steps))))
        bottom-faces (into [] (apply concat
                                     (for [index (range 0 (inc steps))]
                                       (bezier-linear
                                        middle-br-bottom
                                        (nth middle-bm-to-fourth-bl-bottom index)
                                        steps
                                        ))))]
    ;; (generate-bezier-to-point-polyhedron
    ;;  (reverse (nth middle-bm-to-fourth-bl 0))
    ;;  middle-br-top
    ;;   (nth middle-bm-to-fourth-bl 1)
    ;;  middle-br-bottom)
    (generate-bezier-to-point-polyhedron
    (reverse middle-bm-to-fourth-bl-top)  middle-br-top
    middle-bm-to-fourth-bl-bottom middle-br-bottom )
    ))

(defn get-wall-brace-polyhedron-fn [bottom-plate ](if (true? bottom-plate) wall-brace-polyhedron-outer-floor-linear wall-brace-polyhedron ))
(defn get-wall-brace-quadratic-fn [bottom-plate ](if (true? bottom-plate)  wall-brace-quadratic-polyhedron-bottom wall-brace-quadratic-polyhedron))
(defn get-wall-brace-cubic-fn [bottom-plate] (if (true? bottom-plate) wall-brace-cubic-polyhedron-floor-outer wall-brace-cubic-polyhedron ))
(defn get-key-wall-brace-polyhedron-fn [bottom-plate] (if (true? bottom-plate) key-wall-brace-polyhedron-outer-floor-linear key-wall-brace-polyhedron))
(defn get-thumb-wall-brace-polyhedron-fn [bottom-plate] (if (true? bottom-plate) thumb-wall-brace-polyhedron-outer-floor-linear thumb-wall-brace-polyhedron))
(defn get-collect-fn [bottom-plate] (if (true? bottom-plate) concat union))
(defn right-wall-polyhedron [steps &{:keys [bottom-plate] :or {bottom-plate false}}]
(let [wall-brace-quadratic-fn (get-wall-brace-quadratic-fn bottom-plate)
     key-wall-brace-polyhedron-fn (get-key-wall-brace-polyhedron-fn bottom-plate)
     
     top-corner (wall-brace-quadratic-fn (partial key-place lastcol 0) 1 0 "tr" :radians 
                                                 (partial key-place lastcol 0) 1 1 "tr" :radians 
                                                 (partial key-place lastcol 0) 0 1 "tr" :radians
                                                 steps) 
     bottom-corner (wall-brace-quadratic-fn (partial key-place lastcol cornerrow) 0 -1 "br" :radians
                                                    (partial key-place lastcol cornerrow) 1 -1 "br" :radians
                                                    (partial key-place lastcol cornerrow)  1 0 "br" :radians
                                                    steps)
     collect-fn (get-collect-fn bottom-plate)
     conditional-reverse (fn [brace] (if (true? bottom-plate) (reverse brace) brace))
     order-points (fn [list1 list2] (if (true? bottom-plate)(apply concat (interleave list1 list2) [(last list1)]) (union list1 list2))) 
     group (fn [points] (if (true? bottom-plate) (apply concat points) points))
]  (collect-fn
    bottom-corner
    (key-wall-brace-polyhedron-fn lastcol 2 1 0 "br" lastcol 2 1 0 "tr" :steps steps)
    (key-wall-brace-polyhedron-fn lastcol 2 1 0 "tr" lastcol 1 1 0 "br" :steps steps)
    (key-wall-brace-polyhedron-fn lastcol 1 1 0 "br" lastcol 1 1 0 "tr" :steps steps)
    (key-wall-brace-polyhedron-fn lastcol 1 1 0 "tr" lastcol 0 1 0 "br" :steps steps)
    (key-wall-brace-polyhedron-fn lastcol 0 1 0 "br" lastcol 0 1 0 "tr" :steps steps)
    ;(for [y (range 0 lastrow)] (key-wall-brace-polyhedron-fn lastcol y 1 0 "br" lastcol y 1 0 "tr" :steps steps))
    ;(for [y (range 1 lastrow)] (key-wall-brace-polyhedron-fn lastcol y 1 0 "tr" lastcol (dec y) 1 0 "br" :steps steps))
   top-corner 
    )))

(defn back-wall-polyhedron [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [key-wall-brace-polyhedron-fn (get-key-wall-brace-polyhedron-fn bottom-plate)
        wall-brace-cubic-fn (get-wall-brace-cubic-fn bottom-plate)
        zero-to-ncols-fn (fn [x] (key-wall-brace-polyhedron-fn x 0 0 1 "tr" x 0 0 1 "tl"))
        two-to-last-col-fn (fn [x]
                             (wall-brace-cubic-fn
                              {:place1 (partial key-place (inc x) 0)  :dx1 0 :dy1 1 :post-position-1 "tl" :rad-or-deg1 :radians
                               :place-mid1 (partial key-place x 0) :dxmid1 1 :dymid1 0 :post-position-mid1 "tr" :rad-or-degmid1 :radians
                               :place-mid2 (partial key-place x 0) :dxmid2 1 :dymid2 1  :post-position-mid2 "tr" :rad-or-degmid2 :radians
                               :place2 (partial key-place x 0) :dx2 0 :dy2 1 :post-position-2 "tr" :rad-or-deg2 :radians
                               :xy1 wall-xy-offset :xymid1 wall-xy-offset-thin :xymid2 wall-xy-offset-mid :xy2 wall-xy-offset
                               :steps steps}))
        col-1-fn (fn [x] (wall-brace-cubic-fn
                          {:place1 (partial key-place (inc x) 0)  :dx1 0 :dy1 1 :post-position-1 "tl" :rad-or-deg1 :radians
                           :place-mid1 (partial key-place (inc x) 0) :dxmid1 -1 :dymid1 1 :post-position-mid1 "tl" :rad-or-degmid1 :radians
                           :place-mid2 (partial key-place (inc x) 0) :dxmid2 -1 :dymid2 0  :post-position-mid2 "tl" :rad-or-degmid2 :radians
                           :place2 (partial key-place x 0) :dx2 0 :dy2 1 :post-position-2 "tr" :rad-or-deg2 :radians
                           :xy1 wall-xy-offset :xymid1 wall-xy-offset-mid :xymid2  wall-xy-offset-thin :xy2 wall-xy-offset
                           :steps steps}))
        col-0-fn (fn [x] (key-wall-brace-polyhedron-fn (inc x) 0 0 1 "tl" x 0 0 1 "tr"))
        collect-fn (get-collect-fn bottom-plate)] 
   (collect-fn
    (zero-to-ncols-fn 4)
    (two-to-last-col-fn 3)
    (zero-to-ncols-fn 3)
(two-to-last-col-fn 2)
    (zero-to-ncols-fn 2)
    (col-1-fn 1)
(zero-to-ncols-fn 1)
     (col-0-fn 0)
    (zero-to-ncols-fn 0)
 ;   (for [x (range 0 ncols)] (key-wall-brace-polyhedron x 0 0 1 "tr" x 0 0 1 "tl"))

          
          
          
;;           (for [x (range 2 lastcol)]
;;             (wall-brace-cubic-polyhedron 
;;             { :place1 (partial key-place (inc x) 0)  :dx1 0 :dy1 1 :post-position-1 "tl" :rad-or-deg1 :radians
;; :place-mid1 (partial key-place x 0) :dxmid1 1 :dymid1 0 :post-position-mid1 "tr" :rad-or-degmid1 :radians
;; :place-mid2 (partial key-place x 0) :dxmid2 1 :dymid2 1  :post-position-mid2 "tr":rad-or-degmid2 :radians
;; :place2 (partial key-place x 0) :dx2 0 :dy2 1 :post-position-2 "tr" :rad-or-deg2 :radians 
;;               :xy1 wall-xy-offset :xymid1 wall-xy-offset-thin :xymid2 wall-xy-offset-mid :xy2 wall-xy-offset
;; :steps steps}) 
;;             )
          ;; (for [x (range 2 lastcol)]
          ;;   (key-wall-brace-polyhedron
          ;;    (inc x) 0 0 1 "tl"
          ;;     x 0 1 0 "tr"
          ;;    :steps steps :xy1  wall-xy-offset :xy2 wall-xy-offset-mid
          ;;    )
            
          ;;   )
          ;; (for [x (range 2 lastcol)]
          ;;   (wall-brace-quadratic-polyhedron
          ;;    (partial key-place (inc x) 0) 0 1 "tl" :radians
          ;;    (partial key-place (inc x) 0) -1 1 "tl" :radians
          ;;    (partial key-place x 0) 1 0 "tr" :radians
          ;;    wall-xy-offset wall-xy-offset-mid wall-xy-offset-thin
          ;;    steps)) 
          ;; (for [x (range 2 lastcol)]
          ;;   (wall-brace-quadratic-polyhedron
          ;;    (partial key-place x 0) 1 0 "tr" :radians
          ;;    (partial key-place x 0) 1 1 "tr" :radians
          ;;    (partial key-place x 0) 0 1 "tr" :radians
          ;;    steps))
     
          )))

(defn front-wall-polyhedron [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [wall-brace-quadratic-fn (get-wall-brace-quadratic-fn bottom-plate)
key-wall-brace-polyhedron-fn (get-key-wall-brace-polyhedron-fn bottom-plate)
        collect-fn (get-collect-fn bottom-plate)
        ]
    (collect-fn
     (wall-brace-quadratic-fn (partial key-place 1 cornerrow) 1 -0.1 "br" :radians
                                      (partial key-place 2 cornerrow) 0.25 -1 "bl" :radians
                                      (partial key-place 2 cornerrow) 0 -1 "bm" :radians
                                      wall-xy-offset-medium-thin wall-xy-offset-thin wall-xy-offset-thin
                                      steps)
     
      (wall-brace-quadratic-fn (partial key-place 2 cornerrow) 0 -1 "bm" :radians
                                       (partial key-place 2 cornerrow) 0 -1 "br" :radians
                                       (partial key-place 3 cornerrow) -1 0 "bl" :radians
                                       wall-xy-offset-thin wall-xy-offset-thin wall-xy-offset-mid
                                       steps)
     
     (wall-brace-quadratic-fn (partial key-place 3 cornerrow) -1 0 "bl" :radians
                                      (partial key-place 3 cornerrow) -1 -1 "bl" :radians
                                      (partial key-place 3 cornerrow)  0 -1 "bl" :radians
                                      wall-xy-offset-mid wall-xy-offset wall-xy-offset
                                      steps)
     
     (wall-brace-quadratic-fn (partial key-place 3 cornerrow) 0 -1 "bl" :radians
                                      (partial key-place 3 cornerrow) 0 -1 "br" :radians
                                      (partial key-place lastcol cornerrow) -1 0 "bl" :radians
                                      steps)

     (wall-brace-quadratic-fn (partial key-place lastcol cornerrow) -1 0 "bl" :radians
                                      (partial key-place lastcol cornerrow) -1 -1 "bl" :radians
                                      (partial key-place lastcol cornerrow)  0 -1 "bl" :radians
                                      steps) 
      (key-wall-brace-polyhedron-fn lastcol cornerrow 0 -1 "bl" lastcol cornerrow 0 -1 "br" :steps steps) 

     )))

(defn front-wall-connecters-polyhedron [steps]
  (let [index-br-to-middle-bl-to-bm-web-post-curve (web-post-quadratic-curve (partial key-place 1 cornerrow) "br" :radians
                                                                             (partial key-place 2 cornerrow) "bl" :radians
                                                                             (partial key-place 2 cornerrow) "bm" :radians
                                                                             steps
                                                                             :offset1 [0 post-size 0] :offset2 [0 0 0])
middle-bl-web-post (web-post-point (partial key-place 2 cornerrow) "bl" :radians)
index-br-to-middle-bl-to-bm-web-post-polyhedron (generate-bezier-to-point-polyhedron
                                                 (index-br-to-middle-bl-to-bm-web-post-curve :top)
                                                 (middle-bl-web-post :top)
                                                 (reverse (index-br-to-middle-bl-to-bm-web-post-curve :bottom))
                                                 (middle-bl-web-post :bottom))]
    (pinky-to-fourth-br-bl steps)
    (middle-bm-to-fourth-bl-to-middle-br steps)
      index-br-to-middle-bl-to-bm-web-post-polyhedron
    
    )
  )

(defn thumb-walls-polyhedron [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [thumb-wall-brace-polyhedron-fn (get-thumb-wall-brace-polyhedron-fn bottom-plate)
        
        ]  
    {:thumb-bl-tl-to-bl (thumb-wall-brace-polyhedron-fn thumb-bl-place -1  0 "tl" thumb-bl-place -1  0 "bl" :steps steps)
     :thumb-bl-to-br (thumb-wall-brace-polyhedron-fn thumb-bl-place -1  0 "bl" thumb-br-place -1  0 "tl" :steps steps)
:thumb-br-tl-to-bl (thumb-wall-brace-polyhedron-fn thumb-br-place -1  0 "tl" thumb-br-place -1  0 "bl" :steps steps)
:thumb-br-bl-to-br (thumb-wall-brace-polyhedron-fn thumb-br-place  0 -1 "bl" thumb-br-place  0 -1 "br" :steps steps)
:thumb-br-to-mr  (thumb-wall-brace-polyhedron-fn thumb-br-place  0 -1 "br" thumb-mr-place  0 -1 "bl" :steps steps) 
     :thumb-mr-bl-to-br (thumb-wall-brace-polyhedron-fn  thumb-mr-place  0 -1 "bl" thumb-mr-place  0 -1 "br" :steps steps)
:thumb-tr-br-to-tr (thumb-wall-brace-polyhedron-fn thumb-tr-place  1 0 "br" thumb-tr-place  1 -1 "tr" :xy1 wall-xy-offset :xy2 wall-xy-offset-mid :steps steps)
     }    
))

(defn thumb-corners-polyhedron [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [wall-brace-quadratic-fn (get-wall-brace-quadratic-fn bottom-plate)
        thumb-br-bl-corner  (wall-brace-quadratic-fn (partial thumb-br-place) -1 0 "bl" :degrees
                                                     (partial thumb-br-place) -1 -1 "bl" :degrees
                                                     (partial thumb-br-place) 0 -1 "bl" :degrees
                                                     steps)
        thumb-mr-br-corner (wall-brace-quadratic-fn
                            (partial thumb-mr-place) 0 -1 "br" :degrees
                            (partial thumb-mr-place) 1 -1 "br" :degrees
                            (partial thumb-mr-place) 1 0 "br" :degrees
                            steps)
        thumb-mr-to-thumb-tr-corner  (wall-brace-quadratic-fn
                                      (partial thumb-mr-place) 1 0 "br" :degrees
                                      (partial thumb-mr-place) 1 0 "tr" :degrees
                                      (partial thumb-tr-place) 0 -1 "br" :degrees
                                      steps)
        thumb-tr-br-corner (wall-brace-quadratic-fn
                            (partial thumb-tr-place) 0 -1 "br" :degrees
                            (partial thumb-tr-place) 1 -1 "br" :degrees
                            (partial thumb-tr-place) 1 0 "br" :degrees
                            steps)
        ]
  {:thumb-br-bl-corner thumb-br-bl-corner
   :thumb-mr-br-corner thumb-mr-br-corner
   :thumb-mr-to-thumb-tr-corner thumb-mr-to-thumb-tr-corner
   :thumb-tr-br-corner thumb-tr-br-corner}
  )
)


(defn thumb-connecters-polyhedron [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [thumb-mr-tr-top (transform-position thumb-mr-place (web-post-position-top (mapv +  (get-single-plate-corner-position-vector "tr"))))
        thumb-mr-tr-bottom (transform-position thumb-mr-place (web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "tr"))))
        thumb-tr-mr-top (transform-position thumb-tr-place (web-post-position-top (mapv +  (get-single-plate-corner-position-vector "rm"))))
        thumb-tr-mr-bottom (transform-position thumb-tr-place (web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "rm"))))
        thumb-tr-tr-top (transform-position thumb-tr-place (web-post-position-top (mapv +  (get-single-plate-corner-position-vector "tr"))))
        thumb-tr-tr-bottom (transform-position thumb-tr-place (web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "tr"))))
        thumb-tr-tl (web-post-point (partial thumb-tr-place) "tl" :degrees)
        thumb-tr-rm-to-tr-web-post (web-post-linear thumb-tr-place "rm" :degrees thumb-tr-place "tr" :degrees steps)
        index-br-top (transform-position-radians (partial key-place 1 cornerrow) (web-post-position-top (mapv +  (get-single-plate-corner-position-vector "br"))))
        index-br-bottom (transform-position-radians (partial key-place 1 cornerrow) (web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "br"))))
        index-bl (web-post-point (partial key-place 1 cornerrow) "bl" :radians)
        thumb-tl-tr (web-post-point thumb-tl-place "tr" :degrees)
        thumb-tr-mr-curve (wall-brace-polyhedron-curve-points thumb-tr-place 1 0 "rm" :degrees steps)
        thumb-tr-mr-points (wall-brace-polyhedron-points thumb-tr-place 1 0 "rm" :degrees)
        thumb-tr-mr-curve-not-to-floor-outside   (bezier-quartic
                       (thumb-tr-mr-points :web-post-position-top)
                       (thumb-tr-mr-points :wall-locate1-point)
                       (thumb-tr-mr-points :wall-locate-1-to-3-curve-for-polyhedron-control-point)
                       (thumb-tr-mr-points :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)
                       (thumb-tr-mr-points :wall-locate3-point)
                       steps)
       thumb-tr-mr-curve-not-to-floor-inside  (bezier-quadratic
                       (thumb-tr-mr-points :web-post-position-bottom)
                       (thumb-tr-mr-points :wall-locate-2-top)
                       (thumb-tr-mr-points :wall-locate-2-bottom)
                       steps)
        thumb-tr-tr-points (wall-brace-polyhedron-points thumb-tr-place 1 -1 "tr" :degrees wall-xy-offset-mid)               
        thumb-tr-tr-curve-outside (bezier-quintic
                       (thumb-tr-tr-points :web-post-position-top)
                       (thumb-tr-tr-points :wall-locate1-point)
                       (thumb-tr-tr-points :wall-locate-1-to-3-curve-for-polyhedron-control-point)
                       (thumb-tr-tr-points :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)
                       (thumb-tr-tr-points :wall-locate3-point)
                       (thumb-tr-tr-points :wall-locate3-point-floor)
                       steps)
         thumb-tr-tr-curve-inside-adjusted (bezier-cubic
                       (mapv + (thumb-tr-tr-points :web-post-position-bottom) [0 0 (/ web-thickness -1)])
                       (thumb-tr-tr-points :wall-locate-2-top)
                       (thumb-tr-tr-points :wall-locate-2-bottom)
                       (thumb-tr-tr-points :wall-locate-2-bottom-floor)
                       steps)
        thumb-tr-tr-curve (wall-brace-polyhedron-curve-points thumb-tr-place 1 0 "tr" :degrees wall-xy-offset-thin steps )
        index-br-points (wall-brace-polyhedron-points (partial key-place 1 cornerrow) 1 0 "br" :radians wall-xy-offset-thin)
        index-br-curve (wall-brace-polyhedron-curve-points (partial key-place 1 cornerrow) 1 -0.1 "br" :radians wall-xy-offset-medium-thin steps)
        index-br-to-bl-web-post (web-post-linear (partial key-place 1 cornerrow) "br" :radians (partial key-place 1 cornerrow) "bl" :radians steps)
        thumb-tr-to-tl (web-post-linear thumb-tr-place "tr" :degrees thumb-tr-place "tl" :degrees steps)
        thumb-rm-to-tr (web-post-linear thumb-tr-place "rm" :degrees thumb-tr-place "tr" :degrees steps)
        thumb-tr-to-index-br (web-post-linear thumb-tr-place "tr" :degrees (partial key-place 1 cornerrow) "br" :radians steps)
        thumb-tr-to-index-br-top-control-vector [2 0 2]
        thumb-tr-to-index-br-top (bezier-quadratic thumb-tr-tr-top (calculate-point-between-points thumb-tr-tr-top index-br-top thumb-tr-to-index-br-top-control-vector) index-br-top steps)
        thumb-tr-to-index-br-bottom-control-vector [2 0 0]
        thumb-tr-to-index-br-bottom (bezier-quadratic  index-br-bottom (calculate-point-between-points thumb-tr-tr-bottom index-br-bottom thumb-tr-to-index-br-bottom-control-vector) thumb-tr-tr-bottom  steps)
        inner-index-br-point (web-post-point (partial key-place 0 cornerrow) "br" :radians)
        inner-index-bl-point (web-post-point (partial key-place 0 cornerrow) "bl" :radians)
        thumb-tr-to-index-false-wall-brace-floor-points (bezier-linear (thumb-tr-tr-points :wall-locate3-point-floor) (last (index-br-curve :outer-points)) steps)
        ;;  thumb-tr-to-index-polyhedron (generate-bezier-along-bezier-polyhedron-from-points-list-linear
        ;;                                ( thumb-tr-to-tl :top)
        ;;                                (index-br-to-bl-web-post :top)
        ;;                                ( thumb-tr-to-tl :bottom)
        ;;                                (index-br-to-bl-web-post :bottom)
        ;;                                steps
        ;;                                ) 
        thumb-tr-to-index-polyhedron (generate-bezier-along-bezier-polyhedron-from-points-linear
                                      index-br-top thumb-tr-tr-top
                                      (index-bl :top) (thumb-tr-tl :top)
                                      thumb-tr-tr-bottom index-br-bottom
                                      (thumb-tr-tl :bottom) (index-bl :bottom)
                                      steps
                                      ;:outside-upper-control-point-vector  thumb-tr-to-index-br-top-control-vector
                                      :inside-upper-control-point-vector thumb-tr-to-index-br-bottom-control-vector
                                      )
       thumb-tr-rm-to-index-polyhedron (generate-bezier-along-bezier-polyhedron-from-points-list-linear
       thumb-tr-to-index-br-top (thumb-tr-mr-curve :outer-points)
       thumb-tr-to-index-br-bottom (thumb-tr-mr-curve :inner-points)
       steps
       ) 
      ;;(generate-bezier-along-bezier-polyhedron-from-points-linear
      ;;                                 thumb-tr-tr-top index-br-top 
      ;;                                thumb-tr-mr-top index-br-top 
      ;;                                  index-br-bottom thumb-tr-tr-bottom
      ;;                                 index-br-bottom thumb-tr-mr-bottom 
      ;;                                 steps
      ;;                                 :outside-upper-control-point-vector  thumb-tr-to-index-br-top-control-vector
      ;;                                 :inside-upper-control-point-vector thumb-tr-to-index-br-bottom-control-vector
      ;;                                 :outside-lower-control-point-vector [4 0  0]
      ;;                                 :inside-lower-control-point-vector [4 0 0])
        ; (generate-bezier-along-bezier-polyhedron-from-points-linear
        ;;                               (thumb-tr-tl :top) thumb-tr-tr-top 
        ;;                               (thumb-tr-tl :bottom) thumb-tr-tr-bottom 
        ;;                               index-br-top (index-bl :top)  
        ;;                                index-br-bottom (index-bl :bottom)  
        ;;                               steps 
        ;;                               )
        thumb-mr-br-to-thumb-tr-br-curve (web-post-quadratic-curve
                                          thumb-mr-place "br" :degrees
                                          thumb-mr-place "tr" :degrees
                                          thumb-tr-place "br" :degrees
                                          steps)
          thumb-tr-to-index-false-wall-brace-plyhedron (generate-bezier-along-bezier-polyhedron-from-points-list-linear
          (reverse (index-br-curve :inner-points)) (reverse thumb-tr-tr-curve-inside-adjusted)
                                                      (index-br-curve :outer-points) thumb-tr-tr-curve-outside  
                                                      steps
                                                      )           
                                      

        thumb-tr-rm-to-tr-to-index-br-polyhedron (generate-bezier-along-bezier-polyhedron-from-points-list-linear
                                                  thumb-tr-to-index-br-bottom
                                                  (reverse (thumb-tr-mr-curve :inner-points))
                                                  thumb-tr-to-index-br-top
                                                  (thumb-tr-mr-curve :outer-points)
                                                  steps)

        thumb-tr-rm-tr-to-index-br-polyhedron (generate-bezier-to-point-polyhedron
                                               (thumb-tr-rm-to-tr-web-post :top)
                                               index-br-top
                                               (reverse (thumb-tr-rm-to-tr-web-post :bottom))
                                               index-br-bottom)
        index-bl-to-inner-br-to-thumb-tr-tl-polyhedron (generate-bezier-along-bezier-polyhedron-from-points-linear
                                                        (index-bl :top) (thumb-tr-tl :top)
                                                        (inner-index-br-point :top) (thumb-tr-tl :top)
                                                        (thumb-tr-tl :bottom) (index-bl :bottom)
                                                        (thumb-tr-tl :bottom) (inner-index-br-point :bottom)
                                                        steps)
        thumb-tr-mr-curve-bottom-to-index-br (generate-bezier-to-point-polyhedron
                                               (index-br-curve :inner-points)
                                              (last (thumb-tr-mr-curve :inner-points))
                                              (index-br-curve :outer-points)
                                              (last (thumb-tr-mr-curve :outer-points))
                                              )
        inner-index-bl-to-br-thumb-tr-tl-to-thumb-tl-tr-polyhedron (generate-bezier-along-bezier-polyhedron-from-points-linear
                                                                    (inner-index-br-point :top) (thumb-tr-tl :top)
                                                                    (inner-index-bl-point :top) (thumb-tl-tr :top)
                                                                    (thumb-tr-tl :bottom) (inner-index-br-point :bottom)
                                                                    (thumb-tl-tr :bottom) (inner-index-bl-point :bottom)
                                                                    steps)
        polyhedrons (union
                     (generate-bezier-to-point-polyhedron
                      (thumb-mr-br-to-thumb-tr-br-curve :top)
                      thumb-mr-tr-top
                      (reverse (thumb-mr-br-to-thumb-tr-br-curve :bottom))
                      thumb-mr-tr-bottom)
                     thumb-tr-to-index-false-wall-brace-plyhedron
                     thumb-tr-to-index-polyhedron


           ;horizontal

                     (generate-polyhedron-thumb-web-connecters
                      thumb-bl-place "bl" thumb-bl-place "br"
                      thumb-br-place "tl" thumb-br-place "tr"
                      :steps steps)

                     (generate-polyhedron-thumb-web-connecters
                      thumb-bl-place "br" thumb-tl-place "bl"
                      thumb-br-place "tr" thumb-mr-place "tl"
                      :steps steps)


                     (generate-polyhedron-thumb-web-connecters
                      thumb-tl-place "bl" thumb-tl-place "br"
                      thumb-mr-place "tl" thumb-mr-place "tr"
                      :steps steps)

                     (generate-polyhedron-thumb-web-connecters
                      thumb-tl-place "br" thumb-tr-place "bl"
                      thumb-mr-place "tr" thumb-mr-place "tr"
                      :steps steps)

                     (generate-polyhedron-thumb-web-connecters
                      thumb-tr-place "bl" thumb-tr-place "br"
                      thumb-mr-place "tr" thumb-tr-place "br"
                      :steps steps)

           ;vertical

                     (generate-polyhedron-thumb-web-connecters
                      thumb-bl-place "tr" thumb-tl-place "tl"
                      thumb-bl-place "br" thumb-tl-place "bl"
                      :steps steps)


                     (generate-polyhedron-thumb-web-connecters
                      thumb-tl-place "tr" thumb-tr-place "tl"
                      thumb-tl-place "br" thumb-tr-place "bl"
                      :steps steps)

                     (generate-polyhedron-thumb-web-connecters
                      thumb-br-place "tr" thumb-mr-place "tl"
                      thumb-br-place "br" thumb-mr-place "bl"
                      :steps steps)

                     index-bl-to-inner-br-to-thumb-tr-tl-polyhedron
                     inner-index-bl-to-br-thumb-tr-tl-to-thumb-tl-tr-polyhedron)]
                     (if (true? bottom-plate) thumb-tr-to-index-false-wall-brace-floor-points polyhedrons) ))


 (defn key-web-connecters-polyhedron [steps]
 (let [ column-fn (fn [column row]
        (generate-polyhedron-key-web-connecters
         (partial key-place column row) "tr" (partial key-place (inc column) row) "tl"
         (partial key-place column row) "br" (partial key-place (inc column) row) "bl" 
         :steps steps
         )
        )
  column-connections (for [column (range 0 lastcol)
         row (range 0 (dec lastrow)) ]
         (column-fn column row)
         )
  bottom-row-connections [(column-fn 0 2) (column-fn 2 2) (column-fn 3 2)
  (generate-polyhedron-key-web-connecters
         (partial key-place 1 2) "tr" (partial key-place 2 2) "tl"
         (partial key-place 2 2) "bl" (partial key-place 2 2) "bl" 
         :steps steps
         )
         (generate-polyhedron-key-web-connecters
         (partial key-place 1 2) "tr" (partial key-place 2 2) "bl"
         (partial key-place 1 2) "br" (partial key-place 1 2) "br" 
         :steps steps
         )
  ]
   diagonal-connections (for [column (range 0 lastcol)
                        row (range 0 cornerrow)]
          (generate-polyhedron-key-web-connecters
         (partial key-place column row) "br" (partial key-place (inc column) row) "bl"
         (partial key-place column (inc row)) "tr" (partial key-place (inc column) (inc row)) "tl" 
         :steps steps
         )                
                        )  
         
   row-connections (for [column columns
                        row (range 0 cornerrow)]
          (generate-polyhedron-key-web-connecters
         (partial key-place column row) "bl" (partial key-place column row) "br" 
         (partial key-place column (inc row)) "tl" (partial key-place column (inc row)) "tr" 
         :steps steps
         )                
                        )                     
         ]
         (union
          (front-wall-connecters-polyhedron steps)
          (apply union
         (concat 
         column-connections
         row-connections
         diagonal-connections
         bottom-row-connections 
         ) 
         ))
         
         )
 )          
(defn right-side-polyhedron [steps]
  (let [top-row-web-post-tl-top (key-position 0 0 (mapv + web-post-translation-vector web-post-tl-translation-vector [0 0 (/ web-thickness 2)]))
        top-row-web-post-bl-top (key-position 0 0 (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness 2)]))

        middle-row-web-post-tl-top (key-position 0 1 (mapv + web-post-translation-vector web-post-tl-translation-vector [0 0 (/ web-thickness 2)]))
        middle-row-web-post-bl-top (key-position 0 1 (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness 2)]))

        third-row-web-post-tl-top (key-position 0 2 (mapv + web-post-translation-vector web-post-tl-translation-vector [0 0 (/ web-thickness 2)]))
        third-row-web-post-bl-top (key-position 0 2 (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness 2)]))

        top-row-web-post-tl-bottom (key-position 0 0 (mapv + web-post-translation-vector web-post-tl-translation-vector [0 0 (/ web-thickness -2)]))
        top-row-web-post-bl-bottom (key-position 0 0 (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness -2)]))

        middle-row-web-post-tl-bottom (key-position 0 1 (mapv + web-post-translation-vector web-post-tl-translation-vector [0 0 (/ web-thickness -2)]))
        middle-row-web-post-bl-bottom (key-position 0 1 (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness -2)]))

        third-row-web-post-tl-bottom (key-position 0 2 (mapv + web-post-translation-vector web-post-tl-translation-vector [0 0 (/ web-thickness -2)]))
        third-row-web-post-bl-bottom (key-position 0 2 (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness -2)]))
        number-of-slices 14
        total-width (+ tps-65-width tps-65-tolerance)
        max-width-from-zero (/ total-width 2)
        tps-65-width-slice  (/ total-width number-of-slices)
        position-adjustment (fn [upper-or-lower] (mapv + oled-translation-vector
                                                       [0 (+ (tps-radius-compensation-adjust (- tps-65-mount-corner-radius)) (/ oled-post-size 2) -0.1) (/ oled-holder-thickness (cond (= upper-or-lower :upper) 2
                                                                                                                                                                                       (= upper-or-lower :lower) -2))]))

        tps-65-point-to-connect-to-top-row-web-post-tl-top (transform-position (partial tps-65-translate-and-place-at-position [max-width-from-zero
                                                                                                                                (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                (- plate-thickness)])
                                                                               (position-adjustment :upper))
        tps-65-point-to-connect-to-top-row-web-post-bl-top (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 4))
                                                                                                                                (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                (- plate-thickness)])
                                                                               (position-adjustment :upper))
        tps-65-point-to-connect-to-middle-row-web-post-tl-top (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 5))
                                                                                                                                   (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                   (- plate-thickness)]) (position-adjustment :upper))
        tps-65-point-to-connect-to-middle-row-web-post-bl-top (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 6))
                                                                                                                                   (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                   (- plate-thickness)]) (position-adjustment :upper))
        tps-65-point-to-connect-to-third-row-web-post-tl-top (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 10))
                                                                                                                                  (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                  (- plate-thickness)]) (position-adjustment :upper))
        tps-65-point-to-connect-to-third-row-web-post-bl-top (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 14))
                                                                                                                                  (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                  (- plate-thickness)]) (position-adjustment :upper))

        tps-65-point-to-connect-to-top-row-web-post-tl-bottom (transform-position (partial tps-65-translate-and-place-at-position [max-width-from-zero
                                                                                                                                   (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                   (- plate-thickness)]) (position-adjustment :lower))
        tps-65-point-to-connect-to-top-row-web-post-bl-bottom (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 4))
                                                                                                                                   (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                   (- plate-thickness)]) (position-adjustment :lower))
        tps-65-point-to-connect-to-middle-row-web-post-tl-bottom (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 5))
                                                                                                                                      (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                      (- plate-thickness)]) (position-adjustment :lower))
        tps-65-point-to-connect-to-middle-row-web-post-bl-bottom (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 6))
                                                                                                                                      (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                      (- plate-thickness)]) (position-adjustment :lower))
        tps-65-point-to-connect-to-third-row-web-post-tl-bottom (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 10))
                                                                                                                                     (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                     (- plate-thickness)]) (position-adjustment :lower))
        tps-65-point-to-connect-to-third-row-web-post-bl-bottom (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-width-slice 14))
                                                                                                                                     (- tps-65-corner-radius (/ tps-65-length 2))
                                                                                                                                     (- plate-thickness)]) (position-adjustment :lower))

        thumb-tl-tl-web-post-top (transform-position (partial thumb-tl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-tl-tl-web-post-bottom (transform-position (partial thumb-tl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        thumb-tl-tr-web-post-top (transform-position (partial thumb-tl-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-tl-tr-web-post-bottom (transform-position (partial thumb-tl-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))

        tps-65-to-top-row (generate-bezier-along-bezier-polyhedron-from-points-linear
                           tps-65-point-to-connect-to-top-row-web-post-tl-top top-row-web-post-tl-top
                           tps-65-point-to-connect-to-top-row-web-post-bl-top top-row-web-post-bl-top
                           top-row-web-post-tl-bottom tps-65-point-to-connect-to-top-row-web-post-tl-bottom
                           top-row-web-post-bl-bottom tps-65-point-to-connect-to-top-row-web-post-bl-bottom
                           steps
                           :outside-upper-control-point-vector [0 0 -1] :outside-lower-control-point-vector [0 0 -1]
                           :inside-upper-control-point-vector [0 0 -1] :inside-lower-control-point-vector [0 0 -1])
        tps-65-to-top-and-middle-row (generate-bezier-along-bezier-polyhedron-from-points-linear
                                      tps-65-point-to-connect-to-top-row-web-post-bl-top top-row-web-post-bl-top
                                      tps-65-point-to-connect-to-middle-row-web-post-tl-top middle-row-web-post-tl-top
                                      top-row-web-post-bl-bottom tps-65-point-to-connect-to-top-row-web-post-bl-bottom
                                      middle-row-web-post-tl-bottom tps-65-point-to-connect-to-middle-row-web-post-tl-bottom
                                      steps
                                      :outside-upper-control-point-vector [0 0 -1] :outside-lower-control-point-vector [0 0 -1]
                                      :inside-upper-control-point-vector [0 0 -1] :inside-lower-control-point-vector [0 0 -1])

        tps-65-to-middle-row (generate-bezier-along-bezier-polyhedron-from-points-linear
                              tps-65-point-to-connect-to-middle-row-web-post-tl-top middle-row-web-post-tl-top
                              tps-65-point-to-connect-to-middle-row-web-post-bl-top middle-row-web-post-bl-top
                              middle-row-web-post-tl-bottom tps-65-point-to-connect-to-middle-row-web-post-tl-bottom
                              middle-row-web-post-bl-bottom tps-65-point-to-connect-to-middle-row-web-post-bl-bottom
                              steps
                              :outside-upper-control-point-vector [0 0 -1] :outside-lower-control-point-vector [0 0 -1]
                              :inside-upper-control-point-vector [0 0 -1] :inside-lower-control-point-vector [0 0 -1])
        tps-65-to-middle-and-third-row (generate-bezier-along-bezier-polyhedron-from-points-linear
                                        tps-65-point-to-connect-to-middle-row-web-post-bl-top middle-row-web-post-bl-top
                                        tps-65-point-to-connect-to-third-row-web-post-tl-top third-row-web-post-tl-top
                                        middle-row-web-post-bl-bottom tps-65-point-to-connect-to-middle-row-web-post-bl-bottom
                                        third-row-web-post-tl-bottom tps-65-point-to-connect-to-third-row-web-post-tl-bottom
                                        steps
                                        :outside-upper-control-point-vector [0 0 -1] :outside-lower-control-point-vector [0 0 -1]
                                        :inside-upper-control-point-vector [0 0 -1] :inside-lower-control-point-vector [0 0 -1])

        tps-65-to-third-row (generate-bezier-along-bezier-polyhedron-from-points-linear
                             tps-65-point-to-connect-to-third-row-web-post-tl-top third-row-web-post-tl-top
                             tps-65-point-to-connect-to-third-row-web-post-bl-top third-row-web-post-bl-top
                             third-row-web-post-tl-bottom tps-65-point-to-connect-to-third-row-web-post-tl-bottom
                             third-row-web-post-bl-bottom tps-65-point-to-connect-to-third-row-web-post-bl-bottom
                             steps
                             :outside-upper-control-point-vector [0 0 -1] :outside-lower-control-point-vector [0 0 -1]
                             :inside-upper-control-point-vector [0 0 -1] :inside-lower-control-point-vector [0 0 -1])

        tps-65-to-third-row-and-thumb-tl-tl (generate-bezier-along-bezier-polyhedron-from-points-linear
                                             tps-65-point-to-connect-to-third-row-web-post-bl-top third-row-web-post-bl-top
                                             thumb-tl-tl-web-post-top thumb-tl-tr-web-post-top
                                             third-row-web-post-bl-bottom tps-65-point-to-connect-to-third-row-web-post-bl-bottom
                                             thumb-tl-tr-web-post-bottom thumb-tl-tl-web-post-bottom
                                             steps
                                             :outside-upper-control-point-vector [0 0 -1] :outside-lower-control-point-vector [0 0 0]
                                             :inside-upper-control-point-vector [0 0 -1] :inside-lower-control-point-vector [0 0 0])]
    (union
     tps-65-to-top-row
     tps-65-to-top-and-middle-row
     tps-65-to-middle-row
     tps-65-to-middle-and-third-row
     tps-65-to-third-row
     tps-65-to-third-row-and-thumb-tl-tl)))



(defn left-section-back [steps &{:keys [bottom-plate] :or {bottom-plate false}}]
  (let [wall-brace-quadratic-fn (get-wall-brace-quadratic-fn bottom-plate) 
        wall-brace-polyhedron-fn (get-wall-brace-polyhedron-fn bottom-plate)
        collect-fn (get-collect-fn bottom-plate)
        ]
    
    (collect-fn
     
    ;;  (wall-brace-quadratic-polyhedron (partial key-place 0 0) -1 0 "tl" :radians
    ;;                         (partial key-place 0 0) -1 1 "tl" :radians
    ;;                         (partial key-place 0 0) 0 1 "tl" :radians
    ;;                         steps)

     
     
     (wall-brace-quadratic-fn
     (partial key-place 0 0) 0 1 "tl" :radians 
                            (partial key-place 0 0) -1 1 "tl" :radians
                            tps-65-bottom-right 1 1 "centre" :degrees
                            steps)
     (wall-brace-polyhedron-fn tps-65-bottom-right 1 1 "centre" :degrees
                            tps-65-top-right 1 0 "centre" :degrees
                            steps)
    (wall-brace-quadratic-fn
     tps-65-top-right 1 0 "centre" :degrees 
     tps-65-top-right 1 1 "centre" :degrees
     tps-65-top-right 0 1 "centre" :degrees
     steps) 
     )
    
    )
  )

(defn polyhedron-thumb-walls [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [collect-fn (get-collect-fn bottom-plate)
        thumb-walls-polyhedron-map (thumb-walls-polyhedron steps :bottom-plate bottom-plate)
thumb-corners-polyhedron-map (thumb-corners-polyhedron steps :bottom-plate bottom-plate)]
   (collect-fn
   (thumb-walls-polyhedron-map :thumb-bl-tl-to-bl)
   (thumb-walls-polyhedron-map :thumb-bl-to-br)
    (thumb-walls-polyhedron-map :thumb-br-tl-to-bl)
    (thumb-corners-polyhedron-map :thumb-br-bl-corner)
    (thumb-walls-polyhedron-map :thumb-br-bl-to-br)
    (thumb-walls-polyhedron-map :thumb-br-to-mr)
    (thumb-walls-polyhedron-map :thumb-mr-bl-to-br)
    (thumb-corners-polyhedron-map :thumb-mr-br-corner) 
   (thumb-corners-polyhedron-map :thumb-mr-to-thumb-tr-corner)
   (thumb-corners-polyhedron-map :thumb-tr-br-corner) 
    (thumb-walls-polyhedron-map :thumb-tr-br-to-tr)
   
   ))
  )
(defn polyhedron-left-section [steps]
  (union
   (left-section-front-polyhedron steps)
  (back-left-wall-to-screen steps)
  (left-section-back steps)
   under-screen
   ) 
  )

(defn polyhedron-case-walls [steps]
  (union
   (right-wall-polyhedron steps)
  (front-wall-polyhedron steps)
  
  (back-wall-polyhedron steps)))

(defn usb-jack-shape [circle-diameter height steps] 
  (let [
        right-circle-floor-quadrant [(- (/ usb-jack-width 2) (/ usb-jack-height 2)) 0 height]
        left-circle-floor-quadrant [(- (/ usb-jack-height 2) (/ usb-jack-width 2)) 0 height]
        bezier-circle-quadrant #(bezier-quadratic (nth % 0) (nth % 1) (nth % 2) steps)
        top-right-circle-floor-quadrant-coordinates (mapv #(mapv + right-circle-floor-quadrant %) [[0 circle-diameter 0] [circle-diameter circle-diameter 0] [circle-diameter 0 0]])
        bottom-right-circle-floor-quadrant-coordinates (mapv #(mapv + right-circle-floor-quadrant %) [ [circle-diameter 0 0] [circle-diameter (- circle-diameter) 0] [0 (- circle-diameter) 0] ])
        top-right-circle-floor-quadrant-bezier-curve-points (bezier-quadratic (nth top-right-circle-floor-quadrant-coordinates 0)
                                                                              (nth top-right-circle-floor-quadrant-coordinates 1)
                                                                              (nth top-right-circle-floor-quadrant-coordinates 2)
                                                                              steps)
        top-left-circle-floor-quadrant-coordinates (mapv #(mapv + left-circle-floor-quadrant %) [[(- circle-diameter) 0 0] [(- circle-diameter) circle-diameter 0] [0 circle-diameter 0]])
        bottom-left-circle-floor-quadrant-coordinates (mapv #(mapv + left-circle-floor-quadrant %) [[0 (- circle-diameter) 0] [(- circle-diameter) (- circle-diameter) 0] [(- circle-diameter) 0 0] ])
       bottom-right-circle-floor-quadrant-bezier-curve-points (bezier-circle-quadrant bottom-right-circle-floor-quadrant-coordinates)
       top-left-circle-floor-quadrant-bezier-curve-points (bezier-circle-quadrant top-left-circle-floor-quadrant-coordinates)
        bottom-left-circle-floor-quadrant-bezier-curve-points (bezier-circle-quadrant bottom-left-circle-floor-quadrant-coordinates)
        top-linear (bezier-linear (nth top-left-circle-floor-quadrant-coordinates 2) (nth top-right-circle-floor-quadrant-coordinates 0)  steps)
       bottom-linear (bezier-linear (nth bottom-right-circle-floor-quadrant-coordinates 2) (nth bottom-left-circle-floor-quadrant-coordinates 0) steps)
        ]
     (concat 
      (drop-last top-right-circle-floor-quadrant-bezier-curve-points) 
     (drop-last bottom-right-circle-floor-quadrant-bezier-curve-points) 
     (drop-last bottom-linear) 
     (drop-last bottom-left-circle-floor-quadrant-bezier-curve-points) 
     (drop-last top-left-circle-floor-quadrant-bezier-curve-points) 
      top-linear )
     
     
     ))


(def usb-jack-polyhedron
  (let [steps 72
        bottom-points (reverse (usb-jack-shape usb-jack-height 0 (/ steps 6)))
        control-points (reverse (usb-jack-shape (/ usb-jack-height 2) 0 (/ steps 6)))
        upper-points  (reverse (usb-jack-shape (/ usb-jack-height 2) (/ usb-jack-height 2) (/ steps 6)))
        upper-inner-half (bezier-linear [(- (/ usb-jack-width 2) (/ usb-jack-height 2)) 0 (/ usb-jack-height 2)] [(- (/ usb-jack-height 2) (/ usb-jack-width 2)) 0 (/ usb-jack-height 2)] (/ steps 2))
        upper-inner-points (concat (drop-last upper-inner-half) (reverse upper-inner-half))
        cylinder-top-z (+ plate-thickness 1.5)
        centre-bottom-point [0 0 0]
        centre-top-point [0 0 (/ usb-jack-height 2)] 
        cylinder-centre-bottom-point centre-top-point 
        cylinder-centre-top-point [0 0 cylinder-top-z]
        lower-cylinder-points upper-points
        upper-cylinder-points  (reverse (usb-jack-shape (/ usb-jack-height 2) cylinder-top-z (/ steps 6)))
        
        side-points (into [] (apply concat
                                    (for [index (range 0 (inc steps))]
                                      (bezier-quadratic
                                       (nth upper-points index)
                                       (nth control-points index)
                                       (nth bottom-points index)
                                       steps))))
        side-faces (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps)
        centre-top-point-index (count side-points)
        centre-bottom-point-index (inc centre-top-point-index)
        top-faces (for [index (range 0 (inc steps))]
                    [(* (inc steps) index) centre-top-point-index (* (inc steps) (inc index))])
        bottom-faces (for [index (range 0 steps)]
                       [(+ steps (* (inc steps) index))  (+ (*  (inc steps) (inc index)) steps) centre-bottom-point-index])
        points (concat side-points [centre-top-point centre-bottom-point])
        faces (into [] (concat top-faces side-faces  bottom-faces))
        cylinder-side-points (into [] (apply concat
                                             (for [index (range 0 (inc steps))]
                                               (bezier-linear
                                                (nth upper-cylinder-points index)
                                                (nth upper-points index)
                                                steps
                                                ))))
        cylinder-centre-top-point-index (count cylinder-side-points)
        cylinder-centre-bottom-point-index (inc cylinder-centre-top-point-index)
        cylinder-top-faces (for [index (range 0 (inc steps))]
                    [(* (inc steps) index) cylinder-centre-top-point-index (* (inc steps) (inc index))])
        cylinder-side-faces side-faces
        cylinder-bottom-faces (for [index (range 0 steps)]
                       [(+ steps (* (inc steps) index))  (+ (*  (inc steps) (inc index)) steps) cylinder-centre-bottom-point-index])
        cylinder-points (concat cylinder-side-points [cylinder-centre-top-point cylinder-centre-bottom-point])
        cylinder-faces (concat cylinder-top-faces cylinder-side-faces cylinder-bottom-faces)
        ] 
    (translate [0 -2.25 0](union
     (translate [0  plate-thickness 0](rdx 90 (polyhedron points faces)))
    (translate [0   plate-thickness 0] (rdx 90 (polyhedron cylinder-points cylinder-faces) ))))
    )
  )