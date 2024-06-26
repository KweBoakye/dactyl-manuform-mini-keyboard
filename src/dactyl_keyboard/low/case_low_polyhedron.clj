(ns dactyl-keyboard.low.case-low-polyhedron
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.EVQWGD001 :refer :all]
            [dactyl-keyboard.lib.algebra :refer [find-point-on-line-using-x
                                                 find-point-on-line-using-y
                                                 find-point-on-line-using-z]]
            [dactyl-keyboard.lib.curvesandsplines.beziers :refer [bezier-cubic
                                                                  bezier-cubic-through-control-points bezier-linear bezier-quadratic bezier-quartic
                                                                  bezier-quintic]]
            [dactyl-keyboard.lib.curvesandsplines.splines :refer [catmull-rom-spline-curve catmull-rom-spline-segment]]
            [dactyl-keyboard.lib.openscad.hull :refer [chained-hull-to-points]]
            [dactyl-keyboard.lib.openscad.polyhedrons :refer [bezier-along-bezier-polyhedron-generate-front-or-back-faces bezier-along-bezier-polyhedron-generate-side-reverse
                                                              bezier-along-bezier-polyhedron-generate-side-reverse-2
                                                              bezier-along-bezier-polyhedron-generate-side-reverse-3 generate-bezier-along-bezier-polyhedron
                                                              generate-bezier-along-bezier-polyhedron-faces
                                                              generate-bezier-along-bezier-polyhedron-from-points-linear generate-bezier-along-bezier-polyhedron-from-points-list-linear
                                                              generate-bezier-quadratic-polyhedron-from-points
                                                              generate-bezier-quadratic-polyhedron-from-points-and-control-vectors generate-bezier-to-point-polyhedron
                                                              generate-polyhedron-from-points]]
            [dactyl-keyboard.lib.transformations :refer [rdx]]
            [dactyl-keyboard.lib.vectors :refer [calculate-point-between-points vector-distance]]
            [dactyl-keyboard.low.case-low :refer :all]
            [dactyl-keyboard.low.case-low-functions :refer :all]
            [dactyl-keyboard.low.case-low-polyhedron-functions :refer :all]
            [dactyl-keyboard.low.EVQWGD001-placement-functions :refer [EVQWGD001-translate-and-place-at-position]]
            [dactyl-keyboard.low.oled-low-placements :refer :all]
            [dactyl-keyboard.low.placement-functions-low :refer :all]
            [dactyl-keyboard.low.screen-holder-placement-functions :refer :all]
            [dactyl-keyboard.low.screen-holder-placement-points :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.low.thumbs-low :refer :all]
            [dactyl-keyboard.low.tps-65-placement-functions :refer :all]
            [dactyl-keyboard.low.tps-65-placement-points :refer :all]
            [dactyl-keyboard.low.web-connecters-low :refer :all]
            [dactyl-keyboard.oled :refer :all]
            [dactyl-keyboard.sa-keycaps :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.tps-65 :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all])
            )









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


(def screen-holder-top-right-outside-point-translated-for-left-section (mapv + [(- (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2)))) (/ (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2))) -2) 0] screen-holder-top-right-outside-point))
(def screen-holder-top-right-inside-point-translated-for-left-section (mapv + [(- (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2)))) (/ (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2))) -2) 0] screen-holder-top-right-inside-point))
(def screen-holder-bottom-right-outside-point-translated-for-left-section (mapv + [(- (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2)))) (/ (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2))) -2) 0] screen-holder-bottom-right-outside-point))
(def screen-holder-bottom-right-outside-floor-point-translated-for-left-section (mapv + [(- (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2)))) (/ (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2))) -2) 0] screen-holder-bottom-right-outside-floor-point))
(def screen-holder-bottom-right-inside-floor-point-translated-for-left-section (mapv + [(- (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2)))) (/ (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2))) -2) 0] screen-holder-bottom-right-inside-floor-point))
(defn screen-holder-points-fn [top mid bottom steps] (concat (drop-last (bezier-linear top mid (- steps 1)))
                                                           [mid bottom]))
;;  (defn tps-65-outside-control-points-1 [steps] (bezier-linear tps-65-top-right-outer tps-65-mid-right-outer  steps))
;; (defn tps-65-outside-points-1 [steps] (bezier-linear tps-65-top-left-outer tps-65-mid-left-outer steps))
;; (defn thumb-bl-tl [steps] (wall-brace-polyhedron-points thumb-bl-place -1 0 "tl"  :degrees steps))
;; ;[thumb-bl-tr (wall-brace-polyhedron-points thumb-bl-place 0 -1 "tr"  :degrees steps)]
;; (defn thumb-br-tl [steps] (wall-brace-polyhedron-points thumb-br-place -1 0 "tl"  :degrees steps))

;; (defn left-section-to-thumb-cluster-convex-walls-top-outside [steps] (reverse (catmull-rom-spline-curve
;;                                                                  ((thumb-br-tl steps) :web-post-position-top)
;;                                                                  ((thumb-bl-tl steps) :web-post-position-top)
;;                                                                  screen-holder-top-right-outside-point
;;                                                                  screen-holder-top-left-outside-point
;;                                                                  steps
;;                                                                  :alpha-type :centripetal :t1 0.7 :t2 0.9)))
;; (defn left-section-to-thumb-cluster-convex-walls-bottom-outside [steps] (reverse (catmull-rom-spline-curve
;;                                                                           ((thumb-br-tl steps) :wall-locate3-point-floor)
;;                                                                           ((thumb-bl-tl steps) :wall-locate3-point-floor)
;;                                                                           screen-holder-bottom-right-outside-floor-point
;;                                                                           screen-holder-bottom-left-outside-floor-point
;;                                                                           steps
;;                                                                           :alpha-type :centripetal :t1 0.7 :t2 0.9)))
;; (defn outside-points-1 [steps] (into [] (apply concat
;;                                           (for [index (range 0 (inc steps))
;;                                                 :let [control-points (tps-65-outside-control-points-1 steps)
;;                                                       points (tps-65-outside-points-1 steps)
;;                                                       left-section-to-thumb-cluster-convex-walls-top-outside-points (left-section-to-thumb-cluster-convex-walls-top-outside steps)
;;                                                       left-section-to-thumb-cluster-convex-walls-bottom-outside-points (left-section-to-thumb-cluster-convex-walls-bottom-outside steps)]] 
;;                                             (catmull-rom-spline-curve
;;                                              (nth control-points index)
;;                                              (nth points index)
;;                                              (nth left-section-to-thumb-cluster-convex-walls-top-outside-points index)
;;                                              (nth left-section-to-thumb-cluster-convex-walls-bottom-outside-points index)
;;                                              steps
;;                                              :alpha-type :centripetal :t1 0.7 :t2 0.9)))))
(defn back-left-wall-to-screen [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [half-steps1 (if (even? steps) (/ steps 2) (/ (inc steps) 2))
        half-steps2 (if (even? steps) (/ steps 2) (/ (dec steps) 2))

        tps-65-top-right-web-post-outside-point (transform-position
                                                 tps-65-top-right
                                                 [0 0 (/ web-thickness 2)])




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
                                    (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-mid-position)
                                    [0  (+ tps-65-corner-radius 0.05) 0])

        tps-65-top-middle-inner    (transform-position
                                    (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-mid-position)
                                    [0  (+ tps-65-corner-radius 0.05) (/ web-thickness -2)])
       
        tps-65-top-right-to-top-left-outer (bezier-linear  tps-65-top-right-outer tps-65-top-left-outer  (* steps 2))

        tps-65-top-right-control-point-outer (transform-position
                                              (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position  tps-65-mount-corner-radius-with-offset-mod  tps-65-mount-corner-radius-with-offset)
                                              (mapv + oled-translation-vector [(/ oled-post-size -2) (/ oled-post-size 2) (/ oled-holder-thickness 2)]))
        tps-65-top-middle-control-point-outer (transform-position
                                               (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-mid-position  0  tps-65-mount-corner-radius-with-offset)
                                               (mapv + oled-translation-vector [0 (/ oled-post-size 2) (/ web-thickness 2)]))
        tps-65-top-middle-control-point-inner (transform-position
                                               (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-mid-position  0  tps-65-mount-corner-radius-with-offset)
                                               (mapv + oled-translation-vector [0 (/ oled-post-size 2) (/ web-thickness -2)]))
        tps-65-screen-side-upper-control-points (bezier-linear tps-65-top-right-control-point-outer tps-65-top-left-control-point-outer  (* steps 2))
        tps-65-top-right-wall-to-screen-holder-top-right-points-outer (concat top-bezier-points (reverse (drop 1 (bezier-linear  screen-holder-top-right-outside-point screen-holder-top-left-outside-point steps))))


       
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
                                                                                            (calculate-point-between-points (tps-65-top-right-points :wall-locate3-point-floor) screen-holder-bottom-left-outside-floor-point control-vector)
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
                                                                                            (calculate-point-between-points (tps-65-top-right-points :wall-locate-2-bottom-floor) screen-holder-bottom-left-inside-floor-point control-vector)
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
                                                                                                                   screen-holder-inner-points (bezier-linear 
                                                                                                                                               tps-65-top-left-inner-with-same-z-as-screen-holder-top-right-inside-point-translated-with-same-y-as-tps-65-top-left-inner
                                                                                                                                               ;screen-holder-inside-point-with-same-y-as-tps-65-top-left-inner
                                                                                                                                                             screen-holder-top-left-inside-point steps)]]
                                                                                                         (bezier-linear
                                                                                                          (nth tps-65-points index)
                                                                                            ;(nth control-points index)
                                                                                                          (nth screen-holder-inner-points index)
                                                                                                          steps))))
        tps-65-screen-side-bezier-along-bezier-polyhedron (polyhedron (concat tps-65-screen-side-bezier-along-bezier-point-outer tps-65-screen-side-bezier-along-bezier-point-inner)
                                                                      (generate-bezier-along-bezier-polyhedron-faces
                                                                       tps-65-screen-side-bezier-along-bezier-point-outer
                                                                       tps-65-screen-side-bezier-along-bezier-point-inner steps))
        tps-65-screen-side-bezier-along-bezier-middle-to-screen-holder-right-polyhedron (generate-bezier-along-bezier-polyhedron tps-65-screen-side-bezier-along-bezier-middle-to-right-outer-points tps-65-screen-side-bezier-along-bezier-middle-to-screen-holder-right-inner-points steps) 
        ;; (polyhedron (concat tps-65-screen-side-bezier-along-bezier-middle-to-right-outer-points tps-65-screen-side-bezier-along-bezier-middle-to-screen-holder-right-inner-points)
        ;;                                                                                             (generate-bezier-along-bezier-polyhedron-faces
        ;;                                                                                              tps-65-screen-side-bezier-along-bezier-middle-to-right-outer-points
        ;;                                                                                              tps-65-screen-side-bezier-along-bezier-middle-to-screen-holder-right-inner-points
        ;;                                                                                              steps))
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
                    ;;  (chained-hull-for-two-lists (plot-bezier-points
                    ;;                               (bezier-cubic
                    ;;                               screen-holder-outside-point-with-same-y-as-tps-65-top-left-upper
                    ;;                               screen-holder-top-right-inside-point-translated-with-same-y-as-tps-65-top-left-inner
                    ;;                               tps-65-top-left-control-point-outer
                    ;;                               tps-65-top-left-outer
                    ;;                               steps)
                    ;;                               (sphere 0.001)
                    ;;                               ) 
                    ;;                              (plot-bezier-points (reverse (take (inc steps) (outside-points-1 steps))) (sphere 0.001))
                    ;;                              steps
                    ;;                              )
                     
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
                                            )))
                     )]
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
        thumb-bl-tthumb-bl-to-tps-65-bottom-leftside (transform-position
                             (partial thumb-bl-place) (mapv + (wall-locate3-for-polyhedron-point -1 0) curve-post-tl-translation-vector curve-post-translation-vector [(/ curve-post-size 2) (/ curve-post-size 2) (- oled-holder-thickness curve-post-size)]))
        thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside (transform-position
                            (partial thumb-bl-place) (mapv + (wall-locate2 -1 0) oled-post-tl-translation-vector oled-translation-vector [(/ oled-post-size 2) (/ oled-post-size 2)  0]))
        thumb-bl-tthumb-bl-to-tps-65-bottom-leftside-floor (assoc (vec thumb-bl-tthumb-bl-to-tps-65-bottom-leftside) 2 0)
        thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside-floor (assoc (vec thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside) 2 0)
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
                                          (partial tps-65-translate-and-place-with-radius [(- tps-65-corner-radius (/ tps-65-mount-width 2))
                                                                                           (/ (- (/ tps-65-mount-length 2) tps-65-corner-radius) 2)
                                                                                           0] (- tps-65-mount-corner-radius)  0)
                                          (mapv + oled-translation-vector [(/ oled-post-size 2) 0 (/ oled-holder-thickness 2)]))
                                         steps)

        trackpad-thumb-side-points-left-lower (bezier-linear


                                               (transform-position
                                                (partial tps-65-translate-and-place-with-radius [(- tps-65-corner-radius (/ tps-65-mount-width 2))
                                                                                                 (/ (- (/ tps-65-mount-length 2) tps-65-corner-radius) 2)
                                                                                                 0] (- tps-65-mount-corner-radius)  0)
                                                (mapv + oled-translation-vector [(/ oled-post-size 2) 0 (/ oled-holder-thickness -2)]))
                                               (transform-position
                                                (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius)  tps-65-mount-corner-radius)
                                                (mapv + oled-translation-vector [(/ oled-post-size 2) (/ oled-post-size 2) (/ oled-holder-thickness -2)]))
                                               steps)
        trackpad-thumb-side-points-right (bezier-linear
                                          (nth trackpad-thumb-side-points-left  steps)
                                          ;; (transform-position
                                          ;;  (partial tps-65-translate-and-place-with-radius [(- tps-65-corner-radius (/ tps-65-mount-width 2))
                                          ;;                                                   (/ (- (/ tps-65-mount-length 2) tps-65-corner-radius) 2)
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
                                                 (partial tps-65-translate-and-place-with-radius [(- tps-65-corner-radius (/ tps-65-mount-width 2))
                                                                                                  (/ (- (/ tps-65-mount-length 2) tps-65-corner-radius) 2)
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
        EVQWGD001-mount-bottom-right-outside-to-thumb-bl-tthumb-bl-to-tps-65-bottom-leftside-control-point (calculate-point-between-points EVQWGD001-mount-bottom-right-outside thumb-bl-tthumb-bl-to-tps-65-bottom-leftside [0 -2 0])
        EVQWGD001-mount-bottom-right-inside-to-thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside-control-point (calculate-point-between-points EVQWGD001-mount-bottom-right-inside thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside [0 -2 0])
        EVQWGD001-mount-bottom-right-outside-floor-to-thumb-bl-tthumb-bl-to-tps-65-bottom-leftside-floor-control-point (calculate-point-between-points EVQWGD001-mount-bottom-right-outside-floor thumb-bl-tthumb-bl-to-tps-65-bottom-leftside-floor [0 -2 0])
        EVQWGD001-mount-bottom-right-inside-floor-to-thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside-floor-control-point (calculate-point-between-points EVQWGD001-mount-bottom-right-inside-floor thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside-floor [0 -2 0])
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
                                                                      (partial tps-65-translate-and-place-with-radius [(- tps-65-corner-radius (/ tps-65-mount-width 2))
                                                                                                                       (/ (- (/ tps-65-mount-length 2) tps-65-corner-radius) 2)
                                                                                                                       0] (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset)
                                                                      (mapv + oled-translation-vector [(/ oled-post-size -2) (/ oled-post-size -2) (/ oled-holder-thickness 2)]))
                                                                     steps)
        thumb-side-trackpad-to-EVQWGD001-linear-control-points-right (bezier-linear
                                                                      (transform-position
                                                                       (partial tps-65-translate-and-place-with-radius [(- tps-65-corner-radius (/ tps-65-mount-width 2))
                                                                                                                        (/ (- (/ tps-65-mount-length 2) tps-65-corner-radius) 2)
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
        EVQWGD001-mount-bottom-right-outside-to-thumb-bthumb-bl-to-tps-65-bottom-leftside (bezier-quadratic  EVQWGD001-mount-bottom-right-outside (calculate-point-between-points EVQWGD001-mount-bottom-right-outside thumb-bl-tthumb-bl-to-tps-65-bottom-leftside [0 1 0]) thumb-bl-tthumb-bl-to-tps-65-bottom-leftside  steps)
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


        ;;          EVQWGD001-mount-bottom-right-outside-to-thumb-bl-tthumb-bl-to-tps-65-bottom-leftside-points (bezier-quadratic EVQWGD001-mount-bottom-right-outside )
;;  EVQWGD001-mount-bottom-right-inside-to-thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside-points (bezier-quadratic)
;;  EVQWGD001-mount-bottom-right-outside-floor-to-thumb-bl-tthumb-bl-to-tps-65-bottom-leftside-floor-points (bezier-quadratic)
;;  EVQWGD001-mount-bottom-right-inside-floor-to-thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside-floor-points (bezier-quadratic)
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
                                                                           EVQWGD001-mount-bottom-right-outside thumb-bl-tthumb-bl-to-tps-65-bottom-leftside
                                                                           EVQWGD001-mount-bottom-right-outside-floor thumb-bl-tthumb-bl-to-tps-65-bottom-leftside-floor
                                                                           EVQWGD001-mount-bottom-right-inside thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside
                                                                           EVQWGD001-mount-bottom-right-inside-floor thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside-floor
                                                                           steps
                                                                           {:outside-upper-control-point-vector [0 1 0] :outside-lower-control-point-vector [0 1 0]
                                                                            :inside-upper-control-point-vector [0 1 0] :inside-lower-control-point-vector [0 1 0]})
        EVQWGD001-mount-bottom-right-to-thumb-bl-polyhedron (generate-polyhedron-from-points

                                                             thumb-bl-tl-curve
                                                             EVQWGD001-mount-bottom-right-outside-to-thumb-bthumb-bl-to-tps-65-bottom-leftside

                                                             (bezier-linear
                                                              thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside
                                                              thumb-bl-tl-web-post-bottom
                                                              steps)
                                                             (bezier-quadratic thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside (calculate-point-between-points EVQWGD001-mount-bottom-right-inside thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside  [0 1 0])   EVQWGD001-mount-bottom-right-inside steps)



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

(defn left-section-front-polyhedron [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [;; screen-holder-top-right-outside-point (transform-position
        ;;                                        (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
        ;;                                        (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
        ;; screen-holder-top-right-inside-point (transform-position
        ;;                                       (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
        ;;                                       (mapv +  [(/ post-size -2) (/ post-size -2) (/ oled-holder-thickness 2)]))

        screen-holder-top-right-inside-point-wide (transform-position
                                                   (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                                   (mapv + [(/ post-size -2) (/ post-size -2) 0] [(/ oled-holder-thickness 2) 0 (/ oled-holder-thickness 2)]))
        screen-holder-bottom-right-inside-point-wide (transform-position
                                                      (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (- screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                                      (mapv + [(/ post-size -2) (/ post-size 2) 0] [(/ oled-holder-thickness 2) 0 (/ oled-holder-thickness 2)]))
        screen-holder-bottom-right-inside-point-wide-floor (assoc (vec screen-holder-bottom-right-inside-point-wide) 2 0)


        tps-65-mount-top-left-upper (transform-position
                                     (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-left-position)
                                     [(- (+ tps-65-corner-radius 0.05))  (+ tps-65-corner-radius 0.05) 0])

        tps-65-mount-top-left-upper-control-point (transform-position
                                                   (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-left-position)
                                                   [(- (+ (* tps-65-mount-corner-radius-with-offset 2) 0.05))  (+ (* tps-65-mount-corner-radius-with-offset 2) 0.05) 0])
        tps-65-mount-top-left-lower (transform-position
                                     (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-left-position)
                                     [(- (+ tps-65-corner-radius 0.05))  (+ tps-65-corner-radius 0.05) (- (/ web-thickness 2))])
        
        tps-65-mount-bottom-left-upper-catmull-rom-control-point (transform-position
                                                                  (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-bottom-left-position)
                                                                  [(* (+ tps-65-mount-corner-radius-with-offset 0.05) 8)  (- (+ tps-65-corner-radius 0.05)) -200])
        tps-65-mount-bottom-left-upper-control-pont (transform-position
                                                     (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-bottom-left-position)
                                                     [(- (+ (* tps-65-mount-corner-radius-with-offset 2) 0.05))  (- (+ (* tps-65-mount-corner-radius-with-offset 2) 0.05)) 0])
        
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
        EVQWGD001-mount-bottom-left-outside-floor (translate-to-floor EVQWGD001-mount-bottom-left-outside)
        EVQWGD001-mount-bottom-left-inside (transform-position
                                            (partial EVQWGD001-translate-and-place-at-position EVQWGD001-mount-bottom-left)
                                            (mapv + [(/ oled-post-size -1) (/ oled-post-size -1) (- (/ EVQWGD001-mount-y-modifier 1))] [0 0 (+  (/ EVQWGD001-mount-height 4))]))
        EVQWGD001-mount-bottom-left-inside-floor (translate-to-floor EVQWGD001-mount-bottom-left-inside)
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
        thumb-bl-tl (wall-brace-polyhedron-circular-curve-points thumb-bl-place -1 0 "tl" :degrees steps)
        thumb-bl-tthumb-bl-to-tps-65-bottom-leftside (thumb-bl-tl :outer-points)
        thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside (thumb-bl-tl :inner-points)
        thumb-bl-tl-wall-locate3-floor (last thumb-bl-tthumb-bl-to-tps-65-bottom-leftside)
        thumb-bl-tthumb-bl-to-tps-65-bottom-leftside-floor  ((wall-brace-polyhedron-circular-points thumb-bl-place -1 0 "tl" :degrees steps) :circular-point-floor)
        thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside-floor (assoc (vec thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside) 2 0)
        thumb-bl-tl-web-post-top (transform-position (partial thumb-bl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-bl-tl-web-post-bottom (transform-position (partial thumb-bl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        thumb-bl-tr-web-post-top (transform-position (partial thumb-bl-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-bl-tr-web-post-bottom (transform-position (partial thumb-bl-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        thumb-tl-tl-web-post-top (transform-position (partial thumb-tl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-tl-tl-web-post-bottom (transform-position (partial thumb-tl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        tps-65-mount-top-left-upper-with-same-x-as-EVQWGD001-mount-top-left-outside (find-point-on-line-using-x tps-65-mount-top-left-upper tps-65-bottom-left-outer (nth EVQWGD001-mount-top-left-outside 0))
        tps-65-mount-top-left-lower-with-same-x-as-EVQWGD001-mount-top-left-inside (find-point-on-line-using-x tps-65-mount-top-left-lower tps-65-bottom-left-inner (nth EVQWGD001-mount-top-left-inside 0))
        tps-65-mount-upper-with-same-x-as-EVQWGD001-mount-top-right-outside (find-point-on-line-using-x tps-65-mount-top-left-upper tps-65-bottom-left-outer (nth EVQWGD001-mount-top-right-outside 0))
        tps-65-mount-lower-with-same-x-as-EVQWGD001-mount-top-right-inside (find-point-on-line-using-x tps-65-mount-top-left-lower tps-65-bottom-left-inner (nth EVQWGD001-mount-top-right-inside 0))
        tps-65-to-EVQWGD001-mount-top-left-outside-control-point (calculate-point-between-points tps-65-mount-top-left-upper-with-same-x-as-EVQWGD001-mount-top-left-outside EVQWGD001-mount-top-left-outside [0 -4 2])
        tps-65-to-EVQWGD001-mount-top-left-outside (bezier-quadratic EVQWGD001-mount-top-left-outside  tps-65-to-EVQWGD001-mount-top-left-outside-control-point tps-65-mount-top-left-upper-with-same-x-as-EVQWGD001-mount-top-left-outside steps)
        tps-65-to-EVQWGD001-mount-top-right-outside (bezier-quadratic  EVQWGD001-mount-top-right-outside (calculate-point-between-points tps-65-bottom-left-outer EVQWGD001-mount-top-right-outside [0 -4 2]) tps-65-bottom-left-outer  steps)
        tps-65-to-EVQWGD001-mount-top-left-inside-control-point (calculate-point-between-points tps-65-mount-top-left-lower-with-same-x-as-EVQWGD001-mount-top-left-inside EVQWGD001-mount-top-left-inside [0 -4 2])
        tps-65-to-EVQWGD001-mount-top-left-inside (bezier-quadratic tps-65-mount-top-left-lower-with-same-x-as-EVQWGD001-mount-top-left-inside tps-65-to-EVQWGD001-mount-top-left-inside-control-point EVQWGD001-mount-top-left-inside steps)
        tps-65-to-EVQWGD001-mount-top-right-inside (bezier-quadratic  tps-65-bottom-left-inner (calculate-point-between-points tps-65-bottom-left-inner EVQWGD001-mount-top-right-inside [0 -4 2]) EVQWGD001-mount-top-right-inside  steps)
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
        tps-65-mount-position-with-same-x-as-screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point (find-point-on-line-using-x tps-65-mount-top-left-upper  tps-65-bottom-left-outer  (nth screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point 0))
        tps-65-to-EVQWGD001-mount-top-left-outside-control-point-with-same-x-as-screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point (assoc tps-65-to-EVQWGD001-mount-top-left-outside-control-point 0 (nth screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point 0))
        screen-holder-outer-position-with-same-y-as-tps-65-to-EVQWGD001-mount-top-left-outside-control-point (find-point-on-line-using-y screen-holder-top-right-outside-point screen-holder-outside-point-with-same-y-as-tps-65-top-left-upper (nth tps-65-to-EVQWGD001-mount-top-left-outside-control-point 1))

        tps-65-mount-position-with-same-x-as-screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-control-point (find-point-on-line-using-x tps-65-mount-top-left-lower  tps-65-bottom-left-inner  (nth screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-control-point 0))
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
        tps-65-mount-top-left-to-bottom-left-web-post-top (bezier-linear tps-65-mount-top-left-upper tps-65-bottom-left-outer (* steps 3))
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
                 (reverse thumb-bl-tthumb-bl-to-tps-65-bottom-leftside))
        screen-holder-outside-right-with-same-z-as-EVQWGD001-top-left-outside (find-point-on-line-using-z screen-holder-top-right-outside-point screen-holder-bottom-right-outside-point (nth EVQWGD001-mount-top-left-outside 2))
        screen-holder-inside-right-wide-with-same-z-as-EVQWGD001-top-left-outside (find-point-on-line-using-z screen-holder-top-right-inside-point-wide screen-holder-bottom-right-inside-point-wide (nth EVQWGD001-mount-top-left-outside 2))
        screen-holder-inside-right-with-same-z-as-EVQWGD001-top-left-inside (find-point-on-line-using-z screen-holder-top-right-inside-point screen-holder-bottom-right-inside-point (nth EVQWGD001-mount-top-left-inside 2))
        screen-holder-inside-right-wide-with-same-z-as-EVQWGD001-bottom-left-outside (find-point-on-line-using-z screen-holder-top-right-inside-point-wide screen-holder-bottom-right-inside-point-wide (nth EVQWGD001-mount-bottom-left-outside 2))
        screen-holder-inside-right-wide-with-same-z-as-thumb-bl-tl-top (find-point-on-line-using-z screen-holder-top-right-inside-point-wide screen-holder-bottom-right-inside-point-wide (nth thumb-bl-tl-web-post-top 2))

        screen-holder-outside-right-with-same-z-as-EVQWGD001-mount-bottom-left-outside (find-point-on-line-using-z screen-holder-top-right-outside-point screen-holder-bottom-right-outside-point (nth EVQWGD001-mount-bottom-left-outside 2))
        screen-holder-inside-right-with-same-z-as-EVQWGD001-mount-bottom-left-outside (find-point-on-line-using-z screen-holder-top-right-inside-point screen-holder-bottom-right-inside-point (nth EVQWGD001-mount-bottom-left-outside 2))
        screen-holder-inside-right-with-same-z-as-EVQWGD001-mount-bottom-left-inside (find-point-on-line-using-z screen-holder-top-right-inside-point screen-holder-bottom-right-inside-point (nth EVQWGD001-mount-bottom-left-inside 2))
        screen-holder-top-right-inside-point--to-EVQWGD001-mount-top-left-outside-to-EVQWGD001-mount-top-right-outside-to-thumb-bl-tr-web-post-top
        (bezier-cubic-through-control-points screen-holder-top-right-inside-point EVQWGD001-mount-top-left-outside EVQWGD001-mount-top-right-outside thumb-bl-tr-web-post-top (* steps 3) :t1 0.25 :t2 0.75)
        screen-holder-inside-right-with-same-z-as-EVQWGD001-mount-bottom-left-outside-to-EVQWGD001-mount-bottom-left-outside-to-EVQWGD001-mount-bottom-right-outside-to-thumb-bl-tl-web-post-top
        (bezier-cubic-through-control-points screen-holder-inside-right-with-same-z-as-EVQWGD001-mount-bottom-left-outside EVQWGD001-mount-bottom-left-outside  EVQWGD001-mount-bottom-right-outside thumb-bl-tl-web-post-top (* steps 3) :t1 0.25 :t2 0.75)
        screen-holder-bottom-right-inside-floor-point-to-thumb-bl-tthumb-bl-to-tps-65-bottom-leftside-floor (bezier-linear screen-holder-bottom-right-inside-floor-point thumb-bl-tthumb-bl-to-tps-65-bottom-leftside-floor (* steps 3))
        a1 (catmull-rom-spline-curve screen-holder-top-right-outside-point screen-holder-top-right-inside-point EVQWGD001-mount-top-left-outside EVQWGD001-mount-top-right-outside steps :t1 0.6 :t2 0.9 :alphaType :chordal)
        a2 (catmull-rom-spline-curve screen-holder-top-right-inside-point EVQWGD001-mount-top-left-outside EVQWGD001-mount-top-right-outside thumb-bl-tr-web-post-top steps :t1 0.5 :t2 0.7)
        a3  (catmull-rom-spline-curve EVQWGD001-mount-top-left-outside EVQWGD001-mount-top-right-outside thumb-bl-tr-web-post-top thumb-tl-tl-web-post-top steps :t1 0.5 :t2 0.7 :alphaType :chordal)
        b1 (catmull-rom-spline-curve screen-holder-outside-right-with-same-z-as-EVQWGD001-mount-bottom-left-outside screen-holder-inside-right-with-same-z-as-EVQWGD001-mount-bottom-left-outside EVQWGD001-mount-bottom-left-outside  EVQWGD001-mount-bottom-right-outside steps :t1 0.5 :t2 0.75)
        b2 (catmull-rom-spline-curve screen-holder-inside-right-with-same-z-as-EVQWGD001-mount-bottom-left-outside EVQWGD001-mount-bottom-left-outside  EVQWGD001-mount-bottom-right-outside thumb-bl-tl-web-post-top steps :t1 0.5 :t2 0.75)
        b3 (catmull-rom-spline-curve  EVQWGD001-mount-bottom-left-outside  EVQWGD001-mount-bottom-right-outside thumb-bl-tl-web-post-top thumb-bl-tr-web-post-top steps :t1 0.5 :t2 0.75)
        a (concat (drop-last a1) (drop-last a2) a3)
        b (concat (drop-last b1) (drop-last b2) b3)
        tps-65-mount-top-left-upper-catmull-rom-control-point (transform-position
                                                               (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-left-position)
                                                               [(+ (+ tps-65-mount-corner-radius-with-offset 0.05))  (+ tps-65-corner-radius 0.05) 0])
        tps-65-mount-top-left-lower-catmull-rom-control-point (transform-position
                                                               (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-left-position)
                                                               [(+ (+ tps-65-mount-corner-radius-with-offset 0.05))  (+ tps-65-corner-radius 0.05) (/ web-thickness -2)])
        tps-65-mount-top-bottom-upper-catmull-rom-control-point (transform-position
                                                                 (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-bottom-left-position)
                                                                 [(+ (+ tps-65-mount-corner-radius-with-offset 0.05))  (- (+ tps-65-corner-radius 0.05)) 0])
        screen-bottom-to-top-inside-wide (bezier-linear screen-holder-bottom-right-inside-point-wide-floor screen-holder-top-right-inside-point-wide (* steps 2))
        mid2-ctrl (mapv + [0 (+ (* 4 tps-65-mount-corner-radius-with-offset)) -200] tps-65-mount-upper-with-same-x-as-EVQWGD001-mount-top-right-outside)
        mid2-ctrthumb-bl-tr-to-tps-65-bottom-left-innerside (mapv + [0 (+ (* 4 tps-65-mount-corner-radius-with-offset)) -200] tps-65-mount-lower-with-same-x-as-EVQWGD001-mount-top-right-inside)
        tps-65-top-left-to-screen-holder-top-right-inside-wide-catmull (catmull-rom-spline-curve screen-holder-inside-right-wide-with-same-z-as-EVQWGD001-top-left-outside screen-holder-top-right-inside-point-wide tps-65-mount-top-left-upper tps-65-mount-top-left-upper-catmull-rom-control-point steps :t1 0.6 :t2 0.7)
        EVQWGD001-mount-left-parallel-floor (find-point-on-line-using-z EVQWGD001-mount-bottom-left-outside EVQWGD001-mount-top-left-outside 0)
        EVQWGD001-mount-right-parallel-floor (find-point-on-line-using-z EVQWGD001-mount-bottom-right-outside EVQWGD001-mount-top-right-outside 0)
        EVQWGD001-mount-floor-to-bottom-left-catmull-rom (catmull-rom-spline-curve (mapv + [0 0 -2] EVQWGD001-mount-bottom-left-outside-floor) EVQWGD001-mount-bottom-left-outside-floor EVQWGD001-mount-bottom-left-outside EVQWGD001-mount-top-left-outside steps :t1 0.8 :t2 0.9)
        EVQWGD001-mount-bottom-left-top-left-catmull-rom (catmull-rom-spline-curve  EVQWGD001-mount-bottom-left-outside-floor EVQWGD001-mount-bottom-left-outside EVQWGD001-mount-top-left-outside tps-65-mount-top-left-upper-with-same-x-as-EVQWGD001-mount-top-left-outside steps :t1 0.5 :t2 0.5 :alphaType :chordal)
        EVQWGD001-mount-top-left-outside-catmull-rom (catmull-rom-spline-curve EVQWGD001-mount-bottom-left-outside EVQWGD001-mount-top-left-outside tps-65-mount-top-left-upper-with-same-x-as-EVQWGD001-mount-top-left-outside (mapv + [0 (+ (* 1 tps-65-mount-corner-radius-with-offset)) 0] tps-65-mount-top-left-upper-with-same-x-as-EVQWGD001-mount-top-left-outside) steps :t1 0.6 :t2 0.7 :alphaType :centripetal)
        EVQWGD001-mount-floor-to-bottom-right-catmull-rom (catmull-rom-spline-curve (mapv + [0 0 -2] EVQWGD001-mount-bottom-right-outside-floor) EVQWGD001-mount-bottom-right-outside-floor EVQWGD001-mount-bottom-right-outside EVQWGD001-mount-top-right-outside steps :t1 0.4 :t2 0.5 :alphaType :centripetal)
        EVQWGD001-mount-bottom-to-top-right-catmull-rom (catmull-rom-spline-curve EVQWGD001-mount-bottom-right-outside-floor EVQWGD001-mount-bottom-right-outside EVQWGD001-mount-top-right-outside tps-65-mount-upper-with-same-x-as-EVQWGD001-mount-top-right-outside steps :t1 0.5 :t2 0.5 :alphaType :chordal)
        tps-65-mount-bottom-left-to-EVQWGD001-catmull-rom (catmull-rom-spline-curve EVQWGD001-mount-bottom-right-outside EVQWGD001-mount-top-right-outside  tps-65-mount-upper-with-same-x-as-EVQWGD001-mount-top-right-outside mid2-ctrl steps :t1 0.7 :t2 0.8)
        screen-bottom-to-top-inside-wide-to-tps-65-top-left-upper (concat (drop-last screen-bottom-to-top-inside-wide) tps-65-top-left-to-screen-holder-top-right-inside-wide-catmull)
        mid1 (concat (drop-last EVQWGD001-mount-floor-to-bottom-left-catmull-rom)
                     (drop-last EVQWGD001-mount-bottom-left-top-left-catmull-rom)
                     EVQWGD001-mount-top-left-outside-catmull-rom)
        mid2 (concat (drop-last EVQWGD001-mount-floor-to-bottom-right-catmull-rom)
                     (drop-last EVQWGD001-mount-bottom-to-top-right-catmull-rom)
                     tps-65-mount-bottom-left-to-EVQWGD001-catmull-rom)
        thumb-bl-tl-floor-to-thumb-tl-tl (concat (drop-last (reverse ((wall-brace-polyhedron-circular-curve-points thumb-bl-place -1 0 "tl" :degrees (* steps 1)) :outer-points)))
                                                 (drop-last (bezier-linear thumb-bl-tl-web-post-top thumb-bl-tr-web-post-top  (* steps 1)))
                                                 ;(drop-last (bezier-linear  thumb-bl-tr-web-post-top thumb-tl-tl-web-post-top (* steps 3 (/ 2 16))))
                                                 (catmull-rom-spline-curve  thumb-bl-tl-web-post-top thumb-bl-tr-web-post-top tps-65-bottom-left-outer tps-65-mount-top-right-upper (* steps 1) :t1 0.8 :t2 0.9)
                                                 ;(bezier-linear  thumb-tl-tl-web-post-top tps-65-mount-bottom-left-upper (* steps 3 (/  4 16)))
                                                 )

        screen-bottom-to-top-inside (reverse (bezier-linear  screen-holder-bottom-right-inside-floor-point screen-holder-top-right-inside-point (* steps 2)))
        tps-65-top-left-to-screen-holder-top-right-inside-catmull (reverse (catmull-rom-spline-curve screen-holder-inside-right-with-same-z-as-EVQWGD001-top-left-inside screen-holder-top-right-inside-point tps-65-mount-top-left-lower tps-65-mount-top-left-lower-catmull-rom-control-point steps :t1 0.6 :t2 0.7))
        EVQWGD001-mount-floor-to-bottom-left-inside-catmull-rom (reverse (catmull-rom-spline-curve (mapv + [0 0 -2] EVQWGD001-mount-bottom-left-inside-floor) EVQWGD001-mount-bottom-left-inside-floor EVQWGD001-mount-bottom-left-inside EVQWGD001-mount-top-left-inside steps :t1 0.8 :t2 0.9))
        EVQWGD001-mount-bottom-left-top-left-inside-catmull-rom (reverse (catmull-rom-spline-curve  EVQWGD001-mount-bottom-left-inside-floor EVQWGD001-mount-bottom-left-inside EVQWGD001-mount-top-left-inside tps-65-mount-top-left-lower-with-same-x-as-EVQWGD001-mount-top-left-inside steps :t1 0.5 :t2 0.5 :alphaType :chordal))
        EVQWGD001-mount-top-left-inside-catmull-rom (reverse (catmull-rom-spline-curve EVQWGD001-mount-bottom-left-inside EVQWGD001-mount-top-left-inside tps-65-mount-top-left-lower-with-same-x-as-EVQWGD001-mount-top-left-inside (mapv + [0 (+ (* 1 tps-65-mount-corner-radius-with-offset)) 0] tps-65-mount-top-left-lower-with-same-x-as-EVQWGD001-mount-top-left-inside) steps :t1 0.6 :t2 0.7 :alphaType :centripetal))
        EVQWGD001-mount-floor-to-bottom-right-inside-catmull-rom (reverse (catmull-rom-spline-curve (mapv + [0 0 -2] EVQWGD001-mount-bottom-right-inside-floor) EVQWGD001-mount-bottom-right-inside-floor EVQWGD001-mount-bottom-right-inside EVQWGD001-mount-top-right-inside steps :t1 0.4 :t2 0.5 :alphaType :centripetal))
        EVQWGD001-mount-bottom-to-top-right-inside-catmull-rom (reverse (catmull-rom-spline-curve EVQWGD001-mount-bottom-right-inside-floor EVQWGD001-mount-bottom-right-inside EVQWGD001-mount-top-right-inside tps-65-mount-lower-with-same-x-as-EVQWGD001-mount-top-right-inside steps :t1 0.5 :t2 0.5 :alphaType :chordal))
        tps-65-mount-bottom-left-to-EVQWGD001-inside-catmull-rom (reverse (catmull-rom-spline-curve EVQWGD001-mount-bottom-right-inside EVQWGD001-mount-top-right-inside   tps-65-mount-lower-with-same-x-as-EVQWGD001-mount-top-right-inside mid2-ctrthumb-bl-tr-to-tps-65-bottom-left-innerside steps :t1 0.7 :t2 0.8))
        screen-bottom-to-top-inside-to-tps-65-top-left-upper (concat (drop-last tps-65-top-left-to-screen-holder-top-right-inside-catmull)  screen-bottom-to-top-inside)
        mid1-inside (concat (drop-last EVQWGD001-mount-top-left-inside-catmull-rom)
                            (drop-last EVQWGD001-mount-bottom-left-top-left-inside-catmull-rom)
                            EVQWGD001-mount-floor-to-bottom-left-inside-catmull-rom)
        mid2-inside (concat (drop-last tps-65-mount-bottom-left-to-EVQWGD001-inside-catmull-rom)
                            (drop-last EVQWGD001-mount-bottom-to-top-right-inside-catmull-rom)
                            EVQWGD001-mount-floor-to-bottom-right-inside-catmull-rom)

        thumb-bl-tl-floor-to-thumb-tl-tl-bottom (concat (drop-last (reverse (catmull-rom-spline-curve  thumb-bl-tl-web-post-bottom thumb-bl-tr-web-post-bottom tps-65-bottom-left-inner tps-65-mount-top-right-lower (* steps 1) :t1 0.8 :t2 0.9)))
                                                        (drop-last (reverse (bezier-linear thumb-bl-tl-web-post-bottom thumb-bl-tr-web-post-bottom  (* steps 1))))
                                                 ;(drop-last (bezier-linear  thumb-bl-tr-web-post-top thumb-tl-tl-web-post-top (* steps 3 (/ 2 16))))

                                                        ((wall-brace-polyhedron-circular-curve-points thumb-bl-place -1 0 "tl" :degrees (* steps 1)) :inner-points)
                                                        ;(bezier-linear  thumb-tl-tl-web-post-top tps-65-mount-bottom-left-upper (* steps 3 (/  4 16)))
                                                        )
        screen-holder-inside-right-wide-with-same-z-as-first-outside-curve-border (find-point-on-line-using-z screen-holder-top-right-inside-point-wide screen-holder-bottom-right-inside-point-wide (nth (nth screen-bottom-to-top-inside-wide steps) 2))
        screen-holder-inside-right-with-same-z-as-first-inside-curve-border (find-point-on-line-using-z screen-holder-top-right-inside-point screen-holder-bottom-right-inside-point (nth (nth screen-bottom-to-top-inside steps) 2))
        screen-holder-outside-right-with-same-z-as-first-outside-curve-border (find-point-on-line-using-z screen-holder-top-right-outside-point screen-holder-bottom-right-outside-point (nth (nth screen-bottom-to-top-inside-wide steps) 2))

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


        outer-points-fn (fn [index]
                          (bezier-cubic-through-control-points
                           (nth screen-bottom-to-top-inside-wide-to-tps-65-top-left-upper index) ;(nth tps-65-mount-top-left-to-bottom-left-web-post-top index)
                          ;(nth tps-65-mount-top-left-to-bottom-left-web-post-top-control-points index)
                           (nth mid1 index);(nth screen-holder-top-right-inside-point--to-EVQWGD001-mount-top-left-outside-to-EVQWGD001-mount-top-right-outside-to-thumb-bl-tr-web-post-top index)
                           (nth mid2 index) ;(nth screen-holder-inside-right-with-same-z-as-EVQWGD001-mount-bottom-left-outside-to-EVQWGD001-mount-bottom-left-outside-to-EVQWGD001-mount-bottom-right-outside-to-thumb-bl-tl-web-post-top index)
                           (nth thumb-bl-tl-floor-to-thumb-tl-tl index) ;(nth screen-holder-bottom-right-inside-floor-point-to-thumb-bl-tthumb-bl-to-tps-65-bottom-leftside-floor index)
                           (* steps 3)
                           :t1 0.2
                           :t2 0.7))

        inner-points-fn (fn [index]
                          (bezier-cubic-through-control-points
                           (nth screen-bottom-to-top-inside-to-tps-65-top-left-upper index)
                           (nth mid1-inside index)
                           (nth mid2-inside index)
                          ;(nth screen-holder-bottom-right-inside-floor-point-to-thumb-bl-tthumb-bl-to-tps-65-bottom-leftside-floor index)
                           (nth thumb-bl-tl-floor-to-thumb-tl-tl-bottom index)
                           (* steps 3)
                           :t1 0.2
                           :t2 0.7))
        outer-faces (into [] (apply concat
                                    ;outer-faces-start
                                    (for [index (range 0 (inc (* steps 3)))]
                                      (outer-faces-nth index))))
        outer-points (into [] (apply concat
                                    ;outer-faces-start
                                     (for [index (range 0 (inc (* steps 3)))]
                                       (outer-points-fn index))))
        inner-points (into [] (apply concat
                                    ;outer-faces-start
                                     (for [index (range 0 (inc (* steps 3)))]
                                       (inner-points-fn index))))
        screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point-translated-z-pos (mapv + screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point [0 0 (* oled-holder-thickness 2)])
        top-left-corner-surface-outer-points-bottom #(bezier-quadratic screen-holder-top-right-outside-point  (mapv + screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point [0 0 (* oled-holder-thickness 1)]) EVQWGD001-mount-top-left-outside %)
        top-left-corner-surface-outer-points (into [] (apply concat (for [index (range 0 (inc (* steps 2)))
                                                                          :let [top-points (concat (drop-last screen-holder-pos-with-same-y-as-tps-65-left-top-curve)
                                                                                                   (bezier-quadratic tps-65-mount-top-left-upper tps-65-mount-position-with-same-x-as-screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point tps-65-mount-top-left-upper-with-same-x-as-EVQWGD001-mount-top-left-outside steps))
                                                                                control-points (bezier-quadratic screen-holder-outer-position-with-same-y-as-tps-65-to-EVQWGD001-mount-top-left-outside-control-point
                                                                                                                 (mapv + tps-65-to-EVQWGD001-mount-top-left-outside-control-point-with-same-x-as-screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point [0 0 0])
                                                                                                                 tps-65-to-EVQWGD001-mount-top-left-outside-control-point (* steps 2))
                                                                                control-points-2 (bezier-quadratic screen-holder-top-right-outside-point screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside-control-point-translated-z-pos EVQWGD001-mount-top-left-outside (* steps 2))
                                                                                bottom-points (top-left-corner-surface-outer-points-bottom (* steps 2))]]
                                                                      (bezier-quadratic
                                                                       (nth top-points index)
                                                                       (nth control-points index)
                                                                       ;(nth control-points-2 index)
                                                                       (nth bottom-points index)
                                                                       (* steps 2)))))
        top-left-corner-surface-inner-points (into [] (apply concat (for [index (range 0 (inc (* steps 2)))
                                                                          :let [top-points (bezier-quadratic tps-65-mount-top-left-lower-with-same-x-as-EVQWGD001-mount-top-left-inside
                                                                                                             tps-65-top-left-inner-with-same-z-as-screen-holder-top-right-inside-point-translated-with-same-y-as-tps-65-top-left-inner
                                                                                                             screen-holder-inside-point-with-same-y-as-tps-65-top-left-inner
                                                                                                             (* steps 2))
                                                                                control-points (bezier-quadratic  tps-65-to-EVQWGD001-mount-top-left-inside-control-point
                                                                                                                  tps-65-to-EVQWGD001-mount-top-left-inside-control-point-with-same-x-as-screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-control-point
                                                                                                                  screen-holder-inner-position-with-same-y-as-tps-65-to-EVQWGD001-mount-top-left-inside-control-point
                                                                                                                  (* steps 2))
                                                                                lower-points (bezier-quadratic  EVQWGD001-mount-top-left-inside screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside-control-point screen-holder-top-right-inside-point  (* steps 2))]]
                                                                      (bezier-quadratic (nth top-points index) (nth control-points index) (nth lower-points index) (* steps 2)))))
        below-top-left-corner-surface-outer-points (into [] (apply concat (for [index (range 0 (inc steps))
                                                                                :let [top-points (top-left-corner-surface-outer-points-bottom steps)
                                                                                      bottom-points screen-holder-top-right-outside-to-EVQWGD001-mount-top-left-outside]]
                                                                            (bezier-linear (nth top-points index) (nth bottom-points index) steps))))
        below-top-left-corner-surface-inner-points (into [] (apply concat (for [index (range 0 (inc steps))
                                                                                :let [top-points screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside
                                                                                      bottom-points screen-holder-top-right-inside-to-EVQWGD001-mount-top-left-inside]]
                                                                            (bezier-linear
                                                                             (nth top-points index)
                                                                             (nth bottom-points index)
                                                                             steps))))
        start-points (bezier-cubic
                      tps-65-top-left-outer
                      tps-65-top-left-control-point-outer
                      screen-holder-top-right-inside-point-translated-with-same-y-as-tps-65-top-left-inner
                      screen-holder-outside-point-with-same-y-as-tps-65-top-left-upper
                      steps)
        end-points (bezier-quadratic screen-holder-top-right-inside-point-wide
                                     (calculate-point-between-points screen-holder-top-right-outside-point screen-holder-top-right-inside-point-wide [0 -1 0])
                                     screen-holder-top-right-outside-point
                                     steps)
        catmull-lower-control-points (bezier-quadratic screen-holder-inside-right-wide-with-same-z-as-EVQWGD001-top-left-outside
                                                       (calculate-point-between-points screen-holder-outside-right-with-same-z-as-EVQWGD001-top-left-outside screen-holder-inside-right-wide-with-same-z-as-EVQWGD001-top-left-outside [0 -1 0])
                                                       screen-holder-outside-right-with-same-z-as-EVQWGD001-top-left-outside
                                                       steps)
        mid-1-initial  (nth start-points (/ steps 3))
        mid-2-initial  (nth start-points (* 2 (/ steps 3)))
        mid-curve-1 (catmull-rom-spline-curve  (mapv - tps-65-mount-top-left-upper-catmull-rom-control-point (mapv - tps-65-top-left-outer mid-1-initial)) mid-1-initial  (nth end-points (/ steps 3))  (nth catmull-lower-control-points (/ steps 3)) steps :t1 0.6 :t2 0.7)
        mid-curve-2 (catmull-rom-spline-curve (mapv - tps-65-mount-top-left-upper-catmull-rom-control-point (mapv - tps-65-top-left-outer mid-2-initial)) mid-2-initial  (nth end-points (* 2 (/ steps 3)))  (nth catmull-lower-control-points (* 2 (/ steps 3)))  steps :t1 0.6 :t2 0.7)
        end-curve (bezier-linear  screen-holder-outside-point-with-same-y-as-tps-65-top-left-upper screen-holder-top-right-outside-point steps)

        top-left-corner-outer-points (into [] (apply concat (for [index (range 0 (inc steps))
                                                                  :let [top (reverse tps-65-top-left-to-screen-holder-top-right-inside-wide-catmull)]]
                                                              (bezier-cubic-through-control-points
                                                               (nth  top index)
                                                               (nth mid-curve-1 index)
                                                               (nth mid-curve-2 index)
                                                               (nth end-curve index)
                                                               steps))))

        top-left-corner-inner-points (into [] (apply concat (for [index (range 0 (inc steps))
                                                                  :let [start (reverse tps-65-top-left-to-screen-holder-top-right-inside-catmull)
                                                                        end (bezier-linear  screen-holder-top-right-outside-point screen-holder-outside-point-with-same-y-as-tps-65-top-left-upper steps)]]
                                                              (bezier-linear
                                                               (nth start index)
                                                               (nth end index)

                                                               steps))))

        top-left-corner-surface-polyhedron (generate-bezier-along-bezier-polyhedron top-left-corner-surface-outer-points top-left-corner-surface-inner-points (* steps 2))
        below-top-left-corner-surface-polyhedron (generate-bezier-along-bezier-polyhedron below-top-left-corner-surface-outer-points below-top-left-corner-surface-inner-points steps)
        EVQWGD001-mount-bottom-right-outside-floor-thumb-bl-tl-wall-locate3-floor (bezier-linear EVQWGD001-mount-bottom-right-outside-floor thumb-bl-tl-wall-locate3-floor steps)
        screen-holder-bottom-right-outside-floor-point-to-EVQWGD001-mount-bottom-right-outside-floor (bezier-linear screen-holder-bottom-right-outside-floor-point EVQWGD001-mount-bottom-right-outside-floor  steps)
        bottom-points (concat (drop-last (bezier-quadratic screen-holder-bottom-right-outside-floor-point
                                                           (calculate-point-between-points screen-holder-bottom-right-outside-floor-point screen-holder-bottom-right-inside-point-wide-floor [0 -1 0])
                                                           screen-holder-bottom-right-inside-point-wide-floor steps))
                              (filter #(zero? (nth % 2)) outer-points))
        plotted-points (union
                        (plot-bezier-points screen-bottom-to-top-inside-wide (sphere 0.1))
                        (plot-bezier-points tps-65-top-left-to-screen-holder-top-right-inside-wide-catmull (sphere 0.1))

                        (plot-bezier-points EVQWGD001-mount-floor-to-bottom-left-catmull-rom (sphere 0.1))
                        (plot-bezier-points EVQWGD001-mount-bottom-left-top-left-catmull-rom (sphere 0.1))

                        (plot-bezier-points EVQWGD001-mount-top-left-outside-catmull-rom (sphere 0.1))
                        (plot-bezier-points EVQWGD001-mount-floor-to-bottom-right-catmull-rom (sphere 0.1))
                        (plot-bezier-points EVQWGD001-mount-bottom-to-top-right-catmull-rom (sphere 0.1))
                        (plot-bezier-points tps-65-mount-bottom-left-to-EVQWGD001-catmull-rom (sphere 0.1))
                        (plot-bezier-points thumb-bl-tl-floor-to-thumb-tl-tl (sphere 0.1))
                        (translate mid2-ctrl (sphere 1)))
        polyhedrons (union

                     (generate-bezier-along-bezier-polyhedron outer-points inner-points (* steps 3))
                     (generate-bezier-along-bezier-polyhedron top-left-corner-outer-points top-left-corner-inner-points steps)
  ;;  (polyhedron top-left-corner-inner-points 
  ;;              (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps))
                     (generate-bezier-along-bezier-polyhedron-from-points-linear
                      screen-holder-top-right-outside-point screen-holder-top-right-inside-point-wide
                      screen-holder-outside-right-with-same-z-as-first-outside-curve-border screen-holder-inside-right-wide-with-same-z-as-first-outside-curve-border
                      screen-holder-top-right-inside-point screen-holder-top-right-outside-point
                      screen-holder-inside-right-with-same-z-as-first-inside-curve-border screen-holder-outside-right-with-same-z-as-first-outside-curve-border
                      steps
                      :outside-upper-control-point-vector [0 -1 0] :outside-lower-control-point-vector [0 -1 0])
                     (generate-bezier-along-bezier-polyhedron-from-points-linear
                      screen-holder-outside-right-with-same-z-as-first-outside-curve-border screen-holder-inside-right-wide-with-same-z-as-first-outside-curve-border
                      screen-holder-bottom-right-outside-floor-point screen-holder-bottom-right-inside-point-wide-floor
                      screen-holder-inside-right-with-same-z-as-first-inside-curve-border screen-holder-outside-right-with-same-z-as-first-outside-curve-border
                      screen-holder-bottom-right-inside-floor-point screen-holder-bottom-right-outside-floor-point
                      steps
                      :outside-upper-control-point-vector [0 -1 0] :outside-lower-control-point-vector [0 -1 0]))]
    (if (true? bottom-plate)  bottom-points polyhedrons)))
(defn left-section-front-polyhedron-bottom [steps]
  (let
   [thumb-bl-tl-wall-locate3-floor (wall-brace-polyhedron-outer-floor-point thumb-bl-place -1 0 "tl" :degrees)

    EVQWGD001-mount-bottom-right-outside-floor-thumb-bl-tl-wall-locate3-floor (bezier-linear EVQWGD001-mount-bottom-right-outside-floor thumb-bl-tl-wall-locate3-floor steps)
    screen-holder-bottom-right-outside-floor-point-to-EVQWGD001-mount-bottom-right-outside-floor (bezier-linear screen-holder-bottom-right-outside-floor-point EVQWGD001-mount-bottom-right-outside-floor  steps)]
    (concat screen-holder-bottom-right-outside-floor-point-to-EVQWGD001-mount-bottom-right-outside-floor EVQWGD001-mount-bottom-right-outside-floor-thumb-bl-tl-wall-locate3-floor)))

(defn left-section-front-polyhedron-bottom-circular [steps]
  (let
   [thumb-bl-tl-wall-locate3-floor (wall-brace-polyhedron-circular-outer-floor-point thumb-bl-place -1 0 "tl" :degrees)

    EVQWGD001-mount-bottom-right-outside-floor-thumb-bl-tl-wall-locate3-floor (bezier-linear EVQWGD001-mount-bottom-right-outside-floor thumb-bl-tl-wall-locate3-floor steps)
    screen-holder-bottom-right-outside-floor-point-to-EVQWGD001-mount-bottom-right-outside-floor (bezier-linear screen-holder-bottom-right-outside-floor-point EVQWGD001-mount-bottom-right-outside-floor  steps)]
    (concat screen-holder-bottom-right-outside-floor-point-to-EVQWGD001-mount-bottom-right-outside-floor EVQWGD001-mount-bottom-right-outside-floor-thumb-bl-tl-wall-locate3-floor)))

(defn pinky-to-fourth-br-bl [steps]
  (let [pinky-to-fourth-web-post-curve
        (web-post-quadratic-curve (partial key-place lastcol cornerrow)  "bl" :radians
                                  (partial key-place 3 cornerrow) "br" :radians
                                  (partial key-place 3 cornerrow) "bl" :radians
                                  steps)
        fourth-br-top (transform-position-radians (partial key-place 3 cornerrow) (get-web-post-position-top (mapv +  (get-single-plate-corner-position-vector "br"))))
        fourth-br-bottom (transform-position-radians (partial key-place 3 cornerrow) (get-web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "br"))))
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
                                                     fourth-bl-to-br-top (fourth-bl-to-br :top)]]
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
         :offset1 [0 post-size 0]  :offset2 [post-size 0 0])

        middle-bm-to-fourth-bl-top (middle-bm-to-fourth-bl :top)
        middle-bm-to-fourth-bl-bottom (middle-bm-to-fourth-bl :bottom)
        middle-br-top (transform-position-radians (partial key-place 2 cornerrow) (get-web-post-position-top (mapv +  (get-single-plate-corner-position-vector "br"))))
        middle-br-bottom (transform-position-radians (partial key-place 2 cornerrow) (get-web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "br"))))

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
                                        steps))))]
    ;; (generate-bezier-to-point-polyhedron
    ;;  (reverse (nth middle-bm-to-fourth-bl 0))
    ;;  middle-br-top
    ;;   (nth middle-bm-to-fourth-bl 1)
    ;;  middle-br-bottom)
    (generate-bezier-to-point-polyhedron
     (reverse middle-bm-to-fourth-bl-top)  middle-br-top
     middle-bm-to-fourth-bl-bottom middle-br-bottom)))


(defn right-wall-polyhedron [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
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
        order-points (fn [list1 list2] (if (true? bottom-plate) (apply concat (interleave list1 list2) [(last list1)]) (union list1 list2)))
        group (fn [points] (if (true? bottom-plate) (apply concat points) points))]  (collect-fn
                                                                                      bottom-corner
                                                                                      (key-wall-brace-polyhedron-fn lastcol 2 1 0 "br" lastcol 2 1 0 "tr" :steps steps)
                                                                                      (key-wall-brace-polyhedron-fn lastcol 2 1 0 "tr" lastcol 1 1 0 "br" :steps steps)
                                                                                      (key-wall-brace-polyhedron-fn lastcol 1 1 0 "br" lastcol 1 1 0 "tr" :steps steps)
                                                                                      (key-wall-brace-polyhedron-fn lastcol 1 1 0 "tr" lastcol 0 1 0 "br" :steps steps)
                                                                                      (key-wall-brace-polyhedron-fn lastcol 0 1 0 "br" lastcol 0 1 0 "tr" :steps steps)
    ;(for [y (range 0 lastrow)] (key-wall-brace-polyhedron-fn lastcol y 1 0 "br" lastcol y 1 0 "tr" :steps steps))
    ;(for [y (range 1 lastrow)] (key-wall-brace-polyhedron-fn lastcol y 1 0 "tr" lastcol (dec y) 1 0 "br" :steps steps))
                                                                                      top-corner)))
(defn right-wall-polyhedron-catmull-rom-spline [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [wall-brace-quadratic-fn (get-wall-brace-quadratic-fn bottom-plate)
        wall-brace-catmull-rom-spline-fn (get-wall-brace-catmull-rom-spline-fn bottom-plate)
        point-fn (fn [row post-position] {:place (partial key-place lastcol row) :dx 1 :dy 0 :post-position post-position :rad-or-deg :radians :xy wall-xy-offset})
        collect-fn (get-collect-fn bottom-plate)
        points (concat
                [{:place (partial key-place lastcol cornerrow) :dx 1 :dy -1 :post-position "br" :rad-or-deg :radians :xy wall-xy-offset}]
                (apply concat (for [row (range 0 lastrow)]
                                [(point-fn (- cornerrow row) "br")
                                 (point-fn (- cornerrow row) "rm")
                                 (point-fn (- cornerrow row) "tr")]))
                [{:place (partial key-place lastcol 0) :dx 1 :dy 1 :post-position "tr" :rad-or-deg :radians :xy wall-xy-offset}])
        bottom-corner (wall-brace-quadratic-fn (partial key-place lastcol cornerrow) 0 -1 "br" :radians
                                               (partial key-place lastcol cornerrow) 1 -1 "br" :radians
                                               (partial key-place lastcol cornerrow)  1 0 "br" :radians
                                               steps)
        top-corner (wall-brace-quadratic-fn (partial key-place lastcol 0) 1 0 "tr" :radians
                                            (partial key-place lastcol 0) 1 1 "tr" :radians
                                            (partial key-place lastcol 0) 0 1 "tr" :radians
                                            steps)]
;    (for [point points](println point))
    (collect-fn
     bottom-corner
     (apply collect-fn (for [index (range 0 (- (count points) 3))]
                         (wall-brace-catmull-rom-spline-fn (nth points index) (nth points (inc index)) (nth points (+ index 2)) (nth points (+ index 3)) steps :alpha-type :centripetal :t1 0.8 :t2 0.9)))
     top-corner)))
(defn right-wall-polyhedron-circular [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [wall-brace-quadratic-fn (get-wall-brace-quadratic-fn bottom-plate)
        key-wall-brace-polyhedron-fn (get-key-wall-brace-polyhedron-fn bottom-plate)
        key-wall-brace-polyhedron-circular-fn (get-key-wall-brace-polyhedron-circular-fn bottom-plate)
        key-wall-brace-polyhedron-with-circular-fn (get-key-wall-brace-polyhedron-with-circular-fn bottom-plate)

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
        order-points (fn [list1 list2] (if (true? bottom-plate) (apply concat (interleave list1 list2) [(last list1)]) (union list1 list2)))
        group (fn [points] (if (true? bottom-plate) (apply concat points) points))]  (collect-fn
                                                                                      bottom-corner
                                                                                      (key-wall-brace-polyhedron-with-circular-fn lastcol 2 1 0 "br" lastcol 2 1 0 "tr" :steps steps :curve-type1 :standard :xy2 wall-xy-offset-mid)
                                                                                      (key-wall-brace-polyhedron-circular-fn lastcol 2 1 0 "tr" lastcol 1 1 0 "br" :steps steps :xy1 wall-xy-offset-mid :xy2 wall-xy-offset-mid)
                                                                                      (key-wall-brace-polyhedron-circular-fn lastcol 1 1 0 "br" lastcol 1 1 0 "tr" :steps steps :xy1 wall-xy-offset-mid :xy2 wall-xy-offset-mid)
                                                                                      (key-wall-brace-polyhedron-circular-fn lastcol 1 1 0 "tr" lastcol 0 1 0 "br" :steps steps :xy1 wall-xy-offset-mid :xy2 wall-xy-offset-mid)
                                                                                      (key-wall-brace-polyhedron-with-circular-fn lastcol 0 1 0 "br" lastcol 0 1 0 "tr" :steps steps :xy1 wall-xy-offset-mid :curve-type2 :standard)
    ;(for [y (range 0 lastrow)] (key-wall-brace-polyhedron-fn lastcol y 1 0 "br" lastcol y 1 0 "tr" :steps steps))
    ;(for [y (range 1 lastrow)] (key-wall-brace-polyhedron-fn lastcol y 1 0 "tr" lastcol (dec y) 1 0 "br" :steps steps))
                                                                                      top-corner)))
(defn back-wall-polyhedron-catmull-rom [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [wall-brace-catmull-rom-spline-fn (get-wall-brace-catmull-rom-spline-fn bottom-plate)
        collect-fn (get-collect-fn bottom-plate)
        
        points-list (apply concat
                     [(points-fn-rad (partial key-place lastcol 0) 1 1 "tr" wall-xy-offset)]
                      
                      (apply concat (for [col (range 0 ncols)] 
                         (concat
                          [(points-fn-rad (partial key-place (- lastcol col) 0) 0 1 "tr" wall-xy-offset)]
                         [(points-fn-rad (partial key-place (- lastcol col) 0) 0 1 "tl" wall-xy-offset)])
                           
                        ))
                     ;[(points-fn-rad  (partial key-place 0 0) -1 1 "tl" wall-xy-offset)]
                     ;[(points-fn-deg tps-65-bottom-right 1 1 "centre" wall-xy-offset)]
                           
                           [(points-fn-deg tps-65-bottom-right 1 1 "centre" wall-xy-offset)]
                           [[(points-fn-deg tps-65-top-right 1 0 "centre" wall-xy-offset)]]
                     )
        ] 
    ;; (for [point points-list]
    ;;    (println point))
    ;;  (for [index (range 0 (- (count points-list) 3))]
    ;;    (println
    ;;     (nth points-list index)
    ;;     (nth points-list (inc index))
    ;;     (nth points-list (+ index 2))
    ;;     (nth points-list (+ index 3))
    ;;     steps
    ;;     )
    ;;    )
     (apply collect-fn
     (for [index (range 0 (- (count points-list) 3))]
       (wall-brace-catmull-rom-spline-fn
        (nth points-list index)
        (nth points-list (inc index))
        (nth points-list (+ index 2))
        (nth points-list (+ index 3))
        steps
        )
       )
     
     )
    )
  )
(defn back-wall-polyhedron [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [key-wall-brace-polyhedron-fn (get-key-wall-brace-polyhedron-fn bottom-plate)
        wall-brace-cubic-fn (get-wall-brace-cubic-fn bottom-plate)
        wall-brace-quadratic-fn (get-wall-brace-quadratic-fn bottom-plate)
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
        back-corner     (wall-brace-quadratic-fn
                (partial key-place 0 0) 0 1 "tl" :radians
                (partial key-place 0 0) -1 1 "tl" :radians
                tps-65-bottom-right 1 1 "centre" :degrees
                steps)
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
     back-corner
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
        wall-brace-catmull-rom-spline-fn (get-wall-brace-catmull-rom-spline-fn bottom-plate)
        collect-fn (get-collect-fn bottom-plate)
        
        wall-brace-bezier-cubic-through-points-fn (get-wall-brace-bezier-cubic-through-points-fn bottom-plate)
        point-fn (fn [place dx dy post-position  rad-or-deg xy] {:place place :dx dx :dy dy :post-position post-position :rad-or-deg rad-or-deg :xy xy})]
    (collect-fn
     (wall-brace-quadratic-fn (partial key-place 1 cornerrow) 1 -0.1 "br" :radians
                              (partial key-place 2 cornerrow) 0.25 -1 "bl" :radians
                              (partial key-place 2 cornerrow) 0 -1 "bm" :radians
                              wall-xy-offset-medium-thin wall-xy-offset-thin wall-xy-offset-thin 
                              steps)

     ;;       (wall-brace-catmull-rom-spline-fn
;;       (point-fn thumb-tr-place 1 0 "tr" :degrees wall-xy-offset)
;;       (point-fn (partial key-place 1 cornerrow) 1 0 "br" :radians wall-xy-offset-thin)
;;       (point-fn (partial key-place 2 cornerrow) 1 -1 "bl" :radians wall-xy-offset-thin)
;;       (point-fn (partial key-place 2 cornerrow) 0 -1 "bm" :radians wall-xy-offset-thin)
;;       ;(point-fn (partial key-place 2 cornerrow) 0 -1 "br" :radians wall-xy-offset-thin) 
;;       steps
;;       :web-post-top-style :curved)
;;      (wall-brace-catmull-rom-spline-fn 
;;       ;(point-fn thumb-tr-place 1 0 "tr" :degrees wall-xy-offset)
;;       (point-fn (partial key-place 1 cornerrow) 1 0 "br" :radians wall-xy-offset-thin)
;; (point-fn (partial key-place 2 cornerrow) 1 -1 "bl" :radians wall-xy-offset-thin )
;; (point-fn (partial key-place 2 cornerrow) 0 -1 "bm" :radians wall-xy-offset-thin)
;;       (point-fn (partial key-place 2 cornerrow) 0 -1 "br" :radians wall-xy-offset-thin )
;;       steps
;;       :web-post-top-style :curved
;;       )
     
    ;;  (wall-brace-bezier-cubic-through-points-fn (point-fn (partial key-place 1 cornerrow) 1 0 "br" :radians wall-xy-offset-medium-thin)
    ;;                                             (point-fn (partial key-place 1 cornerrow) 1 0.25 "br" :radians wall-xy-offset-medium-thin)
    ;;                                             (point-fn (partial key-place 2 cornerrow) -0.25 -1 "bm" :radians wall-xy-offset-thin)
    ;;                                             (point-fn (partial key-place 2 cornerrow) 0 -1 "bm" :radians wall-xy-offset-thin)
    ;;                                             steps :t1 0.2 :t2 0.7
    ;;                                             )
    ;;  (wall-brace-catmull-rom-spline-fn (catmull-rom-point-fn thumb-tr-place 0 1 "tr" :degrees wall-xy-offset)
    ;;                                    (catmull-rom-point-fn (partial key-place 1 cornerrow) 1 0 "br" :radians wall-xy-offset-medium-thin)
    ;;                                    (catmull-rom-point-fn (partial key-place 2 cornerrow) 0 -1 "bm" :radians wall-xy-offset-thin)
    ;;                                    (catmull-rom-point-fn (partial key-place 2 cornerrow) 0 -1 "br" :radians wall-xy-offset-mid)
    ;;                                    steps :alpha-type :centripetal :t1 0.8 :t2 0.9)

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
     (key-wall-brace-polyhedron-fn lastcol cornerrow 0 -1 "bl" lastcol cornerrow 0 -1 "br" :steps steps))))

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
    (union
     (pinky-to-fourth-br-bl steps)
     (middle-bm-to-fourth-bl-to-middle-br steps)
     index-br-to-middle-bl-to-bm-web-post-polyhedron)))

(defn thumb-walls-polyhedron [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [thumb-wall-brace-polyhedron-fn (get-thumb-wall-brace-polyhedron-fn bottom-plate)]
    {:thumb-bl-tl-to-bl (thumb-wall-brace-polyhedron-fn thumb-bl-place -1  0 "tl" thumb-bl-place -1  0 "bl" :steps steps)
     :thumb-bl-to-br (thumb-wall-brace-polyhedron-fn thumb-bl-place -1  0 "bl" thumb-br-place -1  0 "tl" :steps steps)
     :thumb-br-tl-to-bl (thumb-wall-brace-polyhedron-fn thumb-br-place -1  0 "tl" thumb-br-place -1  0 "bl" :steps steps)
     :thumb-br-bl-to-br (thumb-wall-brace-polyhedron-fn thumb-br-place  0 -1 "bl" thumb-br-place  0 -1 "br" :steps steps)
     :thumb-br-to-mr  (thumb-wall-brace-polyhedron-fn thumb-br-place  0 -1 "br" thumb-mr-place  0 -1 "bl" :steps steps)
     :thumb-mr-bl-to-br (thumb-wall-brace-polyhedron-fn  thumb-mr-place  0 -1 "bl" thumb-mr-place  0 -1 "br" :steps steps)
     :thumb-tr-br-to-tr (thumb-wall-brace-polyhedron-fn thumb-tr-place  1 0 "br" thumb-tr-place  1 -1 "tr" :xy1 wall-xy-offset :xy2 wall-xy-offset-mid :steps steps)}))

(defn points-for-curved-wall-from-thumb-br-bl-to-mr-br [steps]
  {:place1 thumb-br-place :dx1 0 :dy1 -1  :post-position-1 "bl" :rad-or-deg1 :degrees
   :place-mid1 thumb-br-place :dxmid1 0  :dymid1 -1  :post-position-mid1 "br" :rad-or-degmid1 :degrees
   :place-mid2 thumb-mr-place :dxmid2 0 :dymid2 -1 :post-position-mid2  "bl" :rad-or-degmid2 :degrees
   :place2 thumb-mr-place :dx2 0 :dy2 -1  :post-position-2 "br" :rad-or-deg2 :degrees
   :xy1 wall-xy-offset :xymid1 wall-xy-offset :xymid2 wall-xy-offset :xy2 wall-xy-offset :steps steps}
  )

(defn thumb-walls-polyhedron-for-convex-cluster [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [thumb-wall-brace-polyhedron-fn (get-thumb-wall-brace-polyhedron-fn bottom-plate)
        wall-brace-catmull-rom-spline-fn (get-wall-brace-catmull-rom-spline-fn bottom-plate)
        wall-brace-cubic-fn (get-wall-brace-cubic-fn bottom-plate)
        point-fn (fn [place dx dy post-position  rad-or-deg xy] {:place place :dx dx :dy dy :post-position post-position :rad-or-deg rad-or-deg :xy xy})]
    {;:thumb-bl-tl-to-bl (thumb-wall-brace-polyhedron-fn thumb-bl-place -1  0 "tl" thumb-bl-place -1  0 "bl" :steps steps)
    ;:thumb-bl-to-br (thumb-wall-brace-polyhedron-fn thumb-bl-place -1  0 "bl" thumb-br-place -1  0 "tl" :steps steps)
     :thumb-bl-tl-to-lm (thumb-wall-brace-polyhedron-fn thumb-bl-place -1  0 "tl" thumb-bl-place -1  0 "lm" :steps steps :xy1 4 :xy2 wall-xy-offset-thin)
     :thumb-br-tl-to-bl (wall-brace-catmull-rom-spline-fn (point-fn thumb-bl-place -1 0 "tl" :degrees wall-xy-offset)
                                                          (point-fn thumb-br-place -1 0 "tl" :degrees wall-xy-offset)
                                                          (point-fn thumb-br-place -1 0 "bl" :degrees wall-xy-offset)
                                                          (point-fn thumb-br-place -1 -1 "bl" :degrees wall-xy-offset)
                                                          steps :alpha-type :centripetal :t1 0.8 :t2 0.9)
    ;(thumb-wall-brace-polyhedron-fn thumb-br-place -1  0 "tl" thumb-br-place -1  0 "bl" :steps steps)
     :thumb-br-bl-to-br (wall-brace-catmull-rom-spline-fn (point-fn thumb-bl-place -1 0 "bl" :degrees wall-xy-offset)
                                                          (point-fn thumb-br-place 0 -1 "bl" :degrees wall-xy-offset)
                                                          (point-fn thumb-br-place 0 -1 "br" :degrees wall-xy-offset)
                                                          (point-fn thumb-mr-place 0 -1 "bl" :degrees wall-xy-offset)
                                                          steps :alpha-type :uniform :t1 0.5 :t2 0.75)
     
 :thumb-br-bl-to-mr-br 
    ;;  (wall-brace-catmull-rom-spline-fn (point-fn thumb-br-place -1 0 "bl" :degrees wall-xy-offset)
    ;;                                                      (point-fn thumb-br-place 0 -1 "bl" :degrees wall-xy-offset)
    ;;                                                      (point-fn thumb-mr-place 0 -1 "br" :degrees wall-xy-offset)
    ;;                                                      (point-fn thumb-mr-place 1 0 "br" :degrees wall-xy-offset)
    ;;                                                      steps :alpha-type :uniform :t1 0.5 :t2 0.75
    ;;                                                      )
(wall-brace-cubic-fn (points-for-curved-wall-from-thumb-br-bl-to-mr-br steps))
    ;(thumb-wall-brace-polyhedron-fn thumb-br-place  0 -1 "bl" thumb-br-place  0 -1 "br" :steps steps)
     :thumb-br-to-mr (wall-brace-catmull-rom-spline-fn (point-fn thumb-br-place 0 -1 "bl" :degrees wall-xy-offset)
                                                       (point-fn thumb-br-place 0 -1 "br" :degrees wall-xy-offset)
                                                       (point-fn thumb-mr-place 0 -1 "bl" :degrees wall-xy-offset)
                                                       (point-fn thumb-mr-place 0 -1 "br" :degrees wall-xy-offset)
                                                       steps :alpha-type :centripetal :t1 0.8 :t2 1)
    ;(thumb-wall-brace-polyhedron-fn thumb-br-place  0 -1 "br" thumb-mr-place  0 -1 "bl" :steps steps)
     :thumb-br-bl-to-bm (thumb-wall-brace-polyhedron-fn  thumb-br-place  0 -1 "bl" thumb-br-place  0 -1 "bm" :steps steps :xy2 wall-xy-offset-thin)
     :thumb-mr-bl-to-br (wall-brace-catmull-rom-spline-fn (point-fn thumb-br-place 0 -1 "br" :degrees wall-xy-offset)
                                                          (point-fn thumb-mr-place 0 -1 "bl" :degrees wall-xy-offset)
                                                          (point-fn thumb-mr-place 0 -1 "br" :degrees wall-xy-offset)
                                                          (point-fn thumb-mr-place 1 -1 "br" :degrees wall-xy-offset)
                                                          steps :alpha-type :centripetal :t1 0.5 :t2 0.6)
    ;(thumb-wall-brace-polyhedron-fn  thumb-mr-place  0 -1 "bl" thumb-mr-place  0 -1 "br" :steps steps)
     :thumb-tr-br-to-tr (wall-brace-catmull-rom-spline-fn 
                         (points-fn-deg thumb-tr-place 1 -1 "br" wall-xy-offset)
                         (points-fn-deg thumb-tr-place 1 0 "br" wall-xy-offset)
                         (points-fn-deg thumb-tr-place 1 -1 "tr" wall-xy-offset-mid)
                         (points-fn-rad  (partial key-place 1 cornerrow) 1 -0.1 "br"   wall-xy-offset-mid)
                         steps
                         )
;(thumb-wall-brace-polyhedron-fn thumb-tr-place  1 0 "br" thumb-tr-place  1 -1 "tr" :xy1 wall-xy-offset :xy2 wall-xy-offset-mid :steps steps)
}))

(defn thumb-walls-polyhedron-circular [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [thumb-wall-brace-polyhedron-fn (get-thumb-wall-brace-polyhedron-fn bottom-plate)
        thumb-wall-brace-polyhedron-circular-fn (get-thumb-wall-brace-polyhedron-circular-fn bottom-plate)
        thumb-wall-brace-polyhedron-with-circular-fn (get-thumb-wall-brace-polyhedron-with-circular bottom-plate)]
    {:thumb-bl-tl-to-bl (thumb-wall-brace-polyhedron-circular-fn thumb-bl-place -1  0 "tl" thumb-bl-place -1  0 "bl" :steps steps)
     :thumb-bl-to-br (thumb-wall-brace-polyhedron-circular-fn thumb-bl-place -1  0 "bl" thumb-br-place -1  0 "tl" :steps steps)
     :thumb-br-tl-to-bl (thumb-wall-brace-polyhedron-circular-fn thumb-br-place -1  0 "tl" thumb-br-place -1  0 "bl" :steps steps)
     :thumb-br-bl-to-br (thumb-wall-brace-polyhedron-circular-fn thumb-br-place  0 -1 "bl"  thumb-br-place  0 -1 "br" :steps steps)
     :thumb-br-to-mr  (thumb-wall-brace-polyhedron-circular-fn thumb-br-place  0 -1 "br" thumb-mr-place  0 -1 "bl" :steps steps)
     :thumb-mr-bl-to-br (thumb-wall-brace-polyhedron-with-circular-fn  thumb-mr-place  0 -1 "bl" thumb-mr-place  0 -1 "br" :steps steps :curve-type2 :standard)
     :thumb-tr-br-to-tr (thumb-wall-brace-polyhedron-fn thumb-tr-place  1 0 "br" thumb-tr-place  1 -1 "tr" :xy1 wall-xy-offset :xy2 wall-xy-offset-mid :steps steps)}))

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
                            steps)]
    {:thumb-br-bl-corner thumb-br-bl-corner
     :thumb-mr-br-corner thumb-mr-br-corner
     :thumb-mr-to-thumb-tr-corner thumb-mr-to-thumb-tr-corner
     :thumb-tr-br-corner thumb-tr-br-corner}))
(defn get-thumb-mr-br-to-thumb-tr-br-parameters-old [steps] 
  {:place1 thumb-bl-place :dx1 -1 :dy1 0  :post-position-1 "tl" :rad-or-deg1 :degrees
   :place-mid1 thumb-bl-place :dxmid1 -1  :dymid1 0  :post-position-mid1 "bl" :rad-or-degmid1 :degrees
   :place-mid2 thumb-br-place :dxmid2 -1 :dymid2 0.5 :post-position-mid2  "tl" :rad-or-degmid2 :degrees
   :place2 thumb-br-place :dx2 -1 :dy2 0  :post-position-2 "tl" :rad-or-deg2 :degrees
   :xy1 wall-xy-offset :xymid1 wall-xy-offset :xymid2 wall-xy-offset :xy2 wall-xy-offset :steps steps})

(defn get-thumb-mr-br-to-thumb-tr-br-parameters[steps]
  (let [point-fn (fn [place dx dy post-position  rad-or-deg xy] {:place place :dx dx :dy dy :post-position post-position :rad-or-deg rad-or-deg :xy xy})
        ]
    {:place1 thumb-bl-place :dx1 -1 :dy1 0  :post-position-1 "tl" :rad-or-deg1 :degrees
   :place-mid1 thumb-bl-place :dxmid1 -1  :dymid1 0  :post-position-mid1 "bl" :rad-or-degmid1 :degrees
   :place-mid2 thumb-br-place :dxmid2 -1 :dymid2 0.5 :post-position-mid2  "tl" :rad-or-degmid2 :degrees
   :place2 thumb-br-place :dx2 -1 :dy2 0  :post-position-2 "tl" :rad-or-deg2 :degrees
   :xy1 wall-xy-offset :xymid1 wall-xy-offset :xymid2 wall-xy-offset :xy2 wall-xy-offset :steps steps}))



(defn thumb-corners-polyhedron-for-convex-cluster [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [wall-brace-quadratic-fn (get-wall-brace-quadratic-fn bottom-plate)
        wall-brace-cubic-fn (get-wall-brace-cubic-fn bottom-plate)
        collect-fn (get-collect-fn bottom-plate)
        wall-brace-catmull-rom-spline-fn (get-wall-brace-catmull-rom-spline-fn bottom-plate)
        thumb-bl-tl-to-lm-corner (wall-brace-quadratic-fn (partial thumb-bl-place) -1 0 "tl" :degrees
                                                          (partial thumb-bl-place) -1 -1 "tl" :degrees
                                                          (partial thumb-bl-place) -1 0 "lm" :degrees
                                                          wall-xy-offset wall-xy-offset wall-xy-offset
                                                          steps)
        point-fn (fn [place dx dy post-position  rad-or-deg xy] {:place place :dx dx :dy dy :post-position post-position :rad-or-deg rad-or-deg :xy xy}) 
        thumb-bl-tl (wall-brace-polyhedron-curve-points thumb-bl-place -1 0 "tl" :degrees wall-xy-offset steps)
        thumb-br-tl (wall-brace-polyhedron-curve-points thumb-br-place -1 0 "tl" :degrees wall-xy-offset steps)
        thumb-br-bl (wall-brace-polyhedron-curve-points thumb-br-place -1 0 "bl" :degrees wall-xy-offset steps)
        screen-holder-right-outside-points (screen-holder-points-fn screen-holder-top-right-outside-point screen-holder-bottom-right-outside-point screen-holder-bottom-right-outside-floor-point steps)
         screen-holder-right-inside-points
(screen-holder-points-fn screen-holder-top-right-inside-point screen-holder-bottom-right-inside-point screen-holder-bottom-right-inside-floor-point steps)
        thumb-bl-tl-to-br-tl-corner-outer (into [] (apply concat
                                                          (for [index (range 0 (inc steps))]
                                                            (catmull-rom-spline-segment
                                                             (nth (thumb-br-bl :outer-points) index)
                                                             (nth (thumb-br-tl :outer-points) index)
                                                             (nth (thumb-bl-tl :outer-points)index)
                                                             (nth screen-holder-right-outside-points index)
                                                             steps
                                                             :alpha-type :centripetal
                                          ;:t1 0.7 :t2 0.9
                                                             ))))
        thumb-bl-tl-to-br-tl-corner-inner  (into [] (apply concat
                                                           (for [index (range 0 (inc steps))
                                                                 :let [thumb-bl-tl-inner-reversed  (reverse (thumb-bl-tl :inner-points))
                                                                       thumb-br-tl-inner-reversed  (reverse (thumb-br-tl :inner-points))
                                                                       thumb-br-bl-inner-reversed (reverse (thumb-br-bl :inner-points))
                                                                       screen-holder-right-inside-points-reversed (reverse screen-holder-right-inside-points)]]
                                                             (catmull-rom-spline-segment
                                                              (nth thumb-br-bl-inner-reversed index)
                                                              (nth thumb-br-tl-inner-reversed index)
                                                              (nth thumb-bl-tl-inner-reversed index)
                                                              (nth screen-holder-right-inside-points-reversed index)
                                                              steps
                                                              :alpha-type :centripetal
                                         ;:t1 0.7 :t2 0.9
                                         ;:t1 0.7 :t2 0.9
                                                              ))))
        thumb-bl-tl-to-br-tl-corner-polyhedron (generate-bezier-along-bezier-polyhedron thumb-bl-tl-to-br-tl-corner-outer thumb-bl-tl-to-br-tl-corner-inner steps)
        thumb-bl-tl-to-br-tl-corner (if bottom-plate (reverse (filter #(zero? (nth % 2) ) thumb-bl-tl-to-br-tl-corner-outer)) thumb-bl-tl-to-br-tl-corner-polyhedron) 
        ;; (catmull-rom-spline-curve
        ;;                              (point-fn (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (+ (/ screen-holder-depth 2))) 0 -1 :center :degrees 0)
        ;;                              (wall-brace-polyhedron-curve-points thumb-bl-place -1 0 "tl" :degrees wall-xy-offset)
        ;;                              (wall-brace-polyhedron-curve-points thumb-br-place -1 0 "tl" :degrees wall-xy-offset)
        ;;                              (wall-brace-polyhedron-curve-points thumb-br-place -1 0 "bl" :degrees wall-xy-offset)
        ;;                              steps
        ;;                              ) 
        ;; (collect-fn
        ;;                             (wall-brace-catmull-rom-spline-fn
        ;;                              (point-fn (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (+ (/ screen-holder-depth 2))) 0 -1 :center :degrees 0)
        ;;                              (point-fn thumb-bl-place -1 0 "tl" :degrees wall-xy-offset)
        ;;                              (point-fn thumb-bl-place -1 0 "lm":degrees wall-xy-offset)
        ;;                              (point-fn thumb-br-place -1 0.5 "tl" :degrees wall-xy-offset)
        ;;                              steps
        ;;                              )
        ;;                              (wall-brace-catmull-rom-spline-fn 
        ;;                               (point-fn thumb-bl-place -1 0 "tl" :degrees wall-xy-offset)
        ;;                               (point-fn thumb-bl-place -1 0 "lm" :degrees wall-xy-offset)
        ;;                               (point-fn thumb-br-place -1 0.5 "tl" :degrees wall-xy-offset)
        ;;                               (point-fn thumb-br-place -1 0 "tl" :degrees wall-xy-offset) 
        ;;                               steps)
        ;;                              (wall-brace-catmull-rom-spline-fn 
        ;;                               (point-fn thumb-bl-place -1 0 "lm" :degrees wall-xy-offset)
        ;;                               (point-fn thumb-br-place -1 0.5 "tl" :degrees wall-xy-offset)
        ;;                               (point-fn thumb-br-place -1 0 "tl" :degrees wall-xy-offset)
        ;;                               (point-fn thumb-br-place -1 0 "bl" :degrees wall-xy-offset)
        ;;                               steps)
        ;;                              ) 
        ;; (wall-brace-quadratic-fn (partial thumb-bl-place) -1 0 "tl" :degrees
        ;;                                                      (partial thumb-bl-place) -1 0 "bl" :degrees
        ;;                                                      (partial thumb-br-place) -1 0 "tl" :degrees
        ;;                                                      wall-xy-offset wall-xy-offset
        ;;                                                      wall-xy-offset
        ;;                                                      steps)
        ;test-wall 
        thumb-br-tl-corner (wall-brace-quadratic-fn (partial thumb-br-place) 0 1 "tl" :degrees
                                                    (partial thumb-br-place) -1 1 "tl" :degrees
                                                    (partial thumb-br-place) -1 0 "tl" :degrees
                                                    wall-xy-offset-thin wall-xy-offset wall-xy-offset
                                                    steps)
        thumb-br-bl-corner  (wall-brace-quadratic-fn (partial thumb-br-place) -1 0 "bl" :degrees
                                                     (partial thumb-br-place) -1 -1 "bl" :degrees
                                                     (partial thumb-br-place) 0 -1 "bl" :degrees
                                                     steps)
        thumb-br-bl-to-bm-corner  (wall-brace-quadratic-fn (partial thumb-br-place) 0 -1 "bl" :degrees
                                                           (partial thumb-br-place) 1 -1 "bl" :degrees
                                                           (partial thumb-br-place) 0 -1 "bm" :degrees
                                                           wall-xy-offset wall-xy-offset wall-xy-offset
                                                           steps)
        thumb-br-bl-to-mr-bl-corner  (wall-brace-quadratic-fn (partial thumb-br-place) 0 -1 "bm" :degrees
                                                              (partial thumb-br-place) 0 -1 "br" :degrees
                                                              (partial thumb-mr-place) 0 -1 "bl" :degrees
                                                              wall-xy-offset wall-xy-offset wall-xy-offset
                                                              steps)
        thumb-mr-bl-corner (wall-brace-quadratic-fn
                            (partial thumb-mr-place) -1 0 "bl" :degrees
                            (partial thumb-mr-place) -1 -1 "bl" :degrees
                            (partial thumb-mr-place)  0 -1 "bl" :degrees
                            wall-xy-offset-thin wall-xy-offset-mid wall-xy-offset
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
                            steps)]
    {:thumb-bl-tl-to-lm-corner thumb-bl-tl-to-lm-corner
     :thumb-bl-tl-to-br-tl-corner thumb-bl-tl-to-br-tl-corner
     :thumb-br-tl-corner thumb-br-tl-corner
     :thumb-br-bl-corner thumb-br-bl-corner
     :thumb-br-bl-to-bm-corner thumb-br-bl-to-bm-corner
     :thumb-br-bl-to-mr-bl-corner thumb-br-bl-to-mr-bl-corner
     :thumb-mr-bl-corner thumb-mr-bl-corner
     :thumb-mr-br-corner thumb-mr-br-corner
     :thumb-mr-to-thumb-tr-corner thumb-mr-to-thumb-tr-corner
     :thumb-tr-br-corner thumb-tr-br-corner}))

(defn thumb-corners-polyhedron-circular [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [wall-brace-quadratic-fn (get-wall-brace-quadratic-fn bottom-plate)
        wall-brace-quadratic-polyhedron-circular-fn (get-wall-brace-quadratic-polyhedron-circular-fn bottom-plate)
        thumb-br-bl-corner  (wall-brace-quadratic-polyhedron-circular-fn (partial thumb-br-place) -1 0 "bl" :degrees
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
                            steps)]
    {:thumb-br-bl-corner thumb-br-bl-corner
     :thumb-mr-br-corner thumb-mr-br-corner
     :thumb-mr-to-thumb-tr-corner thumb-mr-to-thumb-tr-corner
     :thumb-tr-br-corner thumb-tr-br-corner}))

(defn left-section-to-thumb-cluster-convex-connecters [steps]
  (let [thumb-bl-tl (wall-brace-polyhedron-points thumb-bl-place -1 0 "tl"  :degrees steps) 
        thumb-bl-tr (wall-brace-polyhedron-points thumb-bl-place 0 -1 "tr"  :degrees steps)
        thumb-br-tl (wall-brace-polyhedron-points thumb-br-place -1 0 "tl"  :degrees steps)
        thumb-bl-br (web-post-point thumb-bl-place "br"  :degrees)
        thumb-tl-tl (web-post-point thumb-tl-place "tl"  :degrees)
        thumb-tl-bl (web-post-point thumb-tl-place "bl"  :degrees)
        thumb-tl-tr (web-post-point thumb-tl-place "tr"  :degrees)
        thumb-tr-tl (web-post-point thumb-tr-place "tl"  :degrees)
        thumb-bl-tr-to-thumb-tl-tl-distance (vector-distance (thumb-bl-tr :web-post-position-top) (thumb-tl-tl :top))
        tps-65-outer-point-to-connect-to-thumb-bl-tr (find-point-on-line-using-x tps-65-top-left-outer tps-65-bottom-left-outer (nth (thumb-bl-tr :web-post-position-top) 0))
        tps-65-inner-point-to-connect-to-thumb-bl-tr (find-point-on-line-using-x tps-65-top-left-inner tps-65-bottom-left-inner (nth (thumb-bl-tr :web-post-position-bottom) 0))
        tps-65-outer-point-to-connect-to-thumb-bl-tr-control-point (find-point-on-line-using-x tps-65-top-right-outer tps-65-bottom-right-outer (nth (thumb-bl-tr :web-post-position-top) 0))
        tps-65-inner-point-to-connect-to-thumb-bl-tr-control-point (find-point-on-line-using-x tps-65-top-right-inner tps-65-bottom-right-inner (nth (thumb-bl-tr :web-post-position-bottom) 0))
        
               first-column-cornerrow-bl (web-post-point (partial key-place 0 cornerrow) "bl"  :radians)
        first-column-cornerrow-br (web-post-point (partial key-place 0 cornerrow) "br"  :radians)
        tps-65-top-left-inner-higher    (transform-position
                                         (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-left-position)
                                         [(- (+ tps-65-corner-radius 0.05))  (+ tps-65-corner-radius 0.05) (- (/ web-thickness 4))])
        tps-65-bottom-left-inner-translated (transform-position
                                             (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-bottom-left-position)
                                             [0  (- (+ tps-65-corner-radius 0.05)) (- (/ web-thickness 2))])
        tps-65-outside-control-points-1 (bezier-linear tps-65-top-right-outer tps-65-mid-right-outer  steps)
        tps-65-outside-control-points-2 (bezier-linear tps-65-mid-right-outer tps-65-outer-point-to-connect-to-thumb-bl-tr-control-point steps)
        tps-65-outside-control-points-3 (bezier-linear tps-65-outer-point-to-connect-to-thumb-bl-tr-control-point tps-65-bottom-right-inner steps)
        tps-65-inside-control-points-1 (bezier-linear tps-65-mid-right-inner  tps-65-top-right-outer steps)
        tps-65-inside-control-points-2 (bezier-linear  tps-65-inner-point-to-connect-to-thumb-bl-tr-control-point tps-65-mid-right-inner steps)
        tps-65-inside-control-points-3 (bezier-linear tps-65-bottom-right-inner tps-65-inner-point-to-connect-to-thumb-bl-tr-control-point   steps)
        tps-65-outside-points-1 (bezier-linear  tps-65-top-left-outer tps-65-mid-left-outer steps)
        tps-65-outside-points-2 (bezier-linear tps-65-mid-left-outer tps-65-outer-point-to-connect-to-thumb-bl-tr steps)
        tps-65-outside-points-3 (bezier-linear tps-65-outer-point-to-connect-to-thumb-bl-tr tps-65-bottom-left-outer steps)
        tps-65-inside-points-1 (bezier-linear tps-65-mid-left-inner (mapv + [0 0 0] tps-65-top-left-inner)  steps)
        tps-65-inside-points-2 (bezier-linear tps-65-inner-point-to-connect-to-thumb-bl-tr tps-65-mid-left-inner steps) 
               tps-65-inside-points-3 (bezier-linear tps-65-bottom-left-inner tps-65-inner-point-to-connect-to-thumb-bl-tr steps)
        left-section-to-thumb-cluster-convex-walls-top-outside (reverse (catmull-rom-spline-segment 
                                                                         (thumb-br-tl :web-post-position-top)
                                                                         (thumb-bl-tl :web-post-position-top) 
                                                                         screen-holder-top-right-outside-point
                                                                         screen-holder-top-right-outside-point-translated-for-left-section 
                                                                         steps
                                                                         :alpha-type :centripetal 
                                                                         ))
        left-section-to-thumb-cluster-convex-walls-top-inside  (catmull-rom-spline-segment
                                                                (thumb-br-tl :web-post-position-bottom) 
                                                                (thumb-bl-tl :web-post-position-bottom) 
                                                                screen-holder-top-right-inside-point
                                                                screen-holder-top-right-inside-point-translated-for-left-section 
                                                                steps
                                                                :alpha-type :centripetal )
        left-section-to-thumb-cluster-convex-walls-bottom-outside (reverse (catmull-rom-spline-segment
                                                                            (thumb-br-tl :wall-locate1-point)
                                                                            (thumb-bl-tl :wall-locate1-point)
                                                                            screen-holder-bottom-right-outside-point
                                                                            screen-holder-bottom-right-outside-point-translated-for-left-section
                                                                            steps
                                                                            :alpha-type :centripetal))
        
        left-section-to-thumb-cluster-convex-walls-bottom-inside (catmull-rom-spline-segment
                                                                  (thumb-br-tl :wall-locate-2-bottom-floor) 
                                                                  (thumb-bl-tl :wall-locate-2-bottom-floor)
                                                                  screen-holder-bottom-right-inside-floor-point
                                                                  screen-holder-bottom-right-inside-floor-point-translated-for-left-section 
                                                                  steps
                                                                  :alpha-type :centripetal )
        thumb-bl-tl-to-thumb-bl-tr-top-outside (bezier-linear (thumb-bl-tl :web-post-position-top) (thumb-bl-tr :web-post-position-top) steps)
        thumb-bl-tl-to-thumb-bl-tr-bottom-outside (bezier-linear (thumb-bl-tl :wall-locate1-point) (thumb-bl-tr :wall-locate1-point) steps)
        thumb-bl-tl-to-thumb-bl-tr-top-inside (bezier-linear  (thumb-bl-tr :web-post-position-bottom) (thumb-bl-tl :web-post-position-bottom) steps)
        thumb-bl-tl-to-thumb-bl-tr-bottom-inside (bezier-linear (thumb-br-tl :wall-locate-2-bottom-floor) (thumb-bl-tl :wall-locate-2-bottom-floor)  steps)
        bottom-outside-points (concat (drop-last left-section-to-thumb-cluster-convex-walls-top-outside) thumb-bl-tl-to-thumb-bl-tr-top-outside)
        bottom-outside-control-points (concat (drop-last left-section-to-thumb-cluster-convex-walls-bottom-outside) thumb-bl-tl-to-thumb-bl-tr-bottom-outside)
        bottom-inside-points (concat (drop-last left-section-to-thumb-cluster-convex-walls-top-inside) thumb-bl-tl-to-thumb-bl-tr-top-inside)
        bottom-inside-control-points (concat (drop-last left-section-to-thumb-cluster-convex-walls-bottom-inside) thumb-bl-tl-to-thumb-bl-tr-bottom-inside)
        
        outside-points-1 (into [] (apply concat
                                         (for [index (range 0 (inc steps))]
                                           (catmull-rom-spline-segment
                                            (nth tps-65-outside-control-points-1 index)
                                            (nth tps-65-outside-points-1 index)
                                            (nth left-section-to-thumb-cluster-convex-walls-top-outside index)
                                            (nth left-section-to-thumb-cluster-convex-walls-bottom-outside index)
                                            steps
                                            :alpha-type :centripetal
                                            ))))
        outside-points-2 (into [] (apply concat
                                         (for [index (range 0 (inc steps))]
                                           (catmull-rom-spline-segment
                                            (nth tps-65-outside-control-points-2 index)
                                            (nth tps-65-outside-points-2 index)
                                            (nth thumb-bl-tl-to-thumb-bl-tr-top-outside index)
                                            (nth thumb-bl-tl-to-thumb-bl-tr-bottom-outside index)
                                            steps
                                            :alpha-type :centripetal
                                            ))))
        
        inside-points-1 (into [] (apply concat
                                        (for [index (range 0 (inc steps))]
                                          (bezier-linear
                                           ;catmull-rom-spline-curve
                                           ;(nth tps-65-inside-control-points-1 index)
                                           (nth tps-65-inside-points-1 index)
                                           (nth left-section-to-thumb-cluster-convex-walls-top-inside index)
                                           ;(nth left-section-to-thumb-cluster-convex-walls-bottom-inside index)
                                           steps
                                           ;:alpha-type :centripetal :t1 0.71 :t2 0.8
                                           ))))
        inside-points-2 (into [] (apply concat
                                        (for [index (range 0 (inc steps))
                                              :let [bottom-control-points (reverse thumb-bl-tl-to-thumb-bl-tr-bottom-inside) ]]
                                          (
                                           ;bezier-linear
                                           catmull-rom-spline-segment
                                           (nth tps-65-inside-control-points-2 index)
                                           (nth tps-65-inside-points-2 index)
                                           (nth thumb-bl-tl-to-thumb-bl-tr-top-inside index)
                                           (nth bottom-control-points index)
                                           steps
                                           ;:alpha-type :centripetal
                                           ))))
        top-left-surface-outer-points (into []
                                            (apply concat
                                                   (for [index (range 0 (inc steps))
                                                         :let [left-side-points (bezier-cubic 
                                                                                 screen-holder-outside-point-with-same-y-as-tps-65-top-left-upper
                                                                                 screen-holder-top-right-inside-point-translated-with-same-y-as-tps-65-top-left-inner
                                                                                 tps-65-top-left-control-point-outer 
                                                                                 tps-65-top-left-outer
                                                                                 steps)
                                                               right-side-points (reverse (take (inc steps) outside-points-1))]]
                                                     (bezier-linear 
                                                      (nth left-side-points index)
                                                      (nth right-side-points index) 
                                                      steps))))
        top-left-surface-inner-points (into []
                                            (apply concat
                                                   (for [index (range 0 (inc steps))
                                                         :let [left-side-points   (bezier-linear   tps-65-top-left-inner screen-holder-top-right-outside-point steps)
                                                               ;(map #(mapv + [-0.1 0 0] %) (take-last (inc steps) inside-points-1))
                                                              ;;  (bezier-linear
                                                               
                                                              ;;                    screen-holder-inside-point-with-same-y-as-tps-65-top-left-inner
                                                              ;;                    tps-65-top-left-inner-with-same-z-as-screen-holder-top-right-inside-point-translated-with-same-y-as-tps-65-top-left-inner
                                                              ;;                    steps)
                                                               right-side-points (bezier-linear 
                                                                                  tps-65-top-left-inner
                                                                                  screen-holder-outside-point-with-same-y-as-tps-65-top-left-upper 
                                                                                  steps
                                                                                  )]]
                                                     (bezier-linear 
                                                      (nth right-side-points index)
                                                      (nth left-side-points index)
                                                      ;tps-65-top-left-inner-with-same-z-as-screen-holder-top-right-inside-point-translated-with-same-y-as-tps-65-top-left-inner
                                                      steps))))
        thumb-bl-to-tps-65-outer (take-last (inc steps) outside-points-2)
        thumb-bl-tr-to-tps-65-inner (reverse (take (inc steps) inside-points-2))
        ;;(reverse (catmull-rom-spline-curve
        ;;                                                   ;(map + tps-65-bottom-left-inner [0 -40 -20])
        ;;                                                   tps-65-inner-point-to-connect-to-thumb-bl-tr-control-point
        ;;                                                   tps-65-inner-point-to-connect-to-thumb-bl-tr
        ;;                                                   (thumb-bl-tr :web-post-position-bottom)
        ;;                                                   (mapv + [0 -40 20] (thumb-bl-br :top))
        ;;                                                   steps
        ;;                                                   :alpha-type :centripetal
        ;;                                                   ))
        thumb-tl-tl-to-tps-65-bottom-left-inner (reverse (catmull-rom-spline-segment
                                                          (mapv + tps-65-bottom-left-inner [0 -2 4])
                                                          tps-65-bottom-left-inner
                                                          (thumb-tl-tl :bottom)
                                                          (mapv + [0 -8 0] (assoc (thumb-tl-tl :bottom) 2 0))
                                                          steps
                                                          :alpha-type :centripetal ))
        thumb-tl-tl-to-tps-65-bottom-left-outer (catmull-rom-spline-segment
                                                 tps-65-top-right-outer
                                                 tps-65-bottom-left-outer
                                                 (thumb-tl-tl :top)
                                                 (thumb-tl-bl :top)
                                                 steps
                                                 :alpha-type :centripetal )
        thumb-bl-tr-to-thumb-tl-tl-to-tps-65-bottom-left-outer (into []
                                                                     (apply concat
                                                                            (for [index (range 0 (inc steps))
                                                                                  :let [a (take-last (inc steps) outside-points-2)
                                                                                        b (catmull-rom-spline-segment 
                                                                                           tps-65-top-right-outer
                                                                                           tps-65-bottom-left-outer
                                                                                           (thumb-tl-tl :top)
                                                                                           (assoc (thumb-tl-tl :top) 2 0)
                                                                                           steps
                                                                                           :alpha-type :centripetal
                                                                                           )
                                     ;(bezier-linear tps-65-bottom-left-outer (thumb-tl-tl :top) steps)
                                                                                        ]]
                                                                              (bezier-linear 
                                                                               (nth thumb-tl-tl-to-tps-65-bottom-left-outer index)
                                                                               (nth thumb-bl-to-tps-65-outer index) 
                                                                               steps))))
        
        thumb-bl-tr-to-thumb-tl-tl-to-tps-65-bottom-left-inner (into []
                                                                     (apply concat
                                                                            (for [index (range 0 (inc steps))
                                                                                  :let [a (catmull-rom-spline-curve
                                                                                           [tps-65-bottom-right-inner
                                                                                           tps-65-bottom-left-inner
                                                                                           (thumb-bl-tr :web-post-position-bottom)
                                                                                           (thumb-bl-br :bottom)]
                                                                                           steps
                                                                                           :alpha-type :centripetal :t1 0.7 :t2 0.9) 
                                     ;(take (inc steps) inside-points-2)
                                                                                        b (catmull-rom-spline-curve 
                                                                                           [tps-65-bottom-left-inner-translated
                                                                                           tps-65-bottom-left-inner
                                                                                           (thumb-tl-tl :bottom)
                                                                                           (mapv + [0 -8 0] (assoc (thumb-tl-tl :bottom) 2 0))]
                                                                                           steps
                                                                                           :alpha-type :centripetal :t1 0.7 :t2 0.9
                                                                                           )
                                     ;(bezier-quadratic tps-65-bottom-left-inner-translated (calculate-point-between-points tps-65-bottom-left-inner-translated (thumb-tl-tl :bottom) [0 2 0]) (thumb-tl-tl :bottom) steps)
                                                                                        ]]
                                                                              (bezier-linear 
                                                                               (nth thumb-tl-tl-to-tps-65-bottom-left-inner index)
                                                                               (nth thumb-bl-tr-to-tps-65-inner index) 
                                                                               steps))))
        
        ] 
         
         (union
           
           (generate-bezier-along-bezier-polyhedron outside-points-1 inside-points-1  steps)
         (generate-bezier-along-bezier-polyhedron outside-points-2 inside-points-2 steps)
          ;(generate-bezier-along-bezier-polyhedron top-left-surface-outer-points top-left-surface-inner-points steps)
              ;; (chained-hull-for-four-lists (plot-bezier-points
              ;;                                     (bezier-cubic
              ;;                                     screen-holder-outside-point-with-same-y-as-tps-65-top-left-upper
              ;;                                     screen-holder-top-right-inside-point-translated-with-same-y-as-tps-65-top-left-inner
              ;;                                     tps-65-top-left-control-point-outer
              ;;                                     tps-65-top-left-outer
              ;;                                     steps)
              ;;                                     (sphere 0.001)
              ;;                                     ) 
              ;;                                    (plot-bezier-points (bezier-linear screen-holder-outside-point-with-same-y-as-tps-65-top-left-upper tps-65-top-left-inner  steps) (sphere 0.001))
              ;;                                    (plot-bezier-points (reverse (take (inc steps) outside-points-1 )) (sphere 0.001)) 
              ;;                                  (plot-bezier-points (bezier-linear  screen-holder-top-right-inside-point tps-65-top-left-inner steps) (sphere 0.001))
              ;;                                    steps
              ;;                                    )
          
         ; (-# (plot-bezier-points (take-last (inc steps) top-left-surface-outer-points) (sphere 0.1)))
        ;;(generate-bezier-along-bezier-polyhedron (concat (drop-last  (inc steps) top-left-surface-outer-points) outside-points-1) 
        ;;                                          (concat (drop-last  (inc steps)   inside-points-1) top-left-surface-inner-points )
        ;;                                          (inc (* steps 2))) 
     
         ;(polyhedron top-left-surface-inner-points (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps))
         (generate-bezier-along-bezier-polyhedron thumb-bl-tr-to-thumb-tl-tl-to-tps-65-bottom-left-outer thumb-bl-tr-to-thumb-tl-tl-to-tps-65-bottom-left-inner steps)
         
         (generate-bezier-along-bezier-polyhedron-from-points-list-linear 
          (bezier-linear (first-column-cornerrow-bl :top) (thumb-tl-tr :top) steps)
          thumb-tl-tl-to-tps-65-bottom-left-outer 
          (bezier-linear  (thumb-tl-tr :bottom) (first-column-cornerrow-bl :bottom) steps) 
          thumb-tl-tl-to-tps-65-bottom-left-inner 
          steps
          )
         
   
         (difference (generate-bezier-along-bezier-polyhedron-from-points-linear
          (first-column-cornerrow-bl :top) (first-column-cornerrow-br :top)  
          (thumb-tl-tr :top) (thumb-tr-tl :top)
          (first-column-cornerrow-br :bottom)(first-column-cornerrow-bl :bottom)
          (thumb-tr-tl :bottom)(thumb-tl-tr :bottom)   
          steps
          :inside-upper-control-point-vector [0 2 0])
                     (->> top-nub
                          (mirror [1 0 0])
                          (mirror [0 1 0]) 
                          (rotate (/ Math/PI 2) [0 0 1]) 
                          (key-place 0 cornerrow)))
         
         ;(translate (first (take-last (inc steps) outside-points-2)) (sphere 1))
        
        ;;  (plot-bezier-points (take-last (inc steps) inside-points-1) (sphere 0.1))
        ;;  (plot-bezier-points (bezier-cubic
        ;;                       tps-65-top-left-outer
        ;;                       tps-65-top-left-control-point-outer
        ;;                       screen-holder-top-right-inside-point-translated-with-same-y-as-tps-65-top-left-inner
        ;;                       screen-holder-outside-point-with-same-y-as-tps-65-top-left-upper
        ;;                       steps) (sphere 0.1))
        ;;  (-# (plot-bezier-points (bezier-linear
        ;;                           screen-holder-outside-point-with-same-y-as-tps-65-top-left-upper
        ;;                           tps-65-top-left-inner-higher
        ;;                           steps) (sphere 0.1)))
        ;;  (plot-bezier-points (take (inc steps) outside-points-1) (sphere 0.1))
         ;(plot-bezier-points (take (inc steps) inside-points-2) (sphere 0.1))
        ))) 


(defn left-section-to-thumb-cluster-convex-walls [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [wall-brace-quadratic-fn (get-wall-brace-quadratic-fn bottom-plate)
        wall-brace-polyhedron-fn (get-wall-brace-polyhedron-fn bottom-plate)
        wall-brace-catmull-rom-spline-fn (get-wall-brace-catmull-rom-spline-fn bottom-plate)
        
        screen-holder-left-outside-points  (screen-holder-points-fn screen-holder-top-left-outside-point screen-holder-bottom-left-outside-point screen-holder-bottom-left-outside-floor-point steps)

        screen-holder-left-inside-points
(screen-holder-points-fn screen-holder-top-left-inside-point screen-holder-bottom-left-inside-point screen-holder-bottom-left-inside-floor-point steps)
        
        screen-holder-left-inside-points-translated
        (screen-holder-points-fn 
       (mapv + [(- (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2)))) 0 0] screen-holder-top-left-inside-point)
       (mapv + [(- (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2)))) 0 0] screen-holder-bottom-left-inside-point)
       (mapv + [(- (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2)))) 0 0] screen-holder-bottom-left-inside-floor-point)
        steps )

        screen-holder-right-outside-points  (screen-holder-points-fn screen-holder-top-right-outside-point screen-holder-bottom-right-outside-point screen-holder-bottom-right-outside-floor-point steps)
        screen-holder-right-outside-points-translated  (screen-holder-points-fn screen-holder-top-right-outside-point-translated-for-left-section
                                                                                (mapv + [(- (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2)))) (- (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2)))) 0] screen-holder-bottom-right-outside-point) 
                                                                                screen-holder-bottom-right-outside-floor-point-translated-for-left-section
                                                                                steps)
        screen-holder-right-inside-points
        (screen-holder-points-fn screen-holder-top-right-inside-point screen-holder-bottom-right-inside-point screen-holder-bottom-right-inside-floor-point steps)
        
        screen-holder-right-inside-points-translated
        (screen-holder-points-fn
        screen-holder-top-right-inside-point-translated-for-left-section
        (mapv + [(- (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2)))) (- (+ wall-thickness wall-xy-offset (+ (/ oled-post-size 2)))) 0] screen-holder-bottom-right-inside-point)
        screen-holder-bottom-right-inside-floor-point-translated-for-left-section
         steps)

        thumb-bl-tl (wall-brace-polyhedron-curve-points thumb-bl-place -1 0 "tl"  :degrees steps)
        thumb-bl-bl (wall-brace-polyhedron-curve-points thumb-bl-place -1 0 "bl"  :degrees steps)
        thumb-br-tl (wall-brace-polyhedron-curve-points thumb-br-place -1 0 "tl"  :degrees steps)
        
        outside-points (into [] (apply concat
                                       (for [index (range 0 (inc steps))
                                             :let [thumb-bl-tthumb-bl-to-tps-65-bottom-leftside (thumb-bl-tl :outer-points)
                                                   thumb-bl-bthumb-bl-to-tps-65-bottom-leftside (thumb-br-tl :outer-points)
                                                   thumb-bl-bl-to-tps-65-bottom-leftside (thumb-bl-bl :outer-points)]]
                                         (catmull-rom-spline-segment
                                          (nth thumb-bl-bthumb-bl-to-tps-65-bottom-leftside index)
                                          (nth thumb-bl-tthumb-bl-to-tps-65-bottom-leftside index)
                                          (nth screen-holder-right-outside-points index)
                                          (nth screen-holder-right-outside-points-translated index)
                                          steps
                                          :alpha-type :centripetal 
                                          ;:t1 0.7 :t2 0.9
                                          ))))

        inside-points (into [] (apply concat
                                      (for [index (range 0 (inc steps))
                                            :let [thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside  (reverse (thumb-bl-tl :inner-points))
                                                  thumb-br-tthumb-bl-tr-to-tps-65-bottom-left-innerside  (reverse (thumb-br-tl :inner-points))
                                                     thumb-bl-bl-to-tps-65-bottom-left-innerside (reverse (thumb-bl-bl :inner-points))
                                                  screen-holder-right-inside-points-reversed (reverse screen-holder-right-inside-points)
                                                  screen-holder-left-inside-points-reversed (reverse screen-holder-left-inside-points)
                                                  screen-holder-right-inside-points-translated-reversed (reverse screen-holder-right-inside-points-translated)]]
                                        (catmull-rom-spline-segment
                                         (nth thumb-br-tthumb-bl-tr-to-tps-65-bottom-left-innerside index)
                                         (nth thumb-bl-tthumb-bl-tr-to-tps-65-bottom-left-innerside index)
                                         (nth screen-holder-right-inside-points-reversed index)
                                         (nth screen-holder-right-inside-points-translated-reversed index)
                                         steps
                                         :alpha-type :centripetal 
                                         ;:t1 0.7 :t2 0.9
                                         ;:t1 0.7 :t2 0.9
                                         ))))
        wall-polyhedron (generate-bezier-along-bezier-polyhedron outside-points inside-points steps)
        bottom-points-for-plate (filter #(= (nth % 2) 0.0) outside-points)

        collect-fn (get-collect-fn bottom-plate)]
    
    (collect-fn
     (if bottom-plate bottom-points-for-plate wall-polyhedron)
    ;;  (wall-brace-quadratic-fn
    ;;   tps-65-top-left 0 1 "centre" :degrees
    ;;   thumb-bl-place  0 2 "tr" :degrees
    ;;   thumb-bl-place  0 1 "tr" :degrees
    ;;   steps)
    ;;  (wall-brace-catmull-rom-spline-fn 
    ;;   {:place screen-holder-top-left :dx 0 :dy 0 :post-position :centre :rad-or-deg :degrees :xy wall-xy-offset}
    ;;   {:place screen-holder-top-right :dx -1 :dy 0 :post-position :centre :rad-or-deg :degrees :xy wall-xy-offset}
    ;;   {:place thumb-bl-place :dx -1 :dy 0 :post-position "tl" :rad-or-deg :degrees :xy wall-xy-offset}
    ;;   {:place thumb-bl-place :dx -1 :dy 0 :post-position "bl" :rad-or-deg :degrees :xy wall-xy-offset}
    ;;    steps
    ;;   :alpha-type :centripetal :t1 0.7 :t2 0.9

    ;;   )
    ;;  (wall-brace-polyhedron-fn tps-65-top-left -1 0 "centre" :degrees thumb-bl-place -1 0 "tl" :degrees steps)
    ;;  (wall-brace-quadratic-fn
    ;;  thumb-bl-place 0 1 "tl" :degrees
    ;;  thumb-bl-place -1 1 "tl" :degrees
    ;;  thumb-bl-place -1 0 "tl" :degrees
    ;;  steps)
     )))


(defn thumb-to-body-connecters-polyhedron [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [thumb-tr-mr-top (transform-position thumb-tr-place (get-web-post-position-top (mapv +  (get-single-plate-corner-position-vector "rm"))))
        thumb-tr-mr-bottom (transform-position thumb-tr-place (get-web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "rm"))))
        thumb-tr-tr-top (transform-position thumb-tr-place (get-web-post-position-top (mapv +  (get-single-plate-corner-position-vector "tr"))))
        thumb-tr-tr-bottom (transform-position thumb-tr-place (get-web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "tr"))))
        thumb-tr-tl (web-post-point (partial thumb-tr-place) "tl" :degrees)
        thumb-tr-rm-to-tr-web-post (web-post-linear thumb-tr-place "rm" :degrees thumb-tr-place "tr" :degrees steps)
        index-br-top (transform-position-radians (partial key-place 1 cornerrow) (get-web-post-position-top (mapv +  (get-single-plate-corner-position-vector "br"))))
        index-br-bottom (transform-position-radians (partial key-place 1 cornerrow) (get-web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "br"))))
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
        thumb-tr-tr-curve-outside-double-steps (bezier-quintic
                                   (thumb-tr-tr-points :web-post-position-top)
                                   (thumb-tr-tr-points :wall-locate1-point)
                                   (thumb-tr-tr-points :wall-locate-1-to-3-curve-for-polyhedron-control-point)
                                   (thumb-tr-tr-points :wall-locate-1-to-3-curve-for-polyhedron-second-control-point)
                                   (thumb-tr-tr-points :wall-locate3-point)
                                   (thumb-tr-tr-points :wall-locate3-point-floor)
                                   (* steps 2))
        thumb-tr-tr-curve-inside-adjusted (bezier-cubic
                                           (mapv + (thumb-tr-tr-points :web-post-position-bottom) [0 0 (/ web-thickness -1)])
                                           (thumb-tr-tr-points :wall-locate-2-top)
                                           (thumb-tr-tr-points :wall-locate-2-bottom)
                                           (thumb-tr-tr-points :wall-locate-2-bottom-floor)
                                           steps)
        thumb-tr-tr-curve-inside-adjusted-double-steps (bezier-cubic
                                            (thumb-tr-tr-points :web-post-position-bottom)
                                           (thumb-tr-tr-points :wall-locate-2-top)
                                           (thumb-tr-tr-points :wall-locate-2-bottom)
                                           (thumb-tr-tr-points :wall-locate-2-bottom-floor)
                                           (* steps 2))
        thumb-tr-tr-curve (wall-brace-polyhedron-curve-points thumb-tr-place 1 0 "tr" :degrees wall-xy-offset-thin steps)
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
                                      :outside-upper-control-point-vector  [0 2 10]
                                      :inside-upper-control-point-vector [0 -0.5 12]
                                      )
        thumb-tr-rm-to-index-polyhedron (generate-bezier-along-bezier-polyhedron-from-points-list-linear
                                         thumb-tr-to-index-br-top (thumb-tr-mr-curve :outer-points)
                                         thumb-tr-to-index-br-bottom (thumb-tr-mr-curve :inner-points)
                                         steps)
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

        thumb-tr-to-index-false-wall-brace-plyhedron (generate-bezier-along-bezier-polyhedron-from-points-list-linear
                                                      (reverse (index-br-curve :outer-points)) (reverse (index-br-curve :inner-points))
                                                      thumb-tr-tr-curve-outside  thumb-tr-tr-curve-inside-adjusted 
                                                      steps)
             ss (generate-bezier-along-bezier-polyhedron-from-points-list-linear
                ;(reverse ((wall-brace-polyhedron-curve-points (partial key-place 1 cornerrow) 1 -0.1 "br" :radians wall-xy-offset-medium-thin (* steps 2)) :inner-points))
                (concat (drop-last (reverse (index-br-curve :inner-points)))
                        (bezier-linear  index-br-bottom  thumb-tr-tr-bottom steps))
                (reverse thumb-tr-tr-curve-inside-adjusted-double-steps)
                (concat (drop-last (bezier-quadratic thumb-tr-tr-top (calculate-point-between-points thumb-tr-tr-top  index-br-top [0 2 10]) index-br-top steps))
                        (index-br-curve :outer-points)) thumb-tr-tr-curve-outside-double-steps
                (* steps 2))
        ;;(generate-bezier-along-bezier-polyhedron-from-points-list-linear
            ;;     ;(reverse ((wall-brace-polyhedron-curve-points (partial key-place 1 cornerrow) 1 -0.1 "br" :radians wall-xy-offset-medium-thin (* steps 2)) :inner-points))
            ;;      (concat (drop-last (reverse (index-br-curve :inner-points))) 
            ;;              (bezier-quadratic  index-br-bottom (calculate-point-between-points thumb-tr-tr-bottom index-br-bottom [0 0 0]) thumb-tr-tr-bottom steps))
            ;;     (reverse thumb-tr-tr-curve-inside-adjusted-double-steps)
            ;;     (concat (drop-last (bezier-quadratic thumb-tr-tr-top (calculate-point-between-points thumb-tr-tr-top  index-br-top [0 2 12]) index-br-top steps))
            ;;             (index-br-curve :outer-points)) thumb-tr-tr-curve-outside-double-steps
            ;;     (* steps 2))
        ;; (polyhedron  (into [](concat 
        ;;                              (index-br-curve :outer-points)
        ;;                              thumb-tr-tr-curve-outside-double-steps 
        ;;                      (drop-last (bezier-quadratic thumb-tr-tr-top (calculate-point-between-points thumb-tr-tr-top  index-br-top [0 2 0]) index-br-top steps))
        ;;                      ))
        ;;         (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc (* steps 2)) (inc (* steps 2)) (* steps 2)))                                          


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
                                              (last (thumb-tr-mr-curve :outer-points)))
        inner-index-bl-to-br-thumb-tr-tl-to-thumb-tl-tr-polyhedron (generate-bezier-along-bezier-polyhedron-from-points-linear
                                                                    (inner-index-br-point :top) (thumb-tr-tl :top)
                                                                    (inner-index-bl-point :top) (thumb-tl-tr :top)
                                                                    (thumb-tr-tl :bottom) (inner-index-br-point :bottom)
                                                                    (thumb-tl-tr :bottom) (inner-index-bl-point :bottom)
                                                                    steps)
        polyhedrons (union

                    ;thumb-tr-to-index-false-wall-brace-plyhedron
                     
                     thumb-tr-to-index-polyhedron 
                     ss



                     index-bl-to-inner-br-to-thumb-tr-tl-polyhedron
                     inner-index-bl-to-br-thumb-tr-tl-to-thumb-tl-tr-polyhedron
                     )]
    (if (true? bottom-plate) thumb-tr-to-index-false-wall-brace-floor-points polyhedrons)))

(defn thumb-connecters-polyhedron [steps]
  (let [thumb-mr-br-to-thumb-tr-br-curve   (web-post-quadratic-curve
                                          thumb-mr-place "br" :degrees
                                          thumb-mr-place "tr" :degrees
                                          thumb-tr-place "br" :degrees
                                          steps)
        thumb-bl-tl-to-br-tl-curve-top (catmull-rom-spline-segment
         ((web-post-point thumb-br-place  "bl" :degrees) :top)
                                        ((web-post-point thumb-br-place  "tl" :degrees) :top)
                                        ((web-post-point thumb-bl-place  "tl" :degrees) :top) 
         screen-holder-top-right-outside-point
         steps
         :alpha-type :centripetal)
        thumb-bl-tl-to-br-tl-curve-bottom (catmull-rom-spline-segment
                                              ((web-post-point thumb-br-place  "bl" :degrees) :bottom)
                                              ((web-post-point thumb-br-place  "tl" :degrees) :bottom) 
                                           ((web-post-point thumb-bl-place  "tl" :degrees) :bottom)   
                                           screen-holder-top-right-inside-point
                                              steps
                                              :alpha-type :centripetal)
        thumb-mr-tr-top (transform-position thumb-mr-place (get-web-post-position-top (mapv +  (get-single-plate-corner-position-vector "tr"))))
        thumb-mr-tr-bottom (transform-position thumb-mr-place (get-web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "tr"))))
        thumb-bl-tl-to-br-tl-curve (wall-brace-cubic-polyhedron-curves (get-thumb-mr-br-to-thumb-tr-br-parameters-old steps)) 
        ;; (web-post-quadratic-curve  thumb-bl-place "tl" :degrees
        ;;                                                       thumb-bl-place "bl" :degrees
        ;;                                                       thumb-br-place "tl" :degrees
        ;;                                                       steps)
        thumb-bl-bl-top (transform-position thumb-bl-place (get-web-post-position-top (mapv +  (get-single-plate-corner-position-vector "bl"))))
        thumb-bl-bl-bottom (transform-position thumb-bl-place (get-web-post-position-bottom (mapv +  (get-single-plate-corner-position-vector "bl"))))
        curve-from-thumb-br-bl-to-mr-br-points (wall-brace-cubic-polyhedron-curves (points-for-curved-wall-from-thumb-br-bl-to-mr-br 60))
thumb-br-bl-to-br (web-post-linear thumb-br-place "bl" :degrees thumb-br-place "br" :degrees 12)
thumb-br-br-to-mr-bl (web-post-linear thumb-br-place "br" :degrees thumb-mr-place "bl" :degrees 12)
thumb-mr-bl-to-br (web-post-linear thumb-mr-place "bl" :degrees thumb-mr-place "br" :degrees 12)
thumb-bl-to-mr-linear-top (concat (drop-last (thumb-br-bl-to-br :top))
                                  (drop-last (thumb-br-br-to-mr-bl :top))
                                  (thumb-mr-bl-to-br :top))
thumb-bl-to-mr-linear-bottom  (concat
                               (drop-last (thumb-mr-bl-to-br :bottom))
                               (drop-last (thumb-br-br-to-mr-bl :bottom))
                               (thumb-br-bl-to-br :bottom))
        ] (union
(chained-hull-to-points
  (plot-bezier-points (thumb-mr-br-to-thumb-tr-br-curve :top) (sphere 0.001))
 (translate thumb-mr-tr-top (sphere 0.001))
 (plot-bezier-points (reverse (thumb-mr-br-to-thumb-tr-br-curve :bottom)) (sphere 0.001))
 (translate thumb-mr-tr-bottom (sphere 0.001))
 steps
 )
          ;;  (generate-bezier-to-point-polyhedron
          ;;   (reverse thumb-bl-tl-to-br-tl-curve-top) thumb-bl-bl-top
          ;;   (reverse thumb-bl-tl-to-br-tl-curve-bottom) thumb-bl-bl-bottom 
          ;;   )
            (chained-hull-to-points 
             (plot-bezier-points thumb-bl-tl-to-br-tl-curve-top (sphere 0.001))
 (translate thumb-bl-bl-top (sphere 0.001))
 (plot-bezier-points thumb-bl-tl-to-br-tl-curve-bottom (sphere 0.001))
 (translate thumb-bl-bl-bottom (sphere 0.001))
             steps
            )
                                                                                                                                                    ;;  (generate-bezier-to-point-polyhedron
                                                                                                                                                    ;;   (take 35 (thumb-mr-br-to-thumb-tr-br-curve :top))
                                                                                                                                                    ;;   thumb-mr-tr-top
                                                                                                                                                    ;;   (reverse (take-last 35 (thumb-mr-br-to-thumb-tr-br-curve :bottom)))
                                                                                                                                                    ;;   thumb-mr-tr-bottom)
                                                                                                                                                     
;;            (chained-hull-to-points 
;;             (plot-bezier-points (thumb-bl-tl-to-br-tl-curve :web-post-top-curve) (sphere 0.001))
;; (translate thumb-bl-bl-top (sphere 0.001))
;; (plot-bezier-points (reverse (thumb-bl-tl-to-br-tl-curve :web-post-bottom-curve)) (sphere 0.001))
;; (translate thumb-bl-bl-bottom (sphere 0.001))
;;             steps
;;             )
                                                                                                                                                    ;;  (generate-bezier-to-point-polyhedron
                                                                                                                                                    ;;   (thumb-bl-tl-to-br-tl-curve :web-post-top-curve)
                                                                                                                                                    ;;   thumb-bl-bl-top
                                                                                                                                                    ;;   (reverse (thumb-bl-tl-to-br-tl-curve :web-post-bottom-curve))
                                                                                                                                                    ;;   thumb-bl-bl-bottom)

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
                                                                                                                                                     
                                                                                                                                                    ;;  (chained-hull-for-four-lists
                                                                                                                                                    ;;   (plot-bezier-points
                                                                                                                                                    ;;    thumb-bl-to-mr-linear-top
                                                                                                                                                    ;;    (sphere 0.001))
                                                                                                                                                    ;;   (plot-bezier-points
                                                                                                                                                    ;;    (curve-from-thumb-br-bl-to-mr-br-points :web-post-top-curve)
                                                                                                                                                    ;;    (sphere 0.001))
                                                                                                                                                    ;;   (plot-bezier-points
                                                                                                                                                    ;;    (reverse thumb-bl-to-mr-linear-bottom)
                                                                                                                                                    ;;    (sphere 0.001))
                                                                                                                                                    ;;   (plot-bezier-points
                                                                                                                                                    ;;    (reverse (curve-from-thumb-br-bl-to-mr-br-points :web-post-bottom-curve))
                                                                                                                                                    ;;    (sphere 0.001))
                                                                                                                                                    ;;   60)
                                                                                                                                                     
                                                                                                                                                     )))


(defn key-web-connecters-polyhedron [steps]
  (let [column-fn (fn [column row]
                    (generate-polyhedron-key-web-connecters
                     (partial key-place column row) "tr" (partial key-place (inc column) row) "tl"
                     (partial key-place column row) "br" (partial key-place (inc column) row) "bl"
                     :steps steps))
        column-connections (for [column (range 0 lastcol)
                                 row (range 0 (dec lastrow))]
                             (column-fn column row))
        bottom-row-connections [(column-fn 0 2) (column-fn 2 2) (column-fn 3 2)
                                (generate-polyhedron-key-web-connecters
                                 (partial key-place 1 2) "tr" (partial key-place 2 2) "tl"
                                 (partial key-place 2 2) "bl" (partial key-place 2 2) "bl"
                                 :steps steps)
                                (generate-polyhedron-key-web-connecters
                                 (partial key-place 1 2) "tr" (partial key-place 2 2) "bl"
                                 (partial key-place 1 2) "br" (partial key-place 1 2) "br"
                                 :steps steps)]
        diagonal-connections (for [column (range 0 lastcol)
                                   row (range 0 cornerrow)]
                               (generate-polyhedron-key-web-connecters
                                (partial key-place column row) "br" (partial key-place (inc column) row) "bl"
                                (partial key-place column (inc row)) "tr" (partial key-place (inc column) (inc row)) "tl"
                                :steps steps))

        row-connections (for [column columns
                              row (range 0 cornerrow)]
                          (generate-polyhedron-key-web-connecters
                           (partial key-place column row) "bl" (partial key-place column row) "br"
                           (partial key-place column (inc row)) "tl" (partial key-place column (inc row)) "tr"
                           :steps steps))]

    (apply union
           (concat
            column-connections
            row-connections
            diagonal-connections
            bottom-row-connections))))
(defn right-side-polyhedron [steps]
  (let [key-points-top (apply concat
                              (for [row (range 0 nrows)]
                                [(key-position 0 row (mapv + web-post-translation-vector web-post-tl-translation-vector [0 0 (/ web-thickness 2)]))
                                 (key-position 0 row (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness 2)]))]))
        key-points-bottom (apply concat
                                 (for [row (range 0 nrows)]
                                   [(key-position 0 row (mapv + web-post-translation-vector web-post-tl-translation-vector [0 0 (/ web-thickness -2)]))
                                    (key-position 0 row (mapv + web-post-translation-vector web-post-bl-translation-vector [0 0 (/ web-thickness -2)]))]))
        ;num-slice ()
        top-row-web-post-tl-top (key-position 0 0 (mapv + web-post-translation-vector web-post-tl-translation-vector [0 0 (/ web-thickness 2)]))
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
        total-width (+ tps-65-mount-width tps-65-tolerance)
        max-width-from-zero (/ total-width 2)
        tps-65-mount-width-slice  (/ total-width number-of-slices)
        position-adjustment (fn [upper-or-lower] (mapv + oled-translation-vector
                                                       [0 (+ (tps-radius-compensation-adjust (- tps-65-mount-corner-radius)) (/ oled-post-size 2) -0.1) (/ oled-holder-thickness (cond (= upper-or-lower :upper) 2
                                                                                                                                                                                       (= upper-or-lower :lower) -2))]))
        tps-65-mount-length-adjusted (+ tps-65-mount-length 0.5)
        tps-65-mount-length-adjusted-bottom (- tps-65-mount-length 2)
        tps-65-point-to-connect-to-top-row-web-post-tl-top (transform-position (partial tps-65-translate-and-place-at-position [max-width-from-zero
                                                                                                                                (- tps-65-corner-radius (/ tps-65-mount-length-adjusted 2))
                                                                                                                                (- plate-thickness)])
                                                                               (position-adjustment :upper))
        tps-65-point-to-connect-to-top-row-web-post-bl-top (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-mount-width-slice 4))
                                                                                                                                (- tps-65-corner-radius (/ tps-65-mount-length-adjusted 2))
                                                                                                                                (- plate-thickness)])
                                                                               (position-adjustment :upper))
        tps-65-point-to-connect-to-middle-row-web-post-tl-top (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-mount-width-slice 5))
                                                                                                                                   (- tps-65-corner-radius (/ tps-65-mount-length-adjusted 2))
                                                                                                                                   (- plate-thickness)]) (position-adjustment :upper))
        tps-65-point-to-connect-to-middle-row-web-post-bl-top (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-mount-width-slice 9))
                                                                                                                                   (- tps-65-corner-radius (/ tps-65-mount-length-adjusted 2))
                                                                                                                                   (- plate-thickness)]) (position-adjustment :upper))
        tps-65-point-to-connect-to-third-row-web-post-tl-top (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-mount-width-slice 10))
                                                                                                                                  (- tps-65-corner-radius (/ tps-65-mount-length-adjusted 2))
                                                                                                                                  (- plate-thickness)]) (position-adjustment :upper))
        tps-65-point-to-connect-to-third-row-web-post-bl-top (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-mount-width-slice 14))
                                                                                                                                  (- tps-65-corner-radius (/ tps-65-mount-length 2))
                                                                                                                                  (- plate-thickness)]) (position-adjustment :upper))

        tps-65-point-to-connect-to-top-row-web-post-tl-bottom (transform-position (partial tps-65-translate-and-place-at-position [max-width-from-zero
                                                                                                                                   (- tps-65-corner-radius (/ tps-65-mount-length-adjusted 2))
                                                                                                                                   (- plate-thickness)]) (position-adjustment :lower))
        tps-65-point-to-connect-to-top-row-web-post-bl-bottom (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-mount-width-slice 4))
                                                                                                                                   (- tps-65-corner-radius (/ tps-65-mount-length-adjusted-bottom 2))
                                                                                                                                   (- plate-thickness)]) (position-adjustment :lower))
        tps-65-point-to-connect-to-middle-row-web-post-tl-bottom (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-mount-width-slice 5))
                                                                                                                                      (- tps-65-corner-radius (/ tps-65-mount-length-adjusted-bottom 2))
                                                                                                                                      (- plate-thickness)]) (position-adjustment :lower))
        tps-65-point-to-connect-to-middle-row-web-post-bl-bottom (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-mount-width-slice 6))
                                                                                                                                      (- tps-65-corner-radius (/ tps-65-mount-length-adjusted-bottom 2))
                                                                                                                                      (- plate-thickness)]) (position-adjustment :lower))
        tps-65-point-to-connect-to-third-row-web-post-tl-bottom (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-mount-width-slice 10))
                                                                                                                                     (- tps-65-corner-radius (/ tps-65-mount-length-adjusted-bottom 2))
                                                                                                                                     (- plate-thickness)]) (position-adjustment :lower))
        tps-65-point-to-connect-to-third-row-web-post-bl-bottom (transform-position (partial tps-65-translate-and-place-at-position [(- max-width-from-zero (* tps-65-mount-width-slice 14))
                                                                                                                                     (- tps-65-corner-radius (/ tps-65-mount-length 2))
                                                                                                                                     (- plate-thickness)]) (position-adjustment :lower))

        thumb-tl-tl-web-post-top (transform-position (partial thumb-tl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-tl-tl-web-post-bottom (transform-position (partial thumb-tl-place) (mapv + web-post-tl-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        thumb-tl-tr-web-post-top (transform-position (partial thumb-tl-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness 2)]))
        thumb-tl-tr-web-post-bottom (transform-position (partial thumb-tl-place) (mapv + web-post-tr-translation-vector web-post-translation-vector [0 0 (/ web-thickness -2)]))
        
        
        tt (generate-bezier-along-bezier-polyhedron-from-points-linear
            top-row-web-post-tl-top tps-65-bottom-right-outer
            top-row-web-post-tl-bottom tps-65-bottom-right-inner 
            tps-65-point-to-connect-to-top-row-web-post-tl-top top-row-web-post-tl-top 
            tps-65-point-to-connect-to-top-row-web-post-tl-bottom top-row-web-post-tl-bottom  
            steps                                                     
            ;:outside-lower-control-point-vector [0 0 -1]
            )
        tps-65-to-top-row (generate-bezier-along-bezier-polyhedron-from-points-linear
                           tps-65-point-to-connect-to-top-row-web-post-tl-top top-row-web-post-tl-top
                           tps-65-point-to-connect-to-top-row-web-post-bl-top top-row-web-post-bl-top
                           top-row-web-post-tl-bottom tps-65-point-to-connect-to-top-row-web-post-tl-bottom
                           top-row-web-post-bl-bottom tps-65-point-to-connect-to-top-row-web-post-bl-bottom
                           steps
                           :outside-upper-control-point-vector [0 0 0] :outside-lower-control-point-vector [0 0 -1]
                           :inside-upper-control-point-vector [0 0 0] :inside-lower-control-point-vector [1 0 -1])
        tps-65-to-top-and-middle-row (generate-bezier-along-bezier-polyhedron-from-points-linear
                                      tps-65-point-to-connect-to-top-row-web-post-bl-top top-row-web-post-bl-top
                                      tps-65-point-to-connect-to-middle-row-web-post-tl-top middle-row-web-post-tl-top
                                      top-row-web-post-bl-bottom tps-65-point-to-connect-to-top-row-web-post-bl-bottom
                                      middle-row-web-post-tl-bottom tps-65-point-to-connect-to-middle-row-web-post-tl-bottom
                                      steps
                                      :outside-upper-control-point-vector [0 0 -1] :outside-lower-control-point-vector [0 0 -1]
                                      :inside-upper-control-point-vector [1 0 -1] :inside-lower-control-point-vector [1 0 -1])

        tps-65-to-middle-row (generate-bezier-along-bezier-polyhedron-from-points-linear
                              tps-65-point-to-connect-to-middle-row-web-post-tl-top middle-row-web-post-tl-top
                              tps-65-point-to-connect-to-middle-row-web-post-bl-top middle-row-web-post-bl-top
                              middle-row-web-post-tl-bottom tps-65-point-to-connect-to-middle-row-web-post-tl-bottom
                              middle-row-web-post-bl-bottom tps-65-point-to-connect-to-middle-row-web-post-bl-bottom
                              steps
                              :outside-upper-control-point-vector [0 0 -1] :outside-lower-control-point-vector [0 0 -1]
                              :inside-upper-control-point-vector [1 0 -1] :inside-lower-control-point-vector [0 0 -1])
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
                                             :inside-upper-control-point-vector [0 0 -1] :inside-lower-control-point-vector [0 0 0])
        gap-fill (generate-bezier-along-bezier-polyhedron-from-points-linear
                  tps-65-bottom-right-outer tps-65-point-to-connect-to-top-row-web-post-tl-top
                  tps-65-bottom-left-outer tps-65-bottom-left-outer
                  tps-65-point-to-connect-to-top-row-web-post-tl-bottom tps-65-bottom-right-inner 
                  tps-65-bottom-left-inner tps-65-bottom-left-inner
                  steps
                  )
        ] 
    (union
     tt
     tps-65-to-top-row
     tps-65-to-top-and-middle-row
     tps-65-to-middle-row
     tps-65-to-middle-and-third-row
     tps-65-to-third-row
     gap-fill
     ;tps-65-to-third-row-and-thumb-tl-tl
     )))


(comment(wall-brace-polyhedron tps-65-bottom-right 1 1 :centre :degrees
                          tps-65-top-right 1 0 :centre :degrees
                          30))

(defn left-section-back [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [wall-brace-quadratic-fn (get-wall-brace-quadratic-fn bottom-plate)
        wall-brace-polyhedron-fn (get-wall-brace-polyhedron-fn bottom-plate)
        collect-fn (get-collect-fn bottom-plate)]

    (collect-fn

    ;;  (wall-brace-quadratic-polyhedron (partial key-place 0 0) -1 0 "tl" :radians
    ;;                         (partial key-place 0 0) -1 1 "tl" :radians
    ;;                         (partial key-place 0 0) 0 1 "tl" :radians
    ;;                         steps)

      
     (wall-brace-polyhedron-fn tps-65-bottom-right 1 1 "centre" :degrees
                               tps-65-top-right 1 0 "centre" :degrees
                               steps)
     (wall-brace-quadratic-fn
      tps-65-top-right 1 0 "centre" :degrees
      tps-65-top-right 1 1 "centre" :degrees
      tps-65-top-right 0 1 "centre" :degrees
      steps))))

(defn polyhedron-thumb-walls [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [collect-fn (get-collect-fn bottom-plate)
        thumb-walls-polyhedron-map (thumb-walls-polyhedron-circular steps :bottom-plate bottom-plate)
        thumb-corners-polyhedron-map (thumb-corners-polyhedron-circular steps :bottom-plate bottom-plate)]
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
     (thumb-walls-polyhedron-map :thumb-tr-br-to-tr))))

(defn polyhedron-thumb-walls-for-convex-cluster [steps & {:keys [bottom-plate] :or {bottom-plate false}}]
  (let [collect-fn (get-collect-fn bottom-plate)
        thumb-walls-polyhedron-for-convex-cluster-map (thumb-walls-polyhedron-for-convex-cluster steps :bottom-plate bottom-plate)
        thumb-corners-polyhedron-for-convex-cluster-map (thumb-corners-polyhedron-for-convex-cluster steps :bottom-plate bottom-plate)]
    (collect-fn
     ;(thumb-corners-polyhedron-for-convex-cluster-map :thumb-bl-tl-to-lm-corner)
     ;(thumb-walls-polyhedron-for-convex-cluster-map :thumb-bl-tl-to-lm)
     (thumb-corners-polyhedron-for-convex-cluster-map :thumb-bl-tl-to-br-tl-corner)
     ;(thumb-corners-polyhedron-for-convex-cluster-map :thumb-br-tl-corner)
     (thumb-walls-polyhedron-for-convex-cluster-map :thumb-br-tl-to-bl)
     (thumb-corners-polyhedron-for-convex-cluster-map :thumb-br-bl-corner)
     
     ;(thumb-walls-polyhedron-for-convex-cluster-map :thumb-br-bl-to-br)

    ; (thumb-walls-polyhedron-for-convex-cluster-map :thumb-br-to-mr)

     ;(thumb-walls-polyhedron-for-convex-cluster-map :thumb-mr-bl-to-br)

     (thumb-walls-polyhedron-for-convex-cluster-map :thumb-br-bl-to-mr-br)
     (thumb-corners-polyhedron-for-convex-cluster-map :thumb-mr-br-corner)
     (thumb-corners-polyhedron-for-convex-cluster-map :thumb-mr-to-thumb-tr-corner)
     (thumb-corners-polyhedron-for-convex-cluster-map :thumb-tr-br-corner)
     (thumb-walls-polyhedron-for-convex-cluster-map :thumb-tr-br-to-tr))))

(defn polyhedron-left-section [steps]
  (union 
   (back-left-wall-to-screen steps)
   (left-section-back steps)
   under-screen))

(defn polyhedron-case-walls [steps]
  (union
   (right-wall-polyhedron-catmull-rom-spline steps)
   (front-wall-polyhedron steps)

   (back-wall-polyhedron-catmull-rom steps)))

(defn usb-jack-shape [circle-diameter height steps]
  (let [right-circle-floor-quadrant [(- (/ usb-jack-width 2) (/ usb-jack-height 2)) 0 height]
        left-circle-floor-quadrant [(- (/ usb-jack-height 2) (/ usb-jack-width 2)) 0 height]
        bezier-circle-quadrant #(bezier-quadratic (nth % 0) (nth % 1) (nth % 2) steps)
        top-right-circle-floor-quadrant-coordinates (mapv #(mapv + right-circle-floor-quadrant %) [[0 circle-diameter 0] [circle-diameter circle-diameter 0] [circle-diameter 0 0]])
        bottom-right-circle-floor-quadrant-coordinates (mapv #(mapv + right-circle-floor-quadrant %) [[circle-diameter 0 0] [circle-diameter (- circle-diameter) 0] [0 (- circle-diameter) 0]])
        top-right-circle-floor-quadrant-bezier-curve-points (bezier-quadratic (nth top-right-circle-floor-quadrant-coordinates 0)
                                                                              (nth top-right-circle-floor-quadrant-coordinates 1)
                                                                              (nth top-right-circle-floor-quadrant-coordinates 2)
                                                                              steps)
        top-left-circle-floor-quadrant-coordinates (mapv #(mapv + left-circle-floor-quadrant %) [[(- circle-diameter) 0 0] [(- circle-diameter) circle-diameter 0] [0 circle-diameter 0]])
        bottom-left-circle-floor-quadrant-coordinates (mapv #(mapv + left-circle-floor-quadrant %) [[0 (- circle-diameter) 0] [(- circle-diameter) (- circle-diameter) 0] [(- circle-diameter) 0 0]])
        bottom-right-circle-floor-quadrant-bezier-curve-points (bezier-circle-quadrant bottom-right-circle-floor-quadrant-coordinates)
        top-left-circle-floor-quadrant-bezier-curve-points (bezier-circle-quadrant top-left-circle-floor-quadrant-coordinates)
        bottom-left-circle-floor-quadrant-bezier-curve-points (bezier-circle-quadrant bottom-left-circle-floor-quadrant-coordinates)
        top-linear (bezier-linear (nth top-left-circle-floor-quadrant-coordinates 2) (nth top-right-circle-floor-quadrant-coordinates 0)  steps)
        bottom-linear (bezier-linear (nth bottom-right-circle-floor-quadrant-coordinates 2) (nth bottom-left-circle-floor-quadrant-coordinates 0) steps)]
    (concat
     (drop-last top-right-circle-floor-quadrant-bezier-curve-points)
     (drop-last bottom-right-circle-floor-quadrant-bezier-curve-points)
     (drop-last bottom-linear)
     (drop-last bottom-left-circle-floor-quadrant-bezier-curve-points)
     (drop-last top-left-circle-floor-quadrant-bezier-curve-points)
     top-linear)))


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
                                                steps))))
        cylinder-centre-top-point-index (count cylinder-side-points)
        cylinder-centre-bottom-point-index (inc cylinder-centre-top-point-index)
        cylinder-top-faces (for [index (range 0 (inc steps))]
                             [(* (inc steps) index) cylinder-centre-top-point-index (* (inc steps) (inc index))])
        cylinder-side-faces side-faces
        cylinder-bottom-faces (for [index (range 0 steps)]
                                [(+ steps (* (inc steps) index))  (+ (*  (inc steps) (inc index)) steps) cylinder-centre-bottom-point-index])
        cylinder-points (concat cylinder-side-points [cylinder-centre-top-point cylinder-centre-bottom-point])
        cylinder-faces (concat cylinder-top-faces cylinder-side-faces cylinder-bottom-faces)]
    (translate [0 -2.25 0] (union
                            (translate [0  plate-thickness 0] (rdx 90 (polyhedron points faces)))
                            (translate [0   plate-thickness 0] (rdx 90 (polyhedron cylinder-points cylinder-faces)))))))