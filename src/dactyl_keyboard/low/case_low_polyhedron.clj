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
            [dactyl-keyboard.low.case-low-polyhedron-functions :refer :all] 
            [dactyl-keyboard.low.thumbs-low :refer :all]
            [dactyl-keyboard.low.oled-low-placements :refer :all]
            [dactyl-keyboard.oled :refer :all]
            [dactyl-keyboard.tps-65 :refer :all]
            [dactyl-keyboard.EVQWGD001 :refer :all]))

(defn back-left-wall-to-screen [{:keys [dx1 dy1 place1  post-position-1
                                        dxmid1 dymid1 place-mid1  post-position-mid1
                                        dxmid2 dymid2 place-mid2  post-position-mid2
                                        dx2 dy2 place2  post-position-2
                                        steps] :or {steps 20}}]
  (let [screen-holder-top-left-outside-point (transform-position
                                              (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                              (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
        screen-holder-top-left-inside-point (transform-position
                                             (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                             (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))

        screen-holder-top-left-inside-point-translated (transform-position
                                                        (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                                        (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 1)]))
        screen-holder-bottom-left-outside-point (transform-position
                                                 (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                                 (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
        screen-holder-bottom-left-inside-point (transform-position
                                                (partial screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                                (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
        tps-65-top-right-web-post-outside-point (transform-position
                                                 (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset)
                                                 [0 0 (/ web-thickness 2)])
        screen-holder-bottom-left-outside-floor-point (assoc (vec screen-holder-bottom-left-outside-point) 2 0)
        screen-holder-bottom-left-inside-floor-point (assoc (vec screen-holder-bottom-left-inside-point) 2 0)
        screen-holder-top-right-outside-point (transform-position
                                               (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (+ (/ screen-holder-depth 2)))
                                               (mapv + [0 0 0] [0 0 (/ oled-holder-thickness 2)]))
        screen-holder-top-right-inside-point (transform-position
                                              (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                              (mapv +  [0 0 (/ oled-holder-thickness 2)]))
        screen-holder-top-right-inside-point-translated (transform-position
                                                         (partial screen-holder-translate-and-place-side (/ (+ screen-holder-height) 2) (/ (+ screen-holder-width) 2)  (- (/ screen-holder-depth 2)))
                                                         (mapv +  [0 0 (/ oled-holder-thickness 1)]))
        tps-65-top-right-web-post-inside-point (tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset [0 0 (- (/ web-thickness))])
        outside-vertical-curve-origin [0 0 (/ curve-post-size 2)]
        tps-65-top-right-wall-locate3-outside-point (transform-position
                                                     (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset)
                                                     (mapv + (wall-locate3-xy-for-polyhedron-point 1 0 wall-xy-offset) [(/ curve-post-size 2) (/ curve-post-size 1) 0]
                                                           curve-post-translation-vector))
        tps-65-top-right-wall-locate3-outside-control-point (transform-position
                                                             (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset)
                                                             (mapv + (wall-locate3-xy-for-polyhedron-point 0 1 wall-xy-offset) [0 (/ curve-post-size 1)  0] curve-post-translation-vector))
        tps-65-top-right-wall-locate2-inside-point (transform-position
                                                    (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset)
                                                    (mapv + (wall-locate2-xy 1 0 wall-xy-offset) [(/ curve-post-size -1) curve-post-size  0] oled-translation-vector))
        tps-65-top-right-wall-locate2-inside-point-translated (transform-position
                                                               (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset)
                                                               (mapv + (wall-locate2-xy 1 0 wall-xy-offset) [0 0 (/ oled-holder-thickness 2)] [(/ curve-post-size -1) curve-post-size  0] oled-translation-vector))
        tps-65-top-right-wall-locate2-inside-control-point (transform-position
                                                            (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset)
                                                            (mapv + (wall-locate2-xy 0 1 wall-xy-offset) [(/ curve-post-size -1) curve-post-size 0] oled-translation-vector))
        tps-65-top-right-wall-locate2-inside-control-point-translated (transform-position
                                                                       (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset  tps-65-mount-corner-radius-with-offset)
                                                                       (mapv + (wall-locate2-xy 0 1 wall-xy-offset) [0 0 (/ oled-holder-thickness 2)] [(/ curve-post-size -1) curve-post-size 0] oled-translation-vector))
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
        tps-65-top-left-outer    (transform-position
                                  (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius)  tps-65-mount-corner-radius)
                                  (mapv + oled-translation-vector [(/ oled-post-size 2) (/ oled-post-size 2) (/ oled-holder-thickness 2)]))
        tps-65-top-middle-outer    (transform-position
                                    (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-centre-position 0  tps-65-mount-corner-radius)
                                    (mapv + oled-translation-vector [0 (/ oled-post-size 2) (/ oled-holder-thickness 2)]))

        tps-65-top-middle-inner    (transform-position
                                    (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-centre-position 0  tps-65-mount-corner-radius)
                                    (mapv + oled-translation-vector [0 (/ oled-post-size 2) (/ oled-holder-thickness -2)]))
        tps-65-top-right-outer    (transform-position
                                   (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position  tps-65-mount-corner-radius  tps-65-mount-corner-radius)
                                   (mapv + oled-translation-vector [(/ oled-post-size -2) (/ oled-post-size 2) (/ oled-holder-thickness 2)]))
        tps-65-top-right-to-top-left-outer (bezier-linear  tps-65-top-right-outer tps-65-top-left-outer  (* steps 2))
        tps-65-top-left-control-point-outer (transform-position
                                             (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset)
                                             (mapv + oled-translation-vector [(/ oled-post-size -2) (/ oled-post-size -2) (/ oled-holder-thickness 2)]))

        tps-65-top-right-control-point-outer (transform-position
                                              (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position  tps-65-mount-corner-radius-with-offset-mod  tps-65-mount-corner-radius-with-offset)
                                              (mapv + oled-translation-vector [(/ oled-post-size -2) (/ oled-post-size 2) (/ oled-holder-thickness 2)]))
        tps-65-top-middle-control-point-outer (transform-position
                                               (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-centre-position  0  tps-65-mount-corner-radius-with-offset)
                                               (mapv + oled-translation-vector [0 (/ oled-post-size 2) (/ oled-holder-thickness 2)]))
        tps-65-top-middle-control-point-inner (transform-position
                                               (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-centre-position  0  tps-65-mount-corner-radius-with-offset)
                                               (mapv + oled-translation-vector [0 (/ oled-post-size 2) (/ oled-holder-thickness -2)]))
        tps-65-screen-side-upper-control-points (bezier-linear tps-65-top-right-control-point-outer tps-65-top-left-control-point-outer  (* steps 2))
        tps-65-top-right-wall-to-screen-holder-top-right-points-outer (concat top-bezier-points (reverse (drop 1 (bezier-linear  screen-holder-top-right-outside-point screen-holder-top-left-outside-point steps))))

        tps-65-top-left-inner    (transform-position
                                  (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius)  tps-65-mount-corner-radius)
                                  (mapv + oled-translation-vector [(/ oled-post-size 2) (/ oled-post-size 2) (/ oled-holder-thickness -2)]))
        tps-65-top-right-inner    (transform-position
                                   (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position  tps-65-mount-corner-radius  tps-65-mount-corner-radius)
                                   (mapv + oled-translation-vector [(/ oled-post-size -2) (/ oled-post-size 2) (/ oled-holder-thickness -2)]))
        tps-65-top-right-to-top-left-inner (bezier-linear tps-65-top-left-inner tps-65-top-right-inner  (inc (* steps 2)))
        tps-65-top-left-control-point-inner (transform-position
                                             (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-left-position (- tps-65-mount-corner-radius-with-offset-mod)  tps-65-mount-corner-radius-with-offset)
                                             (mapv + oled-translation-vector [(/ oled-post-size -2) (/ oled-post-size -2) (/ oled-holder-thickness -2)]))

        tps-65-top-right-control-point-inner (transform-position
                                              (partial tps-65-translate-and-place-with-radius tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-radius-with-offset-mod  tps-65-mount-corner-radius-with-offset)
                                              (mapv + oled-translation-vector [(/ oled-post-size -2) (/ oled-post-size 2) (/ oled-holder-thickness -2)]))
        tps-65-screen-side-inner-control-points (bezier-linear tps-65-top-left-control-point-inner tps-65-top-right-control-point-inner (inc (* steps 2)))
        tps-65-top-right-wall-to-screen-holder-top-right-points-inner (concat top-inside-bezier-points (bezier-linear screen-holder-top-left-inside-point screen-holder-top-right-inside-point steps))
        tps-65-screen-side-bezier-along-bezier-point-outer (into []
                                                                 (apply concat
                                                                        (for [index (range 0 (inc steps))]
                                                                          (bezier-cubic
                                                                           (nth (bezier-linear tps-65-top-right-outer tps-65-top-middle-outer steps) index)
                                                                           (nth (bezier-linear tps-65-top-right-control-point-outer tps-65-top-middle-control-point-outer steps) index)
                                                                           (nth (bezier-quadratic tps-65-top-right-wall-locate2-inside-point-translated
                                                                                                  tps-65-top-right-wall-locate2-inside-control-point-translated
                                                                                                  screen-holder-top-left-inside-point-translated
                                                                                                  steps) index)
                                                                           (nth top-bezier-points index)
                                                                           steps))))

        tps-65-screen-side-bezier-along-bezier-point-inner (into []
                                                                 (apply concat
                                                                        (for [index (range 0 (inc steps))]
                                                                          (bezier-quadratic
                                                                           (nth (bezier-linear tps-65-top-middle-inner tps-65-top-right-inner  steps) index)
                                                                           (nth (bezier-linear tps-65-top-middle-control-point-inner tps-65-top-right-control-point-inner  steps) index)
                                                                           (nth top-inside-bezier-points index)
                                                                           steps))))

        tps-65-screen-side-bezier-along-bezier-middle-to-right-outer-points (into []
                                                                                  (apply concat
                                                                                         (for [index (range 0 (inc steps))]
                                                                                           (bezier-cubic
                                                                                            (nth (bezier-linear  tps-65-top-middle-outer tps-65-top-left-outer steps) index)
                                                                                            (nth (bezier-linear  tps-65-top-middle-control-point-outer tps-65-top-left-control-point-outer steps) index)
                                                                                            (nth (bezier-linear screen-holder-top-left-inside-point-translated
                                                                                                                screen-holder-top-right-inside-point-translated
                                                                                                                steps) index)
                                                                                            (nth (bezier-linear screen-holder-top-left-outside-point screen-holder-top-right-outside-point steps) index)
                                                                                            steps))))

        tps-65-screen-side-bezier-along-bezier-middle-to-right-inner-points (into []
                                                                                  (apply concat
                                                                                         (for [index (range 0 (inc steps))
                                                                                               :let [tps-65-points (bezier-linear tps-65-top-left-inner  tps-65-top-middle-inner   steps)
                                                                                                     control-points (bezier-linear tps-65-top-left-control-point-inner tps-65-top-middle-control-point-inner   steps)
                                                                                                     screen-holder-inner-points (bezier-linear screen-holder-top-right-inside-point screen-holder-top-left-inside-point steps)]]
                                                                                           (bezier-quadratic
                                                                                            (nth tps-65-points index)
                                                                                            (nth control-points index)
                                                                                            (nth screen-holder-inner-points index)
                                                                                            steps))))
        tps-65-screen-side-bezier-along-bezier-polyhedron (polyhedron (concat tps-65-screen-side-bezier-along-bezier-point-outer tps-65-screen-side-bezier-along-bezier-point-inner)
                                                                      (generate-bezier-along-bezier-polyhedron-faces
                                                                       tps-65-screen-side-bezier-along-bezier-point-outer
                                                                       tps-65-screen-side-bezier-along-bezier-point-inner steps))
        tps-65-screen-side-bezier-along-bezier-middle-to-right-polyhedron (polyhedron (concat tps-65-screen-side-bezier-along-bezier-middle-to-right-outer-points tps-65-screen-side-bezier-along-bezier-middle-to-right-inner-points)
                                                                                      (generate-bezier-along-bezier-polyhedron-faces
                                                                                       tps-65-screen-side-bezier-along-bezier-middle-to-right-outer-points
                                                                                       tps-65-screen-side-bezier-along-bezier-middle-to-right-inner-points
                                                                                       steps))
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
        ]
    ;(println top-bezier-points)
    (union
     ;top-upper-curve-polyhedron
      ;; (for [index (range 0 20)]
      ;;   (plot-bezier-points (bezier-fn index) (convert-to-curve-post oled-post)))
     (plot-bezier-points  (bezier-linear tps-65-top-right-control-point-outer tps-65-top-middle-control-point-outer steps) curve-post)
     ;(cube 5 5 5)
     tps-65-screen-side-bezier-along-bezier-polyhedron
     tps-65-screen-side-bezier-along-bezier-middle-to-right-polyhedron
     wall-curve
     (-# (hull
          (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (+ screen-holder-width) 2)  0 oled-post)
          (screen-holder-translate-and-place-side (/ (- screen-holder-height) 2) (/ (- screen-holder-width) 2)  0 oled-post))))))

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