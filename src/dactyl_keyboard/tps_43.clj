(ns dactyl-keyboard.tps-43
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [include-bosl2]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.shapes-3d :refer [cuboid]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer :all]))

(def tps-43-width 43)
(def tps-43-length 40)
(def tps-43-mount-width (+ tps-43-width 2))
(def tps-43-mount-length (+ tps-43-length 2))
(def tps-43-corner-radius 3.5)
(def tps-43-depth 2.03)
(def tps-43-corner-cyl (translate [0 0 (- tps-43-depth)] (binding [*fn* 36] (cylinder tps-43-corner-radius (+ tps-43-depth 0.05) :center false))))
(def tps-43-cutout-length (+ tps-43-length 0.4))
(def tps-43-cutout-width (+ tps-43-width 0.4))
(def tps-43-full-cutout-length (- tps-43-length 2))
(def tps-43-full-cutout-width (- tps-43-width 2))
(def tps-43-cutout-x-translate (- (/ tps-43-cutout-width 2) tps-43-corner-radius))
(def tps-43-cutout-y-translate (- (/ tps-43-cutout-length 2) tps-43-corner-radius))
(def tps-43-full-cutout-x-translate (- (/ tps-43-full-cutout-width 2) tps-43-corner-radius))
(def tps-43-full-cutout-y-translate (- (/ tps-43-full-cutout-length 2) tps-43-corner-radius))
(defn tps-43-cutout-with-height [height] 
  (let [corner (binding [*fn* 36](circle tps-43-corner-radius))]
    (extrude-linear {:height height :center false}(hull (translate [tps-43-cutout-x-translate tps-43-cutout-y-translate ] corner)
          (translate [(- tps-43-cutout-x-translate) tps-43-cutout-y-translate ] corner)
          (translate [tps-43-cutout-x-translate (- tps-43-cutout-y-translate) ] corner)
          (translate [(- tps-43-cutout-x-translate) (- tps-43-cutout-y-translate) ] corner)))))
(def tps-43-cutout (hull (translate [tps-43-cutout-x-translate tps-43-cutout-y-translate 0] tps-43-corner-cyl)
                         (translate [(- tps-43-cutout-x-translate) tps-43-cutout-y-translate 0] tps-43-corner-cyl)
                         (translate [tps-43-cutout-x-translate (- tps-43-cutout-y-translate) 0] tps-43-corner-cyl)
                         (translate [(- tps-43-cutout-x-translate) (- tps-43-cutout-y-translate) 0] tps-43-corner-cyl)))
(def tps-43-full-cutout (->> (hull (translate [tps-43-full-cutout-x-translate tps-43-full-cutout-y-translate 0] tps-43-corner-cyl)
                                   (translate [(- tps-43-full-cutout-x-translate) tps-43-full-cutout-y-translate 0] tps-43-corner-cyl)
                                   (translate [tps-43-full-cutout-x-translate (- tps-43-full-cutout-y-translate) 0] tps-43-corner-cyl)
                                   (translate [(- tps-43-full-cutout-x-translate) (- tps-43-full-cutout-y-translate) 0] tps-43-corner-cyl))
                             (translate [0 0 -1.05])))

(defn tps-43-mount-body [&{:keys [fn] :or {fn 8}}] (->> (cuboid [tps-43-mount-width tps-43-mount-length (+ tps-43-depth 1)] :rounding 2
                                           ;:edges [BOTTOM]
                                    :except [TOP]
                                    :trim-corners true  :fn fn)
                            (translate [0 0 (- (+ (/ tps-43-depth 2) 0.5))])))

(defn tps-43-mount [&{:keys [fn] :or {fn 8}}] (difference (tps-43-mount-body :fn fn)
                              tps-43-cutout
                              tps-43-full-cutout))