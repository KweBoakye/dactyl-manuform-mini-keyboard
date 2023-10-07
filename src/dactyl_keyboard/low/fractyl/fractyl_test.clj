(ns dactyl-keyboard.low.fractyl.fractyl-test
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [include-bosl2]]
            [dactyl-keyboard.low.fractyl.fractyl-bottom-plate :refer :all]
            [dactyl-keyboard.low.fractyl.fractyl-case-walls :refer :all]
            [dactyl-keyboard.low.tps-65-placement-functions :refer [tps-65-place]]
            [dactyl-keyboard.tps-65 :refer [tps-65-model tps-65-overlay]]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]) 
  )

(defn fractyl-test [wall-cross-section-steps
                    wall-section-steps &{:keys [steps side show-aviator-assembly trackpoint-cutout trackpoint-mount
                                                show-trackpad]
                       :or {steps wall-section-steps
                            side :right
                            show-aviator-assembly true
                            trackpoint-cutout false
                            trackpoint-mount false
                            show-trackpad true}}]
  (let [trackpad (union (color [0 1 0 1] (tps-65-place (translate [0 0 -1] tps-65-model)))
                 (tps-65-place (translate [0 0 -1] (color [0 0 0 1]
                                                          tps-65-overlay))))]
    (union
   (fractyl-body wall-cross-section-steps wall-section-steps :steps steps
                 :side side :show-aviator-assembly show-aviator-assembly
                 :trackpoint-cutout trackpoint-cutout)
   (fractyl-bottom-plate-and-mounts wall-cross-section-steps wall-section-steps :side side
                                    :trackpoint-mount trackpoint-mount)
   (cond show-trackpad trackpad)
   ))
  )


(spit "things-low/fractyl-right-test.scad"
      (write-scad
       (include include-bosl2)
       (fractyl-test 10 10 :show-aviator-assembly true
                     :trackpoint-cutout true
                     :trackpoint-mount true
                     :show-trackpad false) 
       )
      )

(spit "things-low/fractyl-left-test.scad"
      (write-scad
       (include include-bosl2)
       
        (fractyl-body 10 10 :side :left)
        (fractyl-bottom-plate-and-mounts 10 10 :side :left)
       ))