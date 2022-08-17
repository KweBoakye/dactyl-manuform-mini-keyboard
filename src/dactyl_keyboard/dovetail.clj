(ns dactyl-keyboard.dovetail
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]))

;from https://github.com/hugokernel/OpenSCAD_Dovetail/blob/master/dovetail.scad

(defn tooth-offset [width] (/ width 3))

(defn dovetail-teeth
  "Create one tooth
  width        Tooth width
  height       Tooth height
  thickness    Tooth thickness"
  [width height thickness]
  (let [offset (/ width 3)]
    (->>
     (polygon [[0 0] [width 0] [(- width offset) height] [offset height]])
     (extrude-linear  {:height thickness :twist 0 :center false})
     (translate [(/ width -2) (/ height -2) 0]))))


(defn dovetail 
  ([width tooth-count tooth-height tooth-thickness ] 
   (dovetail width tooth-count tooth-height tooth-thickness 0.1 true false)
   )
  ([width tooth-count tooth-height tooth-thickness clear male debug ] 
   (let [y (/ (/ (- width (* tooth-count 2 clear)) tooth-count) 4)
         compensation -0.1
         tooth-width (* y 3)
         offset (/ tooth-width 3)
         start-at (+ (* clear 2) (/ tooth-width 2) (- (/ width 2)) (- clear))
         ]
     
       (->> 
        (union 
         (for [i (range -1 (- (* tooth-count 2) 0))
           :let [x (+ start-at (/ y 2) (* (+ tooth-width  (- offset) clear) i))]]
       (cond (not= (mod i 2) 0)
         (let [dx (+ x 0.1)]
           (cond (= male true) 
             (->> 
              (dovetail-teeth (- tooth-width clear) (- tooth-height clear) tooth-thickness)
              (rdz 180)
              (translate [(+ dx compensation) (- clear) 0])
              )
             )
           ) :else 
             (cond (= male false)
                  
                    (->> 
                    (dovetail-teeth (- tooth-width clear) (- tooth-height clear) tooth-thickness)
                    (translate [x clear 0])
                    ) 
                   )
         
       )
      ))
      (translate [0 0 (/ (- tooth-thickness) 2)])
      )
     ) 
   ))


(defn cutter 
  ([position dimension teeth] (cutter position dimension teeth true false ))
  ([position dimension teeth male debug]
  (->> 
   (union 
    (dovetail (first dimension) (first teeth) (second teeth) (nth dimension 2) (nth teeth 2) male debug)

    (cond (= male true) 
          (translate [(- (/ (first dimension) 2)) 
                     (-  (- (/ (- (second teeth))  2) 0.1 ) (second dimension))
                      (- (/ (nth dimension 2) 2))]
                     (cube (first dimension) (second dimension) (nth dimension 2) :center false)
                     ) :else
          (translate [(- (/ (first dimension) 2))
                      (- (/ (second teeth)  2) 0.1)
                      (- (/ (nth dimension 2) 2))]
                     (cube (first dimension) (second dimension) (nth dimension 2) :center false)
                     )
          )
    )
   (translate position)
   )
)
)

(def dovetail-test
  (let [shape (cube 50 50 10 :center true)
        dimension  [50 50 10]
        position [0 0 0]
        teeth [5 8 0.5]
        ]
(union

 (intersection
 shape
  (-# (cutter position dimension teeth true false) )
  )
 (intersection
  shape
  (-# (cutter position dimension teeth false false))
  )
 
 )
    )
  
  )
