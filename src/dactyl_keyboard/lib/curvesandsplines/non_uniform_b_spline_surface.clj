(ns dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline-surface 
  (:require [dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline :refer [calculate-knot-span-index calculate-non-vanishing-basis-functions]]
            [dactyl-keyboard.lib.curvesandsplines.curve-utils :refer [project-coordinate]]))


(defn non-uniform-b-spline-surface-point [n p U m q V P u v]
  (let [u-span (calculate-knot-span-index n p u U)
        Nu (calculate-non-vanishing-basis-functions u-span u p U)
        v-span (calculate-knot-span-index m q v V)
        Nv (calculate-non-vanishing-basis-functions v-span v q V)
        temp (double-array (inc q))]
    ;(print "Nv" Nv)
    ;(print "Pw" (get-in Pw [(+ u-span (- p) 0) (+ v-span (- q) 1)]))
    (loop [S [0.0 0.0 0.0] l 0]
      (if (<= l q)
      (do 
        (aset temp l 0.0)
      (doseq [k (range (inc p))]
        (aset temp l (+ (aget temp l)
                        (* (nth Nu k) (get-in P [(+ u-span (- p) k) (+ v-span (- q) l)]))))) 
        (recur (mapv #(+ % (* (nth Nv l) (aget temp l))) S) (inc l)))
        S))))

(defn non-uniform-b-spline-surface [n p m q U V P steps]
  (for [index-u (range (inc steps))
        :let [u (/ index-u steps)]]
    (for [index-v (range (inc steps))
          :let [v (/ index-v steps)]]
      (non-uniform-b-spline-surface-point n p U m q V P u v))))

(defn nurbs-surface-point [n p U m q V Pw u v]
  (let [u-span (calculate-knot-span-index n p u U)
        Nu (calculate-non-vanishing-basis-functions u-span u p U)
v-span (calculate-knot-span-index m q v V)
Nv (calculate-non-vanishing-basis-functions v-span v q V)
        temp (double-array (inc q))]
    ;(print "Nv" Nv)
    ;(print "Pw" (get-in Pw [(+ u-span (- p) 0) (+ v-span (- q) 1)]))
    (doseq [l (range (inc q))]
           (aset temp l 0.0)
           (doseq [k (range (inc p))]
                  (aset temp l (+ (aget temp l)  
                                 (* (nth Nu k) (get-in Pw [(+ u-span (- p) k) (+ v-span (- q) l)]) ) 
                                     ))
                  )
           )
  (loop [Sw [0.0 0.0 0.0 0.0] l 0]
    (if (<= l q)
      (recur (mapv #(+ % (* (nth Nv l) (aget temp l))) Sw) (inc l))
      (subvec (project-coordinate Sw) 0 3)))
    ) 
  )

(defn nurbs-surface [n p m q U V Pw steps]
  (for [index-u (range (inc steps))
        :let [u (/ index-u steps)]]
    (for [index-v (range (inc steps))
          :let [v (/ index-v steps)]]
      (nurbs-surface-point n p U m q V Pw u v)))
  )

(comment (nurbs-surface 8 2 4 2 [0 0 0 1 2 3 4 4 5 5 5]
                                       [0 0 0 1 2 3 3 3] 
                                       [[0 2 4 1] [0 6 4 2] [0 2 0 1] 
                                        [4 6 8 2] [12 24 12 6] [4 6 0 2]
                                        [4 2 2 1] [8 6 4 2] [4 2 0 1]] 10))