(ns dactyl-keyboard.lib.curvesandsplines.b-spline-surface 
  (:require [dactyl-keyboard.lib.curvesandsplines.non-uniform-b-spline :refer [calculate-knot-span-index calculate-non-vanishing-basis-functions]]))



(defn b-spline-surface-point [n p U m q V P u v]
  (let [u-span (calculate-knot-span-index n p u U)
        Nu (calculate-non-vanishing-basis-functions u-span u p U)
        v-span (calculate-knot-span-index m q v V) 

        Nv (calculate-non-vanishing-basis-functions v-span v q V) 

        u-ind (- u-span p)
        
        ]
    
    ;(println v-span)
    (loop [S [0.0 0.0 0.0] l 0]
      (if (<= l q)
        (let [v-ind (inc (- v-span q)) 
              
              temp-outer (loop [temp [0.0 0.0 0.0] k 0]
                           (if (<= k p)
                             (recur (mapv + temp (mapv (partial * (nth Nu k) ) (get-in P [(+ u-ind k) v-ind]))) (inc k))
                             temp))]  
         (recur (mapv + S (mapv (partial * (nth Nv l)) temp-outer)) (inc l)) 
         )
        S
        ) 
      ) 
    )
  )

(defn b-spline-surface-segment [n p U m q V P  steps
                                &{:keys [u-start u-end v-start v-end] :or {u-start 0 u-end (inc u-start) v-start 0 v-end (inc v-start)} }]
  (let [u-increment (/ (- u-end u-start) steps)
        v-increment (/ (- v-end v-start) steps)]
    (vec (for [u (range  u-start (+ u-end u-increment)  u-increment)]
            
             
              (vec (for [v (range  v-start   (+ v-end v-increment)  v-increment)] 
                    (b-spline-surface-point n p U m q V P u v)
                   ))
      ))
    )
  )

(defn b-spline-surface [n p m q U V P steps & {:keys [drop-last-point-of-segment] :or {drop-last-point-of-segment true}}]
  (let [r (- (count U) p 2)
        s (- (count V) q 2)
        P-matrix (mapv vec (partition (inc m) (mapv (partial mapv double )P)))
        u-segments (inc (- n p))
        v-segments (inc (- m q))
        ;drop-last-point-if-not-last-segment (get-drop-last-point-if-not-last-segment number-of-segments drop-last-point-of-segment)
        ] 
    (println P-matrix)
    ;; (for [index-u (range (inc steps ))
    ;;       :let [u (/ index-u steps)]]
    ;;   (for [index-v (range (inc  steps))
    ;;         :let [v (/ index-v steps)]]
    ;;     (do
    ;;       (println "u" u "v" v)
    ;;      (b-spline-surface-point n  p U m q V P-matrix u v))
    ;;     )
    ;;   )
    (vec (apply concat (vec (for [u-index (range u-segments) v-index (range v-segments)]
      (b-spline-surface-segment n p U m q V P-matrix steps :u-start u-index :u-end (inc u-index) :v-start v-index :v-end (inc v-index))
      ))))
    ) 
  )

(comment 
    (b-spline-surface 3 3 3 2  [0 0 0 0 1 1 1 1] (mapv (partial * 2) [0 0 0 0.5 1 1 1]) 
                    [[0 0 0] [0 4 0] [0 8 -3] [0 10 -3] 
                     [2 0 6] [2 4 0] [2 8 0] [2 10 0] 
                     [4 0 0] [4 4 0] [4 8 3] [4 10 3] 
                     [6 0 0] [6 4 -3] [6 8 0] [6 10 0]
                     [8 0 0] [8 4 -3] [8 8 0] [8 10 0]]
                    10))

[[-15.0 0.0 15.0]
 [-15.0 5.0 5.0]
 [-15.0 5.0 -5.0]
 [-15.0 0.0 -15.0]
 [-5.0 5.0 15.0]
 [-5.0 10.0 5.0]
 [-5.0 10.0 -5.0]
 [-5.0 5.0 -15.0]
 [5.0 5.0 15.0]
 [5.0 10.0 5.0]
 [5.0 10.0 -5.0]
 [5.0 0.0 -15.0]
 [15.0 0.0 15.0]
 [15.0 5.0 5.0]
 [15.0 5.0 -5.0]
 [15.0 0.0 -15.0]]





(comment 
  (b-spline-surface 4 2 5 2 (mapv (partial * (inc (+ 4 2)))[0 0 0 (/ 2 3) (/ 3 5) 1 1 1])
                    (mapv (partial * (inc (+ 5 2))) [0 0 0 (/ 1 5) (/ 1 2) (/ 4 5) 1 1 1])
                    [ [0 0 0] [1 0 0] [2 0 0] [0 1 0] [1 1 1] [2 1 -0.5] [0 2 0] [1 2 0] [2 2 0]] 2))

 -15.000000 0.000000 15.000000
-15.000000 5.000000 5.000000
-15.000000 5.000000 -5.000000
-15.000000 0.000000 -15.000000
-5.000000 5.000000 15.000000
-5.000000 10.000000 5.000000
-5.000000 10.000000 -5.000000
-5.000000 5.000000 -15.000000
5.000000 5.000000 15.000000
5.000000 10.000000 5.000000
5.000000 10.000000 -5.000000
5.000000 0.000000 -15.000000
15.000000 0.000000 15.000000
15.000000 5.000000 5.000000
15.000000 5.000000 -5.000000
15.000000 0.000000 -15.000000