(ns dactyl-keyboard.lib.curvesandsplines.linear-surface)


(defn bilinear-surface-point
  "from p60 of"
  [P-zero-zero P-zero-one P-one-zero P-one-one u w]
  
  (mapv + (mapv (partial * (- 1 u) (- 1 w) ) P-zero-zero)
        (mapv (partial * (- 1 u) w ) P-zero-one)
        (mapv (partial * u (- 1 w)) P-one-zero) 
        (mapv (partial * u w) P-one-one))
  )

(defn bilinear-surface [P-zero-zero P-zero-one P-one-zero P-one-one steps]
  (vec 
   (for [u-index (range (inc steps))
        :let [u (/ u-index steps)]]
    (vec (for [w-index (range (inc steps))
          :let [w (/ w-index steps)]]
      (mapv double (bilinear-surface-point P-zero-zero P-zero-one P-one-zero P-one-one u w))
      ))
    )) 
  )

(comment
  (bilinear-surface [0 0 1] [1 0 0] [1 1 1] [0 1 0] 30))

[clojure.core.matrix :refer [magnitude]]