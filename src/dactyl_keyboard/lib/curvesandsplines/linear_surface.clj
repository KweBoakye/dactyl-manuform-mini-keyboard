(ns dactyl-keyboard.lib.curvesandsplines.linear-surface 
  (:require [clojure.core.matrix :refer [mul]]))


(defn bilinear-surface-point
  "from p60 of"
  [P-zero-zero P-zero-one P-one-zero P-one-one u w]
  
  (mapv + (mapv (partial * (- 1 u) (- 1 w) ) P-zero-zero)
        (mapv (partial * (- 1 u) w ) P-zero-one)
        (mapv (partial * u (- 1 w)) P-one-zero) 
        (mapv (partial * u w) P-one-one))
  )

(defn bilinear-surface [P-zero-zero P-zero-one P-one-zero P-one-one u-steps w-steps]
  (vec 
   (for [u-index (range (inc u-steps))
        :let [u (/ u-index u-steps)]]
    (vec (for [w-index (range (inc w-steps))
          :let [w (/ w-index w-steps)]]
      (mapv double (bilinear-surface-point P-zero-zero P-zero-one P-one-zero P-one-one u w))
      ))
    )) 
  )


(defn lofted-surface-point [P-u-one P-u-zero u w]
  (mapv + (mul P-u-zero (- 1 w)) (mul w P-u-one))
  )

(defn lofted-surface [P-u-one P-u-zero u-steps w-steps &{:keys [boundary-curves-generated]
                                                        :or {boundary-curves-generated true}}] 
  (vec
   (for [u-index (range (inc u-steps))
         :let [u (/ u-index u-steps)]]
     (vec (for [w-index (range (inc w-steps))
                :let [w (/ w-index w-steps)]]
            (if boundary-curves-generated  (lofted-surface-point (nth P-u-one u-index) (nth P-u-zero u-index) u w)
                (mapv double (lofted-surface-point (P-u-one u) (P-u-zero u) u w))))))))

()

(comment
  (bilinear-surface [0 0 1] [1 0 0] [1 1 1] [0 1 0] 30 30))

(comment (bilinear-surface-point [0 0 1] [1 0 0] [1 1 1] [0 1 0] 1 1))
