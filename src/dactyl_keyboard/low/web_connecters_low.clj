(ns dactyl-keyboard.low.web-connecters-low
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.sa-keycaps :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.low.placement-functions-low :refer :all]
  ))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 4.5)
(def post-size 0.1)
 (def web-post-translation-vector [0 0 (+ (/ web-thickness -2)
                    plate-thickness)])
(def new-post-size (if (= rounding-radius 0) 1 rounding-radius))
(def web-post (->> (rcylinder  post-size web-thickness)
                   (translate web-post-translation-vector)))

(defn web-post-shape-with-size ([height size]
  (if (= new-post-size 0)
    (cube size size height)
    (rcylinder size height)))
  ([height size fn-val]
   (if (= new-post-size 0)
     (cube size size height)
     (rcylinder size height fn-val))
   )
  )

(defn web-post-shape [height]
  (web-post-shape-with-size height post-size))



(def big-boi-web-post (->> (cube post-size (+ post-size 30) 10)
                           (translate [0 0 (- (+ (/ 10 -2)
    plate-thickness) 1)])))


(def post-adj (/ post-size 2))
(def web-post-tr-translation-vector  [(- (/ mount-width 1.95) post-adj) (- (/ mount-height 1.95) post-adj) 0] )
(def web-post-tl-translation-vector  [(+ (/ mount-width -1.95) post-adj) (- (/ mount-height 1.95) post-adj) 0] )
(def web-post-bl-translation-vector  [(+ (/ mount-width -1.95) post-adj) (+ (/ mount-height -1.95) post-adj) 0])
(def web-post-br-translation-vector  [(- (/ mount-width 1.95) post-adj) (+ (/ mount-height -1.95) post-adj) 0] )
(def web-post-bm-translation-vector  [0 (+ (/ mount-height -1.95) post-adj) 0])
(def web-post-rm-translation-vector  [(- (/ mount-width 1.95) post-adj) 0  0])
(def web-post-lm-translation-vector  [(+ (/ mount-width -1.95) post-adj) 0  0])
(def web-post-tm-translation-vector  [ 0 (- (/ mount-height 1.95) post-adj) 0])
(def web-post-tr (translate  web-post-tr-translation-vector web-post))
(def web-post-tl (translate  web-post-tl-translation-vector web-post))
(def web-post-bl (translate  web-post-bl-translation-vector  web-post))
(def web-post-br (translate  web-post-br-translation-vector web-post))
(def web-post-bm (translate  web-post-bm-translation-vector web-post))

(defn web-post-position-top [corner-translation-vector] (mapv + [0 0 (/ web-thickness 2)] web-post-translation-vector corner-translation-vector))
(defn web-post-position-bottom [corner-translation-vector] (mapv + [0 0 (/ web-thickness -2)] web-post-translation-vector corner-translation-vector))
(def web-post-x-distance-from-single-plate-corner (- (+  1.8  (/ keyswitch-width 2)) (- (/ mount-width 1.95) post-adj)))
(def web-post-y-distance-from-single-plate-corner (-  (- (/ mount-height 1.95) post-adj) (+ 1.5 (/ keyswitch-height 2))))

(defn get-web-corner-translation-vector [position]
  (case position
    "tr" web-post-tr-translation-vector
    "tl" web-post-tl-translation-vector
    "bl" web-post-bl-translation-vector
    "br" web-post-br-translation-vector
    "bm" web-post-bm-translation-vector
    "rm" web-post-rm-translation-vector
    "lm" web-post-lm-translation-vector
    "tm" web-post-tm-translation-vector
    "centre" [0 0 0]
    [0 0 0]))

(defn get-single-plate-corner-position-vector [position]
  (case position
    "tr" (mapv + web-post-tr-translation-vector [web-post-x-distance-from-single-plate-corner (- web-post-y-distance-from-single-plate-corner) 0] )
    "tl" (mapv + web-post-tl-translation-vector [(- web-post-x-distance-from-single-plate-corner) (- web-post-y-distance-from-single-plate-corner) 0])
    "bl" (mapv + web-post-bl-translation-vector [(- web-post-x-distance-from-single-plate-corner) web-post-y-distance-from-single-plate-corner 0])
    "br" (mapv + web-post-br-translation-vector [web-post-x-distance-from-single-plate-corner  web-post-y-distance-from-single-plate-corner 0])
    "bm" (mapv + web-post-bm-translation-vector [0 (- web-post-y-distance-from-single-plate-corner) 0])
    "rm" (mapv + web-post-rm-translation-vector [web-post-x-distance-from-single-plate-corner 0 0])
    "lm" (mapv + web-post-lm-translation-vector [(- web-post-x-distance-from-single-plate-corner) 0 0])
    "centre" [0 0 0]
    [0 0 0])
  )

(defn get-web-post-outer-x-and-y-vector [dx dy]
  (let [x (if (pos? dx) (/ post-size 2) (/ post-size -2))
        y (if (pos? dy) (/ post-size 2) (/ post-size -2))]
    [x y 0]))

; wide posts for 1.5u keys in the main cluster

; wide posts for 1.5u keys in the main cluster
(if pinky-15u
  (do (def wide-post-tr (translate [(- (/ mount-width 1.2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
      (def wide-post-tl (translate [(+ (/ mount-width -1.2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
      (def wide-post-bl (translate [(+ (/ mount-width -1.2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
      (def wide-post-br (translate [(- (/ mount-width 1.2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post)))
  (do (def wide-post-tr web-post-tr)
      (def wide-post-tl web-post-tl)
      (def wide-post-bl web-post-bl)
      (def wide-post-br web-post-br)))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(def connectors
  (apply union
         (concat
          ;; Row connections
          (for [column (range (+ innercol-offset 0) (dec ncols))
                row (range 0 lastrow)]
            (triangle-hulls
             (key-place (inc column) row web-post-tl)
             (key-place column row web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place column row web-post-br)))

          ;; Column connections
          (for [column columns
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))

          ;; Diagonal connections
          (for [column (range 0 (dec ncols))
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place (inc column) (inc row) web-post-tl))))))

(def inner-connectors
  (if inner-column
    (apply union
           (concat
            ;; Row connections
            (for [column (range 0 1)
                  row (range 0 (- nrows 2))]
              (triangle-hulls
               (key-place (inc column) row web-post-tl)
               (key-place column row web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place column row web-post-br)))

            ;; Column connections
            (for [row (range 0 (dec cornerrow))]
              (triangle-hulls
               (key-place innercolumn row web-post-bl)
               (key-place innercolumn row web-post-br)
               (key-place innercolumn (inc row) web-post-tl)
               (key-place innercolumn (inc row) web-post-tr)))

            ;; Diagonal connections
            (for [column (range 0 (dec ncols))
                  row (range 0 2)]
              (triangle-hulls
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place (inc column) (inc row) web-post-tl)))))))


(def extra-connectors
  (if extra-row
    (apply union
           (concat
            (for [column (range 3 ncols)
                  row (range cornerrow lastrow)]
              (triangle-hulls
               (key-place column row web-post-bl)
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tl)
               (key-place column (inc row) web-post-tr)))

            (for [column (range 3 (dec ncols))
                  row (range cornerrow lastrow)]
              (triangle-hulls
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place (inc column) (inc row) web-post-tl)))

            (for [column (range 4 (dec ncols))
                  row (range lastrow nrows)]
              (triangle-hulls
               (key-place (inc column) row web-post-tl)
               (key-place column row web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place column row web-post-br)))))))
