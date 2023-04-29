(ns dactyl-keyboard.low.web-connecters-low
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            
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
(def web-post-tm-translation-vector  [0 (- (/ mount-height 1.95) post-adj) 0])
(def web-post-bm-translation-vector  [0 (+ (/ mount-height -1.95) post-adj) 0])
(def web-post-rm-translation-vector  [(- (/ mount-width 1.95) post-adj) 0  0])
(def web-post-lm-translation-vector  [(+ (/ mount-width -1.95) post-adj) 0  0])
(def web-post-tr-tm-translation-vector  [(/ (- (/ mount-width 1.95) post-adj) 2) (- (/ mount-height 1.95) post-adj) 0])
(def web-post-tr-rm-translation-vector  [(- (/ mount-width 1.95) post-adj) (/ (- (/ mount-height 1.95) post-adj)2) 0])
(def web-post-tl-tm-translation-vector  [(/ (+ (/ mount-width -1.95) post-adj) 2) (- (/ mount-height 1.95) post-adj) 0])
(def web-post-bl-lm-translation-vector  [(+ (/ mount-width -1.95) post-adj) (/ (+ (/ mount-height -1.95) post-adj) 2) 0])
(def web-post-br-rm-translation-vector  [(- (/ mount-width 1.95) post-adj) (/  (+ (/ mount-height -1.95) post-adj) 2) 0])
(def web-post-tl-lm-translation-vector  [(+ (/ mount-width -1.95) post-adj) (/ (- (/ mount-height 1.95) post-adj) 2) 0])
(def web-post-bl-bm-translation-vector  [(/ (+ (/ mount-width -1.95) post-adj) 2) (+ (/ mount-height -1.95) post-adj) 0])
(def web-post-br-bm-translation-vector  [(/ (- (/ mount-width 1.95) post-adj) 2) (+ (/ mount-height -1.95) post-adj) 0])
(def web-post-tr (translate  web-post-tr-translation-vector web-post))
(def web-post-tl (translate  web-post-tl-translation-vector web-post))
(def web-post-bl (translate  web-post-bl-translation-vector  web-post))
(def web-post-br (translate  web-post-br-translation-vector web-post))
(def web-post-bm (translate  web-post-bm-translation-vector web-post))
(def web-post-tr-tm (translate web-post-tr-tm-translation-vector web-post))
(def web-post-tr-rm (translate web-post-tr-rm-translation-vector web-post))
(def web-post-tl-tm (translate web-post-tl-tm-translation-vector web-post))
(def web-post-bl-lm (translate web-post-bl-lm-translation-vector web-post))
(def web-post-br-rm (translate web-post-br-rm-translation-vector web-post))
(def web-post-tl-lm (translate web-post-tl-lm-translation-vector web-post))
(def web-post-bl-bm (translate web-post-bl-bm-translation-vector web-post))
(def web-post-br-bm (translate web-post-br-bm-translation-vector web-post))

(defn get-web-post-position-top [corner-translation-vector] (mapv + [0 0 (/ web-thickness 2)] web-post-translation-vector corner-translation-vector))
(defn get-web-post-position-middle [corner-translation-vector] (mapv + [0 0 0] web-post-translation-vector corner-translation-vector))
(defn get-web-post-position-bottom [corner-translation-vector] (mapv + [0 0 (/ web-thickness -2)] web-post-translation-vector corner-translation-vector))
(def web-post-x-distance-from-single-plate-corner (- (+  1.8  (/ keyswitch-width 2)) (- (/ mount-width 1.95) post-adj)))
(def web-post-y-distance-from-single-plate-corner (-  (- (/ mount-height 1.95) post-adj) (+ 1.5 (/ keyswitch-height 2))))

(defn if-position-is-string-return-keyword [position]
  (if (string? position) (keyword position) position)
  )
(defn get-web-corner-translation-vector [position]
  (case (if-position-is-string-return-keyword position)
    :tr web-post-tr-translation-vector
    :tl web-post-tl-translation-vector
    :bl web-post-bl-translation-vector
    :br web-post-br-translation-vector
    :bm web-post-bm-translation-vector
    :rm web-post-rm-translation-vector
    :lm web-post-lm-translation-vector
    :tm web-post-tm-translation-vector
    :tr-tm web-post-tr-tm-translation-vector
    :tr-rm  web-post-tr-rm-translation-vector
    :tl-tm  web-post-tl-tm-translation-vector
    :bl-lm  web-post-bl-lm-translation-vector
    :br-rm  web-post-br-rm-translation-vector
    :tl-lm  web-post-tl-lm-translation-vector
    :bl-bm  web-post-bl-bm-translation-vector
    :br-bm  web-post-br-bm-translation-vector
    :centre [0 0 0]
    [0 0 0]))

(comment 
  (- (get-web-corner-translation-vector :tm) ))

(defn get-single-plate-corner-position-vector [position]
  (case (if-position-is-string-return-keyword position)
    :tr (mapv + web-post-tr-translation-vector [web-post-x-distance-from-single-plate-corner (- web-post-y-distance-from-single-plate-corner) 0])
    :tl (mapv + web-post-tl-translation-vector [(- web-post-x-distance-from-single-plate-corner) (- web-post-y-distance-from-single-plate-corner) 0])
    :bl (mapv + web-post-bl-translation-vector [(- web-post-x-distance-from-single-plate-corner) web-post-y-distance-from-single-plate-corner 0])
    :br (mapv + web-post-br-translation-vector [web-post-x-distance-from-single-plate-corner  web-post-y-distance-from-single-plate-corner 0])
    :tm (mapv + web-post-tm-translation-vector [0 (- web-post-y-distance-from-single-plate-corner) 0])
    :bm (mapv + web-post-bm-translation-vector [0 web-post-y-distance-from-single-plate-corner 0])
    :rm (mapv + web-post-rm-translation-vector [web-post-x-distance-from-single-plate-corner 0 0])
    :lm (mapv + web-post-lm-translation-vector [(- web-post-x-distance-from-single-plate-corner) 0 0])
    :tr-tm  (mapv + web-post-tr-tm-translation-vector [0  (- web-post-y-distance-from-single-plate-corner) 0] )
    :tr-rm  (mapv + web-post-tr-rm-translation-vector  [web-post-x-distance-from-single-plate-corner 0 0])
    :tl-tm  (mapv + web-post-tl-tm-translation-vector  [0  (- web-post-y-distance-from-single-plate-corner) 0])
    :bl-lm  (mapv + web-post-bl-lm-translation-vector  [(- web-post-x-distance-from-single-plate-corner) 0 0])
    :br-rm  (mapv + web-post-br-rm-translation-vector  [web-post-x-distance-from-single-plate-corner 0 0])
    :tl-lm  (mapv + web-post-tl-lm-translation-vector  [(- web-post-x-distance-from-single-plate-corner) 0 0])
    :bl-bm  (mapv + web-post-bl-bm-translation-vector  [0 web-post-y-distance-from-single-plate-corner 0])
    :br-bm  (mapv + web-post-br-bm-translation-vector  [0 web-post-y-distance-from-single-plate-corner 0])
    :centre [0 0 0]
    [0 0 0])
  )

(defn get-opposite-position [position dx dy]
  (let [return-value-type-fn #(if (string? position) % (keyword %))] 
   (case (if (string? position) (keyword position) position)
     :tr (cond (and (pos? dx) (pos? dy)) (return-value-type-fn "bl")
                (pos? dx) (return-value-type-fn "tl")
                (pos? dy) (return-value-type-fn "br"))
 :tl (cond (and (neg? dx) (pos? dy)) (return-value-type-fn "br")
            (neg? dx) (return-value-type-fn "tr")
            (pos? dy) (return-value-type-fn "bl"))
 :bl (cond (and (neg? dx) (neg? dy)) (return-value-type-fn "tr")
            (neg? dx) (return-value-type-fn "br")
            (neg? dy) (return-value-type-fn "tl"))
 :br (cond (and (pos? dx) (neg? dy)) (return-value-type-fn "tl")
            (pos? dx) (return-value-type-fn "bl")
            (neg? dy) (return-value-type-fn "tr"))
 :tm (return-value-type-fn"bm")
 :bm (return-value-type-fn "tm")
 :rm (return-value-type-fn "lm")
 :lm (return-value-type-fn "rm") 
     :tr-tm (return-value-type-fn "br-bm")
:tr-rm (return-value-type-fn "tl-lm")
:tl-tm (return-value-type-fn "bl-bm")
:bl-lm (return-value-type-fn "br-rm")
:br-rm (return-value-type-fn "bl-lm")
:tl-lm (return-value-type-fn "tr-rm")
:bl-bm (return-value-type-fn "tl-tm")
:br-bm (return-value-type-fn "tr-tm")
    :centre (return-value-type-fn "centre")))
  )
(comment 
  (get-opposite-position :br 1 1))


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
