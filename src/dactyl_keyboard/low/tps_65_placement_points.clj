(ns dactyl-keyboard.low.tps-65-placement-points
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.low.placement-functions-low :refer [transform-position]]
            [dactyl-keyboard.low.tps-65-placement-functions :refer :all]
            [dactyl-keyboard.low.web-connecters-low :refer [post-size
                                                            web-thickness]]
            [dactyl-keyboard.tps-65 :refer :all])
  )

(def tps-65-top-right (partial tps-65-translate-and-place-at-position-with-offset
                               tps-65-mount-corner-cylinder-top-right-position
                               [(+ tps-65-corner-radius 0.05 (/ post-size 2))  (+ tps-65-corner-radius 0.05 (/ post-size 2)) (+ (* (+ tps-65-depth) -2) 0.05)]))


(def tps-65-bottom-right (partial tps-65-translate-and-place-at-position-with-offset
                                  tps-65-mount-corner-cylinder-bottom-right-position
                                  [(+ tps-65-corner-radius 0.05 (/ post-size 2))  (- (+ tps-65-corner-radius 0.05 (/ post-size 2))) (+ (* (+ tps-65-depth) -2) 0.05)]))

(def tps-65-top-left (partial tps-65-translate-and-place-at-position-with-offset
                              tps-65-mount-corner-cylinder-top-left-position
                              [(- (+ tps-65-corner-radius 0.05 (/ post-size 2)))  (+ tps-65-corner-radius 0.05 (/ post-size 2)) (+ (* (+ tps-65-depth) -2) 0.05)]))

(def tps-65-bottom-left (partial tps-65-translate-and-place-at-position-with-offset
                                 tps-65-mount-corner-cylinder-bottom-left-position
                                 [(- (+ tps-65-corner-radius 0.05 (/ post-size 2)))  (- (+ tps-65-corner-radius 0.05 (/ post-size 2))) (+ (* (+ tps-65-depth) -2) 0.05)]))

(def  tps-65-top-right-outer    (transform-position
                                 (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-right-position)
                                 [(+ tps-65-corner-radius 0.05)  (+ tps-65-corner-radius 0.05) 0]))

(def  tps-65-top-right-inner    (transform-position
                                 (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-right-position)
                                 [(+ tps-65-corner-radius 0.05)  (+ tps-65-corner-radius 0.05) (/ web-thickness -2)]))

(def  tps-65-mid-right-outer    (transform-position
                                 (partial tps-65-translate-and-place-at-position [(- (/ tps-65-mount-width 2) tps-65-corner-radius),
                                                                                  0,
                                                                                  0])
                                 [(+ tps-65-corner-radius 0.05)  0 0]))

(def  tps-65-mid-right-inner    (transform-position
                                 (partial tps-65-translate-and-place-at-position [(- (/ tps-65-mount-width 2) tps-65-corner-radius),
                                                                                  0,
                                                                                  0])
                                 [(+ tps-65-corner-radius 0.05)  0 (/ web-thickness -2)]))
(def  tps-65-bottom-right-outer    (transform-position
                                    (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-bottom-right-position)
                                    [(+ tps-65-corner-radius 0.05)  (- (+ tps-65-corner-radius 0.05)) 0]))

(def  tps-65-centre     (partial tps-65-translate-and-place-at-position-with-offset
                                 [0 0 0]
                                 [(+ (* (+ tps-65-depth) -2) 0.05)]))
(def  tps-65-centre-outer (transform-position
                           (partial tps-65-place)
                           [0  0 0]))
(def  tps-65-centre-inner    (transform-position
                              (partial tps-65-place)
                              [0 0 (/ web-thickness -2)]))

(def  tps-65-bottom-right-inner    (transform-position
                                    (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-bottom-right-position)
                                    [(+ tps-65-corner-radius 0.05)  (- (+ tps-65-corner-radius 0.05)) (/ web-thickness -2)]))

(def tps-65-top-left-outer    (transform-position
                               (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-left-position)
                               [(- (+ tps-65-corner-radius 0.05))  (+ tps-65-corner-radius 0.05) 0]))

(def tps-65-top-left-inner    (transform-position
                               (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-left-position)
                               [(- (+ tps-65-corner-radius 0.05))  (+ tps-65-corner-radius 0.05) (- (/ web-thickness 2))]))



(def tps-65-mid-left-outer    (transform-position
                               (partial tps-65-translate-and-place-at-position [(- tps-65-corner-radius (/ tps-65-mount-width 2)),
                                                                                0,
                                                                                0])
                               [(- (+ tps-65-corner-radius 0.05))  0 0]))
(def tps-65-mid-left-inner    (transform-position
                               (partial tps-65-translate-and-place-at-position [(- tps-65-corner-radius (/ tps-65-mount-width 2)),
                                                                                0,
                                                                                0])
                               [(- (+ tps-65-corner-radius 0.05))  0 (- (/ web-thickness 2))]))
(def tps-65-bottom-left-outer (transform-position
                               (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-bottom-left-position)
                               [(- (+ tps-65-corner-radius 0.05))  (- (+ tps-65-corner-radius 0.05)) 0]))

(def tps-65-bottom-left-inner (transform-position
                               (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-bottom-left-position)
                               [(- (+ tps-65-corner-radius 0.05))  (- (+ tps-65-corner-radius 0.05)) (- (/ web-thickness 2))]))

(def tps-65-top-left-control-point-outer (transform-position
                                          (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-left-position)
                                          [(- (+ tps-65-corner-radius 0.05))  (* 1.5 (+ tps-65-corner-radius 0.05)) (/ web-thickness)]))

(def tps-65-top-mid-outer (transform-position
                           (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-mid-position)
                           [0 (+ tps-65-corner-radius 0.05) 0]))
(def tps-65-top-mid-inner (transform-position
                           (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-top-mid-position)
                           [0 (+ tps-65-corner-radius 0.05) (/ web-thickness -2)]))

(def tps-65-top-mid-right-outer (transform-position
                           (partial tps-65-translate-and-place-at-position [(/ (- (/ tps-65-mount-width 2) tps-65-corner-radius) 2),
                                                                            (- (/ tps-65-mount-length 2) tps-65-corner-radius),
                                                                            0])
                           [0 (+ tps-65-corner-radius 0.05) 0]))
(def tps-65-top-mid-right-inner (transform-position
                           (partial tps-65-translate-and-place-at-position [(/ (- (/ tps-65-mount-width 2) tps-65-corner-radius) 2),
                                                                            (- (/ tps-65-mount-length 2) tps-65-corner-radius),
                                                                            0])
                           [0 (+ tps-65-corner-radius 0.05) (/ web-thickness -2)]))

(def tps-65-top-mid-left-outer (transform-position
                                (partial tps-65-translate-and-place-at-position
                                         [(/ (- tps-65-corner-radius (/ tps-65-mount-width 2)) 2),
                                          (- (/ tps-65-mount-length 2) tps-65-corner-radius),
                                          0])
                                [0 (+ tps-65-corner-radius 0.05) 0]))
(def tps-65-top-mid-left-inner (transform-position
                                (partial tps-65-translate-and-place-at-position
                                         [(/ (- tps-65-corner-radius (/ tps-65-mount-width 2)) 2),
                                          (- (/ tps-65-mount-length 2) tps-65-corner-radius),
                                          0])
                                [0 (+ tps-65-corner-radius 0.05) (/ web-thickness -2)]))


(def tps-65-bottom-mid-outer (transform-position
                           (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-bottom-mid-position)
                           [0 (- (+ tps-65-corner-radius 0.05)) 0]))

(def tps-65-bottom-mid-inner (transform-position
                              (partial tps-65-translate-and-place-at-position tps-65-mount-corner-cylinder-bottom-mid-position)
                              [0 (- (+ tps-65-corner-radius 0.05)) (- (/ web-thickness 2))]))

(def tps-65-bottom-mid-right-outer (transform-position
                              (partial tps-65-translate-and-place-at-position [(/ (- (/ tps-65-mount-width 2) tps-65-corner-radius) 2),
                                                                               (- tps-65-corner-radius (/ tps-65-mount-length 2)),
                                                                               0])
                              [0 (- (+ tps-65-corner-radius 0.05)) 0]))

(def tps-65-bottom-mid-right-inner (transform-position
                              (partial tps-65-translate-and-place-at-position [(/ (- (/ tps-65-mount-width 2) tps-65-corner-radius) 2),
                                                                               (- tps-65-corner-radius (/ tps-65-mount-length 2)),
                                                                               0])
                              [0 (- (+ tps-65-corner-radius 0.05)) (- (/ web-thickness 2))]))
(def tps-65-bottom-mid-left-outer (transform-position
                                    (partial tps-65-translate-and-place-at-position [(/ (- tps-65-corner-radius (/ tps-65-mount-width 2)) 2),
                                                                                     (- tps-65-corner-radius (/ tps-65-mount-length 2)),
                                                                                     0])
                                    [0 (- (+ tps-65-corner-radius 0.05)) 0]))

(def tps-65-bottom-mid-left-inner (transform-position
                                    (partial tps-65-translate-and-place-at-position [(/ (- tps-65-corner-radius (/ tps-65-mount-width 2)) 2),
                                                                                     (- tps-65-corner-radius (/ tps-65-mount-length 2)),
                                                                                     0])
                                    [0 (- (+ tps-65-corner-radius 0.05)) (- (/ web-thickness 2))]))
(defn get-tps-65-corner-translation-vector [corner]
  (case corner
    :tl (mapv + tps-65-mount-corner-cylinder-top-right-position [(+ tps-65-corner-radius 0.05)  (+ tps-65-corner-radius 0.05) 0])
    :tr (mapv + tps-65-mount-corner-cylinder-bottom-right-position [(+ tps-65-corner-radius 0.05)  (- (+ tps-65-corner-radius 0.05)) 0])
    :bl (mapv + tps-65-mount-corner-cylinder-top-left-position [(- (+ tps-65-corner-radius 0.05))  (+ tps-65-corner-radius 0.05) 0])
    :br (mapv + tps-65-mount-corner-cylinder-bottom-left-position [(- (+ tps-65-corner-radius 0.05))  (- (+ tps-65-corner-radius 0.05)) 0])
    :tm (mapv + tps-65-mount-corner-cylinder-right-mid-position [(+ tps-65-corner-radius 0.05) 0  0 ])
    :bm (mapv + tps-65-mount-corner-cylinder-left-mid-position [(- (+ tps-65-corner-radius 0.05)) 0 0])
    :lm (mapv + tps-65-mount-corner-cylinder-top-mid-position [0 (+ tps-65-corner-radius 0.05) 0])
    :rm (mapv + tps-65-mount-corner-cylinder-bottom-mid-position [0 (- (+ tps-65-corner-radius 0.05)) 0])
    :tl-lm (mapv + [(/ (- (/ tps-65-mount-width 2) tps-65-corner-radius) 2)
                    (- (/ tps-65-mount-length 2) tps-65-corner-radius)
                    0] [0 (+ tps-65-corner-radius 0.05) 0]) 
    :tr-rm (mapv + [(/ (- (/ tps-65-mount-width 2) tps-65-corner-radius) 2)
                    (- tps-65-corner-radius (/ tps-65-mount-length 2))
                    0] [0 (- (+ tps-65-corner-radius 0.05)) 0])
    :bl-lm (mapv + [(/ (- tps-65-corner-radius (/ tps-65-mount-width 2)) 2)
                    (- (/ tps-65-mount-length 2) tps-65-corner-radius)
                    0] [0 (+ tps-65-corner-radius 0.05) 0])
    :br-rm (mapv + [(/ (- tps-65-corner-radius (/ tps-65-mount-width 2)) 2)
                    (- tps-65-corner-radius (/ tps-65-mount-length 2))
                    0] [0 (- (+ tps-65-corner-radius 0.05)) 0])))

(defn get-position-tps-65 [corner]
  (case corner
    :tl {:tps-65-corner-outer tps-65-top-right-outer
         :tps-65-corner-inner tps-65-top-right-inner}
    :tl-lm {:tps-65-corner-outer tps-65-top-mid-right-outer
            :tps-65-corner-inner tps-65-top-mid-right-inner}
    :tr {:tps-65-corner-outer tps-65-bottom-right-outer
         :tps-65-corner-inner tps-65-bottom-right-inner}
    :tr-rm {:tps-65-corner-outer tps-65-bottom-mid-right-outer
            :tps-65-corner-inner tps-65-bottom-mid-right-inner}
    :bl {:tps-65-corner-outer tps-65-top-left-outer
         :tps-65-corner-inner tps-65-top-left-inner}
    :bl-lm {:tps-65-corner-outer tps-65-top-mid-left-outer
            :tps-65-corner-inner tps-65-top-mid-left-inner}
    :br {:tps-65-corner-outer tps-65-bottom-left-outer
         :tps-65-corner-inner tps-65-bottom-left-inner}
    :br-rm {:tps-65-corner-outer tps-65-bottom-mid-left-outer
         :tps-65-corner-inner tps-65-bottom-mid-left-inner}
    :bm {:tps-65-corner-outer tps-65-mid-left-outer
         :tps-65-corner-inner tps-65-mid-left-inner}
    :tm {:tps-65-corner-outer tps-65-mid-right-outer
         :tps-65-corner-inner tps-65-mid-right-inner}
    :lm {:tps-65-corner-outer tps-65-top-mid-outer
         :tps-65-corner-inner tps-65-top-mid-inner}
    :rm {:tps-65-corner-outer tps-65-bottom-mid-outer
         :tps-65-corner-inner tps-65-bottom-mid-inner}
    ))

(defn dx-dy-to-cardinal [dx dy]
  (cond 
    (pos? dx) (cond 
                (pos? dy) :north-east
                (zero? dy) :east
                (neg? dy) :south-east)
    (zero? dx) (cond 
                 (pos? dy) :north
                 (neg? dy) :south)
    (neg? dx )(cond 
                (pos? dy) :north-west
                (zero? dy) :west
                (neg? dy) :south-west))
  )

(defn dx-dy-to-tps-65-dx-dy [old-dx old-dy]
  (let [dx old-dy
        dy (- old-dx)]
    {:dx dx
     :dy dy})
  )

(comment (dx-dy-to-cardinal 1 1))

(defn get-opposite-position-tps-65 [corner cardinal]
  (case corner
    :tl (case cardinal
          :north {:tps-65-corner-outer-opposite tps-65-top-left-outer
                  :tps-65-corner-inner-opposite tps-65-top-left-inner}
          :north-west {:tps-65-corner-outer-opposite tps-65-bottom-left-outer
                       :tps-65-corner-inner-opposite tps-65-bottom-left-inner}
          :west {:tps-65-corner-outer-opposite tps-65-bottom-right-outer
                 :tps-65-corner-inner-opposite tps-65-bottom-right-inner})
    :tr (case cardinal
          :north {:tps-65-corner-outer-opposite tps-65-bottom-left-outer
                  :tps-65-corner-inner-opposite tps-65-bottom-left-inner}
          :north-east {:tps-65-corner-outer-opposite tps-65-top-left-outer
                       :tps-65-corner-inner-opposite tps-65-top-left-inner}
          :east {:tps-65-corner-outer-opposite tps-65-top-right-outer
                 :tps-65-corner-inner-opposite tps-65-top-right-inner})
    :bl  (case cardinal
           :south {:tps-65-corner-outer-opposite tps-65-top-right-outer
                   :tps-65-corner-inner-opposite tps-65-top-right-inner}
           :south-west {:tps-65-corner-outer-opposite tps-65-bottom-right-outer
                        :tps-65-corner-inner-opposite tps-65-bottom-right-inner}
           :west {:tps-65-corner-outer-opposite tps-65-bottom-right-outer
                  :tps-65-corner-inner-opposite tps-65-bottom-right-inner})

    :br  (case cardinal
           :south {:tps-65-corner-outer-opposite tps-65-bottom-right-outer
                   :tps-65-corner-inner-opposite tps-65-bottom-right-inner}
           :south-east {:tps-65-corner-outer-opposite tps-65-top-right-outer
                        :tps-65-corner-inner-opposite tps-65-top-right-inner}
           :east {:tps-65-corner-outer-opposite tps-65-top-left-outer
                  :tps-65-corner-inner-opposite tps-65-top-left-inner})
    :tm (case cardinal
          :north {:tps-65-corner-outer-opposite tps-65-mid-left-outer
                 :tps-65-corner-inner-opposite tps-65-mid-left-inner}
          :north-west {:tps-65-corner-outer-opposite tps-65-bottom-left-outer
                       :tps-65-corner-inner-opposite tps-65-bottom-left-inner}
          :north-east {:tps-65-corner-outer-opposite tps-65-top-left-outer
                       :tps-65-corner-inner-opposite tps-65-top-left-inner})
    :bm (case cardinal
          :south {:tps-65-corner-outer-opposite tps-65-mid-right-outer
                  :tps-65-corner-inner-opposite tps-65-mid-right-inner}
          :south-east {:tps-65-corner-outer-opposite tps-65-top-right-outer
                       :tps-65-corner-inner-opposite tps-65-top-right-inner}
          :south-west {:tps-65-corner-outer-opposite tps-65-top-left-outer
                       :tps-65-corner-inner-opposite tps-65-top-left-inner})
    :lm (case cardinal 
          :west {:tps-65-corner-outer-opposite tps-65-bottom-mid-outer
                  :tps-65-corner-inner-opposite tps-65-bottom-mid-inner}
          :north-west {:tps-65-corner-outer-opposite tps-65-bottom-left-outer
                       :tps-65-corner-inner-opposite tps-65-bottom-left-inner}
          :south-west {:tps-65-corner-outer-opposite tps-65-bottom-right-outer
                       :tps-65-corner-inner-opposite tps-65-bottom-right-inner})
    :rm (case cardinal
          :east {:tps-65-corner-outer-opposite tps-65-top-mid-outer
                 :tps-65-corner-inner-opposite tps-65-top-mid-inner}
          :north-east {:tps-65-corner-outer-opposite tps-65-top-left-outer
                      :tps-65-corner-inner-opposite tps-65-top-left-outer}  
          :south-east {:tps-65-corner-outer-opposite tps-65-top-right-outer
                       :tps-65-corner-inner-opposite tps-65-top-right-inner})
    :tl-lm (case cardinal
             :west {:tps-65-corner-outer-opposite tps-65-bottom-mid-right-outer
                    :tps-65-corner-inner-opposite tps-65-bottom-mid-right-inner}
             :north-west {:tps-65-corner-outer-opposite tps-65-bottom-left-outer
                          :tps-65-corner-inner-opposite tps-65-bottom-left-inner}
             :south-west {:tps-65-corner-outer-opposite tps-65-bottom-right-outer
                          :tps-65-corner-inner-opposite tps-65-bottom-right-inner})
    :tr-rm (case cardinal
          :east {:tps-65-corner-outer-opposite tps-65-top-mid-outer
                 :tps-65-corner-inner-opposite tps-65-top-mid-inner}
          :north-east {:tps-65-corner-outer-opposite tps-65-top-mid-right-outer
                       :tps-65-corner-inner-opposite tps-65-top-mid-right-inner}
          :south-east {:tps-65-corner-outer-opposite tps-65-top-right-outer
                       :tps-65-corner-inner-opposite tps-65-top-right-inner})
    :bl-lm (case cardinal
             :west {:tps-65-corner-outer-opposite tps-65-bottom-mid-right-outer
                    :tps-65-corner-inner-opposite tps-65-bottom-mid-right-inner}
             :south-west {:tps-65-corner-outer-opposite tps-65-bottom-mid-right-outer
                          :tps-65-corner-inner-opposite tps-65-bottom-mid-right-inner})
    :br-rm (case cardinal
             :east {:tps-65-corner-outer-opposite tps-65-top-mid-right-outer
                    :tps-65-corner-inner-opposite tps-65-top-mid-right-inner})))

(defn get-tps-65-dx-dy [cardinal]
  (case cardinal
    :north {:dx 0 :dy 1}
    :north-east {:dx 1 :dy 1}
    :east {:dx 1 :dy 0}
    :south-east {:dx 1 :dy -1}
    :south {:dx 0 :dy -1}
    :south-west {:dx -1 :dy -1}
    :west {:dx -1 :dy 0}
    :north-west {:dx -1 :dy 1}))