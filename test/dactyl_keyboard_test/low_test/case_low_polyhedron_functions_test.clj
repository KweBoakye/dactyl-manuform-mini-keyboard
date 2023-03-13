(ns dactyl-keyboard-test.low-test.case-low-polyhedron-functions-test
  (:require [clojure.core.matrix :refer [cross magnitude]]
            [clojure.math :refer [pow sqrt]]
            [clojure.pprint :as pp]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :refer [join]]
            [clojure.test :refer :all]
            [dactyl-keyboard.lib.algebra :refer [are-points-collinear?]]
            [dactyl-keyboard.lib.geometry :refer [area-of-triangle]]
            [dactyl-keyboard.low.case-low-polyhedron-functions :refer [wall-brace-polyhedron-points]]
            [dactyl-keyboard.low.placement-functions-low :refer :all]
            [dactyl-keyboard.low.shape-parameters-low :refer [cornerrow
                                                              lastcol wall-xy-offset
                                                              wall-xy-offset-medium-thin wall-xy-offset-mid wall-xy-offset-thin]]))






(comment (mapv - [3 -3 -3] [1 -4 -2]))
(comment (mapv - [5 -1 -2] [1 -4 -2]))
(comment (cross  (mapv - [3 -3] [1 -4]) (mapv - [5 -1] [1 -4])))
(comment (* 0.5 (magnitude (cross  (mapv - [3 -3] [1 -4]) (mapv - [5 -1] [1 -4])))))

(defn colinear [point1 point2 point3]
  (let [area (* 0.5 (magnitude (cross  (mapv - (drop-last point2) (drop-last point1))  (mapv - (drop-last point3) (drop-last point1)))))
        ;area (area-of-triangle (drop-last point1) (drop-last point2) (drop-last point3))
        ]
    (println "area " area)
     (< area 1e-10)
    )
  )

(defn area-of-triangle-2d [point1-2d point2-2d point3-2d]
  (* 0.5 (magnitude (mapv * (mapv - point2-2d point1-2d) (mapv - point3-2d point1-2d))))
  )

(defn colinear-2d [point1 point2 point3]
  (let [point-or-2d #(if (= (count %) 3) (drop-last %) %)
        point1-2d (point-or-2d point1)
        point2-2d (point-or-2d point2)
        point3-2d (point-or-2d point3)
        area (* 0.5 (magnitude (mapv * (mapv - point2-2d point1-2d) (mapv - point3-2d point1-2d))))] 
    (< area 1e-10)
    )
  )

(comment (area-of-triangle-2d [0 0 0] [0 10 0] [0 12 0]))

(deftest wall-locate-1-to-3-curve-for-polyhedron-control-point-is-collinear-test
  (testing "test that the point produced by wall-locate-1-to-3-curve-for-polyhedron-control-point is colinear with the original point and it's oppsite corner"
    ;; (let [rad-or-deg :radians 
    ;;       place (partial key-place lastcol cornerrow)
    ;;       post-position :bl
    ;;       dx 0
    ;;       dy -1 
    ;;       transform(get-transform-fn rad-or-deg place) 
    ;;       web-corner-translation-vector (get-single-plate-corner-position-vector post-position)
    ;;       top-position (get-web-post-position-top web-corner-translation-vector) 
    ;;       opposite-position (get-opposite-position post-position dx dy)
    ;;       opposite-web-corner-translation-vector (get-single-plate-corner-position-vector opposite-position) 
    ;;       web-post-position-top (vec (transform top-position))
    ;;       opposite-web-post-position-top (transform (get-web-post-position-top opposite-web-corner-translation-vector))
    ;;       vector-between-web-post-position-top-and-opposite (mapv - web-post-position-top opposite-web-post-position-top)
    ;;      point-on-tangent-from-plate (mapv + opposite-web-post-position-top (mapv (partial * 1.075) vector-between-web-post-position-top-and-opposite))]
    ;;   (is (true?(are-points-collinear? web-post-position-top opposite-web-post-position-top point-on-tangent-from-plate)));--
    ;;   )
    (let [valid-cols (range (inc lastcol))
          valid-rows (range (inc cornerrow))
          valid-dx-or-dy [-1 0 1]
          valid-post-position [:tl :tr :bl :br ; :tm :bm :rm :lm
                              ]
          ]
      (doseq [index (range 20)
              :let [col (rand-nth valid-cols)
                    row (rand-nth valid-rows)
                    place (partial key-place col row)
                    dx (rand-nth valid-dx-or-dy)
                    dy (rand-nth valid-dx-or-dy)
                    post-position (rand-nth valid-post-position)
                    points (wall-brace-polyhedron-points place dx dy post-position :radians)]] 
            ;;  (is (true? (colinear (:web-post-position-top points) (:opposite-web-post-position-top points) (:point-on-tangent-from-plate points)))
                 
            ;;      (join ["col " col " roiw " row " dx " dx " dy " dy " post-position " ]))
             (is (true? (colinear-2d (:web-post-position-top points) (:opposite-web-post-position-top points) (:wall-locate-1-to-3-curve-for-polyhedron-control-point points)))

                 (join ["col " col " roiw " row " dx " dx " dy " dy " post-position " 
                        ":wall-locate-1-to-3-curve-for-polyhedron-control-point-point " (:wall-locate-1-to-3-curve-for-polyhedron-control-point points)]))
        ))
    
    )
  )

(def valid-place-key-words (s/gen #{:tl :tr :bl :br}))

(s/def ::ks key-place)
(def valid-columns (s/gen (s/int-in 0 (inc lastcol))))
(def valid-rows (s/gen (s/int-in 0 (inc cornerrow))))
(def v-place (s/gen (s/cat :b #{(partial key-place (s/int-in 0 (inc lastcol)) (s/int-in 0 (inc cornerrow))) } ;:a #{key-place} :column (s/int-in 0 (inc lastcol)) :rows (s/int-in 0 (inc cornerrow))
                           )))
(gen/sample v-place)
(def valid-rad-or-deg (s/gen #{:radians :degrees}))
(def valid-xy (s/gen #{wall-xy-offset wall-xy-offset-mid wall-xy-offset-medium-thin wall-xy-offset-thin}))


(s/def :case-low-polyhedron-functions/web-post-position-top vector?)
(s/def :case-low-polyhedron-functions/opposite-web-post-position-top vector?)
(s/def :case-low-polyhedron-functions/point-on-tangent-from-plate (partial are-points-collinear? :case-low-polyhedron-functions/web-post-position-top :case-low-polyhedron-functions/opposite-web-post-position-top))



(d/fdef key-place )

(comment (println (partial key-place 1 2)))

(gen/sample valid-place-key-words 8)

(s/fdef wall-brace-polyhedron-points  
  :args (s/cat :case-low-polyhedron-functions/place v-place :dx number? :dy number? :post-positon keyword?  :rad-or-deg keyword? :xy number?)
  :ret (s/keys :req [:case-low-polyhedron-functions/web-post-position-top :case-low-polyhedron-functions/opposite-web-post-position-top :case-low-polyhedron-functions/point-on-tangent-from-plate]))


(defn num-sort [coll]
  (sort coll))

(s/fdef num-sort
  :args (s/cat :coll (s/coll-of number?))
  :ret  (s/coll-of number?)
  )

(defn vector-magnitude-2 [vector]
  (sqrt (reduce + (mapv #(pow % 2) vector))))

(s/fdef vector-magnitude-2 
  :args (s/cat :vector vector?)
  :ret int?
  )
(pp/pprint
 (stest/check `num-sort))

(pp/pprint
 (stest/check `vector-magnitude-2))

  (stest/check `wall-brace-polyhedron-points)


;(s/def ::place [partial key-place ])
;(s/def ::post-position (s/keys :req-un))