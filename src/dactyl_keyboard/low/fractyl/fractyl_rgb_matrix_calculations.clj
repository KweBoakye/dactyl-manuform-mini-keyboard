(ns dactyl-keyboard.low.fractyl.fractyl-rgb-matrix-calculations
  (:refer-clojure :exclude [use import])
  (:require 
   [dactyl-keyboard.low.shape-parameters-low :refer :all]
   [dactyl-keyboard.low.placement-functions-low :refer :all]
   [dactyl-keyboard.low.thumbs-low :refer :all]
   
   [clojure.string :as string]))
(def all-keys-matrix
  (conj (filterv not-empty (into [] (for [row rows]
                                      (into [] (for [column columns
                                                     :when (check-last-row-middle-and-fourth-keys-only column row)
                                                     :let [position (vec (key-position column row [0 0 0]))
                                                           matrix-pos [column row]]]
                                                 position)))))
        (vec (for [thumb-pos [thumb-bl-place
                              thumb-tl-place
                              thumb-tr-place]]
               (vec (transform-position thumb-pos [0 0 0]))))))
(comment 
  (conj (filterv not-empty(into [](for [row rows]
   (into [] (for [column columns 
        :when (check-last-row-middle-and-fourth-keys-only column row)
        :let [position (vec (key-position column row [0 0 0]))
              matrix-pos [column row]]]
    position)))))
          (vec (for [thumb-pos [thumb-bl-place
                           thumb-tl-place
                           thumb-tr-place]]
            (vec (transform-position thumb-pos [0 0 0]))))))

(def all-keys-matrix-2d
  (mapv (fn [row]
          (mapv #(vec (drop-last %)) row)) all-keys-matrix)
  )

(comment (nth all-keys-matrix 0))
(comment all-keys-matrix-2d)
(def all-keys 
  (->> (concat 
        (for [column columns
              row rows
              :when (check-last-row-middle-and-fourth-keys-only column row)
              :let [position (vec (key-position column row [0 0 0]))
                    matrix-pos [column row]]]
          position)
        (for [thumb-pos [thumb-bl-place
                         thumb-tl-place
                         thumb-tr-place]]
          (vec (transform-position thumb-pos [0 0 0])))
        )
       (mapv vec )
       (into []))
  )


(def max-x (->>(mapv #(nth % 0) all-keys)
            (apply max)))
(def min-x (->> (mapv #(nth % 0) all-keys)
                (apply min)))
(def max-y (->> (mapv #(nth % 1) all-keys)
                (apply max)))
(def min-y (->> (mapv #(nth % 1) all-keys)
                (apply min)))

(def rgb-matrix-x-max 224)
(def rgb-matrix-y-min 64)

(def x-scale-value (/ rgb-matrix-x-max (- max-x min-x)))
(defn scale-x [value]
  (* (- value min-x) x-scale-value))
(def y-scale-value (/ rgb-matrix-y-min (- min-y max-y)))
(defn scale-y [value]
  (abs(* (- value max-y) y-scale-value)))

(defn key-2d-pos-to-rgb-matrix-pos [key-2d-pos]
  [(scale-x (nth key-2d-pos 0)) (scale-y (nth key-2d-pos 1))])

(defn rgb-matrix-pos-string [pos]
  
  (str "{" (string/join ", " (mapv #(format "%.2f" %) pos))"}")
  )

(defn rgb-matrix-row-string [rgb-matrix-row]
  (str (string/join ", " rgb-matrix-row ) "," "\n" )
  )

(defn rgb-matrix-matrix-string [s]
  (printf (str (string/join "" s ) ))
  )

(defn rgb-matrix [&{:keys [side] :or {side :right}}]
  (let [positions (if (= side :left ) (mapv #(vec (reverse %)) all-keys-matrix-2d)
                    all-keys-matrix-2d)
        r #(mapv (fn [pos] (->> pos (key-2d-pos-to-rgb-matrix-pos)
                                (rgb-matrix-pos-string))) %)]
    (rgb-matrix-matrix-string (->> (mapv r positions)
                                   (mapv (fn [row-s] (rgb-matrix-row-string row-s))))))
  )

(defn right-rgb-matrix []
  (rgb-matrix)
  )

(defn left-rgb-matrix []
  (rgb-matrix :side :left))

(comment (right-rgb-matrix))
(comment (left-rgb-matrix))


(defn format-led-flags-row [row]
  (str (string/join ", " row) "," "\n") 
  )
(defn led-flags [] 
   (printf (str "{"(->> (conj
        (filterv not-empty 
                 (into [] (for [row rows]
                                      (into [] 
                                            (for [column columns
                                                     :when (check-last-row-middle-and-fourth-keys-only column row)]
                                                 4)))))
        (vec (for [thumb-pos [thumb-bl-place
                         thumb-tl-place
                         thumb-tr-place]]
          4))) 
       (into [])
        (mapv format-led-flags-row)
        (string/join "" ))
        "}"))
  )

(comment (led-flags))
(comment (key-2d-pos-to-rgb-matrix-pos (transform-position thumb-bl-place [0 0 0])))
(comment (->>(transform-position thumb-bl-place [0 0 0])
          (key-2d-pos-to-rgb-matrix-pos )
          (rgb-matrix-pos-string)))
(comment (mapv #(vec (reverse %)) all-keys-matrix-2d))
(comment (rgb-matrix :side :left))
(comment (let [r #(mapv (fn [pos] (->> pos (key-2d-pos-to-rgb-matrix-pos)
                                       (rgb-matrix-pos-string))) %)]
           (rgb-matrix-matrix-string (->> (mapv r all-keys-matrix-2d)
                (mapv (fn [row-s] (rgb-matrix-row-string row-s)))))))

(comment (rgb-matrix-row-string(mapv #(->> % (key-2d-pos-to-rgb-matrix-pos)
                     (rgb-matrix-pos-string))(nth all-keys-matrix-2d 0))))
(comment (scale-x min-x))
(comment (scale-y min-y))
(comment max-x)
(comment (count all-keys))

(comment
  (for [column columns
        row rows
        :when (check-last-row-middle-and-fourth-keys-only column row)
        :let [position (vec (key-position column row [0 0 0]))
              matrix-pos [column row]]]
    [matrix-pos position]))

(comment
  (->> (for [column columns
             row rows
             :when (check-last-row-middle-and-fourth-keys-only column row)
             :let [position (vec (key-position column row [0 0 0]))
                   matrix-pos [column row]]]
         position)
       (into [])
       (mapv #(nth % 0))
       (apply max)
   ;(max )
       ))

(comment
  (->> (for [column columns
             row rows
             :when (check-last-row-middle-and-fourth-keys-only column row)
             :let [position (vec (key-position column row [0 0 0]))
                   matrix-pos [column row]]]
         position)
       (into [])
       (mapv #(nth % 1))
       (apply max)
   ;(max )
       ))

(comment
  (->> (for [thumb-pos [thumb-bl-place
                        thumb-tl-place
                        thumb-tr-place]]
         (vec (transform-position thumb-pos [0 0 0])))
       (into [])))

(comment
  (->> (for [thumb-pos [thumb-bl-place
                        thumb-tl-place
                        thumb-tr-place]]
         (vec (transform-position thumb-pos [0 0 0])))
       (into [])
       (mapv #(nth % 1))
       (apply min)))

(comment
  (->> (for [thumb-pos [thumb-bl-place
                        thumb-tl-place
                        thumb-tr-place]]
         (vec (transform-position thumb-pos [0 0 0])))
       (into [])
       (mapv #(nth % 0))
       (apply min)))