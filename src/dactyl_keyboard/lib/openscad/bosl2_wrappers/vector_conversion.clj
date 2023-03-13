(ns dactyl-keyboard.lib.openscad.bosl2-wrappers.vector-conversion
  (:refer-clojure :exclude [use import])
  (:require [clojure.string  :as string])
  )

(defn vec-to-scad-vec [vec]
  (str "[" (string/join ", " vec) "]"))

(defn nested-vec-to-nested-scad-vec [nested-vec]
  (vec-to-scad-vec (mapv (partial vec-to-scad-vec) nested-vec))
  ;(str (mapv #(vec-to-scad-vec (mapv (partial vec-to-scad-vec) %) ) [nested-vec])) 
  )

(defn matrix-to-scad [matrix]
  (vec-to-scad-vec (mapv (partial nested-vec-to-nested-scad-vec)  matrix)))

(comment
  (nested-vec-to-nested-scad-vec [[0 0 0] [0 0 0]]))

(comment
  (matrix-to-scad  [[[0 0 0] [0 0 0]] [[0 0 0] [0 0 0]]]))
(comment
  (for [row [[[0 0 0] [0 0 0]] [[0 0 0] [0 0 0]]]]
    (vec-to-scad-vec (for [coordinate row] (vec-to-scad-vec coordinate))))
  ;(mapv #(vec-to-scad-vec (mapv (partial vec-to-scad-vec) %) ) [[[0 0 0] [0 0 0]]]) 
  )