(ns dactyl-keyboard.lib.openscad.openscad-formatting 
  (:require [clojure.core.matrix :refer [dimensionality]]
            [clojure.pprint :refer [cl-format]]
            [clojure.string :refer [join]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vector-conversion :refer [matrix-to-scad nested-vec-to-nested-scad-vec vec-to-scad-vec]]))

(defn format-assign [param-name arg-format arg]
  (format (join [param-name " = " arg-format]) arg))

(comment (format-assign "this" "%b" true))
(defmulti format-arg (fn [param-name arg] [(type arg)]))

(comment (type true))
(defmethod format-arg [Boolean]
  [param-name arg] (format-assign param-name "%b" arg)
  )

(comment (join ["hi" " = " (format "%s" "hi")]))
(comment (dimensionality [[[0 0 0]
                          [0 0 0]]]))

(defmethod format-arg [String]
  [param-name string-arg] (format-assign param-name "%s" string-arg)
  )

  (defmethod format-arg [clojure.lang.PersistentVector]
    [param-name vector-arg] 
    (let [arg (case (dimensionality vector-arg)
                1 (vec-to-scad-vec vector-arg)
                2 (nested-vec-to-nested-scad-vec vector-arg)
                3 (matrix-to-scad vector-arg))]
      (cl-format nil  (join [param-name "= " "~A"]) arg)))

(defn format-decimal-arg [param-name decimal-arg]
  (format-assign param-name "%f" decimal-arg))

(defmethod format-arg [Double]
  [param-name double-arg]
  (format-decimal-arg param-name double-arg))

(defmethod format-arg [Long]
  [param-name long-arg]
  (format-assign param-name "%d" long-arg))
(comment (type 1))

(comment (format-string-quoted "yoo" "hi ")
  )



(comment (format-arg "yo" [[[0 0 0] [0 0 0]]
                           [[0 0 0] [0 0 0]]]))
(comment (type [0 0 0]))
(comment )
(comment (let [hh "hi"]
           (join [hh " = "])))