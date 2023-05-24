(ns dactyl-keyboard.lib.openscad.openscad-formatting 
  (:require [clojure.core.matrix :refer [dimensionality]]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]
            [clojure.pprint :refer [cl-format]]
            [clojure.string :as string]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vector-conversion :refer [matrix-to-scad nested-vec-to-nested-scad-vec vec-to-scad-vec]]))

(defn format-param-name [param-name]
  (string/replace param-name  #"-" "_"))

(defn format-keyword [keyword]
  (format-param-name (name keyword)))

(defn format-assign [param-name arg-format arg]
  (format (string/join [param-name " = " arg-format]) arg))

(comment (format-assign "this" "%b" true))
(defmulti format-arg (fn [param-name arg] [(type arg)]))

(comment (type true))
(defmethod format-arg [Boolean]
  [param-name arg] (format-assign param-name "%b" arg)
  )

(comment (string/join ["hi" " = " (format "%s" "hi")]))
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
      (cl-format nil  (string/join [param-name "= " "~A"]) arg)))

(defn format-decimal-arg [param-name decimal-arg]
  (format-assign param-name "%f" decimal-arg))

(defmethod format-arg [Double]
  [param-name double-arg]
  (format-decimal-arg param-name double-arg))

(defmethod format-arg [Long]
  [param-name long-arg]
  (format-assign param-name "%d" long-arg))
(comment (type 1))
  
  (comment (let [lol 6
                 param lol]
              (name param)))
  

  (comment (let [param-name "hi"](string/join [param-name " =" "\"%s\""])))

(defn format-position-arg [arg]
  (let [format-string (format-arg "" arg)]
    (->>(string/replace format-string #"=" "" )
     (string/trim)))
  )

(comment (format-position-arg 10))
  (defn format-escaped-string [param-name param]
    (format 
     (string/join [param-name " =" "\"%s\""]) 
            param) 
    )



(comment  (format-keyword :fast-distance)
          )
(comment( format-escaped-string "yo" "hi"))
(comment (write-scad (call :hey (format "yo =%s" "hi"))))
(comment (spit "things-low/string-prnt.scad"(write-scad (call :hey (format-escaped-string "yo" "hi"))))
  )
(comment (string/join ["hi" "= \"%s\""]))


(comment (format-arg "yo" [[[0 0 0] [0 0 0]]
                           [[0 0 0] [0 0 0]]]))
(comment (type [0 0 0]))
(comment )
(comment (let [hh "hi"]
           (string/join [hh " = "])))