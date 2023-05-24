(ns dactyl-keyboard.lib.openscad.bosl2-wrappers.transforms
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [include-bosl2]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vector-conversion :refer [vec-to-scad-vec]]
            [dactyl-keyboard.lib.openscad.openscad-formatting :refer :all]
            [clojure.string :as string]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]))

(defn transformation [module-or-function-name movement-arg p &{:keys [module-or-function] :or {module-or-function :function}}]
 (let [formatted-movement-arg (if (vector? movement-arg) (vec-to-scad-vec movement-arg)
                        (format-position-arg movement-arg))](case module-or-function
   :module (call-module-with-block module-or-function-name formatted-movement-arg  p
                                   )
   :function (call module-or-function-name formatted-movement-arg  (format "p = %s" (string/replace (write-scad p) #";" ""))))) 
  )

(defn move [move p &{:keys [module-or-function] :or {module-or-function :function}}]
 (transformation :move move p :module-or-function module-or-function) )

(defn left [x p & {:keys [module-or-function] :or {module-or-function :function}}]
  (transformation :left x p :module-or-function module-or-function))

(defn right [x p & {:keys [module-or-function] :or {module-or-function :function}}]
  (transformation :right x p :module-or-function module-or-function))

(defn up [z p & {:keys [module-or-function] :or {module-or-function :function}}]
  (transformation :up z p :module-or-function module-or-function))

(defn down [z p & {:keys [module-or-function] :or {module-or-function :function}}]
  (transformation :down z p :module-or-function module-or-function))

(defn fwd [y p & {:keys [module-or-function] :or {module-or-function :function}}]
(transformation :fwd y p :module-or-function module-or-function))

(defn back [y p & {:keys [module-or-function] :or {module-or-function :function}}]
  (transformation :back y p :module-or-function module-or-function))

(comment (spit "things-low/move-test.scad"
               (write-scad
                (include include-bosl2)
                (back 10  (cube 10 10 10)))))

