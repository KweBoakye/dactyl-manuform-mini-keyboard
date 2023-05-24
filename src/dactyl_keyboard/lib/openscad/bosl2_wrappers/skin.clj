(ns dactyl-keyboard.lib.openscad.bosl2-wrappers.skin
  (:refer-clojure :exclude [use import])
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.string :as string]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.constants :refer [CENTER include-bosl2]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.transforms :refer :all]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vector-conversion :refer :all]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vnf :refer :all]
            [dactyl-keyboard.lib.openscad.openscad-formatting :refer :all] 
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all])
  )
(keyword "length")
(keyword "segment")

(defn get-sampling-string-from-keyword [sampling-keyword]
  (case sampling-keyword
    :length "length"
    :segment "segment"
    )
  )

(keyword "distance")
(keyword "fast-distance")
(keyword "tangent")
(keyword "direct")
(keyword "reindex")

(comment (string/replace "fast-distance" #"-" "_"))

(keyword "hull")
(keyword "intersect")

(keyword "centroid")
(keyword "mean")
(keyword "box")

(defn skin [profiles &{:keys [slices refine sampling closed caps method z convexity anchor spin orient atype cp style]
                       :or {slices 10 refine 1 sampling :length  closed false  method :direct convexity 10 anchor CENTER spin 0  atype :hull cp :centroid style :min-edge}} ]
  (let [caps-value (if caps caps (false? closed))
        style-string (get-vnf-vertex-array-style-from-keyword style)]
    (call-module :skin  (mapv (fn [profile]  (string/replace (write-scad profile) #";" ""))profiles)
                 (format-arg "slices" slices) (format-arg "refine" refine) (format-escaped-string  "sampling" (get-sampling-string-from-keyword sampling)) (format-arg "caps" caps-value) (format-escaped-string "method" (format-keyword method)) (format-arg "z" z) (format-arg "convexity" convexity) (cl-format nil "anchor = ~A" (vec-to-scad-vec anchor) ) (format-arg "spin" spin)
                 (cond orient (format-arg "orient" orient)) (format-escaped-string "atype" (format-keyword atype)) (format-escaped-string "cp" (format-keyword cp))  (format-escaped-string "style" style-string))))

(defn linear-sweep [region &{:keys [module-or-function height center twist scale shift slices maxseg
                                    texture tex-size tex-counts tex-inset 
                                    tex-scale tex-samples style caps cp 
                                    atype anchor spin orient]
                             :or {module-or-function :module height 1 center false twist 0 scale 1 shift [0 0] tex-size [5,5] tex-inset false tex-samples 8 style :default caps true cp :centroid atype :hull anchor CENTER spin 0 }}]
  (let [call-fn (case module-or-function
                  :module call-module
                  :function call)
        converted-region (case module-or-function
                           :module  (string/replace (write-scad region) #";" "")
                           :function (format-position-arg region)) ]
    (call-fn :linear_sweep  converted-region (format-arg "height" height) (format-arg "center "center) (format-arg "twist" twist)
               (format-arg "scale" scale) (format-arg "shift" shift)
               (cond slices (format-arg "slices" slices)) 
               (cond maxseg (format-arg "maxseg" maxseg))
                (cond texture (do (format-arg texture) (format-arg "tex_size" tex-size) (cond tex-counts (format-arg "tex_counts" tex-counts)) (format-arg "tex_inset" tex-inset) (cond tex-scale (format-arg "tex_scale" tex-scale)) (format-arg "tex_samples" tex-samples)  )) 
               (format-escaped-string "style" (format-keyword style)) (format-arg "caps" caps) (format-escaped-string "cp" (format-keyword cp)) (format-escaped-string "atype" (format-keyword atype)) (cond orient (format-arg "orient" orient))(cl-format nil "anchor = ~A" (cond anchor(vec-to-scad-vec anchor))) (format-arg "spin" spin) 
               
               )))

(comment ;(write-scad 
          (format-position-arg (vec (apply concat ananse)))
  ;);(string/replace (write-scad ) #";" "")
         )

;; (comment(spit "things-low/skin-test.scad" (write-scad 
;;                                            (include include-bosl2)
;;                                         ;;    (skin [(circle 4) (circle 8)] :z [0 3] :slices 10 )
;;                                            (vnf-polyhedron ;(down 2 (vnf-bend 
;;                                                             ;(up 2
;;                                                                 (linear-sweep (mapv #(mapv * [0.1 0.1] %) ananse)  
;;                                                                               :slices 10
;;                                                                               :height 0.5 
;;                                                                               :module-or-function :function
;;                                                                               :scale 0.8) 
;;                                                                 ;:module-or-function :function) 
;;                                                             ; :axis "X" ) :module-or-function :function)
;;                                                            )
;;                                            )))

