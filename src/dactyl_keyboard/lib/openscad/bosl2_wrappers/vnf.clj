(ns dactyl-keyboard.lib.openscad.bosl2-wrappers.vnf
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all] 
            [clojure.pprint :refer [cl-format]]
            [clojure.string  :as string]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.vector-conversion :refer :all]))



(keyword "default")
(keyword "alt")
(keyword "min-edge")
(keyword "quincunx")
(keyword "convex")
(keyword "concave")

(defn filter-nil-args [scad-func &{:keys [args-index] :or {args-index 2}}]
  (map-indexed 
   (fn [index element] (if (= index args-index)
                        (keep #(if (not= % nil) %) element)
                        element))
   scad-func
   )
  )
(def default-vnf-vertex-array-args {:caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default})

(defn vnf-vertex-array-args [&{:keys [caps  cap1  cap2 col-wrap  row-wrap  reverse  style ] :or {caps true cap1 false cap2 false col-wrap true row-wrap false reverse false style :default}}]
  {:caps caps :cap1 cap1 :cap2 cap2 :col-wrap col-wrap :row-wrap row-wrap :reverse reverse :style style})

(defn get-vnf-vertex-array-style-from-keyword [style-keyword]
   (case style-keyword
                 :default "default"
                 :alt "alt"
                 :min-edge "min_edge"
                 :quincunx "quincunx"
                 :convex "convex"
                 :concave "concave"))

(defn vnf-vertex-array [points  &{:keys [^Boolean caps ^Boolean cap1 ^Boolean cap2 ^Boolean col-wrap ^Boolean row-wrap ^Boolean reverse ^Boolean style]
                                   :or {caps true cap1 false cap2 false col-wrap true row-wrap false reverse false style :default}}]
  ;; (assert (and (false? (or caps cap1 cap2)) (false? col-wrap)) "col_wrap must be true if caps are requested")
  ;; (assert (and (false? (or caps cap1 cap2)) row-wrap ) "Cannot combine caps with row_wrap")
  ;; (assert (some #(= some style %) [:default :alt :quincunx :convex :concave :min_edge]))
  ;; ;(assert (matrix? ))

  (let [points-for-scad (matrix-to-scad points)
        style-string (get-vnf-vertex-array-style-from-keyword style)]
    (map-indexed (fn [index element] (if (= index 2)
                                       (keep #(if (not= % nil) %) element)
                                       element)) 
         (call :vnf_vertex_array  (cl-format nil "points = ~A" points-for-scad) (format "caps =  %b" caps)
          (cond (false? caps) (format "cap1 =  %b" cap1)) (cond (false? caps) (format "cap2 =  %b" cap2))
          (format "col_wrap =  %b" col-wrap) (format "row_wrap =  %b" row-wrap) (format "reverse =  %b" reverse) 
          (format "style =\"%s\"" style-string)))))

(defn vnf-tri-array [points &{:keys [row-wrap reverse] :or {row-wrap false  reverse false}}]
  (let [points-for-scad (matrix-to-scad points)]
    (call :vnf_tri_array (cl-format nil "points = ~A" points-for-scad) (format "row_wrap =  %b" row-wrap) (format "reverse =  %b" reverse)) 
    ) 
  )

(comment  (vnf-vertex-array [[[0 0 0] [0 4 0] [0 8 -3] [0 10 -3]]
                             [[2 0 6] [2 4 0] [2 8 0] [2 10 0]]
                             [[4 0 0] [4 4 0] [4 8 3] [4 10 3]]
                             [[6 0 0] [6 4 -3] [6 8 0] [6 10 0]]
                             [[8 0 0] [8 4 -3] [8 8 0] [8 10 0]]]
                            :caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default
                            ) )

(comment (mapv #(vec (apply concat %))  (partition 2 (interleave 
                      (concat [[[0 0 0] [0 4 0] [0 8 -3] [0 10 -3]]
                [[2 0 6] [2 4 0] [2 8 0] [2 10 0]]
                [[4 0 0] [4 4 0] [4 8 3] [4 10 3]]
                [[6 0 0] [6 4 -3] [6 8 0] [6 10 0]]
                [[8 0 0] [8 4 -3] [8 8 0] [8 10 0]]] 
               [[[0 0 0] [0 4 0] [0 8 -3] [0 10 -3]]
                [[2 0 6] [2 4 0] [2 8 0] [2 10 0]]
                [[4 0 0] [4 4 0] [4 8 3] [4 10 3]]
                [[6 0 0] [6 4 -3] [6 8 0] [6 10 0]]
                [[8 0 0] [8 4 -3] [8 8 0] [8 10 0]]]))) )
)

(comment (partition 3 (apply interleave
                        [[[[0 0 0] [0 4 0] [0 8 -3] [0 10 -3]]
                                [[2 0 6] [2 4 0] [2 8 0] [2 10 0]]
                                [[4 0 0] [4 4 0] [4 8 3] [4 10 3]]
                                [[6 0 0] [6 4 -3] [6 8 0] [6 10 0]]
                                [[8 0 0] [8 4 -3] [8 8 0] [8 10 0]]]
                               [[[0 0 0] [0 4 0] [0 8 -3] [0 10 -3]]
                                [[2 0 6] [2 4 0] [2 8 0] [2 10 0]]
                                [[4 0 0] [4 4 0] [4 8 3] [4 10 3]]
                                [[6 0 0] [6 4 -3] [6 8 0] [6 10 0]]
                                [[8 0 0] [8 4 -3] [8 8 0] [8 10 0]]]
                       [[[0 0 0] [0 4 0] [0 8 -3] [0 10 -3]]
                        [[2 0 6] [2 4 0] [2 8 0] [2 10 0]]
                        [[4 0 0] [4 4 0] [4 8 3] [4 10 3]]
                        [[6 0 0] [6 4 -3] [6 8 0] [6 10 0]]
                        [[8 0 0] [8 4 -3] [8 8 0] [8 10 0]]]])))
(defn format-vnf-as-argument [vnf]
  (string/replace (write-scad vnf) #";" ""))

(defn vnf-join [vnf-list]
  (let [vnf-list-formatted-for-scad (vec-to-scad-vec (mapv #(string/replace (write-scad %) #";" "") vnf-list))]
    (call :vnf_join vnf-list-formatted-for-scad)))

(defn vnf-reverse-faces [vnf]
  (let [vnf-string (string/replace (write-scad vnf) #";" "")]
    (call :vnf_reverse_faces vnf-string)))

(defn vnf-merge-points [vnf & {:keys [eps] :or {eps "EPSILON"}}]
  (call :vnf_merge_points (format-vnf-as-argument vnf) eps))

(defn vnf-drop-unused-points [vnf]
  (call :vnf_drop_unused_points (format-vnf-as-argument vnf)))

(defn vnf-triangulate [vnf]
  (call :vnf_triangulate (format-vnf-as-argument vnf)))

(defn vnf-slice [vnf dir cuts]
  (call :vnf_slice (format-vnf-as-argument vnf) (format "dir = \"%s\"" dir) (cl-format nil "points = ~A" (vec-to-scad-vec cuts))))



(defn vnf-polyhedron [vnf & {:keys [convexity  cp  anchor  spin orient atype multiple-vnf]
                             :or {convexity 2 cp  "centroid" anchor  "origin" spin 0 orient "UP" atype  "hull" multiple-vnf false}}]

  (let [vnf-string  (if multiple-vnf   (mapv #(string/replace (write-scad %) #";" "") vnf) (string/replace (write-scad vnf) #";" "")) 
        vnf-format (if multiple-vnf
                     (format "vnf = [%s]"(string/join ","(for [index (range (count vnf-string))]
                                                     (format "%s" (nth vnf-string index)))))
                     (format "vnf = %s" vnf-string))]
    ;(println "vnf-string" vnf-string)
    ;(println "vnf-format" vnf-format)
    (call-module :vnf_polyhedron vnf-format (format "convexity = %d" convexity)  (format "cp = \"%s\"" cp)
                 (format "anchor = \"%s\"" anchor) (format "spin = %d" spin) (format "orient = %s" orient)
                 (format "atype = \"%s\"" atype))))

(comment (let [vnf (vnf-vertex-array [[[0 0 0] [0 4 0] [0 8 -3] [0 10 -3]]
                                      [[2 0 6] [2 4 0] [2 8 0] [2 10 0]]
                                      [[4 0 0] [4 4 0] [4 8 3] [4 10 3]]
                                      [[6 0 0] [6 4 -3] [6 8 0] [6 10 0]]
                                      [[8 0 0] [8 4 -3] [8 8 0] [8 10 0]]]
                                     :caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default)]
           (write-scad (vnf-polyhedron vnf :multiple-vnf false))))

(defn vnf-wireframe [vnf &{:keys [width] :or {width 1}}]
  (call-module :vnf_wireframe (format-vnf-as-argument vnf) (format "width = %s" width)))

(defn vnf-volume [vnf]
  (call :vnf_volume (format-vnf-as-argument vnf)))

(defn vnf-halfspace [plane vnf & {:keys [closed boundary] :or {closed true boundary false}}]
  (call :vnf_halfspace (vec-to-scad-vec plane) (format-vnf-as-argument vnf) (format "closed = %b" closed) (format "boundary = %b" boundary)))

;(defn vnf-halfspace [vnf r {:keys [d axis] :or {d axis}}])

(defn debug-vnf [vnf &{:keys [faces vertices opacity convexity size filter] :or {faces true vertices true opacity 0.5 convexity 6 size 1 filter nil}}]
  (println (write-scad (filter-nil-args
            (call :debug_vnf (format-vnf-as-argument vnf) (format "faces = %b" faces) (format "vertices = %b" vertices) (format "opacity = %f" opacity) (format "convexity = %d" convexity) (format "size = %d" size) (if (false? (nil? filter)) filter)))))
  (filter-nil-args
   (call :debug_vnf (format-vnf-as-argument vnf) (format "faces = %b" faces) (format "vertices = %b" vertices) (format "opacity = %f" opacity) (format "convexity = %d" convexity) (format "size = %d" size) (if (false? (nil? filter)) filter))))

(defn vnf-validate [vnf &{:keys [size show-warns check-isects opacity label-verts label-faces wireframe adjacent] 
                               :or {size 1 show-warns true check-isects false opacity 0.67  label-verts false label-faces false wireframe false adjacent false}}]
  
   (call :vnf_validate (format-vnf-as-argument vnf) (format "size = %d" size) (format "show_warns = %b" show-warns) (format "check_isects = %b" check-isects)
         (format "opacity = %f" opacity) (format "label_verts = %b" label-verts) (format "label_faces = %b" label-faces)  (format "wireframe = %b" wireframe) (format "adjacent = %b" adjacent)
         ))
(comment  (debug-vnf (vnf-vertex-array [[[0 0 0] [0 4 0] [0 8 -3] [0 10 -3]]
                             [[2 0 6] [2 4 0] [2 8 0] [2 10 0]]
                             [[4 0 0] [4 4 0] [4 8 3] [4 10 3]]
                             [[6 0 0] [6 4 -3] [6 8 0] [6 10 0]]
                             [[8 0 0] [8 4 -3] [8 8 0] [8 10 0]]]
                            :caps true :cap1 false :cap2 false :col-wrap true :row-wrap false :reverse false :style :default)))

(defn vnf-from-region [region]
  (call :vnf_from_region region))

(defn vnf-bend [vnf &{:keys [r d axis] :or {axis "Z"}
                      }]
  (call :vnf_bend (format-vnf-as-argument vnf) (cond r (format "r = %f" (double r)))
        (cond d (format "d = %f" (double d))) (format "axis = \"%s\"" axis))
  )