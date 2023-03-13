(ns dactyl-keyboard.lib.openscad.polyhedrons
  (:refer-clojure :exclude [use import])
  (:require
   [scad-clj.scad :refer :all]
[scad-clj.model :refer :all]
   [dactyl-keyboard.lib.curvesandsplines.beziers :refer [bezier-linear bezier-quadratic]]
   [dactyl-keyboard.lib.vectors :refer [vec-if-not calculate-point-between-points]] 
   ))

(defn bezier-polyhedron-generate-front-or-back-faces [top-left-point-index top-right-point-index bottom-left-point-index bottom-right-point-index steps]
  (into [] (concat (for [index (range 0 steps)]
                     [(+ top-left-point-index index) (+ top-left-point-index (inc index)) (+ bottom-left-point-index  index)])
                   (for [index (range 0 steps)]
                     [(+ bottom-left-point-index index) (+ top-left-point-index (inc index)) (+ bottom-left-point-index (inc index))]))))

(defn bezier-polyhedron-generate-side-faces [top-left-point-index top-right-point-index bottom-left-point-index bottom-right-point-index]
  [[top-left-point-index bottom-right-point-index bottom-left-point-index]
   [top-left-point-index top-right-point-index bottom-right-point-index]])

(defn bezier-polyhedron-generate-bottom-faces [outside-lower-start   inside-lower-end steps]
  (into [] (concat (for [index (range 0 steps)]
                     [(+ outside-lower-start index) (+ outside-lower-start  (inc index)) (- inside-lower-end (inc index))])
                   (for [index (range 0 steps)]
                     [(+ outside-lower-start index) (- inside-lower-end (inc index)) (- inside-lower-end index)]))))

(defn bezier-polyhedron-generate-top-faces [outside-upper-start  inside-upper-end steps]
  (into [] (concat (for [index (range 0 steps)]
                     [(- inside-upper-end index) (+ outside-upper-start (inc index)) (+ outside-upper-start index)])
                   (for [index (range 0 steps)]
                     [(- inside-upper-end index) (- inside-upper-end (inc index)) (+ outside-upper-start (inc index))]))))

(defn bezier-polyhedron-generate-faces [outside-upper-start outside-upper-end outside-lower-start outside-lower-end inside-upper-start inside-upper-end inside-lower-start inside-lower-end steps]
  (let [front (bezier-polyhedron-generate-front-or-back-faces outside-upper-start outside-upper-end outside-lower-start outside-lower-end steps)
        back  (bezier-polyhedron-generate-front-or-back-faces inside-upper-start inside-upper-end inside-lower-start inside-lower-end steps)
        left (bezier-polyhedron-generate-side-faces inside-upper-end outside-upper-start inside-lower-end outside-lower-start)
        right (bezier-polyhedron-generate-side-faces outside-upper-end inside-upper-start outside-lower-end inside-lower-start)
        top   (bezier-polyhedron-generate-top-faces outside-upper-start inside-upper-end steps)
        bottom (bezier-polyhedron-generate-bottom-faces outside-lower-start inside-lower-end steps)]
    (into [] (concat front back left right top bottom))))

(defn generate-polyhedron-from-points [outside-upper-points outside-lower-points inside-upper-points inside-lower-points steps]
  (let [get-end-from-start (fn [start-index] (+ start-index  steps))
        outside-upper-start 0
        outside-upper-end (get-end-from-start outside-upper-start)
        outside-lower-start (inc outside-upper-end)
        outside-lower-end (get-end-from-start outside-lower-start)
        inside-upper-start (inc outside-lower-end)
        inside-upper-end (get-end-from-start inside-upper-start)
        inside-lower-start (inc inside-upper-end)
        inside-lower-end (get-end-from-start inside-lower-start)
        faces (bezier-polyhedron-generate-faces outside-upper-start outside-upper-end outside-lower-start outside-lower-end inside-upper-start inside-upper-end inside-lower-start inside-lower-end steps)
        points (into [] (concat outside-upper-points outside-lower-points inside-upper-points inside-lower-points))]
    (polyhedron points faces)))

(defn generate-bezier-quadratic-polyhedron-from-points ([outside-upper-left outside-upper-right outside-lower-left outside-lower-right inside-upper-left inside-upper-right inside-lower-left inside-lower-right steps]

                                                        (generate-bezier-quadratic-polyhedron-from-points outside-upper-left outside-upper-right outside-lower-left outside-lower-right
                                                                                                          inside-upper-left inside-upper-right inside-lower-left inside-lower-right
                                                                                                          (calculate-point-between-points outside-upper-left outside-upper-right [0 -2 0])
                                                                                                          (calculate-point-between-points outside-lower-left outside-lower-right [0 -2 0])
                                                                                                          (calculate-point-between-points inside-upper-left inside-upper-right [0 -2 0])
                                                                                                          (calculate-point-between-points inside-lower-left inside-lower-right [0 -2 0])
                                                                                                          steps))

  ([outside-upper-left outside-upper-right outside-lower-left outside-lower-right
    inside-upper-left inside-upper-right inside-lower-left inside-lower-right
    outside-upper-control-point outside-lower-control-point inside-upper-control-point inside-lower-control-point
    steps]
   (let [outside-upper-points (bezier-quadratic outside-upper-left outside-upper-control-point outside-upper-right steps)
         outside-lower-points (bezier-quadratic outside-lower-left outside-lower-control-point outside-lower-right steps)
         inside-upper-points  (bezier-quadratic inside-upper-right inside-upper-control-point inside-upper-left steps)
         inside-lower-points (bezier-quadratic inside-lower-right inside-lower-control-point inside-lower-left steps)]
     (generate-polyhedron-from-points outside-upper-points outside-lower-points inside-upper-points inside-lower-points steps))))

(defn generate-bezier-quadratic-polyhedron-from-points-and-control-vectors
  [outside-upper-left outside-upper-right outside-lower-left outside-lower-right inside-upper-left inside-upper-right inside-lower-left inside-lower-right
   steps
   {:keys [outside-upper-control-point-vector outside-lower-control-point-vector inside-upper-control-point-vector inside-lower-control-point-vector]}]
  (generate-bezier-quadratic-polyhedron-from-points outside-upper-left outside-upper-right outside-lower-left outside-lower-right inside-upper-left inside-upper-right inside-lower-left inside-lower-right
                                                    (calculate-point-between-points outside-upper-left outside-upper-right outside-upper-control-point-vector)
                                                    (calculate-point-between-points outside-lower-left outside-lower-right outside-lower-control-point-vector)
                                                    (calculate-point-between-points inside-upper-left inside-upper-right inside-upper-control-point-vector)
                                                    (calculate-point-between-points inside-lower-left inside-lower-right inside-lower-control-point-vector)
                                                    steps))

(defn bezier-along-bezier-polyhedron-generate-front-or-back-faces ([count-inner count-outer steps] (bezier-along-bezier-polyhedron-generate-front-or-back-faces count-inner count-outer steps 0))

  ([count-inner count-outer steps start-point]  (into [] (concat
                                                          (for [index-outer (range 0 (dec count-outer)) index-inner (range 0   (dec count-inner))]
                                                            [(+ (* index-outer count-inner) index-inner start-point) (+ (* (inc index-outer) count-inner) (inc index-inner) start-point) (+ (* index-outer count-inner) (inc index-inner) start-point)])
                                                          (for [index-outer (range 0  (dec count-outer)) index-inner (range 0   (dec count-inner))]
                                                            [(+ (* index-outer count-inner) index-inner start-point) (+ (* (inc index-outer) count-inner) index-inner start-point) (+ (* (inc index-outer) count-inner) (inc index-inner) start-point)])))))

(defn bezier-along-bezier-polyhedron-generate-side [index-start1 index-start2 steps]
  (apply concat (for [index (range 0  steps)]
                  [[(+ index-start1 index) (+ index-start2 index) (+ index-start2 (inc index))]
                   [(+ index-start1 index) (+ index-start2 (inc index)) (+ index-start1 (inc index))]])))

(defn bezier-along-bezier-polyhedron-generate-side-reverse [index-start1 index-start2 steps]
  (concat (apply concat (for [index (range 1   steps)]
                          [[(- index-start2 index) (+ index-start1 (inc index)) (+ index-start1  index)]
                           [(- index-start2 index) (- index-start2 (inc index))  (+ index-start1 (inc index))]]))
          [[(inc index-start1) index-start1   (dec index-start2)]]
          [[(+ index-start1  steps) (- index-start2 steps) (dec (- index-start2 steps))]]))

(defn bezier-along-bezier-polyhedron-generate-side-reverse-2 [index-start1 index-start2 steps]
  (apply concat (for [index (range 0   steps)]
                  [[(- index-start1 (inc index))  (- index-start1 index) (+ index-start2  index)]
                   [(- index-start1 (inc index)) (+ index-start2  index) (+ index-start2 (inc index))]])))

(defn bezier-along-bezier-polyhedron-generate-side-reverse-3 [index-start1 index-start2 steps size]
  (apply concat (for [index (range 0    steps)]
                  [[(+ index-start1 (* index size)) (+ index-start2  (* (inc index)  size)) (+ index-start1 (* (inc index) size))]
                   [(+ index-start1 (* index size)) (+ index-start2 (* index size)) (+ index-start2 (* (inc index) size))]])))

(defn bezier-along-bezier-polyhedron-generate-top [front-start front-end back-start back-end size]
  (let [front-end-extra (inc front-end)]
    (concat (for [index (range 0  (dec size))]
              [(- front-end-extra (* size index)) (+ back-start (* size (inc index))) (+ back-start (* size  index))])
            (for [index (range 0 (dec size))]
              [(- front-end-extra (* size index)) (- front-end-extra (* size (inc index))) (+ back-start (* size (inc index)))])
            [[front-start (- back-end  size) (+ front-start size)]])))

(defn bezier-along-bezier-polyhedron-generate-bottom [front-start back-end size]
  (apply concat (for [index (range 0 (dec size))
                      :let [front-start-less (dec front-start)
                            back-end-extra (dec back-end)]]
                  [[(+ front-start-less (* (inc index) size)) (- back-end-extra (* (inc index) size)) (- back-end-extra (*  index size))]
                   [(+ front-start-less (* (inc index) size)) (+ front-start-less (* (+ index 2) size)) (- back-end-extra (*  (inc index) size))]])))

(defn generate-bezier-along-bezier-polyhedron-faces [front-points back-points steps]
  (let [front-points-count (count front-points)
        back-points-count (count back-points)
        front-points-start 0
        front-points-end (dec front-points-count)
        back-points-start (inc front-points-end)
        back-points-end (+ back-points-start back-points-count)]
    (concat (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps)
            (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps back-points-start)
            (bezier-along-bezier-polyhedron-generate-side (- back-points-end (inc steps)) front-points-start   steps)
            (bezier-along-bezier-polyhedron-generate-side  (inc (- front-points-end  (inc steps))) back-points-start steps)
            (bezier-along-bezier-polyhedron-generate-top front-points-start front-points-end back-points-start back-points-end (inc steps))
            (bezier-along-bezier-polyhedron-generate-bottom front-points-start back-points-end   (inc steps)))))

(defn generate-bezier-along-bezier-polyhedron [outer-points inner-points steps]
  (polyhedron (into [] (concat  outer-points inner-points))
              (generate-bezier-along-bezier-polyhedron-faces outer-points inner-points steps)))



(defn generate-bezier-along-bezier-new-face [start-indexes new-indexes-start end-indexes steps]
  (concat
   (apply concat
          (for [index (range 0 steps)
                :let [steps-to-jump (dec steps)]]
            [[(nth start-indexes index) (+ new-indexes-start (* index steps-to-jump)) (+  new-indexes-start (* (inc index) steps-to-jump))]
             [(nth start-indexes index) (+  new-indexes-start (* (inc index) steps-to-jump)) (nth start-indexes (inc index))]]))
   (apply concat
          (for [index-outer (range 0 (- steps 2)) index-inner (range 0 steps)
                :let [steps-to-jump (dec steps)]]
            [[(+ new-indexes-start (* index-inner steps-to-jump) index-outer) (+ (inc new-indexes-start) (* index-inner steps-to-jump) index-outer) (+ (inc new-indexes-start) (* (inc index-inner) steps-to-jump) index-outer)]
             [(+ new-indexes-start (* index-inner steps-to-jump) index-outer) (+ (inc new-indexes-start) (* (inc index-inner) steps-to-jump) index-outer) (+ new-indexes-start (* (inc index-inner) steps-to-jump) index-outer)]]))

   (apply concat
          (for [index (range 0 steps)
                :let [start (+ new-indexes-start (- steps 2))
                      steps-to-jump (dec steps)]]
            [[(+ start (* index steps-to-jump)) (nth end-indexes index) (nth end-indexes (inc index))]
             [(+ start (* index steps-to-jump)) (nth end-indexes (inc index)) (+ start (* (inc index) steps-to-jump))]]))))
(defn generate-bezier-along-bezier-polyhedron-all-sides [outer-points inner-points steps]
  (let [points (into [] (concat  outer-points inner-points))
        front-points-count (count outer-points)
        back-points-count (count inner-points)
        front-points-start 0
        front-points-end (dec front-points-count)
        back-points-start (inc front-points-end)
        back-points-end-extra (+ back-points-start back-points-count)
        back-points-end (dec back-points-end-extra)
        get-side-points-indexes #(for [index (range 0 (inc steps))
                                       :let [start %]]
                                   (+ start index))
        get-top-and-bottom-points-indexes #(for [index (range 0 (inc steps))
                                                 :let [steps-to-jump (inc steps)]]
                                             (+ % (* index steps-to-jump)))
        generate-bezier-along-bezier-new-face-points  (fn [start-points end-points] (->>
                                                                                     (into []
                                                                                           (apply concat
                                                                                                  (for [index (range 0 (inc steps))]
                                                                                                    (bezier-linear
                                                                                                     (nth start-points index)
                                                                                                     (nth end-points index)
                                                                                                     steps))))
                                                                                     (remove #(some (fn [value] (=  % value)) (concat start-points end-points)))))
        front-start-side-points-indexes (get-side-points-indexes front-points-start)
        front-end-side-points-indexes (get-side-points-indexes  (inc (- front-points-end  (inc steps))))
        back-start-side-points-indexes (get-side-points-indexes back-points-start)
        back-end-side-points-indexes (get-side-points-indexes (- back-points-end-extra (inc steps)))
        front-bottom-points-indexes (get-top-and-bottom-points-indexes (+ front-points-start steps))
        front-top-points-indexes (get-top-and-bottom-points-indexes front-points-start)
        back-top-points-indexes (get-top-and-bottom-points-indexes back-points-start)
        back-bottom-points-indexes (get-top-and-bottom-points-indexes (+ back-points-start steps))
        get-points-from-indexes (fn [indexes] (map #(nth points %) indexes))
        front-start-side-points (get-points-from-indexes front-start-side-points-indexes)
        front-end-side-points  (get-points-from-indexes front-end-side-points-indexes)
        back-end-side-points (get-points-from-indexes back-end-side-points-indexes)
        back-start-side-points (get-points-from-indexes back-start-side-points-indexes)
        front-top-points (get-points-from-indexes front-top-points-indexes)
        front-bottom-points (get-points-from-indexes front-bottom-points-indexes)
        back-top-points (get-points-from-indexes back-top-points-indexes)
        back-bottom-points (get-points-from-indexes back-bottom-points-indexes)
        side-front-to-back-new-points (generate-bezier-along-bezier-new-face-points front-end-side-points back-start-side-points)
        side-back-to-front-new-points (generate-bezier-along-bezier-new-face-points back-end-side-points front-start-side-points)
        top-new-points (generate-bezier-along-bezier-new-face-points front-top-points (reverse back-top-points))
        bottom-new-points (generate-bezier-along-bezier-new-face-points (reverse front-bottom-points) back-bottom-points)
        side-front-to-back-new-points-start (inc back-points-end)
        side-front-to-back-new-points-end (dec (+ side-front-to-back-new-points-start (count side-front-to-back-new-points)))
        side-back-to-front-new-points-start (inc side-front-to-back-new-points-end)
        side-back-to-front-new-points-end (dec (+ side-back-to-front-new-points-start (count side-back-to-front-new-points)))
        top-new-points-start (inc side-back-to-front-new-points-end)
        top-new-points-end (dec (+ top-new-points-start (count top-new-points)))
        bottom-new-points-start (inc top-new-points-end)
        front (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps)
        back (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps back-points-start)
        side-back-to-front (generate-bezier-along-bezier-new-face back-end-side-points-indexes side-back-to-front-new-points-start front-start-side-points-indexes steps)
          ;(bezier-along-bezier-polyhedron-generate-side (- back-points-end-extra (inc steps)) front-points-start   steps)
        side-front-to-back (generate-bezier-along-bezier-new-face front-end-side-points-indexes side-front-to-back-new-points-start back-start-side-points-indexes steps)
          ;(bezier-along-bezier-polyhedron-generate-side  (inc (- front-points-end  (inc steps))) back-points-start steps)
        top (generate-bezier-along-bezier-new-face front-top-points-indexes top-new-points-start (reverse back-top-points-indexes) steps)
          ;(bezier-along-bezier-polyhedron-generate-top front-points-start front-points-end back-points-start back-points-end-extra (inc steps))
        bottom (generate-bezier-along-bezier-new-face    (reverse front-bottom-points-indexes) bottom-new-points-start  back-bottom-points-indexes steps)
          ;(bezier-along-bezier-polyhedron-generate-bottom front-points-start back-points-end-extra   (inc steps))
        points-extended (concat points side-front-to-back-new-points side-back-to-front-new-points top-new-points bottom-new-points)]
    (union
     (polyhedron points-extended
                 (concat front
                         back
                         side-back-to-front
                         side-front-to-back
                         top
                         bottom))
      ;;  (plot-bezier-points  back-end-side-points (sphere 0.1))
      ;;  ;(plot-bezier-points front-end-side-points (sphere 0.1))
      ;;  ;(plot-bezier-points back-end-side-points (sphere 0.1))
      ;;  (plot-bezier-points front-bottom-points (sphere 0.1))
      ;;  (plot-bezier-points back-top-points (sphere 0.1))
      ;;  (plot-bezier-points back-bottom-points (sphere 0.1))
      ;;  (plot-bezier-points side-front-to-back-new-points (sphere 0.1))
      ;;  (color [1 0 0 1](translate (nth points-extended side-front-to-back-new-points-start) (sphere 0.5)))
     )))


(defn generate-bezier-along-bezier-polyhedron-from-points-list-linear [outer-upper-points outer-lower-points
                                                                       inner-upper-points inner-lower-points
                                                                       steps]
  (let [points-fn (fn [upper lower]
                    (into [] (apply concat
                                    (for [index (range 0 (inc steps))]
                                      (bezier-linear
                                       (nth upper index)
                                       (nth lower index)
                                       steps)))))
        outer-points (points-fn outer-upper-points outer-lower-points)
        lower-points (points-fn inner-upper-points inner-lower-points)]
    (generate-bezier-along-bezier-polyhedron outer-points lower-points steps)))

(defn generate-bezier-along-bezier-polyhedron-from-points-list-linear-with-higher-resolution-top-and-bottom [outer-upper-points outer-lower-points
                                                                                                             inner-upper-points inner-lower-points
                                                                                                             steps]
  (let [points-fn (fn [upper lower]
                    (into [] (apply concat
                                    (for [index (range 0 (inc steps))]
                                      (bezier-linear
                                       (nth upper index)
                                       (nth lower index)
                                       steps)))))
        outer-points (points-fn outer-upper-points outer-lower-points)
        inner-points (points-fn inner-upper-points inner-lower-points)
        upper-points (points-fn  (reverse outer-upper-points) inner-upper-points)
        lower-points (points-fn outer-lower-points (reverse inner-lower-points))
        outer-side-edge-1 (bezier-linear (nth outer-upper-points steps) (nth outer-lower-points steps) steps)
        inner-side-edge-1 (bezier-linear (nth inner-upper-points 0) (nth inner-lower-points 0) steps)
        outer-side-edge-2 (bezier-linear (nth outer-upper-points 0) (nth outer-lower-points 0) steps)
        inner-side-edge-2 (bezier-linear (nth inner-upper-points steps) (nth inner-lower-points steps) steps)
        side-1-points (points-fn  inner-side-edge-1 outer-side-edge-1)
        side-2-points (points-fn outer-side-edge-2 inner-side-edge-2)
        outer-points-count (count outer-points)
        inner-points-count (count inner-points)
        upper-points-count (count upper-points)
        lower-points-count (count lower-points)
        side-1-points-count (count side-1-points)
        side-2-points-count (count side-2-points)
        outer-points-start 0
        outer-points-end (dec outer-points-count)
        inner-points-start (inc outer-points-end)
        inner-points-end (+ inner-points-start (dec inner-points-count))
        upper-points-start (inc inner-points-end)
        upper-points-end (+ upper-points-start (dec outer-points-count))
        lower-points-start (inc upper-points-end)
        lower-points-end (+ lower-points-start (dec lower-points-count))
        side-1-points-start (inc lower-points-end)
        side-1-points-end (+ side-1-points-start (dec side-1-points-count))
        side-2-points-start (inc side-1-points-end)
        side-2-points-end (+ side-2-points-start (dec side-2-points-count))
        faces (concat
               (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps)
               (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps inner-points-start)
               (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps upper-points-start)
               (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps lower-points-start)
               (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps side-1-points-start)
               (bezier-along-bezier-polyhedron-generate-front-or-back-faces (inc steps) (inc steps) steps side-2-points-start))
        points (into [] (concat  outer-points inner-points upper-points lower-points side-1-points side-2-points))]
    (polyhedron points faces)))

(defn generate-bezier-along-bezier-polyhedron-from-points-linear
  [outside-upper-left outside-upper-right
   outside-lower-left outside-lower-right
   inside-upper-left inside-upper-right
   inside-lower-left inside-lower-right
   steps
   & {:keys [outside-upper-control-point-vector outside-lower-control-point-vector inside-upper-control-point-vector inside-lower-control-point-vector]
      :or {outside-upper-control-point-vector [0 0 0] outside-lower-control-point-vector [0 0 0] inside-upper-control-point-vector [0 0 0] inside-lower-control-point-vector [0 0 0]}}]
  (let [outside-upper-control-point (calculate-point-between-points outside-upper-left outside-upper-right outside-upper-control-point-vector)
        outside-lower-control-point-vector (calculate-point-between-points outside-lower-left outside-lower-right outside-lower-control-point-vector)
        inside-upper-control-point-vector (calculate-point-between-points inside-upper-left inside-upper-right inside-upper-control-point-vector)
        inside-lower-control-point-vector (calculate-point-between-points inside-lower-left inside-lower-right inside-lower-control-point-vector)
        outer-upper-points (bezier-quadratic outside-upper-left outside-upper-control-point outside-upper-right steps)
        outer-lower-points (bezier-quadratic outside-lower-left outside-lower-control-point-vector  outside-lower-right steps)
        inner-upper-points (bezier-quadratic  inside-upper-left inside-upper-control-point-vector  inside-upper-right steps)
        inner-lower-points (bezier-quadratic inside-lower-left inside-lower-control-point-vector inside-lower-right steps)]
    (generate-bezier-along-bezier-polyhedron-from-points-list-linear outer-upper-points outer-lower-points
                                                                     inner-upper-points inner-lower-points
                                                                     steps)))

(defn generate-bezier-along-bezier-polyhedron-from-points-list-quadratic [outer-upper-ponts outer-control-ponts outer-lower-ponts
                                                                          inner-upper-points inner-control-points inner-lower-points
                                                                          steps]
  (let [points-fn (fn [upper control lower]
                    (into [] (apply concat
                                    (for [index (range 0 (inc steps))]
                                      (bezier-quadratic
                                       (nth upper index)
                                       (nth control index)
                                       (nth lower index)
                                       steps)))))
        outer-points (points-fn outer-upper-ponts outer-control-ponts outer-lower-ponts)
        lower-points (points-fn inner-upper-points inner-control-points inner-lower-points)]
    (generate-bezier-along-bezier-polyhedron outer-points lower-points steps)))

;; (defn generate-bezier-along-bezier-polyhedron-from-points-quadratic

;;   [outside-upper-left outside-upper-right outside-lower-left outside-lower-right outside-control-ponint
;;    inside-upper-left inside-upper-right inside-lower-left inside-lower-right steps
;;    {:keys [outside-upper-control-point-vector outside-lower-control-point-vector inside-upper-control-point-vector inside-lower-control-point-vector
;;            outside-control-point outside-right-control-point-vector inside-left-control-point-vector inside-right-control-point-vector]
;;     :or {outside-upper-control-point-vector [0 0 0] outside-lower-control-point-vector [0 0 0] inside-upper-control-point-vector [0 0 0] inside-lower-control-point-vector [0 0 0]
;;          outside-left-control-point-vector [0 0 0] outside-right-control-point-vector [0 0 0] inside-left-control-point-vector [0 0 0] inside-right-control-point-vector [0 0 0]}}]
;;   (let [outside-upper-control-point (calculate-point-between-points outside-upper-left outside-upper-right outside-upper-control-point-vector)
;;         outside-lower-control-point (calculate-point-between-points outside-lower-left outside-lower-right outside-lower-control-point-vector)
;;         inside-upper-control-point  (calculate-point-between-points inside-upper-left inside-upper-right inside-upper-control-point-vector)
;;         inside-lower-control-point (calculate-point-between-points inside-lower-left inside-lower-right inside-lower-control-point-vector)
;;         outside-left-control-point (calculate-point-between-points outside-upper-left outside-lower-left outside-left-control-point-vector)
;;         outside-right-control-point (calculate-point-between-points outside-upper-right outside-lower-left outside-right-control-point-vector)
;;         outside-left-to-right-control-point (calculate-point-between-points outside-upper-right outside-lower-left outside-right-control-point-vector)
;;         inside-left-control-point (calculate-point-between-points inside-upper-left inside-lower-left inside-left-control-point-vector)
;;         inside-right-control-point (calculate-point-between-points inside-upper-right inside-lower-right inside-right-control-point-vector)

;;         outside-upper (bezier-quadratic outside-upper-left outside-upper-control-point outside-upper-right steps)
;;         outside-lower (bezier-quadratic outside-lower-left outside-lower-control-point-vector outside-lower-right steps)
;;         inside-upper  (bezier-quadratic inside-upper-left inside-upper-control-point inside-upper-right steps)
;;         inside-lower (bezier-quadratic inside-lower-left inside-lower-control-point inside-lower-right steps)
;; ;; outside-left (bezier-quadratic outside-upper-left outside-left-control-point outside-lower-left steps)
;; ;; outside-right (bezier-quadratic outside-upper-right  outside-right-control-point outside-lower-left steps)
;; ;; inside-left (bezier-quadratic inside-upper-left inside-left-control-point inside-lower-left steps)
;; ;; inside-right (bezier-quadratic inside-upper-right inside-right-control-point inside-lower-right steps)
;;         ]
;;     (generate-bezier-along-bezier-polyhedron outside-upper outside-lower inside-upper inside-lower)))

(defn generate-bezier-to-point-polyhedron-top-face [bezier-start-index bezier-end-index point-index]
  (into [] (concat (for [index (range bezier-start-index (inc bezier-end-index))]
                     [index point-index (inc index)]))))

(defn generate-bezier-to-point-polyhedron-bottom-face [bezier-start-index bezier-end-index point-index]
  (into [] (concat (for [index (range bezier-start-index (inc bezier-end-index))]
                     [(inc index) point-index index]))))

(defn generate-bezier-to-point-polyhedron-curved-face [bezier-upper-start bezier-upper-end bezier-lower-start]
  (into [] (apply concat (for [index (range bezier-upper-start bezier-upper-end)]
                           [[index (inc (+ bezier-lower-start index)) (+ bezier-lower-start index)]
                            [index (inc index) (inc (+ bezier-lower-start index))]]))))

(defn generate-bezier-to-point-polyhedron-left-flat-face [point-upper-index bezier-upper-start point-lower-index bezier-lower-start]
  [[point-upper-index bezier-lower-start point-lower-index] [point-upper-index bezier-upper-start bezier-lower-start]])

(defn generate-bezier-to-point-polyhedron-right-flat-face [bezier-upper-end point-upper-index  bezier-lower-end point-lower-index]
  [[bezier-upper-end point-upper-index point-lower-index] [bezier-upper-end point-lower-index  bezier-lower-end]])
(defn generate-bezier-to-point-faces [bezier-upper bezier-lower]
  (let [bezier-upper-start 0
        bezier-upper-size (count bezier-upper)
        bezier-upper-end (dec bezier-upper-size)
        point-upper-index (inc bezier-upper-end)
        bezier-lower-start (inc point-upper-index)
        bezier-lower-size (count bezier-lower)
        bezier-lower-end (dec (+ bezier-lower-start bezier-lower-size))
        point-lower-index (inc bezier-lower-end)]
    (concat (generate-bezier-to-point-polyhedron-top-face bezier-upper-start bezier-upper-end point-upper-index)
            (generate-bezier-to-point-polyhedron-bottom-face bezier-lower-start bezier-lower-end point-lower-index)
            (generate-bezier-to-point-polyhedron-curved-face bezier-upper-start bezier-upper-end bezier-lower-start)
            (generate-bezier-to-point-polyhedron-left-flat-face point-upper-index bezier-upper-start point-lower-index bezier-lower-start)
            (generate-bezier-to-point-polyhedron-right-flat-face bezier-upper-end point-upper-index  bezier-lower-end point-lower-index))))

(defn generate-bezier-to-point-polyhedron [bezier-upper point-upper bezier-lower point-lower]
  (let [bezier-to-point-polyhedron-points (apply conj (conj (vec-if-not bezier-upper) point-upper) (conj (vec-if-not bezier-lower) point-lower))
        bezier-to-point-polyhedron-faces (generate-bezier-to-point-faces bezier-upper bezier-lower)]
    (polyhedron bezier-to-point-polyhedron-points bezier-to-point-polyhedron-faces)))

(defn generate-polyhedron-face-list [inner-list-size outer-list-size]
  (let [main-face-fn (fn [outer-list-start] (into [] (apply concat (for [outer-index (range outer-list-start (+ (dec outer-list-size) outer-list-start)) inner-index (range (dec inner-list-size))]
                                                                     [[(+ (* outer-index inner-list-size) inner-index) (+ (* (inc outer-index) inner-list-size) inner-index)
                                                                       (+ (* (inc outer-index) inner-list-size) (inc inner-index))]
                                                                      [(+ (* outer-index inner-list-size) inner-index) (+ (* (inc outer-index) inner-list-size) (inc inner-index))
                                                                       (+ (* outer-index inner-list-size) (inc inner-index))]]))))
        main-face-front (main-face-fn 0)
        main-face-back (main-face-fn outer-list-size)
        side-fn  (fn [initial-left-index-in-side-view initial-right-index-in-side-view]
                   (into [] (apply concat (for [index (range (dec inner-list-size))]
                                            [[(+ index initial-left-index-in-side-view) (+ index initial-right-index-in-side-view) (+ index (inc initial-right-index-in-side-view))]
                                             [(+ index initial-left-index-in-side-view) (+ index (inc initial-right-index-in-side-view)) (+ index (inc initial-left-index-in-side-view))]]))))
        front-to-back (side-fn (* (dec outer-list-size) inner-list-size) (* outer-list-size inner-list-size))
        back-to-front (side-fn (+ (* (dec outer-list-size) inner-list-size) (* outer-list-size inner-list-size)) 0)
        top-or-bottom-fn (fn [increasing-side-start-index decreasing-side-start-index]
                           (into [] (apply concat (for [index (range 0 (dec outer-list-size))]
                                                    [[(+ (* index inner-list-size) increasing-side-start-index) (- decreasing-side-start-index (* index inner-list-size)) (- decreasing-side-start-index (* (inc index) inner-list-size))]
                                                     [(+ (* index inner-list-size) increasing-side-start-index) (- decreasing-side-start-index (* (inc index) inner-list-size)) (+ (* (inc index) inner-list-size) increasing-side-start-index)]]))))
        top (top-or-bottom-fn 0 (+ (* (dec outer-list-size) inner-list-size) (* outer-list-size inner-list-size)))
        bottom (into [] (apply concat (for [index (range 0 (dec outer-list-size))
                                            :let [increasing-side-start-index  (+ (dec inner-list-size) (* outer-list-size inner-list-size))
                                                  decreasing-side-start-index (+ (dec inner-list-size) (* (dec outer-list-size) inner-list-size))]]
                                        [[(- decreasing-side-start-index (* index inner-list-size)) (+ (* index inner-list-size) increasing-side-start-index)   (+ (* (inc index) inner-list-size) increasing-side-start-index)]
                                         [(- decreasing-side-start-index (* index inner-list-size))   (+ (* (inc index) inner-list-size) increasing-side-start-index) (- decreasing-side-start-index (* (inc index) inner-list-size))]])))]
    (into [] (concat main-face-front main-face-back front-to-back back-to-front top bottom))))

(defn generate-polyhedron-face-list-with-different-list-lengths [inner-list-size-front outer-list-size-front inner-list-size-back outer-list-size-back]
  ())

(defn generate-polyhedron [points inner-list-size outer-list-size]
  (polyhedron points (generate-polyhedron-face-list inner-list-size outer-list-size)))