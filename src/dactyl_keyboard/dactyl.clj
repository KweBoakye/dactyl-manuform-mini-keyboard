(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

(def is-preview false)

(defn rx [radians shape] (rotate radians [1 0 0] shape))
(defn ry [radians shape] (rotate radians [0 1 0] shape))
(defn rz [radians shape] (rotate radians [0 0 1] shape))


(defn rdx [degrees shape] (rx (deg2rad degrees) shape))
(defn rdy [degrees shape] (ry (deg2rad degrees) shape))
(defn rdz [degrees shape] (rz (deg2rad degrees) shape))


(defn rd [x y z shape] (->> shape
                            (rdx x)
                            (rdy y)
                            (rdz z)))

(defn rcylinder [radius height]
  (if is-preview
    (cylinder radius height)
    (->>
     (hull
      (translate [0 0 (- (/ height 2) (/ radius 2))] (sphere (/ radius 2)))
      (translate [0 0 (+ (/ height -2) (/ radius 2))] (sphere (/ radius 2))))
     (with-fn 20))))

(defn add-vec  [& args]
  "Add two or more vectors together"
  (when  (seq args)
    (apply mapv + args)))

(defn sub-vec  [& args]
  "Subtract two or more vectors together"
  (when  (seq args)
    (apply mapv - args)))

(defn div-vec  [& args]
  "Divide two or more vectors together"
  (when  (seq args)
    (apply mapv / args)))
;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 5)
(def ncols 7)

(def trackball-enabled true)
(def joystick-enabled false)
(def printed-hotswap? false) ; Whether you want the 3d printed version of the hotswap or you ordered some from krepublic

(def α (/ π 12))                        ; curvature of the columns
(def β (/ π 36))                        ; curvature of the rows
(def centerrow (- nrows 3))             ; controls front-back tilt
(def centercol 4)                       ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (/ π 12))            ; or, change this for more precise tenting control
(def column-style
  (if (> nrows 5) :orthographic :standard))  ; options include :standard, :orthographic, and :fixed
; (def column-style :fixed)
(def pinky-15u false)
(def first-15u-row 0)                   ; controls which should be the first row to have 1.5u keys on the outer column
(def last-15u-row 3)                    ; controls which should be the last row to have 1.5u keys on the outer column

(def extra-row true)                   ; adds an extra bottom row to the outer columns
(def inner-column true)
(def thumb-style "mini")

(defn column-offset [column]
  (if inner-column
    (cond (<= column 1) [0 -2 0]
          (= column 3) [0 2.82 -4.5]
          (>= column 5) [0 -12 5.64]    ; original [0 -5.8 5.64]
          :else [0 0 0])
    (cond (= column 2) [0 2.82 -4.5]
          (>= column 4) [0 -12 5.64]    ; original [0 -5.8 5.64]
          :else [0 0 0])))

(def thumb-offsets [6 0 10])

(def keyboard-z-offset 10)               ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

(def extra-width 2.5)                   ; extra space between the base of keys; original= 2
(def extra-height 1.0)                  ; original= 0.5

(def wall-z-offset -8)                 ; original=-15 length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 5)                  ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 3)                  ; wall thickness parameter; originally 5
(def wall-xy-offset-thin 1)

;; Settings for column-style == :fixed
;; The defaults roughly match Maltron settings
;;   http://patentimages.storage.googleapis.com/EP0219944A2/imgf0002.png
;; Fixed-z overrides the z portion of the column ofsets above.
;; NOTE: THIS DOESN'T WORK QUITE LIKE I'D HOPED.
(def fixed-angles [(deg2rad 10) (deg2rad 10) 0 0 0 (deg2rad -15) (deg2rad -15)])
(def fixed-x [-41.5 -22.5 0 20.3 41.4 65.5 89.6])  ; relative to the middle finger
(def fixed-z [12.1    8.3 0  5   10.7 14.5 17.5])
(def fixed-tenting (deg2rad 0))

; If you use Cherry MX or Gateron switches, this can be turned on.
; If you use other switches such as Kailh, you should set this as false
(def create-side-nubs? false)
(def round-case true)
;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def lastcol (dec ncols))
(def extra-cornerrow (if extra-row lastrow cornerrow))
(def innercol-offset (if inner-column 1 0))



(def rounding-radius (if round-case 1 0))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 14.15) ;; Was 14.1, then 14.25
(def keyswitch-width 14.15)

(def sa-profile-key-height 12.7)

(def plate-thickness 4)
(def side-nub-thickness 4)
(def retention-tab-thickness 1.5)
(def retention-tab-hole-thickness  (- (+ plate-thickness 0.5) retention-tab-thickness))
(def mount-width (+ keyswitch-width 3.2))
(def mount-height (+ keyswitch-height 2.7))

(def single-plate
 (let [top-wall (->> (cube (+ keyswitch-width 3) 1.5 (+ plate-thickness 0.5))
                      (translate [0
                                  (+ (/ 1.5 2) (/ keyswitch-height 2))
                                  (- (/ plate-thickness 2) 0.25)]))
        left-wall (->> (cube 1.8 (+ keyswitch-height 3) (+ plate-thickness 0.5))
                       (translate [(+ (/ 1.8 2) (/ keyswitch-width 2))
                                   0
                                    (- (/ plate-thickness 2) 0.25)]))
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ π 2) [1 0 0])
                      (translate [(+ (/ keyswitch-width 2)) 0 1])
                      (hull (->> (cube 1.5 2.75 side-nub-thickness)
                                 (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                             0
                                             (/ side-nub-thickness 2)])))
                      (translate [0 0 (- plate-thickness side-nub-thickness)]))
        plate-half (union top-wall left-wall (if create-side-nubs? (with-fn 100 side-nub)))
        top-nub (->> (cube 5 5 retention-tab-hole-thickness)
                     (translate [(+ (/ keyswitch-width 2.5)) 0 (- (/ retention-tab-hole-thickness 2) 0.5)]))
        top-nub-pair (union top-nub
                            (->> top-nub
                                 (mirror [1 0 0])
                                 (mirror [0 1 0])))]
    (difference
     (union plate-half
            (->> plate-half
                 (mirror [1 0 0])
                 (mirror [0 1 0])))
     (->>
      top-nub-pair
      (rotate (/ π 2) [0 0 1])))))

;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18.25)
(def sa-double-length 37.5)
(def sa-cap {1 (let [bl2 (/ 18.5 2)
                     m (/ 17 2)
                     key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 6]))
                                   (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [220/255 163/255 163/255 1])))
             2 (let [bl2 sa-length
                     bw2 (/ 18.25 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [127/255 159/255 127/255 1])))
             1.5 (let [bl2 (/ 18.25 2)
                       bw2 (/ 27.94 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 12])))]
                   (->> key-cap
                        (translate [0 0 (+ 5 plate-thickness)])
                        (color [240/255 223/255 175/255 1])))})

;; Fill the keyholes instead of placing a a keycap over them
(def keyhole-fill (->> (cube keyswitch-height keyswitch-width plate-thickness)
                       (translate [0 0 (/ plate-thickness 2)])))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(def columns (range (+ innercol-offset 0) ncols))
(def rows (range 0 nrows))

(def innercolumn 0)
(def innerrows (range 0 (- nrows 2)))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius (+ (/ (/ (+ mount-height extra-height) 2)
                      (Math/sin (/ α 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))


(defn offset-for-column [col, row]
  (if (and pinky-15u
           (= col lastcol)
           (<= row last-15u-row)
           (>= row first-15u-row))
    4.7625
    0))

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
  (let [column-angle (* β (- centercol column))
        placed-shape (->> shape
                          (translate-fn [(offset-for-column column, row) 0 (- row-radius)])
                          (rotate-x-fn  (* α (- centerrow row)))
                          (translate-fn [0 0 row-radius])
                          (translate-fn [0 0 (- column-radius)])
                          (rotate-y-fn  column-angle)
                          (translate-fn [0 0 column-radius])
                          (translate-fn (column-offset column)))
        column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
        placed-shape-ortho (->> shape
                                (translate-fn [0 0 (- row-radius)])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 row-radius])
                                (rotate-y-fn  column-angle)
                                (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
                                (translate-fn (column-offset column)))
        placed-shape-fixed (->> shape
                                (rotate-y-fn  (nth fixed-angles column))
                                (translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
                                (translate-fn [0 0 (- (+ row-radius (nth fixed-z column)))])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 (+ row-radius (nth fixed-z column))])
                                (rotate-y-fn  fixed-tenting)
                                (translate-fn [0 (second (column-offset column)) 0]))]
    (->> (case column-style
               :orthographic placed-shape-ortho
               :fixed        placed-shape-fixed
               placed-shape)
         (rotate-y-fn  tenting-angle)
         (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place [column row shape]
  (apply-key-geometry translate
                      (fn [angle obj] (rotate angle [1 0 0] obj))
                      (fn [angle obj] (rotate angle [0 1 0] obj))
                      column row shape))

(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle)    (Math/cos angle)]]
   position))

(defn rotate-around-y [angle position]
  (mmul
   [[(Math/cos angle)     0 (Math/sin angle)]
    [0                    1 0]
    [(- (Math/sin angle)) 0 (Math/cos angle)]]
   position))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))

(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [(+ innercol-offset 2) (+ innercol-offset 3)] column)
                         (and (.contains [(+ innercol-offset 4) (+ innercol-offset 5)] column) extra-row (= ncols (+ innercol-offset 6)))
                         (and (.contains [(+ innercol-offset 4)] column) extra-row (= ncols (+ innercol-offset 5)))
                         (and inner-column (not= row cornerrow) (= column 0))
                         (not= row lastrow))]
           (->> single-plate
                (key-place column row)))))

(def caps
  (apply union
         (conj (for [column columns
                     row rows
                     :when (or (and (= column 0) (< row 3))
                               (and (.contains [1 2] column) (< row 4))
                               (.contains [3 4 5 6] column))]
                 (->> (sa-cap (if (and pinky-15u (= column lastcol) (not= row lastrow)) 1.5 1))
                      (key-place column row)))
               (list (key-place 0 0 (sa-cap 1))
                     (key-place 0 1 (sa-cap 1))
                     (key-place 0 2 (sa-cap 1))))))

(def caps-fill
  (apply union
         (conj (for [column columns
                     row rows
                     :when (or (.contains [(+ innercol-offset 2) (+ innercol-offset 3)] column)
                               (and (.contains [(+ innercol-offset 4) (+ innercol-offset 5)] column) extra-row (= ncols (+ innercol-offset 6)))
                               (and (.contains [(+ innercol-offset 4)] column) extra-row (= ncols (+ innercol-offset 5)))
                               (and inner-column (not= row cornerrow) (= column 0))
                               (not= row lastrow))]
                 (key-place column row keyhole-fill))
               (list (key-place 0 0 keyhole-fill)
                     (key-place 0 1 keyhole-fill)
                     (key-place 0 2 keyhole-fill)))))

(def key-holes-inner
  (if inner-column
    (apply union
           (for [row innerrows]
             (->> single-plate
                  ;               (rotate (/ π 2) [0 0 1])
                  (key-place 0 row))))
    :else))

(defn add-vec  [& args]
 ; "Add two or more vectors together"
  (when  (seq args)
    (apply mapv + args)))

;from https://github.com/oysteinkrog/dactyl-manuform-mini-keyboard
;;;;;;;;;;;;;;;;;;;;;;;;
;; OLED screen holder ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(def oled-pcb-size [27.35 28.3 (- plate-thickness 1)])
(def oled-screen-offset [0 -0.5 0])
(def oled-screen-size [24.65 16.65 (- plate-thickness 1)])
(def oled-viewport-size [24.0 13.0 (+ 0.1 plate-thickness)])
(def oled-viewport-offset [0 1.0 0])
(def oled-mount-size [23.1 23.75 0.5])
(def oled-holder-width (+ 3 (nth oled-pcb-size 0)))
(def oled-holder-height (+ 3 (nth oled-pcb-size 1)))
(def oled-holder-thickness plate-thickness)
(def oled-holder-size [oled-holder-width oled-holder-height oled-holder-thickness])
(def oled-mount-rotation-x-old (deg2rad 20))
(def oled-mount-rotation-z-old (deg2rad -3))
(def oled-x-position -2.6)
(def oled-y-position 0)
(def oled-z-position -4)

(def oled-mount-width 12.5)  ; whole OLED width
(def oled-mount-height 39.0)  ; whole OLED length
(def oled-mount-rim 2.0)
(def oled-mount-depth 7.0)
(def oled-mount-cut_depth 20.0)
(def oled-mount-location-x -78.0)
(def oled-mount-location-y 20.0)
(def oled-mount-location-z 62.0)
(def oled-mount-rotation-x  12.0)
(def oled-mount-rotation-y  0.0)
(def oled-mount-rotation-z -6.0)
(def oled-left-wall-x-offset-override 24.0)
(def oled-left-wall-z-offset-override 0.0)
(def oled-thickness 4.2)  ; thickness of OLED, )plus clearance.  Must include components
(def oled-mount-bezel-thickness 3.5)  ; z thick)ness of clip bezel
(def oled-mount-bezel-chamfer 2.0)  ; depth of )the 45 degree chamfer
(def oled-mount-connector-hole 6.0)
(def oled-screen-start-from-conn-end 6.5)
(def oled-screen-length 24.5)
(def oled-screen-width 10.5)
(def oled-clip-thickness 1.5)
(def oled-clip-width 6.0)
(def oled-clip-overhang 1.0)
(def oled-clip-extension 5.0)
(def oled-clip-width-clearance 0.5)
(def oled-clip-undercut 0.5)
(def oled-clip-undercut-thickness 2.5)
(def oled-clip-y-gap 0.2)
(def oled-clip-z-gap 0.2)
(def oled-clip-mount-external-width (+ oled-mount-width (* 2 oled-mount-rim)))
(def oled-clip-mount-external-height (+ oled-mount-height (* oled-clip-thickness 2) (* oled-clip-undercut 2) ( * oled-clip-overhang 2) (* oled-mount-rim 2)) )
(def oled-clip-mount-slot (cube (+ oled-clip-width (* 2 oled-clip-width-clearance)) 
                                (+ oled-mount-height (* 2 oled-clip-thickness) (* 2 oled-clip-overhang))
                                (+ oled-mount-depth 0.1) :center true))
(def oled-clip-mount-undercut (translate [0 0 oled-clip-undercut-thickness] 
                                         (cube (+ oled-clip-width (* 2 oled-clip-width-clearance))
                                                  (+ oled-mount-height (* 2 oled-clip-thickness) (* 2 oled-clip-overhang) (* 2  oled-clip-undercut))
                                                  (+ oled-mount-depth 0.1)
                                               :center true)))

(def oled-clip-mount-plate (translate [0 0 (/ (- oled-thickness) 2)]
                            (cube (+ oled-mount-width 0.1)
                                  (- oled-mount-height (* oled-mount-connector-hole 2)) 
                                  (- oled-mount-depth oled-thickness) 
                                  :center true)))

(def oled-clip-mount-frame-hole
  (translate [oled-mount-location-x oled-mount-location-y oled-mount-location-z] 
             (rotate [oled-mount-rotation-x oled-mount-rotation-y oled-mount-rotation-z]
             (cube oled-clip-mount-external-width oled-clip-mount-external-height (+ oled-mount-cut_depth 0.1) :center true)
             )) 
  )

(def oled-clip-mount-frame-shape
  (translate [oled-mount-location-x oled-mount-location-y oled-mount-location-z]
             (rotate [oled-mount-rotation-x oled-mount-rotation-y oled-mount-rotation-z]
                     (union 
                    oled-clip-mount-plate                    
                      (difference
                       oled-clip-mount-undercut
                       oled-clip-mount-slot
                       (cube oled-clip-mount-external-width oled-clip-mount-external-height (+ oled-mount-depth 0.1) :center true)
                         (cube oled-clip-mount-external-width oled-clip-mount-external-height oled-mount-depth :center true)  
                        )
                      ))))

(def oled-clip-mount-frame (union oled-clip-mount-frame-hole oled-clip-mount-frame-shape ))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 4.5)
(def post-size 0.1)
(def new-post-size (if (= rounding-radius 0) 1 rounding-radius))
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))

(defn web-post-shape [height]
  (if (= new-post-size 0)
    (cube post-size post-size height)
    (rcylinder post-size height)))

(def big-boi-web-post (->> (cube post-size (+ post-size 30) 10)
                           (translate [0 0 (- (+ (/ 10 -2)
                                                 plate-thickness) 1)])))

(def oled-post (->> (web-post-shape oled-holder-thickness)
                    (translate [0 0 (+ (/ oled-holder-thickness -2) plate-thickness)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 1.95) post-adj) (- (/ mount-height 1.95) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -1.95) post-adj) (- (/ mount-height 1.95) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -1.95) post-adj) (+ (/ mount-height -1.95) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 1.95) post-adj) (+ (/ mount-height -1.95) post-adj) 0] web-post))

; wide posts for 1.5u keys in the main cluster

; wide posts for 1.5u keys in the main cluster
(if pinky-15u
  (do (def wide-post-tr (translate [(- (/ mount-width 1.2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
      (def wide-post-tl (translate [(+ (/ mount-width -1.2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
      (def wide-post-bl (translate [(+ (/ mount-width -1.2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
      (def wide-post-br (translate [(- (/ mount-width 1.2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post)))
  (do (def wide-post-tr web-post-tr)
      (def wide-post-tl web-post-tl)
      (def wide-post-bl web-post-bl)
      (def wide-post-br web-post-br)))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(def connectors
  (apply union
         (concat
          ;; Row connections
          (for [column (range (+ innercol-offset 0) (dec ncols))
                row (range 0 lastrow)]
            (triangle-hulls
             (key-place (inc column) row web-post-tl)
             (key-place column row web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place column row web-post-br)))

          ;; Column connections
          (for [column columns
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))

          ;; Diagonal connections
          (for [column (range 0 (dec ncols))
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place (inc column) (inc row) web-post-tl))))))

(def inner-connectors
  (if inner-column
    (apply union
           (concat
            ;; Row connections
            (for [column (range 0 1)
                  row (range 0 (- nrows 2))]
              (triangle-hulls
               (key-place (inc column) row web-post-tl)
               (key-place column row web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place column row web-post-br)))

            ;; Column connections
            (for [row (range 0 (dec cornerrow))]
              (triangle-hulls
               (key-place innercolumn row web-post-bl)
               (key-place innercolumn row web-post-br)
               (key-place innercolumn (inc row) web-post-tl)
               (key-place innercolumn (inc row) web-post-tr)))

            ;; Diagonal connections
            (for [column (range 0 (dec ncols))
                  row (range 0 2)]
              (triangle-hulls
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place (inc column) (inc row) web-post-tl)))))
               :else))

(def extra-connectors
  (if extra-row
    (apply union
           (concat
            (for [column (range 3 ncols)
                  row (range cornerrow lastrow)]
              (triangle-hulls
               (key-place column row web-post-bl)
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tl)
               (key-place column (inc row) web-post-tr)))

            (for [column (range 3 (dec ncols))
                  row (range cornerrow lastrow)]
              (triangle-hulls
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place (inc column) (inc row) web-post-tl)))

            (for [column (range 4 (dec ncols))
                  row (range lastrow nrows)]
              (triangle-hulls
               (key-place (inc column) row web-post-tl)
               (key-place column row web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place column row web-post-br)))))
               :else))

;;;;;;;;;;;;;;;;;;;
;; Default Thumb ;;
;;;;;;;;;;;;;;;;;;;

(def thumborigin
  (map + (key-position (+ innercol-offset 1) cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
       thumb-offsets))



(defn thumb-tr-place [shape]
  (->> shape
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad -23) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-12 -16 3])))
(defn thumb-tl-place [shape]
  (->> shape
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad -23) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-32 -15 -2])))
(defn thumb-mr-place [shape]
  (->> shape
       (rotate (deg2rad  -6) [1 0 0])
       (rotate (deg2rad -34) [0 1 0])
       (rotate (deg2rad  48) [0 0 1])
       (translate thumborigin)
       (translate [-29 -40 -13])))
(defn thumb-ml-place [shape]
  (->> shape
       (rotate (deg2rad   6) [1 0 0])
       (rotate (deg2rad -34) [0 1 0])
       (rotate (deg2rad  40) [0 0 1])
       (translate thumborigin)
       (translate [-51 -25 -12])))
(defn thumb-br-place [shape]
  (->> shape
       (rotate (deg2rad -16) [1 0 0])
       (rotate (deg2rad -33) [0 1 0])
       (rotate (deg2rad  54) [0 0 1])
       (translate thumborigin)
       (translate [-37.8 -55.3 -25.3])))
(defn thumb-bl-place [shape]
  (->> shape
       (rotate (deg2rad  -4) [1 0 0])
       (rotate (deg2rad -35) [0 1 0])
       (rotate (deg2rad  52) [0 0 1])
       (translate thumborigin)
       (translate [-56.3 -43.3 -23.5])))

(defn thumb-1x-layout [shape]
  (union
   (thumb-mr-place shape)
   (thumb-ml-place shape)
   (thumb-br-place shape)
   (thumb-bl-place shape)))

(defn thumb-15x-layout [shape]
  (union
   (thumb-tr-place shape)
   (thumb-tl-place shape)))

(def larger-plate
  (let [plate-height (/ (- sa-double-length mount-height) 3)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))]
    (union top-plate (mirror [0 1 0] top-plate))))

(def larger-plate-half
  (let [plate-height (/ (- sa-double-length mount-height) 3)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))]
    (union top-plate (mirror [0 0 0] top-plate))))

(def thumbcaps
  (union
   (thumb-1x-layout (sa-cap 1))
   (thumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1.5)))))

(def thumbcaps-fill
  (union
   (thumb-1x-layout keyhole-fill)
   (thumb-15x-layout (rotate (/ π 2) [0 0 1] keyhole-fill))))

(def thumb
  (union
   (thumb-1x-layout (rotate (/ π 2) [0 0 0] single-plate))
   (thumb-tr-place (rotate (/ π 2) [0 0 1] single-plate))
   (thumb-tr-place larger-plate)
   (thumb-tl-place (rotate (/ π 2) [0 0 1] single-plate))
   (thumb-tl-place larger-plate-half)))

(def thumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  1.1) post-adj) 0] web-post))
(def thumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  1.1) post-adj) 0] web-post))
(def thumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -1.1) post-adj) 0] web-post))
(def thumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -1.1) post-adj) 0] web-post))

(def thumb-connectors
  (union
   (triangle-hulls    ; top two
    (thumb-tl-place thumb-post-tr)
    (thumb-tl-place (translate [-0.33 -0.25 0] web-post-br))
    (thumb-tr-place thumb-post-tl)
    (thumb-tr-place thumb-post-bl))
   (triangle-hulls    ; bottom two on the right
    (thumb-br-place web-post-tr)
    (thumb-br-place web-post-br)
    (thumb-mr-place web-post-tl)
    (thumb-mr-place web-post-bl))
   (triangle-hulls    ; bottom two on the left
    (thumb-bl-place web-post-tr)
    (thumb-bl-place web-post-br)
    (thumb-ml-place web-post-tl)
    (thumb-ml-place web-post-bl))
   (triangle-hulls    ; centers of the bottom four
    (thumb-br-place web-post-tl)
    (thumb-bl-place web-post-bl)
    (thumb-br-place web-post-tr)
    (thumb-bl-place web-post-br)
    (thumb-mr-place web-post-tl)
    (thumb-ml-place web-post-bl)
    (thumb-mr-place web-post-tr)
    (thumb-ml-place web-post-br))
   (triangle-hulls    ; top two to the middle two, starting on the left
    (thumb-tl-place thumb-post-tl)
    (thumb-ml-place web-post-tr)
    (thumb-tl-place (translate [0.25 0.1 0] web-post-bl))
    (thumb-ml-place web-post-br)
    (thumb-tl-place (translate [-0.33 -0.25 0] web-post-br))
    (thumb-mr-place web-post-tr)
    (thumb-tr-place thumb-post-bl)
    (thumb-mr-place web-post-br)
    (thumb-tr-place thumb-post-br))
   (triangle-hulls    ; top two to the main keyboard, starting on the left
    (thumb-tl-place thumb-post-tl)
    (key-place (+ innercol-offset 0) cornerrow web-post-bl)
    (thumb-tl-place thumb-post-tr)
    (key-place (+ innercol-offset 0) cornerrow web-post-br)
    (thumb-tr-place thumb-post-tl)
    (key-place (+ innercol-offset 1) cornerrow web-post-bl)
    (thumb-tr-place thumb-post-tr)
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (thumb-tr-place thumb-post-tr)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (thumb-tr-place thumb-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-bl)
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 3) lastrow web-post-tl)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl)
    (key-place (+ innercol-offset 3) lastrow web-post-tr)
    (key-place (+ innercol-offset 3) cornerrow web-post-br))
   (triangle-hulls
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) cornerrow web-post-bl)
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 2) cornerrow web-post-br)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl))
   (if extra-row
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) lastrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl)))
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))))))

;;;;;;;;;;;;;;;;
;; Mini Thumb ;;
;;;;;;;;;;;;;;;;

(defn minithumb-tr-place [shape]
  (->> shape
        (rotate (deg2rad  -7) [1 0 0])
        (rotate (deg2rad -45) [0 1 0])
        (rotate (deg2rad  27) [0 0 1]) ; original 10
        (translate thumborigin)
        (translate [-21 -12.5 11]))) ; original 1.5u  (translate [-12 -16 3])
(def trackball-middle-translate [-6.5 6 -0.5])
(def minithumb-tip-offset [-35 -16 -6.5])
(def minithumb-tip-origin (map + thumborigin minithumb-tip-offset))
(def tl-minithumb-loc (map + minithumb-tip-offset (if (or trackball-enabled joystick-enabled) trackball-middle-translate [0 0 0])))
(defn minithumb-tl-place [shape]
  (->> shape
           (rotate (deg2rad  -12) [1 0 0])
           (rotate (deg2rad -54) [0 1 0])
           (rotate (deg2rad  35) [0 0 1]) ; original 10
           (translate thumborigin)
           (translate tl-minithumb-loc))) ; original 1.5u (translate [-32 -15 -2])))

(def mr-minithumb-loc (map + [-23.5 -36.5 -2] (if (or trackball-enabled joystick-enabled) trackball-middle-translate [0 0 0])))

(defn minithumb-mr-place [shape]
  (->> shape
       (rotate (deg2rad  -12) [1 0 0])
       (rotate (deg2rad -54) [0 1 0])
       (rotate (deg2rad  35) [0 0 1])
       (translate thumborigin)
       (translate mr-minithumb-loc)))
(def br-minithumb-loc (map + [-34.5 -44 -20] (if (or trackball-enabled joystick-enabled) [2 -12 2] [0 0 0])))

(defn minithumb-br-place [shape]
  (->> shape
       (rotate (deg2rad   -18) [1 0 0])
       (rotate (deg2rad -55) [0 1 0])
       (rotate (deg2rad  37) [0 0 1])
       (translate thumborigin)
       (translate br-minithumb-loc)))

(def bl-minithumb-loc (map + [-44 -23 -24] (if (or trackball-enabled joystick-enabled) [2 -12 2] [0 0 0])))
(defn minithumb-bl-place [shape]
  (->> shape
       (rotate (deg2rad   -18) [1 0 0])
       (rotate (deg2rad -55) [0 1 0])
       (rotate (deg2rad  37) [0 0 1])
       (translate thumborigin)
       (translate bl-minithumb-loc))) ;        (translate [-51 -25 -12])))

(def tm-minithumb-loc (map + [-36.0 -9 3.2] (if (or trackball-enabled joystick-enabled) trackball-middle-translate [0 0 0])))
(defn minithumb-tm-place [shape]
  (->> shape
       (rd 6 -5 12)
       (translate thumborigin)
       (translate tm-minithumb-loc))) ; original 1.5u (translate [-32 -15 -2])))

(defn minithumb-1x-layout [shape]
  (union
   (minithumb-mr-place shape)
   (minithumb-br-place shape)
  (if (or trackball-enabled joystick-enabled) nil(minithumb-tl-place shape))
   (minithumb-bl-place shape)))

(defn minithumb-15x-layout [shape]
  (union
   (minithumb-tr-place shape)))

(def minithumbcaps
  (union
   (minithumb-1x-layout (sa-cap 1))
   (minithumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1)))))

(def minithumbcaps-fill
  (union
   (minithumb-1x-layout keyhole-fill)
   (minithumb-15x-layout (rotate (/ π 2) [0 0 1] keyhole-fill))))

(def minithumb
  (union
   (minithumb-1x-layout single-plate)
   (minithumb-15x-layout single-plate)))

(def minithumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
(def minithumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
(def minithumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def minithumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post))

(def minithumb-connectors
  (if (or trackball-enabled joystick-enabled)
    (union
     ; top right vertical
     (triangle-hulls
      (minithumb-tr-place web-post-br)
      (minithumb-tr-place web-post-bl)
      (minithumb-mr-place web-post-br))
     ; Between the top and middle
     (triangle-hulls
      (minithumb-tr-place web-post-tl)
      (minithumb-mr-place web-post-tr)
      (minithumb-mr-place web-post-br))
     (triangle-hulls
      (minithumb-tr-place web-post-bl)
      (minithumb-tr-place web-post-tl)
      (minithumb-mr-place web-post-br))
     ; Between middle and first bottom
     (triangle-hulls
      (minithumb-mr-place web-post-tl)
      (minithumb-br-place web-post-tr)
      (minithumb-br-place web-post-br))
     (triangle-hulls
      (minithumb-mr-place web-post-bl)
      (minithumb-mr-place web-post-tl)
      (minithumb-br-place web-post-br)
      (minithumb-bl-place web-post-br))
     ; Between the top and middle over by the trackball
     (triangle-hulls
      (minithumb-tr-place web-post-tl)
      (minithumb-mr-place web-post-tr)
      (minithumb-mr-place web-post-tl))
     ; Between the bottom two
     (triangle-hulls
      (minithumb-br-place web-post-tr)
      (minithumb-br-place web-post-tl)
      (minithumb-bl-place web-post-br))
     (triangle-hulls
      (minithumb-bl-place web-post-br)
      (minithumb-bl-place web-post-bl)
      (minithumb-br-place web-post-tl))
     ; Between the middle and the bl
     (triangle-hulls
      (minithumb-mr-place web-post-tl)
      (minithumb-bl-place web-post-tr)
      (minithumb-bl-place web-post-br))
     (triangle-hulls    ; top two to the main keyboard, starting on the left
      (key-place (+ innercol-offset 0) cornerrow web-post-br)
      (minithumb-tr-place minithumb-post-tl)
      (key-place (+ innercol-offset 1) cornerrow web-post-bl)
      (minithumb-tr-place minithumb-post-tr)
      (key-place ( + innercol-offset 1) cornerrow web-post-br)
      (key-place (+ innercol-offset 2) lastrow web-post-tl)
      (key-place (+ innercol-offset 2) lastrow web-post-bl)
      (minithumb-tr-place minithumb-post-tr)
      (key-place (+ innercol-offset 2) lastrow web-post-bl)
      (minithumb-tr-place minithumb-post-br)
      (key-place (+ innercol-offset 2) lastrow web-post-br)
      (key-place (+ innercol-offset 3) lastrow web-post-bl)
      (key-place (+ innercol-offset 2) lastrow web-post-tr)
      (key-place (+ innercol-offset 3) lastrow web-post-tl)
      (key-place (+ innercol-offset 3) cornerrow web-post-bl)
      (key-place (+ innercol-offset 3) lastrow web-post-tr)
      (key-place (+ innercol-offset 3) cornerrow web-post-br)
      (key-place (+ innercol-offset 4) cornerrow web-post-bl))
     (triangle-hulls
      (key-place (+ innercol-offset 1) cornerrow web-post-br)
      (key-place (+ innercol-offset 2) lastrow web-post-tl)
      (key-place (+ innercol-offset 2) cornerrow web-post-bl)
      (key-place (+ innercol-offset 2) lastrow web-post-tr)
      (key-place (+ innercol-offset 2) cornerrow web-post-br)
      (key-place (+ innercol-offset 3) cornerrow web-post-bl))
     (triangle-hulls
      (key-place (+ innercol-offset 3) lastrow web-post-tr)
      (key-place (+ innercol-offset 3) lastrow web-post-br)
      (key-place (+ innercol-offset 3) lastrow web-post-tr)
      (key-place (+ innercol-offset 4) cornerrow web-post-bl)))
  (union
   (triangle-hulls    ; top two
    (minithumb-tl-place web-post-tr)
    (minithumb-tl-place web-post-br)
    (minithumb-tr-place minithumb-post-tl)
    (minithumb-tr-place minithumb-post-bl))
   (triangle-hulls    ; bottom two
    (minithumb-br-place web-post-tr)
    (minithumb-br-place web-post-br)
    (minithumb-mr-place web-post-tl)
    (minithumb-mr-place web-post-bl))
   (triangle-hulls
    (minithumb-mr-place web-post-tr)
    (minithumb-mr-place web-post-br)
    (minithumb-tr-place minithumb-post-br))
   (triangle-hulls    ; between top row and bottom row
    (minithumb-br-place web-post-tl)
    (minithumb-bl-place web-post-bl)
    (minithumb-br-place web-post-tr)
    (minithumb-bl-place web-post-br)
    (minithumb-mr-place web-post-tl)
    (minithumb-tl-place web-post-bl)
    (minithumb-mr-place web-post-tr)
    (minithumb-tl-place web-post-br)
    (minithumb-tr-place web-post-bl)
    (minithumb-mr-place web-post-tr)
    (minithumb-tr-place web-post-br))
   (triangle-hulls    ; top two to the middle two, starting on the left
    (minithumb-tl-place web-post-tl)
    (minithumb-bl-place web-post-tr)
    (minithumb-tl-place web-post-bl)
    (minithumb-bl-place web-post-br)
    (minithumb-mr-place web-post-tr)
    (minithumb-tl-place web-post-bl)
    (minithumb-tl-place web-post-br)
    (minithumb-mr-place web-post-tr))
   (triangle-hulls    ; top two to the main keyboard, starting on the left
    (minithumb-tl-place web-post-tl)
    (key-place (+ innercol-offset 0) cornerrow web-post-bl)
    (minithumb-tl-place web-post-tr)
    (key-place (+ innercol-offset 0) cornerrow web-post-br)
    (minithumb-tr-place minithumb-post-tl)
    (key-place (+ innercol-offset 1) cornerrow web-post-bl)
    (minithumb-tr-place minithumb-post-tr)
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (minithumb-tr-place minithumb-post-tr)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (minithumb-tr-place minithumb-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-bl)
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 3) lastrow web-post-tl)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl)
    (key-place (+ innercol-offset 3) lastrow web-post-tr)
    (key-place (+ innercol-offset 3) cornerrow web-post-br))
   (triangle-hulls
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) cornerrow web-post-bl)
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 2) cornerrow web-post-br)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl))
   (if extra-row
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) lastrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl)))
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl)))))))

;;;;;;;;;;;;;;;;
;; cf Thumb ;;
;;;;;;;;;;;;;;;;

(defn cfthumb-tl-place [shape]
  (->> shape
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad -24) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-13 -9.8 4])))
(defn cfthumb-tr-place [shape]
  (->> shape
       (rotate (deg2rad  6) [1 0 0])
       (rotate (deg2rad -24) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-7.5 -29.5 0])))
(defn cfthumb-ml-place [shape]
  (->> shape
       (rotate (deg2rad  8) [1 0 0])
       (rotate (deg2rad -31) [0 1 0])
       (rotate (deg2rad  14) [0 0 1])
       (translate thumborigin)
       (translate [-30.5 -17 -6])))
(defn cfthumb-mr-place [shape]
  (->> shape
       (rotate (deg2rad  4) [1 0 0])
       (rotate (deg2rad -31) [0 1 0])
       (rotate (deg2rad  14) [0 0 1])
       (translate thumborigin)
       (translate [-22.2 -41 -10.3])))
(defn cfthumb-br-place [shape]
  (->> shape
       (rotate (deg2rad   2) [1 0 0])
       (rotate (deg2rad -37) [0 1 0])
       (rotate (deg2rad  18) [0 0 1])
       (translate thumborigin)
       (translate [-37 -46.4 -22])))
(defn cfthumb-bl-place [shape]
  (->> shape
       (rotate (deg2rad   6) [1 0 0])
       (rotate (deg2rad -37) [0 1 0])
       (rotate (deg2rad  18) [0 0 1])
       (translate thumborigin)
       (translate [-47 -23 -19])))

(defn cfthumb-1x-layout [shape]
  (union
   (cfthumb-tr-place (rotate (/ π 2) [0 0 0] shape))
   (cfthumb-mr-place shape)
   (cfthumb-br-place shape)
   (cfthumb-tl-place (rotate (/ π 2) [0 0 0] shape))))

(defn cfthumb-15x-layout [shape]
  (union
   (cfthumb-bl-place shape)
   (cfthumb-ml-place shape)))

(def cfthumbcaps
  (union
   (cfthumb-1x-layout (sa-cap 1))
   (cfthumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1.5)))))

(def cfthumbcaps-fill
  (union
   (cfthumb-1x-layout keyhole-fill)
   (cfthumb-15x-layout (rotate (/ π 2) [0 0 1] keyhole-fill))))

(def cfthumb
  (union
   (cfthumb-1x-layout single-plate)
   (cfthumb-15x-layout larger-plate-half)
   (cfthumb-15x-layout single-plate)))

(def cfthumb-connectors
  (union
   (triangle-hulls    ; top two
    (cfthumb-tl-place web-post-tl)
    (cfthumb-tl-place web-post-bl)
    (cfthumb-ml-place thumb-post-tr)
    (cfthumb-ml-place web-post-br))
   (triangle-hulls
    (cfthumb-ml-place thumb-post-tl)
    (cfthumb-ml-place web-post-bl)
    (cfthumb-bl-place thumb-post-tr)
    (cfthumb-bl-place web-post-br))
   (triangle-hulls    ; bottom two
    (cfthumb-br-place web-post-tr)
    (cfthumb-br-place web-post-br)
    (cfthumb-mr-place web-post-tl)
    (cfthumb-mr-place web-post-bl))
   (triangle-hulls
    (cfthumb-mr-place web-post-tr)
    (cfthumb-mr-place web-post-br)
    (cfthumb-tr-place web-post-tl)
    (cfthumb-tr-place web-post-bl))
   (triangle-hulls
    (cfthumb-tr-place web-post-br)
    (cfthumb-tr-place web-post-bl)
    (cfthumb-mr-place web-post-br))
   (triangle-hulls    ; between top row and bottom row
    (cfthumb-br-place web-post-tl)
    (cfthumb-bl-place web-post-bl)
    (cfthumb-br-place web-post-tr)
    (cfthumb-bl-place web-post-br)
    (cfthumb-mr-place web-post-tl)
    (cfthumb-ml-place web-post-bl)
    (cfthumb-mr-place web-post-tr)
    (cfthumb-ml-place web-post-br)
    (cfthumb-tr-place web-post-tl)
    (cfthumb-tl-place web-post-bl)
    (cfthumb-tr-place web-post-tr)
    (cfthumb-tl-place web-post-br))
   (triangle-hulls    ; top two to the main keyboard, starting on the left
    (cfthumb-ml-place thumb-post-tl)
    (key-place (+ innercol-offset 0) cornerrow web-post-bl)
    (cfthumb-ml-place thumb-post-tr)
    (key-place (+ innercol-offset 0) cornerrow web-post-br)
    (cfthumb-tl-place web-post-tl)
    (key-place (+ innercol-offset 1) cornerrow web-post-bl)
    (cfthumb-tl-place web-post-tr)
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (cfthumb-tl-place web-post-tr)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (cfthumb-tl-place web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-bl)
    (cfthumb-tl-place web-post-br)
    (cfthumb-tr-place web-post-tr))
   (triangle-hulls
    (key-place (+ innercol-offset 3) lastrow web-post-tr)
    (key-place (+ innercol-offset 3) cornerrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-tl)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl))
   (triangle-hulls
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 2) lastrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-tl)
    (key-place (+ innercol-offset 3) lastrow web-post-bl))
   (triangle-hulls
    (cfthumb-tr-place web-post-br)
    (cfthumb-tr-place web-post-tr)
    (key-place (+ innercol-offset 3) lastrow web-post-bl))
   (triangle-hulls
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) cornerrow web-post-bl)
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 2) cornerrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-tl)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl))
   (if extra-row
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) lastrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl)))
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))))))

;switching connectors, switchplates, etc. depending on thumb-style used
(when (= thumb-style "default")
  (def thumb-type thumb)
  (def thumb-connector-type thumb-connectors)
  (def thumbcaps-type thumbcaps)
  (def thumbcaps-fill-type thumbcaps-fill))

(when (= thumb-style "cf")
  (def thumb-type cfthumb)
  (def thumb-connector-type cfthumb-connectors)
  (def thumbcaps-type cfthumbcaps)
  (def thumbcaps-fill-type cfthumbcaps-fill))

(when (= thumb-style "mini")
  (def thumb-type minithumb)
  (def thumb-connector-type minithumb-connectors)
  (def thumbcaps-type minithumbcaps)
  (def thumbcaps-fill-type minithumbcaps-fill))

;;;;;;;;;;
;; Hand ;;
;;;;;;;;;;

(defn finger [one two three finger-radius]
  (let
   [three-cyl-height (- three finger-radius)
    height-loss (* finger-radius (Math/sin 15))]
    (union
          ;; First joint to second joint
     (translate [0 0 (/ one 2)]
                (cylinder finger-radius one))
     (translate [0 0 one]
                (rotate (deg2rad 15) [1 0 0]
                        (union
                               ;; Second joint to third
                         (translate [0 0 (/ two 2)]
                                    (cylinder finger-radius two))
                               ;; Third to end
                         (translate [0 (* -1 (- three-cyl-height height-loss) (Math/cos (deg2rad 75))) (+ two (/ three-cyl-height 2))]
                                    (rotate (deg2rad 15) [1 0 0]
                                            (union
                                             (cylinder finger-radius three-cyl-height)
                                                    ;; Make the fingertip round
                                             (translate [0 0 (/ three-cyl-height 2)] (sphere finger-radius)))))))))))

(def fingers
  ;; Move over by half the width of index finger to half index finger at 0 on x
  (translate [10.5 0 0]
             (union
               ;; Index
              (finger 47 22 20 10.5)
               ;; Middle
              (translate [25.5 0 0] (finger 53.5 29 22 9.2))
               ;; Ring
              (translate [(+ 20 25.5) 0 0] (finger 44 28.5 23 8.25))
               ;; Pinky
              (translate [(+ 20 25.5 22) 0 0] (finger 30 22.5 20 8.25)))))

(def palm
  (translate [42.5 0 -40] (union
                           (cube 85 30 80)
                           (rotate (deg2rad 35) [1 0 0]
                                   (translate [(+ 7 (/ -85 2)) -25 25]
                                              (cylinder 10.5 100))))))

(def hand
  (union
   fingers
   (rotate (deg2rad -45) [1 0 0] palm)))

(defn buckle [& {:keys [triangle-length triangle-width buckle-width-adjust buckle-width buckle-thickness buckle-length buckle-end-length buckle-height include-middle end-supports?] :or [end-supports? true]}]
  (let
   [buckle-end-width (- buckle-width (* 2 buckle-thickness))
    palm-buckle-triangle (polygon [[0 triangle-length] [triangle-width 0] [0 0]])
    palm-buckle-side (translate [0 (- (+ buckle-length buckle-end-length))]
                                (square buckle-thickness (+ buckle-length buckle-end-length) :center false))
    palm-buckle-2d (union
                     ; Triangles
                    (translate [(/ buckle-width 2) 0 0] palm-buckle-triangle)
                    (translate [(- (/ buckle-width 2)) 0 0]
                               (mirror [1 0] palm-buckle-triangle))
                     ; Sticks on the triangles
                    (translate [(/ buckle-width 2) 0 0] palm-buckle-side)
                    (translate [(- (/ buckle-width 2)) 0 0]
                               (mirror [1 0] palm-buckle-side))
                    (if include-middle
                      (union
                        ; Square in the middle
                       (translate [0 (- (+ buckle-length (/ buckle-end-length 2)))]
                                  (square buckle-end-width buckle-end-length))
                        ; Bar at the end
                       (translate [0 (- (+ buckle-length buckle-end-length (/ buckle-thickness 2)))]
                                  (square (+ buckle-width (* 2 buckle-thickness)) buckle-thickness)))
                      nil))]
    (extrude-linear {:height buckle-height} palm-buckle-2d)))

(defn buckle-holes [& {:keys [buckle-thickness buckle-length buckle-width buckle-width-adjust triangle-length triangle-width buckle-height]}]
  (let [hole-x-translate (- (/ (+ buckle-width buckle-width-adjust) 2) (- triangle-width buckle-thickness) 0.2)]
    (union
     (translate [hole-x-translate 0 0]
                (cube (+ triangle-width 0.5) 10 (+ buckle-height 0.5) :center false))
     (translate [(+ hole-x-translate (- triangle-width buckle-thickness)) buckle-length 0] ; clear out some space on the other end of the buckle
                (cube (+ triangle-width 0.25) 2 (+ buckle-height 0.5) :center false))
     (translate [(- hole-x-translate) 0 0]
                (mirror [1 0] (cube (+ triangle-width 0.5) 10 (+ buckle-height 0.5) :center false)))
     (translate [(- (- hole-x-translate) (- triangle-width buckle-thickness)) buckle-length 0] ;clear out some space on the other end of the buckle
                (mirror [1 0] (cube (+ triangle-width 0.25) 2 (+ buckle-height 0.5) :center false))))))

;;;;;;;;;;;;;
;; Hotswap ;;
;;;;;;;;;;;;;

(def pin-cutout-height 0.7)
(def pin-offset 1.7)
(def socket-pin  (translate [0 (/ pin-cutout-height 2) 0] (union
                                                           (translate [0 0 2] (cube 0.8 pin-cutout-height 4))
                                                           (translate [0 0 (+ 4 2.5)] (cube 2.2 pin-cutout-height 5))
                                                           (translate [0 0 (+ 9 0.5)] (cube 1.7 pin-cutout-height 1)))))
;; Hotswap socket test
(def socket-distance 5.5)
(def socket-height 5.5)
(def socket-width (+ socket-distance 4))
(def hotswap-buckle-length 4)
(def grip-length 1)
(defn pins-place [socket-pin]
  (union
   (translate [(- (/ socket-distance 2)) 0 (- -4 (/ socket-height 2))] socket-pin)
   (translate [(/ socket-distance 2) 0 (- -4 (/ socket-height 2))] socket-pin)))
(def hotswap-socket-pins (pins-place socket-pin))
(def socket-join-height (if printed-hotswap? (- socket-height 3) 3))
(def hotswap-clamp
  (let [grip-width    2
        grip-height 3
        grip            (polygon [[0 0] [grip-width 0] [grip-width grip-length] [0 grip-length]])
        thickness 1
        width (+ socket-width 0.25) ; give some wiggle room
        length (+ hotswap-buckle-length 0.15) ; give some wiggle room
        grip-offset     (+ (/ width 2) thickness)
        socket-slot-height (- socket-height 1)
        flat-model (union
                    (translate [(/ width 2) (- length)] (square thickness length :center false))
                    (translate [(/ width -2) (- length)] (mirror [1 0] (square thickness length :center false)))
                    (translate [0 (+ (- (+ length (/ thickness 2))) (/ pin-offset 2))] (square (+ width (* 2 thickness)) (+ thickness pin-offset))))
        flat-grip-model (union
                         (translate [(- grip-offset) 0] grip)
                         (translate [grip-offset 0] (mirror [1 0] grip)))]
    (union
     (extrude-linear {:height socket-slot-height} flat-model)
     (translate [0 0 (/ (- grip-height socket-slot-height) 2)] (extrude-linear {:height grip-height} flat-grip-model))
                           ; Bottom part of the holder
     (let [bottom-width (+ width thickness thickness)
           bottom-length (+ length thickness grip-length)]
       (difference
        (translate [0 (+ (/ bottom-length -2) grip-length) (- (/ socket-slot-height -2) (/ thickness 2))] (cube bottom-width bottom-length thickness))
        (translate [0 (- (- length pin-offset)) 0] hotswap-socket-pins))))))
(def hotswap-socket (difference
                     (translate [0 (/ (- hotswap-buckle-length pin-offset) 2) 0] (cube socket-width (- hotswap-buckle-length pin-offset) socket-height))
                     hotswap-socket-pins
;                     (translate [insert-path-x-offset 0.7 0] insert-path)
;                     (translate [(- insert-path-x-offset) 0.7 0] insert-path)
;                     (translate [0 -1.5 0] (pins-place socket-pin-square))
                     ))

(defn official-hotswap [width length height wings?] (translate [0 0 0] (difference
                                                                        (union
                                                                         (translate [0 -0.4 0] (cube width length height))
                                                                         (translate [(* 0.866 socket-distance) (* -0.5 socket-distance) 0] (cube width length height))
                                                                         (if wings?
                                                                           (union
                                                                            (translate [(/ width -2) -0.4 0] (cube width 2.5 height))
                                                                            (translate [(+ (* 0.866 socket-distance) (/ width 2)) (* -0.5 socket-distance) 0] (cube width 2.5 height)))
                                                                           nil)))))
(def official-hotswap-clamp (translate [0 -2.5 0] (difference
                                                   (official-hotswap 6.25 6.25 5.5 false)
                                                   (translate [0 0 2.5] (official-hotswap 5.25 5.25 2 true))
                                                   ; The middle piece
                                                   (->>
                                                    (cube 2 5 2)
                                                    (translate [(+ (/ (* 0.866 socket-distance) 2) 0.5) (+ (/ (* 0.5 socket-distance) -1) 2) 2.5])
                                                    (rotate (deg2rad -30) [0 0 1])))))


(def plate-mount-buckle-width (- keyswitch-width 4))
(defn position-socket-clamp [shape] (->>
                                     shape
                                     (translate [0 hotswap-buckle-length 0])
                                     (rotate (deg2rad -30) [0 0 1])
                                     (translate [-3 0.5 (/ socket-height 2)])))
(def distance-from-socket 1.6)
(defn position-official-socket-clamp [shape] (->>
                                              shape
                                              (translate [0 hotswap-buckle-length 0])
                                              (translate [-5 (+ distance-from-socket 0.8) (/ socket-height 2)])))

(def rotated-socket-clamp
  (->>
   hotswap-clamp
   position-socket-clamp))

(def clamp-buckle-y-offset (+ -1 (- distance-from-socket)))
(def plate-mount-buckle-height 2)
(def clamp-buckle (->>
                   (buckle
                    :include-middle      false
                    :triangle-length     1.75
                    :triangle-width      3.4
                    :buckle-width-adjust 0
                    :buckle-width        plate-mount-buckle-width
                    :buckle-thickness    1.8
                    :buckle-length       (+ socket-height plate-thickness -0.1) ; Remove some length to make less wiggle room
                    :buckle-end-length   0
                    :buckle-height       (+ plate-mount-buckle-height 0.35)) ; Add more thickness than the holes to account for wanting no wiggle room
                   (rotate (deg2rad 90) [1 0 0])
                   (translate [0  clamp-buckle-y-offset (+ socket-height plate-thickness)])))
(def hotswap-clamp-key-mount
  (union
   rotated-socket-clamp
   clamp-buckle
   ; Connect the left buckle to the socket
   (hull
    (translate [(- (/ plate-mount-buckle-width -2) -3.5) -1.2 (/ socket-join-height 2)]
               (rotate (deg2rad -30) [0 0 1] (cube 7 0.1 socket-join-height)))
    (translate [(- (/ plate-mount-buckle-width -2) 0) clamp-buckle-y-offset (/ socket-join-height 2)]
               (cube 2 plate-mount-buckle-height socket-join-height)))
   ; Connect the right buckle to the socket
   (hull
    (->> (cube 0.1 hotswap-buckle-length socket-join-height)
         (translate
          [(/ (+ socket-width 0.6) 2)
           (/ (+ hotswap-buckle-length 0.5) -2)
           (+ (/ socket-join-height -2))])
         position-socket-clamp)
    (translate [(- (/ plate-mount-buckle-width 2) 1) -1.5 (/ socket-join-height 2)]
               (cube 1 1 socket-join-height))
    (translate [(+ (/ plate-mount-buckle-width 2) 0.5) clamp-buckle-y-offset (/ socket-join-height 2)]
               (cube 1 plate-mount-buckle-height socket-join-height)))))

(def official-hotswap-clamp-key-mount (union
                                       (position-official-socket-clamp official-hotswap-clamp)
                                       clamp-buckle
                                       ; Connect the buckles together with a cube
                                       (difference
                                        (translate [(- (/ plate-mount-buckle-width -2) 1.8) (- clamp-buckle-y-offset (/ (+ plate-mount-buckle-height 0.35) 2)) 0]
                                                   (cube (+ 1.8 plate-mount-buckle-width) (+ (- clamp-buckle-y-offset) (/ (+ plate-mount-buckle-height 0.35) 2) 2) socket-join-height :center false))
                                        (position-official-socket-clamp (translate [0 -2.5 2.5] (official-hotswap 6 6 4 true))))))
(def buckle-hole-y-translate (+ (/ keyswitch-height 2) plate-mount-buckle-height distance-from-socket))
(def buckle-holes-on-key (->>
                          (buckle-holes
                           :buckle-thickness 1.8
                           :buckle-width plate-mount-buckle-width
                           :buckle-width-adjust 0
                           :buckle-length (+ socket-height plate-thickness)
                           :triangle-width 3.4
                           :triangle-length 1.75
                           :buckle-height plate-mount-buckle-height)
                          (rotate (deg2rad 90) [1 0 0])
                          (translate [0 buckle-hole-y-translate (+ (- socket-height))])))
(def single-plate-with-hotswap (difference
                                (translate [0 2 (/ plate-thickness 2)] (cube (+ keyswitch-width 4) (+ keyswitch-height 7) 3))
                                (translate [0 0 (/ plate-thickness 2)] (cube keyswitch-width keyswitch-height 3))
                                buckle-holes-on-key))

(defn hotswap-place [hotswap] (let [bottom-hotswap (rotate (deg2rad 180) [0 0 1] hotswap)] (union
                                        ; Bottom mounts
                                                                                            (apply union
                                                                                                   (for [column columns
                                                                                                         row [0 1]
                                                                                                         :when (or (.contains [2 3] column)
                                                                                                                   (not= row lastrow))]
                                                                                                     (->> bottom-hotswap
                                                                                                          (key-place column row))))
                                                                                            (apply union
                                                                                                   (for [column columns
                                                                                                         row [2 3]
                                                                                                         :when (or (.contains [2 3] column)
                                                                                                                   (not= row lastrow))]
                                                                                                     (->> hotswap
                                                                                                          (key-place column row))))
                                                                                            (minithumb-mr-place (if trackball-enabled bottom-hotswap hotswap))
                                                                                            (minithumb-br-place hotswap)
                                                                                            (if trackball-enabled nil (minithumb-tl-place bottom-hotswap))
                                                                                            (minithumb-bl-place bottom-hotswap)
                                                                                            (minithumb-tr-place bottom-hotswap))))

(def hotswap-holes (hotswap-place buckle-holes-on-key))

(def unified-pin-hotswap-mount (translate
                                [0 (- buckle-hole-y-translate distance-from-socket plate-mount-buckle-height 0.25) (- socket-height)]
                                (rotate (deg2rad 180) [0 0 1]
                                        (if printed-hotswap? (union
                                                              hotswap-clamp-key-mount
                                                              (->>
                                                               (union
                                                                hotswap-socket-pins
                                                                hotswap-socket)
                                                               (translate [0 (- (- hotswap-buckle-length pin-offset)) 0])
                                                               position-socket-clamp)) official-hotswap-clamp))))

(def hotswap-tester (hotswap-place unified-pin-hotswap-mount))

(def single-hotswap-clearance
  (->>
   (cube (+ socket-width 4) (+ hotswap-buckle-length 4) (+ socket-height 3))
   (translate [0 (+ distance-from-socket) -1.5])
   (translate [0 (- hotswap-buckle-length) 0])
   position-socket-clamp
   (rotate (deg2rad 180) [0 0 1])
   (translate
    [0 (- buckle-hole-y-translate distance-from-socket plate-mount-buckle-height 0.25) (- socket-height)])))

(def hotswap-clearance (hotswap-place single-hotswap-clearance))

;;;;;;;;;;;;;;;
;; Trackball ;;
;;;;;;;;;;;;;;;

(def dowel-depth-in-shell 1.5)
(def bearing-protrude (- 3 dowel-depth-in-shell)) ; Radius of the baring minus how deep it's going into the shell
(def trackball-width 34)
(def trackball-width-plus-bearing (+ bearing-protrude trackball-width 1)) ; Add one just to give some wiggle
(def holder-thickness 4.2)
(def outer-width (+ (* 2 holder-thickness) trackball-width-plus-bearing))

(def axel-angle 15)
(def dowell-width 3)
(def dowel-top-change 0)
(def dowel-top-height 1.5)
(def dowell-height 6) ; Dowel height is actually 6mm. But attempting to get it to "snap" in place
(def dowell (union (cylinder (- (/ dowell-width 2) dowel-top-change) (+ dowell-height dowel-top-height) :fn 50) (cylinder (/ dowell-width 2) dowell-height :fn 50)))
(def bearing (cylinder (/ 8.5 2) 3)) ; Bearing is actually 6mm x 2.5mm, model it as 8.5mm x 3 to give it room to spin
(def dowell-bearing (rotate (deg2rad 90) [1 0 0] (union dowell bearing)))
(defn rotated_dowell [angle]
  (rotate (deg2rad angle) [0, 0, 1] (rotate (deg2rad axel-angle) [0, 1, 0] (translate [(+ (/ trackball-width-plus-bearing 2) dowel-depth-in-shell) 0 0] (union
                                                                                                                                                          ; Add a cube on the side of the dowell so there's an insertion point when we diff with the shell
                                                                                                                                                         (translate [(- (/ dowell-width 2)) 0 0] (cube (+ dowell-width 1) (- dowell-height dowel-top-change) dowell-width))
                                                                                                                                                         dowell-bearing)))))

(def dowells (union
              (rotated_dowell 0)
              (rotated_dowell 120)
              (rotated_dowell 240)))
(def vertical-hold 0) ; Millimeters of verticle hold after the curviture of the sphere ends to help hold the ball in

(def cup (difference
          (union
           (sphere (/ outer-width 2)) ; Main cup sphere
           (translate [0, 0, (/ vertical-hold 2)] (cylinder (/ outer-width 2) vertical-hold)) ; add a little extra to hold ball in
           )
          (sphere (/ trackball-width-plus-bearing 2))
          (translate [0, 0, (+ (/ outer-width 2) vertical-hold)] (cylinder (/ outer-width 2) outer-width)) ; cut out the upper part of the main cup spher
          ))

; We know the ball will sit approx bearing-protrude over the sensor holder. Eliminate the bottom and make it square
; up to that point with trim
(def trim (- (+ holder-thickness bearing-protrude) 0.5))
(def bottom-trim-origin [0 0 (- (- (/ outer-width 2) (/ trim 2)))])
(def bottom-trim ; trim the bottom off of the cup to get a lower profile
  (translate bottom-trim-origin (cube outer-width outer-width trim)))

(def holder-negatives (union
                       dowells
                       bottom-trim))
(def cup-bottom
  (translate [0 0 (- (- (/ outer-width 2) (/ trim 2)))] (cube outer-width outer-width trim)))
(def test-holder
  (difference
   cup
   holder-negatives))

(def test-ball (sphere (/ trackball-width 2)))

(def test-holder-with-ball (union
                            (translate [0 0 (- (/ holder-thickness 2))] cup)
                            test-ball))

(defn clearance [extrax extray extraz]
  (translate [0 0 (/ extraz 2)]
             (cube (+ keyswitch-width extrax) (+ keyswitch-width extray) extraz)))

(def thumb-key-clearance (union
                          (thumb-1x-layout (clearance 0 0 30))
                          (thumb-15x-layout (rotate (/ π 2) [0 0 1] (clearance 2.5 2.5 30)))))

(def trackball-hotswap-clearance
  (union
   (key-place 0 2 single-hotswap-clearance)
   (key-place 1 2 single-hotswap-clearance)
   (key-place 2 3 single-hotswap-clearance)))
(def key-clearance (union
                    (apply union
                           (for [column columns
                                 row rows
                                 :when (or (.contains [2 3] column)
                                           (not= row lastrow))]
                             (->> (clearance keyswitch-width keyswitch-width 30)
                                  (key-place column row))))
                    trackball-hotswap-clearance))

(defn trackball-mount-rotate [thing] (rotate (deg2rad -12) [0 0 1]
                                             (rotate (deg2rad 34) [1 0 0]
                                                     (rotate (deg2rad -39) [0 1 0] thing))))

(def sensor-length 28)
(def sensor-width 22)
(def sensor-holder-width (/ sensor-width 2))
(def sensor-height 7)
(def sensor-holder-arm (translate [0 -0.5 0]
                                  (union
                                   (translate [0 (- (/ 4 2) (/ 1 2)) 1] (cube sensor-holder-width 4 2))
                                   (translate [0 0 (- (/ sensor-height 2))] (cube sensor-holder-width 1 sensor-height))
                                   (translate [0 (- (/ 4 2) (/ 1 2)) (- (+ sensor-height (/ 1 2)))] (cube sensor-holder-width 4 1)))))
(def sensor-holder
  (translate (map + bottom-trim-origin [0 0 (/ trim 2)])
             (union
              (translate [0 (- (/ sensor-length 2)) 0] sensor-holder-arm)
              (->>
               sensor-holder-arm
               (mirror [0 1 0])
               (translate [0 (/ sensor-length 2) 0])))))

(defn sensor-hole-angle [shape] (->> shape
                                     (rotate (deg2rad -55) [0 1 0])
                                     (rotate (deg2rad 40) [0 0 1])))
(defn dowell-angle [shape] (->> shape
                                (rotate (deg2rad (+ 90 35)) [0 0 1])
                                (rotate (deg2rad -30) [0 1 0])
                                (rotate (deg2rad 25) [1 0 0])))

(def rotated-dowells
  (dowell-angle
   (translate [0 0 (- (/ holder-thickness 2))] dowells)))

(def rotated-bottom-trim     (sensor-hole-angle bottom-trim))

; This makes sure we can actually insert the trackball by leaving a column a little wider than it's width
(def trackball-insertion-cyl (dowell-angle (translate [0 0 (- (/ trackball-width 2) (/ holder-thickness 2))]
                                                      (cylinder (+ (/ trackball-width 2) 1) (+ (/ outer-width 2) 10)))))

(def trackball-raise (+ bearing-protrude 0.5))
(defn filler-rotate [p] (->> p
                             (trackball-mount-rotate)
                             ;                       (rotate (deg2rad 0) [0 1 0])
                             (rotate (deg2rad 20) [0 0 1])
                             ;                         (rotate (deg2rad -40) [1 0 0])
                             ))
(def filler-half-circle (->>  (difference
                               (sphere (/ trackball-width-plus-bearing 2))
                               (translate [0 0 (+ (/ outer-width 2) vertical-hold)] (cylinder (/ outer-width 2) outer-width)) ; cut out the upper part of the main cup spher
                               )
                              (translate [0 0 trackball-raise])
                              filler-rotate))

(def trackball-mount
  (union
   (difference
    (union
     (trackball-mount-rotate cup)
     (filler-rotate cup))
    ; subtract out room for the axels
    rotated-dowells
    ; Subtract out the bottom trim clearing a hole for the sensor
    rotated-bottom-trim)
   (sensor-hole-angle sensor-holder)))

(def raised-trackball (translate [0 0 trackball-raise] (sphere (+ (/ trackball-width 2) 0.5))))
(def trackball-origin (map + minithumb-tip-origin [-8.5 10 -5]))

(def oled-holder-cut
  (->>
   (union
      ; cut for oled pcb
    (difference
     (translate [0 0 1] (apply cube (add-vec [0.5 0.5 0.1] oled-pcb-size)))
     (for [x [-2 2] y [-2 2]]
       (translate (div-vec oled-mount-size [x y 1])
                  (cylinder 2.5 (- oled-holder-thickness 2.5)))))
      ; cut for oled screen
    (translate oled-screen-offset (apply cube oled-screen-size))
      ; cut for oled screen viewport
    (translate oled-viewport-offset (apply cube oled-viewport-size))
      ; cutout for oled cable
    (->> (cube 10 2 10)
         (translate oled-screen-offset)
         (translate [0 (- (+ (/ (nth oled-screen-size 1) 2) 1)) (+ plate-thickness 1.0)]))
    (for [x [-2 2] y [-2 2]]
      (translate (div-vec oled-mount-size [x y 1]) (cylinder (/ 2.5 2) 10))))
   (rdy 180)
   (translate [0 0 (/ oled-holder-thickness 2)])))

(def oled-holder
  (->>
    ; main body
   (apply cube oled-holder-size)
   (rdy 180)
   (translate [0 0 (/ oled-holder-thickness 2)])))

(def oled-holder-block
  (->>
  ; main body
 (apply cube oled-holder-size)
 (rdy 180)
 (translate [0 0 -2])))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(def case-filler-cup (difference (translate trackball-origin filler-half-circle)
                                 key-clearance
                                 thumb-key-clearance
                                 (translate trackball-origin rotated-dowells)))

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (- (/ height 2) 10)])))


(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

(def left-wall-x-offset 33.2)
(def left-wall-x-offset-trackball 0)
(def left-wall-x-offset-oled -5)
(def left-wall-z-offset 3)

(defn left-key-position [row direction]
  (map - (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0]) [left-wall-x-offset 0 left-wall-z-offset]) )

(defn left-key-position-oled [row direction]
  (map - (key-position 0 row [(*  mount-width -0.5) (* direction  mount-height 0.5) 0])
       [left-wall-x-offset 0 left-wall-z-offset]
       (key-position 0 row [(*  oled-holder-width -0.5) (* direction oled-holder-height 0.5) 0])))

(defn left-key-place [row direction shape]
  (translate (left-key-position row direction) shape))

(defn left-key-position-narrow [row direction]
  (map - (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0]) [left-wall-x-offset-trackball 0 left-wall-z-offset]))

(defn left-key-place-narrow [row direction shape]
  (translate (left-key-position-narrow row direction) shape))

(defn left-wall-plate-position [xdir ydir]
  (->>
   (add-vec
    [left-wall-x-offset-oled 0 (- left-wall-z-offset 2)]
    (key-position 0 0 [0 0 0])
    [(* mount-width -0.5) (* mount-width 0.5) 0]
    [(* oled-holder-width -0.5) (* oled-holder-height -0.5) 0]
    [(* xdir oled-holder-width 0.5) (* ydir oled-holder-height 0.5) 0]
    [-3 7 -7])))


(defn left-wall-plate-place [xdir ydir shape]
  (->> shape
       (translate (left-wall-plate-position xdir ydir))
       (rotate oled-mount-rotation-x-old [1 0 0])
       (rotate oled-mount-rotation-z-old [0 0 1])))

(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) -1])
(defn wall-locate2 [dx dy] [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset])
(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness)) (* dy (+ wall-xy-offset wall-thickness)) wall-z-offset])

(defn wall-locate2-xy [dx dy xy] [(* dx xy) (* dy xy) wall-z-offset])
(defn wall-locate3-xy [dx dy xy] [(* dx (+ xy wall-thickness)) (* dy (+ xy wall-thickness)) wall-z-offset])

(defn wall-brace-xy [place1 dx1 dy1 post1 place2 dx2 dy2 post2 xy1 xy2]
  (union
   (hull
    (place1 post1)
    (place1 (translate (wall-locate1 dx1 dy1) post1))
    (place1 (translate (wall-locate2-xy dx1 dy1 xy1) post1))
    (place1 (translate (wall-locate3-xy dx1 dy1 xy1) post1))
    (place2 post2)
    (place2 (translate (wall-locate1 dx2 dy2) post2))
    (place2 (translate (wall-locate2-xy dx2 dy2 xy2) post2))
    (place2 (translate (wall-locate3-xy dx2 dy2 xy2) post2)))
   (bottom-hull
    (place1 (translate (wall-locate2-xy dx1 dy1 xy1) post1))
    (place1 (translate (wall-locate3-xy dx1 dy1 xy1) post1))
    (place2 (translate (wall-locate2-xy dx2 dy2 xy2) post2))
    (place2 (translate (wall-locate3-xy dx2 dy2 xy2) post2)))))

(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (union
   (hull
    (place1 post1)
    (place1 (translate (wall-locate1 dx1 dy1) post1))
    (place1 (translate (wall-locate2 dx1 dy1) post1))
    (place1 (translate (wall-locate3 dx1 dy1) post1))
    (place2 post2)
    (place2 (translate (wall-locate1 dx2 dy2) post2))
    (place2 (translate (wall-locate2 dx2 dy2) post2))
    (place2 (translate (wall-locate3 dx2 dy2) post2)))
   (bottom-hull
    (place1 (translate (wall-locate2 dx1 dy1) post1))
    (place1 (translate (wall-locate3 dx1 dy1) post1))
    (place2 (translate (wall-locate2 dx2 dy2) post2))
    (place2 (translate (wall-locate3 dx2 dy2) post2)))))

(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2))

(def right-wall
   (if pinky-15u
    (union
     ; corner between the right wall and back wall
     (if (> first-15u-row 0)
       (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
       (union (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 0 1 wide-post-tr)
              (key-wall-brace lastcol 0 0 1 wide-post-tr lastcol 0 1 0 wide-post-tr)))
     ; corner between the right wall and front wall
     (if (= last-15u-row extra-cornerrow)
       (union (key-wall-brace lastcol extra-cornerrow 0 -1 web-post-br lastcol extra-cornerrow 0 -1 wide-post-br)
              (key-wall-brace lastcol extra-cornerrow 0 -1 wide-post-br lastcol extra-cornerrow 1 0 wide-post-br))
       (key-wall-brace lastcol extra-cornerrow 0 -1 web-post-br lastcol extra-cornerrow 1 0 web-post-br))

     (if (>= first-15u-row 2)
       (for [y (range 0 (dec first-15u-row))]
         (union (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br)
                (key-wall-brace lastcol y 1 0 web-post-br lastcol (inc y) 1 0 web-post-tr))))

     (if (>= first-15u-row 1)
       (for [y (range (dec first-15u-row) first-15u-row)] (key-wall-brace lastcol y 1 0 web-post-tr lastcol (inc y) 1 0 wide-post-tr)))

     (for [y (range first-15u-row (inc last-15u-row))] (key-wall-brace lastcol y 1 0 wide-post-tr lastcol y 1 0 wide-post-br))
     (for [y (range first-15u-row last-15u-row)] (key-wall-brace lastcol (inc y) 1 0 wide-post-tr lastcol y 1 0 wide-post-br))

     (if (<= last-15u-row (- extra-cornerrow 1))
       (for [y (range last-15u-row (inc last-15u-row))] (key-wall-brace lastcol y 1 0 wide-post-br lastcol (inc y) 1 0 web-post-br)))

     (if (<= last-15u-row (- extra-cornerrow 2))
       (for [y (range (inc last-15u-row) extra-cornerrow)]
         (union (key-wall-brace lastcol y 1 0 web-post-br lastcol (inc y) 1 0 web-post-tr)
                (key-wall-brace lastcol (inc y) 1 0 web-post-tr lastcol (inc y) 1 0 web-post-br))))
     )
    (union (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
           (if extra-row
             (union (for [y (range 0 (inc lastrow))] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br))
                    (for [y (range 1 (inc lastrow))] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr)))
             (union (for [y (range 0 lastrow)] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br))
                    (for [y (range 1 lastrow)] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr)))
             )
           (key-wall-brace lastcol extra-cornerrow 0 -1 web-post-br lastcol extra-cornerrow 1 0 web-post-br)
           )))

(def cf-thumb-wall
  (union
   ; thumb walls
   (wall-brace cfthumb-mr-place  0 -1 web-post-br cfthumb-tr-place  0 -1 web-post-br)
   (wall-brace cfthumb-mr-place  0 -1 web-post-br cfthumb-mr-place  0 -1.15 web-post-bl)
   (wall-brace cfthumb-br-place  0 -1 web-post-br cfthumb-br-place  0 -1 web-post-bl)
   (wall-brace cfthumb-bl-place -0.3  1 thumb-post-tr cfthumb-bl-place  0  1 thumb-post-tl)
   (wall-brace cfthumb-br-place -1  0 web-post-tl cfthumb-br-place -1  0 web-post-bl)
   (wall-brace cfthumb-bl-place -1  0 thumb-post-tl cfthumb-bl-place -1  0 web-post-bl)
   ; cfthumb corners
   (wall-brace cfthumb-br-place -1  0 web-post-bl cfthumb-br-place  0 -1 web-post-bl)
   (wall-brace cfthumb-bl-place -1  0 thumb-post-tl cfthumb-bl-place  0  1 thumb-post-tl)
   ; cfthumb tweeners
   (wall-brace cfthumb-mr-place  0 -1.15 web-post-bl cfthumb-br-place  0 -1 web-post-br)
   (wall-brace cfthumb-bl-place -1  0 web-post-bl cfthumb-br-place -1  0 web-post-tl)
   (wall-brace cfthumb-tr-place  0 -1 web-post-br (partial key-place (+ innercol-offset 3) lastrow)  0 -1 web-post-bl)
   ; clunky bit on the top left cfthumb connection  (normal connectors don't work well)
   (bottom-hull
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (cfthumb-bl-place (translate (wall-locate2 -0.3 1) thumb-post-tr))
    (cfthumb-bl-place (translate (wall-locate3 -0.3 1) thumb-post-tr)))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (cfthumb-bl-place (translate (wall-locate2 -0.3 1) thumb-post-tr))
    (cfthumb-bl-place (translate (wall-locate3 -0.3 1) thumb-post-tr))
    (cfthumb-ml-place thumb-post-tl))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 web-post)
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (cfthumb-ml-place thumb-post-tl))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 web-post)
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
    (key-place 0 (- cornerrow innercol-offset) web-post-bl)
    (cfthumb-ml-place thumb-post-tl))
   (hull
    (cfthumb-bl-place thumb-post-tr)
    (cfthumb-bl-place (translate (wall-locate1 -0.3 1) thumb-post-tr))
    (cfthumb-bl-place (translate (wall-locate2 -0.3 1) thumb-post-tr))
    (cfthumb-bl-place (translate (wall-locate3 -0.3 1) thumb-post-tr))
    (cfthumb-ml-place thumb-post-tl))
   ; connectors below the inner column to the thumb & second column
   (if inner-column
     (union
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 0 (dec cornerrow) web-post-br)
       (key-place 0 cornerrow web-post-tr))
      (hull
       (key-place 0 cornerrow web-post-tr)
       (key-place 1 cornerrow web-post-tl)
       (key-place 1 cornerrow web-post-bl))
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 0 cornerrow web-post-tr)
       (key-place 1 cornerrow web-post-bl))
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 1 cornerrow web-post-bl)
       (cfthumb-ml-place thumb-post-tl))))))

(def mini-thumb-wall
  (union
   ; thumb walls
   ;(wall-brace minithumb-mr-place  0 -1 web-post-br minithumb-tr-place  0 -1 minithumb-post-br)
   ;(wall-brace minithumb-mr-place  0 -1 web-post-br minithumb-mr-place  0 -1 web-post-bl)
   (color [0.1 0.4 0.5 1](wall-brace minithumb-bl-place -1  0 web-post-bl minithumb-br-place -1  0 web-post-tl)
   (wall-brace minithumb-br-place  0 -1 web-post-br minithumb-br-place  0 -1 web-post-bl)
   (wall-brace minithumb-br-place -1  0 web-post-tl minithumb-br-place -1  0 web-post-bl)
   (wall-brace minithumb-bl-place -1  0 web-post-tl minithumb-bl-place -1  0 web-post-bl)
)
   ( color [0.8 0.2 0.5 1] (wall-brace minithumb-br-place  0 -1 web-post-br minithumb-br-place  0 -1 web-post-bl))
  (if (or trackball-enabled joystick-enabled) nil (wall-brace minithumb-bl-place  0  1 web-post-tr minithumb-bl-place  0  1 web-post-tl))
   (color [0.5 0.1 0.8 0.8](wall-brace minithumb-br-place -1  0 web-post-tl minithumb-br-place -1  0 web-post-bl))
   (color [0.3 0.5 0.7 1](wall-brace minithumb-bl-place -1  0 web-post-tl minithumb-bl-place -1  0 web-post-bl))
;
   ; minithumb corners
   (wall-brace minithumb-br-place -1  0 web-post-bl minithumb-br-place  0 -1 web-post-bl)
   (color [1 0 0 1](wall-brace minithumb-bl-place -1  0 web-post-tl minithumb-bl-place  0  1 web-post-tl))
   ; minithumb tweeners
  
   (wall-brace minithumb-bl-place -1  0 web-post-bl minithumb-br-place -1  0 web-post-tl)
   (wall-brace minithumb-tr-place  0 -1 minithumb-post-br (partial key-place (+ innercol-offset 3) lastrow)  0 -1 web-post-bl)   
    
   ; connectors below the inner column to the thumb & second column
   (if inner-column
     (color [0.3 0.4 0.8 1](union
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 0 (dec cornerrow) web-post-br)
       (key-place 0 cornerrow web-post-tr))
      (hull
       (key-place 0 cornerrow web-post-tr)
       (key-place 1 cornerrow web-post-tl)
       (key-place 1 cornerrow web-post-bl))
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 0 cornerrow web-post-tr)
       (key-place 1 cornerrow web-post-bl))
     ;removed so it doesnt clash with the trackball
     (if (or trackball-enabled joystick-enabled) nil (hull
       (key-place 1 lastrow web-post-tl)
       (key-place 1 cornerrow web-post-bl)
       (minithumb-tl-place minithumb-post-tl)))
                            
       (triangle-hulls
         (key-place 0 (dec cornerrow) web-post-bl)
         (key-place 1 cornerrow web-post-bl)
         (key-place 0 cornerrow web-post-bl))               
                            )))))

(def default-thumb-wall
  (union
   ; thumb walls
   (wall-brace thumb-mr-place  0 -1 web-post-br thumb-tr-place  0 -1 thumb-post-br)
   (wall-brace thumb-mr-place  0 -1 web-post-br thumb-mr-place  0 -1 web-post-bl)
   (wall-brace thumb-br-place  0 -1 web-post-br thumb-br-place  0 -1 web-post-bl)
   (wall-brace thumb-ml-place -0.3  1 web-post-tr thumb-ml-place  0  1 web-post-tl)
   (wall-brace thumb-bl-place  0  1 web-post-tr thumb-bl-place  0  1 web-post-tl)
   (wall-brace thumb-br-place -1  0 web-post-tl thumb-br-place -1  0 web-post-bl)
   (wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place -1  0 web-post-bl)
   ; thumb corners
   (wall-brace thumb-br-place -1  0 web-post-bl thumb-br-place  0 -1 web-post-bl)
   (wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place  0  1 web-post-tl)
   ; thumb tweeners
   (wall-brace thumb-mr-place  0 -1 web-post-bl thumb-br-place  0 -1 web-post-br)
   (wall-brace thumb-ml-place  0  1 web-post-tl thumb-bl-place  0  1 web-post-tr)
   (wall-brace thumb-bl-place -1  0 web-post-bl thumb-br-place -1  0 web-post-tl)
   (wall-brace thumb-tr-place  0 -1 thumb-post-br (partial key-place (+ innercol-offset 3) lastrow)  0 -1 web-post-bl)
   ; clunky bit on the top left thumb connection  (normal connectors don't work well)
   (bottom-hull
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
    (thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr)))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
    (thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr))
    (thumb-tl-place thumb-post-tl))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 web-post)
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (thumb-tl-place thumb-post-tl))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 web-post)
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
    (key-place 0 (- cornerrow innercol-offset) web-post-bl)
    (key-place 0 (- cornerrow innercol-offset) (translate (wall-locate1 0 0) web-post-bl))
    (thumb-tl-place thumb-post-tl))
   ; connectors below the inner column to the thumb & second column
   (if inner-column
     (union
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 0 (dec cornerrow) web-post-br)
       (key-place 0 cornerrow web-post-tr))
      (hull
       (key-place 0 cornerrow web-post-tr)
       (key-place 1 cornerrow web-post-tl)
       (key-place 1 cornerrow web-post-bl))
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 0 cornerrow web-post-tr)
       (key-place 1 cornerrow web-post-bl))
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 1 cornerrow web-post-bl)
       (thumb-tl-place thumb-post-tl))))
   (hull
    (thumb-ml-place web-post-tr)
    (thumb-ml-place (translate (wall-locate1 -0.3 1) web-post-tr))
    (thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
    (thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr))
    (thumb-tl-place thumb-post-tl))))

;switching walls depending on thumb-style used
(def thumb-wall-type
  (case thumb-style
    "default" default-thumb-wall
    "cf" cf-thumb-wall
    "mini" mini-thumb-wall))

(def trackball-walls
  (color [0 1 1 1](union
   ; clunky bit on the top left thumb connection  (normal connectors don't work well)
   ; merging with hulls to the trackball mount
          (difference
           (union
     ; Thumb to rest of case
            (bottom-hull
             (bottom 25 (left-key-place-narrow cornerrow -1 (translate (wall-locate3 -1 2) big-boi-web-post)))
             ;     (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
             ;small wall at bottom between thumb and trackball
             (minithumb-bl-place web-post-tr)
             (minithumb-bl-place web-post-tl)))
           key-clearance
           thumb-key-clearance
           (translate trackball-origin rotated-bottom-trim)
           (translate trackball-origin rotated-dowells)))))

(def thumb-to-left-wall (union
                         (hull
                          (minithumb-bl-place  web-post-tr)
                          (minithumb-bl-place (translate (wall-locate3 0 1) web-post-tl))
                          (key-place 1 cornerrow web-post-bl)
                          )
                         ;(wall-brace-xy (partial left-key-place 2 -1) -1 0 oled-post  (partial key-place 0 (- lastrow innercol-offset)) -2 1  web-post-bl wall-xy-offset-thin wall-xy-offset-thin)
                         ;(wall-brace (partial key-place 0 (- lastrow innercol-offset)) -1 0   web-post-bl (partial key-place 0 (- lastrow innercol-offset)) 0 0 web-post-br)
                        ; clunky bit on the top left thumb connection  (normal connectors don't work well)
                        ;(bottom-hull
                        ; (left-key-place 0 (- cornerrow innercol-offset)  web-post-bl)
                        ; (left-key-place 0 (- cornerrow innercol-offset)  web-post-bl)
                        ; (thumb-bl-place  web-post-tr)
                        ; (thumb-bl-place  web-post-tr))
                         ;(hull
                         ; (bottom 10 (key-place  0 2 web-post-bl)  )
                         ; (key-place 0 2 web-post-br)
                         ;   (minithumb-bl-place )
                         ; 
                         ; )
                        ; (hull
                        ; (left-key-place (- lastrow innercol-offset) -1 (translate (wall-locate2 7 0) web-post))
                        ; (left-key-place (- lastrow innercol-offset) -1 (translate (wall-locate3 7 0) web-post))
                        ; ;(minithumb-bl-place (translate (wall-locate2 -2 1) web-post-tr))
                        ; ;(minithumb-bl-place (translate (wall-locate3 -2 1) web-post-tr))
                        ; (minithumb-tl-place web-post-tl))
                         ;(hull
                         ; (left-key-place (- cornerrow innercol-offset) -1 web-post)
                         ; (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
                         ; (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
                         ; (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
                         ; (thumb-tl-place web-post-tl))
                         ;(hull
                         ; (left-key-place (- cornerrow innercol-offset) -1 web-post)
                         ; (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
                         ; (key-place 0 (- cornerrow innercol-offset) web-post-bl)
                         ; (thumb-tl-place web-post-tl))
                         ;(hull
                         ; (thumb-bl-place web-post-tr)
                         ; (thumb-bl-place (translate (wall-locate1 -0.3 1) web-post-tr))
                         ; (thumb-bl-place (translate (wall-locate2 -0.3 1) web-post-tr))
                         ; (thumb-bl-place (translate (wall-locate3 -0.3 1) web-post-tr))
                         ; (thumb-tl-place web-post-tl))
                         ; Tiny little piece leading to the left
                         ;(wall-brace thumb-bl-place  0  1 web-post-tr thumb-bl-place  0  1 web-post-tl)
                         ))

(def trackball-to-case (difference (union
                        ; Trackball mount to left outside of case
                                    (color [1 0 0 1] (hull
                                     (left-key-place-narrow cornerrow -1 (translate (wall-locate3 -1 1) big-boi-web-post))
                                     case-filler-cup))
                        ; Gap between trackball mount and top key
                                   ; (hull
                                   ;  (key-place 0 cornerrow web-post-bl)
                                   ;  (key-place 0 cornerrow web-post-br)
                                   ;  (key-place 0 cornerrow web-post-tl)
                         ;(left-key-place-narrow  cornerrow -1 (translate (wall-locate3 -1 0) big-boi-web-post))
                                     ;(color [0.5 0 1 1](key-place 0 (- lastrow innercol-offset) big-boi-web-post))
                                     ;(key-place 0 (- lastrow innercol-offset) web-post-bl)
                                     ;(color [1 1 0.5 1](key-place 0 (- lastrow innercol-offset) big-boi-web-post))
                                      ;(wall-brace (partial key-place 0 cornerrow) 0 0 web-post-bl (partial key-place 0 cornerrow) 0 0 web-post-bl) 
                                      ;(key-place 0 (- cornerrow innercol-offset) web-post-r)
                                     ;(left-key-place-narrow (- lastrow innercol-offset) 0 (translate (wall-locate2 -1 0) big-boi-web-post))
                                     ;(left-key-place-narrow (- lastrow innercol-offset) 0 (translate (wall-locate3 -1 0) big-boi-web-post))
                                     ;       )
                        ; Between the trackball and the outside of the case near the bottom, to ensure a nice seal
                                    ( color [0 1 0 1](hull
                                     (bottom 25 (left-key-place-narrow cornerrow -1 (translate (wall-locate3 -1 2)  big-boi-web-post)))
                                     (translate trackball-origin (trackball-mount-rotate cup))))
                                    )
                                   (translate trackball-origin rotated-dowells)
                                   (translate trackball-origin rotated-bottom-trim)))

; NOTE: Using -1.5 instead of -1 to make these a bit bigger to make room for the hotswaps
(def wall-multiplier (if trackball-enabled 1.5 1))
(def trackball-tweeners (union
                         (wall-brace minithumb-mr-place  0 (- wall-multiplier) web-post-br minithumb-br-place  0 -1 web-post-br)))
(def back-convex-thumb-wall-0 ; thumb tweeners
  (if (or trackball-enabled joystick-enabled)
    trackball-tweeners
    (union
     (wall-brace minithumb-mr-place  0 (- wall-multiplier) web-post-bl minithumb-br-place  0 -1 web-post-br))))
(def back-convex-thumb-wall-1 (wall-brace minithumb-mr-place  0 (- wall-multiplier) web-post-br minithumb-tr-place 0 (- wall-multiplier) minithumb-post-br))
(def back-convex-thumb-wall-2 (if (or trackball-enabled joystick-enabled)
                                ; Back right thumb to the middle one
                                (triangle-hulls
                                 (minithumb-mr-place web-post-br)
                                 (minithumb-mr-place web-post-bl)
                                 (minithumb-br-place web-post-br))
                                (union
                                 (wall-brace minithumb-mr-place  0 (- wall-multiplier) web-post-br minithumb-mr-place  0 (- wall-multiplier) web-post-bl))))
(def thumb-walls  ; thumb walls
  (union
   (wall-brace minithumb-bl-place -1  0 web-post-bl minithumb-br-place -1  0 web-post-tl)
   (wall-brace minithumb-br-place  0 -1 web-post-br minithumb-br-place  0 -1 web-post-bl)
   (wall-brace minithumb-br-place -1  0 web-post-tl minithumb-br-place -1  0 web-post-bl)
   (wall-brace minithumb-bl-place -1  0 web-post-tl minithumb-bl-place -1  0 web-post-bl)))

(def thumb-corners ; thumb corners
  (union
   (wall-brace minithumb-br-place -1  0 web-post-bl minithumb-br-place  0 -1 web-post-bl)
   (if (or trackball-enabled joystick-enabled) nil (wall-brace minithumb-bl-place -1  0 web-post-tl minithumb-bl-place  0  1 web-post-tl))))

(def pro-micro-wall (union
                     (for [x (range 0 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
                     (for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))))
;(def back-pinky-wall (for [x (range 4 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl x       cornerrow 0 -1 web-post-br)))


(def back-wall
  (color [1 0 0 1]( union 
  (for [x (range 0 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
 (for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
  )))

(def left-wall
 (difference 
( union
  
(left-wall-plate-place 0 0 oled-holder)
 ;(left-wall-plate-place oled-x-position oled-y-position oled-holder-block)

(wall-brace-xy (partial key-place 0 0) 0 1 web-post-tl  (partial left-wall-plate-place 1 1) 0 1 oled-post wall-xy-offset wall-xy-offset-thin)
(wall-brace-xy  (partial left-wall-plate-place 1 1) 0 1 oled-post  (partial left-wall-plate-place -1 1) 0 1 oled-post wall-xy-offset-thin wall-xy-offset-thin)
(wall-brace-xy  (partial left-wall-plate-place -1 1) 0 1 oled-post  (partial left-wall-plate-place -1 1) -1 0 oled-post wall-xy-offset-thin wall-xy-offset-thin)
(wall-brace-xy  (partial left-wall-plate-place -1 1) -1 0 oled-post  (partial left-wall-plate-place -1 -1) -1 -1 oled-post wall-xy-offset-thin wall-xy-offset-thin)
;(wall-brace-xy (partial left-wall-plate-place -1 -1) -1 -1 oled-post  (partial key-place 0 (- lastrow innercol-offset)) 0 0 web-post-tl wall-xy-offset-thin wall-xy-offset-thin)
(wall-brace-xy (partial left-wall-plate-place -1 -1) -1 -1 oled-post  (partial left-key-place 2 -1) -1 0 oled-post wall-xy-offset-thin wall-xy-offset-thin)
(color [1 0 0 1](wall-brace-xy (partial left-key-place 2 -1) -1 0 oled-post  (partial key-place 0 (- lastrow innercol-offset)) 0 1  web-post-bl wall-xy-offset-thin wall-xy-offset-thin)) 
 (if (or trackball-enabled joystick-enabled) nil (color [0 1 0 1](wall-brace-xy (partial key-place 0 (- lastrow innercol-offset)) -2 1   web-post-bl (partial key-place 0 (- lastrow innercol-offset)) 0 0 web-post-br wall-xy-offset-thin wall-xy-offset-thin)))
     ;(if trackball-enabled nil (wall-brace-xy (partial key-place 0 (- lastrow innercol-offset)) 0 0 web-post-br (partial key-place 0 (- lastrow innercol-offset) ) 0 0 minithumb-post-bl wall-xy-offset-thin wall-xy-offset-thin) )
 ;(if trackball-enabled nil (color [1 0 0 1](wall-brace-xy (partial key-place 0 (- lastrow innercol-offset)) 0 0 web-post-bl (partial key-place 0 cornerrow ) 0 0 minithumb-post-tl wall-xy-offset-thin wall-xy-offset-thin)))
 ;(wall-brace (partial key-place 0 cornerrow) 0 0 minithumb-post-tl minithumb-bl-place  0  1 web-post-tl)
 (if (or trackball-enabled joystick-enabled) nil(wall-brace (partial key-place 1 cornerrow) -1 0 web-post-bl (partial key-place 1 lastrow )1 1 web-post-tl))
 ;(wall-brace-xy  minithumb-tl-place -1 0 web-post-tl minithumb-tl-place -1 0 web-post-bl wall-xy-offset wall-xy-offset)
;  (for [y (range 1.7 (+ (- lastrow innercol-offset ) 0/6))] (union (wall-brace (partial left-key-place y 1) -1 0 web-post (partial left-key-place y -1) -1 0 web-post)
;                                                        (hull (key-place 0 y web-post-tl)
;                                                              (key-place 0 y web-post-bl)
;                                                              (left-key-place y  1 web-post)
;                                                              (left-key-place y -1 web-post))))
;(for [y (range 2.7 (+ (- lastrow innercol-offset) 0.6))] (union
;                                                (wall-brace (partial left-key-place (dec y) -1) -1 0 web-post (partial left-key-place y  1) -1 0 web-post)
;                                                (hull (key-place 0 y       web-post-tl)
;                                                      (key-place 0 (dec y) web-post-bl)
;                                                      (left-key-place y        1 web-post)
;                                                      (left-key-place (dec y) -1 web-post))))
;(wall-brace (partial key-place 0 0) 0 1 web-post-tl (partial left-key-place 0 1) -0 1 web-post)
;(wall-brace (partial left-key-place 0 1) -0 1 web-post thumb-tl-place -1 0 web-post)

  )
 
  ;(left-wall-plate-place oled-x-position oled-y-position (translate [0 0 -6] (cube (first oled-pcb-size) (second oled-pcb-size) 4 :center true) ))
  )
  )

  (def joystick (import "../things/joystick hole.stl"))
  (def joycon-joystick-case-mount (import "../things/Billiam_joy_case_mount.stl"))
  (def joystick-cutout (color [0 0 1] (translate [0 0 6] (cylinder (/ 33.6 2) 3.5))))
  (defn joystick-position [shape]
   (->> shape 
        (rotate (deg2rad 60) [0 1 0] )
       (rotate (deg2rad -35)[0 0 1])
       (translate [64 -35 36] ))
  )

  (def joystick-wall 
      (difference
       (union
      ;(wall-brace-xy (partial key-place 0 (- lastrow innercol-offset)) 0 1  web-post-bl (partial key-place 1 lastrow )1 1 web-post-tr wall-xy-offset-thin wall-xy-offset-thin)
      ; (wall-brace minithumb-bl-place  0  1 web-post-tl (partial key-place 0 cornerrow) 0 1 web-post-bl )
      ; (hull 
      ;  (key-place 1 cornerrow web-post-bl)
      ;  (minithumb-bl-place web-post-tl)
      ;  (minithumb-tl-place web-post)
      ; 
      ;  )
       (color [0 1 0 1] (triangle-hulls
                         (minithumb-tl-place web-post-tl)
                         (minithumb-bl-place minithumb-post-tl)
                         (minithumb-bl-place minithumb-post-tr)))

       (color [0 0 1 1] (triangle-hulls
                         (minithumb-mr-place minithumb-post-tl)
                         (minithumb-bl-place minithumb-post-tr)
                         (minithumb-tr-place minithumb-post-tl)))

       (color [0 1 1 1](triangle-hulls
        (minithumb-tr-place minithumb-post-tl)
        (minithumb-bl-place minithumb-post-tr)
        (key-place 1 cornerrow web-post-bl )
        ))
       
      (triangle-hulls
         (color [1 1 0 1](key-place 1 cornerrow web-post-bl)
        (minithumb-bl-place minithumb-post-tr)
        (minithumb-tl-place web-post-tl))
       
       (color [0.3 0.4 0 1](minithumb-tl-place web-post-bl)

              (minithumb-bl-place web-post-tl)
              (minithumb-bl-place minithumb-post-tr))
       
     ;(color [1 0 1 1](minithumb-tr-place web-post-tl)
     ;       (minithumb-tl-place web-post-tl)
     ;       (minithumb-bl-place minithumb-post-tr))
       )

       ;(hull
       ; (minithumb-tl-place  web-post-br)
       ;  (key-place 0 cornerrow web-post-br)
       ; (minithumb-bl-place minithumb-post-tl)
       ; (minithumb-tl-place minithumb-post-tl)
       ; (minithumb-bl-place web-post-tl)
       ; (key-place 0 cornerrow web-post-bl)
       ; )
      ;
       
       (hull
        (minithumb-bl-place minithumb-post-tl)
        (minithumb-bl-place web-post-tl)
        (minithumb-tr-place minithumb-post-tl)
        (minithumb-tl-place web-post-tl)
        (wall-brace-xy minithumb-bl-place 0  0 minithumb-post-tl minithumb-tl-place 0 0 web-post-tr  wall-xy-offset-thin wall-xy-offset-thin)        
        )
       (wall-brace-xy minithumb-tl-place  -2 0 web-post-tr (partial key-place 0 cornerrow) -2 0 web-post-br wall-xy-offset-thin wall-xy-offset-thin)
       (wall-brace-xy  (partial key-place 0 cornerrow) 0 0 web-post-br (partial key-place 0 cornerrow) 0 0 web-post-bl wall-xy-offset-thin wall-xy-offset-thin)
      

        ;(wall-brace minithumb-tl-place 0 0 web-post-br (partial key-place 0 cornerrow) 0 0 web-post-br )
        

      ;(triangle-hulls
      ; (minithumb-mr-place web-post-tl)
      ; (minithumb-tl-place web-post-bl)
      ; (minithumb-tl-place web-post-tl)
      ; )
       )
       joystick-cutout)
    
    )

  (def left-section
    (union
     ;(oled-place oled-holder)
     (triangle-hulls
     (color [1 0 0 1] (left-wall-plate-place 1 1 oled-post)
      (key-place 0 0 web-post-tl)
      (key-place 0 0 web-post-bl))

      (key-place 0 0 web-post-bl)
      (left-wall-plate-place 1 1 oled-post)
      (left-wall-plate-place 1 -1 oled-post)

     (left-wall-plate-place 1 -1 oled-post)
     (key-place 0 1 web-post-tl)
     (key-place 0 2 web-post-tl)

     (key-place  0 2  web-post-bl)
     (key-place 0 2 web-post-tl)
     (left-wall-plate-place -1 -1 oled-post)

     (left-wall-plate-place -1 -1 oled-post)
     (left-wall-plate-place 1 -1 oled-post)
     (key-place 0 (- cornerrow innercol-offset) web-post-bl)

      (key-place 0 (- cornerrow innercol-offset) web-post-bl)
      (left-wall-plate-place -1 -1 oled-post)
      (left-key-place 2 -1  oled-post)
      
      (key-place 0 (- cornerrow innercol-offset) web-post-bl)
      (key-place 0 (- lastrow innercol-offset) web-post-bl)
      (left-key-place 2 -1 oled-post)


     ;(left-wall-plate-place 1 -1 oled-post)
     ;(left-wall-plate-place -1 -1 oled-post)
     ;(minithumb-bl-place web-post-tl)
;
     ; (minithumb-bl-place web-post-tl)
     ; (left-wall-plate-place -1 -1 oled-post)
     ; (left-wall-plate-place 1 -1 oled-post)
;
    
;
     ; (key-place 0 1 web-post-bl)
     ; (left-wall-plate-place 1 -1 oled-post)
     ; (minithumb-tl-place web-post-tl)
;
     ; (key-place 0 1 web-post-bl)
     ; (key-place 0 2 web-post-tl)
     ; (minithumb-bl-place web-post-bl)
;
     ; (minithumb-tl-place web-post-tl)
     ; (minithumb-tl-place web-post-tr)
     ; (key-place 0 2 web-post-tl)
;
     ; (key-place 0 2 web-post-tl)
     ; (key-place 0 2 web-post-bl)
     ; (minithumb-tl-place web-post-tr)
;
     ; (minithumb-tl-place web-post-tr)
     ; (key-place 0 2 web-post-bl)
     ; (minithumb-tl-place web-post-tl)
      )))

  (def front-wall
   ( color [0.4 0.2 0.7 1] ( union
  (key-wall-brace (+ innercol-offset 3) lastrow  0 -1 web-post-bl (+ innercol-offset 3) lastrow   0 -1 web-post-br)
   (key-wall-brace (+ innercol-offset 3) lastrow  0 -1 web-post-br (+ innercol-offset 4) extra-cornerrow 0 -1 web-post-bl)
   (for [x (range (+ innercol-offset 4) ncols)] (key-wall-brace x extra-cornerrow 0 -1 web-post-bl x       extra-cornerrow 0 -1 web-post-br))
   (for [x (range (+ innercol-offset 5) ncols)] (key-wall-brace x extra-cornerrow 0 -1 web-post-bl (dec x) extra-cornerrow 0 -1 web-post-br))
       )))
  
  (def non-thumb-walls (union
                        left-wall
                       ; back-wall
                            ; front wall
                        front-wall
;                            (for [x (range 5 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl (dec x) cornerrow 0 -1 web-post-br))
                            ; Right before the start of the thumb
                        ;(color [ 0 0 1 1] (wall-brace minithumb-tr-place  0 -1 minithumb-post-br (partial key-place 3 (- lastrow innercol-offset))  0 -1 web-post-bl))
                        ))
  
   

(def case-walls
  (union
   thumb-wall-type
   right-wall
   back-wall
   ;left-wall
   ;front-wall
   left-section
   non-thumb-walls
   ;pro-micro-wall

   back-convex-thumb-wall-0
   back-convex-thumb-wall-1
   back-convex-thumb-wall-2
   thumb-corners
   (if (or trackball-enabled joystick-enabled) nil thumb-to-left-wall)
   back-convex-thumb-wall-0
   ))



; Offsets for the controller/trrs holder cutout
(def holder-offset
  (case nrows
    4 -3.5
    5 0
    6 (if inner-column
        3.2
        2.2)))

(def notch-offset
  (case nrows
    4 3.35
    5 0.15
    6 -5.07))

; Cutout for controller/trrs jack holder
(def usb-holder-ref (key-position 0 0 (map - (wall-locate2  0  -1) [0 (/ mount-height 2) 0])))
;(def usb-holder-position (map + [(+ 18.8 holder-offset) 18.7 1.3] [(first usb-holder-ref) (second usb-holder-ref) 2]))
;(def usb-holder-space  (rotate (deg2rad 90) [1 0 0](translate (map + usb-holder-position [-1.5 (* -1 wall-thickness) 2.9]) (cube 28.666 30 12.4))))
;(def usb-holder-notch  (translate (map + usb-holder-position [-1.5 (+ 4.4 notch-offset) 2.9]) (cube 31.366 1.3 12.4)))
;(def trrs-notch        (translate (map + usb-holder-position [-10.33 (+ 3.6 notch-offset) 6.6]) (cube 8.4 2.4 19.8)))

; code adapted from https://gist.github.com/jamiehs/de163e7d469e4fb4220e504b58613806
(def aviator-start (map + [-32 -12  20] (key-position 0 0 (map + (wall-locate3 0 1) [0 (/ mount-height  2) 0]))))
(def aviator-position [(first aviator-start) (second aviator-start) 12])
(def aviator-diameter 16.2)
(def aviator-hole (translate aviator-position
                             (rotate (deg2rad 90) [1 0 0]
                                     ;(rotate (deg2rad 45) [0 1 0]
                                             (translate [3 0 0]
                                                        (cylinder (/  aviator-diameter 2) 20)))))

(def resetswitch-start (map + [0 -3  20] (key-position 0 0 (map + (wall-locate3 0 1) [0 (/ mount-height  2) 0]))))
(def resetswitch-position [(first resetswitch-start) (second resetswitch-start) 12])
(def resetswitch-diameter 8.5)

(def reset-hole
  (union
  ; (->> (union (binding [*fn* 36] (cylinder [(/ resetswitch-diameter 2) (/ resetswitch-diameter 2)] 10)))
  ;      (rotate (/ π 2) [1 0 0])
  ;      (translate [(+ 4  (first aviator-start)) (- (second aviator-start) 1) (/ (+ 44 aviator-diameter 0) 2)]))
  ;    ; thinner wall
   (->> (union (binding [*fn* 36] (cylinder [(/ resetswitch-diameter 2) (/ resetswitch-diameter 2)] 20))) ;; depth here matters; has been eyeballed
        (rotate (/ π 2) [1 0 0])
        (translate [(+ 32 (first resetswitch-start)) (- (second resetswitch-start) 2) (/ (+ 8 aviator-diameter 0) 2)]))))

(def resetswitch-hole (translate aviator-position
                             (rotate (deg2rad 90) [1 0 0]
                                     ;(rotate (deg2rad 45) [0 1 0]
                                     (translate [4 0 0]
                                                (cylinder (/  resetswitch-diameter 2) 20)))))

(def usb-holder-position (key-position 1 1 (map + (wall-locate1 -2 (- 4.9 (* 0.2 nrows))) [0 (/ mount-height 2) 0])))

;(def usb-holder-size [19 33.65 5.5]);[5.5 33.65 19])	;;5.5 33.34 18.4
;(def usb-hole-size [19 33.65 9.5]);[9.5 33.65 19]) ;;9.5 33.34 18.4
;(def usb-hole-size-left [8.0 35.6 9.5]);[9.5 35.6 8.0]) ;;9.5 35.6 8.0
;(def usb-hole-size-right [10.0 35.6 6]);[6 35.6 10.0]) ;;6 35.6 10.0

(def usb-holder-size [5.5 33.65 19])	;;5.5 33.34 18.4
(def usb-hole-size [9.5 33.65 19]) ;;9.5 33.34 18.4
(def usb-hole-size-left [9.5 35.6 8.0]) ;;9.5 35.6 8.0
(def usb-hole-size-right [6 35.6 10.0]) ;;6 35.6 10.0
(def usb-holder-thickness 5)
(def usb-holder
  (->> 
   (difference

        (cube (+ (first usb-holder-size) usb-holder-thickness) (+ (second usb-holder-size) usb-holder-thickness) (+ (last usb-holder-size) usb-holder-thickness))

	; (cube 5 5 5)
) ; (apply cube usb-hole-size))


       (translate [(first usb-holder-position) (second usb-holder-position) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])
       
		; (rotate -0.6 [0 0 1])
		;( translate [(- (first usb-holder-position) 10) (+ (second usb-holder-position) 4) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])
       ))



(def usb-holder-hole
  (->>
   (union
    (->> (apply cube usb-hole-size)
         (translate [(+ (first usb-holder-position) 2) (second usb-holder-position) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)]))
    (->> (apply cube usb-hole-size-left)
         (translate [(+ (first usb-holder-position) 2) (- (second usb-holder-position) 10) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)]))
    (->> (apply cube usb-hole-size-right)
         (translate [(+ (first usb-holder-position) 2) (+ (second usb-holder-position) 10) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])))))

(def encoder-pos (add-vec (left-wall-plate-position 0 -1.75) [0 -13 0]))
(def encoder-rot-x oled-mount-rotation-x-old)
(def encoder-rot-z oled-mount-rotation-z-old)
(def encoder-cutout-shape (cylinder (/ 6.5 2) 1000))
(def encoder-cutout (->> encoder-cutout-shape
                         (rx encoder-rot-x)
                         (rz encoder-rot-z)
                         (translate encoder-pos)))

; Screw insert definition & position
(defn screw-insert-shape [bottom-radius top-radius height]
  (union
   (->> (binding [*fn* 30]
          (cylinder [bottom-radius top-radius] height)))))

(defn screw-insert [column row bottom-radius top-radius height offset]
  (let [shift-right   (= column lastcol)
        shift-left    (= column 0)
        shift-up      (and (not (or shift-right shift-left)) (= row 0))
        shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
        position      (if shift-up     (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
                          (if shift-down  (key-position column row (map - (wall-locate2  0 -2.5) [0 (/ mount-height 2) 0]))
                              (if shift-left (map + (left-key-position row 0) (wall-locate3 -1 0))
                                  (key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0])))))]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate (map + offset [(first position) (second position) (/ height 2)])))))

; Offsets for the screw inserts dependent on extra-row & pinky-15u
(when (and pinky-15u extra-row)
  (def screw-offset-tr [1 7 0])
  (def screw-offset-br [7 14 0]))
(when (and pinky-15u (false? extra-row))
  (def screw-offset-tr [1 7 0])
  (def screw-offset-br [6.5 15.5 0]))
(when (and (false? pinky-15u) extra-row)
  (def screw-offset-tr [-3.5 6.5 0])
  (def screw-offset-br [-3.5 -6.5 0]))
(when (and (false? pinky-15u) (false? extra-row))
  (def screw-offset-tr [-4 6.5 0])
  (def screw-offset-br [-6 13 0]))

; Offsets for the screw inserts dependent on thumb-style & inner-column
(when (and (= thumb-style "cf") inner-column)
  (def screw-offset-bl [9 4 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [13 -7 0]))
(when (and (= thumb-style "cf") (false? inner-column))
  (def screw-offset-bl [-7.7 2 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [13 -7 0]))
(when (and (= thumb-style "mini") inner-column)
  (def screw-offset-bl [14 8 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [-1 -7 0]))
(when (and (= thumb-style "mini") (false? inner-column))
  (def screw-offset-bl [-1 4.2 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [-1 -7 0]))
(when (and (= thumb-style "default") inner-column)
  (def screw-offset-bl [5 -6 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [8 -1 0]))
(when (and (= thumb-style "default") (false? inner-column))
  (def screw-offset-bl [-11.7 -8 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [8 -1 0]))

 (defn screw-insert-all-shapes [bottom-radius top-radius height]
   (union (screw-insert 0 1         bottom-radius top-radius height [8 21.5 0])
          (screw-insert 0 cornerrow   bottom-radius top-radius height (map + [-8 8 0] screw-offset-bl))
          (screw-insert lastcol lastrow  bottom-radius top-radius height screw-offset-br)
          (screw-insert lastcol 0         bottom-radius top-radius height screw-offset-tr)
          (screw-insert (+ 2 innercol-offset) 0         bottom-radius top-radius height  screw-offset-tm)
          (screw-insert (+ 1 innercol-offset) lastrow         bottom-radius top-radius height (map + [-6 0 0] screw-offset-bm))))

; Hole Depth Y: 4.4
(def screw-insert-height 6)

; Hole Diameter C: 4.1-4.4
(def screw-insert-bottom-radius (/ 4.0 2))
(def screw-insert-top-radius (/ 3.9 2))
(def screw-insert-holes  (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))

; Wall Thickness W:\t1.65
(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1)))
(def screw-insert-screw-holes  (screw-insert-all-shapes 1.7 1.7 350))
 
 (defn screw-insert-top-fn [bottom-radius top-radius height]
   (union
    (screw-insert 0 0 bottom-radius top-radius height [3 -5 93])
    (screw-insert 0 lastrow bottom-radius top-radius height [-3 19 90])
    (screw-insert lastcol lastrow bottom-radius top-radius height [-27 16 15])
    (screw-insert lastcol 0         bottom-radius top-radius height [-13 4 30])
    (screw-insert (+ 1 innercol-offset) lastrow bottom-radius top-radius height [10 6 52]) ;thumb
    ))

 (def screw-insert-top-obj (screw-insert-top-fn (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1.5)))
(def screw-insert-top-holes (screw-insert-top-fn 1.7 1.7 350))
(def screw-insert-top
  (difference screw-insert-top-obj
              screw-insert-top-holes))

(defn OLED [posx posy posz]
  (translate [posx posy posz]
             (rotate (/ π -6)  [0 1 0]
                     (rotate (/ π 2) [0 0 1]
                             (union
                              (cube 39.5 13.5 3)
                              (translate [1 0 2] (cube 26 12 4)) ;5 0 2
                              (translate [18 0 -4] (cube 3.5 10 8)) ;36 0 6
                              )))))

; Connectors between outer column and right wall when 1.5u keys are used
(def pinky-connectors
  (if pinky-15u
    (apply union
           (concat
            ;; Row connections
            (for [row (range first-15u-row (inc last-15u-row))]
              (triangle-hulls
               (key-place lastcol row web-post-tr)
               (key-place lastcol row wide-post-tr)
               (key-place lastcol row web-post-br)
               (key-place lastcol row wide-post-br)))
            (if-not (= last-15u-row extra-cornerrow) (for [row (range last-15u-row (inc last-15u-row))]
                                                       (triangle-hulls
                                                        (key-place lastcol (inc row) web-post-tr)
                                                        (key-place lastcol row wide-post-br)
                                                        (key-place lastcol (inc row) web-post-br))))
            (if-not (= first-15u-row 0) (for [row (range (dec first-15u-row) first-15u-row)]
                                          (triangle-hulls
                                           (key-place lastcol row web-post-tr)
                                           (key-place lastcol (inc row) wide-post-tr)
                                           (key-place lastcol row web-post-br))))

            ;; Column connections
            (for [row (range first-15u-row last-15u-row)]
              (triangle-hulls
               (key-place lastcol row web-post-br)
               (key-place lastcol row wide-post-br)
               (key-place lastcol (inc row) web-post-tr)
               (key-place lastcol (inc row) wide-post-tr)))
            (if-not (= last-15u-row extra-cornerrow) (for [row (range last-15u-row (inc last-15u-row))]
                                                       (triangle-hulls
                                                        (key-place lastcol row web-post-br)
                                                        (key-place lastcol row wide-post-br)
                                                        (key-place lastcol (inc row) web-post-tr))))
            (if-not (= first-15u-row 0) (for [row (range (dec first-15u-row) first-15u-row)]
                                          (triangle-hulls
                                           (key-place lastcol row web-post-br)
                                           (key-place lastcol (inc row) wide-post-tr)
                                           (key-place lastcol (inc row) web-post-tr))))))))


;(def pinky-walls
;  (union
 ;  (key-wall-brace lastcol cornerrow 0 -1 web-post-br lastcol cornerrow 0 -1 wide-post-br)
 ;  (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 0 1 wide-post-tr)))

(def right-wall-plate
  (let [tr (if (true? pinky-15u) wide-post-tr web-post-tr)
        br (if (true? pinky-15u) wide-post-br web-post-br)
        hull-with (translate (key-position 0 0 [0 0 0]) (square 1 1))]
    (union (hull (cut (key-wall-brace lastcol 0 0 1 tr lastcol 0 1 0 tr)) hull-with)
           (for [y (range 0 lastrow)] (hull (cut (key-wall-brace lastcol y 1 0 tr lastcol y 1 0 br)) hull-with))
           (for [y (range 1 lastrow)] (hull (cut (key-wall-brace lastcol (dec y) 1 0 br lastcol y 1 0 tr)) hull-with))
           (hull (cut (key-wall-brace lastcol cornerrow 0 -1 br lastcol cornerrow 1 0 br)) hull-with))))

(def bottom-plate-thickness 2)

(def plate-attempt (difference
                     (extrude-linear {:height bottom-plate-thickness}
                 (union
                                     ; pro micro wall
                                     (for [x (range 0 (- ncols 1))] (hull  (cut (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr)) (translate (key-position x (- lastrow innercol-offset) [0 0 0]) (square (+ keyswitch-width 15) keyswitch-height))))
                                     (for [x (range 1 ncols)] (hull (cut (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr)) (translate (key-position x 2 [0 0 0]) (square 1 1))))
                                     (hull (cut right-wall) (translate (key-position lastcol 0 [0 0 0]) (square keyswitch-width keyswitch-height)))
                                     (hull (cut mini-thumb-wall) (translate bl-minithumb-loc (square 1 1)))
                                     right-wall-plate
                                     (hull (cut back-convex-thumb-wall-0) (translate bl-minithumb-loc (square 1 1)))
                                     (hull (cut back-convex-thumb-wall-1) (translate bl-minithumb-loc (square 1 1)))
                                     (hull (cut back-convex-thumb-wall-2) (translate bl-minithumb-loc (square 1 1)))
                                     (hull (cut thumb-corners))
                                     (if (or trackball-enabled joystick-enabled) nil (hull (cut thumb-to-left-wall) (translate (key-position (- lastcol 1) (- lastrow 1) [0 0 0]) (square 1 1))))
                                     (hull (cut non-thumb-walls)) 
                  ))
                     (translate [0 0 -10] screw-insert-screw-holes)
                    ))

(def hand-on-test
  (translate [-5 -60 92]
             (rotate (deg2rad -27) [1 0 0]
                     (rotate (deg2rad 12) [0 0 1]
                             (rotate (+ tenting-angle (deg2rad 5)) [0 1 0]
                                     (rotate
                                      (deg2rad -90) [1 0 0]
                                      (mirror [0 1 0] hand)))))))

(def tent-ball-rad 7)
(def tent-stand-rad 5)
(def crank-rad 1.5)
(def crank-len 20)
(def tent-stand-thread-height 25)
(def tent-stand-thread-lead 1.25)
(def tent-thread ( call-module "thread" tent-stand-rad tent-stand-thread-height tent-stand-thread-lead))
(def tent-stand (union
                 tent-thread
                 (translate [0 0 (- tent-stand-rad)] (sphere tent-ball-rad))))


(def tent-foot-width 25)
(def tent-foot-height 30)
(def tent-foot-thickness 2)
(def tent-ball-holder-thickness 4)
(def hook-angle 40)

; Some convoluted logic to create a little hook to hold the ball in
(defn ball-hook [with-hook?]
  (let
   [hook-height (if with-hook? tent-ball-rad (/ tent-ball-rad 1.5))]
    (rotate (deg2rad 90) [1 0 0]
            (union
             (translate [0 (/ hook-height 2) 0]
                        (rotate (deg2rad 90) [1 0 0] (cube tent-ball-holder-thickness tent-ball-holder-thickness hook-height)))
             (if with-hook? (translate [(- (+ tent-ball-rad (/ tent-ball-holder-thickness 2))) tent-ball-rad 0]
                                       (extrude-rotate {:angle hook-angle :convexity 10} (translate [(+ tent-ball-rad (/ tent-ball-holder-thickness 2)) 0]
                                                                                                    (square tent-ball-holder-thickness tent-ball-holder-thickness)))) nil)))))

(defn rotated-ball-hook [angle with-hook?]
  (rotate (deg2rad angle) [0 0 1] (translate [(+ tent-ball-rad (/ tent-ball-holder-thickness 2)) 0 (/ tent-foot-thickness 2)] (ball-hook with-hook?))))

(def tent-foot (union
                (cube tent-foot-width tent-foot-height tent-foot-thickness)
                (rotated-ball-hook 0 true)
                (rotated-ball-hook 90 true)
                (rotated-ball-hook 180 true)
                (rotated-ball-hook 270 false)))

(def thumb-tent-origin (map + [-52 -74 -1] (if trackball-enabled [3 -12 0] [0 0 0])))
(def index-tent-origin [-104 27 -1])

(def tent-nut-height 6)
(def tent-thread
  (translate [0 0 tent-nut-height] (rotate (deg2rad 180) [0 1 0]
                                           (call-module "thread" (+ tent-stand-rad 0.5) (+ tent-nut-height bottom-plate-thickness) tent-stand-thread-lead))))
(def tent-nut (difference
               (translate [0 0 (/ tent-nut-height 2)] (cylinder (+ tent-stand-rad 1.5) tent-nut-height))
               tent-thread))

(def key-trackball-clearance 
                                              (translate [0 0 (- plate-thickness)](hull
                              (key-place 1 cornerrow web-post-tl)
                              (key-place 1 cornerrow web-post-tr)
                              (key-place 1 cornerrow web-post-bl)
                              (key-place 1 cornerrow web-post-br)
                              )))

(def trackball-subtract (union
                         ; Subtract out the actual trackball
                         (translate trackball-origin (dowell-angle raised-trackball))
                         ; Subtract out space for the cup, because sometimes things from the keyboard creep in
                         (translate trackball-origin (sphere (/ trackball-width-plus-bearing 2)))
                         ; Just... double check that we have the full dowell negative
                         (translate trackball-origin rotated-dowells)
                         ;key-trackball-clearance
                         hotswap-clearance))

(def trackball-mount-translated-to-model (difference
                                          (union
                                           (translate trackball-origin trackball-mount)
                                           trackball-walls
                                           trackball-to-case)
                                          trackball-subtract
                                          key-clearance
                                          thumb-key-clearance
                                          (translate trackball-origin trackball-insertion-cyl)
                                          ))



(def model-right (difference
                   ;oled-clip-mount-frame-hole
                  (union
                   key-holes
                   key-holes-inner
                   pinky-connectors
                   extra-connectors
                   connectors
                   inner-connectors
                   thumb-type
                   thumb-connector-type
                   ;(color [1 0 0 1] key-trackball-clearance)
                   (if joystick-enabled joystick-wall nil)
                   
                   ;(color [1 0 0 1]oled-clip-mount-frame-shape)
                   ;(OLED -65 -6 99)
                   (difference (union case-walls
                                      usb-holder
                                      screw-insert-outers
                                      )
                               (if trackball-enabled trackball-walls nil)
                               ; Leave room to insert the ball
                (if trackball-enabled (translate trackball-origin trackball-insertion-cyl) nil)
                               
                               ;(translate palm-hole-origin (palm-rest-hole-rotate palm-buckle-holes))
                               ;usb-holder-space
                              ; trrs-notch
                               ;usb-holder-notch
                               screw-insert-holes
                               
                               ))
                  ;(println str "this is " (first usb-holder-position))
                  (if trackball-enabled (translate trackball-origin (dowell-angle raised-trackball)) nil)
                  
                    usb-holder-hole
                    usb-holder
                    aviator-hole
                    reset-hole
                  encoder-cutout
                  (left-wall-plate-place 0 0 oled-holder-cut)
                  

                  
                  
                   ;( translate [0 0 (- (- oled-holder-thickness) 0.1)]
                   ; (left-wall-plate-place oled-x-position oled-y-position oled-holder-cut))
                  (translate [0 0 -20] (cube 350 350 40))))

(spit "things/right.scad"
       ;(include "../nutsnbolts/cyl_head_bolt.scad")
      (write-scad model-right))

;(spit "things/left.scad"
;      (write-scad (union (mirror [-1 0 0] model-right)
;                         ;(joystick-position joystick)
;                         ;(joystick-position joystick-cutout)
;                         (translate [ 0 35 0 ] (rotate (deg2rad -160) [0 0 1] joycon-joystick-case-mount))
;                        )))
;
;(spit "things/right-test.scad"
;      (write-scad (union model-right
;                         thumbcaps-type
;                         caps)))

(def cf-right-plate 
  (extrude-linear
          {:height 2.6 :center false}
          (project
            (difference
              (union
                key-holes
                key-holes-inner
                pinky-connectors
                extra-connectors
                connectors
                inner-connectors
                thumb-type
                thumb-connector-type
                case-walls
                thumbcaps-fill-type
                caps-fill
                screw-insert-outers)
              (translate [0 0 -10] screw-insert-screw-holes)))))

(def right-plate 
                  (difference
                   
                   ;(translate [0.75 -0.75 -2] usb-holder)
                   ;(translate [4.5 -0.75 1.5] usb-holder)
                  (union
                   (if trackball-enabled trackball-mount-translated-to-model nil)
                    (translate thumb-tent-origin tent-nut)
                    (translate index-tent-origin tent-nut)
                   cf-right-plate
                   ;(translate [0 0 (/ bottom-plate-thickness -2)] plate-attempt)
                   ;key-holes
                   ;key-holes-inner
                   ;pinky-connectors
                   ;extra-connectors
                   ;connectors
                   ;inner-connectors
                   ;thumb-type
                   ;thumb-connector-type
                   ;case-walls
                   ;thumbcaps-fill-type
                   ;caps-fill
                   ;screw-insert-outers
                    ;(translate [0 0 -10] screw-insert-screw-holes)
                   
                  
                   )
                 
                  (translate thumb-tent-origin tent-thread)
                  (translate index-tent-origin tent-thread)
                  (translate [0 0 -20] (cube 350 350 40))
                 ; mini-thumb-wall
                  (union 
                   (translate [-0.75 -0.75 -2] usb-holder)
                   (translate [0.75 -0.75 -2] usb-holder)
                   (translate [4.5 -0.75 2] usb-holder))
                  ; model-right
                  ))

(spit "things/right-plate.scad"
      (write-scad 
       (include "../nutsnbolts/cyl_head_bolt.scad")
       right-plate))

;(spit "things/right-plate.scad"
;      (write-scad
;       (extrude-linear
;        {:height 2.6 :center false}
;        (project
;         (difference
;          (union
;           (if trackball-enabled trackball-mount-translated-to-model nil)
;           key-holes
;           key-holes-inner
;           pinky-connectors
;           extra-connectors
;           connectors
;           inner-connectors
;           thumb-type
;           thumb-connector-type
;           case-walls
;           thumbcaps-fill-type
;           caps-fill
;           screw-insert-outers
;            model-right)
;          (translate [0 0 -10] screw-insert-screw-holes))))))

;(spit "things/right-plate-laser.scad"
;      (write-scad
;       (cut
;        (translate [0 0 -0.1]
;                   (difference (union case-walls
;                                      screw-insert-outers)
;                               (translate [0 0 -10] screw-insert-screw-holes))))))

;(spit "things/test.scad"
 ;     (write-scad
 ;      (difference trrs-holder trrs-holder-hole)))

(defn -main [dum] 1)  ; dummy to make it easier to batch
