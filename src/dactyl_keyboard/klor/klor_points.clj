(ns dactyl-keyboard.klor.klor-points
(:refer-clojure :exclude [use import])
  (:require
 [dactyl-keyboard.klor.klor-config :refer :all]
 [dactyl-keyboard.oled :refer [oled-holder-height oled-holder-width]]
 [dactyl-keyboard.tps-43 :refer [tps-43-mount-width tps-43-mount-length]]
 ))

(def tolerance 0.5)
(def extra-pinky-top-left-corner [213.627717 86.501225])
(def extra-pinky-top-right-corner [231.186547 90.273904] )
(def extra-pinky-bottom-right-corner [224.452135 128.3533])
(def extra-pinky-bottom-left-corner [206.646493 125.88519])
(def pinky-top-inner-corner [210.595467 71.622056])
(def pinky-top-right-corner   [215.038063 72.715076])
(def pinky-bottom-left-corner  [184.292479 126.343555])
(def pinky-bottom-right-corner  [204.178385 130.504079])
(def pcb-top-right-corner [210.595468 50.255285 0])
(def pcb-top-left-corner  (mapv + [80.984498 50.220026] [-0.5 -0.5]))
(def pcb-bottom-left-corner-before-bottom-left-thumb [80.984498 133.430551] )
(def pcb-bottom-inner-left-corner-before-bottom-left-thumb  [89.199773 134.135726])

(def usb-c-cutout-bottom-left-corner [92.196762 56.989695])
 (def usb-c-cutout-bottom-right-corner [102.457044 56.989694])
(def usb-c-cutout-top-left-corner [92.196762 49.514851])
(def usb-c-cutout-top-right-corner [102.457044 49.514851])



(def thumb-right-inner-corner [153.828964 121.795181 0])
(def bottom-left-thumb-key-bottom-right-corner [98.331777 151.941367 0])
(def bottom-left-thumb-key-bottom-left-corner [85.321318 163.541477 0] )
 (def bottom-left-thumb-key-bottom-mid-point [93.148749 156.666031 0])
(def bottom-left-thumb-key-top-left-corner [71.46465 148.697567 0])
 (def bottom-left-thumb-key-top-right-corner [81.019757 139.177719 0])
 
(def thumb-right-inner-corner-wall-point (mapv + thumb-right-inner-corner [2 2 0]))
(def middle-left-thumb-key-bottom-mid-point  [107.816364 145.277472])
(def middle-left-thumb-key-bottom-right-point  [113.351981 142.068933])
 (def middle-left-thumb-key-bottom-left-point [98.649106 150.531019])

(def middle-right-thumb-key-bottom-left-point  [119.310701 139.600822])
 (def middle-right-thumb-key-bottom-mid-point [124.952091 138.084698])
  (def middle-right-thumb-key-bottom-right-point [131.122366 136.427539])

(def right-thumb-key-bottom-left-point  [137.433669 135.581333])
 (def  right-thumb-key-bottom-mid-point [143.321874 135.546073])
(def right-thumb-key-bottom-right-point [153.828964 134.840899])

(def middle-finger-protusion-top-left-corner   (mapv + [148.892747 45.883205] [(- tolerance ) (- tolerance)]))
(def middle-finger-protusion-top-right-corner (mapv + [169.906932 46.235792] [tolerance (- tolerance)]))
(def middle-finger-protusion-bottom-right-corner  (mapv + [169.906933 49.514851] [tolerance 0])) 
(def middle-finger-protusion-bottom-left-corner (mapv + [148.892748 49.514851] [(- tolerance) 0]))


(def fourth-finger-bottom-left-corner  [169.801155 121.795181])
 (def fourth-finger-bottom-right-corner  [184.891877 122.852943])

 
(def left-thumb-key-mid-point [143.321874 135.546073])
(def thumb-bottom-right-corner  [153.123792 135.546073])

(def haptic-cutout-top-left-corner   [193.153965 57.52018])
(def haptic-cutout-top-right-corner   [207.553965 57.520181])
(def haptic-cutout-bottom-left-corner   [193.153965 68.920181])
(def haptic-cutout-bottom-right-corner   [207.553965 68.920181])
 
 (def top-right-mounting-hole [189.343965 53.100181 0])
(def  top-right-inner-mounting-hole [174.503965 52.09018 0])
(def bottom-left-inner-mounting-hole [170.303964 118.080182 0])
(def top-left-mounting-hole [83.673965 52.090182 0])
(def top-middle-mounting-hole [145.753965 52.090181 0])
(def bottom-right-mounting-hole [181.913965 118.840181 0])
(def middl-hole [167.653965 84.420181 0])
(def pinky-mounting-hole-2.2 [193.603965 71.42018])
(def above-thumb-mounting-hole [83.673965 131.52018 0])

(def pcb-points-list
  [pcb-top-left-corner
   pcb-bottom-left-corner-before-bottom-left-thumb
   bottom-left-thumb-key-top-right-corner
   bottom-left-thumb-key-top-left-corner
   bottom-left-thumb-key-bottom-left-corner
   bottom-left-thumb-key-bottom-mid-point
   bottom-left-thumb-key-bottom-right-corner
   middle-left-thumb-key-bottom-left-point
   middle-left-thumb-key-bottom-mid-point
   middle-left-thumb-key-bottom-right-point
   middle-right-thumb-key-bottom-left-point
   middle-right-thumb-key-bottom-mid-point
   middle-right-thumb-key-bottom-right-point
   right-thumb-key-bottom-left-point
   right-thumb-key-bottom-mid-point
   right-thumb-key-bottom-right-point
   thumb-right-inner-corner
   fourth-finger-bottom-left-corner
   fourth-finger-bottom-right-corner
   pinky-bottom-left-corner
   pinky-bottom-right-corner
   extra-pinky-bottom-left-corner
   extra-pinky-bottom-right-corner
   extra-pinky-top-right-corner
   extra-pinky-top-left-corner
   pinky-top-right-corner
   pinky-top-inner-corner
   pcb-top-right-corner
   middle-finger-protusion-bottom-right-corner
   middle-finger-protusion-top-right-corner
   middle-finger-protusion-top-left-corner
   middle-finger-protusion-bottom-left-corner
   usb-c-cutout-top-right-corner
   usb-c-cutout-bottom-right-corner
   usb-c-cutout-bottom-left-corner
   usb-c-cutout-top-left-corner 
   ]
  )


(def keycap-spacing-south-east [(/ keycap-width 2) (/ keycap-width -2) 0])
(def keycap-spacing-south-west [(/ keycap-width -2) (/ keycap-width -2) 0])
(def keycap-spacing-south [0 (/ keycap-width -2) 0])
(def keycap-spacing-north [0 (/ keycap-width 2) 0])
(def keycap-spacing-east [(/ keycap-width 2) 0 0])
(def keycap-spacing-west [(/ keycap-width -2) 0 0])
(def keycap-spacing-north-east [(/ keycap-width 2) (/ keycap-width 2) 0])
(def keycap-spacing-north-west [(/ keycap-width -2) (/ keycap-width 2) 0])

(def key-spacing-south-east [(/ key-spacing-width 2) (/ key-spacing-length -2) 0])
(def key-spacing-south-west [(/ key-spacing-width -2) (/ key-spacing-length -2) 0])
(def key-spacing-south [0 (/ key-spacing-length -2) 0])
(def key-spacing-north [0 (/ key-spacing-length 2) 0])
(def key-spacing-east [(/ key-spacing-width 2) 0 0])
(def key-spacing-west [(/ key-spacing-width -2) 0 0])
(def key-spacing-north-east [(/ key-spacing-width 2) (/ key-spacing-length 2) 0])
(def key-spacing-north-west [(/ key-spacing-width -2) (/ key-spacing-length 2) 0])

(def key-spacing-inner-south-east [(/ inner-spacing-width 2) (/ inner-spacing-length -2) 0])
(def key-spacing-inner-south-west [(/ inner-spacing-width -2) (/ inner-spacing-length -2) 0])
(def key-spacing-inner-south [0 (/ inner-spacing-length -2) 0])
(def key-spacing-inner-north [0 (/ inner-spacing-length 2) 0])
(def key-spacing-inner-east [(/ inner-spacing-width 2) 0 0])
(def key-spacing-inner-west [(/ inner-spacing-width -2) 0 0])
(def key-spacing-inner-north-east [(/ inner-spacing-width 2) (/ inner-spacing-length 2) 0])
(def key-spacing-inner-north-west [(/ inner-spacing-width -2) (/ inner-spacing-length 2) 0])

(defn klor-spacing [corner]
  (case corner
    :tm key-spacing-north
    :rm key-spacing-east
    :bm key-spacing-south
    :lm key-spacing-west
    :tl key-spacing-north-west
    :tr key-spacing-north-east
    :br key-spacing-south-east
    :bl key-spacing-south-west) 
  )

(defn klor-inner-spacing [corner]
  (case corner
    :tm key-spacing-inner-north
    :rm key-spacing-inner-east
    :bm key-spacing-inner-south
    :lm key-spacing-inner-west
    :tl key-spacing-inner-north-west
    :tr key-spacing-inner-north-east
    :br key-spacing-inner-south-east
    :bl key-spacing-inner-south-west))

(defn oled-holder-spacing [corner]
  (case corner
    :tm [0 (/ oled-holder-height 2) 0]
    :rm [(/ oled-holder-width 2) 0 0]
    :bm [0 (/ oled-holder-height 2) 0]
    :lm [(/ oled-holder-width -2) 0 0]
    :tl [(/ oled-holder-width -2) (/ oled-holder-height 2) 0]
    :tr [(/ oled-holder-width 2) (/ oled-holder-height 2) 0]
    :br [(/ oled-holder-width 2) (/ oled-holder-height 2) 0]
    :bl [(/ oled-holder-width -2) (/ oled-holder-height -2) 0])
  )

(defn oled-holder-spacing-inner [corner]
  (let [oled-holder-inner-height (- oled-holder-height 2.5)
        oled-holder-inner-width (- oled-holder-width 2.5)]
    (case corner
    :tm [0 (/ oled-holder-inner-height 2) 0]
    :rm [(/ oled-holder-inner-width 2) 0 0]
    :bm [0 (/ oled-holder-inner-height 2) 0]
    :lm [(/ oled-holder-inner-width -2) 0 0]
    :tl [(/ oled-holder-inner-width -2) (/ oled-holder-inner-height 2) 0]
    :tr [(/ oled-holder-inner-width 2) (/ oled-holder-inner-height 2) 0]
    :br [(/ oled-holder-inner-width 2) (/ oled-holder-inner-height 2) 0]
    :bl [(/ oled-holder-inner-width -2) (/ oled-holder-inner-height -2) 0])))

(defn tps-43-mount-spacing [corner]
  (case corner
    :tm [0 (/ tps-43-mount-length 2) 0]
    :rm [(/ tps-43-mount-width 2) 0 0]
    :bm [0 (/ tps-43-mount-length -2) 0]
    :lm [(/ tps-43-mount-width -2) 0 0]
    :tl [(/ tps-43-mount-width -2) (/ tps-43-mount-length 2) 0]
    :tr [(/ tps-43-mount-width 2) (/ tps-43-mount-length 2) 0]
    :br [(/ tps-43-mount-width 2) (/ tps-43-mount-length -2) 0]
    :bl [(/ tps-43-mount-width -2) (/ tps-43-mount-length -2) 0]))


(defn tps-43-mount-inner-spacing [corner]
  (case corner
    :tm [0 (/ tps-43-mount-inner-length 2) 0]
    :rm [(/ tps-43-mount-inner-width 2) 0 0]
    :bm [0 (/ tps-43-mount-inner-length -2) 0]
    :lm [(/ tps-43-mount-inner-width -2) 0 0]
    :tl [(/ tps-43-mount-inner-width -2) (/ tps-43-mount-inner-length 2) 0]
    :tr [(/ tps-43-mount-inner-width 2) (/ tps-43-mount-inner-length 2) 0]
    :br [(/ tps-43-mount-inner-width 2) (/ tps-43-mount-inner-length -2) 0]
    :bl [(/ tps-43-mount-inner-width -2) (/ tps-43-mount-inner-length -2) 0]))



(def outer-thumb-tm-north-inset {:type :thumb :column 3 :corner :tm :offset [2.5 3.0 0] :direction :north })
(def outer-thumb-tm-north {:type :thumb :column 3 :corner :tm :offset [0 3.0 0] :direction :north })
(def outer-thumb-tm-north-inset-left {:type :thumb :column 3 :corner :tm :offset [-5 3.0 0] :direction :north})
(def outer-thumb-tl-north {:type :thumb :column 3 :corner :tl :offset [-2.5 3.0 0] :direction :north})
(def outer-thumb-tl-north-west {:type :thumb :column 3 :corner :tl :offset [-2.5 3.0 0] :direction :north-west})
(def outer-thumb-tl-west {:type :thumb :column 3 :corner :tl :offset [-2.5 3.0 0] :direction :west})
(def outer-thumb-bl-west {:type :thumb :column 3 :corner :bl :offset [-2.5 -3.0 0] :direction :west})
(def outer-thumb-bl-south-west {:type :thumb :column 3 :corner :bl :offset [-2.5 -3.0 0] :direction :south-west})
(def outer-thumb-bl-south {:type :thumb :column 3 :corner :bl :offset [-2.5 -3.0 0] :direction :south})
(def outer-thumb-br-south {:type :thumb :column 3 :corner :br :offset [-0.75 -3.25 0] :direction :south})
(def mid-left-thumb-bl-south {:type :thumb :column 2 :corner :bl :offset [0.75 -3.25 0] :direction :south})
(def mid-left-thumb-br-south {:type :thumb :column 2 :corner :br :offset [0 -3.25 0] :direction :south})
(def mid-right-thumb-bl-south {:type :thumb :column 1 :corner :bl :offset [0 -3.25 0] :direction :south})
(def mid-right-thumb-br-south {:type :thumb :column 1 :corner :br :offset [-0.75 -3.25 0] :direction :south})
(def inner-thumb-bl-south {:type :thumb :column 0 :corner :bl :offset [0.75 -3.25 0] :direction :south})
(def inner-thumb-br-south {:type :thumb :column 0 :corner :br :offset [2.75 -3.25 0] :direction :south})
(def inner-thumb-br-south-east {:type :thumb :column 0 :corner :br :offset [2.75 -3.25 0] :direction :south-east})
(def inner-thumb-br-east {:type :thumb :column 0 :corner :br :offset [2.75 -3.25 0] :direction :east})
(def inner-thumb-rm {:type :thumb :column 0 :corner :rm  :offset [3 1 0] :direction :south-east})
(def fourth-bottom-bl {:type :main-body :column 3 :row 0 :corner :bl :offset [0 -11 0] :direction :south})
(def fourth-bottom-br {:type :main-body :column 3 :row 0 :corner :br :offset [-4 -11 0] :direction :south-west :xy 2})
(def fourth-bottom-bm {:type :main-body :column 3 :row 0 :corner :bm :offset [0 -11 0] :direction :south :xy 1})
(def pinky-bottom-bl-south-west-inset {:type :main-body :column 4 :row 0 :corner :bl :offset [-3 0.75 0] :direction :south-west :xy 2})
(def pinky-bottom-bl-west {:type :main-body :column 4 :row 0 :corner :bl :offset [-2.75 -3 0] :direction :west :xy 2})
(def pinky-bottom-lm-south-west {:type :main-body :column 4 :row 0 :corner :lm :offset [-2.75 -6 0] :direction :south-west :xy 2})
(def pinky-bottom-bl-south-west {:type :main-body :column 4 :row 0 :corner :bl :offset [-2.75 -3 0] :direction :south-west :xy 2})
(def pinky-bottom-bl-south {:type :main-body :column 4 :row 0 :corner :bl :offset [-2.75 -3 0] :direction :south :xy 2})
(def pinky-bottom-bm {:type :main-body :column 4 :row 0 :corner :bm :offset [0 -3 0] :direction :south})
(def pinky-bottom-br-south {:type :main-body :column 4 :row  0 :corner :br :offset [2.5 -3 0] :direction :south})
(def pinky-bottom-br-south-east {:type :main-body :column 4 :row 0 :corner :br :offset [2.5 -3 0] :direction :south-east})
(def pinky-bottom-br-east {:type :main-body :column 4 :row 0 :corner :br :offset [2.5 -3 0] :direction :east})
(def pinky-bottom-br-south-east-inset {:type :main-body :column 4 :row 0 :corner :br :offset [2.5 1.5 0] :direction :south-east})
(def outer-pinky-bottom-bl {:type :main-body :column 5 :row 0 :corner :bl :offset [2.8 -3.25 0] :direction :south-east})
(def outer-pinky-bottom-bm {:type :main-body :column 5 :row 0 :corner :bm :offset [0 -3.0 0] :direction :south})
(def outer-pinky-bottom-br-south {:type :main-body :column 5 :row 0 :corner :br :offset [2.75 -3.0 0] :direction :south})
(def outer-pinky-bottom-br-south-east {:type :main-body :column 5 :row 0 :corner :br :offset [2.75 -3.0 0] :direction :south-east})
(def outer-pinky-bottom-br-east {:type :main-body :column 5 :row 0 :corner :br :offset [2.75 -3.0 0] :direction :east})
(def outer-pinky-top-br {:type :main-body :column 5 :row 1 :corner :br :offset [2.75 0 0] :direction :east})
(def outer-pinky-top-tr-east {:type :main-body :column 5 :row 1 :corner :tr :offset [2.75 2.6 0] :direction :east})
(def outer-pinky-top-tr-north-east {:type :main-body :column 5 :row 1 :corner :tr :offset [2.75 2.6 0] :direction :north-east})
(def outer-pinky-top-tr-north {:type :main-body :column 5 :row 1 :corner :tr :offset [2.75 2.6 0] :direction :north})
(def outer-pinky-top-tl {:type :main-body :column 5 :row 1 :corner :tl :offset [2.9 2.6 0] :direction :north-east})
(def pinky-top-tr-east {:type :main-body :column 4 :row 2 :corner :tr :offset [2.75 2.6 0] :direction :east})
(def pinky-top-tr-north-east {:type :main-body :column 4 :row 2 :corner :tr :offset [2.75 2.6 0] :direction :north-east})
(def pinky-top-tr-north {:type :main-body :column 4 :row 2 :corner :tr :offset [2.75 2.6 0] :direction :north})
(def pinky-top-tr-inset {:type :main-body :column 4 :row 2 :corner :tr :offset [0.5 2.8 0] :direction :north-east})
(def pinky-top-tm-north-east {:type :main-body :column 4 :row 2 :corner :tm :offset [2.75 2.6 0] :direction :north-east})
(def pcb-top-right-corner-point-east  {:type :pcb-top-right :direction :east})
(def pcb-top-right-corner-point-north-east {:type :pcb-top-right :direction :north-east})
(def pcb-top-right-corner-point-north {:type :pcb-top-right :direction :north})
(def middle-top-tr-inset {:type :main-body :column 2 :row 2 :corner :tr :offset [3 -1 0] :direction :north-east})
(def middle-top-tr-east {:type :main-body :column 2 :row 2 :corner :tr :offset [3 2.6 0] :direction :east})
(def middle-top-tr-north-east {:type :main-body :column 2 :row 2 :corner :tr :offset [3 2.6 0] :direction :north-east})
(def middle-top-tr-north {:type :main-body :column 2 :row 2 :corner :tr :offset [3 2.6 0] :direction :north})
(def middle-top-bm {:type :main-body :column 2 :row 2 :corner :tm :offset [0 2.6 0] :direction :north})
(def middle-top-tl-inset {:type :main-body :column 2 :row 2 :corner :tl :offset [-3 -1 0] :direction :north-west})

(def middle-top-tl-north  {:type :main-body :column 2 :row 2 :corner :tl :offset [-3 2.6 0] :direction :north})
(def middle-top-tl-north-west {:type :main-body :column 2 :row 2 :corner :tl :offset [-3 2.6 0] :direction :north-west})
(def middle-top-tl-west {:type :main-body :column 2 :row 2 :corner :tl :offset [-3 2.6 0] :direction :west})
(def inner-index-top-tr  {:type :main-body :column 0 :row 2 :corner :tr :offset [0 10.5 0] :direction :north})
(def oled-holder-tr-inset {:type :oled :corner :tr :direction :north-east :offset [1.5 (- klor-case-offset 2.5) 0]})
(def oled-holder-tr-east {:type :oled :corner :tr :direction :east})
(def oled-holder-tr-north-east {:type :oled :corner :tr :direction :north-east})
(def oled-holder-tr-north {:type :oled :corner :tr :direction :north})
(def oled-holder-tm-north {:type :oled :corner :tm :direction :north})
(def oled-holder-tl-north {:type :oled :corner :tl :direction :north})
(def oled-holder-tl-north-west {:type :oled :corner :tl :direction :north-west})
(def oled-holder-tl-west {:type :oled :corner :tl :direction :west})
(def oled-holder-lm-west {:type :oled :corner :lm :direction :west})
(def oled-holder-bl-west {:type :oled :corner :bl :direction :west})
(def oled-holder-bl-inset {:type :oled :corner :bl :direction :west :offset [0 3 0]})
(def tps-43-tl-north {:type :tps-43 :corner :tl :direction :north})
(def tps-43-tl-north-west {:type :tps-43 :corner :tl :direction :north-west})
(def tps-43-tl-west {:type :tps-43 :corner :tl :direction :west})
(def above-trrs-jack {:type :tps-43 :corner :bl :direction :west :xy 0.5 :offset (mapv + key-spacing-inner-west [(/ tps-43-mount-width 2) -2 0])})
(def below-trrs-jack {:type :tps-43 :corner :bl :direction :west :xy 0.5 :offset (mapv + key-spacing-inner-south-west [(/ tps-43-mount-width 2) -6 0])})
;(def above-outer-thumb {:type :tps-43 :corner :bl :direction :west :offset (mapv + key-spacing-inner-south-west [(/ tps-43-mount-width 2) -2 0])} )