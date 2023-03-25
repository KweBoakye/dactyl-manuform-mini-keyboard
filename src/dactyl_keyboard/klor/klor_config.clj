(ns dactyl-keyboard.klor.klor-config)

(def nrows 3)
(def ncols 6)
(def nthumb-keys 4)

(def key-spacing-horizontal 19.00)
(def key-spacing-vertical 19.00)

(def inner-index-stagger [0 4 0])
(def index-stagger [0 7 0])
(def middle-stagger [0 16 0])
(def ring-stagger [-0.5 9.75 0])
(def pinky-stagger [-0.5 4.75 0])
(def extra-pinky-stagger [-0.5 9.75 0])

(def inner-pre-translation [0 0 0])
(def index-pre-translation [0 0 0])
(def middle-pre-translation [0 0 0])
(def ring-pre-translation  [-12 -10  0]
  )
(def pinky-pre-translation  [0 0  0])
(def extra-pinky-pre-translation [0 0  0])

(def anchor-rotation 10)
(def inner-index-rotation 0)
(def index-rotation 0)
(def middle-rotation 0)
(def ring-rotation -4)
(def pinky-rotation -10)
(def extra-pinky-rotation -10)

(def thumb-anchor [22 -18 0])
(def thumb-splay 15)
(def thumb-spread -21.25)
(def thumb-origin [11 -1 0] )



(defn column-offset [column]
  (cond
    (= column 0) inner-index-stagger
    (= column 1) index-stagger
    (= column 2) middle-stagger
    (= column 3) ring-stagger
    (= column 4) pinky-stagger
    (= column 5) extra-pinky-stagger
    :else [0 0 0]))

(defn pre-translation [column]
  (cond 
    (= column 0) inner-pre-translation
    (= column 1) index-pre-translation
    (= column 2) middle-pre-translation
    (= column 3) ring-pre-translation
    (= column 4) pinky-pre-translation
    (= column 5) extra-pinky-pre-translation
    :else [0 0 0]))

(defn column-rotation [column] 
   (cond (= column 0) inner-index-rotation
         (= column 1) index-rotation
         (= column 2) middle-rotation
         (= column 3) ring-rotation
         (= column 4) pinky-rotation
         (= column 5) extra-pinky-rotation
         :else 0)
   )