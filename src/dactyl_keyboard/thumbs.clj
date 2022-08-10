(ns dactyl-keyboard.thumbs
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.shape-parameters :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.placement-functions :refer :all]
            [dactyl-keyboard.sa-keycaps :refer :all]
            [dactyl-keyboard.web-connecters :refer :all]
            [unicode-math.core :refer :all]))

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
   (if (or trackball-enabled joystick-enabled joycon-joystick-enabled) nil (minithumb-tl-place shape))
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
    (color [0 1 0 1] (triangle-hulls    ; top two to the main keyboard, starting on the left
      ;(minithumb-tl-place web-post-tl)
      ;(key-place (+ innercol-offset 0) cornerrow web-post-bl)
      ;(minithumb-tl-place web-post-tr)

      (key-place (+ innercol-offset 0) cornerrow web-post-br)
      (minithumb-tr-place minithumb-post-tl)
      (key-place (+ innercol-offset 1) cornerrow web-post-bl)
      (minithumb-tr-place minithumb-post-tr)
      (key-place (+ innercol-offset 1) cornerrow web-post-br)
      (key-place (+ innercol-offset 1) lastrow web-post-br)
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
      (key-place (+ innercol-offset 3) cornerrow web-post-br)))
     (color [1 0 0 1](triangle-hulls
      (key-place (+ innercol-offset 1) cornerrow web-post-br)
      (key-place (+ innercol-offset 2) lastrow web-post-tl)
      (key-place (+ innercol-offset 2) cornerrow web-post-bl)
      (key-place (+ innercol-offset 2) lastrow web-post-tr)
      (key-place (+ innercol-offset 2) cornerrow web-post-br)
      (key-place (+ innercol-offset 3) cornerrow web-post-bl)))
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
