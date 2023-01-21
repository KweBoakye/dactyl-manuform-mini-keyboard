(ns dactyl-keyboard.case
   (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.shape-parameters :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.placement-functions :refer :all]
            [dactyl-keyboard.web-connecters :refer :all]
             [dactyl-keyboard.thumbs :refer :all]
            [dactyl-keyboard.oled :refer :all]
            [dactyl-keyboard.trackball :refer :all]
            [dactyl-keyboard.joycon-joystick :refer :all]
            ))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(def oled-post (->> (web-post-shape oled-holder-thickness )
                    (translate [0 0 (+ (/ oled-holder-thickness -2) plate-thickness)])))


(def case-filler-cup (difference (translate trackball-origin filler-half-circle)
                                 key-clearance
                                 thumb-key-clearance
                                 (translate trackball-origin rotated-dowells)))



(def left-wall-x-offset 33.2)
(def left-wall-x-offset-trackball 0)
(def left-wall-x-offset-oled -10)
(def left-wall-z-offset 3)

(defn left-key-position [row direction]
  (map - (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0]) [left-wall-x-offset 0 left-wall-z-offset]))

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

(defn left-wall-plate-place-no-rotation [xdir ydir shape]
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
                (key-wall-brace lastcol (inc y) 1 0 web-post-tr lastcol (inc y) 1 0 web-post-br)))))
    (union (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
           (if extra-row
             (union (for [y (range 0 (inc lastrow))] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br))
                    (for [y (range 1 (inc lastrow))] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr)))
             (union (for [y (range 0 lastrow)] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br))
                    (for [y (range 1 lastrow)] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr))))
           (key-wall-brace lastcol extra-cornerrow 0 -1 web-post-br lastcol extra-cornerrow 1 0 web-post-br))))

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

(def inner-column-bottom-section
  (color [0.3 0.4 0.8 1] (union
                          (hull
                           (key-place 0 (dec cornerrow) web-post-bl)
                           (key-place 0 (dec cornerrow) web-post-br)
                           (key-place 0 cornerrow web-post-tr))

                          (hull
                           (key-place 0 (dec cornerrow) web-post-bl)
                           (key-place 0 cornerrow (translate (wall-locate1 0 0) web-post-tl))
                           (key-place 0 cornerrow web-post-tr))

                          (hull
                           (key-place 0 cornerrow web-post-tr)
                           (key-place 1 cornerrow web-post-tl)
                           (key-place 1 cornerrow web-post-bl))
                          (hull
                           (key-place 0 cornerrow (translate (wall-locate1 0 0) web-post-tl))
                           (key-place 0 cornerrow web-post-tr)
                           (key-place 0 cornerrow web-post-br))
     ;removed so it doesnt clash with the trackball
                            ; (if (or trackball-enabled joystick-enabled) nil (hull
                            ;                                                  (key-place 1 lastrow web-post-tl)
                            ;                                                  (key-place 1 cornerrow web-post-bl)
                            ;                                                  (minithumb-tl-place minithumb-post-tl)))

                          (triangle-hulls
                           (key-place 0  cornerrow (translate (wall-locate1 0 0) web-post-tl))
                           (key-place 0 cornerrow web-post-br)
                           (key-place 0 cornerrow (translate (wall-locate3 0 0) web-post-bl)))

                          (triangle-hulls
                           (key-place 0  cornerrow web-post-tr)
                           (key-place 1 cornerrow web-post-bl)
                           (key-place 0 cornerrow web-post-br)))))

(def mini-thumb-wall
  (union
   ; thumb walls
   ;(wall-brace minithumb-mr-place  0 -1 web-post-br minithumb-tr-place  0 -1 minithumb-post-br)
   ;(wall-brace minithumb-mr-place  0 -1 web-post-br minithumb-mr-place  0 -1 web-post-bl)
   (color [0.1 0.4 0.5 1] (wall-brace minithumb-bl-place -1  0 web-post-bl minithumb-br-place -1  0 web-post-tl)
          (wall-brace minithumb-br-place  0 -1 web-post-br minithumb-br-place  0 -1 web-post-bl)
          (wall-brace minithumb-br-place -1  0 web-post-tl minithumb-br-place -1  0 web-post-bl)
          (wall-brace minithumb-bl-place -1  0 web-post-tl minithumb-bl-place -1  0 web-post-bl))
   (color [0.8 0.2 0.5 1] (wall-brace minithumb-br-place  0 -1 web-post-br minithumb-br-place  0 -1 web-post-bl))
   ;(if (or trackball-enabled joystick-enabled) nil (wall-brace minithumb-bl-place  0  1 web-post-tr minithumb-bl-place  0  1   web-post-tl))

   (color [0.5 0.1 0.8 0.8] (wall-brace minithumb-br-place -1  0 web-post-tl minithumb-br-place -1  0 web-post-bl))
   (color [0.3 0.5 0.7 1] (wall-brace minithumb-bl-place -1  0 web-post-tl minithumb-bl-place -1  0 web-post-bl))
;
   ; minithumb corners
   ;(wall-brace minithumb-br-place -1  0 web-post-bl minithumb-br-place  0 -1 web-post-bl)
   (color [1 0 0 1] (wall-brace minithumb-bl-place -1  0 web-post-tl minithumb-bl-place  0  1 web-post-tl))
   ; minithumb tweeners

   (wall-brace minithumb-bl-place -1  0 web-post-bl minithumb-br-place -1  0 web-post-tl)
   (wall-brace minithumb-tr-place  0 -1 minithumb-post-br (partial key-place (+ innercol-offset 3) lastrow)  0 -1 web-post-bl)

   (if joycon-joystick-enabled (color [0 0 1 1] joycon-joystick-case-cover))

   ; connectors below the inner column to the thumb & second column
   (if inner-column inner-column-bottom-section)))


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
  (color [0 1 1 1] (union
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

(def thumb-to-front-wall (union
                          (wall-brace-xy (partial key-place 0 (- lastrow innercol-offset)) 1 1  (translate (wall-locate3 0 0) web-post-bl)  (partial key-place 0 (- lastrow innercol-offset)) 0 0  web-post-br wall-xy-offset-thin wall-xy-offset-thin)
                          (wall-brace-xy (partial key-place 0 (- lastrow innercol-offset)) 0 0 web-post-br  (partial key-place 1 (- lastrow innercol-offset)) 0 0  web-post-bl wall-xy-offset wall-xy-offset)
                          (wall-brace-xy (partial key-place 1 (- lastrow innercol-offset)) 0 0 web-post-bl  (partial key-place 1 (- lastrow innercol-offset)) 0 0  web-post-br wall-xy-offset wall-xy-offset)
                          ;frommiddle key closest to 
                          (if (or trackball-enabled joystick-enabled) nil (color [0 1 1 1] (wall-brace minithumb-tl-place  0  1 web-post-tl minithumb-tl-place  0  0   web-post-tr)))
                          (if (or trackball-enabled joystick-enabled) nil (color [0 0 1 1] (wall-brace minithumb-tl-place  0  1 web-post-tl minithumb-bl-place  0  1   web-post-tr)))
                          (if (or trackball-enabled joystick-enabled) nil (color [1 0 1 1] (wall-brace minithumb-bl-place  0  1 web-post-tl minithumb-bl-place  0  1   web-post-tr)))

                          (color [0 1 0 1]
                                 (triangle-hulls
                                  (minithumb-tl-place minithumb-post-tr)
                                  (minithumb-tr-place minithumb-post-tl)
                                  (key-place 1 cornerrow web-post-br)
                           ;(minithumb-tr-place web-post-tl)
                                  (key-place 1 cornerrow web-post-br)
                                  (minithumb-tr-place minithumb-post-tl)
                                  (minithumb-tl-place (translate (wall-locate3 0 0) web-post-tr))

                                  (minithumb-tl-place minithumb-post-tr)
                                  (minithumb-tr-place minithumb-post-tl)
                                  (key-place 1 cornerrow web-post-br)))



                          (color [1 0 0 1] (triangle-hulls
                                            (minithumb-tl-place minithumb-post-tr)
                                            (minithumb-tr-place minithumb-post-tl)
                                            (key-place 1 cornerrow web-post-bl)

                                            (minithumb-tr-place minithumb-post-tl)
                                            (key-place 1 cornerrow web-post-bl)
                                            (key-place 1 cornerrow web-post-br)))




                          (color [255/256 165/256 0 1]
                                 (triangle-hulls
                                  (minithumb-tl-place (translate (wall-locate3 0 1) minithumb-post-tl))
                                  (minithumb-tl-place minithumb-post-tr)
                                  (key-place 1 cornerrow (translate (wall-locate3 0 0) web-post-bl))


                                  (minithumb-tl-place minithumb-post-tr)
                                  (key-place 1 cornerrow web-post-bl)
                                  (key-place 1 cornerrow (translate (wall-locate3 0 0) web-post-bl))))))


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
                                    (color [0 1 0 1] (hull
                                                      (bottom 25 (left-key-place-narrow cornerrow -1 (translate (wall-locate3 -1 2)  big-boi-web-post)))
                                                      (translate trackball-origin (trackball-mount-rotate cup)))))
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

(def default-back-wall
  (union
   (for [x (range 0 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
   (for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))))


(def smooth-back-wall
  (union
   (for [x (range 0 (- ncols 2))] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
   (for [x (range 1 (- ncols 2))] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
   (key-wall-brace (+ innercol-offset 3) 0 0 1 web-post-tr (+ innercol-offset 4) 0 0 1 web-post-tr)
   (key-wall-brace (+ innercol-offset 4) 0 0 1 web-post-tr lastcol 0 0 1 web-post-tr)
   (triangle-hulls
    (key-place (+ innercol-offset 3) 0  web-post-tr)
    (key-place (+ innercol-offset 4) 0  web-post-tl)
    (key-place (+ innercol-offset 4) 0  web-post-tr))))

   ; (key-place (+ innercol-offset 3) 0  web-post-tr)
   ; (key-place lastcol 0  web-post-tl)
   ; (key-place lastcol 0  web-post-tr)




(def back-wall
  (if smooth-back-wall-on smooth-back-wall default-back-wall))


(def left-wall
  (difference
   (union

    (left-wall-plate-place 0 0 oled-holder)
 ;(left-wall-plate-place oled-x-position oled-y-position oled-holder-block)

    (wall-brace-xy (partial key-place 0 0) 0 1 web-post-tl  (partial left-wall-plate-place 1 1) 0 1 oled-post wall-xy-offset wall-xy-offset-thin)
    (wall-brace-xy  (partial left-wall-plate-place 1 1) 0 1 oled-post  (partial left-wall-plate-place -1 1) 0 1 oled-post wall-xy-offset-thin wall-xy-offset-thin)
    (wall-brace-xy  (partial left-wall-plate-place -1 1) 0 1 oled-post  (partial left-wall-plate-place -1 1) -1 0 oled-post wall-xy-offset-thin wall-xy-offset-thin)
    (wall-brace-xy  (partial left-wall-plate-place -1 1) -1 0 oled-post  (partial left-wall-plate-place -1 -1) -1 -1 oled-post wall-xy-offset-thin wall-xy-offset-thin)
;(wall-brace-xy (partial left-wall-plate-place -1 -1) -1 -1 oled-post  (partial key-place 0 (- lastrow innercol-offset)) 0 0 web-post-tl wall-xy-offset-thin wall-xy-offset-thin)
    (wall-brace-xy (partial left-wall-plate-place -1 -1) -1 -1 oled-post  (partial left-wall-plate-place -1 -2) -1 0 oled-post wall-xy-offset-thin wall-xy-offset-thin)
    (wall-brace-xy (partial left-wall-plate-place -1 -2) -1 0 oled-post (partial left-wall-plate-place -1 -3) -1 0 (translate [0 0 left-wall-z-offset] oled-post) wall-xy-offset-thin wall-xy-offset-thin)
    ;(color [1 0 0 1] (wall-brace-xy (partial left-key-place 2 -1) -1 0 oled-post  (partial key-place 0 (- lastrow 1)) 1 1  (translate (wall-locate3 0 0 ) web-post-bl) wall-xy-offset-thin wall-xy-offset-thin))
    (color [1 0 0 1] (wall-brace-xy (partial left-wall-plate-place -1 -3) -1 0 (translate [0 0 left-wall-z-offset] oled-post) (partial key-place 0 (- lastrow 1)) 1 1  (translate (wall-locate3 0 0) web-post-bl) wall-xy-offset-thin wall-xy-offset-thin)))))
    ;(if (or trackball-enabled joystick-enabled) nil (color [0 1 0 1] (wall-brace-xy (partial key-place 0 (- lastrow innercol-offset)) -2 1   web-post-bl (partial key-place 0 (- lastrow innercol-offset)) 0 0 web-post-br wall-xy-offset-thin wall-xy-offset-thin)))
     ;(if trackball-enabled nil (wall-brace-xy (partial key-place 0 (- lastrow innercol-offset)) 0 0 web-post-br (partial key-place 0 (- lastrow innercol-offset) ) 0 0 minithumb-post-bl wall-xy-offset-thin wall-xy-offset-thin) )
 ;(if trackball-enabled nil (color [1 0 0 1](wall-brace-xy (partial key-place 0 (- lastrow innercol-offset)) 0 0 web-post-bl (partial key-place 0 cornerrow ) 0 0 minithumb-post-tl wall-xy-offset-thin wall-xy-offset-thin)))
 ;(wall-brace (partial key-place 0 cornerrow) 0 0 minithumb-post-tl minithumb-bl-place  0  1 web-post-tl)

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


  ;(left-wall-plate-place oled-x-position oled-y-position (translate [0 0 -6] (cube (first oled-pcb-size) (second oled-pcb-size) 4 :center true) ))


(def joystick (import "../things/joystick hole.stl"))
(def joystick-cutout (color [0 0 1] (translate [0 0 6] (cylinder (/ 33.6 2) 3.5))))
(defn joystick-position [shape]
  (->> shape
       (rotate (deg2rad 60) [0 1 0])
       (rotate (deg2rad -35) [0 0 1])
       (translate [64 -35 36])))

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

    (color [0 1 1 1] (triangle-hulls
                      (minithumb-tr-place minithumb-post-tl)
                      (minithumb-bl-place minithumb-post-tr)
                      (key-place 1 cornerrow web-post-bl)))

    (triangle-hulls
     (color [1 1 0 1] (key-place 1 cornerrow web-post-bl)
            (minithumb-bl-place minithumb-post-tr)
            (minithumb-tl-place web-post-tl))

     (color [0.3 0.4 0 1] (minithumb-tl-place web-post-bl)

            (minithumb-bl-place web-post-tl)
            (minithumb-bl-place minithumb-post-tr)))

     ;(color [1 0 1 1](minithumb-tr-place web-post-tl)
     ;       (minithumb-tl-place web-post-tl)
     ;       (minithumb-bl-place minithumb-post-tr))


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
     (wall-brace-xy minithumb-bl-place 0  0 minithumb-post-tl minithumb-tl-place 0 0 web-post-tr  wall-xy-offset-thin wall-xy-offset-thin))
    (wall-brace-xy minithumb-tl-place  -2 0 web-post-tr (partial key-place 0 cornerrow) -2 0 web-post-br wall-xy-offset-thin wall-xy-offset-thin)
    (wall-brace-xy  (partial key-place 0 cornerrow) 0 0 web-post-br (partial key-place 0 cornerrow) 0 0 web-post-bl wall-xy-offset-thin wall-xy-offset-thin))


        ;(wall-brace minithumb-tl-place 0 0 web-post-br (partial key-place 0 cornerrow) 0 0 web-post-br )


      ;(triangle-hulls
      ; (minithumb-mr-place web-post-tl)
      ; (minithumb-tl-place web-post-bl)
      ; (minithumb-tl-place web-post-tl)
      ; )

   joystick-cutout))

(def left-section
  (union
   (color [0 1 0 1] (triangle-hulls

                     (left-wall-plate-place -1 -3 (translate [0 0 left-wall-z-offset] oled-post))
                     (left-wall-plate-place 1 -3 (translate [0 0 left-wall-z-offset] oled-post))
                     (key-place 0 cornerrow (translate (wall-locate3 0 0) web-post-bl))))




;   (triangle-hulls
;    (left-wall-plate-place 1 -3 (translate [0 0 left-wall-z-offset] oled-post))
;(key-place 0  cornerrow (translate (wall-locate2 0 0) web-post-tl))
;(key-place 0 cornerrow (translate (wall-locate3 0 0) web-post-bl)))
     ;(oled-place oled-holder)
   (triangle-hulls
    (left-wall-plate-place 1 1 oled-post)
    (key-place 0 0 web-post-tl)
    (key-place 0 0 web-post-bl))

   ;(left-wall-plate-place 1 1 oled-post)
   ;(key-place 0 0 web-post-bl)
   ;(key-place 0 1 web-post-tl)

   (triangle-hulls
    (left-wall-plate-place 1 1 oled-post)
    (left-wall-plate-place 1 0 oled-post)
    (key-place 0 0 web-post-bl))

   (triangle-hulls
    ;top triangle between oled area and case at first row
    (key-place 0 0 web-post-bl)
    (key-place 0 1 web-post-tl)
    (left-wall-plate-place 1 0 oled-post))



    ;key-place 0 0 web-post-bl)
    ;key-place 0 1 web-post-tl)
    ;left-wall-plate-place 1 -1 oled-post)

   (triangle-hulls
    ; second row
    (left-wall-plate-place 1 0 oled-post)
    (key-place 0 1 web-post-tl)
    (key-place 0 1 web-post-bl))

   (triangle-hulls
    (left-wall-plate-place 1 -1 oled-post)
    (left-wall-plate-place 1 0 oled-post)
    (key-place 0 1 web-post-bl))

   (triangle-hulls
    ;small triangle between second and third row and left section
    (left-wall-plate-place 1 -1 oled-post)
    (key-place 0 1 web-post-bl)
    (key-place 0 2 web-post-tl))


   (triangle-hulls
    ;triangle between left section and  of 3rd row
    (left-wall-plate-place 1 -1 oled-post)
    (key-place 0 2 web-post-tl)
    (key-place 0 2 web-post-bl))

   (triangle-hulls
    ;triangle between left section and 3rd row
    (left-wall-plate-place 1 -1 oled-post)
    (left-wall-plate-place 1 -2 oled-post)
    (key-place 0 2 web-post-bl))


   (triangle-hulls
    (left-wall-plate-place 1 -2 oled-post)
    (key-place 0 2 web-post-bl)
    (key-place 0 cornerrow (translate (wall-locate1 0 0) web-post-tl)))

   (triangle-hulls
    (left-wall-plate-place 1 -2 oled-post)
    (key-place 0 cornerrow (translate (wall-locate1 0 0) web-post-tl))
    (key-place 0 cornerrow (translate (wall-locate3 0 0) web-post-bl)))

   (triangle-hulls
    (left-wall-plate-place 1 -2 oled-post)
    (left-wall-plate-place 1 -3 (translate [0 0 left-wall-z-offset] oled-post))
    (key-place 0 cornerrow (translate (wall-locate3 0 0) web-post-bl)))


   (triangle-hulls
   ;top righttriangle of encoder section
    (left-wall-plate-place 1 -1 oled-post)
    (left-wall-plate-place 1 -2 oled-post)
    (left-wall-plate-place -1 -1 oled-post))

   (triangle-hulls
  ;bottom left triangle of encpoder section
    (left-wall-plate-place -1 -1 oled-post)
    (left-wall-plate-place -1 -2 oled-post)
    (left-wall-plate-place 1 -2 oled-post))

   (triangle-hulls
   ;top left triangle below encoder
    (left-wall-plate-place -1 -2 oled-post)
    (left-wall-plate-place 1 -2 oled-post)
    (left-wall-plate-place -1 -3 (translate [0 0 left-wall-z-offset] oled-post)))

   (triangle-hulls
   ;bottom right triangle below encoder
    (left-wall-plate-place 1 -2 oled-post)
    (left-wall-plate-place -1 -3 (translate [0 0 left-wall-z-offset] oled-post))
    (left-wall-plate-place 1 -3 (translate [0 0 left-wall-z-offset] oled-post)))


   (triangle-hulls)))
 ;(left-wall-plate-place 1 -2 oled-post)
 ; (key-place 0 2 web-post-bl)
 ;(key-place 0 cornerrow (translate (wall-locate2 0 0) web-post-tl))

 ; (left-wall-plate-place 1 -2 oled-post)
 ; (left-wall-plate-place 1 -3 (translate [0 0 left-wall-z-offset] oled-post))
 ;  (key-place 0 cornerrow (translate (wall-locate2 0 0) web-post-tl))



;
;  (left-wall-plate-place 1 -3 (translate [0 0 left-wall-z-offset] oled-post))
;key-place 0  cornerrow (translate (wall-locate2 0 0) web-post-tl))
;key-place 0 2 web-post-bl)
;
   ;(key-place 0 3 (translate (wall-locate3 0 0) web-post-tl))


 ;  (left-key-place 2 -1 oled-post)
 ;  (key-place 0 2 web-post-bl)
 ; (key-place 0 3 (translate (wall-locate1 0 0) web-post-tl))

 ;  (left-key-place 2 -1 oled-post)
 ;  (key-place 0 3 (translate (wall-locate1 0 0) web-post-tl))
 ;  (key-place 0 3 (translate (wall-locate3 0 0 ) web-post-bl))

;;;;;;;;;;

   ;(key-place  0 2  web-post-bl)
   ;(key-place 0 2 web-post-tl)
   ;(left-wall-plate-place -1 -1 oled-post)

    ;l
    ;(left-wall-plate-place -1 -1 oled-post)
    ;(left-wall-plate-place 1 -1 oled-post)
    ;(left-wall-plate-place 1 -2 oled-post)
    ;(key-place 0 (- cornerrow innercol-offset) web-post-bl)

    ;l
    ;(left-wall-plate-place -1 -2 oled-post)
    ;(left-wall-plate-place -1 -1 oled-post)
    ;(left-wall-plate-place 1 -2 oled-post)

   ; (key-place 0 (- cornerrow innercol-offset) web-post-bl)
   ; (left-wall-plate-place -1 -1 oled-post)
   ; (left-key-place 2 -1  oled-post)
;
   ; (key-place 0 (- cornerrow innercol-offset) web-post-bl)
   ; (key-place 0 (- lastrow innercol-offset) web-post-bl)
   ; (left-key-place 2 -1 oled-post)
;;;;;;;;;;;;;;;;;;;;;;;;;

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


(def smoother-front-wall
  (if extra-row
    (union
     (key-wall-brace (+ innercol-offset 3) lastrow  0 -1 web-post-bl (+ innercol-offset 4) lastrow  0 -1 web-post-bl)
     (key-wall-brace (+ innercol-offset 4) lastrow  0 -1 web-post-bl lastcol extra-cornerrow 0 -1 web-post-br)
     (triangle-hulls
      (key-place (+ innercol-offset 3) lastrow web-post-bl)
      (key-place (+ innercol-offset 4) lastrow web-post-bl)
      (key-place (+ innercol-offset 3) lastrow web-post-br)))



    (union
     (key-wall-brace (+ innercol-offset 3) lastrow  0 -1 web-post-bl (+ innercol-offset 3) lastrow   0 -1 web-post-br)
     (key-wall-brace (+ innercol-offset 3) lastrow   0 -1 web-post-br (+ innercol-offset 4) extra-cornerrow 0 -1 web-post-br)
     (key-wall-brace (+ innercol-offset 4) extra-cornerrow 0 -1 web-post-br lastcol extra-cornerrow 0 -1 web-post-br)
     (triangle-hulls
      (key-place (+ innercol-offset 3) lastrow web-post-br)
      (key-place (+ innercol-offset 4) extra-cornerrow web-post-bl)
      (key-place (+ innercol-offset 4) extra-cornerrow web-post-br)))))

  ;  (key-place (+ innercol-offset 3) lastrow web-post-br)
  ;  (key-place lastcol extra-cornerrow  web-post-bl)
  ;  (key-place lastcol extra-cornerrow  web-post-br)




(def default-front-wall
  (union
   (key-wall-brace (+ innercol-offset 3) lastrow  0 -1 web-post-bl (+ innercol-offset 3) lastrow   0 -1 web-post-br)
   (key-wall-brace (+ innercol-offset 3) lastrow  0 -1 web-post-br (+ innercol-offset 4) extra-cornerrow 0 -1 web-post-bl)
   (for [x (range (+ innercol-offset 4) ncols)] (key-wall-brace x extra-cornerrow 0 -1 web-post-bl x       extra-cornerrow 0 -1 web-post-br))
   (for [x (range (+ innercol-offset 5) ncols)] (key-wall-brace x extra-cornerrow 0 -1 web-post-bl (dec x) extra-cornerrow 0 -1 web-post-br))))


(def front-wall
  (color [0.4 0.2 0.7 1]
         (if smooth-front-wall-on smoother-front-wall default-front-wall)))



(def non-thumb-walls (union
                      left-wall
                       ; back-wall
                            ; front wall
                      front-wall))
;                            (for [x (range 5 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl (dec x) cornerrow 0 -1 web-post-br))
                            ; Right before the start of the thumb
                        ;(color [ 0 0 1 1] (wall-brace minithumb-tr-place  0 -1 minithumb-post-br (partial key-place 3 (- lastrow innercol-offset))  0 -1 web-post-bl))




(def case-walls
  (difference
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
    (if (or trackball-enabled joystick-enabled) nil
        thumb-to-front-wall)
    back-convex-thumb-wall-0)
   (if joycon-joystick-enabled (joycon-joystick-place (translate [0,0,-1] joycon-joystick-top-right-screw-mount-clearance)))
   (if joycon-joystick-enabled (joycon-joystick-place joycon-joystick-case))
   (if joycon-joystick-enabled (joycon-joystick-place joycon-joystick-case-clearance))
   ))
   ;(joycon-joystick-place joycon-joystick-test-cut)

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