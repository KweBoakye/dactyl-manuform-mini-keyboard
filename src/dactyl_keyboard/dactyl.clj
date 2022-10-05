(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.shape-parameters :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.sa-keycaps :refer :all]
            [dactyl-keyboard.placement-functions :refer :all]
            [dactyl-keyboard.web-connecters :refer :all]
            [dactyl-keyboard.thumbs :refer :all]
            [dactyl-keyboard.oled :refer :all]
            [dactyl-keyboard.hand :refer :all]
            [dactyl-keyboard.hotswap :refer :all]
            [dactyl-keyboard.trackball :refer :all]
            [dactyl-keyboard.joycon-joystick :refer :all]
            [dactyl-keyboard.IS31FL3731-mount :refer :all]
            [dactyl-keyboard.case :refer :all]
            [dactyl-keyboard.aviator-and-reset :refer :all]
            [dactyl-keyboard.mcu-holder :refer :all]
            [dactyl-keyboard.encoder :refer :all]
            [dactyl-keyboard.screw-inserts :refer :all]
            [dactyl-keyboard.plate :refer :all]
            [dactyl-keyboard.tenting-hardware :refer :all]
            [dactyl-keyboard.palm-rest :refer :all]
            [dactyl-keyboard.cirque-circle-trackpad :refer :all]
            [dactyl-keyboard.cirque-circle-trackpad-placement-functions :refer :all]
            [dactyl-keyboard.toggle-switch :refer :all]
            [dactyl-keyboard.led-holder-pcb :refer :all]
            [dactyl-keyboard.joystick :refer :all]
            [dactyl-keyboard.pimoroni-haptic-mount :refer :all]
            [unicode-math.core :refer :all]))









   

;; (def left-joystick-module
;;   (mirror [-1 0 0 ] joystick-module))

;; (defn joystick-place [shape]
;;   (minithumb-tl-place
;;    (rotate (deg2rad -60) [0 0 1] (translate [0 1 0] shape))))


;; ;(def pinky-walls
;; ;  (union
;;  ;  (key-wall-brace lastcol cornerrow 0 -1 web-post-br lastcol cornerrow 0 -1 wide-post-br)
;;  ;  (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 0 1 wide-post-tr)))



;; (def hand-on-test
;;   (translate [-25 -60 92]
;;              (rotate (deg2rad -27) [1 0 0]
;;                      (rotate (deg2rad 12) [0 0 1]
;;                              (rotate (+ tenting-angle (deg2rad 5)) [0 1 0]
;;                                      (rotate
;;                                       (deg2rad -90) [1 0 0]
;;                                       (mirror [0 1 0] hand)))))))

;; (def toggle-test 
;;   (difference
;;                   (toggle-switch-place (translate [-7 0 12](cube 50 50 plate-thickness)) false 0 0)
;;                    upper-toggle-switch-cover-base-cutout
;;                    lower-toggle-switch-cover-base-cutout
;;                    upper-toggle-switch-hole
;;                    lower-toggle-switch-hole
                   
;;   ))

;; (def IS31FL3731-screw-mounts-test
;;  (difference (union
;;               (cube 35 50 2.6 :center false)
;;               (translate [4 4 4] IS31FL3731-screw-mounts-set))
;;              (translate [7.5 7.5 -0.1] (cube 20 35 2.8 :center false))) )


;; (spit "things/palm-rest.scad" (write-scad
;;                                (include "../nutsnbolts/cyl_head_bolt.scad")
;;                                palm-rest))
;; (spit "things/left-palm-rest.scad" (write-scad
;;                                     (include "../nutsnbolts/cyl_head_bolt.scad")
;;                                     (mirror [-1 0 0] palm-rest)))

;; (spit "things/palm-attach-test.scad" (write-scad
;;                                       palm-attach-rod))

;; (def key-trackball-clearance
;;  ( let [clearance (hull
;;                    (key-place 1 cornerrow web-post-tl)
;;                    (key-place 1 cornerrow web-post-tr)
;;                    (key-place 1 cornerrow web-post-bl)
;;                    (key-place 1 cornerrow web-post-br))]
;;  (union
;;   (translate [0 0 (- plate-thickness) ] clearance )
;;   (translate [0 0 (* -2 plate-thickness) ] clearance)
;;   )
;; )
;; )

;; (def model-right (difference

;;                   (union
;;                    key-holes
;;                    key-holes-inner
;;                    pinky-connectors
;;                    extra-connectors
;;                    connectors
;;                    inner-connectors
;;                    ;(color [1 0 0 1] usb-holder)
;;                    ;(color [1 0 0 1] aviator-male-connecter-clearence-test)
;;                   ;(translate [0 -10 0] (color [0 1 0 1] aviator-female-connecter-clearence-test))
;;                    (cirque-TM040040-place cirque-TM040040-mount)
;;                   (difference
;;                    (cirque-TM040040-place cirque-TM040040-mount-walls)
;;                    cirque-TM040040-mount-walls-mask-block
;;                    )
;;                   ; (color [0 1 1 1] (toggle-switch-place (translate [0 (+ (/ (- toggle-switch-main-body-y) 2) 7.64) 0] toggle-switch)  true 0 upper-toggle-switch-z-position))
;;                    ;(color [0 0 1 1](toggle-switch-place  (translate [0 (+ (/ (- toggle-switch-main-body-y) 2) 7.64) 0]toggle-switch ) false 0 lower-toggle-switch-z-position))
                  
;;                    ;upper-toggle-switch-cover
;;                    ;lower-toggle-switch-cover
;;                   ;(encoder-place encoder-placeholder)
;;                   (encoder-place (translate [ 0 0 6] encoder-mount))
;;                    (key-place 0 2 keyhole-fill)
                   
;;                    ;(color [1 0 0 1](cirque-place cirque))
;;                    ;(joystick-place (translate [0 -25 (- joystick-module-depth)] left-joystick-module))
;;                    ;(joycon-joystick-place (color [1 0 0 1] joycase))
;;                    (difference
;;                     (union thumb-type
;;                            thumb-connector-type)
;;                     (if joycon-joystick-enabled (joycon-joystick-place joycon-joystick-top-right-screw-mount-clearance))
;;                     ;(cirque-TM040040-place cirque-TM040040-mount)
;;                     ;(cirque-TM040040-place cirque-TM040040-mount-walls)
;;                     (if joycon-joystick-enabled (joycon-joystick-place joycon-joystick-bottom-left-screw-mount-clearance))
;;                      (if joycon-joystick-enabled (joycon-joystick-place joycon-joystick-top-right-screw-mount-hole))
;;                       (if joycon-joystick-enabled (joycon-joystick-place joycon-joystick-bottom-left-screw-mount-hole))
;;                     (if joycon-joystick-enabled (joycon-joystick-place joycon-joystick-case-clearance)))

;;                    ;(color [1 0 0 1] key-trackball-clearance)
;;                    ;(if joystick-enabled joystick-wall nil)
;;                    ;(joycon-joystick-place joycon-joystick-hole)
;;                    ;(color [1 0 0 1](joycon-joystick-place joycon-joystick-case))
;;                    (if joycon-joystick-enabled (color [1 0 1 1] (joycon-joystick-place (difference joycon-joystick-top-right-screw-mount joycon-joystick-top-right-screw-mount-hole))))
;;                    (if joycon-joystick-enabled (color [1 0 1 1] (joycon-joystick-place (difference joycon-joystick-bottom-left-screw-mount joycon-joystick-bottom-left-screw-mount-hole))))
;;                    ;(color [0 1 1 1] (joycon-joystick-place joycon-joystick-top-right-screw-mount-hole))
;;                    ;(color [0 1 1 1] (joycon-joystick-place joycon-joystick-bottom-left-screw-mount-hole))
;;                  ;  (if joycon-joystick-enabled (color [0 0 1 1] (joycon-joystick-place joycon-joystick-top-right-screw-mount-hole-position-test)))
;;                  ;  (if joycon-joystick-enabled (color [0 0 1 1] (joycon-joystick-place joycon-joystick-bottom-left-screw-mount-hole-position-test)))

;;                    ;(joycon-joystick-place joycon-joystick-screw-mount-corner-rounding-mask)
;;                    ;joycon-joystick-screw-mount-corner-rounding-mask


;;                    ;(color [0 0 1 1] joycon-joystick-case-cover)


;;                    ;(color [1 0 0 1]oled-clip-mount-frame-shape)
;;                    ;(OLED -65 -6 99)
;;                    (difference (union case-walls
;;                                       ;(color [1 0 0 1] usb-holder)
;;                                       screw-insert-outers)
;;                                (if trackball-enabled trackball-walls nil)
;;                                ; Leave room to insert the ball
;;                                (if joycon-joystick-enabled (joycon-joystick-place joycon-joystick-top-right-screw-mount-hole))
;;                                (if trackball-enabled (translate trackball-origin trackball-insertion-cyl) nil)
;;                                (cirque-TM040040-place (translate [0 0 0] cirque-TM040040-clearance))
;;                                (translate palm-hole-origin (palm-rest-hole-rotate palm-buckle-holes))
;;                                usb-holder
;;                                usb-holder-extra-clearance
;;                                dovetail-clearance-left
;;                                dovetail-clearance-right
;;                                ;usb-holder-space
;;                               ; trrs-notch
;;                                ;usb-holder-notch
;;                                screw-insert-holes))
;;                   ;(println str "this is " (first usb-holder-position))
;;                   (if trackball-enabled (translate trackball-origin (dowell-angle raised-trackball)) nil)
;;                   ;(joycon-joystick-place joycon-joystick-bottom-left-screw-mount-clearance)
;;                   ;(cirque-place cirque-clearance)
;;                   ;(cirque-place cirque-connector-clearance)
;;                   (cirque-TM040040-place (translate [0 0 cirque-circle-trackpad-depth-to-asic-plus-tolerance] cirque-TM040040-clearance))
;;                   (cirque-TM040040-place (translate [0 0 2] cirque-TM040040-connector-clearance))
;;                   (cirque-TM040040-place (translate [0 0 (- (+ cirque-TM040040-mount-height 4))]cirque-TM040040-under-clearance))

;;                   usb-holder-hole
                  
;;                   aviator-hole
;;                   reset-hole
;;                   encoder-cutout
;;                   (encoder-place (translate [0 0 -3] (cube 10 10 12)))
;;                   (encoder-place (translate [0 0 1] (cube 14 14 14)))
;;                   (color [0 1 1 1] upper-toggle-switch-cover-base-cutout)
;;                   (color [0 0 1 1] lower-toggle-switch-cover-base-cutout )
;;                   (color [1 0 0 1] upper-toggle-switch-hole)
;;                   lower-toggle-switch-hole
;;                   ;(cirque-place asic-cutout)
;;                   ;(cirque-place (translate [0 0 2]asic-cutout))
;;                   (if joycon-joystick-enabled (joycon-joystick-place (translate [0 0 2] joycon-joystick-hole)))
;;                   (left-wall-plate-place 0 0 oled-holder-cut)
                   




;;                    ;( translate [0 0 (- (- oled-holder-thickness) 0.1)]
;;                    ; (left-wall-plate-place oled-x-position oled-y-position oled-holder-cut))
;;                   (translate [0 0 -20] (cube 350 350 40))))

;; (def cirque-TM040040-mount-test
;;   (difference (union cirque-TM040040-mount)
;;               (translate [0 0 cirque-circle-trackpad-depth-to-asic-plus-tolerance] cirque-TM040040-clearance)
;;               (translate [0 0 2] cirque-TM040040-connector-clearance)
;;               ;(translate [0 0 0] asic-cutout)
;;               ;(translate [0 0 (- asic-depth)] asic-cutout)
;;               ))

;; (def joycon-test
;;   (mirror [-1 0 0] (difference
;;                     (union
;;                      (translate [0 0 2.5] (cube 25 27 5))
;;                      joycon-joystick-top-right-screw-mount
;;                      joycon-joystick-bottom-left-screw-mount)
;;                     (translate [0,0,-1] joycon-joystick-top-right-screw-mount-clearance)
;;                     joycon-joystick-case
;;                     joycon-joystick-case-clearance
;;                     joycon-joystick-bottom-left-screw-mount-clearance
;;                     joycon-joystick-top-right-screw-mount-hole
;;                     joycon-joystick-bottom-left-screw-mount-hole
;;                     (translate [0 0 2] joycon-joystick-hole))))

;; (def encoder-hole-test (difference (cube 12 12 plate-thickness)
;;                                    encoder-cutout-shape)
;;   )

;; (def reset-hole-test 
;;   (difference (cube 10 10 plate-thickness)
;;                                  (binding [*fn* 36] (cylinder [(/ resetswitch-diameter 2) (/ resetswitch-diameter 2)] 20))))

;; (def oled-test (difference oled-holder oled-holder-cut))

;; (def aviator-test
;;   (difference
;;    (cube 17 17 plate-thickness)
;;    (binding [*fn* 36] (cylinder (/ aviator-diameter 2) plate-thickness))))

;; (def model-left (union (mirror [-1 0 0] model-right)))

;; (def right-test (union model-right
;;                        thumbcaps-type
;;                        caps))

;; (def cf-right-plate
;;   (extrude-linear
;;    {:height 2.6 :center false}
;;    (project
;;     (difference
;;      (union
;;       dovetail-clearance-left
;; dovetail-clearance-right
;; usb-holder-extra-clearance
;;       key-holes
;;       key-holes-inner
;;       pinky-connectors
;;       extra-connectors
;;       connectors
;;       inner-connectors
;;       thumb-type
;;       thumb-connector-type
;;       case-walls
;;       thumbcaps-fill-type
;;       caps-fill
;;       screw-insert-outers
;;      (if trackball-enabled  (color [1 0 0 1] plate-trackball-hole-healer) nil)
;;       )
;;      (translate [0 0 -10] screw-insert-screw-holes)))))

;; (def right-plate
;;   (difference

;;                    ;(translate [0.75 -0.75 -2] usb-holder)
;;                    ;(translate [4.5 -0.75 1.5] usb-holder)
;;    (union
    
;;     (if trackball-enabled trackball-mount-translated-to-model nil)
;;     (translate thumb-tent-origin tent-nut)
;;     (translate index-tent-origin tent-nut)
;;     (translate [-30 -20 4] IS31FL3731-screw-mounts-set)
;;    ; (translate [(+ (- IS31FL3731-width) -90 -10) 0 4] IS31FL3731-screw-mounts-set)
;;    ;; (translate [IS31FL3731-bottom-left])
;;      cf-right-plate)
;;                    ;(translate [0 0 (/ bottom-plate-thickness -2)] plate-attempt)
;;                    ;key-holes
;;                    ;key-holes-inner
;;                    ;pinky-connectors
;;                    ;extra-connectors
;;                    ;connectors
;;                    ;inner-connectors
;;                    ;thumb-type
;;                    ;thumb-connector-type
;;                    ;case-walls
;;                    ;thumbcaps-fill-type
;;                    ;caps-fill
;;                    ;screw-insert-outers
;;                     ;(translate [0 0 -10] screw-insert-screw-holes)

;;    (if trackball-enabled key-trackball-clearance nil)
;;    (translate thumb-tent-origin tent-thread)
;;    (translate index-tent-origin tent-thread)
;;    (translate [0 0 -20] (cube 350 350 40))
;;                  ; mini-thumb-wall
;;   ;(union
;;   ; (translate [-0.75 -0.75 -2] usb-holder)
;;   ; (translate [0.75 -0.75 -2] usb-holder)
;;   ; (translate [4.5 -0.75 2] usb-holder))
;;    ))

;; (def right-plate-new
;;   (cut model-right ))

;; (def right-hands-on-test
;;   (difference
;;    (union
;;     hand-on-test
;;     (color [220/255 120/255 120/255 1] hotswap-tester)
;;     (color [220/255 163/255 163/255 1] right-plate)
;;     model-right
;;     (translate (map + palm-hole-origin [0 (+ buckle-length 3) (/ buckle-height 2)])
;;                (palm-rest-hole-rotate palm-rest))
;; ;         (if trackball-enabled (translate trackball-origin test-ball) nil)
;;     thumbcaps
;;     caps)

;;    (translate [0 0 -20] (cube 350 350 40)))
;;   )

;; (def right-test
;;   (difference
;;    (union
;;          ;hand-on-test
;;     (color [220/255 120/255 120/255 1] hotswap-tester)
;;     (color [220/255 163/255 163/255 1] right-plate)
;;     model-right
;;          ;(translate (map + palm-hole-origin [0 (+ buckle-length 3) (/ buckle-height 2)])
;;          ;           (palm-rest-hole-rotate palm-rest))
;; ;         (if trackball-enabled (translate trackball-origin test-ball) nil)
;;     thumbcaps-type
;;     caps)

;;    (translate [0 0 -20] (cube 350 350 40)))
;;   )

;; (def controller-holder-slot-test
;;  (difference (union
;;               (for [x (range 0 2)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
;;               (for [x (range 1 3)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr)))
;;              (scale [1 1 1] usb-holder)
;;              dovetail-clearance-left
;;              dovetail-clearance-right
;;              usb-holder-extra-clearance
;;              (translate [(first usb-holder-position) (second usb-holder-size) 46.5] (cube 40 15 60))
;;              (translate [(first usb-holder-position) (+ (second usb-holder-size) 3) -10] (cube 40 15 20)))
;; )

;; ;https://github.com/jaredjennings/dactyl-keyboard/blob/cd43dfd01e5847cc262664da073d67bb9cae5617/src/dactyl_keyboard/dactyl.clj
;; (def case-right
;;   (let [simplified (union
;;                     key-holes
;;                     connectors
;;                     thumb
;;                     thumb-connectors
;;                     case-walls
;;                     screw-insert-outers
;;                     usb-holder
;;                     ; wire-posts

;;                     thumbcaps
;;                     caps)
;;         ;; determined by trial and error; not tight. big enough for 6x6
;;         bounding-cube-size [240 260 120]
;;         bottom-allowance 3
;;         ;; x and y determined by trial and error
;;         bounding-cube (translate [-17 -32 (-
;;                                            (* 1/2 (bounding-cube-size 2))
;;                                            bottom-allowance)]
;;                                  (apply cube bounding-cube-size))
;;         ;; make the hole some constant amount bigger than the object
;;         ;; in each dimension. this probably only works with a convex
;;         ;; hole.
;;         add-scale (fn [plus bounding-cube-size]
;;                     (map #(/ (+ % plus) %) bounding-cube-size))
;;         hole-add 4
;;         skin-add 14
;;         hole-scale (add-scale hole-add bounding-cube-size)
;;         skin-scale (add-scale skin-add bounding-cube-size)
;;         the-hull (hull simplified
;;                        (translate [0 0 -100]
;;                                   simplified))
;;         keyboard-space (scale hole-scale the-hull)
;;         skin (scale skin-scale the-hull)
;;         nudge #(translate [(* 0.3 hole-add) (* 0.3 hole-add) 0] %)]
;;     (intersection
;;      (difference (nudge skin)
;;                  keyboard-space
;;                  ;(translate rj9-position rj9-socket-in-holder))
;;                  ;; a core taken out to find the right z for the
;;                  ;; bounding cube, by rendering + TLAR method
;;                  ;; (translate [-50 -30 0] (cube 10 10 400)))
;;      bounding-cube))))

;; (spit "things/hole-right.scad"
;;       (write-scad case-right))

;; (spit "things/dactyl-and-case-test.scad"
;;       (write-scad (union case-right model-right)))

;; (spit "things/dovetail.scad"
;;       (write-scad dovetail-shape))

;; (spit "things/toggle-switch.scad"
;;       (write-scad toggle-switch))

;; (spit "things/toggle-test.scad"
;;       (write-scad toggle-test))

;; (spit "things/IS31FL3731-screw-mounts-test.scad"
;;       (write-scad IS31FL3731-screw-mounts-test))

;; (spit "things/cirque-test.scad"
;;       (write-scad cirque-test))

;; (spit "things/plate-trackball-hole-healer.scad"
;;       (write-scad plate-trackball-hole-healer))


;; (spit "things/right.scad"
;;        ;(include "../nutsnbolts/cyl_head_bolt.scad")
;;       (write-scad model-right))

;;       (spit "things/cirque-TM040040-mount-test.scad"
;;             (write-scad cirque-TM040040-mount-test))


;; (spit "things/controller-holder-test.scad"
;;        ;(include "../nutsnbolts/cyl_head_bolt.scad")
;;       (write-scad (difference (union (color [1 0 0 1]usb-holder)
                                    
;;                          ;(translate [(first usb-holder-position) (+ (second usb-holder-size) 1.5) 6] (cube 30 3 12))
                                     
;;                                      )
;;                               usb-holder-hole)
;;                   ))

;; (spit "things/controller-holder-to-be-glued.scad"
;;        ;(include "../nutsnbolts/cyl_head_bolt.scad")
;;       (write-scad (difference (union (color [1 0 0 1] usb-holder)

;;                          ;(translate [(first usb-holder-position) (+ (second usb-holder-size) 1.5) 6] (cube 30 3 12))
;;                                      )
;;                               usb-holder-to-glue-to-left
;;                               usb-holder-hole
;;                               )))

;; (spit "things/controller-holder-slot-test.scad"
;; (write-scad controller-holder-slot-test)
;;       )


;; (spit "things/controller-holder-with-holder-slot-test.scad"
;;       (write-scad (difference (union 
;;                                (color [1 0 0 1] usb-holder)
;;                          controller-holder-slot-test
;;                                      )
;;                               usb-holder-hole)))

;; (spit "things/joycon-test.scad" (write-scad joycon-test))

;; (spit "things/encoder-hole-test.scad"
;;       (write-scad encoder-hole-test))

;; (spit "things/reset-hole-test.scad"
;;       (write-scad reset-hole-test))

;; (spit "things/oled-test.scad"
;;     (write-scad oled-test))

;; (spit "things/aviator-test.scad"
;; (write-scad aviator-test ))


;; (spit "things/left.scad"
;;       (write-scad model-left))
;;                          ;(joystick-position joystick)
;;                          ;(joystick-position joystick-cutout)
;;                          ;(translate [ 0 35 0 ] (rotate (deg2rad -160) [0 0 1] joycon-joystick-case-mount))
                        

;; (spit "things/right-test.scad"
;;       (write-scad right-test))







;;                   ; model-right
   

;; (spit "things/right-plate.scad"
;;       (write-scad
;;        (include "../nutsnbolts/cyl_head_bolt.scad")
;;        right-plate))

;; (spit "things/right-plate-new.scad" 
;;       (write-scad right-plate-new))

;; (spit "things/right-hands-on-test.scad"
;;       (write-scad
;;        (include "../nutsnbolts/cyl_head_bolt.scad")
;;        right-hands-on-test
;;        ))

;; (spit "things/right-test.scad"
;;       (write-scad
;;        (include "../nutsnbolts/cyl_head_bolt.scad")
;;        right-test))

;; (spit "things/3d-Printed-PCB.scad"
;;       (write-scad three-D-printed-pcb
;;        ))

;; (spit "things/joycon-joystick-support.scad"
;;       (write-scad (mirror [-1 0 0] joycon-joystick-support)
;;                   ))

;; (spit "things/left-plate.scad"
;;       (write-scad
;;        (include "../nutsnbolts/cyl_head_bolt.scad")
;;        (mirror [-1 0 0] right-plate)))

;; (spit "things/tent-nut.scad" (write-scad
;;                               (include "../nutsnbolts/cyl_head_bolt.scad")
;;                               tent-nut))

;; (spit "things/tent-foot.scad"
;;       (write-scad tent-foot))

;; (spit "things/tent-stand.scad"
;;       (write-scad
;;        (include "../nutsnbolts/cyl_head_bolt.scad")
;;        tent-stand))

;; (spit "things/pimoroni-haptic-mount.scad"
;;       (write-scad pimoroni-haptic-mount))


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
