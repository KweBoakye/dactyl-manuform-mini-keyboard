(ns dactyl-keyboard.tps-65
(:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [magnitude]]
            [dactyl-keyboard.lib.openscad.bosl2-wrappers.shapes-3d :refer [cuboid]]
            [dactyl-keyboard.lib.transformations :refer [rdx rdz]]
            [dactyl-keyboard.utils :refer :all]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all])
)
 (def tps-65-includes  (map  include ["../BOSL/shapes.scad" "../BOSL/constants.scad" "../BOSL/masks.scad"]))
 (def tps-65-depth 2.03)
  (def tps-65-width 65.00)
(def tps-65-length 49.00)
 (def tps-65-mount-width (+ tps-65-width 2))
 (def tps-65-mount-length (+ tps-65-length 2))
 (def tps-65-tolerance 0.2)
 (def tps-65-cutout-tolerance 0.4)
 (def tps-65-depth-tolerance (/ tps-65-depth 10))
 (def tps-65-depth-with-tolerance  (+ tps-65-depth tps-65-depth-tolerance tps-65-depth-tolerance))
 (def tps-65-width-with-tolerance  (+ tps-65-mount-width tps-65-tolerance 0.8))
 (def tps-65-length-with-tolerance (+ tps-65-mount-length tps-65-tolerance 0.8))
 (def tps-65-depth-with-negative-tolerance  (- tps-65-depth tps-65-depth-tolerance 0.2))
 (def tps-65-width-with-negative-tolerance  (- tps-65-mount-width tps-65-tolerance 0.2))
 (def tps-65-length-with-negative-tolerance (- tps-65-mount-length tps-65-tolerance 0.2))
 (def tps-65-trackpad-only-thickness 1.25)
 (def tps-65-overlay-thickness 0.5)
 (def tps-65-connecter-height(- (+ tps-65-depth tps-65-depth-tolerance) tps-65-trackpad-only-thickness))
 (def tps-65-connecter-cutout-width 9.5)
 (def tps-65-connecter-cutout-length 15.14)
 (def tps-65-component-cutout-area-length 22.5)
 (def tps-65-component-cutout-area-width 22.5)
 (def tps-65-component-length-from-short-side-to-connecter-cutout  20.5)
 (def tps-65-component-length-from-long-side-to-connecter-cutout  33.5)
 (def tps-65-component-length-from-top-to-connecter-cutout  8.06)
 (def tps-65-component-length-from-bottom-to-connecter-cutout 33.2)
 (def tps-65-component-length-from-side-to-component-cutout  22.0)
 (def tps-65-component-length-from-bottom-to-component-cutout 19.0)
 (def tps-65-0603-resistor-height 0.5)
 (def tps-65-component-cutout-area-depth (+ tps-65-0603-resistor-height 0.2));(+ (/ tps-65-trackpad-only-thickness 2)tps-65-0603-resistor-height))
 (def tps-65-corner-radius 3.5)
 (def tps-65-z-modifier 0.1)
 (def tps-65-y-modifier 1)
 (def tps-65-mount-corner-radius tps-65-corner-radius)
 (def tps-65-mount-corner-radius-with-offset (+ tps-65-mount-corner-radius 1))
 (def tps-65-radius-compensation 0.5)

 ;(def tps-65-corner-cutout )
 (def tps-65-overlay-corner-cylinder 
  (binding [*fn* 100](cylinder tps-65-corner-radius tps-65-overlay-thickness :center true)) )
 

 (def tps-65-overlay-corner-cylinder-top-left 
   (translate [(- tps-65-corner-radius (/ tps-65-width-with-negative-tolerance 2) )
               (- (/ tps-65-length-with-negative-tolerance 2) tps-65-corner-radius)
               0
               ] tps-65-overlay-corner-cylinder))

(def tps-65-overlay-corner-cylinder-top-right
  (translate [(- (/ tps-65-width-with-negative-tolerance 2) tps-65-corner-radius )
              (- (/ tps-65-length-with-negative-tolerance 2) tps-65-corner-radius)
              0] tps-65-overlay-corner-cylinder))

(def tps-65-overlay-corner-cylinder-bottom-left
  (translate [(- tps-65-corner-radius (/ tps-65-width-with-negative-tolerance 2))
              (- tps-65-corner-radius (/ tps-65-length-with-negative-tolerance 2) )
              0] tps-65-overlay-corner-cylinder))

(def tps-65-overlay-corner-cylinder-bottom-right
  (translate [(- (/ tps-65-width-with-negative-tolerance 2) tps-65-corner-radius)
              (- tps-65-corner-radius (/ (- tps-65-length-with-negative-tolerance tps-65-tolerance) 2))
              0] tps-65-overlay-corner-cylinder))
 
  (def tps-65-overlay
    (union
     (hull
    ;(cube tps-65-mount-width tps-65-mount-length tps-65-overlay-thickness)
     tps-65-overlay-corner-cylinder-top-left
     tps-65-overlay-corner-cylinder-top-right
     tps-65-overlay-corner-cylinder-bottom-left
     tps-65-overlay-corner-cylinder-bottom-right)
     (color [1 0 0 1] (rdx 90 (import "reform2-trackpad-support-20201005.stl")) )
     ))



 
 (def tps-65-mount-corner-cylinder
   (binding [*fn* 100] (cylinder tps-65-mount-corner-radius (+ tps-65-trackpad-only-thickness tps-65-depth-tolerance tps-65-overlay-thickness) :center true)))
  
  (def tps-65-cutout-corner-cylinder
    (binding [*fn* 100] (cylinder tps-65-mount-corner-radius (+ tps-65-trackpad-only-thickness tps-65-depth-tolerance) :center true)))

(def tps-65-mount-post
  (cube tps-65-mount-corner-radius tps-65-mount-corner-radius 2))

(def tps-65-mount-corner-half-cylinder
  (difference
   (binding [*fn* 100] (cylinder tps-65-mount-corner-radius 2 :center true))
   (translate [0 (- tps-65-mount-corner-radius) -0.5]
              (cube (* tps-65-mount-corner-radius 2) (* tps-65-mount-corner-radius 2) (+ 2 (* 0.2 2))))))

(def tps-65-mount-corner-quarter-cylinder
  (difference
   tps-65-mount-corner-half-cylinder
   (rdz -90 tps-65-mount-corner-half-cylinder)))

(def tps-65-mount-corner-cylinder-top-left-position
   [(- tps-65-corner-radius (/ tps-65-mount-width 2)),
              (- (/ tps-65-mount-length 2) tps-65-corner-radius),
              0] )

(def tps-65-mount-corner-cylinder-top-right-position
   [(- (/ tps-65-mount-width 2) tps-65-corner-radius),
              (- (/ tps-65-mount-length 2) tps-65-corner-radius),
              0] )

(def tps-65-mount-corner-cylinder-top-mid-position
  [0,
   (- (/ tps-65-mount-length 2) tps-65-corner-radius),
   0])

(def tps-65-mount-corner-cylinder-bottom-mid-position
  [0,
   (- tps-65-corner-radius (/ tps-65-mount-length 2)),
   0]) 

(def tps-65-mount-corner-cylinder-bottom-left-position
   [(- tps-65-corner-radius (/ tps-65-mount-width 2)),
              (- tps-65-corner-radius (/ tps-65-mount-length 2)),
              0] )

(def tps-65-mount-corner-cylinder-bottom-right-position
   [(- (/ tps-65-mount-width 2) tps-65-corner-radius),
              (- tps-65-corner-radius (/ tps-65-mount-length 2)),
              0] )
(def tps-65-mount-corner-cylinder-right-mid-position
  [(- (/ tps-65-mount-width 2) tps-65-corner-radius),
   0,
   0]
  )

(def tps-65-mount-corner-cylinder-left-mid-position
  [(- tps-65-corner-radius (/ tps-65-mount-width 2)),
      0,
      0])

(def tps-65-corner-cylinder-top-left-position
  [(- tps-65-corner-radius (/ tps-65-width 2)),
   (- (/ tps-65-length 2) tps-65-corner-radius),
   0])

(def tps-65-corner-cylinder-top-right-position
  [(- (/ tps-65-width 2) tps-65-corner-radius),
   (- (/ tps-65-length 2) tps-65-corner-radius),
   0])


(def tps-65-corner-cylinder-bottom-left-position
  [(- tps-65-corner-radius (/ tps-65-width 2)),
   (- tps-65-corner-radius (/ tps-65-length 2)),
   0])

(def tps-65-corner-cylinder-bottom-right-position
  [(- (/ tps-65-width 2) tps-65-corner-radius),
   (- tps-65-corner-radius (/ tps-65-length 2)),
   0])

(def tps-65-cutout-corner-cylinder-top-left-position
  [(- tps-65-corner-radius (/ tps-65-width 2) tps-65-cutout-tolerance),
   (- (+ (/ tps-65-length 2) tps-65-cutout-tolerance) tps-65-corner-radius),
   0])

(def tps-65-cutout-corner-cylinder-top-right-position
  [(- (+ (/ tps-65-width 2) tps-65-cutout-tolerance) tps-65-corner-radius),
   (- (+ (/ tps-65-length 2) tps-65-cutout-tolerance) tps-65-corner-radius),
   0])


(def tps-65-cutout-corner-cylinder-bottom-left-position
  [(- tps-65-corner-radius (/ tps-65-width 2) tps-65-cutout-tolerance),
   (- tps-65-corner-radius (/ tps-65-length 2) tps-65-cutout-tolerance),
   0])

(def tps-65-cutout-corner-cylinder-bottom-right-position
  [(- (+ (/ tps-65-width 2) tps-65-cutout-tolerance) tps-65-corner-radius),
   (- tps-65-corner-radius (/ tps-65-length 2) tps-65-cutout-tolerance),
   0])


 
 (def tps-65-mount-corner-cylinder-top-left
   (translate tps-65-mount-corner-cylinder-top-left-position tps-65-mount-corner-cylinder))

(def tps-65-mount-corner-cylinder-top-right
  (translate tps-65-mount-corner-cylinder-top-right-position tps-65-mount-corner-cylinder))

(def tps-65-mount-corner-cylinder-bottom-left
  (translate tps-65-mount-corner-cylinder-bottom-left-position tps-65-mount-corner-cylinder))

(def tps-65-mount-corner-cylinder-bottom-right
  (translate tps-65-mount-corner-cylinder-bottom-right-position tps-65-mount-corner-cylinder))

 (def tps-65-corner-cylinder-top-left
   (translate tps-65-corner-cylinder-top-left-position tps-65-mount-corner-cylinder))

(def tps-65-corner-cylinder-top-right
  (translate tps-65-corner-cylinder-top-right-position tps-65-mount-corner-cylinder))

(def tps-65-corner-cylinder-bottom-left
  (translate tps-65-corner-cylinder-bottom-left-position tps-65-mount-corner-cylinder))

(def tps-65-corner-cylinder-bottom-right
  (translate tps-65-corner-cylinder-bottom-right-position tps-65-mount-corner-cylinder))

(def tps-65-cutout-corner-cylinder-top-left
  (translate tps-65-cutout-corner-cylinder-top-left-position tps-65-mount-corner-cylinder))

(def tps-65-cutout-corner-cylinder-top-right
  (translate tps-65-cutout-corner-cylinder-top-right-position tps-65-mount-corner-cylinder))

(def tps-65-cutout-corner-cylinder-bottom-left
  (translate tps-65-cutout-corner-cylinder-bottom-left-position tps-65-mount-corner-cylinder))

(def tps-65-cutout-corner-cylinder-bottom-right
  (translate tps-65-cutout-corner-cylinder-bottom-right-position tps-65-mount-corner-cylinder))
 
 (def tps-65-mount-main-cutout 
   (->>
    (hull
    tps-65-cutout-corner-cylinder-top-left
    tps-65-cutout-corner-cylinder-top-right
    tps-65-cutout-corner-cylinder-bottom-left
    tps-65-cutout-corner-cylinder-bottom-right
    )
    (translate [0 0 (/ (+ tps-65-trackpad-only-thickness tps-65-depth-tolerance tps-65-overlay-thickness) -2)])
    ) 
   )

(def tps-65-mount-main-cutout-smaller
  (->>
   (hull
    tps-65-corner-cylinder-top-left
    tps-65-corner-cylinder-top-right
    tps-65-corner-cylinder-bottom-left
    tps-65-corner-cylinder-bottom-right)
   (translate [0 0 (/ (+ tps-65-trackpad-only-thickness tps-65-depth-tolerance tps-65-overlay-thickness) -2)])))

(def tps-65-component-cutout
  (->>
   (cube tps-65-component-cutout-area-width tps-65-component-cutout-area-length tps-65-component-cutout-area-depth)
   (translate [(+ (- (/ tps-65-width-with-tolerance 2)) tps-65-component-length-from-short-side-to-connecter-cutout (/ tps-65-component-cutout-area-width 2))
               (- (/ tps-65-length-with-tolerance 2) tps-65-component-length-from-top-to-connecter-cutout (/ tps-65-component-cutout-area-length 2))
               (- (+ (/ tps-65-component-cutout-area-depth 2) (/ tps-65-trackpad-only-thickness 2)))])))


 
 (def tps-65-connecter-cutout
   (->> 
    (cube tps-65-connecter-cutout-width tps-65-connecter-cutout-length tps-65-connecter-height)
    (translate [(+ (- (/ tps-65-width-with-tolerance 2)) tps-65-component-length-from-short-side-to-connecter-cutout (/ tps-65-connecter-cutout-width 2))
                (- (/ tps-65-length-with-tolerance 2) tps-65-component-length-from-top-to-connecter-cutout (/ tps-65-connecter-cutout-length 2))
                (- (+ (/ tps-65-component-cutout-area-depth 2) (/ tps-65-trackpad-only-thickness 2) (/ tps-65-connecter-height 2)))])
    ))

(def ct
  (->>
   (cube (- tps-65-width (* tps-65-mount-corner-radius 2)) (- tps-65-length (* tps-65-mount-corner-radius 2)) (+ tps-65-depth tps-65-depth-tolerance))
   (translate [0 0 (- (/ (+ tps-65-depth tps-65-depth-tolerance) -2) 0.5)])))
(def tps-65-mount-cutout
  (->>(union
   tps-65-mount-main-cutout    
   ;tps-65-component-cutout
   ;tps-65-connecter-cutout
   ;(translate [0 0 (- 0.2 tps-65-connecter-height)] tps-65-connecter-cutout)
       (translate [0 0 -1] ct)
       )
   (translate [0 0 0])
   ))
 
 (def test-base 
   (->>     
    (cube (+ tps-65-width-with-tolerance 2) (+  tps-65-length-with-tolerance 2) tps-65-depth-with-tolerance)
    (translate [0 0 (- (/ tps-65-depth-with-tolerance 2))])
    ))
 (def tps-65-mount-test
   (union 
    test-base
    (translate [0 0 (- (/  tps-65-trackpad-only-thickness 2))] tps-65-mount-cutout)
    )
   )
 (defn tps-radius-compensation-adjust [radius-compensation]
   (if  (pos? radius-compensation) (+ radius-compensation tps-65-radius-compensation) (- radius-compensation tps-65-radius-compensation)))
 
 (defn tps-radius-compensation-adjust-reverse [radius-compensation]
   (if  (pos? radius-compensation)  (- radius-compensation tps-65-radius-compensation) (+ radius-compensation tps-65-radius-compensation)))
 
 (def tps-65-mount-base 
   (->> (let [corner (square tps-65-corner-radius tps-65-corner-radius) half-radius (/ tps-65-corner-radius 2)]
          (hull 
          (translate (map + tps-65-mount-corner-cylinder-top-left-position [(tps-radius-compensation-adjust (- half-radius)) (tps-radius-compensation-adjust half-radius) 0]) corner)
          (translate (map + tps-65-mount-corner-cylinder-top-right-position [(tps-radius-compensation-adjust half-radius) (tps-radius-compensation-adjust half-radius) 0]) corner)
          (translate (map + tps-65-mount-corner-cylinder-bottom-left-position  [(tps-radius-compensation-adjust (- half-radius)) (tps-radius-compensation-adjust (- half-radius)) 0 ]) corner)
          (translate (map + tps-65-mount-corner-cylinder-bottom-right-position [(tps-radius-compensation-adjust half-radius) (tps-radius-compensation-adjust (- half-radius)) 0]) corner)
           ;(translate tps-65-mount-corner-cylinder-top-left-position corner)
           ;(translate tps-65-mount-corner-cylinder-top-right-position corner)
           ;(translate tps-65-mount-corner-cylinder-bottom-left-position corner)
           ;(translate tps-65-mount-corner-cylinder-bottom-right-position corner)
           )
          ))
   )
 
 (def tps-65-mount-new
   (let [corner  [tps-65-corner-radius tps-65-corner-radius 0] half-radius (/ tps-65-corner-radius 2)
         tl (mapv + tps-65-mount-corner-cylinder-top-left-position [(tps-radius-compensation-adjust (- half-radius)) (tps-radius-compensation-adjust half-radius) 0] [(- half-radius) half-radius 0])
         tr (mapv + tps-65-mount-corner-cylinder-top-right-position [(tps-radius-compensation-adjust half-radius) (tps-radius-compensation-adjust half-radius) 0] [half-radius half-radius 0])
         bl (mapv + tps-65-mount-corner-cylinder-bottom-left-position  [(tps-radius-compensation-adjust (- half-radius)) (tps-radius-compensation-adjust (- half-radius)) 0] [(- half-radius) (- half-radius) 0])
         br (mapv + tps-65-mount-corner-cylinder-bottom-right-position [(tps-radius-compensation-adjust half-radius) (tps-radius-compensation-adjust (- half-radius)) 0] [half-radius (- half-radius) 0]) 
         x (magnitude (mapv - tr tl))
         y (magnitude (mapv - tl bl))
         z (+ tps-65-y-modifier tps-65-depth tps-65-depth-tolerance)
         ] 
     (->>
      (cuboid [x y z] :chamfer 1 )
      (translate [0 0 (+ (- (+ tps-65-depth tps-65-depth-tolerance)) 0.6)])
      )
     )
   )
 
 
 
 (def tps-65-mount-body
    (->> (hull
          (for [i [0 0.25 0.5 0.75 1]
                :let [y (* (Math/cos (* i 90)) tps-65-y-modifier) x  (+ -0.95 (* (Math/sin (* i 90)) 1))]]

            (extrude-linear {:height (+ (- (+ tps-65-depth tps-65-depth-tolerance) tps-65-y-modifier) y) :center false}
                            (offset-delta {:delta x  :chamfer false :r false} tps-65-mount-base))))
        ;(translate [0 0 (- (/ (+ tps-65-depth tps-65-depth-tolerance) 2))])
         )
   )
 
 

  (def tps-65-mount 
    
    (->> (union 
          tps-65-mount-body
          (translate [0 0 -0.5] (extrude-linear {:height 0.5 :center false} tps-65-mount-base)))
    ;(call-module-with-block "fillet" {:fillet tps-65-corner-radius :size [(+ (- tps-65-mount-width (* tps-65-corner-radius 2)) (* tps-65-mount-corner-radius-with-offset 2)) 
    ;                       (+ (- tps-65-mount-length (* tps-65-corner-radius 2)) (* tps-65-mount-corner-radius-with-offset 2)) 
    ;                       tps-65-depth-with-tolerance] :edges "EDGES_Z_ALL" :$fn 24}
    ; 
    ; (call-module "cuboid" [(+ (- tps-65-mount-width (* tps-65-corner-radius 2)) (* tps-65-mount-corner-radius-with-offset 2)) 
    ;                       (+ (- tps-65-mount-length (* tps-65-corner-radius 2)) (* tps-65-mount-corner-radius-with-offset 2)) 
    ;                       tps-65-depth-with-tolerance] {:chamfer 1 :edges "EDGES_TOP" :$fn 24}))
     
     ;(translate [0 0 (- (/ (+ tps-65-depth tps-65-depth-tolerance) 2))] )
     (translate [0 0(- (+ tps-65-depth tps-65-depth-tolerance))])
     ))
  
  

  
 (def tps-65-base
   (->> 
    (cube (+ tps-65-mount-width tps-65-tolerance) (+ tps-65-mount-length tps-65-tolerance) (+ tps-65-depth tps-65-depth-tolerance))
    (translate [0 0 (-(/ (+ tps-65-depth tps-65-depth-tolerance) 2))])
 ))

 (def tps-65 
   (hull 
    ;tps-65-base
    tps-65-overlay-corner-cylinder-top-left
tps-65-overlay-corner-cylinder-top-right
tps-65-overlay-corner-cylinder-bottom-left
tps-65-overlay-corner-cylinder-bottom-right
    ))
 
 (def tps-65-mount-corner-radius-mod (+ tps-65-mount-corner-radius 0))
(def tps-65-mount-corner-radius-with-offset-mod (+ tps-65-mount-corner-radius-with-offset 2))
(def tps-65-mount-corner-radius-with-offset-mod-neg (- tps-65-mount-corner-radius-with-offset 0.5))
