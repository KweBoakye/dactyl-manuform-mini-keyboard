(ns dactyl-keyboard.klor.klor-points)

(def tolerance 0.5)
(def extra-pinky-top-left-corner [213.627717 86.501225])
(def extra-pinky-top-right-corner [231.186547 90.273904] )
(def extra-pinky-bottom-right-corner [224.452135 128.3533])
(def extra-pinky-bottom-left-corner [206.646493 125.88519])
(def pinky-top-inner-corner [210.595467 71.622056])
(def pinky-top-right-corner   [215.038063 72.715076])
(def pinky-bottom-left-corner  [184.292479 126.343555])
(def pinky-bottom-right-corner  [204.178385 130.504079])
(def pcb-top-right-corner [210.595468 50.255285])
(def pcb-top-left-corner  (mapv + [80.984498 50.220026] [-0.5 -0.5]))
(def pcb-bottom-left-corner-before-bottom-left-thumb [80.984498 133.430551] )
(def pcb-bottom-inner-left-corner-before-bottom-left-thumb  [89.199773 134.135726])

(def usb-c-cutout-bottom-left-corner [92.196762 56.989695])
 (def usb-c-cutout-bottom-right-corner [102.457044 56.989694])
(def usb-c-cutout-top-left-corner [92.196762 49.514851])
(def usb-c-cutout-top-right-corner [102.457044 49.514851])



(def thumb-right-inner-corner [153.828964 121.795181])
(def bottom-left-thumb-key-bottom-right-corner [98.331777 151.941367])
(def bottom-left-thumb-key-bottom-left-corner [85.321318 163.541477] )
 (def bottom-left-thumb-key-bottom-mid-point [93.148749 156.666031])
(def bottom-left-thumb-key-top-left-corner [71.46465 148.697567])
 (def bottom-left-thumb-key-top-right-corner [81.019757 139.177719])
 
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


