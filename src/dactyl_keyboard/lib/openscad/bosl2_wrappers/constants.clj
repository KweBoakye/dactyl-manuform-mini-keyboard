(ns dactyl-keyboard.lib.openscad.bosl2-wrappers.constants)

(def include-bosl2  "../BOSL2/std.scad")
(def include-bosl2-joiners  "../BOSL2/joiners.scad")
(def include-bosl2-screws "../BOSL2/screws.scad")
(def LEFT [-1 0 0])

(def RIGHT [1 0 0])

(def FRONT [0 -1 0])
(def FWD FRONT)
(def FORWARD FRONT)

(def BACK [0 1 0])

(def BOTTOM [0 0 -1])
(def BOT BOTTOM)
(def DOWN BOTTOM)

(def TOP [0 0 1])
(def UP TOP)

(def CENTER [0 0 0])
(def CTR CENTER)
(def CENTRE CENTER)

(def EDGES_ALL  [[1 1 1 1] [1 1 1 1] [1 1 1 1]])
(def Z [[0 0 0 0] [0 0 0 0] [1 1 1 1]])
(def CORNERS_ALL [1 1 1 1 1 1 1 1])
