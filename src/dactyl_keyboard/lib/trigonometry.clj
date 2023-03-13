(ns dactyl-keyboard.lib.trigonometry
  (:require
   [clojure/math :refer [PI]]
   )
  )

(defn deg2rad [degrees]
  (* (/ degrees 180) PI))

(defn rad2deg [radians]
  (/ (* radians 180) PI))

