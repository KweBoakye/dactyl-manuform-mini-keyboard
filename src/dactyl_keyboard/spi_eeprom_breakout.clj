(ns dactyl-keyboard.spi-eeprom-breakout
  (:refer-clojure :exclude [use import])
  (:require
   [scad-clj.scad :refer :all]
   [scad-clj.model :refer :all]
   [dactyl-keyboard.utils :refer :all]
   [dactyl-keyboard.lib.transformations :refer [rdz]])
  )


(def spi-eeprom-breakout-mounting-hole-y-coordinate -70)
(def spi-eeprom-breakout-mounting-hole-position-left [120.4468 spi-eeprom-breakout-mounting-hole-y-coordinate])
(def spi-eeprom-breakout-mounting-hole-position-right [135.4836 spi-eeprom-breakout-mounting-hole-y-coordinate])
(def spi-eeprom-breakout-mounting-hole-mid-x-coordinate (/ (+ (nth spi-eeprom-breakout-mounting-hole-position-left 0) (nth spi-eeprom-breakout-mounting-hole-position-right 0)) -2))
(def spi-eeprom-breakout-mounting-hole-position-diameter 2)
(def spi-eeprom-breakout-mounting-standoff-inner-diameter 3.2)
(def spi-eeprom-breakout-mounting-standoff-outer-diameter (+ spi-eeprom-breakout-mounting-standoff-inner-diameter 2))
(def spi-eeprom-breakout-mounting-standoff-inner-radius (/ spi-eeprom-breakout-mounting-standoff-inner-diameter 2))
(def spi-eeprom-breakout-mounting-standoff-outer-radius (/ spi-eeprom-breakout-mounting-standoff-outer-diameter 2))
(def spi-eeprom-breakout-mounting-standoff-height 4)
(def spi-eeprom-breakout (translate [0 0 4] (import "../parts/spi_eeprom_breakout.stl")))

(def spi-eeprom-breakout-support-bar
  (->>
   (cube 4 spi-eeprom-breakout-mounting-standoff-outer-diameter spi-eeprom-breakout-mounting-standoff-height)
   (translate [(- spi-eeprom-breakout-mounting-hole-mid-x-coordinate)
               (- spi-eeprom-breakout-mounting-hole-y-coordinate 4)
               (/ spi-eeprom-breakout-mounting-standoff-height 2)])))
(def spi-eeprom-breakout-mounting-standoffs
  (multiple-standoffs spi-eeprom-breakout-mounting-standoff-inner-radius
                      spi-eeprom-breakout-mounting-standoff-outer-radius
                      spi-eeprom-breakout-mounting-standoff-height
                      [spi-eeprom-breakout-mounting-hole-position-left spi-eeprom-breakout-mounting-hole-position-right]))

(defn spi-eeprom-breakout-place [shape]
  (translate [-160 30 0] shape))

(def spi-eeprom-breakout-mount
  (union 
   spi-eeprom-breakout-mounting-standoffs
  spi-eeprom-breakout-support-bar)
  )
(spit "things-low/spi-eeprom-breakout.scad"
      (write-scad
       (spi-eeprom-breakout-place (union 
                                spi-eeprom-breakout
                               spi-eeprom-breakout-mounting-standoffs
                               spi-eeprom-breakout-support-bar))))