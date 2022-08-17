(ns dactyl-keyboard.eeprom-adapter
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.utils :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]))

(def eeprom-adapter-length 11)
(def eeprom-adapter-width 10)
(def eeprom-adapter-height 2)
(def eeprom-adapter-tolerance 0.4)
(def eeprom-adapter-mount-wall-thickness 2)


(def eeprom-adapter-cutout-length (+ eeprom-adapter-length eeprom-adapter-tolerance))
(def eeprom-adapter-cutout-width (+  eeprom-adapter-width eeprom-adapter-tolerance))
(def eeprom-adapter-mount-length (+ eeprom-adapter-cutout-length eeprom-adapter-tolerance))
(def eeprom-adapter-mount-width (+  eeprom-adapter-cutout-width eeprom-adapter-tolerance))
;(def eeprom-adapter-height(/  eeprom-adapter-height)
