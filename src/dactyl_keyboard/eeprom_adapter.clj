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