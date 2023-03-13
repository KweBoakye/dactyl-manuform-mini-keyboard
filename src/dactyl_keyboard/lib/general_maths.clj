(ns dactyl-keyboard.lib.general-maths)

(defn factorial
  ([n]                    ; when only one argument is passed in
   (factorial n 1))
  ([n acc]                ; when two arguments are passed in
   (if  (= n 0)  acc
        (recur (dec n) (* acc n)))))