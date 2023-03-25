(ns dactyl-keyboard.lib.general-maths)

(defn factorial
  ([n]                    ; when only one argument is passed in
   (factorial n 1))
  ([n acc]                ; when two arguments are passed in
   (if  (= n 0)  acc
        (recur (dec n) (* acc n)))))

(defn binomial-coefficient [n k]
  (let [rprod (fn [a b] (reduce * (range a (inc b))))]
    (/ (rprod (- n k -1) n) (rprod 1 k))))

(defn binomial-coefficient-2 [k i]
  (cond (> i k) 0.0
        :else (let [k-factorial (factorial k)
                    i-factorial (factorial i)
                    k-i-factorial (factorial (- k i))]
                (/ k-factorial 
                   (* k-i-factorial i-factorial))
                ))
  )

(comment (binomial-coefficient 5 8))

(defn all-binomials-for-n [n k]
  (vec (for [i (range (inc k))]
    (binomial-coefficient n i)))
  )

(comment (all-binomials-for-n 5 3))
