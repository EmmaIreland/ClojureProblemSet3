(ns SAT-Evaluation)

;; Arguments are in a vector/list and are forms of strings [a-coll]
;; Arguments must be valid arguments, any syntax error will return "error in argument"

;; and is represented as ^
;; or is represented as |
;; parentheses are represented as ()
;; not is represented as ~
;; each clause must be seperated by a space. example: "( p ^ q )"

;; solution variables are in a vector/list and the form of a map where keys = string variables and values = true or false [k-coll]
;; example: [{"p" true "f" false "t" true} {"p" false "f" true "t" true}]

;; takes a list starting with has an extra ")" out of the pairs: example: '(true "^" false ")")
(defn negation-args [a]
  (loop [arg a counter 0]
    (cond
      ;; termination when we reach a ")" we return the negated arg
      (= ")" (nth arg counter)) (concat (take counter arg) (nthrest arg (inc counter)))
      ;; change true to false
      (= true (nth arg counter)) (recur (concat (take counter arg) (list false) (nthrest arg (inc counter))) (inc counter))
      ;; change false to true
      (= false (nth arg counter)) (recur (concat (take counter arg) (list true) (nthrest arg (inc counter))) (inc counter))
      ;; change and to or
      (= "^" (nth arg counter)) (recur (concat (take counter arg) (list "|") (nthrest arg (inc counter))) (inc counter))
      ;; change or to and
      (= "|" (nth arg counter)) (recur (concat (take counter arg) (list "^") (nthrest arg (inc counter))) (inc counter))
      ;; the case where it's ~ ( some args ) or ( some args )
;      (or
;        (and (= "~" (nth arg counter))
;        (= "(" (nth arg (inc counter))))
;        
;        (= "(" (nth arg counter))) (recur (concat (take counter arg) 
;                                                  (semantics (take (inc (.indexOf (nthrest arg counter) ")")) (nthrest arg counter)))
;                                                  (nthrest (nthrest arg (inc counter)) (inc (.indexOf (nthrest arg (inc counter)) ")"))) ) 
;                                          (inc counter))
      ;; the case where it's ~ boolean
      (and (= "~" (nth arg counter))
           (or (= true (nth arg (inc counter)))
               (= false (nth arg (inc counter)))) ) (if (= (nth arg (inc counter)) true) 
                                                      (recur (concat (take counter arg) (list true) (nthrest arg (+ counter 2)) ) (inc counter))
                                                      (recur (concat (take counter arg) (list false) (nthrest arg (+ counter 2)) ) (inc counter)))
      ) )
  )

(defn semantics [a]
  
  (loop [arg a counter 0]
    (cond
      ;; termination when we have only true or false left in list
      (and (= (count arg) 1) 
           (or (= true (nth arg counter))
               (= false (nth arg counter))) ) (first arg)
      ;; the case where it's ~ boolean
      (and
        (= "~" (nth arg counter))
        (or (= (nth arg  (inc counter)) true) (= (nth arg (inc counter)) false)))
                           (if (= (nth arg (inc counter)) true) (recur (concat (take counter arg) (list false) (nthrest arg (+ counter 2)) ) counter)
                                                                (recur (concat (take counter arg) (list true) (nthrest arg (+ counter 2)) ) counter)
                             
                             )
      ;; the case where it's ~ ( some args )
      (and
        (= "~" (nth arg counter))
        (= "(" (nth arg (inc counter)))) (recur (concat (take counter arg) (negation-args (nthrest arg (+ counter 2)))) 0)
      ;; When we are at the front of the list
      (= counter  0) (recur arg (inc counter))
      ;; When we are at the end of the list
      (= counter (dec (count arg))) (recur arg 0)  
      ;; the case where it's ( some args )
      (and (= "(" (nth arg counter))
           (> (.indexOf (nthrest arg counter) ")") 2)       
           )
           (recur 
                                  (concat (take counter arg) 
                                          (list (semantics (take (.indexOf (nthrest arg (inc counter)) ")") (nthrest arg (inc counter)))))
                                          (nthrest (nthrest arg (inc counter)) (inc (.indexOf (nthrest arg (inc counter)) ")"))) )
                                   0) 
      ;; the case where it's ( boolean )
      (and
        (= "(" (nth arg (dec counter))) 
        (= ")" (nth arg (inc counter))) 
        (or (= (nth arg counter) true) (= (nth arg counter) false))) 
                           (recur (concat (take (dec counter) arg) (list (nth arg counter)) (nthrest arg (+ counter 2)) ) (dec counter))    
      ;; the case where it's true ^ true
      (and
        (= true (nth arg (dec counter)))
        (= true (nth arg (inc counter)))
        (= (nth arg counter) "^"))
                           (recur (concat (take (dec counter) arg) (list true) (nthrest arg (+ counter 3)) ) (dec counter))
        
      ;; all other cases of boolean ^ boolean
      (or
        (and 
          (= true (nth arg (dec counter)))
          (= false (nth arg (inc counter)))
          (= (nth arg counter) "^")
         )
        (and
          (= false (nth arg (dec counter)))
          (= true (nth arg (inc counter)))
          (= (nth arg counter) "^")
         )
        (and
          (= false (nth arg (dec counter)))
          (= false (nth arg (inc counter)))
          (= (nth arg counter) "^")
         ))                (recur (concat (take (dec counter) arg) (list false) (nthrest arg (+ counter 3)) ) (dec counter))
      
      ;; the case where it's false | false
      (and
        (= false (nth arg (dec counter)))
        (= false (nth arg (inc counter)))
        (= (nth arg counter) "|"))
                           (recur (concat (take (dec counter) arg) (list false) (nthrest arg (+ counter 3)) ) (dec counter))
              
      ;; all other cases of boolean | boolean
      (or
        (and 
          (= true (nth arg (dec counter)))
          (= true (nth arg (inc counter)))
          (= (nth arg counter) "|")
         )
        (and
          (= true (nth arg (dec counter)))
          (= false (nth arg (inc counter)))
          (= (nth arg counter) "|")
         )
        (and
          (= false (nth arg (dec counter)))
          (= true (nth arg (inc counter)))
          (= (nth arg counter) "|")
         ))                (recur (concat (take (dec counter) arg) (list true) (nthrest arg (+ counter 3)) ) (dec counter))
      :else "error in argument" 
   )
  )
)

(defn decode-vars [arg k-map]
  (let [decoded-arg-pieces (clojure.string/split arg #"\s")
        var-change #(if (contains? %2 %1) (get %2 %1) %1)
        ]
    (map #(var-change % k-map) decoded-arg-pieces)
    )
)

(defn sat-evaluation [a-coll k-coll]
  (let [replace-all-vars (pmap #(pmap (fn [x] (decode-vars % x)) k-coll) a-coll)]
  (pmap #(pmap (fn [x] (semantics x)) %) replace-all-vars))
)

;["p ^ ( q | r )" "( ~ p ^ q ) | r"] [{"p" true "q" false "r" true} {"p" true "q" false "r" false} {"p" false "q" true "r" true}]

;["( ~ p ^ q ) | r"] [{"p" true "q" false "r" true} {"p" true "q" false "r" false} {"p" false "q" true "r" true}]