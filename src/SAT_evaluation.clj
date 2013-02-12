(ns SAT-evaluation)

;; Arguments are in a vector/list and are forms of strings [a-coll]
;; each clause must be seperated by a space. example: ( p ^ q )

;; and is represented as ^
;; or is represented as |
;; parentheses are represented as ()
;; not is represented as ~

;; solution variables are in a vector/list and the form of a map where keys = string variables and values = true or false [k-coll]
;; example: [{"p" true "f" false "t" true} {"p" false "f" true "t" true}]


(defn semantics [a]
  (loop [arg a counter 0]
    (cond
      (and (= (count arg) 1) (first arg)) (first arg)
      (= (counter arg) 0) (recur arg (inc counter))
      (= (counter arg) (dec (count arg))) (recur arg 0)
      
      ;; the case where it's ( boolean )
      (and
        (= "(" (nth arg (dec counter))) 
        (= ")" (nth arg (inc counter))) 
        (or (= (nth arg counter) true) (= (nth arg counter) false))) ()
      
      ;; need cases for:
      ;; bool ^ bool
      ;; bool | bool
      ;; ~ bool
      ;; ~ ( stuff )
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

;["p ^ ( q | r )" "( ~ p ^ q ) | r"] [{"p" true, "q" false, "r" true} {"p" true, "q" false, "r" false} {"p" false, "q" true, "r" true}]

;["( ~ p ^ q ) | r"] [{"p" true, "q" false, "r" true} {"p" true, "q" false, "r" false} {"p" false, "q" true, "r" true}]
