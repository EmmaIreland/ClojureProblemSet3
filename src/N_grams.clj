(ns N-grams)

;; Goal is a function that takes a list of file names, reads the contents of those files,
;; and generates a map of tri-gram counts.


(defn tri-grams-helper [coll]
  (loop [words coll tri-grams-list []]
    (if (<= (count words) 3)
      (conj tri-grams-list words)
      (recur (subvec words 1) (conj tri-grams-list (subvec words 0 3)))
      )
    )
  )


(defn tri-grams [files]
  (let [split-by-whitelines #(clojure.string/split % #"\s")]
    (apply merge-with +(map #(frequencies %) 
                          (map #(tri-grams-helper (filterv (fn [x] (not (= "" x))) (split-by-whitelines (slurp %))) ) 
                               files)
                        )
    )
  )
)


;; Regular version of tri-grams
;; "Elapsed time: 369.589437 msecs"
;; "Elapsed time: 223.049432 msecs"
;; "Elapsed time: 216.928432 msecs"

(defn run-timing [n]
  (let [s ["/home/irela065/Desktop/AllsWellThatEndsWell.txt" "/home/irela065/Desktop/AsYouLikeIt.txt"
                               "/home/irela065/Desktop/ClojureRemoveLines.txt" "/home/irela065/Desktop/ComedyOfErrors.txt"
                               "/home/irela065/Desktop/Cymbeline.txt" "/home/irela065/Desktop/LovesLaboursLost.txt"]]
    (time (tri-grams s))))
