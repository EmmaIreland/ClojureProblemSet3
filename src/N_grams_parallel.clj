(ns N_grams_parallel)

;; Goal is a function that takes a list of file names, reads the contents of those files,
;; and generates a map of tri-gram counts.


(defn tri-grams-parallel-helper [coll]
  (loop [words (apply vector coll) tri-grams-list []]
    (if (<= (count words) 3)
      (conj tri-grams-list words)
      (recur (subvec words 1) (conj tri-grams-list (subvec words 0 3)))
      )
    )
  )


(defn tri-grams-parallel [f numBlocks]
    (let [
          makeList (flatten (map #(concat %) 
                        (pmap #(filterv (fn [x] (not (= "" x))) (clojure.string/split (slurp %) #"\s")) f)
                        )) 
          numChars (count makeList)
          blockSize (quot numChars numBlocks)
          partials (pmap #(frequencies (tri-grams-parallel-helper %)) (partition blockSize makeList))
          ]
    (apply merge-with + partials)
    )    
  )

;; Parallel version of tri-grams (partition of 4)
;; "Elapsed time: 293.155796 msecs"
;; "Elapsed time: 284.242165 msecs"
;; "Elapsed time: 270.936182 msecs"

(defn run-timing-with-parallel [n]
  (let [
        fileList ["/home/irela065/Desktop/AllsWellThatEndsWell.txt" "/home/irela065/Desktop/AsYouLikeIt.txt"
                               "/home/irela065/Desktop/ClojureRemoveLines.txt" "/home/irela065/Desktop/ComedyOfErrors.txt"
                               "/home/irela065/Desktop/Cymbeline.txt" "/home/irela065/Desktop/LovesLaboursLost.txt"]
                        
        ]
    (time (tri-grams-parallel fileList n))))
