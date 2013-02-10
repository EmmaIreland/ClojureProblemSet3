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

(defn tri-grams-parallel [coll numBlocks]
  (let [makeList (apply merge-with + 
                          (pmap #(filterv (fn [x] (not (= "" x))) (clojure.string/split (slurp %) #"\s")) 
                               coll)
                        )
        numChars (count (makeList coll))
        blockSize (quot numChars numBlocks)
        extractBlock (fn [blockNumber]
                       (subvec (makeList coll) (* blockNumber blockSize) (* (inc blockNumber) blockSize)))
        partials (pmap (comp tri-grams-helper extractBlock) (range numBlocks))
        ]
    partials)
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
