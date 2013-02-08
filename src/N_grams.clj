(ns N-grams)

;; Goal is a function that takes a list of file names, reads the contents of those files,
;; and generates a map of tri-gram counts.


(defn 3-grams-helper [coll]
  (loop [words coll 3-grams-list []]
    (if (<= (count words) 3)
      (conj 3-grams-list words)
      (recur (subvec words 1) (conj [] (subvec words 0 3)))
      )
    )
  )

(defn 3-grams [files]
  (let [split #(clojure.string/split % #"\s")]
  (map #(3-grams-helper %)  files)
  ))