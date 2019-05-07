(ns metaprob-multimixture.examples.census-search
  (:require
   [metaprob-multimixture.examples.census-model :as model]
   [metaprob-multimixture.examples.census-data :refer [data]]
   [metaprob-cgpm.main :refer [cgpm-kl-divergence]]))

;; Example usage of metaprob-CGPM, using a cgpm-multimix model to
;; implement a search-by-example feature

(defn rowwise-similarity [cgpm latent-vars row-0 row-1 samples]
  (let [kl-divergence-fn (fn [r0 r1] (cgpm-kl-divergence
                                      cgpm
                                      latent-vars
                                      latent-vars
                                      r0
                                      r1
                                      {}
                                      samples))]
    ;; Return the symmetrized kl
    (+ (kl-divergence-fn row-0 row-1)
       (kl-divergence-fn row-1 row-0))))

(defn search
  "Given a model, a vector of maps representing a data table, and
  a (possibly sparse) example row, return a list of `(index, score)`
  tuples, where `index` is the index of a row in the data table and
  `score` is a measure of its similarity to the provided example
  row. The returned vector will be sorted by `score`, most similar
  rows first"
  [cgpm rows example latent-vars samples]

  (->> rows
       (map-indexed
        (fn [i r]
          [i (rowwise-similarity cgpm latent-vars example r samples)]))
       (sort-by second)))


(comment

  (search model/census-cgpm
          (map #(dissoc % "geo_fips" "district_name") data)
          {"percap" 50000}
          []
          10)

  )
