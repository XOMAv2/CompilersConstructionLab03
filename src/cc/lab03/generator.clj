(ns cc.lab03.generator
  (:require [cc.lab03.helpers :refer [ru->en]]
            [clojure.string]))

(defn transliterate-term [term]
  (-> term
      (clojure.string/trim)
      (clojure.string/replace #"\s+" "-")
      (ru->en)))

#_(transliterate-term "список операторов")

