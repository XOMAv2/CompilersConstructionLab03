(ns cc.lab03.generator
  (:require [cc.lab03.helpers :refer [ru->en when-let*]]
            [clojure.string]))

(defn transliterate-term [term]
  (-> term
      (clojure.string/trim)
      (clojure.string/replace #"\s+" "-")
      (ru->en)))

#_(transliterate-term "список операторов")

(def debug? false)

;; (defmacro term? [term tokens]
;;   `(when (= ~term (first ~tokens))
;;      [(rest ~tokens) [:term (first ~tokens)]]))

(defn term? [term tokens]
  (when debug? (println {:name (format "term `%s`" term) :tokens tokens}))
  (when (= term (first tokens))
    [(rest tokens) [:term (first tokens)]]))
