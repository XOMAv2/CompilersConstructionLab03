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

(declare programma?
         blok?
         spisok-operatorov?
         operator?
         hvost?
         ident?
         vyrajenie?)

;; (defmacro term? [term tokens]
;;   `(when (= ~term (first ~tokens))
;;      [(rest ~tokens) (first ~tokens)]))

(defn term? [term tokens]
  (when debug? (println {:name (format "term `%s`" term) :tokens tokens}))
  (when (= term (first tokens))
    [(rest tokens) (first tokens)]))

(defn programma? [tokens]
  (when debug? (println {:name "programma" :tokens tokens}))
  (if-let [branch-res (let [outputs []]
                        (when-let [[tokens res] (block? tokens)]
                          (let [outputs (conj outputs res)]
                            [tokens outputs])))]
    [(first branch-res) (into [:programma] (second branch-res))]
    nil))

(defn block? [tokens]
  (when debug? (println {:name "block" :tokens tokens}))
  (if-let [branch-res (let [outputs []]
                        (when-let* [[tokens res] (term? "{" tokens)
                                    outputs (conj outputs res)]
                                   (when-let* [[tokens res] (spisok-operatorov? tokens)
                                               outputs (conj outputs res)]
                                              (when-let* [[tokens res] (term? "}" tokens)
                                                          outputs (conj outputs res)]
                                                         [tokens outputs]))))]
    [(first branch-res) (into [:block] (second branch-res))]
    nil))

(defn spisok-operatorov? [tokens]
  (when debug? (println {:name "spisok-operatorov" :tokens tokens}))
  (if-let [branch-res (let [outputs []]
                        (when-let* [[tokens res] (operator? tokens)
                                    outputs (conj outputs res)]
                                   (when-let* [[tokens res] (hvost? tokens)
                                               outputs (conj outputs res)]
                                              [tokens outputs])))]
    [(first branch-res) (into [:spisok-operatorov] (second branch-res))]
    (if-let [branch-res (let [outputs []]
                          (when-let* [[tokens res] (operator? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
      [(first branch-res) (into [:spisok-operatorov] (second branch-res))]
      nil)))

(defn hvost? [tokens]
  (when debug? (println {:name "hvost" :tokens tokens}))
  (if-let [branch-res (let [outputs []]
                        (when-let* [[tokens res] (term? ";" tokens)
                                    outputs (conj outputs res)]
                                   (when-let* [[tokens res] (operator? tokens)
                                               outputs (conj outputs res)]
                                              (when-let* [[tokens res] (hvost? tokens)
                                                          outputs (conj outputs res)]
                                                         [tokens outputs]))))]
    [(first branch-res) (into [:hvost] (second branch-res))]
    (if-let [branch-res (let [outputs []]
                          (when-let* [[tokens res] (term? ";" tokens)
                                      outputs (conj outputs res)]
                                     (when-let* [[tokens res] (operator? tokens)
                                                 outputs (conj outputs res)]
                                                [tokens outputs])))]
      [(first branch-res) (into [:hvost] (second branch-res))]
      nil)))

(defn operator? [tokens]
  (when debug? (println {:name "operator" :tokens tokens}))
  (if-let [branch-res (let [outputs []]
                        (when-let* [[tokens res] (ident? tokens)
                                    outputs (conj outputs res)]
                                   (when-let* [[tokens res] (term? "=" tokens)
                                               outputs (conj outputs res)]
                                              (when-let* [[tokens res] (vyrajenie? tokens)
                                                          outputs (conj outputs res)]
                                                         [tokens outputs]))))]
    [(first branch-res) (into [:operator] (second branch-res))]
    (if-let [branch-res (let [outputs []]
                          (when-let* [[tokens res] (block? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
      [(first branch-res) (into [:operator] (second branch-res))]
      nil)))

(defn ident? [tokens]
  (when debug? (println {:name "ident" :tokens tokens}))
  [(rest tokens) (into [:ident] (first tokens))])

(defn vyrajenie? [tokens]
  (when debug? (println {:name "vyrajenie" :tokens tokens}))
  [(rest tokens) (into [:vyrajenie] (first tokens))])

#_(programma? ["{"
               "идент"
               "="
               "выражение"
               ";"
               "{"
               "{"
               "{"
               "идент"
               "="
               "выражение"
               "}"
               "}"
               ";"
               "идент"
               "="
               "выражение"
               "}"
               ";"
               "идент"
               "="
               "выражение"
               ";"
               "идент"
               "="
               "выражение"
               "}"])
