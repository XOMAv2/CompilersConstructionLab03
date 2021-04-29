(ns cc.lab03.analyzer
  (:require [cc.lab03.helpers :refer [when-let*]]
            [clojure.string]))

(def program (slurp "resources/program.lab"))

(def tokens (-> program
                (clojure.string/trim)
                (clojure.string/replace #"\s+" " ")
                (clojure.string/replace #"[a-zA-Z]+" "идент")
                (clojure.string/replace #"\d+" "конст")
                (clojure.string/split #" ")))

(def debug? false)

(declare programma?)
(declare block?)
(declare spisok-operatorov?)
(declare operator?)
(declare hvost?)

(defn term? [term tokens]
  (when debug? (println {:name (format "term `%s`" term) :tokens tokens}))
  (when (= term (first tokens))
    [(rest tokens) [:term (first tokens)]]))

(defn programma? [tokens]
  (when debug? (println {:name "programma" :tokens tokens}))
  (if-let [branch-res (let [outputs []]
                        (when-let* [[tokens res] (block? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
    [(first branch-res) (into [:programma] (second branch-res))]
    nil))

(defn block? [tokens]
  (when debug? (println {:name "block" :tokens tokens}))
  (if-let [branch-res (let [outputs []]
                        (when-let* [[tokens res] (term? "{" tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (spisok-operatorov? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (term? "}" tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
    [(first branch-res) (into [:block] (second branch-res))]
    nil))

(defn spisok-operatorov? [tokens]
  (when debug? (println {:name "spisok-operatorov" :tokens tokens}))
  (if-let [branch-res (let [outputs []]
                        (when-let* [[tokens res] (operator? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (hvost? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
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
                                    outputs (conj outputs res)
                                    [tokens res] (operator? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (hvost? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
    [(first branch-res) (into [:hvost] (second branch-res))]
    (if-let [branch-res (let [outputs []]
                          (when-let* [[tokens res] (term? ";" tokens)
                                      outputs (conj outputs res)
                                      [tokens res] (operator? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
      [(first branch-res) (into [:hvost] (second branch-res))]
      nil)))

(defn operator? [tokens]
  (when debug? (println {:name "operator" :tokens tokens}))
  (if-let [branch-res (let [outputs []]
                        (when-let* [[tokens res] (term? "идент" tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (term? "=" tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (term? "выражение" tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
    [(first branch-res) (into [:operator] (second branch-res))]
    (if-let [branch-res (let [outputs []]
                          (when-let* [[tokens res] (block? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
      [(first branch-res) (into [:operator] (second branch-res))]
      nil)))

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
