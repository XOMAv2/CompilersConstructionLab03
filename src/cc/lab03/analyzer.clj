(ns cc.lab03.analyzer
  (:require [cc.lab03.helpers :refer [when-let*]]))

(def program (slurp "resources/program.lab"))

(def tokens [["{" "{"]
             ["идент" "b"]
             ["=" "="]
             ["конст" "4"]
             [";" ";"]
             ["{" "{"]
             ["{" "{"]
             ["{" "{"]
             ["идент" "b"]
             ["=" "="]
             ["-" "-"]
             ["конст" "4"]
             ["==" "=="]
             ["конст" "5"]
             ["}" "}"]
             ["}" "}"]
             [";" ";"]
             ["идент" "a"]
             ["=" "="]
             ["идент" "b"]
             ["}" "}"]
             [";" ";"]
             ["идент" "d"]
             ["=" "="]
             ["(" "("]
             ["идент" "a"]
             ["*" "*"]
             ["конст" "3"]
             [")" ")"]
             ["<" "<"]
             ["идент" "bb"]
             ["or" "or"]
             ["not" "not"]
             ["("]
             ["идент" "c"]
             ["mod" "mod"]
             ["конст" "2"]
             [")" ")"]
             [";" ";"]
             ["идент" "c"]
             ["=" "="]
             ["идент" "d"]
             ["-" "-"]
             ["конст" "32"]
             ["}" "}"]
             [nil "<EOF>"]])

(def debug? false)

(declare programma?)
(declare blok?)
(declare spisok-operatorov?)
(declare operator?)
(declare hvost?)
(declare ident?)
(declare vyrajenie?)

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
