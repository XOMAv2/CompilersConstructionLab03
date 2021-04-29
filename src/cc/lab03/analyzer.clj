(ns cc.lab03.analyzer
  (:require [cc.lab03.helpers :refer [when-let*]]
            [cc.lab03.generator :refer [rollback-alts]]
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
  (let [outputs []]
    (lazy-seq
     (cons (rollback-alts
            [nt-res (block? tokens)]
            (when-let* [[tokens res] nt-res
                        outputs (conj outputs res)]
                       [tokens (into [:programma] outputs)]))
           nil))))

(defn block? [tokens]
  (when debug? (println {:name "block" :tokens tokens}))
  (let [outputs []]
    (lazy-seq
     (cons (when-let* [[tokens res] (term? "{" tokens)
                       outputs (conj outputs res)]
                      (rollback-alts
                       [nt-res (spisok-operatorov? tokens)]
                       (when-let* [[tokens res] nt-res
                                   outputs (conj outputs res)
                                   [tokens res] (term? "}" tokens)
                                   outputs (conj outputs res)]
                                  [tokens (into [:block] outputs)])))
           nil))))

(defn spisok-operatorov? [tokens]
  (when debug? (println {:name "spisok-operatorov" :tokens tokens}))
  (let [outputs []]
    (lazy-seq
     (cons (rollback-alts
            [nt-res (operator? tokens)]
            (when-let* [[tokens res] nt-res
                        outputs (conj outputs res)]
                       [tokens (into [:spisok-operatorov] outputs)]))
           (lazy-seq
            (cons (rollback-alts
                   [nt-res (operator? tokens)]
                   (when-let* [[tokens res] nt-res
                               outputs (conj outputs res)]
                              (rollback-alts
                               [nt-res (hvost? tokens)]
                               (when-let* [[tokens res] nt-res
                                           outputs (conj outputs res)]
                                          [tokens (into [:spisok-operatorov] outputs)]))))
                  nil))))))

(defn hvost? [tokens]
  (when debug? (println {:name "hvost" :tokens tokens}))
  (let [outputs []]
    (lazy-seq
     (cons (when-let* [[tokens res] (term? ";" tokens)
                       outputs (conj outputs res)]
                      (rollback-alts
                       [nt-res (operator? tokens)]
                       (when-let* [[tokens res] nt-res
                                  outputs (conj outputs res)]
                                  (rollback-alts
                                   [nt-res (hvost? tokens)]
                                   (when-let* [[tokens res] nt-res
                                               outputs (conj outputs res)]
                                              [tokens (into [:hvost] outputs)])))))
           (lazy-seq
            (cons (when-let* [[tokens res] (term? ";" tokens)
                              outputs (conj outputs res)]
                             (rollback-alts
                              [nt-res (operator? tokens)]
                              (when-let* [[tokens res] nt-res
                                          outputs (conj outputs res)]
                                         [tokens (into [:hvost] outputs)])))
                  nil))))))

(defn operator? [tokens]
  (when debug? (println {:name "operator" :tokens tokens}))
  (let [outputs []]
    (lazy-seq
     (cons (when-let* [[tokens res] (term? "идент" tokens)
                       outputs (conj outputs res)
                       [tokens res] (term? "=" tokens)
                       outputs (conj outputs res)
                       [tokens res] (term? "выражение" tokens)
                       outputs (conj outputs res)]
                      [tokens (into [:operator] outputs)])
           (lazy-seq
            (cons (rollback-alts
                   [nt-res (block? tokens)]
                   (when-let* [[tokens res] nt-res
                               outputs (conj outputs res)]
                              [tokens (into [:operator] outputs)]))
                  nil))))))

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
