(ns cc.lab03.analyzer
   (:require [cc.lab03.helpers :refer [when-let*]]))

(def debug? false)

(declare prostoe-vyrajenie?)
(declare spisok-operatorov-1?)
(declare prostoe-vyrajenie-1?)
(declare vyrajenie-5?)
(declare programma?)
(declare vyrajenie-1?)
(declare op-tipa-umnojeniiu?)
(declare ttterm-3?)
(declare blok?)
(declare op-otnosheniiu?)
(declare ttterm?)
(declare ttterm-1?)
(declare vyrajenie-6?)
(declare prostoe-vyrajenie-4?)
(declare vyrajenie-2?)
(declare vyrajenie?)
(declare spisok-operatorov?)
(declare ttterm-2?)
(declare vyrajenie-3?)
(declare faktor?)
(declare operator?)
(declare prostoe-vyrajenie-3?)
(declare prostoe-vyrajenie-2?)
(declare hvost?)
(declare znak?)
(declare vyrajenie-4?)
(declare op-tipa-slojeniiu?)
(declare hvost-1?)

(defn term? [term tokens]
    (when debug? (println {:name (format "term '%s'" term), :tokens tokens}))
    (when (= term (first tokens)) [(rest tokens) [:term (first tokens)]]))

(defn epsilon? [tokens]
    (when debug? (println {:name "epsilon", :tokens tokens}))
    [tokens [:epsilon]])

(defn prostoe-vyrajenie? [tokens]
  (when debug? (println {:name "prostoe-vyrajenie", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "prostoe-vyrajenie" 1))
                        (when-let* [[tokens res] (ttterm? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (prostoe-vyrajenie-1? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:prostoe-vyrajenie] (second branch))]
      (if-let [branch (do (when debug? (println "prostoe-vyrajenie" 2))
                          (when-let* [[tokens res] (znak? tokens)
                                      outputs (conj outputs res)
                                      [tokens res] (ttterm? tokens)
                                      outputs (conj outputs res)
                                      [tokens res] (prostoe-vyrajenie-4? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:prostoe-vyrajenie] (second branch))]
        nil))))

(defn spisok-operatorov-1? [tokens]
  (when debug? (println {:name "spisok-operatorov-1", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "spisok-operatorov-1" 1))
                        (when-let* [[tokens res] (hvost? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:spisok-operatorov-1] (second branch))]
      (if-let [branch (do (when debug? (println "spisok-operatorov-1" 2))
                          (when-let* [[tokens res] (epsilon? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:spisok-operatorov-1] (second branch))]
        nil))))

(defn prostoe-vyrajenie-1? [tokens]
  (when debug? (println {:name "prostoe-vyrajenie-1", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "prostoe-vyrajenie-1" 1))
                        (when-let* [[tokens res] (prostoe-vyrajenie-2? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:prostoe-vyrajenie-1] (second branch))]
      (if-let [branch (do (when debug? (println "prostoe-vyrajenie-1" 2))
                          (when-let* [[tokens res] (epsilon? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:prostoe-vyrajenie-1] (second branch))]
        nil))))

(defn vyrajenie-5? [tokens]
  (when debug? (println {:name "vyrajenie-5", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "vyrajenie-5" 1))
                        (when-let* [[tokens res] (op-otnosheniiu? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (prostoe-vyrajenie? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:vyrajenie-5] (second branch))]
      (if-let [branch (do (when debug? (println "vyrajenie-5" 2))
                          (when-let* [[tokens res] (epsilon? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:vyrajenie-5] (second branch))]
        nil))))

(defn programma? [tokens]
  (when debug? (println {:name "programma", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "programma" 1))
                        (when-let* [[tokens res] (blok? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:programma] (second branch))]
      nil)))

(defn vyrajenie-1? [tokens]
  (when debug? (println {:name "vyrajenie-1", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "vyrajenie-1" 1))
                        (when-let* [[tokens res] (ttterm-2? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (vyrajenie-4? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:vyrajenie-1] (second branch))]
      (if-let [branch (do (when debug? (println "vyrajenie-1" 2))
                          (when-let* [[tokens res] (prostoe-vyrajenie-2? tokens)
                                      outputs (conj outputs res)
                                      [tokens res] (vyrajenie-3? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:vyrajenie-1] (second branch))]
        (if-let [branch (do (when debug? (println "vyrajenie-1" 3))
                            (when-let* [[tokens res] (op-otnosheniiu? tokens)
                                        outputs (conj outputs res)
                                        [tokens res] (prostoe-vyrajenie? tokens)
                                        outputs (conj outputs res)]
                                       [tokens outputs]))]
          [(first branch) (into [:vyrajenie-1] (second branch))]
          (if-let [branch (do (when debug? (println "vyrajenie-1" 4))
                              (when-let* [[tokens res] (epsilon? tokens)
                                          outputs (conj outputs res)]
                                         [tokens outputs]))]
            [(first branch) (into [:vyrajenie-1] (second branch))]
            nil))))))

(defn op-tipa-umnojeniiu? [tokens]
  (when debug? (println {:name "op-tipa-umnojeniiu", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "op-tipa-umnojeniiu" 1))
                        (when-let* [[tokens res] (term? "*" tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:op-tipa-umnojeniiu] (second branch))]
      (if-let [branch (do (when debug? (println "op-tipa-umnojeniiu" 2))
                          (when-let* [[tokens res] (term? "/" tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:op-tipa-umnojeniiu] (second branch))]
        (if-let [branch (do (when debug? (println "op-tipa-umnojeniiu" 3))
                            (when-let* [[tokens res] (term? "div" tokens)
                                        outputs (conj outputs res)]
                                       [tokens outputs]))]
          [(first branch) (into [:op-tipa-umnojeniiu] (second branch))]
          (if-let [branch (do (when debug? (println "op-tipa-umnojeniiu" 4))
                              (when-let* [[tokens res] (term? "mod" tokens)
                                          outputs (conj outputs res)]
                                         [tokens outputs]))]
            [(first branch) (into [:op-tipa-umnojeniiu] (second branch))]
            (if-let [branch (do (when debug? (println "op-tipa-umnojeniiu" 5))
                                (when-let* [[tokens res] (term? "and" tokens)
                                            outputs (conj outputs res)]
                                           [tokens outputs]))]
              [(first branch) (into [:op-tipa-umnojeniiu] (second branch))]
              nil)))))))

(defn ttterm-3? [tokens]
  (when debug? (println {:name "ttterm-3", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "ttterm-3" 1))
                        (when-let* [[tokens res] (ttterm-2? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:ttterm-3] (second branch))]
      (if-let [branch (do (when debug? (println "ttterm-3" 2))
                          (when-let* [[tokens res] (epsilon? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:ttterm-3] (second branch))]
        nil))))

(defn blok? [tokens]
  (when debug? (println {:name "blok", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "blok" 1))
                        (when-let* [[tokens res] (term? "{" tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (spisok-operatorov? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (term? "}" tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:blok] (second branch))]
      nil)))

(defn op-otnosheniiu? [tokens]
  (when debug? (println {:name "op-otnosheniiu", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "op-otnosheniiu" 1))
                        (when-let* [[tokens res] (term? "<=" tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:op-otnosheniiu] (second branch))]
      (if-let [branch (do (when debug? (println "op-otnosheniiu" 2))
                          (when-let* [[tokens res] (term? ">=" tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:op-otnosheniiu] (second branch))]
        (if-let [branch (do (when debug? (println "op-otnosheniiu" 3))
                            (when-let* [[tokens res] (term? "<>" tokens)
                                        outputs (conj outputs res)]
                                       [tokens outputs]))]
          [(first branch) (into [:op-otnosheniiu] (second branch))]
          (if-let [branch (do (when debug? (println "op-otnosheniiu" 4))
                              (when-let* [[tokens res] (term? ">" tokens)
                                          outputs (conj outputs res)]
                                         [tokens outputs]))]
            [(first branch) (into [:op-otnosheniiu] (second branch))]
            (if-let [branch (do (when debug? (println "op-otnosheniiu" 5))
                                (when-let* [[tokens res] (term? "<" tokens)
                                            outputs (conj outputs res)]
                                           [tokens outputs]))]
              [(first branch) (into [:op-otnosheniiu] (second branch))]
              (if-let [branch (do (when debug? (println "op-otnosheniiu" 6))
                                  (when-let* [[tokens res] (term? "==" tokens)
                                              outputs (conj outputs res)]
                                             [tokens outputs]))]
                [(first branch) (into [:op-otnosheniiu] (second branch))]
                nil))))))))

(defn ttterm? [tokens]
  (when debug? (println {:name "ttterm", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "ttterm" 1))
                        (when-let* [[tokens res] (faktor? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (ttterm-1? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:ttterm] (second branch))]
      nil)))

(defn ttterm-1? [tokens]
  (when debug? (println {:name "ttterm-1", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "ttterm-1" 1))
                        (when-let* [[tokens res] (ttterm-2? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:ttterm-1] (second branch))]
      (if-let [branch (do (when debug? (println "ttterm-1" 2))
                          (when-let* [[tokens res] (epsilon? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:ttterm-1] (second branch))]
        nil))))

(defn vyrajenie-6? [tokens]
  (when debug? (println {:name "vyrajenie-6", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "vyrajenie-6" 1))
                        (when-let* [[tokens res] (op-otnosheniiu? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (prostoe-vyrajenie? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:vyrajenie-6] (second branch))]
      (if-let [branch (do (when debug? (println "vyrajenie-6" 2))
                          (when-let* [[tokens res] (epsilon? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:vyrajenie-6] (second branch))]
        nil))))

(defn prostoe-vyrajenie-4? [tokens]
  (when debug? (println {:name "prostoe-vyrajenie-4", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "prostoe-vyrajenie-4" 1))
                        (when-let* [[tokens res] (prostoe-vyrajenie-2? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:prostoe-vyrajenie-4] (second branch))]
      (if-let [branch (do (when debug? (println "prostoe-vyrajenie-4" 2))
                          (when-let* [[tokens res] (epsilon? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:prostoe-vyrajenie-4] (second branch))]
        nil))))

(defn vyrajenie-2? [tokens]
  (when debug? (println {:name "vyrajenie-2", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "vyrajenie-2" 1))
                        (when-let* [[tokens res] (prostoe-vyrajenie-2? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (vyrajenie-5? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:vyrajenie-2] (second branch))]
      (if-let [branch (do (when debug? (println "vyrajenie-2" 2))
                          (when-let* [[tokens res] (op-otnosheniiu? tokens)
                                      outputs (conj outputs res)
                                      [tokens res] (prostoe-vyrajenie? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:vyrajenie-2] (second branch))]
        (if-let [branch (do (when debug? (println "vyrajenie-2" 3))
                            (when-let* [[tokens res] (epsilon? tokens)
                                        outputs (conj outputs res)]
                                       [tokens outputs]))]
          [(first branch) (into [:vyrajenie-2] (second branch))]
          nil)))))

(defn vyrajenie? [tokens]
  (when debug? (println {:name "vyrajenie", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "vyrajenie" 1))
                        (when-let* [[tokens res] (faktor? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (vyrajenie-1? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:vyrajenie] (second branch))]
      (if-let [branch (do (when debug? (println "vyrajenie" 2))
                          (when-let* [[tokens res] (znak? tokens)
                                      outputs (conj outputs res)
                                      [tokens res] (ttterm? tokens)
                                      outputs (conj outputs res)
                                      [tokens res] (vyrajenie-2? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:vyrajenie] (second branch))]
        nil))))

(defn spisok-operatorov? [tokens]
  (when debug? (println {:name "spisok-operatorov", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "spisok-operatorov" 1))
                        (when-let* [[tokens res] (operator? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (spisok-operatorov-1? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:spisok-operatorov] (second branch))]
      nil)))

(defn ttterm-2? [tokens]
  (when debug? (println {:name "ttterm-2", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "ttterm-2" 1))
                        (when-let* [[tokens res] (op-tipa-umnojeniiu? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (faktor? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (ttterm-3? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:ttterm-2] (second branch))]
      nil)))

(defn vyrajenie-3? [tokens]
  (when debug? (println {:name "vyrajenie-3", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "vyrajenie-3" 1))
                        (when-let* [[tokens res] (op-otnosheniiu? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (prostoe-vyrajenie? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:vyrajenie-3] (second branch))]
      (if-let [branch (do (when debug? (println "vyrajenie-3" 2))
                          (when-let* [[tokens res] (epsilon? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:vyrajenie-3] (second branch))]
        nil))))

(defn faktor? [tokens]
  (when debug? (println {:name "faktor", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "faktor" 1))
                        (when-let* [[tokens res] (term? "конст" tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:faktor] (second branch))]
      (if-let [branch (do (when debug? (println "faktor" 2))
                          (when-let* [[tokens res] (term? "(" tokens)
                                      outputs (conj outputs res)
                                      [tokens res] (prostoe-vyrajenie? tokens)
                                      outputs (conj outputs res)
                                      [tokens res] (term? ")" tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:faktor] (second branch))]
        (if-let [branch (do (when debug? (println "faktor" 3))
                            (when-let* [[tokens res] (term? "идент" tokens)
                                        outputs (conj outputs res)]
                                       [tokens outputs]))]
          [(first branch) (into [:faktor] (second branch))]
          (if-let [branch (do (when debug? (println "faktor" 4))
                              (when-let* [[tokens res] (term? "not" tokens)
                                          outputs (conj outputs res)
                                          [tokens res] (faktor? tokens)
                                          outputs (conj outputs res)]
                                         [tokens outputs]))]
            [(first branch) (into [:faktor] (second branch))]
            nil))))))

(defn operator? [tokens]
  (when debug? (println {:name "operator", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "operator" 1))
                        (when-let* [[tokens res] (term? "{" tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (spisok-operatorov? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (term? "}" tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:operator] (second branch))]
      (if-let [branch (do (when debug? (println "operator" 2))
                          (when-let* [[tokens res] (term? "идент" tokens)
                                      outputs (conj outputs res)
                                      [tokens res] (term? "=" tokens)
                                      outputs (conj outputs res)
                                      [tokens res] (vyrajenie? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:operator] (second branch))]
        nil))))

(defn prostoe-vyrajenie-3? [tokens]
  (when debug? (println {:name "prostoe-vyrajenie-3", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "prostoe-vyrajenie-3" 1))
                        (when-let* [[tokens res] (prostoe-vyrajenie-2? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:prostoe-vyrajenie-3] (second branch))]
      (if-let [branch (do (when debug? (println "prostoe-vyrajenie-3" 2))
                          (when-let* [[tokens res] (epsilon? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:prostoe-vyrajenie-3] (second branch))]
        nil))))

(defn prostoe-vyrajenie-2? [tokens]
  (when debug? (println {:name "prostoe-vyrajenie-2", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "prostoe-vyrajenie-2" 1))
                        (when-let* [[tokens res] (op-tipa-slojeniiu? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (ttterm? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (prostoe-vyrajenie-3? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:prostoe-vyrajenie-2] (second branch))]
      nil)))

(defn hvost? [tokens]
  (when debug? (println {:name "hvost", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "hvost" 1))
                        (when-let* [[tokens res] (term? ";" tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (operator? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (hvost-1? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:hvost] (second branch))]
      nil)))

(defn znak? [tokens]
  (when debug? (println {:name "znak", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "znak" 1))
                        (when-let* [[tokens res] (term? "-" tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:znak] (second branch))]
      (if-let [branch (do (when debug? (println "znak" 2))
                          (when-let* [[tokens res] (term? "+" tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:znak] (second branch))]
        nil))))

(defn vyrajenie-4? [tokens]
  (when debug? (println {:name "vyrajenie-4", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "vyrajenie-4" 1))
                        (when-let* [[tokens res] (prostoe-vyrajenie-2? tokens)
                                    outputs (conj outputs res)
                                    [tokens res] (vyrajenie-6? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:vyrajenie-4] (second branch))]
      (if-let [branch (do (when debug? (println "vyrajenie-4" 2))
                          (when-let* [[tokens res] (op-otnosheniiu? tokens)
                                      outputs (conj outputs res)
                                      [tokens res] (prostoe-vyrajenie? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:vyrajenie-4] (second branch))]
        (if-let [branch (do (when debug? (println "vyrajenie-4" 3))
                            (when-let* [[tokens res] (epsilon? tokens)
                                        outputs (conj outputs res)]
                                       [tokens outputs]))]
          [(first branch) (into [:vyrajenie-4] (second branch))]
          nil)))))

(defn op-tipa-slojeniiu? [tokens]
  (when debug? (println {:name "op-tipa-slojeniiu", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "op-tipa-slojeniiu" 1))
                        (when-let* [[tokens res] (term? "-" tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:op-tipa-slojeniiu] (second branch))]
      (if-let [branch (do (when debug? (println "op-tipa-slojeniiu" 2))
                          (when-let* [[tokens res] (term? "+" tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:op-tipa-slojeniiu] (second branch))]
        (if-let [branch (do (when debug? (println "op-tipa-slojeniiu" 3))
                            (when-let* [[tokens res] (term? "or" tokens)
                                        outputs (conj outputs res)]
                                       [tokens outputs]))]
          [(first branch) (into [:op-tipa-slojeniiu] (second branch))]
          nil)))))

(defn hvost-1? [tokens]
  (when debug? (println {:name "hvost-1", :tokens tokens}))
  (let [outputs []]
    (if-let [branch (do (when debug? (println "hvost-1" 1))
                        (when-let* [[tokens res] (hvost? tokens)
                                    outputs (conj outputs res)]
                                   [tokens outputs]))]
      [(first branch) (into [:hvost-1] (second branch))]
      (if-let [branch (do (when debug? (println "hvost-1" 2))
                          (when-let* [[tokens res] (epsilon? tokens)
                                      outputs (conj outputs res)]
                                     [tokens outputs]))]
        [(first branch) (into [:hvost-1] (second branch))]
        nil))))
