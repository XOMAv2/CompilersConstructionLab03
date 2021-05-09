(ns cc.lab03.generator
  (:require [cc.lab03.helpers :refer [ru->en when-let* expand-selected-macros]]
            [clojure.string]
            [clojure.pprint]))

(defn transliterate-nonterm [nonterm]
  (-> nonterm
      (clojure.string/trim)
      (clojure.string/replace #"\s+" "-")
      (ru->en)
      (#(if (#{"debug" "term" "epsilon"} %)
          (throw (Exception. (str "Недопустимое имя '" nonterm "' для нетерминала.")))
          %))))

#_(transliterate-nonterm "список операторов")

(defmacro for-alts
  "nt-alternatives - последовательность, каждый элемент которой - результат разбора (возможно nil)
                     одного из альтернативных правил для нетерминала nt.
   alt-sym - символ, с которым будет связан текущий рассматриваемый элемент последовательности
             nt-alternatives.
   form - какая-то форма, в которой доступен символ alt-sym со связанным значением.
   Если form возвращает nil, то будет рассмотрен следующий символ из nt-alternatives, иначе обход
   прекращается и возвращается результат form."
  [[alt-sym nt-alternatives] form]
  `(let [#_#_rollback-count# (atom 0)]
     (-> (fn [_# ~alt-sym]
; Чтобы отладочная печать заработала необходимо раскрыть макрос for-alts (см. gen-analyzer).
           #_(when (some? ~alt-sym)
             (swap! rollback-count# inc)
             (when (and debug? (> @rollback-count# 1))
               (println "rollback №" @rollback-count#)
               (println "rollback tokens" (first ~alt-sym))
               (println "rollback outputs" (second ~alt-sym))))
           (when-let [result# ~form]
             (reduced result#)))
       (reduce nil ~nt-alternatives))))

(defmacro succ-lazy
  [chain {:keys [terms nonterms epsilon] :as grammar} tokens outputs]
  (if (seq chain)
    (let [sym (first chain)
          curr-alt (gensym "curr-alt-")
          succ-body `(when-let* [[~tokens res#] ~(cond (get terms sym) `(term? ~sym ~tokens)
                                                       (= epsilon sym) `(epsilon? ~tokens)
                                                       :else curr-alt)
                                 ~outputs (conj ~outputs res#)]
                                (succ-lazy ~(rest chain) ~grammar ~tokens ~outputs))]
      (if (get nonterms sym)
        `(for-alts [~curr-alt (~(-> (transliterate-nonterm sym)
                                    (str "?")
                                    (symbol)) ~tokens)]
                   ~succ-body)
        succ-body))
    [tokens outputs]))

(defmacro succ
  [chain {:keys [terms epsilon] :as grammar} tokens outputs]
  (if (seq chain)
    `(when-let* [[~tokens res#] ~(let [sym (first chain)]
                                   (cond (get terms sym) `(term? ~sym ~tokens)
                                         (= epsilon sym) `(epsilon? ~tokens)
                                         :else `(~(-> (transliterate-nonterm sym)
                                                      (str "?")
                                                      (symbol)) ~tokens)))
                 ~outputs (conj ~outputs res#)]
                (succ ~(rest chain) ~grammar ~tokens ~outputs))
    [tokens outputs]))

(defmacro alt-lazy [nt alt-number nt-prods grammar tokens outputs]
  (when (seq nt-prods)
    `(lazy-seq
      (cons (when-let [branch# (do (when debug? (println ~nt ~alt-number))
                                   (succ-lazy ~(first nt-prods) ~grammar ~tokens ~outputs))]
              [(first branch#) (into [~(keyword nt)] (second branch#))])
            (alt-lazy ~nt ~(inc alt-number) ~(rest nt-prods) ~grammar ~tokens ~outputs)))))

(defmacro alt [nt alt-number nt-prods grammar tokens outputs]
  (when (seq nt-prods)
    `(if-let [branch# (do (when debug? (println ~nt ~alt-number))
                          (succ ~(first nt-prods) ~grammar ~tokens ~outputs))]
       [(first branch#) (into [~(keyword nt)] (second branch#))]
       (alt ~nt ~(inc alt-number) ~(rest nt-prods) ~grammar ~tokens ~outputs))))

(defmacro nt-analyzer [nt grammar lazy?]
  (let [translit (transliterate-nonterm nt)]
    `(defn ~(symbol (str translit "?")) [tokens]
       (when debug? (println {:name ~translit :tokens tokens}))
       (let [outputs []]
         (~(if lazy? `alt-lazy `alt)
          ~translit 1 ~(-> grammar :prods (get nt)) ~grammar tokens outputs)))))

(defn simplify [program]
  (-> program
      (clojure.string/replace #"(branch|res)__\d+__auto__" "$1")
      (clojure.string/replace #"curr-alt-\d+" "curr-alt")
      (clojure.string/replace #"(outputs|\[tokens res\])\r\n\s+" "$1 ")))

(defn gen-analyzer [grammar & {:keys [ns-name lazy? debug?]
                               :or {ns-name "cc.lab03.autogen"
                                    lazy? true debug? true}}]
  (let [res (-> `((ns ~(symbol (name ns-name))
                    (:require ~@(when lazy? `([cc.lab03.generator :refer [for-alts]]))
                              [cc.lab03.helpers :refer [when-let*]]))

                  (def debug? ~debug?)

                  ~@(for [nt (:nonterms grammar)]
                      `(declare ~(-> (transliterate-nonterm nt) (str "?") (symbol))))

                  (defn term? [term tokens]
                    (when debug? (println {:name (format "term '%s'" term) :tokens tokens}))
                    (when (= term (first tokens))
                      [(rest tokens) [:term (first tokens)]]))

                  (defn epsilon? [tokens]
                    (when debug? (println {:name "epsilon" :tokens tokens}))
                    [tokens [:epsilon]])

                  ~@(for [nt (:nonterms grammar)]
                      `(cc.lab03.generator/nt-analyzer ~nt ~grammar ~lazy?)))
                (expand-selected-macros ['cc.lab03.generator/nt-analyzer
                                         'cc.lab03.generator/alt
                                         'cc.lab03.generator/succ
                                         'cc.lab03.generator/alt-lazy
                                         'cc.lab03.generator/succ-lazy
                                         #_'cc.lab03.generator/for-alts])
                (#(with-out-str
                    (apply clojure.pprint/write % [:dispatch clojure.pprint/code-dispatch
                                                   :right-margin 100
                                                   :suppress-namespaces true]))))
        res (clojure.string/replace res #"\s+\((def|declare|defn) " "\r\n\r\n($1 ")
        res (apply str (butlast (rest res)))
        res (simplify res)]
    res))
