(ns cc.lab03.generator
  (:require [cc.lab03.helpers :refer [ru->en when-let* expand-selected-macros
                                      json->grammar]]
            [clojure.string]
            [clojure.pprint]))

(defn transliterate-nonterm [nonterm]
  (-> nonterm
      (clojure.string/trim)
      (clojure.string/replace #"\s+" "-")
      (ru->en)))

#_(transliterate-nonterm "список операторов")

(def debug? false)

(defmacro rollback-alts
  "nt-alternatives - последовательность, каждый элемент которой - результат разбора (возможно nil)
                     одного из альтернативных правил для нетерминала nt.
   alt-sym - символ, с которым будет связан текущий рассматриваемый элемент последовательности
             nt-alternatives.
   form - какая-то форма, в которой доступен символ alt-sym со связанным значением.
   Если form возвращает nil, то будет рассмотрен следующий символ из nt-alternatives, иначе обход
   прекращается и возвращается результат form."
  [[alt-sym nt-alternatives] form]
  `(-> (fn [_# ~alt-sym]
         (when-let [result# ~form]
           (reduced result#)))
       (reduce nil ~nt-alternatives)))

#_(macroexpand-1 '(nt-alternatives [nt-res (spisok-operatorov? tokens)]
                                   (with-let* [[tokens res] nt-res])))

(defmacro succ
  [chain terms tokens outputs]
  (if (seq chain)
    `(when-let* [[~tokens res#] ~(if (get terms (first chain))
                                   `(term? ~(first chain) ~tokens)
                                   `(~(-> (first chain)
                                          (transliterate-nonterm)
                                          (str "?")
                                          (symbol)) ~tokens))
                 ~outputs (conj ~outputs res#)]
                (succ ~(rest chain) ~terms ~tokens ~outputs))
    [tokens outputs]))

(defmacro alt [nt nt-prods terms tokens]
  (when (seq nt-prods)
    (let [outputs (gensym "outputs-")]
      `(if-let [branch# (let [~outputs []]
                          (succ ~(first nt-prods) ~terms ~tokens ~outputs))]
         [(first branch#) (into [~(keyword nt)] (second branch#))]
         (alt ~nt ~(rest nt-prods) ~terms ~tokens)))))

(defmacro nt-analyzer [nt grammar]
  (let [translit (transliterate-nonterm nt)]
    `(defn ~(symbol (str translit "?")) [tokens]
       (alt ~translit ~(-> grammar :prods (get nt)) ~(:terms grammar) tokens))))

(defn nt-analyzer->str [nt grammar]
  (->> `(cc.lab03.generator/nt-analyzer ~nt ~grammar)
       (#(expand-selected-macros % ['cc.lab03.generator/nt-analyzer
                                    'cc.lab03.generator/alt
                                    'cc.lab03.generator/succ
                                    'cc.lab03.helpers/when-let*]))
       (#(with-out-str (clojure.pprint/write %
                                             :dispatch clojure.pprint/code-dispatch
                                             :right-margin 100
                                             :suppress-namespaces true)))))

(defn gen-analyzer [grammar ns-name]
  (let [res (str "(ns " (name ns-name) ")\r\n\r\n")
        res (reduce #(str % "(declare " (transliterate-nonterm %2) "?)\r\n")
                    res
                    (:nonterms grammar))
        res (str res "\r\n")
        res (reduce #(str % (nt-analyzer->str %2 grammar) "\r\n\r\n")
                    res
                    (:nonterms grammar))]
    res))

(spit "autogen.clj" (gen-analyzer (json->grammar "resources/grammar.json") "cc.lab03.autogen"))

; (->> #_'(cc.lab03.generator/alt :bbb #{["1" "2"] ["3"]} #{"1"} (gensym "tokens-"))
;        `(cc.lab03.generator/nt-analyzer "простое выражение" ~(json->grammar "resources/grammar.json"))
;      (#(expand-selected-macros % ['cc.lab03.generator/nt-analyzer
;                                   'cc.lab03.generator/alt
;                                   'cc.lab03.generator/succ
;                                   'cc.lab03.helpers/when-let*]))
;      (#(with-out-str (clojure.pprint/write %
;                                            :dispatch clojure.pprint/code-dispatch
;                                            :right-margin 100
;                                            :suppress-namespaces true)))
;      #_(#(clojure.string/replace % #"clojure.core/" ""))
;      (spit "autogen.clj"))
