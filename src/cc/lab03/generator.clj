(ns cc.lab03.generator
  (:require [cc.lab03.helpers :refer [ru->en when-let* expand-selected-macros
                                      json->grammar]]
            [clojure.string]
            [clojure.pprint]))

(defn transliterate-nonterm [nonterm]
  (-> nonterm
      (clojure.string/trim)
      (clojure.string/replace #"\s+" "-")
      (ru->en)
      (#(if (#{"debug" "term"} %)
          (throw (Exception. (str "Недопустимое имя '" nonterm "' для нетерминала.")))
          %))))

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
  `(let [rollback-count# (atom 0)]
     (-> (fn [_# ~alt-sym]
           (when (some? ~alt-sym)
             (swap! rollback-count# inc)
             (when (and debug? (> @rollback-count# 1))
               (println "rollback №" @rollback-count#)
               (println (first ~alt-sym))
               (println (second ~alt-sym))))
           (when-let [result# ~form]
             (reduced result#)))
       (reduce nil ~nt-alternatives))))

#_(macroexpand-1 '(nt-alternatives [nt-res (spisok-operatorov? tokens)]
                                   (with-let* [[tokens res] nt-res])))

(defmacro succ
  [chain terms tokens outputs]
  (if (seq chain)
    (if (get terms (first chain))
      `(when-let* [[~tokens res#] (term? ~(first chain) ~tokens)
                   ~outputs (conj ~outputs res#)]
                  (succ ~(rest chain) ~terms ~tokens ~outputs))
      `(rollback-alts [nt-res# (~(-> (first chain)
                                    (transliterate-nonterm)
                                    (str "?")
                                    (symbol)) ~tokens)]
                      (when-let* [[~tokens res#] nt-res#
                                  ~outputs (conj ~outputs res#)]
                                 (succ ~(rest chain) ~terms ~tokens ~outputs))))
    [tokens outputs]))

(defmacro alt [nt alt-number nt-prods terms tokens outputs]
  (when (seq nt-prods)
    `(lazy-seq (cons (do (when debug? (println ~nt ~alt-number))
                         (when-let [branch# (succ ~(first nt-prods) ~terms ~tokens ~outputs)]
                           [(first branch#) (into [~(keyword nt)] (second branch#))]))
                     (alt ~nt ~(inc alt-number) ~(rest nt-prods) ~terms ~tokens ~outputs)))))

(defmacro nt-analyzer [nt grammar]
  (let [translit (transliterate-nonterm nt)]
    `(defn ~(symbol (str translit "?")) [tokens]
       (when debug? (println {:name ~translit :tokens tokens}))
       (let [outputs []]
         (alt ~translit 1 ~(-> grammar :prods (get nt)) ~(:terms grammar) tokens outputs)))))

(def pprint-options [:dispatch clojure.pprint/code-dispatch
                     :right-margin 100
                     :suppress-namespaces true])

(defn nt-analyzer->str [nt grammar]
  (-> `(cc.lab03.generator/nt-analyzer ~nt ~grammar)
       (expand-selected-macros ['cc.lab03.generator/nt-analyzer
                                'cc.lab03.generator/alt
                                'cc.lab03.generator/succ
                                #_'cc.lab03.generator/rollback-alts])
       (#(with-out-str (apply clojure.pprint/write % pprint-options)))))

(defn term? [term tokens]
  (when debug? (println {:name (format "term `%s`" term) :tokens tokens}))
  (when (= term (first tokens))
    [(rest tokens) [:term (first tokens)]]))

(defn gen-analyzer [grammar ns-name]
  (let [res (-> `(ns ~(symbol (name ns-name))
                   (:require [cc.lab03.generator :refer [rollback-alts]]
                             [cc.lab03.helpers :refer [when-let*]]))
                (#(with-out-str (apply clojure.pprint/write % pprint-options)))
                (str "\r\n\r\n"))
        res (reduce #(str % "(declare " (transliterate-nonterm %2) "?)\r\n")
                    res
                    (:nonterms grammar))
        res (str res "\r\n(def debug? true)\r\n\r\n")
        res (-> '(defn term? [term tokens]
                   (when debug? (println {:name (format "term '%s'" term) :tokens tokens}))
                   (when (= term (first tokens))
                     [(rest tokens) [:term (first tokens)]]))
                (#(with-out-str (apply clojure.pprint/write % pprint-options)))
                (#(str res % "\r\n\r\n")))
        res (reduce #(str % (nt-analyzer->str %2 grammar) "\r\n\r\n")
                    res
                    (:nonterms grammar))]
    res))

(defn simplify [program]
  (-> program
      (clojure.string/replace #"branch__\d+__auto__" "branch")
      (clojure.string/replace #"nt-res__\d+__auto__" "nt-res")
      (clojure.string/replace #" res__\d+__auto__" " res")
      (clojure.string/replace #"\[tokens res\]\r\n\s+" "[tokens res] ")
      (clojure.string/replace #"outputs\r\n\s+" "outputs ")))

(-> (json->grammar "resources/grammar-simple.json")
    (gen-analyzer "cc.lab03.autogen")
    (simplify)
    (->> (spit "src/cc/lab03/autogen.clj")))
