(ns cc.lab03.generator
  (:require [cc.lab03.helpers :refer [ru->en when-let* expand-selected-macros]]
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

(defmacro succ
  [chain tokens outputs]
  (if (seq chain)
    `(when-let* [[~tokens res#] (term? ~(first chain) ~tokens)
                 ~outputs (conj ~outputs res#)]
                (succ ~(rest chain) ~tokens ~outputs))
    [tokens outputs]))

(defmacro alt [rhs-set]
  (when (seq rhs-set)
    (let [tokens (gensym "tokens-")
          outputs (gensym "outputs-")]
      `(if-let [branch# (let [~outputs []]
                          (succ ~(first rhs-set) ~tokens ~outputs))]
         [(first branch#) (into [:a] (second branch#))]
         (alt ~(rest rhs-set))))))

#_(->> '(cc.lab03.generator/alt #{["1" "2"] ["3"]})
     (#(expand-selected-macros % ['cc.lab03.generator/alt
                                  'cc.lab03.generator/succ]))
     (#(with-out-str (clojure.pprint/write %
                                           :dispatch clojure.pprint/code-dispatch
                                           :right-margin 100
                                           :suppress-namespaces true)))
     #_(#(clojure.string/replace % #"clojure.core/" ""))
     (spit "autogen.clj"))
