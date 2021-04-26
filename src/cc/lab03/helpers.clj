(ns cc.lab03.helpers
  (:require [clojure.data.json :as json]
            [camel-snake-kebab.core :as csk]
            [clojure.string]
            [clojure.walk]
            [clojure.set]))

(defn format-top-level-keys [hmap format-fn]
  (->> hmap
       (keys)
       (mapcat #(vector % (format-fn %)))
       (apply hash-map)
       (clojure.set/rename-keys hmap)))

(defn json->grammar [path]
  (let [json-string (slurp path)
        grammar (json/read-str json-string
                               :key-fn str)
        grammar (format-top-level-keys grammar
                                       csk/->kebab-case-keyword)
        grammar (-> grammar
                    (update :terms set)
                    (update :nonterms set))
        prods (->> (:prods grammar)
                   (mapcat (fn [[k v]]
                             [k (set v)]))
                   (apply hash-map))]
    (assoc grammar :prods prods)))

(defn grammar->json [path grammar]
  (let [json-string (-> grammar
                        (format-top-level-keys csk/->camelCaseString)
                        (json/write-str :key-fn name :escape-unicode false))]
    (spit path json-string)))

#_(grammar->json "resources/grammar-res.json" (json->grammar "resources/grammar.json"))

(defn upper-case? [input]
  (when (not= (clojure.string/upper-case input)
              (clojure.string/lower-case input))
    (= (str input) (clojure.string/upper-case input))))

(def ru->en-map {"а" "a", "б" "b", "в" "v", "г" "g", "д" "d", "е" "e", "ё" "e", "ж" "j", "з" "z",
                 "и" "i", "й" "i", "к" "k", "л" "l", "м" "m", "н" "n", "о" "o", "п" "p", "р" "r",
                 "с" "s", "т" "t", "у" "u", "ф" "f", "х" "h", "ц" "c", "ч" "ch", "ш" "sh", "щ" "sc",
                 "ъ" "", "ы" "y", "ь" "", "э" "e", "ю" "iu", "я" "iu"})

(defn ru->en
  "Транслитерация в формате международной телеграммы."
  [input]
  (apply str (map #(if (upper-case? %)
                     (->> %
                          (str)
                          (clojure.string/lower-case)
                          (get ru->en-map)
                          ((fn [sym] (if (nil? sym) % (clojure.string/upper-case sym)))))
                     (->> %
                          (str)
                          (get ru->en-map)
                          ((fn [sym] (if (nil? sym) % sym)))))
                  input)))

(defmacro when-let*
  ([bindings & body]
   (if (seq bindings)
     `(when-let [~(first bindings) ~(second bindings)]
        (when-let* ~(drop 2 bindings) ~@body))
     `(do ~@body))))

(defn expand-selected-macros
  [form namespaced-syms]
  (clojure.walk/postwalk (fn [el]
                           (if (and (or (seq? el)
                                        (list? el))
                                    (some #{(first el)} namespaced-syms))
                             (expand-selected-macros (macroexpand-1 el) namespaced-syms)
                             el))
                         form))
