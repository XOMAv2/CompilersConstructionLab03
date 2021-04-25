(ns cc.lab03.helpers
  (:require [clojure.data.json :as json]
            [camel-snake-kebab.core :as csk]
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

