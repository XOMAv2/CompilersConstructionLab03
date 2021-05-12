(ns cc.lab03.core
  (:require [cc.lab03.generator :refer [gen-analyzer]]
            [cc.lab03.analyzer :refer [programma?]]
            [cc.lab03.helpers :refer [json->grammar]]
            [clojure.string])
  (:gen-class))

(defn -main
  [path]
  (let [program (slurp path)
        tokens (-> program
                   (clojure.string/trim)
                   (clojure.string/replace #"\s+" " ")
                   (clojure.string/split #" "))]
    (second (programma? tokens))))

#_(-main "resources/program.lab")

(comment
  "Прежде чем использовать анализатор его необходимо сгенерировать, а затем скомпилировать."
  (-> (json->grammar "resources/grammar.json")
      (gen-analyzer :debug? false :lazy? true :ns-name "cc.lab03.autogen")
      (->> (spit "src/cc/lab03/autogen.clj"))))

(comment
  "Токены для грамматики grammar-simple.json."
  (auto/programma? ["{"
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
                    "}"]))
