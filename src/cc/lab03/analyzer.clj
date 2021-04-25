(ns cc.lab03.analyzer)

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