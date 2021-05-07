(ns cc.lab03.helpers-test
  (:require [clojure.test :refer [deftest is]]
            [cc.lab03.helpers :refer [ru->en upper-case?]]))

(deftest ru->en-test
  (is (= "Nikita" (ru->en "Никита")))
  (is (= "" (ru->en "ььь")))
  (is (= "" (ru->en nil)))
  (is (= "Nikita" (ru->en "Nikita")))
  (is (= "NikitaNikitovich" (ru->en "NikitaНикитович"))))

(deftest upper-case?-test
  (is (thrown? Exception (upper-case? nil)))
  (is (= nil (upper-case? "*")))
  (is (= true (upper-case? "N")))
  (is (= true (upper-case? "Ж")))
  (is (= false (upper-case? "з")))
  (is (= false (upper-case? "z")))
  (is (= false (upper-case? \з))))
