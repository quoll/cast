(ns cst.reader-test
  (:use [clojure.test]
        [cst.reader])
  (:import [cst SyntaxElement]))

(defn roundtrip
  [s]
  (let [cst (cst-read-string s)]
    (is (= s (SyntaxElement/emit cst)))))

(deftest simple-roundtrip
  (roundtrip "[1 2 3]")
  (roundtrip "(1 2 3)")
  (roundtrip "#{1 2 3}")
  (roundtrip "\"1 2 3\"")
  (roundtrip "\"1 \\\"2 3\"")
  (roundtrip "{:a 1 :b 2 :c 3}")
  (roundtrip "{:a 1, :b 2, :c 3}")
  (roundtrip "a")
  (roundtrip "'a")
  (roundtrip "\"a\"")
  (roundtrip "\"a\"")
  (roundtrip "#\"a\"")
  (roundtrip "#\"a\\(\"")
  (roundtrip "(.toString [1 2])")
  (roundtrip "#(= 5 %)")
  (roundtrip "#(= %1 %2)"))

(deftest nested-roundtrip
  (roundtrip "[[1] [2]]")
  (roundtrip "[{:a 1 :b 2} {:a 1 :c 3}]")
  (roundtrip "[{:a 1 :b 2} '(1 3)]")
  (roundtrip "(let [^String x (.toString y)] x)"))
