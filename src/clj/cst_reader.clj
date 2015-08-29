(ns cst-reader
  (require [clojure.java.io :as io])
  (import [java.io PushbackReader StringReader Writer]
          [cst LispReader SyntaxElement]))

; (defmethod print-method cst.CommaSyntax [o ^Writer w]
;(.write w ","))

(defmethod print-method SyntaxElement [^SyntaxElement o ^Writer w]
  (let [writer-print (fn [^String s]
                       (dotimes [n (count s)]
                         (.write w (int (.charAt s n)))))]
    (writer-print (str o))))

(defn cst-read
  " Reads the stream into a cst structure form "
  [io]
  (LispReader/read io nil))

(defn cst-read-string
  " Read a string in a cst structure form "
  [s]
  (with-open [io (PushbackReader. (StringReader. s))]
    (cst-read io)))