(ns cst.reader
  (require [clojure.java.io :as io])
  (import [java.io PushbackReader StringReader Writer]
          [java.util UUID]
          [java.net URI]
          [cst LispReader SyntaxElement SyntaxElement$Type]))

(defmethod print-method SyntaxElement [^SyntaxElement o ^Writer w]
  (let [writer-print (fn [^String s]
                       (dotimes [n (count s)]
                         (.write w (int (.charAt s n)))))]
    (writer-print (str o))))

(defn new-location [] (URI. (str "uuid:" (UUID/randomUUID))))

(defn cst-read-all
  "Reads an entire string into a sequence of elements"
  ([io] (cst-read-all io (new-location)))
  ([io location] (cst-read-all io location nil))
  ([io location external-opts]
   (let [eof (Object.)
         opts (merge external-opts {:eof eof})]
     (loop [element (LispReader/read io opts) context []]
       (if (= eof element)
         (SyntaxElement. SyntaxElement$Type/FILE {:data context :location location})
         (recur (LispReader/read io opts) (conj context element)))))))

(defn cst-read-all-string
  "Read all forms from a string into a seq of CST structures"
  ([^String s] (cst-read-all-string s (new-location) nil))
  ([^String s location] (cst-read-all-string s location nil))
  ([^String s location opts]
   (with-open [io (PushbackReader. (StringReader. s))]
     (cst-read-all io location opts))))

(defn cst-read
  "Reads the first element parsed from the stream into a single cst structure form"
  ([^PushbackReader io] (cst-read io nil))
  ([^PushbackReader io opts] (LispReader/read io opts)))

(defn cst-read-string
  "Read the first element parsed from the a string in a single cst structure form"
  ([^String s] (cst-read-string s nil))
  ([^String s opts]
    (with-open [io (PushbackReader. (StringReader. s))]
     (cst-read io opts))))

