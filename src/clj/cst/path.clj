(ns cst.path
  (:require [clojure.string :as str])
  (:import [java.net URL URI]
           [java.io File]))

(def file-schema "file:")

(defn has-schema
  "Simple test for a schema (RFC 3986)"
  [s]
  (re-find #"^[a-zA-Z][a-zA-Z+\-.]*:" s))

(defprotocol URIable
  (to-uri [x] "Converts the value to a URI"))

(extend-protocol URIable
  String
  (to-uri [^String x]
    (if (has-schema x)
      (URI. x)
      (URI. (str file-schema x))))
  URL
  (to-uri [^URL x] (.toURI x))
  URI
  (to-uri [x] x))
