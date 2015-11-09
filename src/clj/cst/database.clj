(ns cst.database
  (:require [cst.data :as data]
            [datomic.api :refer [q] :as d])
  (:import [datomic Peer]
           [datomic.db DbId]
           [clojure.lang Keyword Symbol IPersistentList IPersistentVector]
           [java.util Date UUID Map]
           [java.net URI]
           [java.math BigInteger BigDecimal]
           [cst SyntaxElement]))

(def dburl "datomic:dev://localhost:4334/source")

(defn load-schema
  "Loads the cast schema into a database connection"
  [c]
  (d/transact c data/schema)
  (d/transact c data/reader-macros))

(defn database
  "Creates and initializes a database, returning a connection"
  ([] (database dburl))
  ([uri]
   (let [newdb (d/create-database uri)
         c (d/connect uri)]
     (when newdb (load-schema c))
     c)))

(defn node [] (Peer/tempid :db.part/cst))

(defprotocol WithProperty
  (data-property [x] "Return the correct property to use for x"))

(extend-protocol WithProperty
  Keyword
  (data-property [_] :cst.value/keyword)
  Symbol
  (data-property [_] :cst.value/symbol)
  String
  (data-property [_] :cst.value/string)
  Boolean
  (data-property [_] :cst.value/boolean)
  Long
  (data-property [_] :cst.value/long)
  BigInteger
  (data-property [_] :cst.value/bigint)
  Float
  (data-property [_] :cst.value/float)
  Double
  (data-property [_] :cst.value/double)
  BigDecimal
  (data-property [_] :cst.value/bigdec)
  Date
  (data-property [_] :cst.value/instant)
  UUID
  (data-property [_] :cst.value/uuid)
  URI
  (data-property [_] :cst.value/uri)
  Object
  (data-property [_] :cst.value/object))

(defprotocol ConvertedToDb
  (smb [x] "Converts the parameter to a type that can be stored in the database"))

(extend-protocol ConvertedToDb
  Symbol
  (smb [x] (name x))
  Object
  (smb [x] x))

(defprotocol Data
  (object-data [x] "Returns a single value suitable for a transaction,
   paired with a seq of any supporting transaction data"))

(defn list-struct
  "Converts a seq into a transaction seq, of a provided type and using a given function for seq elements.
  The provided function returns a pair: [list-element, auxiliary-data]
  Returns a pair: [list-ID, tx-sequence]."
  [s t l efn]
  (let [elt-data (map-indexed efn s)
        aux (apply concat (map second elt-data))
        elements (map first elt-data)
        list-id (node)
        list-element {:db/id list-id, :cst/type t, :cst/element (map :db/id elements)}
        list-element (if l (assoc list-element :cst/location l) list-element)]
    [list-id (concat aux elements [list-element])]))

(defn list-data
  "Takes a sequence and its type id, and returns a seq of transaction elements."
  ([s type] (list-data s type nil))
  ([s type loc]
   (list-struct s type loc (fn [n o]
                             (let [[d aux] (object-data o)]
                               [{:db/id            (node)
                                 :cst/index        n
                                 (data-property d) (smb d)}
                                aux])))))

(extend-protocol Data
  Object
  (object-data [x] [x []])
  Map
  (object-data [x]
    (list-struct x :map nil (fn [n [k v]]
                              (let [[vd vaux] (object-data v)
                                    [kd kaux] (object-data k)]
                                [{:db/id     (node)
                                  :cst/index n
                                  :cst/key   kd
                                  :cst/value vd} (concat vaux kaux)]))))
  SyntaxElement
  (object-data [^SyntaxElement x]
    (let [node-id (node)
          etype (. x id)
          data (. x data)
          s {:db/id    node-id
             :cst/type etype}]
      (if (= :cst/file etype)                               ;; file elements are treated differently
        (list-data (:data data) :file (:location data))     ;; list structure for the file contents
        (if data
          (let [[d auxd] (object-data data)]
            [node-id (concat auxd [(assoc s (data-property d) (smb d))])])
          [node-id []]))))
  IPersistentList
  (object-data [^IPersistentList x] (list-data x :list))
  IPersistentVector
  (object-data [^IPersistentVector x] (list-data x :vector)))

(defn tx-data
  "Convert an object into transaction data. The final item is always the Object."
  ([obj]
   (let [location (URI. (str "uuid:" (UUID/randomUUID)))]
     (tx-data obj location)))
  ([obj location]
   (let [[element aux] (object-data obj)]
     (if (instance? DbId element)
       aux
       (concat aux
               [(if (map? element)
                  element
                  {:db/id                  (node)
                   :cst/type               :native
                   (data-property element) (smb element)})])))))

