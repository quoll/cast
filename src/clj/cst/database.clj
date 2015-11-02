(ns cst.database
  (:require [cst.data :as data]
            [datomic.api :refer [q] :as d])
  (:import [datomic Peer]
           [clojure.lang Keyword]
           [java.util Date UUID Map]
           [java.net URI]
           [java.math BigInteger BigDecimal]
           (sun.jvm.hotspot.oops BitData)))

(def dburl "datomic:dev://localhost:4334/source")

(defn load-schema
  "Loads the cast schema into a database connection"
  [c]
  (d/transact c data/schema))

(defn database
  "Creates and initializes a database, returning a connection"
  [uri]
  (let [newdb (d/create-database uri)
        c (d/connect uri)]
    (when newdb (load-schema c))
    c))

(defn node [] (Peer/tempid :db.part/cst))

(defprotocol WithProperty
  (data-property [x] "Return the correct property to use for x"))

; [:keyword :string :boolean :long :bigint :float :double :bigdec :instant :uuid :uri]
(extend-protocol WithProperty
  Keyword
  (data-property [_] :cst.value/keyword)
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
  (data-property [_] :cst.value/ref))

(defn list-struct
  "Converts a seq into a transaction seq, of a provided type and using a given function for seq elements.
  The first element in the transaction seq is the node representing the seq."
  [s t efn]
  (let [elements-data (map-indexed efn s)
        aux (apply concat (map second elements))
        elements (apply concat (map first elements))]
    (concat [{:db/id (node), :cst/type t, :cst/element (map :db/id elements)}] aux elements)))

(defn list-data
  "Takes a sequence, and returns a seq of transaction elements."
  [s]
  (list-struct s :list (fn [n o]
                         (let [[d aux] (object-data o)]
                           [{:db/id (node)
                             :cst/index n
                             (data-property o) d}
                            aux]))))

(defprotocol Data
  (object-data [x] "Returns a single value suitable for a transaction,\
   paired with a seq of any supporting transaction data"))

(extend-protocol Data
  Object
  (object-data [x] [x []])
  Map
  (object-data [x]
    (let [[n & tx] (list-struct x :map (fn [n [k v]]
                                         (let [[vd vaux] (object-data v)
                                               [kd kaux] (object-data k)]
                                           [{:db/id     (node)
                                             :cst/index n
                                             :cst/key   kd
                                             :cst/value vd}
                                            (concat vaux kaux)])))]
      [n tx])))

(defn tx-data
  "Convert an object into transaction data"
  [obj]
  )

