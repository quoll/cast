(ns cst.database
  (:require [cst.schema :as data]
            [cst.path :as path]
            [datomic.api :refer [q] :as d])
  (:import [datomic Peer]
           [datomic.db DbId]
           [clojure.lang Keyword Symbol IPersistentList IPersistentVector IPersistentMap]
           [java.util Date UUID Map]
           [java.net URI]
           [java.math BigInteger BigDecimal]
           [cst SyntaxElement SyntaxElement$Type]))

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

(declare list-data)

(extend-protocol Data
  Object
  (object-data [x] [x []])
  SyntaxElement
  (object-data [^SyntaxElement x]
    (let [etype (. x id)
          data (. x data)
          node-id (node)]
      (cond
        (= :cst/file etype) (let [location (or (path/to-uri (:location data))
                                               (URI. (str "uuid:" (UUID/randomUUID))))]
                              (list-data (:data data) :file node-id location)) ;; list structure for the file contents
        (= :cst/conditional etype) (let [[o auxo] (object-data (:form data))
                                         splice? (:splice data)
                                         condo {:db/id node-id
                                                :cst/type etype
                                                :cst.cond/splice splice?
                                                :cst.cond/form o}]
                                     [node-id (concat auxo [condo])])
        data (let [[d auxd] (object-data data)]
               [node-id (concat auxd [(assoc {:db/id node-id, :cst/type etype}
                                        (data-property d) (smb d))])])
        :default [node-id []])))
  IPersistentList
  (object-data [^IPersistentList x] (list-data x :list (node)))
  IPersistentVector
  (object-data [^IPersistentVector x] (list-data x :vector (node)))
  IPersistentMap
  (object-data [^IPersistentMap x] (list-data (seq x) :map (node))))


(defn- list-struct
  [[head & tail] pre-node]
  (let [pre-node (or pre-node (node))
        [d aux] (object-data head)
        list-elt {:db/id pre-node, (data-property d) (smb d)}]
    ;; if d is an object, then drop it in as a replacement at the end, or add rest to it
    (if-not (seq tail)
      [[list-elt aux]]
      (let [next-node (node)]
        (lazy-seq (cons [(assoc list-elt :cst/rest next-node) aux]
                        (list-struct tail next-node)))))))

(defn list-data
  "Converts a seq into a transaction seq, of a provided type and using a given function for seq elements.
  s - sequence
  t - type
  l - location
  The provided function returns a pair: [list-element, auxiliary-data]
  Returns a pair: [list-ID, tx-sequence]."
  ([s t n] (list-data s t n nil))
  ([s t n l]
   (let [elt-data (list-struct s n)
         aux (apply concat (map second elt-data))
         [{list-id :db/id :as head} & srest] (map first elt-data)
         head (assoc head :cst/type t)
         head (if l (assoc head :cst/location l) head)]
     [list-id (concat aux (cons head srest))])))

(defn tx-data
  "Convert an object into transaction data. The final item is always the Object."
  [obj]
  (let [[element aux] (object-data obj)]
    (if (instance? DbId element)
      aux
      (concat aux
              [(if (map? element)
                 element
                 {:db/id                  (node)
                  :cst/type               :native
                  (data-property element) (smb element)})]))))

(declare reconstruct)

; "Creates a value from v based on the type associated with the property p"
(defmulti value-of (fn [p v] p))

(defmethod value-of :cst.value/symbol
  [p ^String v]
  (symbol v))

(defmethod value-of :cst.value/object
  [p m]
  (reconstruct m))

(defmethod value-of :default [p m] m)

(defn value-fn
  "Retrieves the value from a structure, "
  [e]
  (some (fn [[k v]] (if (= (namespace k) "cst.value")
                      (value-of k v)))
        e))

(defn rebuild-list
  [l]
  (map value-fn (sort-by :cst/index l)))

(defmulti reconstruct :cst/type)

(defmethod reconstruct :file
  [f]
  (let [elements (rebuild-list (:cst/element f))]
    (SyntaxElement. SyntaxElement$Type/FILE (map reconstruct elements))))

(defmethod reconstruct :vector
  [v]
  (let [elements (rebuild-list (:cst/element v))]
    (SyntaxElement. SyntaxElement$Type/VECTOR (apply vector (map reconstruct elements)))))

(defmethod reconstruct :list
  [l]
  (let [elements (rebuild-list (:cst/element l))]
    (SyntaxElement. SyntaxElement$Type/LIST (map reconstruct elements))))

(defmethod reconstruct :map
  [m]
  (let [elements (rebuild-list (:cst/element m))]
    (SyntaxElement. SyntaxElement$Type/MAP (map reconstruct elements))))

(defmethod reconstruct :conditional
  [c]
  (let [form (reconstruct (:cst.cond/form c))
        splice? (:cst.cond/splice c)]
    (SyntaxElement. SyntaxElement$Type/CONDITIONAL {:splice splice?, :form form})))

(defmethod reconstruct :default [v] v)

(defn get-filenames
  "Retrieves the locations (or paths) for each file stored in the database."
  [db]
  (q '[:find [?l ...] :where [?e :cst/type :file] [?e :cst/location ?l]] db))

(defn get-cst
  "Retrieves the Concrete Syntax Tree for a file location. Returns nil if the location is unknown."
  [db location]
  (when-let [eid (q '[:find ?e . :in $ ?l :where [?e :cst/location ?l] [?e :cst/type :file]]
                    db
                    (path/to-uri location))]
    (let [fdata (d/pull db '[*] eid)]
      (reconstruct fdata))))

