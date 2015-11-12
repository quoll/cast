(ns cst.schema
  (:require [clojure.string :as str])
  (:import [datomic Peer]
           [cst SyntaxElement$Type]))

(def std-types [:keyword :string :boolean :long :bigint :float :double :bigdec :instant :uuid :uri])

(def types (concat [{:db/id                 (Peer/tempid :db.part/db)
                     :db/ident              :cst.value/object
                     :db/valueType          :db.type/ref
                     :db/cardinality        :db.cardinality/one
                     :db/isComponent        true
                     :db.install/_attribute :db.part/db}
                    {:db/id                 (Peer/tempid :db.part/db)
                     :db/ident              :cst.value/symbol
                     :db/valueType          :db.type/string
                     :db/cardinality        :db.cardinality/one
                     :db/fulltext           true
                     :db.install/_attribute :db.part/db}]
                   (map (fn [a]
                          (let [nm (name a)
                                attr {:db/id                 (Peer/tempid :db.part/db)
                                      :db/ident              (keyword "cst.value" nm)
                                      :db/valueType          (keyword "db.type" nm)
                                      :db/cardinality        :db.cardinality/one
                                      :db.install/_attribute :db.part/db}]
                            (if (= :string a)
                              (assoc attr :db/fulltext true)
                              attr)))
                        std-types)))

(def basic-schema
  [{:db/id (Peer/tempid :db.part/db)
    :db/ident :cst/type
    :db/valueType :db.type/keyword                          ;; TODO: change to ref
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id (Peer/tempid :db.part/db)
    :db/ident :cst/element
    :db/valueType :db.type/ref
    :db/isComponent true
    :db/cardinality :db.cardinality/many
    :db.install/_attribute :db.part/db}
   {:db/id (Peer/tempid :db.part/db)
    :db/ident :cst/index
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id (Peer/tempid :db.part/db)
    :db/ident :cst/location
    :db/valueType :db.type/uri
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db.install/_attribute :db.part/db}
   {:db/id (Peer/tempid :db.part/db)
    :db/ident :cst.cond/form
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id (Peer/tempid :db.part/db)
    :db/ident :cst.cond/splice
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   ])

(def partitions
  [{:db/id (Peer/tempid :db.part/db)
    :db/ident :db.part/cst
    :db.install/_partition :db.part/db}])

(def schema (concat partitions basic-schema types))

(def reader-macros
  (conj
    (map (fn [e]
           {:db/id    (Peer/tempid :db.part/cst)
            :db/ident (keyword (str/lower-case (.name e)))})
         (SyntaxElement$Type/values))
    {:db/id (Peer/tempid :db.part/cst)
     :db/ident :native}))

