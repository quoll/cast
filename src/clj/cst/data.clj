(ns cst.data
  (:import [datomic Peer]))

(def std-types [:keyword :string :boolean :long :bigint :float :double :bigdec :instant :uuid :uri])

(def types (map #(let [nm (name %)]
                  {:db/id (Peer/tempid :db.part/db)
                   :db/ident (keyword "cst" nm)
                   :db/valueType (keyword "db.type" nm)
                   :db/cardinality :db.cardinality/one
                   :db.install/_attribute :db.part/db})
                std-types))

(def basic-schema
  [{:db/id (Peer/tempid :db.part/db)
    :db/ident :cst/type
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id (Peer/tempid :db.part/db)
    :db/ident :cst/element
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db.install/_attribute :db.part/db}
   {:db/id (Peer/tempid :db.part/db)
    :db/ident :cst/index
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id (Peer/tempid :db.part/db)
    :db/ident :cst.value/object
    :db/valueType :db.type/object
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}])

(def schema (concat schema types))

