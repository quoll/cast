(ns cst.data
  (:require [clojure.string :as str])
  (:import [datomic Peer]
           [cst SyntaxElement$Macro]))

(def std-types [:keyword :string :boolean :long :bigint :float :double :bigdec :instant :uuid :uri])

(def types (cons {:db/id (Peer/tempid :db.part/db)
                  :db/ident :cst.value/object
                  :db/valueType :db.type/ref
                  :db/cardinality :db.cardinality/one
                  :db.install/_attribute :db.part/db}
             (map #(let [nm (name %)]
                        {:db/id                 (Peer/tempid :db.part/db)
                         :db/ident              (keyword "cst.value" nm)
                         :db/valueType          (keyword "db.type" nm)
                         :db/cardinality        :db.cardinality/one
                         :db.install/_attribute :db.part/db})
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
    :db/cardinality :db.cardinality/many
    :db.install/_attribute :db.part/db}
   {:db/id (Peer/tempid :db.part/db)
    :db/ident :cst/index
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}])

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
         (SyntaxElement$Macro/values))
    {:db/id (Peer/tempid :db.part/cst)
     :db/ident :native}))

