(ns cst.database
  (:require [cst.data :as data]
            [datomic.api :refer [q] :as d]))

(def dburl "datomic:dev://localhost:4334/source")

(defn load-schema
  [c]
  (d/transact c data/schema))

(defn database [uri]
  (when (d/create-database uri)
    (load-schema uri))
  (d/connect uri))
