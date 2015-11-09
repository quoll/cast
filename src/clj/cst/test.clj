(ns cst.test
  (:require [cst.reader :as reader]
            [cst.database :as cdb]
            [datomic.api :refer [q] :as db]))

(def test-data "(list 1 2 3) [:a :b]")

(defn main- [& args]
  (let [db (cdb/database cdb/dburl)
        cst-data (reader/cst-read-all-string test-data)
        tx-data (map cdb/tx-data (.data cst-data))]
    (println "TX:")
    (println tx-data)
    (println "----")
    ;; (println (db/transact db tx-data))
    )
  )
