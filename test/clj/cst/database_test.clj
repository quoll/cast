(ns cst.database-test
  (:use [clojure.test]
        [clojure.pprint]
        [util.macro]
        [cst.database]
        [cst.reader]
        [datomic.api :refer [q] :as d])
  (:require [clojure.string :as str]
            [cst.path :as path])
  (:import [datomic.db DbId]
           [datomic.query EntityMap]
           (java.util Map)))

(defn blankify-nodes                                        ;; TODO Skolemize for more detailed comparisons
  [m]
  (letfn [(to-blank [n]
                    (cond
                      (instance? DbId n) :blank
                      (sequential? n) (map to-blank n)
                      :default n))]
    (into {} (keep (fn [[k v]]
                     (if (not= k :cst/location)
                       [k (to-blank v)]
                       (if-not (is (str/starts-with? (str v) "uuid:"))
                         [k v])))
                   m))))

(deftest simple-list
  (let [[id-node the-data] (list-data [1 2] :vector)
        elts (map blankify-nodes the-data)
        node (last the-data)
        bnode (last elts)]
    (is (= id-node (:db/id node)))
    (is (= {:db/id :blank
            :cst/type :vector
            :cst/element [:blank :blank]}
           bnode))
    (is (every? #(= :blank (:db/id %)) elts))
    (is (every? #(= (dec (:cst.value/long %)) (:cst/index %)) (take 2 elts)))
    (is (= (count elts) 3))))

(deftest natives
  (let [tx (tx-data (cst-read-all-string "5"))
        btx (map blankify-nodes tx)]
    (is (= [{:db/id          :blank
             :cst/index      0
             :cst.value/long 5}
            {:db/id       :blank
             :cst/type    :file
             :cst/element [:blank]}] btx)))
  (let [tx (tx-data (cst-read-all-string "5\n:foo"))
        btx (map blankify-nodes tx)]
    (is (= [{:db/id          :blank
             :cst/index      0
             :cst.value/long 5}
            {:db/id          :blank
             :cst/index      1
             :cst.value/keyword :foo}
            {:db/id       :blank
             :cst/type    :file
             :cst/element [:blank :blank]}] btx))))

(deftest short-ns
  (let [fhello "(ns cst.test-hello)\n(println \"Hello world\")"
        chello (cst-read-all-string fhello)
        tx (tx-data chello)
        btx (map blankify-nodes tx)]
    (is (= [{:db/id :blank :cst/index 0 :cst.value/symbol "ns"}
            {:db/id :blank :cst/index 1 :cst.value/symbol "cst.test-hello"}
            {:db/id :blank :cst/type :list :cst/element [:blank :blank]}
            {:db/id :blank :cst/index 0 :cst.value/symbol "println"}
            {:db/id :blank :cst/index 1 :cst.value/string "Hello world"}
            {:db/id :blank :cst/type :list :cst/element [:blank :blank]}
            {:db/id :blank :cst/index 0 :cst.value/object :blank}
            {:db/id :blank :cst/index 1 :cst.value/object :blank}
            {:db/id :blank :cst/type :file :cst/element [:blank :blank]}]
           btx))))

(deftest save-program
  (let [fhello "(ns cst.test-hello)\n(println \"Hello world\")"
        chello (cst-read-all-string fhello)
        tx (tx-data chello)
        cnx (database "datomic:mem://src")
        _ (d/transact cnx tx)
        db (d/db cnx)
        pid (q '[:find ?pid . :where [?pid :cst/type :file]] db)
        prog (d/touch (d/entity db pid))
        p (d/pull db '[*] pid)]
     (pprint p)
    ))

(defmacro with-connection [bindings & body]
  (assert-args
    (vector? bindings) "a vector for its binding"
    (even? (count bindings)) "an even number of forms in binding vector")
  (cond
    (= (count bindings) 0) `(do ~@body)
    (symbol? (bindings 0))
    (let [s# (bindings 0)
          uri# (bindings 1)]
      `(if (d/create-database ~uri#)
         (let [~s# (d/connect ~uri#)]
           (try
             (load-schema ~s#)
             (with-connection ~(subvec bindings 2) ~@body)
             (finally (d/release ~s#)
                      (d/delete-database ~uri#))))
         (throw (ex-info (str "Unable to create database: " ~uri#) {:uri ~uri#}))))
    :else (throw (IllegalArgumentException. "with-db only allows Symbols to be bound"))))

(def hello-program "(ns cst.test-hello)\n(println \"Hello world\")")

(deftest rt-hello
  (with-connection [c "datomic:mem://source"]
    (let [chello (cst-read-all-string hello-program "foo")
          tx (tx-data chello)
          _ (d/transact c tx)
          location (:cst/location (last tx))
          db (d/db c)
          reloaded (get-cst db "foo")]
      (is (= location (path/to-uri "foo")))
      (is (= hello-program (.emit reloaded))))

    (let [chello (cst-read-all-string hello-program)
          tx (tx-data chello)
          _ (d/transact c tx)
          location (:cst/location (last tx))
          db (d/db c)
          reloaded (get-cst db location)]
      (is (= hello-program (.emit reloaded))))))

