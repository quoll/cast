(ns cst.database-test
  (:use [clojure.test]
        [clojure.pprint]
        [cst.database]
        [cst.reader]
        [datomic.api :refer [q] :as d])
  (:require [clojure.string :as str])
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

(declare expand-map)

(defn expand [v]
  (cond
    (instance? EntityMap v) (expand-map (d/touch v))
    (set? v) (map expand v)
    :default v))

(defn expand-map
  [x]
  (into {} (map (fn [[k v]] [k (expand v)]) x)))

(deftest save-program
  (let [fhello "(ns cst.test-hello)\n(println \"Hello world\")"
        chello (cst-read-all-string fhello)
        tx (tx-data chello)
        cnx (database "datomic:mem://source")
        _ (d/transact cnx tx)
        db (d/db cnx)
        pid (q '[:find ?pid . :where [?pid :cst/type :file]] db)
        _ (println "---" pid)
        prog (expand-map (d/touch (d/entity db pid)))]
    (pprint prog)
    ))
