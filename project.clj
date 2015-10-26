(defproject cast "0.1.0-SNAPSHOT"
  :description "Clojure AST reader"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0-beta1"]
                 [com.datomic/datomic-pro "0.9.5327"]]
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  :main cst.reader
  :repositories {"my.datomic.com" {:url "https://my.datomic.com/repo"
                                   :creds :gpg}} )
