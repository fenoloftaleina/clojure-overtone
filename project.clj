(defproject asdf "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :user {:plugins [[cider/cider-nrepl "0.12.0"]
                   [refactor-nrepl    "2.2.0"]]}
  :dependencies [
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.3.442"]
                 [overtone "0.10.1"]
                 [lein-figwheel "0.5.8"]
                 [quil "2.5.0"]])
