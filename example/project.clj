(defproject ziad-example "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.8.40"]
                 [reagent "0.6.0-alpha"]
                 [ziad "0.1.0-SNAPSHOT"]]
  :plugins [[lein-cljsbuild "1.1.3"]]
  :cljsbuild
  {:builds [{:id "test"
             :source-paths ["src"]
             :compiler {:output-to "example.js"
                        :main ziad-example.core
                        :optimizations :advanced}}]})
