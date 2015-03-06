(defproject seisei "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
    [org.clojure/clojure "1.6.0"]
    [org.clojure/data.json "0.2.5"]
    [midje "1.6.3" :exclusions [org.clojure/clojure]]
  ]
  :plugins [[lein-midje "3.1.3"]]
  :source-paths ["src/main/clojure" "src/tools/clojure"]
  :main ^:skip-aot seisei.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all} }
  :aliases {"cities" ["run" "-m" "seisei.tools.cities"]})
