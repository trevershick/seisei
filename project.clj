(defproject seisei "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/data.json "0.2.5"]
                 [midje "1.6.3" :exclusions [org.clojure/clojure]]
                 [clj-time "0.9.0"]
                 [compojure "1.3.1"]
                 [ring/ring-defaults "0.1.2"]
                 [ring/ring-core "1.3.2"]
                 [ring/ring-json "0.3.1"]
                 ]
  :plugins [[lein-midje "3.1.3"]
            [lein-marginalia "0.8.0"]
            [lein-ring "0.8.13"]]
  :ring {:handler seisei.handler/app :port 8888 :auto-refresh? true}
  :source-paths ["src/main/clojure" "src/tools/clojure" "src/generated/clojure"]
  :target-path "target/%s"
  :main ^:skip-aot seisei.core
  :profiles {:uberjar {:aot :all}
             :dev     {:dependencies [[javax.servlet/servlet-api "2.5"]
                                      [ring-mock "0.1.5"]]}}
  :aliases {"data" ["run" "-m" "seisei.tools.all"]
            "cities" ["run" "-m" "seisei.tools.cities"]
            "states" ["run" "-m" "seisei.tools.states"]
            }
  )
