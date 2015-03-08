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
                 [clj-http "1.0.1"]]
  :bower-dependencies [[bootstrap "3.3.2"]
                       [ace-builds "1.1.8"]
                       [riotjs "2.0.12"]]

  :plugins [[lein-midje "3.1.3"]
            [lein-marginalia "0.8.0"]
            [lein-resource "14.10.1"]
            [lein-ring "0.8.13"]
            [lein-bower "0.5.1"]]
  :ring {:handler seisei.web.handler/app
         :port 8888
         :auto-refresh? true
         :init seisei.web.handler/startupcheck }
  :bower {:directory "bower_components"}
  :resource {
             :resource-paths [
                              ["bower_components/riotjs" { :includes [#".*/riot.min.js"] }]
                              ["bower_components/bootstrap/dist/js" { :includes [#".*/bootstrap.min.js"] }]
                              ["bower_components/bootstrap/dist/css" { :includes [#".*/bootstrap.min.css"] }]
                              ["bower_components/jquery/dist" { :includes [#".*/jquery.min.js"] }]
                              ["bower_components/ace-builds/src-min" { :includes [#".*/ace.js"
                                                                                  #".*/mode-javascript.js"
                                                                                  #".*/theme-twilight.js"
                                                                                  #".*/worker-javascript.js"] }]
                              ]
             :target-path "resources/public/vendor"
             :verbose false
             }
  :source-paths ["src/main/clojure" "src/tools/clojure" "src/generated/clojure"]
  :target-path "target/%s"
  :clean-targets [:target-path [:bower :directory]]
  :main ^:skip-aot seisei.core
  :profiles {:uberjar {:aot :all}
             :dev     {:dependencies [[javax.servlet/servlet-api "2.5"]
                                      [ring-mock "0.1.5"]]}}
  :aliases {"full" ["do" ["bower" "install"] "resource" "compile"]
            "data" ["run" "-m" "seisei.tools.all"]
            "cities" ["run" "-m" "seisei.tools.cities"]
            "states" ["run" "-m" "seisei.tools.states"]
            }
  )
