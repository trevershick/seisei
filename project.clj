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
                 [clj-http "1.0.1"]
                 [com.taoensso/faraday "1.5.0" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.logging "0.2.4"]
                 [org.slf4j/slf4j-log4j12 "1.7.1"]
                 [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jmdk/jmxtools
                                                    com.sun.jmx/jmxri]]
                 [environ "1.0.0"]]
  :bower-dependencies [[bootstrap "3.3.2"]
                       [ace-builds "1.1.8"]
                       [riotjs "2.0.12"]
                       [lodash "3.5.0"]
                       [bootstrap-social "4.8.0"]
                       [mousetrap "1.4.6"]]
  
  :plugins [[lein-midje "3.1.3"]
            [lein-marginalia "0.8.0"]
            [lein-resource "14.10.1"]
            [lein-ring "0.8.13"]
            [lein-bower "0.5.1"]
            [lein-riot "0.0.1"]]
  
  :riot {:tags [["resources/public/" "resources/public/seisei-tags.js"]]}
  :ring {:handler seisei.web.handler/app
         :port 8888
         :auto-refresh? true
         :init seisei.web.handler/startupcheck }
  :bower {:directory "bower_components"}
  :resource {
             :resource-paths [
                              ["bower_components/mousetrap/plugins/global-bind/" { :includes [#".*/.*.min.js"] }]
                              ["bower_components/mousetrap" { :includes [#".*/mousetrap.min.js"] }]
                              ["bower_components/riotjs" { :includes [#".*/riot.min.js"] }]
                              ["bower_components/lodash" { :includes [#".*/lodash.min.js"] }]
                              ["bower_components/bootstrap/dist/js" { :includes [#".*/bootstrap.min.js"] }]
                              ["bower_components/bootstrap/dist/css" { :includes [#".*/bootstrap.min.css"] }]
                              ["bower_components/jquery/dist" { :includes [#".*/jquery.min.js"] }]
                              ["bower_components/bootstrap-social" { :includes [#".*/bootstrap-social.css"] }]
                              ["bower_components/font-awesome/css" { :includes [#".*/font-awesome.min.css"] }]
                              ["bower_components/font-awesome/fonts" { :includes [#".*"] :target-path "resources/public/fonts" }]
                              ["bower_components/ace-builds/src-min" { :includes [#".*/ace.js"
                                                                                  #".*/mode-javascript.js"
                                                                                  #".*/theme-twilight.js"
                                                                                  #".*/worker-javascript.js"
                                                                                  #".*/ext-searchbox.js"] }]
                              ]
             :target-path "resources/public/vendor"
             :verbose false
             :skip-stencil [ #".*" ]
             }
  :source-paths ["src/main/clojure" "src/tools/clojure" "src/generated/clojure"]
  :resource-paths ["src/main/resources" "resources"]
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
