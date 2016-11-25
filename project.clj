(def aws-s3-access-key  (-> (System/getenv) (get "AWS_S3_ACCESS_KEY")))
(def aws-s3-secret-key  (-> (System/getenv) (get "AWS_S3_SECRET_KEY")))

(defproject seisei "0.1.0-SNAPSHOT"
  :description "Seisei - JSON Generation Web App"
  :url "http://github.com/trevershick/seisei"
  :debug true
  :license {            :name "Eclipse Public License"
                        :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies         [[org.clojure/core.async "0.2.395"]
                        [sablono "0.4.0"]
                        [org.omcljs/om "0.9.0"]
                        [cljs-ajax "0.5.1"]
                        [secretary "1.2.3"]
                        [org.clojure/clojure "1.7.0"]
                        [org.clojure/clojurescript "1.7.170"]
                        [org.clojure/data.json "0.2.5"]
                        [com.amazonaws/aws-java-sdk-core "1.10.37"]
                        [com.amazonaws/aws-java-sdk-s3 "1.10.37"]
                        [com.amazonaws/aws-java-sdk-dynamodb "1.10.37"]
                        [clj-time "0.12.2"]
                        [compojure "1.4.0"]
                        [ring/ring-defaults "0.1.5"]
                        [ring/ring-core "1.4.0"]
                        [ring/ring-json "0.4.0"]
                        [clj-http "1.1.2"]
                        [com.taoensso/faraday "1.5.0" :exclusions [org.clojure/clojure com.amazonaws/aws-java-sdk]]
                        [org.clojure/tools.logging "0.2.4"]
                        [org.slf4j/slf4j-log4j12 "1.7.1"]
                        [environ "1.0.1"]
                        [com.github.javafaker/javafaker "0.11"]
                        [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                          javax.jms/jms
                                                          com.sun.jmdk/jmxtools
                                                          com.sun.jmx/jmxri]]]
  :bower-dependencies   [[mousetrap "1.5.3"]]
  :plugins              [[lein-midje "3.2"]
                        [lein-auto "0.1.2"]
                        [lein-codox "0.9.0"]
                        [lein-cloverage "1.0.6"]
                        [lein-resource "15.10.1"]
                        [lein-ring "0.9.7"]
                        [lein-bower "0.5.1"]
                        [lein-beanstalk "0.2.7"]
                        [org.clojure/clojurescript "1.7.170"]
                        [lein-cljsbuild "1.1.1"]]

  :ring {         :handler       seisei.web.handler/app
                  :port          8888
                  :auto-refresh? true
                  :init          seisei.web.handler/startupcheck }

  :aws {          :access-key ~aws-s3-access-key
                  :secret-key ~aws-s3-secret-key
                  :beanstalk  {:environments ["seisei-prod" "seisei-test"] :s3-bucket "trevershick-seisei" } }

  :resource {
                  :resource-paths  ["bower_components/mousetrap/plugins/global-bind/"]
                  :target-path     "resources/public/vendor/"
                  :verbose         false
                  :skip-stencil    [ #".*" ] }
  :source-paths   ["src/main/clojure" "src/tools/clojure" "src/generated/clojure"]
  :test-paths     ["src/test/clojure"]
  :resource-paths ["src/main/resources" "resources"]
  :target-path    "target/%s"
  :bower          {:directory "bower_components"}
  :clean-targets ^{:protect false} [:target-path [:bower :directory] "resources/private/js" "resources/public/js"]
  :main ^:skip-aot seisei.core
  :profiles       {
                    :dev      { :plugins [[lein-figwheel "0.5.0-1"]]
                                :dependencies [[javax.servlet/servlet-api "2.5"]
                                              [ring-mock "0.1.5"]
                                              [midje "1.8.2" :exclusions [org.clojure/clojure]]]
                                :hooks [leiningen.cljsbuild]
                                :source-paths ["src/main/cljs"]
                                :figwheel {     :server-port  8888
                                                :ring-handler seisei.web.handler/app
                                                :nrepl-port   7888
                                                :css-dirs     ["resources/public/css"] }
                                :cljsbuild {
                                  :test-commands { "unit" ["phantomjs" "src/phantom/test-driver.js" "resources/private/test-runner.html"] }
                                  :builds { :dev {
                                              :source-paths ["src/main/cljs"]
                                              :figwheel true
                                              :compiler { :main         seisei.ui.main
                                                          :asset-path   "js"
                                                          :output-to    "resources/public/js/main.js"
                                                          :output-dir   "resources/public/js"
                                                          :verbose      true }}
                                            :test {
                                              :source-paths ["src/main/cljs" "src/test/cljs" ]
                                              :compiler {
                                                        :pretty-print   true
                                                        :output-dir     "resources/private/js"
                                                        :output-to      "resources/private/js/test_deps.js" ; MUST be named *deps.js
                                                        :verbose        true }}
                                  }
                                }
                              }
                    :uberjar  { :aot :all
                                :omit-source true
                                :hooks [leiningen.cljsbuild]
                                :cljsbuild ^:replace {
                                  :builds {
                                    :test nil
                                    :dev {
                                      :source-paths ["src/main/cljs"]
                                      :compiler {
                                                  :output-to      "resources/public/js/main.js"
                                                  :optimizations  :advanced
                                                  :externs        ["src/externs/externs.js"]
                                                  :verbose        true }}}
                                }
                    }

  }
  :aliases {"full-build" ["do" ["clean"] ["bower" "install"] ["resource"] ["test"] ["codox"] ["cloverage"]]
            "ci-deploy" ["do" ["clean"] ["bower" "install"] ["resource"] ["beanstalk" "deploy" "seisei-prod"]]
            "ci-deploy-test" ["do" ["clean"] ["bower" "install"] ["resource"] ["beanstalk" "deploy" "seisei-test"]]
            "data" ["run" "-m" "seisei.tools.all"]
            "cities" ["run" "-m" "seisei.tools.cities"]
            "states" ["run" "-m" "seisei.tools.states"] }
)
