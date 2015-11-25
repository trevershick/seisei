(def aws-s3-access-key  (-> (System/getenv) (get "AWS_S3_ACCESS_KEY")))
(def aws-s3-secret-key  (-> (System/getenv) (get "AWS_S3_SECRET_KEY")))

(defproject seisei "0.1.0-SNAPSHOT"
  :description "Seisei - JSON Generation Web App"
  :url "http://github.com/trevershick/seisei"
  :debug true
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
            :dependencies [[org.clojure/core.async "0.2.374"]
                           [sablono "0.4.0"]
                           [org.omcljs/om "0.9.0"]
                           [cljs-ajax "0.5.1"]
                           [org.clojure/clojure "1.7.0"]
                           [org.clojure/clojurescript "1.7.170"]
                           [org.clojure/data.json "0.2.5"]
                           [midje "1.8.2" :exclusions [org.clojure/clojure]]
                           [clj-time "0.11.0"]
                           [compojure "1.4.0"]
                           [clj-aws-s3 "0.3.10" :exclusions [joda-time]]
                           [ring/ring-defaults "0.1.2"]
                           [ring/ring-core "1.3.2"]
                           [ring/ring-json "0.3.1"]
                           [clj-http "1.0.1"]
                           [com.taoensso/faraday "1.5.0" :exclusions [org.clojure/clojure]]
                           [org.clojure/tools.logging "0.2.4"]
                           [org.slf4j/slf4j-log4j12 "1.7.1"]
                           [environ "1.0.0"]
                           [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                              javax.jms/jms
                                                              com.sun.jmdk/jmxtools
                                                              com.sun.jmx/jmxri]]]
  :bower-dependencies [[mousetrap "1.5.3"]]
  :plugins [[lein-midje "3.2"]
            [lein-marginalia "0.8.0"]
            [lein-resource "15.10.1"]
            [lein-ring "0.8.13"]
            [lein-bower "0.5.1"]
            [lein-beanstalk "0.2.7"]
            [lein-riot "0.0.1"]
            [lein-figwheel "0.5.0-1"]
            [org.clojure/clojurescript "1.7.170"]
            [lein-cljsbuild "1.1.1"]]
  :cljsbuild {
    :test-commands {"unit" ["phantomjs" "phantom/unit-test.js" "resources/private/main.html"]}
    :builds [ { :id "dev"
                :source-paths ["src/main/cljs"]
                :figwheel true
                :compiler { :main         seisei.ui.core
                            :asset-path   "js"
                            :output-to    "resources/public/js/main.js"
                            :output-dir   "resources/public/js"
                            :verbose      true }}
              { :id "test"
                :source-paths ["src/main/cljs" "src/test/cljs" ]
                :test-paths ["src/test/cljs"]
                :compiler {
                            :pretty-print   true
                            :output-dir     "resources/private/js"
                            :output-to      "resources/private/js/test_deps.js" ; MUST be named *deps.js
                            :verbose        true }}
            ]}
  :figwheel {
    :server-port 8888
    :ring-handler seisei.web.handler/app
    :nrepl-port 7888
  }
  :ring {:handler seisei.web.handler/app
         :port 8888
         :auto-refresh? true
         :init seisei.web.handler/startupcheck }
  :aws {  :access-key ~aws-s3-access-key
          :secret-key ~aws-s3-secret-key
          :beanstalk  {:environments ["seisei-prod"]
                       :s3-bucket "trevershick-seisei" }}
  :bower {:directory "bower_components"}
  :riot {
         :compact true
         :tags [["resources/public/" "resources/public/seisei-tags.js"]]}
  :resource {
             :resource-paths ["bower_components/mousetrap/plugins/global-bind/"]
             :target-path "resources/public/vendor/"
             :verbose false
             :skip-stencil [ #".*" ]
             }
  :source-paths ["src/main/clojure" "src/tools/clojure" "src/generated/clojure"]
  :test-paths ["src/test/clojure"]
  :resource-paths ["src/main/resources" "resources"]
  :target-path "target/%s"
  :clean-targets [:target-path [:bower :directory]]
  :main ^:skip-aot seisei.core
  :profiles {:uberjar {:aot :all}
             :dev     {:dependencies [[javax.servlet/servlet-api "2.5"]
                                      [ring-mock "0.1.5"]
                                      [figwheel-sidecar "0.5.0-1"]]
                       :source-paths ["src/main/cljs"]}}
  :aliases {"ci-deploy" ["do" ["clean"] ["bower" "install"] ["riot"] ["resource"] ["beanstalk" "deploy" "seisei-prod"]]
            "data" ["run" "-m" "seisei.tools.all"]
            "cities" ["run" "-m" "seisei.tools.cities"]
            "states" ["run" "-m" "seisei.tools.states"]
            }
  )
