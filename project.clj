(def aws-s3-access-key  (-> (System/getenv) (get "AWS_S3_ACCESS_KEY")))
(def aws-s3-secret-key  (-> (System/getenv) (get "AWS_S3_SECRET_KEY")))

(defproject seisei "0.1.0-SNAPSHOT"
  :description "Seisei - JSON Generation Web App"
  :url "http://github.com/trevershick/seisei"
  :debug true
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.json "0.2.5"]
                 [midje "1.6.3" :exclusions [org.clojure/clojure]]
                 [clj-time "0.9.0"]
                 [compojure "1.3.1"]
                 [clj-aws-s3 "0.3.10" :exclusions [joda-time]]
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
  :bower-dependencies [[mousetrap "1.5.3"]]

  :plugins [[lein-midje "3.1.3"]
            [lein-marginalia "0.8.0"]
            [lein-resource "14.10.1"]
            [lein-ring "0.8.13"]
            [lein-bower "0.5.1"]
            [lein-beanstalk "0.2.7"]
            [lein-riot "0.0.1"]]

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
  :resource-paths ["src/main/resources" "resources"]
  :target-path "target/%s"
  :clean-targets [:target-path [:bower :directory]]
  :main ^:skip-aot seisei.core
  :profiles {:uberjar {:aot :all}
             :dev     {:dependencies [[javax.servlet/servlet-api "2.5"]
                                      [ring-mock "0.1.5"]]}}
  :aliases {"ci-deploy" ["do" ["clean"] ["bower" "install"] ["riot"] ["resource"] ["beanstalk" "deploy" "seisei-prod"]]
            "data" ["run" "-m" "seisei.tools.all"]
            "cities" ["run" "-m" "seisei.tools.cities"]
            "states" ["run" "-m" "seisei.tools.states"]
            }
  )
