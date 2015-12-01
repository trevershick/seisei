(ns seisei.web.s3
  (:require [clojure.tools.logging :as log]
            [seisei.web.s3v2 :as s3]
            [clojure.data.json :as json]
            [environ.core :refer [env]])
            (:import com.amazonaws.auth.DefaultAWSCredentialsProviderChain))

(def aws-s3-bucket      (env :aws-s3-bucket "trevershick-seisei-json"))
(def aws-s3-endpoint    (str "http://s3.amazonaws.com/" aws-s3-bucket "/"))

(defn aws-s3-client-opts [] {})


(defn unpublish-from-s3
  "Returns a URL"
  [slug]
  (let [filename (str slug ".json")]
    (log/infof "Removing %s from s3 %s" slug aws-s3-bucket)
    (s3/delete-object (aws-s3-client-opts)
                      aws-s3-bucket
                      filename)))

(defn publish-to-s3
  "Returns a URL"
  [slug content]
  (let [filename (str slug ".json")]
    (log/infof "Publishing %s to s3 %s with %s" (str content) aws-s3-bucket (:access-key (aws-s3-client-opts)))
    (s3/put-object (aws-s3-client-opts)
                   aws-s3-bucket
                   filename
                   (json/write-str content))
    (s3/update-object-acl (aws-s3-client-opts)
                          aws-s3-bucket
                          filename
                          (s3/grant :all-users :read))
    (str aws-s3-endpoint filename)))
