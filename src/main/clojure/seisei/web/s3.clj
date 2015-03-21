(ns seisei.web.s3
  (:require [clojure.tools.logging :as log]
            [aws.sdk.s3 :as s3]
            [clojure.data.json :as json]
            [environ.core :refer [env]]))

(def aws-s3-access-key  (env :aws-s3-access-key))
(def aws-s3-secret-key  (env :aws-s3-secret-key))
(def aws-s3-bucket      (env :aws-s3-bucket))
(def aws-s3-endpoint    (env :aws-s3-endpoint))

(def aws-s3-client-opts {:access-key aws-s3-access-key
                         :secret-key aws-s3-secret-key })

(defn unpublish-from-s3
  "Returns a URL"
  [slug]
  (let [filename (str slug ".json")]
    (log/infof "Removing %s from s3 %s" slug aws-s3-bucket)
    (s3/delete-object aws-s3-client-opts 
                      aws-s3-bucket
                      filename)))

(defn publish-to-s3
  "Returns a URL"
  [slug content]
  (let [filename (str slug ".json")]
    (log/infof "Publishing %s to s3 %s" (str content) aws-s3-bucket)
    (s3/put-object aws-s3-client-opts 
                   aws-s3-bucket
                   filename
                   (json/write-str content))  
    (s3/update-object-acl aws-s3-client-opts 
                          aws-s3-bucket
                          filename
                          (s3/grant :all-users :read))  
    (str aws-s3-endpoint filename)))


(defn startupcheck []
  (if (nil? aws-s3-access-key)
    (log/error "AWS_S3_ACCESS_KEY is not set"))
  (if (nil? aws-s3-secret-key)
    (log/error "AWS_S3_SECRET_KEY is not set"))
  (if (nil? aws-s3-endpoint)
    (log/error "AWS_S3_ENDPOINT is not set"))
  (if (nil? aws-s3-bucket)
    (log/error "AWS_S3_BUCKET is not set")))