(ns seisei.web.db)



(def env (System/getenv))
(def aws-dynamodb-access-key (get env "AWS_DYNAMODB_ACCESS_KEY"))
(def aws-dynamodb-secret-key (get env "AWS_DYNAMODB_SECRET_KEY"))
(def aws-dynamodb-endpoint   (get env "AWS_DYNAMODB_ENDPOINT"))

(defn startupcheck []
  (if (nil? aws-dynamodb-access-key)
    (println "ERROR - AWS_DYNAMODB_ACCESS_KEY" "is not set"))
  (if (nil? aws-dynamodb-secret-key)
    (println "ERROR - AWS_DYNAMODB_SECRET_KEY" "is not set"))
  (if (nil? aws-dynamodb-endpoint)
    (println "ERROR - AWS_DYNAMODB_ENDPOINT" "is not set")))
