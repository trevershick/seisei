(ns seisei.web.db
  (:require [taoensso.faraday :as far]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]))



(def aws-dynamodb-access-key (env :aws-dynamodb-access-key))
(def aws-dynamodb-secret-key (env :aws-dynamodb-secret-key))
(def aws-dynamodb-endpoint   (env :aws-dynamodb-endpoint))


(def aws-dynamodb-client-opts {:access-key aws-dynamodb-access-key
                               :secret-key aws-dynamodb-secret-key
                               :endpoint aws-dynamodb-endpoint})


(defn random-slug
  []
  (.toUpperCase (Integer/toString (+ 62193781 (* (- 2176782335 62193781) (rand))) 36)))





;; User
;;  username (string) -> id?
;;  admin (boolean)
;;  access_key
;;  email
;;  company
;;  name
;;  last-login

;; Template
;;  userid  - {username}
;;  id      - {uuid}
;;  Title
;;  template content
;;  last-processed
;;  last-saved
;;  processed content
;;  processed link (pointing at s3?)
;;  published-slug
;;
;; Published Template ( this should be cached methinks )
;;  id (slug)
;;  Template

(def capacities {:sessions      { :read 5 :write 5 }
                 :users         { :read 5 :write 5 }
                 :templates     { :read 5 :write 5 } })

(defn user-templates 
  [user-id]
  (let [order-by        :title
        pk-cond         {:user [:eq user-id]} 
        opts            {:return ["title" "id"] 
                         :order :asc }
        table           :templates
        results         (far/query aws-dynamodb-client-opts table pk-cond opts)]
    (sort-by :title results)
    ))



(defn startup-database
  []
  (log/info "Ensuring database is setup...")
  (far/ensure-table 
    aws-dynamodb-client-opts
    :sessions ;; table name
    [:id :s] ;; key structure
    {:throughput (:sessions capacities)
     :block? true })
  
  (far/ensure-table 
    aws-dynamodb-client-opts
    :users ;; table name
    [:id :s] ;; key structure (username)
    {:throughput (:users capacities)
     :block? true })
  
  (far/ensure-table 
    aws-dynamodb-client-opts
    :templates ;; table name
    [:user :s] ;; key structure (username)
    {:range-keydef [ :id :s ]
     :throughput (:templates capacities)
     :block? true })
  (log/info "Done."))


(defn startupcheck []
  (if (nil? aws-dynamodb-access-key)
    (log/error "AWS_DYNAMODB_ACCESS_KEY is not set"))
  (if (nil? aws-dynamodb-secret-key)
    (log/error "AWS_DYNAMODB_SECRET_KEY is not set"))
  (if (nil? aws-dynamodb-endpoint)
    (log/error "AWS_DYNAMODB_ENDPOINT is not set"))
  (try 
    (startup-database)
    (catch Exception e (log/error e))))