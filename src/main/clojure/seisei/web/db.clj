(ns seisei.web.db
  (:require [taoensso.faraday :as far]))



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


(def aws-dynamodb-client-opts {:access-key aws-dynamodb-access-key
                               :secret-key aws-dynamodb-secret-key
                                :endpoint aws-dynamodb-endpoint})


(defn random-slug
  []
  (.toUpperCase (Integer/toString (+ 62193781 (* (- 2176782335 62193781) (rand))) 36)))



(def capacities {:sessions      { :read 5 :write 5 }
                 :users         { :read 5 :write 5 }
                 :templates     { :read 5 :write 5 } })


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
(defn user-templates 
  [user-id]
  (let [order-by        :title
        pk-cond         {:user [:eq user-id]} 
        opts            {:return ["title" "id"] 
                         :order :asc }
        table           :templates
        results         (far/query aws-dynamodb-client-opts table pk-cond opts)]
    (sort-by :title results)
    )
  
)



(defn startup-database
  []
  (far/create-table 
    aws-dynamodb-client-opts
    :sessions ;; table name
    [:id :s] ;; key structure
    {:throughput (:sessions capacities)
     :block? true })
  
  (far/create-table 
    aws-dynamodb-client-opts
    :users ;; table name
    [:id :s] ;; key structure (username)
    {:throughput (:users capacities)
     :block? true })
  
  (far/create-table 
    aws-dynamodb-client-opts
    :templates ;; table name
    [:user :s] ;; key structure (username)
    {:range-keydef [ :id :s ]
     :throughput (:templates capacities)
     :block? true })
  )