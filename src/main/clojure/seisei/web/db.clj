(ns seisei.web.db
  (:require [taoensso.faraday :as far]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]))


(def table-users :users)
(def table-sessions :sessions)
(def table-templates :templates)
(def table-slugs :slugs)

(def aws-dynamodb-access-key (env :aws-dynamodb-access-key))
(def aws-dynamodb-secret-key (env :aws-dynamodb-secret-key))
(def aws-dynamodb-endpoint   (env :aws-dynamodb-endpoint))


(def aws-dynamodb-client-opts {:access-key aws-dynamodb-access-key
                               :secret-key aws-dynamodb-secret-key
                               :endpoint aws-dynamodb-endpoint})


(defn random-slug
  []
  (.toUpperCase (Integer/toString (+ 62193781 (* (- 2176782335 62193781) (rand))) 36)))


(defn random-id [] (str (java.util.UUID/randomUUID)))


(defn now
  []
  (java.util.Date.))

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

(defn insert-template
  [user-id template]
  (let [template-id (random-slug)
        new-template {:user user-id 
                      :slug template-id
                      :title (str "Template " template-id)
                      :content template }]
    (log/infof "New Template %s" (str new-template) )
    (far/put-item aws-dynamodb-client-opts 
                  table-templates
                  new-template)
    new-template))



(defn update-user-template
  [user-id slug content title]
  (log/debugf "Save template for user/slug %s/%s" user-id slug)
  (let [updated (far/update-item aws-dynamodb-client-opts
                table-templates
                {:user user-id :slug slug}
                {:content [:put content]
                 :title [:put title]
                 :updated [:put (.getTime (now))]} 
                {:return :all-new } )]
    (println "Updated template is %s" updated)
    updated))

(defn user-template
  [user-id slug]
  (log/debugf "Load template for user/slug %s/%s" user-id slug)
  (far/get-item aws-dynamodb-client-opts
                table-templates
                {:user user-id :slug slug}))
  
(defn user-templates 
  [user-id]
  (let [order-by        :title
        pk-cond         {:user [:eq user-id]} 
        opts            {:return ["title" "slug" "updated"]}
        table           :templates
        results         (far/query aws-dynamodb-client-opts table pk-cond opts)]
    ;; if updated is missing add it as '0', then sort descending
    (sort-by 
      #(* -1 (:updated %)) 
      (map 
        #(merge {:updated 0} %) 
        results))))





(def capacities {:sessions      { :read 5 :write 5 }
                 :users         { :read 5 :write 5 }
                 :templates     { :read 5 :write 5 }
                 :slugs         { :read 5 :write 5 } })

(defn startup-database
  []
  (log/info "Ensuring database is setup...")
  (far/ensure-table 
    aws-dynamodb-client-opts
    table-sessions ;; table name
    [:id :s] ;; key structure
    {:throughput (:sessions capacities)
     :block? true })
  
  (far/ensure-table 
    aws-dynamodb-client-opts
    table-users ;; table name
    [:id :s] ;; key structure (username)
    {:throughput (:users capacities)
     :block? true })
  
  (far/ensure-table 
    aws-dynamodb-client-opts
    table-templates ;; table name
    [:user :s] ;; key structure (username)
    {:range-keydef [ :slug :s ]
     :throughput (:templates capacities)
     :block? true })

  (far/ensure-table  ;; this is the public slugs table that will piont to a user/slug combo (maybe)
    aws-dynamodb-client-opts
    table-slugs ;; table name
    [:slug :s] ;; key structure (username)
    { :throughput (:templates capacities)
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