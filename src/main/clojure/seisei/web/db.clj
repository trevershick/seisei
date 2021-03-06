(ns seisei.web.db
  (:require [taoensso.faraday :as far]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]))

(def table-users :users)
(def table-sessions :sessions)
(def table-templates :templates)
(def table-slugs :slugs)

(def aws-dynamodb-client-opts {})

(def not-nil? (comp not nil?))

(defn random-slug
  []
  (.toUpperCase (Integer/toString (* Integer/MAX_VALUE (rand)) 36)))

(defn random-id [] (str (java.util.UUID/randomUUID)))

(defn- now
  []
  (java.util.Date.))

(defn dynamo-update-removals
  [arg-map]
  (->> arg-map
       (filter (fn [[k v]] (nil? v)))
       (map (fn [[k v]] (hash-map k [:delete])))
       (into {})))

(defn dynamo-update-puts
  [arg-map]
  (->> arg-map
       (filter (fn [[k v]] (not-nil? v)))
       (map (fn [[k v]] (hash-map k (conj [:put] v))))
       (into {})))

(defn dynamo-updates
  [arg-map]
  (let [x       {}
        x       (into x (dynamo-update-removals arg-map))
        x       (into x (dynamo-update-puts arg-map))]
    x))

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
;;  public
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
                      :content template}]
    (log/infof "New Template %s" (str new-template))
    (far/put-item aws-dynamodb-client-opts
                  table-templates
                  new-template)
    new-template))

(defn delete-user-template
  [user-id slug]
  (log/debugf "Delete template for user/slug %s/%s" user-id slug)
  (far/delete-item aws-dynamodb-client-opts
                   table-templates
                   {:user user-id :slug slug}))

(defn update-user-template-attrs
  [user-id slug updated-attrs]
  (log/debugf "Update template attrs for user/slug %s/%s with %s" user-id slug updated-attrs)
  (let [updates (dynamo-updates updated-attrs)
        updates (assoc updates :updated [:put (.getTime (now))])
        updated (far/update-item aws-dynamodb-client-opts
                                 table-templates
                                 {:user user-id :slug slug}
                                 updates
                                 {:return :all-new})]
    updated))

(defn find-dynamic-template
  [slug]
  (far/get-item aws-dynamodb-client-opts
                table-slugs
                {:slug slug}))

(defn update-dynamic
  [slug updated-attrs]
  (log/debugf "Update dynamic template attrs for slug %s with %s" slug updated-attrs)
  (let [updates (dynamo-updates updated-attrs)
        updates (assoc updates :updated [:put (.getTime (now))])
        updated (far/update-item aws-dynamodb-client-opts
                                 table-slugs
                                 {:slug slug}
                                 updates
                                 {:return :all-new})]
    updated))

(defn insert-dynamic
  [slug updated-attrs]
  (log/debugf "Insert dynamic template for slug %s with %s" slug updated-attrs)
  (let [updates updated-attrs
        updates (assoc updates :slug slug)
        updates (assoc updates :updated (.getTime (now)))
        updated (far/put-item aws-dynamodb-client-opts
                              table-slugs
                              updates)]
    updated))

(defn delete-dynamic
  [slug]
  (log/debugf "Delete template for user/slug %s" slug)
  (far/delete-item aws-dynamodb-client-opts
                   table-slugs
                   {:slug slug}))

(defn update-user-template
  [user-id slug content title]
  (log/debugf "Save template for user/slug %s/%s" user-id slug)
  (update-user-template-attrs user-id
                              slug
                              {:content content
                               :title title
                               :updated (.getTime (now))}))

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
        opts            {:return ["title" "slug" "updated" "static-url" "dynamic-url" "public"]}
        table           :templates
        results         (far/query aws-dynamodb-client-opts table pk-cond opts)]
    ;; if updated is missing add it as '0', then sort descending
    (sort-by
     #(* -1 (:updated %))
     (map
      #(merge {:updated 0} %)
      results))))

(defn user-public-templates
  "returns the public templates for the specified user"
  [user-id]
  (filter :public (user-templates user-id)))

(def capacities {:sessions      {:read 5 :write 5}
                 :users         {:read 5 :write 5}
                 :templates     {:read 5 :write 5}
                 :slugs         {:read 5 :write 5}})

(defn startup-database
  []
  (log/info "Ensuring database is setup...")
  (far/ensure-table
   aws-dynamodb-client-opts
   table-sessions ;; table name
   [:id :s] ;; key structure
   {:throughput (:sessions capacities)
    :block? true})

  (far/ensure-table
   aws-dynamodb-client-opts
   table-users ;; table name
   [:id :s] ;; key structure (username)
   {:throughput (:users capacities)
    :block? true})

  (far/ensure-table
   aws-dynamodb-client-opts
   table-templates ;; table name
   [:user :s] ;; key structure (username)
   {:range-keydef [:slug :s]
    :throughput (:templates capacities)
    :block? true})

  (far/ensure-table  ;; this is the public slugs table that will piont to a user/slug combo (maybe)
   aws-dynamodb-client-opts
   table-slugs ;; table name
   [:slug :s] ;; key structure (username)
   {:throughput (:templates capacities)
    :block? true})

  (log/info "Done."))
