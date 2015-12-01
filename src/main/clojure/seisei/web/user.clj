(ns seisei.web.user
  (:require [seisei.db.core :as db]
            [clojure.tools.logging :as log]
            [taoensso.faraday :as far]))


(defn logged-in? [session] (or (:logged-in session) false))
(defn logged-in!
  [session b]
  (assoc session :logged-in b))

(defn user-id
  [session]
  (-> session :user :id))


;; User
;;  username (string) -> id? (:id)
;;  admin (boolean)
;;  access_key :access-key
;;  email
;;  company
;;  name
;;  last-login
; (defrecord User [ id admin access-key email company name last-login])

(defn public-user-attributes
  [user]
  (select-keys user [:name :email :last-login :id]))

(defn get-users [ids]
  (let [results (far/batch-get-item
    db/aws-dynamodb-client-opts
    { db/table-users { :prim-kvs { :id ids }}})]
    (get results db/table-users)))

(defn lookup-user
  [user-id]
  (let [user (far/get-item db/aws-dynamodb-client-opts
                           db/table-users
                           { :id user-id })]
    (log/debugf "lookup-user, user =%s" user)
    user))

(defn lookup-user-by
  [fieldname value]
  (let [results (far/query
                  db/aws-dynamodb-client-opts
                  db/table-users
                  {fieldname [:eq value]}
                  {:index (str (name fieldname) "-index")})]
    (log/debugf "lookup-user-by, results =%s" results)
    (first results)))

(defn create-user
  [user-id attr-map]
  (let [ clean-attr (into {} (remove (comp nil? val) attr-map))]
    (log/infof "Creating user %s with %s" user-id attr-map)
    (far/put-item db/aws-dynamodb-client-opts
                  db/table-users
                  (merge { :id user-id } clean-attr))
    (assoc clean-attr :id user-id)))

(defn user-logged-in!
  [user-id provider]
  (far/update-item db/aws-dynamodb-client-opts
                   db/table-users
                   { :id user-id }
                   { :last-method [ :put (name provider) ]
                     :last-login [ :put (.getTime (java.util.Date.)) ] }))
