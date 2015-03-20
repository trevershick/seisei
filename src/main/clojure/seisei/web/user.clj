(ns seisei.web.user
  (:require [seisei.web.db :as db]
            [clojure.tools.logging :as log]
            [taoensso.faraday :as far]))


(defn logged-in? [session] (:logged-in session))
(defn logged-in!
  [session b]
  (assoc session :logged-in b))

(defn user-id
  [session s]
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

(defn lookup-user 
  [user-id]
  (let [user (far/get-item db/aws-dynamodb-client-opts 
                           db/table-users 
                           { :id user-id })]
    (log/debugf "lookup-user, user =%s" user)
    user))

(defn create-user
  [user-id attr-map]
  (let [ clean-attr (into {} (remove (comp nil? val) attr-map))]
    (log/infof "Creating user %s with %s" user-id attr-map)
    (far/put-item db/aws-dynamodb-client-opts 
                  db/table-users 
                  (merge { :id user-id } clean-attr))
    (assoc clean-attr :id user-id)))

(defn user-logged-in
  [user-id]
  (far/update-item db/aws-dynamodb-client-opts
                   db/table-users
                   { :id user-id }
                   { :last-login [ :put (.getTime (java.util.Date.)) ] }))

