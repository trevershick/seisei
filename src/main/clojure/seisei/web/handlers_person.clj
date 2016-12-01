(ns seisei.web.handlers-person
  (:require [compojure.core :refer [GET defroutes]]
            [seisei.web.user :as user]
            [seisei.web.db :as db]
            [clojure.tools.logging :as log]))

(defn- handle-user
  [user-id request]
  (log/debugf "Looking up user %s" user-id)
  (let [the-user (user/lookup-user user-id)]
    (log/debugf "Found user %s" (:name the-user))
    (if the-user
      {:body (select-keys the-user [:name :email :gravatar])})))

(defn- handle-user-templates
  [user-id request]
  (db/user-public-templates user-id))

(defroutes unprotected-routes
           (GET "/users/:u" [u :as request] (handle-user u request))
           (GET "/users/:user-id/templates" [user-id :as request] (handle-user-templates user-id request)))

(defroutes person-routes
           unprotected-routes)
