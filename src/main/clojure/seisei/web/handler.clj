(ns seisei.web.handler
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [compojure.core :refer [GET ANY defroutes routes]]
            [ring.util.response :refer [resource-response response]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :as json-middle]
            [seisei.web.github-oauth]
            [seisei.web.session]
            [seisei.web.user]
            [seisei.web.db :as db]
            [seisei.web.logout]
            ))

(defn startupcheck []
  (seisei.web.github-oauth/startupcheck)
  (seisei.web.db/startupcheck))


(defn my-templates
  [{session :session}]
  (let [logged-in (seisei.web.user/logged-in? session)]
    (-> (if logged-in 
          {:body (or (db/user-templates "trevershick") []) }
          {:status 403})
           )))

(defn my-account 
  [{session :session}]
  (let [logged-in (seisei.web.user/logged-in? session)]
    (-> (if logged-in {:logged-in logged-in
                       :name "Trever Shick"
                       :company "Rally Software"
                       :email "trevershick@yahoo.com"}
                      {:logged-in false})
        response)
    ))


(defroutes app-routes
  (ANY "*" [] seisei.web.logout/logout-routes)
  (ANY "*" [] seisei.web.github-oauth/github-oauth-routes)
  (GET "/my/templates" r (my-templates r))
  (GET "/my/account" r (my-account r))
  (GET "/" [] (resource-response "index.html" {:root "public"}))
  (route/resources "/")
  (route/not-found "Page not found"))

(def session-store (seisei.web.session.DynamoDbStore.))

(def app
  (-> (handler/site app-routes {:session {:store session-store
                                          :cookie-attrs {:max-age 3600} }})
      (json-middle/wrap-json-body {:keywords? true})
      (json-middle/wrap-json-response)))
