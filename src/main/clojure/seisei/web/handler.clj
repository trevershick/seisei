(ns seisei.web.handler
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [compojure.core :refer [POST GET ANY defroutes routes]]
            [ring.util.response :refer [resource-response response]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :as json-middle]
            [seisei.web.github-oauth]
            [seisei.web.session]
            [seisei.web.user]
            [seisei.web.db :as db]
            [seisei.web.logout]
            [seisei.engine]
            [seisei.json]
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

(defn run-template 
  [request]
  (let [session (:session request)
        logged-in (seisei.web.user/logged-in? session)
        template (-> request :body :template)
        parsed-json (seisei.json/parse-with-error template)
        processed (seisei.engine/process (:output parsed-json))]
      (println "request" request)
      (println "template" (:template request))
      (println "parsed-json" parsed-json)
      (println "processed" {:processed processed})
      {:body {:processed processed 
              :errors (:errors parsed-json) 
              :input (:input parsed-json)}}
    ))


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
  (POST "/template/process" r (run-template r))
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
