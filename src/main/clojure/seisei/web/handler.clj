(ns seisei.web.handler
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [compojure.core :refer [GET ANY defroutes routes]]
            [ring.util.response :refer [resource-response response]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :as json-middle]
            [seisei.web.github-oauth]
            [seisei.web.session]
            [seisei.web.db]
))

(defn startupcheck []
  (seisei.web.github-oauth/startupcheck)
  (seisei.web.db/startupcheck))


(def my-templates
  [{:id "ZIFJ35" :name (str "Template " (rand-int 10))}
   {:id "KDLI3J" :name (str "Template " (rand-int 10))}
   {:id "ADKIEJ" :name (str "Template " (rand-int 10))}
   ])



(defroutes app-routes
  (ANY "*" [] seisei.web.github-oauth/github-oauth-routes)
  (GET "/" []
       (resource-response "index.html" {:root "public"}))
  (GET "/my/templates" [] (response my-templates))
  (route/resources "/")
  (route/not-found "Page not found"))

(def session-store (seisei.web.session.DynamoDbStore.))

(def app
  (-> (handler/site app-routes {:session { :store session-store } })
      (json-middle/wrap-json-body {:keywords? true})
      (json-middle/wrap-json-response)))
