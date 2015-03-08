(ns seisei.web.handler
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [compojure.core :refer [GET ANY defroutes routes]]
            [ring.util.response :refer [resource-response response]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :as json-middle]
            [seisei.web.github-oauth :as ghoauth]
))

(defn startupcheck []
  (ghoauth/startupcheck))


(def my-templates
  [{:id "ZIFJ35" :name (str "Template " (rand-int 10))}
   {:id "KDLI3J" :name (str "Template " (rand-int 10))}
   {:id "ADKIEJ" :name (str "Template " (rand-int 10))}
   ])



(defroutes app-routes
  (ANY "*" [] ghoauth/github-oauth-routes)
  (GET "/" []
       (resource-response "index.html" {:root "public"}))
  (GET "/my/templates" [] (response my-templates))
  (route/resources "/")
  (route/not-found "Page not found"))


(def app
  (-> (handler/site app-routes)
      (json-middle/wrap-json-body {:keywords? true})
      (json-middle/wrap-json-response)))
