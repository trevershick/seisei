(ns seisei.web.handler
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [compojure.core :refer [POST GET ANY defroutes routes]]
            [ring.util.response :refer [resource-response response header]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :as json-middle]
            [clojure.tools.logging :as log]
            [seisei.web.github-oauth]
            [seisei.web.oauth-facebook]
            [seisei.web.healthcheck :refer [healthcheck-routes]]
            [seisei.web.session]
            [seisei.web.user]
            [seisei.web.db :as db]
            [seisei.web.s3]
            [seisei.web.template-handlers]
            [seisei.web.logout]
            [seisei.engine]
            [seisei.json]
            ))

(defn startupcheck []
  (seisei.web.github-oauth/startupcheck)
  (seisei.web.s3/startupcheck)
  (seisei.web.db/startupcheck))

(defn hot-flashes [{{flash :flash} :session session :session}]
  (->
    (response { :flash flash })
    (assoc :session (assoc session :flash []))))

(defn my-account [{session :session}]
  (let [logged-in (seisei.web.user/logged-in? session)]
    (-> (if
          logged-in
          (assoc
            (select-keys (session :user) [:name :email :last-method :last-login])
            :logged-in true )
          {:logged-in false})
        response)
    ))


(defroutes app-routes
  (ANY "*" [] seisei.web.logout/logout-routes)
  (ANY "*" [] seisei.web.oauth-facebook/facebook-oauth-routes)
  (ANY "*" [] seisei.web.github-oauth/github-oauth-routes)
  (ANY "*" [] seisei.web.template-handlers/template-routes)
  (GET "/my/hot-flashes"    r (hot-flashes r))
  (GET "/my/account"        r (my-account r))
  (GET "/" [] ( ->
     (resource-response "index.html" {:root "public"})
     (header "Content-Type" "text/html; charset=utf-8")))
  (route/resources "/")
  (route/not-found "Page not found"))

(def session-store (seisei.web.session.DynamoDbStore.))

(def my-defaults (let [opts site-defaults
                       opts (assoc-in opts [:session :flash] true)
                       opts (assoc-in opts [:session :store] session-store)
                       opts (assoc-in opts [:session :cookie-attrs :max-age] 3600)
                       opts (assoc-in opts [:security :anti-forgery] false) ]
                   opts ))

(defn wrap-flash-header [handler]
  (fn [request]
    (-> (handler request)
        (header "X-seisei-flash" (not (nil? (-> request :session :flash)))))))



(def app
  (routes
    healthcheck-routes ; healthcheck-routes doesn't need all the wrappers
    (-> app-routes
        (wrap-flash-header)
        (json-middle/wrap-json-body {:keywords? true})
        (json-middle/wrap-json-response)
        (wrap-defaults my-defaults)
    )))
