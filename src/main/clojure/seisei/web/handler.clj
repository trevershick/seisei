(ns seisei.web.handler
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [compojure.core :refer [POST GET ANY defroutes routes]]
            [ring.util.response :refer [resource-response response header]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :as json-middle]
            [clojure.tools.logging :as log]
            [seisei.web.auth-github :as auth-github]
            [seisei.web.auth-facebook :as auth-facebook]
            [seisei.web.healthcheck :refer [healthcheck-routes]]
            [seisei.web.sessionstore]
            [seisei.web.user]
            [seisei.web.auth :as auth]
            [seisei.web.db :as db]
            [seisei.web.s3]
            [seisei.web.handlers-template :as handlers-template]
            [seisei.web.handlers-person :as handlers-person]
            [seisei.web.handlers-logout :as logout]
            [seisei.engine]
            [seisei.gravatar]
            [seisei.web.flash]
            [seisei.json]))

(defn startupcheck []
  (auth-github/startupcheck)
  (auth-facebook/startupcheck))

(defn my-account [request]
  (let [logged-in (auth/logged-in? request)]
    (-> (if logged-in
          (assoc (select-keys (auth/user-from request) [:name :email :last-method :last-login])
                 :logged-in true
                 :gravatar (seisei.gravatar/gravatar (auth/user-from request)))
          {:logged-in false})
        response)))

(defroutes app-routes
           (ANY "*" [] logout/logout-routes)
           (ANY "*" [] auth-facebook/facebook-oauth-routes)
           (ANY "*" [] auth-github/github-oauth-routes)
           (ANY "*" [] handlers-template/template-routes)
           (ANY "*" [] handlers-person/person-routes)
           (GET "/my/hot-flashes"    r (seisei.web.flash/hot-flashes r))
           (GET "/my/account"        r (my-account r))
           (GET "/" [] (->
                        (resource-response "index.html" {:root "public"})
                        (header "Content-Type" "text/html; charset=utf-8")))
           (route/resources "/")
           (route/not-found "Page not found"))

(def session-store (seisei.web.sessionstore.DynamoDbStore.))

(def my-defaults (let [opts site-defaults
                       opts (assoc-in opts [:session :flash] true)
                       opts (assoc-in opts [:session :store] session-store)
                       opts (assoc-in opts [:session :cookie-attrs :max-age] 3600)
                       opts (assoc-in opts [:security :anti-forgery] false)]
                   opts))

(def app
  (routes
   healthcheck-routes ; healthcheck-routes doesn't need all the wrappers
   (-> app-routes
       (seisei.web.flash/wrap-flash-header)
       (json-middle/wrap-json-body {:keywords? true})
       (json-middle/wrap-json-response)
       (auth/with-user)                                 ; add's :identity to the request if there
       (wrap-defaults my-defaults))))
