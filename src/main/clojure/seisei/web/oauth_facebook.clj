(ns seisei.web.oauth-facebook
  (:require [compojure.core :refer [GET defroutes]]
            [ring.util.response :refer [redirect]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [clj-http.client :as client]
            [clojure.tools.logging :as log]
            [seisei.web.user :as user]
            [environ.core :refer [env]]))


(def facebook-oauth-app-id      (env :facebook-oauth-app-id))
(def facebook-oauth-secret      (env :facebook-oauth-secret))
(def facebook-oauth-callback    (env :facebook-oauth-callback))

(defn startupcheck []
  (when (nil? facebook-oauth-app-id)
    (log/error "FACEBOOK_OAUTH_APP_ID is not set"))
  (when (nil? facebook-oauth-secret)
    (log/error "FACEBOOK_OAUTH_SECRET is not set"))
  (when (nil? facebook-oauth-callback)
    (log/error "FACEBOOK_OAUTH_CALLBACK is not set")))

(def facebook-login-url
  (str "https://www.facebook.com/v2.0/dialog/oauth"
    "?"
    "app_id="
    facebook-oauth-app-id
    "&"
    "redirect_uri="
    facebook-oauth-callback
    "&"
    "scope="
    "email"))

(defn auth-facebook
  [{session :session}]
  (log/debugf "session is %s" session)
  (cond
    (user/logged-in? session)
    (redirect "/") ;; ur already logged in
    :else
    (redirect facebook-login-url)))

(defn get-facebook-account [access-token]
  "Returns the raw json response from facebook for the 'current' authed user"
  (let [u           "https://graph.facebook.com/me?fields=name,email"
        getargs     {:accept :json
                     :headers {"Authorization" (str "Bearer " access-token)}
                     :as :json}
        response    (client/get u getargs)]
    (log/debugf "oauth-facebook/get-facebook-account response=%s" response)
    (:body response)))

(defn get-facebook-access-token
  [facebook-session-code]
  (let [form-params {:client_id     facebook-oauth-app-id
                     :client_secret facebook-oauth-secret
                     :code          facebook-session-code
                     :redirect_uri  facebook-oauth-callback }]
    (log/debugf "form-params is %s" form-params)
    (->
      (client/post
        "https://graph.facebook.com/v2.4/oauth/access_token"
        { :form-params    form-params
          :socket-timeout 1000        ;; in milliseconds
          :conn-timeout   1000        ;; in milliseconds
          :accept         :json
          :as             :json })
      :body
      :access_token)))


(defn user-from-facebook-account
  [ access-token facebook-account ]
  {:access-token    access-token
   :id              (str "fb:" (:name facebook-account))
   :fbid            (:id      facebook-account)
   :email           (:email   facebook-account)
   :name            (:name    facebook-account)
   :last-login      (.getTime (java.util.Date.)) })


(defn auth-facebook-callback
  [{ session :session { code :code } :params }]
  (let [access-token            (get-facebook-access-token code)
        authenticated           (if access-token true false)
        facebook-account        (if access-token (get-facebook-account access-token) {})
        updated-session         session
        updated-session         (user/logged-in! updated-session authenticated)
        user-record             (if authenticated (user/lookup-user-by :fbid (:id facebook-account)))
        user-record             (if
                                  (and authenticated (nil? user-record))
                                  (user/create-user
                                    (str "fb:" (:name facebook-account))
                                    (user-from-facebook-account access-token facebook-account))
                                  user-record)
        _                       (if authenticated (user/user-logged-in! (:id user-record) :facebook))
        updated-session         (if authenticated (assoc updated-session :user user-record)) ]
    (log/debugf "User Record is %s" user-record)
    (log/debugf "Access Token is %s" access-token)
    (log/debugf "updated-session is %s" updated-session)

    (-> (redirect "/")
        (assoc :session updated-session))))




(defroutes facebook-oauth-routes
  (GET "/auth/facebook" r (auth-facebook r))
  (GET "/auth/facebook/callback" r (auth-facebook-callback r)))
