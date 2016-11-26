(ns seisei.web.oauth-facebook
  (:require [compojure.core :refer [GET defroutes]]
            [ring.util.response :refer [redirect]]
            [ring.util.response :refer [header]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [clj-http.client :as client]
            [clojure.tools.logging :as log]
            [seisei.web.user :as user]
            [seisei.web.flash :refer [add-flash]]
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


(defn auth-facebook-callback-failure
  [session]
  (user/logged-in! session false))


(defn auth-facebook-callback-success
  [session access-token]
  (let [facebook-account        (get-facebook-account access-token)  ; get fb account details

        user-record             (user/lookup-user-by               ; lookup the user by the ghid (github login)
                                  :fbid                            ; if not found then we'll lookup by email
                                  (:id facebook-account))
        email                   (or
                                  (:email user-record)
                                  (:email facebook-account))

        user-record             (or user-record (user/lookup-user email))           ; lookup the user by email
        user-record             (if user-record
                                  (user/update-user
                                    email
                                    (assoc user-record :fb-access-token access-token :fbid (:id facebook-account)))
                                  (user/create-user
                                    email
                                    (user-from-facebook-account access-token facebook-account)))

        session                 (user/logged-in! session true)
        session                 (assoc session :user user-record)]

    session))

(defn auth-facebook-callback
  "initial entry point of the facebook oauth callback"
  [{ session :session {code :code} :params }]
  (let [ access-token   (get-facebook-access-token code)
         session        (if access-token
                          (auth-facebook-callback-success     session access-token)
                          (auth-facebook-callback-failure     session))]
    (-> (ring.util.response/redirect "/")
        (assoc :session session))))


(defroutes facebook-oauth-routes
  (GET "/auth/facebook" r (auth-facebook r))
  (GET "/auth/facebook/callback" r (auth-facebook-callback r)))
