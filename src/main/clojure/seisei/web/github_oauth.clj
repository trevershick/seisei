(ns seisei.web.github-oauth
  (:require [compojure.core :refer [GET defroutes]]
            [ring.util.response :refer [resource-response response]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [clj-http.client :as client]
            [clojure.tools.logging :as log]
            [seisei.web.user :as user]
            [environ.core :refer [env]]))


(def ^:dynamic github-oauth-client-id (env :github-oauth-client-id))
(def ^:dynamic github-oauth-secret    (env :github-oauth-secret))

(defn startupcheck []
  (if (nil? github-oauth-client-id)
    (log/error "GITHUB_OAUTH_CLIENT_ID is not set"))
  (if (nil? github-oauth-secret)
    (log/error "GITHUB_OAUTH_SECRET is not set")))

(defn github-login-url []
  (str "https://github.com/login/oauth/authorize"
  "?"
  "client_id="
  github-oauth-client-id
  "&"
  "scope="
  "user:email"))

(defn auth-github
  [{session :session}]
  (log/debugf "session is %s" session)
  (cond
    (user/logged-in? session)
    (ring.util.response/redirect "/") ;; ur already logged in
    :else
    (ring.util.response/redirect (github-login-url))
    ))

(defn get-github-account [access-token]
  "Returns the raw json response from github for the 'current' authed user"
  (let [u           "https://api.github.com/user"
        getargs     {:accept :json
                     :headers {"Authorization" (str "token " access-token)}
                     :as :json}
        response    (client/get u getargs)]
    (:body response)
    ))

(defn get-github-email [access-token]
  (let [u           "https://api.github.com/user/emails"
        getargs     {:accept :json
                     :headers {"Authorization" (str "token " access-token)}
                     :as :json}
        response    (client/get u getargs)]
    (:email (first (:body response)))))


(defn get-github-access-token
  [github-session-code]
  (let [form-params {:client_id github-oauth-client-id
                     :client_secret github-oauth-secret
                     :code github-session-code}]
    (log/debugf "form-params is %s" form-params)
    ( ->
     (client/post
       "https://github.com/login/oauth/access_token"
       {:form-params form-params
        :socket-timeout 1000  ;; in milliseconds
        :conn-timeout 1000    ;; in milliseconds
        :accept :json
        :as :json })
     :body
     :access_token
     )))


(defn user-from-github-account
  [ access-token github-account ]
  { :gh-access-token     access-token
    :ghid             (:login github-account)
    :email            (:email github-account)
    :id               (:email github-account)
    :company          (:company github-account)
    :name             (:name github-account)
    :last-login       (.getTime (java.util.Date.)) })


(defn auth-github-callback-authed
  [session access-token]
  (let [ github-account     (get-github-account access-token)  ; get github account details

         user-record        (user/lookup-user-by               ; lookup the user by the ghid (github login)
                              :ghid                            ; if not found then we'll lookup by email
                              (:login github-account))
         email              (or
                              (:email user-record)
                              (get-github-email access-token))    ; get the user's email
         user-record        (or user-record (user/lookup-user email))           ; lookup the user by email
         user-record        (if user-record
                              (user/update-user email (assoc user-record :gh-access-token access-token))
                              (user/create-user email (assoc (user-from-github-account access-token github-account) :email email)))
         session            (user/logged-in! session true)
         session            (assoc session :user user-record)]

    session)) ; finally return the udpated session


(defn auth-github-callback-not-authed
  [session]
  (user/logged-in! session false))


(defn auth-github-callback
  "Entry point to handle the callback from github. If there's
  an access token to be had then auth-github-callback-authed is called
  otherwise auth-github-callback-not-authed is called"
  [{ session :session {code :code} :params }]
  (let [ access-token   (get-github-access-token code)
         session        (if access-token
                          (auth-github-callback-authed     session access-token)
                          (auth-github-callback-not-authed session))]
    (-> (ring.util.response/redirect "/")
        (assoc :session session))))


(defroutes github-oauth-routes
  (GET "/auth/github" r (auth-github r))
  (GET "/auth/github/callback" r (auth-github-callback r)))
