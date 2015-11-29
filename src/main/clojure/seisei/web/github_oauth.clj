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
  { :access-token     access-token
    :id               (str "gh:" (:login github-account))
    :ghid             (:login github-account)
    :email            (:email github-account)
    :company          (:company github-account)
    :name             (:name github-account)
    :last-login       (.getTime (java.util.Date.)) })


(defn auth-github-callback
  [{ session :session
    { code :code } :params }]
  (let [ access-token       (get-github-access-token code)
         github-account     (if access-token (get-github-account access-token) {})
         login              (str "gh:" (:login github-account))
         email              (if access-token (get-github-email access-token) nil)
         authenticated      (if access-token true false)
         session            (user/logged-in! session authenticated)
         user-record        (if authenticated (user/lookup-user-by :ghid (:login github-account)))
         user-record        (if
                              (and authenticated (nil? user-record))
                              (user/create-user (str "gh:" (:login github-account)) (assoc (user-from-github-account access-token github-account) :email email))
                              user-record)
        session             (when
                              authenticated
                              (user/user-logged-in! login :github)
                              (assoc session :user user-record)) ]
    (log/debugf "User Record is %s" user-record)
    (log/debugf "Access Token is %s" access-token)
    (log/debugf "Session is %s" session)

    (-> (ring.util.response/redirect "/")
        (assoc :session session))))




(defroutes github-oauth-routes
  (GET "/auth/github" r (auth-github r))
  (GET "/auth/github/callback" r (auth-github-callback r)))
