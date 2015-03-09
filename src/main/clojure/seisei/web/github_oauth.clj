(ns seisei.web.github-oauth
  (:require [compojure.core :refer [GET defroutes]]
            [ring.util.response :refer [resource-response response]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [clj-http.client :as client]
            [seisei.web.user :as user]))


(def env (System/getenv))
(def github-oauth-client-id (get env "GITHUB_OAUTH_CLIENT_ID"))
(def github-oauth-secret    (get env "GITHUB_OAUTH_SECRET"))

(defn startupcheck []
  (if (nil? github-oauth-client-id)
    (println "ERROR - GITHUB_OAUTH_CLIENT_ID" "is not set"))
  (if (nil? github-oauth-secret)
    (println "ERROR - GITHUB_OAUTH_SECRET" "is not set")))

(def github-login-url (str "https://github.com/login/oauth/authorize"
                           "?"
                           "client_id="
                           github-oauth-client-id
                           "&"
                           "scope="
                           "user:email"))

(defn auth-github
  [{session :session}]
  (println "session is" session)
  (cond
    (user/logged-in? session)
      (ring.util.response/redirect "/") ;; ur already logged in
    :else
      (ring.util.response/redirect github-login-url)
    ))

(defn github-account [access-token]
  (let [u           "https://api.github.com/user"
        _           (println "u" u)
        getargs     {:accept :json 
                     :headers {"Authorization" (str "token " access-token)}
                     :as :json}
        _           (println "getargs" getargs)
        response    (client/get u getargs)
        _           (println "response" response)]
    (:body response)
  ))
(defn github-email [access-token]
  (let [u           "https://api.github.com/user/emails"
        getargs     {:accept :json 
                     :headers {"Authorization" (str "token " access-token)}
                     :as :json}
        response    (client/get u getargs)]
    (:email (first (:body response)))
  ))


(defn github-access-token
  [github-session-code]
  (let [form-params {:client_id github-oauth-client-id
                     :client_secret github-oauth-secret
                     :code github-session-code}]
    (println "form-params is" form-params)
    (client/post
      "https://github.com/login/oauth/access_token"
      {:form-params form-params
       :socket-timeout 1000  ;; in milliseconds
       :conn-timeout 1000    ;; in milliseconds
       :accept :json
       :as :json })
    ))

(defn auth-github-callback
  [{session :session
    {code :code} :params}]
  (let [github-response (github-access-token code)
        body (:body github-response :body)
        access-token (:access_token body)
        github-account (if access-token (github-account access-token) {})
        email (if access-token (github-email access-token) nil)
        logged-in (if access-token true false)
        session (user/logged-in! session logged-in)
        session (assoc session :email email)]
    (println "Github Account" github-account)
    (println "Email Is" email)
    (println "Session is" session)
    (println "Github Code is" code)
    (println "Access Token is" access-token)
    (-> (ring.util.response/redirect "/")
        (assoc :session session))
    ))


(defroutes github-oauth-routes
  (GET "/auth/github" r (auth-github r))
  (GET "/auth/github/callback" r (auth-github-callback r)))

