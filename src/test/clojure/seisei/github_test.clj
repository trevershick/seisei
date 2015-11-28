(ns seisei.github-test
  (:use midje.sweet)
  (:require   [clojure.test     :refer [testing is deftest]]
              [seisei.web.github-oauth :as o]
              [clojure.tools.logging :as l]
              [clj-http.client :as c]
              [seisei.web.user :as user]
              [seisei.web.flash :refer [add-flash wrap-flash-header hot-flashes]] ))
;
; (defn wrapped-with-flash [session-flash-messages]
;   "Given an initial state for flash messages, executes a basic
;   handler that has been wrapped by wrap-flash-header and returns
;   the response"
;   (let [request       { :session { :flash session-flash-messages }}
;         response      { :headers {} }
;         root          (fn [_] response)
;         response      ((wrap-flash-header root) request)]
;     response))


(deftest github-login-url
  (facts "github-login-url"
    (fact "it should check for client id and secret"
      (binding [o/github-oauth-client-id  "abc"]
        (o/github-login-url) => "https://github.com/login/oauth/authorize?client_id=abc&scope=user:email"
      ))))

(deftest test-auth-github
  (facts "auth-github"
    (fact "it should redirect back to / if already logged in"
      (let [response (o/auth-github { :session { :logged-in true }})]
        (-> response :headers (get "Location")) => "/"
        (-> response :status) => 302)
    )
    (fact "it should redirect to github url if not logged in"
      (let [response (o/auth-github { :session { :logged-in false }})]
        (-> response :headers (get "Location")) => (o/github-login-url)
        (-> response :status) => 302)
    )
    (fact "it should redirect to github url if not logged in"
      (let [response (o/auth-github { })]
        (-> response :headers (get "Location")) => (o/github-login-url)
        (-> response :status) => 302)
      )))
(defchecker has-header [request name value]
  (= value (-> request :headers (get name))))

(deftest test-get-github-account
  (facts "get-github-account"
    (fact "passes through github account values"
      (o/get-github-account "at1") => {:x 1}
      (provided
        (c/get anything anything) => {:body {:x 1}}
      ))
      (fact "passes in the correct auth value"
        (o/get-github-account "at1") => anything
        (provided
          (c/get anything {:accept :json, :headers {"Authorization" "token at1"}, :as :json}) => {:body {:x 1}}
        ))
  ))

(deftest test-get-github-email
  (facts "get-github-email"
    (fact "strips out the first email from a github response"
      (o/get-github-email "at1") => 1
      (provided
        (c/get anything anything) => {:body [{:email 1} {:email 2}]}
      ))
    (fact "passes in the correct auth value"
      (o/get-github-email "at1") => anything
      (provided
        (c/get anything {:accept :json, :headers {"Authorization" "token at1"}, :as :json}) => {:body [{:email 1}]}
      ))
  ))



(deftest test-get-github-access-token
  (facts "get-github-access-token"
    (fact "strips the access token out of the response body"
      (o/get-github-access-token "sess-code") => "access-token"
      (provided
        (c/post anything anything) => {:body { :access_token "access-token"}}))

    (fact "strips the access token out of the response body"
      (o/get-github-access-token "sess-code") => anything
      (provided
        (c/post "https://github.com/login/oauth/access_token" anything) => {}))
    ))


(deftest test-user-from-github-account
  ; [ access-token github-account ]
  (facts "user-from-github-account"
    (fact "provides access-token"
      (:access-token (o/user-from-github-account "at1" {})) => "at1")
    (fact "sets user id to gh:<login>"
      (:id (o/user-from-github-account "at1" {:login "x"})) => "gh:x")
    (fact "provides ghid"
      (:ghid (o/user-from-github-account "at1" {:login "thelogin"})) => "thelogin")
    (fact "provides email"
      (:email (o/user-from-github-account "at1" {:email "a@b"})) => "a@b" )
    (fact "provides company"
      (:company (o/user-from-github-account "at1" {:company "overstock"})) => "overstock" )
    (fact "provides name"
      (:name (o/user-from-github-account "at1" {:name "Trever"})) => "Trever" )
    (fact "provides last-login"
      (:last-login (o/user-from-github-account "at1" {})) => truthy )
  ))



(deftest test-auth-github-callback
  (facts "auth-github-callback"
    (fact "auths from github for existing user"
      (o/auth-github-callback { :session { } :params { :code "cx" }}) => {:body ""
                                                                          :headers {"Location" "/"}
                                                                          :session {:logged-in true, :user {:z 1}}
                                                                          :status 302}
      (provided
        (o/get-github-access-token anything) => "at1"
        (o/get-github-account anything) =>  { :login "l1" :ghid "gl1" }
        (o/get-github-email anything) => "a@b.com"
        (user/user-logged-in! anything anything) => { :y 1 }
        (user/lookup-user-by :ghid "l1") => { :z 1 }
      )
    )))
  ; [{ session :session
  ;   { code :code } :params }]
  ; (let [ access-token       (get-github-access-token code)
  ;        github-account     (if access-token (get-github-account access-token) {})
  ;        login              (str "gh:" (:login github-account))
  ;        email              (if access-token (get-github-email access-token) nil)
  ;        authenticated      (if access-token true false)
  ;        session            (user/logged-in! session authenticated)
  ;        user-record        (if authenticated (user/lookup-user-by :ghid (:login github-account)))
  ;        user-record        (if
  ;                             (and authenticated (nil? user-record))
  ;                             (user/create-user (str "gh:" (:login github-account)) (assoc (user-from-github-account access-token github-account) :email email))
  ;                             user-record)
  ;       session             (when
  ;                             authenticated
  ;                             (user/user-logged-in! login :github)
  ;                             (assoc session :user user-record)) ]
  ;   (log/debugf "User Record is %s" user-record)
  ;   (log/debugf "Access Token is %s" access-token)
  ;   (log/debugf "Session is %s" session)
  ;
  ;   (-> (ring.util.response/redirect "/")
  ;       (assoc :session session))))
