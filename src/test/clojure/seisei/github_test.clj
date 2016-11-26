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
    (fact "provides gh-access-token"
      (:gh-access-token (o/user-from-github-account "at1" {})) => "at1")
    (fact "sets user id to gh:<login>"
      (:id (o/user-from-github-account "at1" {:email "x@y.com"})) => "x@y.com")
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

(clojure.test/run-all-tests)

(deftest test-auth-github-callback
  (facts "auth-github-callback"
    (fact "calls auth-github-callback-authed when access-token obtained"
      (o/auth-github-callback { :session { } :params { :code "cx" }}) => {:body ""
                                                                          :headers {"Location" "/"}
                                                                          :session {:zzz 1}
                                                                          :status 302}
      (provided
        (o/get-github-access-token anything) => "at1"
        (o/auth-github-callback-authed anything "at1") => {:zzz 1}
      )
    )
    (fact "calls auth-github-callback-not-authed when access-token not obtained"
      (o/auth-github-callback { :session { } :params { :code "cx" }}) => {:body ""
                                                                          :headers {"Location" "/"}
                                                                          :session {:zzz 2}
                                                                          :status 302}
      (provided
        (o/get-github-access-token anything) => nil
        (o/auth-github-callback-not-authed anything) => {:zzz 2}
      )
    )
))

