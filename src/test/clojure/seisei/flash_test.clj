(ns seisei.flash-test
  (:use midje.sweet)
  (:require   [clojure.test     :refer [testing is deftest]]
              [seisei.web.flash :refer [add-flash wrap-flash-header hot-flashes]] ))

(defn wrapped-with-flash [session-flash-messages]
  "Given an initial state for flash messages, executes a basic
  handler that has been wrapped by wrap-flash-header and returns
  the response"
  (let [request       { :session { :flash session-flash-messages }}
        response      { :headers {} }
        root          (fn [_] response)
        response      ((wrap-flash-header root) request)]
    response))


(deftest about-flash
  (facts "about flash"
    (fact "it should create a list if empty"
      (let [session           {}
            session           (add-flash session "test")]
        { :flash ["test"] } => session ))
    (fact "it should adds to an existing list"
      (let [session           { :flash [ 1 ] }
            session           (add-flash session "test")]
        { :flash [1 "test"] } => session ))
  )

  (facts "about hot-flashes"
    (fact "should never return null"
      (let [response (hot-flashes { })]
        (-> response :body :flash) => []))
    (fact "should return all flash messages"
      (let [response (hot-flashes { :session { :flash [7 8 9] } })]
        (-> response :body :flash) => [7 8 9]))
    (fact "should clear out flash messages"
      (let [response (hot-flashes { :session { :flash [7 8 9] } })]
        (-> response :body :flash) => [7 8 9]
        (-> response :session :flash) => []))
  )

  (facts "about wrap-flash-header"
    (fact "it should set X-Seisei-Flash to true when there are flash messages"
      (let [response      (wrapped-with-flash [1])]
          (empty? (response :headers))  => false
          (-> response
              :headers
              (get "X-Seisei-Flash"))   => "true"))

    (fact "it should set X-Seisei-Flash to false when there are no flash messages"
      (let [response      (wrapped-with-flash [])]
          (empty? (response :headers))  => false
          (-> response
              :headers
              (get "X-Seisei-Flash"))   => "false"))

    (fact "it should set X-Seisei-Flash to false when there are no flash messages"
      (let [response      (wrapped-with-flash nil)]
          (empty? (response :headers))  => false
          (-> response
              :headers
              (get "X-Seisei-Flash"))   => "false"))
  )
)
