(ns seisei.ui.comp.messages
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [seisei.ui.dispatcher :as d]
            [sablono.core :as html :refer-macros [html]]))

(defonce messages-classes {
  :alert    ["alert" "alert-danger" "message"]
  :info     ["alert" "alert-info" "message"]
  :warn     ["alert" "alert-warning" "message"]
  :success  ["alert" "alert-success" "message"] })

(defn- slide-up [dom-element complete]
   (js/Velocity dom-element "slideUp" #js {"duration" 500 "complete" complete}))

(defn- slide-down [dom-element complete]
  (js/Velocity dom-element "slideDown" #js {"duration" 500 "complete" complete}))

(defn- close-alert [owner id]
  "Returns a function, that when called will submit a close message for the given id"
  (fn[]
    (slide-up (om/get-node owner) #(d/action :close-message (:id id)))))

(defn- message [data owner]
  (reify
    om/IRender
    (render [_]
      (html
          [:div { :style { :display "none" } :className (clojure.string/join " " (messages-classes (data :type))) }
            [:button { :type "button" :className "close" :aria-label "Close"
              :onClick (close-alert owner data) }
              [:span { :aria-hidden true } "x"]]
              (data :message)
        ]))
    om/IDidMount
    (did-mount [_]
      (slide-down (om/get-node owner) nil)
      (.setTimeout js/window (close-alert owner data) 3000))
  ))

(defn- messages [data owner]
  (om/component
    (html
      [:div {:className "messages-overlay"} (om/build-all message data {:key :id}) ] )))
