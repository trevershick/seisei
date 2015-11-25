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

(defn- message [data owner]
  (reify
    om/IRender
    (render [_]
      (html
        [:div
          [:div { :className (clojure.string/join " " (messages-classes (data :type))) }
            [:button { :type "button" :className "close" :aria-label "Close" :onClick (fn [e] (d/action :close-message (:id data)) ) }
              [:span { :aria-hidden true } "x"]]
              (data :message)
        ]]))
    om/IDidMount
    (did-mount [_]
      (.setTimeout js/window #(d/action :close-message (:id data)) 3000))
  ))

(defn- messages [data owner]
  (om/component
    (html
      [:div {:className "messages-overlay"} (om/build-all message data {:key :id}) ] )))
