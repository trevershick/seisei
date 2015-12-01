(ns seisei.ui.main
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:import goog.History)
  (:require [om.core :as om :include-macros true]
            [seisei.ui.dispatcher :as d]
            [seisei.ui.state :as state]
            [seisei.ui.leaderboard :refer [leaderboard]]
            [seisei.ui.messages :refer [messages]]
            [seisei.ui.editor :refer [editor-view]]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [secretary.core :as sec :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [cljs.core.async :refer [put! chan <!]]))

(defn navigation-view [_ _]
  (om/component
    (html [:noscript])))
      ; [:div
      ;   [:a { :href "#/" } "Home" ]
      ;   [:a { :href "#/something" } "Something" ]
      ;   [:a { :href "#/about" } "About" ]])))


(defn leaderboard-page-view [data owner]
  (om/component
    (html
      [:div
        (om/build navigation-view {})
        (om/build messages (data :messages))
        (om/build leaderboard (data :leaderboard)) ])))

(defn editor-page-view [data owner]
  (om/component
    (html
      [:div
        (om/build messages (data :messages))
        (om/build editor-view data) ])))


; (om/root app state/app-state {:target app-element})
(def ^:private app-element (. js/document (getElementById "app")))

; this is needed. goog.History pukes some times if i don't construct
; my own hidden input field.
(def ^:private history-element (. js/document (getElementById "hidden-history")))
(sec/set-config! :prefix "#")

(sec/defroute index-page "/" []
  (om/root editor-page-view state/app-state {:target app-element}))

(sec/defroute editor-with-template "/template/:slug" [slug]
  (om/root editor-page-view state/app-state {:target app-element})
  ; fire the route as a message.
  ; that way, whoever wants to can handle it. i certainly can't
  ; handle it.  right now the editor/store handles it because it
  ; knows how. maybe this route should be defined up there?
  (d/action :route-editor-template-slug slug))

(sec/defroute about-page "/popular" []
  (om/root leaderboard-page-view state/app-state {:target app-element}))

(sec/defroute catchall "*" []
  (-> js/document .-location (set! "#/")))

(let [history (History. false nil history-element)
      navigation EventType/NAVIGATE]
  (goog.events/listen history
                     navigation
                     #(-> % .-token sec/dispatch!))
  (doto history (.setEnabled true)))
