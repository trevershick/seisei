(ns seisei.ui.templates
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
    [om.dom :as dom :include-macros true]
    [ajax.core :refer [GET POST]]
    [seisei.ui.dispatcher :as d]
    [seisei.ui.state :as state]
    [seisei.ui.account :as acct]
    [cljs.core.async :refer [put! mult tap chan sub <!]]))

(declare refresh-templates)
(declare clear-templates)

(enable-console-print!)
(println "Initializing State for Templates")
(swap! state/app-state assoc :templates [])

(defn cursor-from-root [root-cursor]
  (root-cursor :templates))




;; Components
(defn view-template-temporary [data owner]
  (reify
    om/IDisplayName
    (display-name [_] "view-template-temporary")
    om/IRender
    (render [_]
      (dom/div nil (data :title)))))


(defn view-my-templates [data owner]
  (reify
    om/IDisplayName
    (display-name [_] "view-my-templates")
    om/IRender
    (render [_]
      ; (println "view-my-templates, data = " data)
      (dom/h1 nil "Templates")
      (dom/div nil
        (om/build-all view-template-temporary data {:key :slug})
        (dom/button #js {:onClick refresh-templates } "Refresh Templates")
      ))))

; watch for messages to come across the account channel,
; if one comes across, then the user was logged in or out.
; and we need to request 'my templates'
(defn handle-my-account [message]
  (println "Templates Got Message from dispatcher" message)
  (if (-> message :data :logged-in)
    (refresh-templates)
    (clear-templates)))


;; API Calls
(defn refresh-templates []
  (println "Loading Templates...")
  (GET "/my/templates" { :keywords? true
                         :response-format :json
                         :handler (fn [ts] (d/action :templates-received ts))
                         }))
(defn clear-templates []
  (println "Clearing Templates...")
  (d/action :templates-received []))

;; store methods -- actually update the state
(def templates-received-channel (d/subscribe :templates-received))
(go-loop []
  (let [msg (<! templates-received-channel)
        data (om/root-cursor state/app-state)
        menu-state (data :menu)]
        (om/update! data :templates (msg :data))
        (om/update! menu-state :templates (msg :data))))

(def my-account-channel (d/subscribe :my-account))
(go-loop []
  (let [msg (<! my-account-channel)]
    (println "on chan1" msg)
    (handle-my-account msg)))
