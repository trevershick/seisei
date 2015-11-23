(ns seisei.ui.templates
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
    [om.dom :as dom :include-macros true]
    [ajax.core :refer [GET POST]]
    [seisei.ui.state :as state]
    [seisei.ui.account :as acct]
    [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)
(println "Initializing State for Templates")
(swap! state/app-state assoc :templates [])

(defn cursor-from-root [root-cursor]
  (root-cursor :templates))

(defn handler-my-templates [response]
  (println "My Templates Response is " (str response))
  (let [data (om/root-cursor state/app-state)]
    (om/update! data :templates response)))

(defn refresh-templates []
  (GET "/my/templates" { :keywords? true
                         :response-format :json
                         :handler handler-my-templates }))

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
      (println "view-my-templates, data = " data)
      (dom/h1 nil "Templates")
      (dom/div nil
        (om/build-all view-template-temporary data {:key :slug})
        (dom/button #js {:onClick refresh-templates } "Refresh Templates")
      )
      )))

; watch for messages to come across the account channel,
; if one comes across, then the user was logged in or out.
; and we need to request 'my templates'
(go-loop []
  (let [msg (<! acct/account-channel)]
    (println "Got Message from account-channel" msg)
    (refresh-templates)
    ))

; Initialize the state
