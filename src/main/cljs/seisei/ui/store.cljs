(ns seisei.ui.store
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
    [om.dom :as dom :include-macros true]
    [ajax.core :refer [GET POST]]
    [seisei.ui.dispatcher :as d]
    [seisei.ui.state :as state]
    [seisei.ui.api :as api]
    [seisei.ui.account :as acct]
    [cljs.core.async :refer [put! mult tap chan sub <!]]))


;; Startup Code
(defn init []
  (api/load-my-account))

; (.assign (aget js/window "location") "/auth/logout"))

(defmulti handle (fn [x] (contains? x))

;; store methods -- handle requests, call the API, update the store...
(def channel (d/subscribe))
(go-loop []
  (let [msg (<! channel)]
    (println "Got Dispatcher Message " msg)))
    ; (.assign (aget js/window "location") "/auth/github")))



;; store methods -- actually update the state
; (def templates-received-channel (d/subscribe :templates-received))
; (go-loop []
;   (let [msg (<! templates-received-channel)
;         data (om/root-cursor state/app-state)
;         menu-state (data :menu)]
;         (om/update! data :templates (msg :data))
;         (om/update! menu-state :templates (msg :data))))
;
; (def my-account-channel (d/subscribe :my-account))
; (go-loop []
;   (let [msg (<! my-account-channel)]
;     (println "on my-account-channel" msg)
;     (let [data (om/root-cursor state/app-state)]
;       (om/update! data :account (msg :data))
;       (println "type of account is " (type (msg :data)))
;       (println "app state is now " state/app-state))
;     (if (-> msg :data :logged-in)
;       (api/refresh-templates)
;       (api/clear-templates))))
