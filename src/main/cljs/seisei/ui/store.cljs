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

;; store methods -- actually update the state
; (def templates-received-channel (d/subscribe :templates-received))
; (go-loop []
(defmulti handle-action (fn [x] (println "retunrg " (x :action))(x :action)))


(defmethod handle-action :login [msg]
  (println "handle-action :login")
  (api/login))

(defmethod handle-action :logout [msg]
  (api/logout))

(defmethod handle-action :menu-tidy [msg]
  (println "handle-action :menu-tidy")
  (let [state        (om/root-cursor state/app-state)
        editor-state (state :editor)]
        (try
          (let [data         (editor-state :content)
                parsed       (.parse js/JSON data)
                stringified  (.stringify js/JSON parsed nil 2)]
            (om/update! editor-state :content stringified))
        (catch :default e
          (println "ERROR " e)
          (om/update! editor-state :invalid true)))))

(defmethod handle-action :editor-updated [{:keys [data]}]
  (let [state (om/root-cursor state/app-state)
        editor-state (state :editor) ]
      (println "Updating app state with template data " data)
      (om/update! editor-state :content data)))

(defmethod handle-action :templates-received [{:keys [data]}]
  (println "handle action :my-account, data is " data)
  (let [state (om/root-cursor state/app-state)
        menu-state (state :menu)]
        (om/update! state :templates data)
        (om/update! menu-state :templates data)))
;
; (def my-account-channel (d/subscribe :my-account))
; (go-loop []
;   (let [msg (<! my-account-channel)]
(defmethod handle-action :my-account [{:keys [data]}]
    (println "on my-account-channel" data)
    (let [state (om/root-cursor state/app-state)]
      (om/update! state :account data)
      (om/update! (state :menu) :logged-in (data :logged-in))
      (println "type of account is " (type :data))
      (println "app state is now " state))
    (if (data :logged-in)
      (api/refresh-templates)
      (api/clear-templates)))

(defmethod handle-action :default [msg]
  (js/alert (str "No handle-action method for " msg)))

;; store methods -- handle requests, call the API, update the store...
(def channel (d/subscribe))
(go-loop []
  (let [msg (<! channel)]
    (println "Got Dispatcher Message " msg)
    (handle-action msg))
    (recur))
    ; (.assign (aget js/window "location") "/auth/github")))
