(ns seisei.ui.store
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
    [seisei.ui.dispatcher :as d]
    [seisei.ui.state :refer [app-state]]
    [seisei.ui.api :as api]
    [cljs.core.async :refer [<!]]))


;; Startup Code
(defn init []
  (api/load-my-account))

;; store methods -- actually update the state
(defmulti handle-action (fn [x] (x :action)))

(defmethod handle-action :login [msg]
  ; (println "handle-action :login")
  (api/login))

(defmethod handle-action :logout [msg]
  (api/logout))

(defmethod handle-action :menu-help [_]
  (om/update! (om/root-cursor app-state) :show-hotkeys true))
  ; (println app-state))

(defmethod handle-action :toggle-hotkeys [_]
  (om/transact! (om/root-cursor app-state) :show-hotkeys (fn [x] (not x))))

; rebind :hotkey-tidy to :menu-tidy
; some of the hotkeys need additional logic to determine if
; they're enabled or not so I used specific :hotkey-XXX keywords
; to separate out the handlers
(defmethod handle-action :hotkey-tidy [msg]
  (d/action :menu-tidy nil))

(defmethod handle-action :menu-tidy [msg]
  ; (println "handle-action :menu-tidy")
  (let [state        (om/root-cursor app-state)
        editor-state (state :editor)]
        (try
          (let [data         (editor-state :content)
                parsed       (.parse js/JSON data)
                stringified  (.stringify js/JSON parsed nil 2)]
            (om/update! editor-state :content stringified))
        (catch :default e
          ; (println "ERROR " e)
          (om/update! editor-state :invalid true)))))

(defmethod handle-action :editor-updated [{:keys [data]}]
  (let [state (om/root-cursor app-state)
        editor-state (state :editor) ]
      ; (println "Updating app state with template data " data)
      (om/update! editor-state :content data)))

(defmethod handle-action :templates-received [{:keys [data]}]
  ; (println "handle action :my-account, data is " data)
  (let [state (om/root-cursor app-state)
        menu-state (state :menu)]
        (om/update! state :templates data)
        (om/update! menu-state :templates data)))
;
; (def my-account-channel (d/subscribe :my-account))
; (go-loop []
;   (let [msg (<! my-account-channel)]
(defmethod handle-action :my-account [{:keys [data]}]
    (let [state (om/root-cursor app-state)]
      (om/update! state :account data)
      (om/update! (state :menu) :logged-in (data :logged-in))
    (if (data :logged-in)
      (api/refresh-templates)
      (api/clear-templates))))

(defmethod handle-action :default [msg]
  (js/alert (str "No handle-action method for " msg)))

;; store methods -- handle requests, call the API, update the store...
(def channel (d/subscribe))
(go-loop []
  (let [msg (<! channel)]
    ; (println "Got Dispatcher Message " msg)
    (handle-action msg))
    (recur))
    ; (.assign (aget js/window "location") "/auth/github")))
