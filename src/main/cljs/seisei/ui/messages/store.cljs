(ns seisei.ui.messages.store
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
    [seisei.ui.dispatcher :as d]
    [secretary.core :as sec]
    [seisei.ui.state :refer [app-state]]
    [seisei.ui.api :as api]
    [seisei.ui.util :refer [clj->json nnil? debug]]
    [cljs.core.async :refer [<!]]))

(defonce ^:private message-counter (atom 0))

;; store methods -- actually update the state
(defmulti handle-action :action)

(defmethod handle-action :close-message [{:keys [data]}]
  (let [ state            (om/root-cursor app-state)
         messages         (state :messages)
         messages         (remove #(= data (:id %)) messages)]
    (om/update! state :messages messages)))

(defn- show-message [type text]
  (let [state             (om/root-cursor app-state)
        messages          (state :messages)
        messages          (conj messages {:type type :message text :id (swap! message-counter inc)})]
    (om/update! state :messages messages)))

(defmethod handle-action :show-error [{:keys [data]}] (show-message :alert data))
(defmethod handle-action :show-info [{:keys [data]}] (show-message :info data))
(defmethod handle-action :show-warn [{:keys [data]}] (show-message :warn data))
(defmethod handle-action :show-success [{:keys [data]}] (show-message :success data))

(defmethod handle-action :default [msg]
  (debug "messages/store/handle-action :default ignored msg=" msg))

(defonce ^:private channel (d/subscribe))
(go-loop []
  (let [msg (<! channel)]
    ; (debug "Got Dispatcher Message " msg)
    (try
      (handle-action msg)
      (catch :default e (println e)))
    (recur)))
    ; (.assign (aget js/window "location") "/auth/github")))
