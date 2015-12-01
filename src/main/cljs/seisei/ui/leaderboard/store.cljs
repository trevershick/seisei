(ns seisei.ui.leaderboard.store
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
    [seisei.ui.dispatcher :as d]
    [secretary.core :as sec]
    [seisei.ui.api :as api]
    [seisei.ui.leaderboard.api :as lapi]
    [seisei.ui.state :refer [app-state]]
    [seisei.ui.util :refer [clj->json nnil? debug]]
    [cljs.core.async :refer [<!]]))



(defmulti handle-action :action)

(defmethod handle-action :load-leaderboard [_]
  (let [ state            (om/root-cursor app-state)]
    (om/update! state :leaderboard {}))
  (lapi/load-leaderboard))

(defmethod handle-action :leaderboard-received [{:keys [data]}]
  (debug "leaderboard/handle-action :leaderboard-received data=" data)
  (let [ state            (om/root-cursor app-state)]
    (om/update! state :leaderboard data)))

(defmethod handle-action :default [msg]
  (debug "leaderboard/store/handle-action :default ignored msg=" msg))

(defonce ^:private channel (d/subscribe))
(go-loop []
  (let [msg (<! channel)]
    ; (debug "Got Dispatcher Message " msg)
    (try
      (handle-action msg)
      (catch :default e (println e)))
    (recur)))
    ; (.assign (aget js/window "location") "/auth/github")))
