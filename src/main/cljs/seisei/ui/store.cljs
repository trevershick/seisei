(ns seisei.ui.store
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
            [seisei.ui.dispatcher :as d]
            [seisei.ui.util :refer [debug]]
            [seisei.ui.api :as api]
            [cljs.core.async :refer [<!]]))

(defmulti handle-action :action)

(defmethod handle-action :login-github [msg] (api/login))
(defmethod handle-action :login-facebook [msg] (api/login-facebook))
(defmethod handle-action :logout [msg] (api/logout))

(defmethod handle-action :default [msg]
  ;; do nothing
  (debug "store/handle-action :default ignored msg=" msg))

(defmethod handle-action :load-flash [_]
  (api/load-flash))

(defonce channel (d/subscribe))
(go-loop []
  (let [msg (<! channel)]
    (try
      (handle-action msg)
      (catch :default e (println e)))
    (recur)))

(api/load-my-account)
