(ns seisei.ui.api
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
    [om.dom :as dom :include-macros true]
    [ajax.core :refer [GET POST]]
    [seisei.ui.dispatcher :as d]
    [seisei.ui.state :as state]
    [seisei.ui.account :as acct]
    [cljs.core.async :refer [put! mult tap chan sub <!]]))



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

(defn load-my-account []
  (println "Loading /my/account")
  (GET "/my/account" {  :keywords? true
                      :response-format :json
                      :handler (fn [acct] (d/action :my-account acct)) }))
