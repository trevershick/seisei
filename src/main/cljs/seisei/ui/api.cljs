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
  ; (println "Loading Templates...")
  (GET "/my/templates" { :keywords? true
                         :response-format :json
                         :handler (fn [ts] (d/action :templates-received ts))
                         }))

(defn clear-templates []
  ; (println "Clearing Templates...")
  (d/action :templates-received []))

(defn load-my-account []
  ; (println "Loading /my/account")
  (GET "/my/account" {  :keywords? true
                      :response-format :json
                      :handler (fn [acct] (println "/my/account" acct) (d/action :my-account acct)) }))
(defn process-template [template]
  (println "api/process-template " template)
  (POST "/template/process" { :format :json
                              :response-format :json
                              :keywords? true
                              :cache false
                              :params {:template template}
                              :with-credentials true
                              :handler (fn [response] (println "/template/process repsonse:" response) (d/action :processed-template response))
                              :error-handler (fn [response] (js/alert response)) }))
;
;     })
;   var body = {
;     template : this.getCurrentTemplate()
;   };
;   $.ajax({
;     url: "/template/process",
;     method: "POST",
;     dataType: "json",
;     contentType: "application/json",
;     cache: false,
;     data : JSON.stringify(body),
;     error: this.onProcessError.bind(this),
;     success: this.onReceivedTemplateProcessed.bind(this)
;   });
; )

(defn login []
  (.assign (aget js/window "location") "/auth/github"))

(defn logout []
  (.assign (aget js/window "location") "/auth/logout"))
