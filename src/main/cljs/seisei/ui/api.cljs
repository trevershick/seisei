(ns seisei.ui.api
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
    [om.dom :as dom :include-macros true]
    [ajax.core :refer [GET POST]]
    [seisei.ui.dispatcher :as d]
    [seisei.ui.state :as state]
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

(defn load-samples []
  (GET "/samples.json" {:keywords? true
                        :response-format :json
                        :handler (fn [r] (println "/samples.json" r) (d/action :samples-received r)) }))

(defn load-my-account []
  ; (println "Loading /my/account")
  (GET "/my/account" {  :keywords? true
                      :response-format :json
                      :handler (fn [acct] (println "/my/account" acct) (d/action :my-account-received acct)) }))
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


(defn load-template [slug]
  (GET
    (str "/my/templates/" slug)
    { :keywords? true
      :response-format :json
      :handler (fn [response] (println "/my/templates/" slug " : " response) (d/action :loaded-template response))
      :error-handler (fn [response] (js/alert response))
    }))

(defn publish-template [template]
  (println "api/publish-template template:" template)
  (if (nil? (template :processed)) (throw "Processed not set"))
  (POST
    (str "/my/templates/" (template :slug) "/publish")
    { :format :json
      :response-format :json
      :keywords? true
      :cache false
      :params {:template template}
      :with-credentials true
      :handler  (fn [response]
                  (println (str "/my/templates/" (template :slug) "/publish") " response:" response)
                  (d/action :published-template response))
      :error-handler (fn [response] (js/alert response)) }))


(defn publish-template-dynamic [template]
  (println "api/publish-template-dynamic template:" template)
  (POST
    (str "/my/templates/" (template :slug) "/publish")
    { :format :json
      :response-format :json
      :keywords? true
      :cache false
      :params {:template template}
      :with-credentials true
      :handler  (fn [response]
                  (println (str "/my/templates/" (template :slug) "/publishdynamic") " response:" response)
                  (d/action :published-template-dynamic response))
      :error-handler (fn [response] (js/alert response)) }))

(defn goto-github-issues []
  (.open js/window "https://github.com/trevershick/seisei/issues" "_blank"))

(defn login []
  (.open js/window "/auth/github" "_self"))

(defn logout []
  (.open js/window "/auth/logout" "_self"))
