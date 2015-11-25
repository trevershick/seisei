(ns seisei.ui.api
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
    [om.dom :as dom :include-macros true]
    [ajax.core :refer [GET POST DELETE]]
    [seisei.ui.dispatcher :as d]
    [seisei.ui.state :as state]
    [cljs.core.async :refer [put! mult tap chan sub <!]]))

(defn default-error-handler [ajax-response]
  (d/action :show-error (str ajax-response)))

;; API Calls
(defn refresh-templates []
  ; (println "Loading Templates...")
  (GET "/my/templates"
    { :keywords? true
      :response-format :json
      :error-handler default-error-handler
      :handler  (fn [ts]
                  (d/action :templates-received ts))
    }))

(defn clear-templates []
  ; (println "Clearing Templates...")
  (d/action :templates-received []))

(defn load-samples []
  (GET "/samples.json"
    { :keywords?        true
      :response-format  :json
      :error-handler default-error-handler
      :handler          (fn [r]
                          (println "/samples.json" r)
                          (d/action :samples-received r))
    }))

(defn load-my-account []
  ; (println "Loading /my/account")
  (GET "/my/account"
    { :keywords? true
      :response-format :json
      :handler  (fn [acct]
                  (println "api:GET /my/account acct:" acct)
                  (d/action :my-account-received acct))
    }))

(defn process-template [template]
  (println "api/process-template " template)
  (POST "/template/process" {
    :format :json
    :response-format :json
    :keywords? true
    :cache false
    :params {:template template}
    :with-credentials true
    :handler  (fn [response]
                (println "api:POST /template/process response:" response)
                (d/action :processed-template response))
    :error-handler default-error-handler }))


(defn load-template [slug]
  (GET
    (str "/my/templates/" slug)
    { :keywords? true
      :response-format :json
      :handler (fn [response]
        (println "/my/templates/" slug " : " response)
        (d/action :show-success "Loaded.")
        (d/action :loaded-template response))
      :error-handler (fn [response] (js/alert response))
    }))

(defn unpublish-dynamic-template [template]
  (println "api/unpublish-dynamic-template template:" template)
  (DELETE (str "/my/templates/" (template :slug) "/publishdynamic")
    { :keywords?          true
      :response-format    :json
      :error-handler      default-error-handler
      :handler            (fn [r]
                            (d/action :show-success "Dynamic endpoint unpublished.")
                            (d/action :unpublished-dynamic-template template))
    }))

(defn unpublish-static-template [template]
  (println "api/unpublish-static-template template:" template)
  (DELETE (str "/my/templates/" (template :slug) "/publish")
    { :keywords?          true
      :response-format    :json
      :error-handler      default-error-handler
      :handler            (fn [r]
                            (d/action :show-success "Static endpoint unpublished.")
                            (d/action :unpublished-static-template template))
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
                  (d/action :show-success "Static Endpoint Published Successfully.")
                  (println (str "/my/templates/" (template :slug) "/publish") " response:" response)
                  (d/action :published-template (response :template)))
      :error-handler default-error-handler }))


(defn publish-template-dynamic [template]
  (println "api/publish-template-dynamic template:" template)
  (POST
    (str "/my/templates/" (template :slug) "/publishdynamic")
    { :format :json
      :response-format :json
      :keywords? true
      :cache false
      :params {:template template}
      :with-credentials true
      :handler  (fn [response]
                  (println (str "/my/templates/" (template :slug) "/publishdynamic") " response:" response)
                  (d/action :show-success "Dynamic Endpoint Published Successfully.")
                  (d/action :published-template-dynamic (response :template)))
      :error-handler (fn [response] (js/alert response)) }))

(defn goto-github-issues []
  (.open js/window "https://github.com/trevershick/seisei/issues" "_blank"))

(defn login []
  (.open js/window "/auth/github" "_self"))

(defn logout []
  (.open js/window "/auth/logout" "_self"))
