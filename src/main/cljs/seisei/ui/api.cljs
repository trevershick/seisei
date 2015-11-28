(ns seisei.ui.api
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
    [om.dom :as dom :include-macros true]
    [ajax.core :refer [GET POST DELETE default-interceptors to-interceptor get-header]]
    [seisei.ui.dispatcher :as d]
    [seisei.ui.state :as state]
    [seisei.ui.util :refer [debug]]
    [cljs.core.async :refer [put! mult tap chan sub <!]]))

(declare load-my-account)

(defn default-error-handler [ajax-response]
  (if
    (= 403 (ajax-response :status))
    (load-my-account)
    (d/action :show-error (str ajax-response))
  ))


(def flash-interceptor
     (to-interceptor {:name "JSON special case nil"
                      :response (fn [response]
                        (when
                          (= "true" (.getResponseHeader response "X-Seisei-Flash"))
                          (d/action :load-flash))
                        response)
                      }))

(swap! default-interceptors concat [flash-interceptor])

;; API Calls
(defn refresh-templates []
  (debug "api/refresh-templates")
  (GET "/my/templates"
    { :keywords? true
      :response-format :json
      :error-handler default-error-handler
      :handler  (fn [ts]
                  (d/action :templates-received ts))
    }))

(defn clear-templates []
  (debug "api/clear-templates")
  (d/action :templates-received []))

(defn load-samples []
  (GET "/samples.json"
    { :keywords?        true
      :response-format  :json
      :error-handler default-error-handler
      :handler          (fn [r] (d/action :samples-received r))
    }))

(defn show-flash [message]
  (if
    (= (:type message) "error")
    (d/action :show-error (str "Error : " (message :message)))
    (d/action :show-info (str "Info : " (message :message)))))

(defn load-flash []
  (GET "/my/hot-flashes"
    { :keywords?        true
      :response-format  :json
      :error-handler    default-error-handler
      :handler          (fn [r]
                          (doseq [x (:flash r)] (show-flash x))
                          )}))

(defn load-my-account []
  (debug "api/load-my-account")
  (GET "/my/account"
    { :keywords? true
      :response-format :json
      :handler  (fn [acct]
                  (debug "api/load-my-account GET /my/account, response=" acct)
                  (d/action :my-account-received acct))
    }))

(defn process-template [template]
  (debug "api/process-template " template)
  (POST "/template/process" {
    :format :json
    :response-format :json
    :keywords? true
    :cache false
    :params {:template template}
    :with-credentials true
    :handler  (fn [response]
                (debug "api:POST /template/process, response=" response)
                (d/action :processed-template response))
    :error-handler default-error-handler }))


(defn load-template [slug]
  (GET
    (str "/my/templates/" slug)
    { :keywords? true
      :response-format :json
      :handler (fn [response]
        (debug (str "/my/templates/" slug ", response=") response)
        (d/action :show-success "Loaded.")
        (d/action :loaded-template response))
      :error-handler (fn [response] (js/alert response))
    }))

(defn unpublish-dynamic-template [template]
  (debug "api/unpublish-dynamic-template template:" template)
  (DELETE (str "/my/templates/" (template :slug) "/publishdynamic")
    { :keywords?          true
      :response-format    :json
      :error-handler      default-error-handler
      :handler            (fn [r]
                            (d/action :show-success "Dynamic endpoint unpublished.")
                            (d/action :unpublished-dynamic-template template))
    }))

(defn unpublish-static-template [template]
  (debug "api/unpublish-static-template template:" template)
  (DELETE (str "/my/templates/" (template :slug) "/publish")
    { :keywords?          true
      :response-format    :json
      :error-handler      default-error-handler
      :handler            (fn [r]
                            (d/action :show-success "Static endpoint unpublished.")
                            (d/action :unpublished-static-template template))
    }))



(defn delete-template [template]
  (let [slug    (template :slug)
        url     (str "/my/templates/" slug)]
    (DELETE url
      { :response-format :json
        :keywords? true
        :with-credentials true
        :handler (fn [response]
          (d/action :show-success "Deleted.")
          (d/action :deleted-template)
          (refresh-templates))
        :error-handler default-error-handler
        })))

(defn save-template [template]
  (let [slug    (template :slug)
        isnew   (nil? (template :slug))
        url     (if isnew "/my/templates" (str "/my/templates/" slug)) ]
    (POST url
      { :response-format :json
        :format :json
        :keywords? true
        :cache false
        :params {:template template}
        :with-credentials true
        :handler  (fn [response]
                    (d/action :show-success "Saved.")
                    (d/action :loaded-template response)
                    (refresh-templates)
                    (process-template (response :template)))
        :error-handler default-error-handler })))

(defn publish-template [template]
  (debug "api/publish-template template:" template)
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
                  (debug (str "/my/templates/" (template :slug) "/publish, response=") response)
                  (d/action :published-template (response :template)))
      :error-handler default-error-handler }))


(defn publish-template-dynamic [template]
  (debug "api/publish-template-dynamic template:" template)
  (POST
    (str "/my/templates/" (template :slug) "/publishdynamic")
    { :format :json
      :response-format :json
      :keywords? true
      :cache false
      :params {:template template}
      :with-credentials true
      :handler  (fn [response]
                  (debug (str "/my/templates/" (template :slug) "/publishdynamic, response=") response)
                  (d/action :show-success "Dynamic Endpoint Published Successfully.")
                  (d/action :published-template-dynamic (response :template)))
      :error-handler (fn [response] (js/alert response)) }))

(defn goto-github-issues []
  (.open js/window "https://github.com/trevershick/seisei/issues" "_blank"))

(defn login-facebook []
  (.open js/window "/auth/facebook" "_self"))

(defn login []
  (.open js/window "/auth/github" "_self"))

(defn logout []
  (.open js/window "/auth/logout" "_self"))
