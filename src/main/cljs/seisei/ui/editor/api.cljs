;; This is the 'API' for the editor. These methods are called from the editor
;; there is NO updating of state here.  Any updates of state must occur
;; in the store.cljs. So if you want to get data, you make your call through
;; here -> server -> handler -> publish action -> store
(ns seisei.ui.editor.api
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [ajax.core :refer [GET POST DELETE PUT default-interceptors to-interceptor]]
            [seisei.ui.dispatcher :as d]
            [seisei.ui.api :refer [default-error-handler]]
            [seisei.ui.state :as state]
            [seisei.ui.util :refer [debug]]
            [cljs.core.async :refer [put! mult tap chan sub <!]]))

;; API Calls
(defn refresh-templates []
  (debug "api/refresh-templates")
  (GET "/my/templates"
       {:keywords? true
        :response-format :json
        :error-handler default-error-handler
        :handler  (fn [ts]
                    (d/action :templates-received ts))}))

(defn clear-templates []
  (debug "api/clear-templates")
  (d/action :templates-received []))

(defn load-samples []
  (GET "/samples.json"
       {:keywords?        true
        :response-format  :json
        :error-handler default-error-handler
        :handler          (fn [r] (d/action :samples-received r))}))

(defn process-template [template]
  (debug "api/process-template " template)
  (POST "/template/process" {:format :json
                             :response-format :json
                             :keywords? true
                             :cache false
                             :params {:template template}
                             :with-credentials true
                             :handler  (fn [response]
                                         (debug "api:POST /template/process, response=" response)
                                         (d/action :processed-template response))
                             :error-handler default-error-handler}))

(defn load-template [slug]
  (GET
   (str "/my/templates/" slug)
   {:keywords? true
    :response-format :json
    :handler (fn [response]
               (debug (str "/my/templates/" slug ", response=") response)
               (d/action :loaded-template response))
    :error-handler default-error-handler}))

(defn unpublish-dynamic-template [template]
  (debug "api/unpublish-dynamic-template template:" template)
  (DELETE (str "/my/templates/" (template :slug) "/publishdynamic")
          {:keywords?          true
           :response-format    :json
           :error-handler      default-error-handler
           :handler            (fn [r]
                                 (d/action :show-success "Dynamic endpoint unpublished.")
                                 (d/action :unpublished-dynamic-template template))}))

(defn unpublish-static-template [template]
  (debug "api/unpublish-static-template template:" template)
  (DELETE (str "/my/templates/" (template :slug) "/publish")
          {:keywords?          true
           :response-format    :json
           :error-handler      default-error-handler
           :handler            (fn [r]
                                 (d/action :show-success "Static endpoint unpublished.")
                                 (d/action :unpublished-static-template template))}))

(defn delete-template [template]
  (let [slug    (template :slug)
        url     (str "/my/templates/" slug)]
    (DELETE url
            {:response-format :json
             :keywords? true
             :with-credentials true
             :handler (fn [response]
                        (d/action :show-success "Deleted.")
                        (d/action :deleted-template)
                        (refresh-templates))
             :error-handler default-error-handler})))

(defn save-template [template]
  (let [slug    (template :slug)
        isnew   (nil? (template :slug))
        url     (if isnew "/my/templates" (str "/my/templates/" slug))]
    (POST url
          {:response-format :json
           :format :json
           :keywords? true
           :cache false
           :params {:template template}
           :with-credentials true
           :handler  (fn [response]
                       (d/action :show-success "Saved.")
                       (d/action :loaded-template response)
                       (when isnew (-> js/document .-location (set! (str "#/template/" (-> response :template :slug)))))
                       (refresh-templates)
                       (process-template (response :template)))
           :error-handler default-error-handler})))

(defn make-template-public
  [{:keys [slug]}] ;; expect { :slug ??? } as an argument, a 'template' works
  (debug "api/make-template-public:" slug)
  (PUT
   (str "/my/templates/" slug "/public")
   {:response-format   :json
    :cache             false
    :with-credentials  true
    :error-handler     default-error-handler
    :handler           (fn [response]
                         (d/action :show-success "Template is now Public.")
                         (debug (str "api/make-template-public: /my/templates/" slug "/public, response=") response)
                         (d/action :made-template-public (response :template)))}))
(defn make-template-private
  [{:keys [slug]}] ;; expect { :slug ??? } as an argument, a 'template' works
  (debug "api/make-template-private:" slug)
  (DELETE
   (str "/my/templates/" slug "/public")
   {:response-format   :json
    :cache             false
    :with-credentials  true
    :error-handler     default-error-handler
    :handler           (fn
                         [response]
                         (d/action :show-success "Template is now Private.")
                         (debug (str "api/make-template-private: /my/templates/" slug "/public, response=") response)
                         (d/action :made-template-private (response :template)))}))

(defn publish-template [template]
  (debug "api/publish-template template:" template)
  (if (nil? (template :processed)) (throw "Processed not set"))
  (POST
   (str "/my/templates/" (template :slug) "/publish")
   {:format :json
    :response-format :json
    :keywords? true
    :cache false
    :params {:template template}
    :with-credentials true
    :handler  (fn [response]
                (d/action :show-success "Static Endpoint Published Successfully.")
                (debug (str "/my/templates/" (template :slug) "/publish, response=") response)
                (d/action :published-template (response :template)))
    :error-handler default-error-handler}))

(defn publish-template-dynamic [template]
  (debug "api/publish-template-dynamic template:" template)
  (POST
   (str "/my/templates/" (template :slug) "/publishdynamic")
   {:format :json
    :response-format :json
    :keywords? true
    :cache false
    :params {:template template}
    :with-credentials true
    :handler  (fn [response]
                (debug (str "/my/templates/" (template :slug) "/publishdynamic, response=") response)
                (d/action :show-success "Dynamic Endpoint Published Successfully.")
                (d/action :published-template-dynamic (response :template)))
    :error-handler (fn [response] (js/alert response))}))
