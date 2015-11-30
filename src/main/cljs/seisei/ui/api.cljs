(ns seisei.ui.api
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
    [om.dom :as dom :include-macros true]
    [ajax.core :refer [GET POST DELETE default-interceptors to-interceptor]]
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

;; This interceptor catches the flash notification
;; this is done via cljs-ajax capabilities
(def flash-interceptor
     (to-interceptor {:name "JSON special case nil"
                      :response (fn [response]
                        (when
                          (= "true" (.getResponseHeader response "X-Seisei-Flash"))
                          (d/action :load-flash))
                        response)
                      }))

(swap! default-interceptors concat [flash-interceptor])


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

(defn goto-github-issues []
  (.open js/window "https://github.com/trevershick/seisei/issues" "_blank"))

(defn login-facebook []
  (.open js/window "/auth/facebook" "_self"))

(defn login []
  (.open js/window "/auth/github" "_self"))

(defn logout []
  (.open js/window "/auth/logout" "_self"))
