(ns seisei.ui.store
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
    [seisei.ui.dispatcher :as d]
    [seisei.ui.state :refer [app-state message-counter]]
    [seisei.ui.api :as api]
    [seisei.ui.util :refer [clj->json nnil?]]
    [cljs.core.async :refer [<!]]))


;; Startup Code
(defn init []
  (api/load-my-account)
  (api/load-samples))

;; store methods -- actually update the state
(defmulti handle-action (fn [x] (x :action)))

(defmethod handle-action :login [msg]
  ; (println "handle-action :login")
  (api/login))

(defmethod handle-action :logout [msg]
  (api/logout))

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

(defmethod handle-action :show-error [{:keys [data]}]
  (show-message :alert data))

(defmethod handle-action :show-info [{:keys [data]}]
  (show-message :info data))

(defmethod handle-action :show-warn [{:keys [data]}]
  (show-message :warn data))

(defmethod handle-action :show-success [{:keys [data]}]
  (show-message :success data))

(defmethod handle-action :menu-feedback [_] (api/goto-github-issues))
(defmethod handle-action :menu-help [_]
  (om/update! (om/root-cursor app-state) :show-hotkeys true))
  ; (println app-state))

(defmethod handle-action :toggle-hotkeys [_]
  (om/transact! (om/root-cursor app-state) :show-hotkeys (fn [x] (not x))))

; rebind :hotkey-tidy to :menu-tidy
; some of the hotkeys need additional logic to determine if
; they're enabled or not so I used specific :hotkey-XXX keywords
; to separate out the handlers
(defmethod handle-action :hotkey-tidy [_]
  (d/action :menu-tidy nil))

(defmethod handle-action :hotkey-new [_] (d/action :menu-new nil))
(defmethod handle-action :hotkey-delete [_] (d/action :menu-delete nil))
(defmethod handle-action :hotkey-run [_] (d/action :menu-run nil))
(defmethod handle-action :hotkey-save [_] (d/action :menu-save nil))

(defmethod handle-action :menu-toggle-static [_]
  (let [state            (om/root-cursor app-state)
        current-template (state :template)]
    (if (current-template :static-url)
      (api/unpublish-static-template current-template)
      (d/action :menu-publish-static))))

(defmethod handle-action :menu-toggle-dynamic [_]
  (let [state            (om/root-cursor app-state)
        current-template (state :template)]
    (if (current-template :dynamic-url)
      (api/unpublish-dynamic-template current-template)
      (d/action :menu-publish-dynamic))))

(defmethod handle-action :unpublished-static-template [{:keys [data]}]
  (println "store/:unpublished-static-template data:" data)
  (let [state             (om/root-cursor app-state)
        cursor            (-> state :template :static-url)]
    (om/update! cursor nil)
    (println "template state is now " (-> state :template))
  ))

(defmethod handle-action :unpublished-dynamic-template [{:keys [data]}]
  (println "store/:unpublished-dynamic-template data:" data)
  (let [state             (om/root-cursor app-state)
        cursor            (-> state :template :dynamic-url)]
    (om/update! cursor nil)
    (println "template state is now " (-> state :template))
  ))


(defmethod handle-action :menu-publish-static [_]
  (let [state            (om/root-cursor app-state)
        current-template (state :template)
        processed-text   (-> state :editor :output)
        template         (assoc current-template :processed processed-text)]
        (api/publish-template template)))

(defmethod handle-action :menu-publish-dynamic [_]
  (let [state            (om/root-cursor app-state)
        template (state :template)]
        (api/publish-template-dynamic template)))

(defmethod handle-action :published-template [{:keys [data]}]
  (println "store/:published-template data:" data))

(defmethod handle-action :menu-template [{{:keys [slug]} :data}]
  (println "store/:menu-template slug:" slug)
  (api/load-template slug))

(defmethod handle-action :menu-tidy [msg]
  ; (println "handle-action :menu-tidy")
  (let [state        (om/root-cursor app-state)
        editor-state (state :editor)]
        (try
          (let [data         (editor-state :content)
                parsed       (.parse js/JSON data)
                stringified  (.stringify js/JSON parsed nil 2)]
            (om/update! editor-state :content stringified))
        (catch :default e
          ; (println "ERROR " e)
          (om/update! editor-state :invalid true)))))

(defmethod handle-action :editor-updated [{:keys [data]}]
  (let [state (om/root-cursor app-state)
        editor-state (state :editor) ]
      ; (println "Updating app state with template data " data)
      (om/update! editor-state :content data)
      (om/update! editor-state :dirty true)))

(defmethod handle-action :samples-received [{:keys [data]}]
  (println "store/:samples-received data:" data)
  (let [state (om/root-cursor app-state)]
        (om/update! state :samples data)))

(defmethod handle-action :templates-received [{:keys [data]}]
  ; (println "handle action :my-account, data is " data)
  (let [state (om/root-cursor app-state)
        menu-state (state :menu)]
        (om/update! state :templates data)
        (om/update! menu-state :templates data)))

(defmethod handle-action :my-account-received [{:keys [data]}]
    (let [state (om/root-cursor app-state)]
      (om/update! state :account data)
      (om/update! (state :menu) :logged-in (data :logged-in))
    (if (data :logged-in)
      (api/refresh-templates)
      (api/clear-templates))))

(defmethod handle-action :menu-run [_]
  (let [state         (om/root-cursor app-state)
        editor-state  (state :editor)
        content       (editor-state :content)
        id            (editor-state :id)
        title         (editor-state :title)
        template      {:content content :id id :title title}]
        (api/process-template template)))



;; coming from server
(defmethod handle-action :processed-template [{:keys [data]}]
  (println "store/:processed-template data:" data)
  (let [state        (om/root-cursor app-state)
        editor-state (state :editor)
        output       (clj->json (data :processed))
        errors       (data :errors)]
        (om/update! editor-state :output output)))

(defmethod handle-action :loaded-template [{{:keys [processed template]} :data}]
  (println "store/:loaded-template template:" template)
  (let [state        (om/root-cursor app-state)
        editor-state (state :editor)
        menu-state   (state :menu)
        output       (clj->json processed)]
        (om/update! state :template template) ; store off the template.
        (om/update! editor-state :content (template :content))
        (om/update! editor-state :output output)
        (om/update! editor-state :dirty false)
        ;; update the menu's state
        (om/transact! state :menu (fn [s]
          (let [s (assoc s :new-enabled true)
                s (assoc s :save-enabled true)
                s (assoc s :delete-enabled true)
                s (assoc s :sharing-enabled true)
                s (assoc s :template-shared-statically (nnil? (template :static-url)))
                s (assoc s :template-shared-dynamically (nnil? (template :dynamic-url)))
                s (assoc s :template-title (template :title))]
            s )))
        ))

  ; :template {
  ;   :dynamic-url "http://seisei.elasticbeanstalk.com/templates/WMX64U",
  ;   :static-url "http://trevershick-seisei-json.s3-website-us-east-1.amazonaws.com/WMX64U.json",
  ;   :title "This is a long name for a template or an een really longer name",
  ;   :updated 1427210586549,
  ;   :user "trevershick",
  ;   :slug "WMX64U",
  ;   :content "{\n    \"x\": \"{{city}}\"\n}"},
  ;   :processed {:x "Thoreau"},
  ;   :errors [],
  ;   :input "{\n    \"x\": \"{{city}}\"\n}"}}

(defmethod handle-action :process-template [{:keys [data]}]
  (api/process-template data))

(defmethod handle-action :default [msg]
  (js/alert (str "No handle-action method for " msg)))

;; store methods -- handle requests, call the API, update the store...
(defonce channel (d/subscribe))
(go-loop []
  (let [msg (<! channel)]
    ; (println "Got Dispatcher Message " msg)
    (handle-action msg))
    (recur))
    ; (.assign (aget js/window "location") "/auth/github")))
