(ns seisei.ui.store
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
    [seisei.ui.dispatcher :as d]
    [secretary.core :as sec]
    [seisei.ui.state :refer [app-state message-counter]]
    [seisei.ui.api :as api]
    [seisei.ui.util :refer [clj->json nnil? debug]]
    [cljs.core.async :refer [<!]]))


;; Startup Code
(defn init []
  (api/load-my-account)
  (api/load-samples))

;; store methods -- actually update the state
(defmulti handle-action :action)

(defmethod handle-action :login-github [msg] (api/login))
(defmethod handle-action :login-facebook [msg] (api/login-facebook))
(defmethod handle-action :logout [msg] (api/logout))

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

(defmethod handle-action :show-error [{:keys [data]}] (show-message :alert data))
(defmethod handle-action :show-info [{:keys [data]}] (show-message :info data))
(defmethod handle-action :show-warn [{:keys [data]}] (show-message :warn data))
(defmethod handle-action :show-success [{:keys [data]}] (show-message :success data))

(defmethod handle-action :menu-feedback [_] (api/goto-github-issues))
(defmethod handle-action :menu-help [_] (d/action :toggle-hotkeys))

(defmethod handle-action :toggle-hotkeys [_]
  (om/transact! (om/root-cursor app-state) :show-hotkeys not))

; rebind :hotkey-tidy to :menu-tidy
; some of the hotkeys need additional logic to determine if
; they're enabled or not so I used specific :hotkey-XXX keywords
; to separate out the handlers
(defmethod handle-action :hotkey-help [_] (d/action :menu-help nil))
(defmethod handle-action :hotkey-tidy [_] (d/action :menu-tidy nil))
(defmethod handle-action :hotkey-new [_] (d/action :menu-new nil))
(defmethod handle-action :hotkey-save [_]
  (if (-> app-state om/root-cursor :menu :save-enabled)
    (d/action :menu-save nil)))
(defmethod handle-action :hotkey-delete [_]
  (if (-> app-state om/root-cursor :menu :delete-enabled)
    (d/action :menu-delete)))

(defmethod handle-action :hotkey-run [_] (d/action :menu-run nil))

(defmethod handle-action :menu-toggle-static [_]
  (let [state            (om/root-cursor app-state)
        current-template (state :template)
        static-url       (current-template :static-url)
        published        (nnil? static-url)]
    (debug "store/:menu-toggle-static static-url:" static-url)
    (debug "store/:menu-toggle-static current-template:" current-template)
    (debug "store/:menu-toggle-static published:" published)
    (if published
      (api/unpublish-static-template current-template)
      (d/action :menu-publish-static)
      )))

(defmethod handle-action :menu-toggle-dynamic [_]
  (let [state            (om/root-cursor app-state)
        current-template (state :template)
        dynamic-url      (current-template :dynamic-url)
        published        (nnil? dynamic-url)]
    (debug "store/:menu-toggle-dynamic current-template:" current-template)
    (if published
      (api/unpublish-dynamic-template current-template)
      (d/action :menu-publish-dynamic)
      )))

(defmethod handle-action :unpublished-static-template [{:keys [data]}]
  (debug "store/:unpublished-static-template data:" data)
  (let [state           (om/root-cursor app-state)
        menu-cursor     (-> state :menu)
        template-cursor (-> state :template)]
    (api/refresh-templates)
    (om/update! template-cursor :static-url nil)
    (om/update! menu-cursor     :template-shared-statically false)
    (debug "store/:unpublished-static-template template state is now " template-cursor)
  ))

(defmethod handle-action :unpublished-dynamic-template [{:keys [data]}]
  (debug "store/:unpublished-dynamic-template data:" data)
  (let [state           (om/root-cursor app-state)
        menu-cursor     (-> state :menu )
        template-cursor (-> state :template)]
        (api/refresh-templates)
    (om/update! template-cursor :dynamic-url nil)
    (om/update! menu-cursor :template-shared-dynamically false)
    (debug "template state is now " (-> state :template))
  ))

(defn save-existing []
  (let [state            (om/root-cursor app-state)
        editor-cursor    (state :editor)
        template         (state :template)
        template         (assoc template :content (editor-cursor :content))]
        (debug "store/save-existing template:" template)
        (api/save-template template)))

(defn save-new []
  (let [state            (om/root-cursor app-state)
        editor-cursor    (state :editor)
        new-template     { :content (editor-cursor :content) :id nil :title "Untitled"}]
        (debug "store/save-new new-template:" new-template)
        (api/save-template new-template)))

(defmethod handle-action :menu-save [_]
  (let [state            (om/root-cursor app-state)
        is-new           (nil? (state :template))]
    (if is-new
      (save-new)
      (save-existing))))

(defmethod handle-action :menu-rename [_]
  (let [state           (om/root-cursor app-state)
        template-cursor (state :template)
        rename-cursor   (state :rename)
        current-title   (if template-cursor (template-cursor :title) nil)]
    (when
      current-title
      (om/update! rename-cursor :value current-title)
      (om/update! rename-cursor :show true))))

(defmethod handle-action :menu-delete [_]
  (let [state           (om/root-cursor app-state)
        template        (state :template)
        title           (template :title)]
    (debug "store/:menu-delete")
    (om/transact! state :confirm (fn [confirm]
      (let [c confirm
            c (assoc c :show true)
            c (assoc c :question (str "Are you sure you want to delete " title " ?"))
            c (assoc c :title "Delete your template?")
            c (assoc c :yes   "Delete")
            c (assoc c :no    "Don't Delete")
            c (assoc c :confirm-action :delete-template)]
        c))
    (debug "store/:menu-delete state is now:" (state :confirm)))))

;; this version is used to delete the template directly without confirmation
(defmethod handle-action :delete-template [_]
  (let [state           (om/root-cursor app-state)]
    (api/delete-template (state :template))))

(defmethod handle-action :menu-publish-static [_]
  (let [state            (om/root-cursor app-state)
        current-template (state :template)
        processed        (-> state :editor :processed)
        template         (assoc current-template :processed processed)]
        (debug "store/:menu-publish-static editor:" (-> state :editor))
        (debug "store/:menu-publish-static template:" template)
        (debug "store/:menu-publish-static processed:" processed)
        (api/publish-template template)))

(defmethod handle-action :menu-publish-dynamic [_]
  (let [state            (om/root-cursor app-state)
        template (state :template)]
        (api/publish-template-dynamic template)))

(defmethod handle-action :published-template [{:keys [data]}]
  (debug "store/:published-template data:" data)
  (let [state                       (om/root-cursor app-state)
        menu-cursor                 (-> state :menu)
        template-cursor             (-> state :template)]
    (api/refresh-templates)
    (om/update! template-cursor :static-url (data :static-url))
    (om/update! menu-cursor :template-shared-statically true)
    (debug "template state is now " (-> state :template))
  ))

(defmethod handle-action :published-template-dynamic [{:keys [data]}]
  (debug "store/:published-template-dynamic data:" data)
  (let [state                 (om/root-cursor app-state)
        menu-cursor           (-> state :menu)
        template-cursor       (-> state :template)]
    (api/refresh-templates)
    (om/update! template-cursor :dynamic-url (data :dynamic-url))
    (om/update! menu-cursor :template-shared-dynamically true)
    (debug "store/:published-template-dynamic template is now:" template-cursor)
  ))

(defmethod handle-action :route-with-template-slug [{slug :data}]
  (debug "store/:route-with-template-slug slug:" slug)
  (api/load-template slug))

(defmethod handle-action :menu-template [{{:keys [slug]} :data}]
  (debug "store/:menu-template slug:" slug)
  ;; i need to figure out where i want this.
  (-> js/document .-location (set! (str "#/template/" slug))))

(defmethod handle-action :menu-tidy [msg]
  ; (debug "handle-action :menu-tidy")
  (let [state        (om/root-cursor app-state)
        editor-state (state :editor)]
        (try
          (let [data         (editor-state :content)
                parsed       (.parse js/JSON data)
                stringified  (.stringify js/JSON parsed nil 2)]
            (om/update! editor-state :content stringified))
        (catch :default e
          ; (debug "ERROR " e)
          (om/update! editor-state :invalid true)))))

(defmethod handle-action :editor-updated [{:keys [data]}]
  (let [state (om/root-cursor app-state)
        editor-state (state :editor) ]
      ; (debug "Updating app state with template data " data)
      (om/update! editor-state :content data)
      (om/update! editor-state :dirty true)))

(defmethod handle-action :samples-received [{:keys [data]}]
  (let [state (om/root-cursor app-state)]
        (om/update! state :samples data)))

(defmethod handle-action :templates-received [{:keys [data]}]
  ; (debug "handle action :my-account, data is " data)
  (let [state (om/root-cursor app-state)
        menu-state (state :menu)]
        (om/update! state :templates data)
        (om/update! menu-state :templates data)))

(defmethod handle-action :my-account-received [{:keys [data]}]
    (let [state (om/root-cursor app-state)]
      (om/update! state :account data)
      (om/update! (state :menu) :logged-in (data :logged-in))
      (om/update! (state :menu) :save-enabled (data :logged-in))
      (if (data :logged-in)
        (api/refresh-templates)
        (api/clear-templates)
      )))

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
  (debug "store/:processed-template data:" data)
  (let [state        (om/root-cursor app-state)
        editor-state (state :editor)
        output       (clj->json (data :processed))
        errors       (data :errors)]
        (om/update! editor-state :processed (data :processed))
        (om/update! editor-state :output output)))

(defmethod handle-action :menu-new [_]
  ; this is the same as 'deleted-template', need to merge
  ; somehow intelligently
  (let [state        (om/root-cursor app-state)
        editor-state (state :editor)
        menu-state   (state :menu)]
        (om/update! state :template nil)
        (om/update! editor-state :content "{}")
        (om/update! editor-state :output "{}")
        (om/update! editor-state :dirty false)
        (om/update! editor-state :processed {})
        ;; update the menu's state
        (om/transact! state :menu (fn [s]
          (let [s (assoc s :new-enabled true)
                s (assoc s :save-enabled true)
                s (assoc s :delete-enabled false)
                s (assoc s :sharing-enabled false)
                s (assoc s :template-shared-statically false)
                s (assoc s :template-shared-dynamically false)
                s (assoc s :template-title "Untitled")]
            s )))
        )
  (-> js/document .-location (set! "#/")))

(defmethod handle-action :deleted-template [_]
  (let [state        (om/root-cursor app-state)
        editor-state (state :editor)
        menu-state   (state :menu)]
        (om/update! state :template nil)
        (om/update! editor-state :content "{}")
        (om/update! editor-state :output "{}")
        (om/update! editor-state :dirty false)
        (om/update! editor-state :processed {})
        ;; update the menu's state
        (om/transact! state :menu (fn [s]
          (let [s (assoc s :new-enabled true)
                s (assoc s :save-enabled true)
                s (assoc s :delete-enabled false)
                s (assoc s :sharing-enabled false)
                s (assoc s :template-shared-statically false)
                s (assoc s :template-shared-dynamically false)
                s (assoc s :template-title "Untitled")]
            s )))
        )
  (-> js/document .-location (set! "#/")))


(defmethod handle-action :loaded-template [{{:keys [processed template]} :data}]
  (debug "store/:loaded-template template:" template)
  (let [state        (om/root-cursor app-state)
        editor-state (state :editor)
        menu-state   (state :menu)
        output       (clj->json processed)]
        (om/update! state :template template) ; store off the template.
        (om/update! editor-state :content (template :content))
        (om/update! editor-state :output output)
        (om/update! editor-state :dirty false)
        (om/update! editor-state :processed processed)
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

(defmethod handle-action :rename-template [{:keys [data]}]
  (let [state        (om/root-cursor app-state)
        template-cursor (state :template)]
    (om/update! template-cursor :title data)
    (d/action :menu-save)))

(defmethod handle-action :process-template [{:keys [data]}]
  (api/process-template data))

(defmethod handle-action :default [msg]
  (js/alert (str "No handle-action method for " msg)))

(defmethod handle-action :load-flash [_]
  (api/load-flash))

;; store methods -- handle requests, call the API, update the store...
; (defn debounce [in ms]
;   (let [out (chan)]
;     (go-loop [last-val nil]
;       (let [val (if (nil? last-val) (<! in) last-val)
;             timer (timeout ms)
;             [new-val ch] (alts! [in timer])]
;         (condp = ch
;           timer (do (>! out val) (recur nil))
;           in (recur new-val))))
;     out))
(defonce channel (d/subscribe))
(go-loop []
  (let [msg (<! channel)]
    ; (debug "Got Dispatcher Message " msg)
    (try
      (handle-action msg)
      (catch :default e (println e)))
    (recur)))
    ; (.assign (aget js/window "location") "/auth/github")))
