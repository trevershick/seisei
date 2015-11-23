(ns seisei.ui.account
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
    [om.dom :as dom :include-macros true]
    [seisei.ui.state :as state]
    [seisei.ui.dispatcher :as d]
    [ajax.core :refer [GET POST]]
    [cljs.core.async :refer [<! put!]]))

(enable-console-print!)
(println "Initializing State for Account")
(swap! state/app-state assoc :account {})
(println "Swapped in state is " state/app-state)

(defn cursor-from-root [root-cursor]
  ; (println "cursor from root " root-cursor)
  (root-cursor :account))

;; Ajax Handlers
(defn handler-my-account [response]
  ; (println "My Account Response is " (str response))
  (let [data (om/root-cursor state/app-state)]
    (om/update! data :account response)
    (d/action :my-account response)))

;; event Handlers
(defn on-click-logout [e] (.assign (aget js/window "location") "/auth/logout"))
(defn on-click-login-github [e] (.assign (aget js/window "location") "/auth/github"))









;; Components
(defn view-not-logged-in [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (dom/span nil "Not Logged In")
        (dom/a #js {:onClick on-click-login-github } "Login With Github")
      ))))

(defn view-logged-in [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (dom/span nil (str "Logged In As " (data :name)))
        (dom/a #js {:onClick on-click-logout } "Logout")
      ))))

(defn view-logged-inornot [data owner]
  (reify
    om/IRender
    (render [_]
      (println "data logged in " data)
      (println "logged in? " (data :logged-in))
      (if (data :logged-in)
        (om/build view-logged-in data)
        (om/build view-not-logged-in data)))))
