(ns seisei.ui.templates
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
    [om.dom :as dom :include-macros true]
    [ajax.core :refer [GET POST]]
    [seisei.ui.dispatcher :as d]
    [seisei.ui.state :as state]
    [seisei.ui.account :as acct]
    [cljs.core.async :refer [put! mult tap chan sub <!]]))

(declare refresh-templates)
(declare clear-templates)

(enable-console-print!)
(println "Initializing State for Templates")
(swap! state/app-state assoc :templates [])

(defn cursor-from-root [root-cursor]
  (root-cursor :templates))




;; Components
(defn view-template-temporary [data owner]
  (reify
    om/IDisplayName
    (display-name [_] "view-template-temporary")
    om/IRender
    (render [_]
      (dom/div nil (data :title)))))


(defn view-my-templates [data owner]
  (reify
    om/IDisplayName
    (display-name [_] "view-my-templates")
    om/IRender
    (render [_]
      ; (println "view-my-templates, data = " data)
      (dom/h1 nil "Templates")
      (dom/div nil
        (om/build-all view-template-temporary data {:key :slug})
        (dom/button #js {:onClick refresh-templates } "Refresh Templates")
      ))))
