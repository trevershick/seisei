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
