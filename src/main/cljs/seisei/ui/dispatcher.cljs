(ns seisei.ui.dispatcher
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
    [cljs.core.async :refer [sub pub put! chan mult tap <!]]))

(def dispatcher (chan))

(def mult-c (mult dispatcher))

; (def action-topic (pub dispatcher :action))
(defn subscribe []
   (let [the-channel (chan)]
    (tap mult-c the-channel)
    the-channel))

; (defn subscribe [topic-name]
;   (let [the-channel (chan)]
;     (sub action-topic topic-name the-channel)
;     the-channel))

(defn action [action data]
  (println "Firing " action " with " data)
  (put! dispatcher {:action action :data data}))
