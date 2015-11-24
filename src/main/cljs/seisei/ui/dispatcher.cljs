(ns seisei.ui.dispatcher
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
    [cljs.core.async :as async]))

(def dispatcher (async/chan 1000))

(def mult-c (async/mult dispatcher))

; (def action-topic (pub dispatcher :action))
(defn subscribe []
   (let [the-channel (async/chan)]
    (async/tap mult-c the-channel)
    the-channel))

; (defn subscribe [topic-name]
;   (let [the-channel (chan)]
;     (sub action-topic topic-name the-channel)
;     the-channel))

(defn action [action data]
  (let [message   {:action action :data data}
        _         (println "Got Dispatcher Message - Firing " message)
        result    (async/put! dispatcher message)]))
