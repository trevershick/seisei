(ns seisei.ui.dispatcher
  (:require [seisei.ui.util :refer [debug]]
    [cljs.core.async :as async]))

(defonce ^:private dispatcher
  (async/chan 1000))

(defonce ^:private mult-c
  (async/mult dispatcher))

(defn subscribe []
  "Takes no argument and returns a core.async channel
  which can be polled for messages fired via seisei.ui.dispatcher/action"
  (let [the-channel (async/chan)]
    (async/tap mult-c the-channel)
    the-channel))

(defn action
  "Puts a message on the dispatcher in the form
  {:action action-keyword :data opts}."
  ([action-keyword]
    (action action-keyword nil))
  ([action-keyword opts]
    (let [message   {:action action-keyword :data opts}
          result    (async/put! dispatcher message)]
      (debug "dispatcher/action put!" message))))
