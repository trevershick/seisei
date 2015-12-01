(ns seisei.ui.leaderboard.api
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [ajax.core :refer [GET]]
            [seisei.ui.dispatcher :as d]
            [seisei.ui.leaderboard.store]
            [seisei.ui.api :refer [default-error-handler]]
            [seisei.ui.util :refer [debug]]))


(defn load-leaderboard []
  (debug "leaderboard/api/load-leaderboard")
  (GET "/leaderboard"
    { :keywords?        true
      :response-format  :json
      :error-handler    seisei.ui.api/default-error-handler
      :handler          (fn [r] (d/action :leaderboard-received r))
    }))
