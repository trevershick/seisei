(ns seisei.web.leaderboard
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [compojure.core :refer [GET defroutes]]
            [seisei.db.leaderboard :as lb]))


(defroutes routes
  (GET "/leaderboard" [request] {:status 200 :body (lb/full-leader-board)}))
