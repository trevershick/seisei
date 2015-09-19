(ns seisei.web.healthcheck
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [compojure.core :refer [GET defroutes]]))


(defroutes healthcheck-routes
  (GET "/healthcheck" [request] {:status 200 :body "ok"}))
