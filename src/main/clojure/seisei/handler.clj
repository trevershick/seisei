(ns seisei.handler
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [compojure.core :refer [GET defroutes]]
            [ring.util.response :refer [resource-response response]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :as middleware]))


(defroutes app-routes
  (GET "/" [] (resource-response "index.html" {:root "public"}))
  (GET "/callback" [] (response [{:name "Widget 1"} {:name "Widget 2"}]))
  (GET "/widgets" [] (response [{:name "Widget 1"} {:name "Widget 2"}]))
  (GET "/my/templates" [] (response ["a" "b"]))
  (route/resources "/")
  (route/not-found "Page not found"))

(def app
  (wrap-defaults app-routes site-defaults))

(def app
  (-> (handler/api app-routes)
      (middleware/wrap-json-body)
      (middleware/wrap-json-response)))