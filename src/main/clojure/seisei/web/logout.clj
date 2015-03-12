(ns seisei.web.logout
  (:require [compojure.core :refer [GET defroutes]]
            [ring.util.response :refer [resource-response response]]))

(defn logout-user
  []
  (-> (ring.util.response/redirect "/")
      (assoc :session nil)))


(defroutes logout-routes
  (GET "/auth/logout" [] (logout-user)))

