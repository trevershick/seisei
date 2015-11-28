(ns seisei.web.flash
  (:require [ring.util.response :refer [response header]]
            [clojure.tools.logging :as log]))

(defn add-flash [session message]
  "Adds message to the :flash stack for session"
  (log/debugf "flash/add-flash called, message=%s" message)
  (let [flashes       (or (session :flash) [])
        flashes       (conj flashes message)
        session       (assoc session :flash flashes)]
    session))

(defn hot-flashes [{{flash :flash} :session session :session}]
  (->
    (response { :flash flash })
    (assoc :session (assoc session :flash []))))

(defn wrap-flash-header [handler]
  (fn [request]
    (-> (handler request)
        (header "X-Seisei-Flash" (not (empty? (-> request :session :flash)))))))
