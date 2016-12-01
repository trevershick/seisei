(ns seisei.web.auth
  (:require [buddy.auth :refer [authenticated?]]
            [buddy.auth.backends.session :refer [session-backend]]
            [buddy.auth.middleware :refer [wrap-authentication wrap-authorization]]
            [buddy.auth.protocols :as proto]
            [buddy.auth.http :as http]))

(def auth-backend
  (session-backend))

(defn logged-in? [request] (authenticated? request))

(defn logged-in!
  [session user-record]
  (assoc session :identity user-record))

(defn user-from
  [request]
  (-> request :identity))

(defn user-id
  [request]
  (:id (user-from request)))

(defn- requires-user-middleware [handler]
  (fn
    [request]
    (if
     (logged-in? request)
      (handler request)
      {:status 403})))

;; applies a buddy authentication backend.  this doesn't PROTECT
;; the resources, it simply associates the request with an identity if
;; available
(defn with-user [handler]
  (wrap-authentication handler auth-backend))

;; ensures that any route wrapped by this middleware is only
;; called if the user is authenticated
;; usage (wrap-routes protected-routes auth/ensure-authenticated)
(defn ensure-authenticated
  [routes]
  (requires-user-middleware routes))
