(ns seisei.web.user)


(defn logged-in? [session] (:logged-in session))
(defn logged-in!
  [session b]
  (assoc session :logged-in b))
