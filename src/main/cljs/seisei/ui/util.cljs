(ns seisei.ui.util)

(enable-console-print!)

(defn clj->json
  ([edn] (clj->json edn 2))
  ([edn spaces]
      (.stringify js/JSON (clj->js edn) nil spaces)))

(defn nnil? [v] (not (nil? v)))
(defn debug
  ([message]
    (.debug js/console message))
  ([message arg]
    (.debug js/console message (clj->js arg))))
