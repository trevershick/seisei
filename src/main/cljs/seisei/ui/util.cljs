(ns seisei.ui.util)

(defn clj->json
  ([edn] (clj->json edn 2))
  ([edn spaces]
      (.stringify js/JSON (clj->js edn) nil spaces)))
