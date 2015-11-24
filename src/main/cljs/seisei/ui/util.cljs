(ns seisei.ui.util)

(defn clj->json
  [ds]
  (.stringify js/JSON (clj->js ds) nil 2))
