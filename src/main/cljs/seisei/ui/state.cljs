(ns seisei.ui.state)
(enable-console-print!)
(println "Initializing State")
(defonce app-state (atom { :list ["Lion" "Zebra" "Buffalo" "Antelope"] }))
