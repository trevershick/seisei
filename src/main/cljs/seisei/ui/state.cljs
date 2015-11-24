(ns seisei.ui.state)
(enable-console-print!)
(println "Initializing State")
(def app-state (atom { :menu {  :logged-in false
                                :new-enabled true
                                :save-enabled true
                                :run-enabled true
                                :tidy-enabled true
                                :delete-enabled true
                                :sharing-enabled true
                                :help-enabled true
                                :template-title "TT Here"
                                :feedback-enabled true } }))
