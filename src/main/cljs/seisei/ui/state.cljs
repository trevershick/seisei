(ns seisei.ui.state)
(enable-console-print!)
(println "Initializing State")
(def app-state (atom {  :editor {
                          :output   "Test Output"
                          :id       nil
                          :title    "Untitled"
                          :content  "{ \"Nothing here\" : true }" }
                        :menu { :logged-in        false
                                :new-enabled      false
                                :save-enabled     false
                                :run-enabled      true
                                :tidy-enabled     true
                                :delete-enabled   false
                                :sharing-enabled  false
                                :help-enabled     true
                                :template-title   "(new template)"
                                :feedback-enabled true }}))
