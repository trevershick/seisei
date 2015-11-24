(ns seisei.ui.state)
(enable-console-print!)
(println "Initializing State")
(def app-state (atom {  :editor     {
                                      :dirty    false
                                      :output   "Test Output" ; always a string
                                      :content  "{ \"Nothing here\" : true }" }
                        :samples      {
                          :samples  []
                          :mixed    []
                        }
                        :templates    []
                        :show-hotkeys false
                        :account      { :logged-in false }
                        :menu { :logged-in        false ; duplicateion from :account, but is that ok?
                                :new-enabled      false
                                :save-enabled     false
                                :run-enabled      true
                                :tidy-enabled     true
                                :delete-enabled   false
                                :sharing-enabled  false
                                :help-enabled     true
                                :template-title   "(new template)"
                                :template-shared-statically false
                                :template-shared-dynamically false
                                :feedback-enabled true }}))
