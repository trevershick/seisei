(ns seisei.ui.state)
(enable-console-print!)

(def message-counter (atom 0))
(def app-state (atom {  :messages   [
                          ; {:type :alert    :message "test alert"   :id (swap! message-counter inc)}
                          ; {:type :info     :message "test info"    :id (swap! message-counter inc)}
                          ; {:type :warn     :message "test warn"    :id (swap! message-counter inc)}
                          ; {:type :success  :message "test success" :id (swap! message-counter inc)}
                        ]
                        :editor     {
                                      :dirty    false
                                      :processed  {}
                                      :output     "" ; always a string
                                      :content    "{\"c\":[\"{{repeat(3)}}\",{\"x\":1}]}" }
                        :samples      {
                          :samples  []
                          :mixed    []
                        }
                        :templates    []
                        :show-hotkeys false
                        :account      { :logged-in false }
                        :menu { :logged-in        false ; duplicateion from :account, but is that ok?
                                :new-enabled      false
                                :save-enabled     true
                                :run-enabled      true
                                :tidy-enabled     true
                                :delete-enabled   false
                                :sharing-enabled  false
                                :help-enabled     true
                                :template-title   "(new template)"
                                :template-shared-statically false
                                :template-shared-dynamically false
                                :feedback-enabled true }}))
