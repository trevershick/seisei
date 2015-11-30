(ns seisei.ui.state)

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
                        :rename     {
                                      :show false
                        }
                        :confirm    {
                                      :show false
                                      :title nil
                                      :question nil
                                      :confirm-action nil
                                      :confirm-data nil
                                      :deny-action nil
                                      :deny-data nil
                        }
                        :samples      {
                          :samples  []
                          :mixed    []
                        }
                        :templates    []
                        :show-hotkeys false
                        :account      { :logged-in false }
                        :menu { :logged-in        false ; duplicateion from :account, but is that ok?
                                :new-enabled      true
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
