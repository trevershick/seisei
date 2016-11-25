(ns seisei.ui.state
  (:require [seisei.ui.util :refer [clj->json nnil? debug]]))

(def ^{:private true} starting-json
  {
    :cats [
      "{{repeat(2)}}"
      {
        :i           "{{index}}"
        :name        "{{cat.name}}"
        :breed       "{{cat.breed}}"
        :registry    "{{cat.registry}}"
        :homepage    "http://{{internet.url}}"
       }
    ]
    :code {
      :isbn10 "{{code.isbn10}}"
      :isbn13 "{{code.isbn13}}"
    }
    :chuck-norris "{{chuckNorris.fact}}"
  })

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
                                      :content    (clj->json starting-json) }
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
                        :samples-collapsed true
                        :samples      {
                          :samples    []
                          :mixed      []
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
