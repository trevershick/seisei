(ns seisei.ui.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [seisei.ui.dispatcher :as d]
		        [seisei.ui.state :as state]
            [seisei.ui.comp.messages :refer [messages]]
            [seisei.ui.comp.menu :refer [editor-menu]]
            [seisei.ui.comp.editor :refer [editor editor-ro editor-help]]
            [seisei.ui.comp.modals :refer [confirm-modal rename-modal]]
            [seisei.ui.store :as store]
            [sablono.core :as html :refer-macros [html]]
            [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(defn tweet [data owner]
  (dom/a #js { :href "https://twitter.com/intent/tweet?button_hashtag=seisei&text=Thanks%20@trevermshick%20I%20love%20it!"
      :className "twitter-hashtag-button"
      :data-related "trevermshick"
      :data-url "http://seisei.elasticbeanstalk.com" } "Tweet #seisei"))

(defn footer [data owner]
  (om/component
  (dom/div #js {:style #js {:position "absolute" :bottom 5 :left 10 :right 0}}
    (dom/a #js { :href "https://github.com/trevershick" }
      (dom/span nil "Check me out on ")
      (dom/img #js { :className "emoji"
        :title ":octocat:"
        :alt ":octocat:"
        :src "https://assets.github.com/images/icons/emoji/octocat.png"
        :height "20"
        :width "20"
        :align "absmiddle"})
      (dom/span nil " or ")
      (dom/a #js { :href "http://trevershick.github.io" } "my blog")
      (dom/span nil " or even ")
      (dom/div #js { :style  #js { :display "inline-block" :margin 0 :padding 0 :verticalAlign "middle"}}
        (tweet data owner))
      ))))


(defn hotkey-prompt [data owner]
  (om/component
    (html
      [:div {:className "lr"}
        [:kbd (data :key)]
        [:span " " (data :purpose)]])))

(defn hotkeys [data owner]
  (om/component
    (if (data :show-hotkeys)
      (html
        [:div {:className "backdrop" }
          [:div {:className "hotkeys"}
            [:ul
              [:li [:span { :className "kk" } [:kbd { :className "light cmd"} "⌘"] "+" [:kbd { :className "light"} "i"]] [:p "Tidy up your JSON"]]
      				[:li [:span { :className "kk" } [:kbd { :className "light cmd"} "⌘"] "+" [:kbd { :className "light"} "r"]] [:p "Run your template on the Server"]]
      				[:li [:span { :className "kk" } [:kbd { :className "light cmd"} "⌘"] "+" [:kbd { :className "light"} "s"]] [:p "Save your template on the server"]]
              [:li [:span { :className "kk" } [:kbd { :className "light cmd"} "⌘"] "+" [:kbd { :className "light"} "e"]] [:p "Create a new template"]]
              [:li [:span { :className "kk" } [:kbd { :className "light cmd"} "⌘"] "+" [:kbd { :className "light"} "s"]] [:p "Delete the current template"]]
            ]]
          (om/build hotkey-prompt {:key "⌘ + h" :purpose "to dismiss"})
        ])
      (html [:noscript]))))


(defn app [data owner]
  (om/component
    (html
      [:div {:className "editor-app"}
        (om/build confirm-modal (data :confirm))
        (om/build rename-modal (data :rename))
        (om/build hotkeys data)
        (om/build messages (data :messages))
        [:div { :className "row" } (om/build editor-menu (data :menu))]
        [:div { :className "row" } (om/build editor (data :editor))]
        [:div { :className "row editor-row" }
            [:div { :className "row" :style { :height "100%" } }
                [:div { :className "col-sm-6 left-side" :style {:height "100%" :padding 0} }
                    (om/build editor (data :editor))
                    [:div { :style { :textAlign "right" } }
                      [:b { :className "floater" } [:span { :className "PROCESSEDCLASSES" } ]]
                    ]
                  ]
                [:div { :className "col-sm-6 right-side" :style { :height "100%" :padding 0} }
                    (om/build editor-ro (data :editor))
                    (om/build editor-help (data :samples))
                  ]]]
        (om/build hotkey-prompt {:key "⌘ + h" :purpose "to show hotkeys"})
        (om/build footer data)
      ]
    )))

(def app-element (. js/document (getElementById "app")))
(om/root app state/app-state {:target app-element})
(store/init)
