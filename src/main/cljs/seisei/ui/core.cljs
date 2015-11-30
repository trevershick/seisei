(ns seisei.ui.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:import goog.History)
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [seisei.ui.dispatcher :as d]
            [seisei.ui.state :as state]
            [seisei.ui.comp.messages :refer [messages]]
            [seisei.ui.comp.menu :refer [editor-menu]]
            [seisei.ui.comp.editor :refer [editor editor-ro editor-help]]
            [seisei.ui.comp.modals :refer [confirm-modal rename-modal]]
            [goog.events :as events]
            [goog.history.EventType :as EventType]            [seisei.ui.store :as store]
            [secretary.core :as sec :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(defn navigation-view [_ _]
  (om/component
    (html [:noscript])))
      ; [:div
      ;   [:a { :href "#/" } "Home" ]
      ;   [:a { :href "#/something" } "Something" ]
      ;   [:a { :href "#/about" } "About" ]])))

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
        (om/build navigation-view {})
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



(defn something-page-view [_ _]
  (om/component
    (html
      [:div
        (om/build navigation-view {})
          [:div "Something"]])))

(defn about-page-view [_ _]
  (om/component
    (html
      [:div
        (om/build navigation-view {})
          [:div "About Page"]])))



; (om/root app state/app-state {:target app-element})
(def ^:private app-element (. js/document (getElementById "app")))
(def ^:private history-element (. js/document (getElementById "hidden-history")))
(sec/set-config! :prefix "#")

(sec/defroute index-page "/" []
  (om/root app state/app-state {:target app-element}))

(sec/defroute editor-with-template "/template/:slug" [slug]
  (om/root app state/app-state {:target app-element})
  (d/action :route-with-template-slug slug))

(sec/defroute something-page "/something" []
  (om/root something-page-view state/app-state {:target app-element}))

(sec/defroute about-page "/about" []
  (om/root about-page-view state/app-state {:target app-element}))

(sec/defroute catchall "*" []
  (js/alert "Set to #/")
  (-> js/document .-location (set! "#/")))

(let [history (History. false nil history-element)
      navigation EventType/NAVIGATE]
  (goog.events/listen history
                     navigation
                     #(-> % .-token sec/dispatch!))
  (doto history (.setEnabled true)))

(store/init)
