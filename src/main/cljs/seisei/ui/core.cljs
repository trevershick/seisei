(ns seisei.ui.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [ajax.core :refer [GET POST]]
		        [seisei.ui.state :as state]
            [seisei.ui.account :as acct]
            [seisei.ui.templates :as tmpl]
            [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)


(defn handle-delete [d]
  (let [data (om/root-cursor state/app-state)]
    (om/transact! data :list
      (fn [animals] (subvec animals 1)))))

(defn tweet [data owner]
  (dom/a #js { :href "https://twitter.com/intent/tweet?button_hashtag=seisei&text=%23seisei%20is%20great!"
      :className "twitter-hashtag-button"
      :data-related "trevermshick"
      :data-url "http://seisei.elasticbeanstalk.com" } "Tweet #seisei"))

(defn list-component [data owner]
  (reify
    om/IRender
    (render [_]
      (println "animaals is " data)
      (dom/div nil
        (dom/div nil
        (dom/button #js {:onClick #(handle-delete data) } "Delete")
          (apply dom/ul nil
            (map (fn [text] (dom/li nil text)) data)))))))


(defn footer [data owner]
  (dom/div #js {:style #js {:position "absolute" :bottom 5 :left 10 :right 0}}
    (dom/a #js { :href "https://github.com/trevershick" }
      (dom/span nil "Check me out on")
      (dom/img #js { :class "emoji"
        :title ":octocat:"
        :alt ":octocat:"
        :src "https://assets.github.com/images/icons/emoji/octocat.png"
        :height "20"
        :width "20"
        :align "absmiddle"})
      (dom/span nil "or")
      (dom/a { :href "http://trevershick.github.io" } "my blog")
      (dom/span nil "or even")
      (dom/div #js { :style  #js { :display "inline-block" :margin 0 :padding 0 :verticalAlign "middle"}}
        (tweet data owner))
      )))


(defn hotkey-prompt [data owner]
  (dom/span #js {:class "kbd"} "esc")
  (dom/span nil " to show hotkeys"))


(defn app [data owner]
  (println "App, data is " data)
  (om/component
    (dom/div nil
      (om/build list-component (data :list))
      (om/build acct/view-logged-inornot (acct/cursor-from-root data))
      (om/build tmpl/view-my-templates (tmpl/cursor-from-root data))
    )))

(println (str "app-state is " @state/app-state))

(def app-element (. js/document (getElementById "app")))
(om/root app state/app-state {:target app-element})
(acct/init)
