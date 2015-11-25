(ns seisei.ui.comp.menu
  (:require [om.core :as om :include-macros true]
            [seisei.ui.dispatcher :as d]
            [sablono.core :as html :refer-macros [html]]))

; overloaded with multiple arities
(defn- simple-menu-item-handler
  ([action]
    (simple-menu-item-handler action nil))
  ([action opts]
    (fn [e]
      (.stopPropagation e)
      (d/action action opts))))

(defn- highlight-first [s mnemonic]
  (let [idx     (.indexOf s mnemonic)
        left    (subs s 0 idx)
        right   (subs s (inc idx) (count s))]
    (html
      [:span left [:u mnemonic] right])))


(defn logout-button [data owner]
  (om/component
    (html
      [:a {:onClick (simple-menu-item-handler :logout) } "Logout"])))

(defn login-with-github [data owner]
  (om/component
    (html
      [:a { :className "btn btn-block btn-social btn-github" :onClick (simple-menu-item-handler :login) }
        [:i {:className "fa fa-github"}]
        "Sign in with Github" ])))

(defn template-submenu [data owner]
  (println "template-submenu data is " data)
  (om/component
    (let [dyn-url   (data :dynamic-url)
          sta-url   (data :static-url)
          title     (data :title)
          slug      (data :slug)]
          (html
            [:li
              (if sta-url [:a { :target "_new" :className "static-link" :href sta-url} [:span {:className "glyphicon glyphicon-link"}]])
              (if dyn-url [:a { :target "_new" :className "dynamic-link" :href dyn-url } [:span {:className "glyphicon glyphicon-flash"}]])
                [:a { :onClick (simple-menu-item-handler :menu-template { :slug slug }) :className "main-link"} title]]))))

(defn templates-submenu [data owner]
  ; (println "templates-submenu data is " data)
  (println "count is " (> (count (data :templates) 0)))
  (om/component
    (if (and (contains? data :templates) (> (count (data :templates)) 0))
      (html
        [:li {:className "templates-menu"}
            [:a {:href "#" :className "dropdown-toggle" :data-toggle "dropdown" :role "button" :aria-expanded "false"}
              "Templates"
              [:span {:className "caret"}]]
            [:ul {:className "dropdown-menu" :role "menu"}
                (om/build-all template-submenu (data :templates) {:key :slug})
                ]])
      (html [:noscript]))))

;; this should only be shown if we have a 'current template'
(defn sharing-submenu [data owner]
  (let [ dynamically-shared   (data :template-shared-dynamically)
         statically-shared    (data :template-shared-statically)
         dynamic-class-name   (if dynamically-shared "glyphicon-check" "glyphicon-unchecked")
         static-class-name    (if statically-shared "glyphicon-check" "glyphicon-unchecked")]
  (om/component
    (html
      [:li {:className "sharing-menu"}
        [:a {:href "#" :className "dropdown-toggle" :data-toggle "dropdown" :role "button" :aria-expanded "false"}
          "Sharing" [:span {:className "caret"}]]
          [:ul {:className "dropdown-menu" :role "menu"}
            ;; if statically or dynamically shared, then change the checkbox accordingly
            ;; clicking the main menu portion will share the item
            ;; clicking the checkbox portion will toggle the published state
            [:li
              [:a {:onClick (simple-menu-item-handler :menu-toggle-static) :target "_new" :className "static-link"} [:span {:className (str "glyphicon " static-class-name)}]]
              [:a {:onClick (simple-menu-item-handler :menu-publish-static) :className "main-link" :href "#"} "Static Version"]]
            [:li
              [:a {:onClick (simple-menu-item-handler :menu-toggle-dynamic) :target "_new" :className "static-link"} [:span {:className (str "glyphicon " dynamic-class-name)}]]
              [:a {:onClick (simple-menu-item-handler :menu-publish-dynamic) :className "main-link" :href "#"} "Dynamic Version"]]]]))))

(defn menu-item [title & {:keys [mnemonic icon action opts]}]
  (let [ menu-text (if mnemonic (highlight-first title mnemonic) title)
         className (if icon (str "glyphicon glyphicon-" icon)) ]
    (html
      [:li
        [:a {:href "#" :onClick (simple-menu-item-handler action opts) }
          [:span {:className className}] menu-text
    ]])))

(defn editor-menu [data owner]
  (om/component
    (html
      [:nav { :className "navbar navbar-default" }
        [:div {:className "container-fluid"}
          [:div {:className "navbar-header"}
            [:a {:className "navbar-brand" :href "#"} "生成"]
          ]
          [:div {:className "collapse navbar-collapse" :id "bs-example-navbar-collapse-1"}
            [:div
              [:ul {:className "nav navbar-nav"}
                (if (data :new-enabled)
                  (menu-item "New" :mnemonic "e" :icon "plus" :action :menu-new))
                (if (data :save-enabled)
                  (menu-item "Save" :mnemonic "S" :icon "pencil" :action :menu-save))
                (if (data :run-enabled)
                  (menu-item "Run" :mnemonic "R" :icon "play" :action :menu-run))
                (if (data :tidy-enabled)
                  (menu-item "Tidy" :mnemonic "i" :icon "indent-left" :action :menu-tidy))
                (if (data :delete-enabled)
                  (menu-item "Delete" :mnemonic "D" :icon "trash" :action :menu-delete))
                (if (data :sharing-enabled)
                  (om/build sharing-submenu data))
                (om/build templates-submenu data)
                (if (data :help-enabled)
                  (menu-item "Help" :action :menu-help)) ]

                [:ul {:className "nav navbar-nav navbar-right"}
                  (if (data :feedback-enabled)
                  (menu-item "Feedback" :icon "feedback" :action :menu-feedback))
                  [:li
                    (if (not (data :logged-in))
                      (om/build login-with-github data)
                      (om/build logout-button data))]]

              (if (data :template-title)
                [:ul {:className "nav navbar-nav navbar-right"}
                  [:li {:id "rename-menu-item"}
                    [:a { :onClick (simple-menu-item-handler :menu-rename )} (data :template-title) ]]])
          ]]]])))
(if js/Mousetrap
  (do
    (.bindGlobal js/Mousetrap "esc" (fn [] (d/action :toggle-hotkeys nil) false))
    (.bindGlobal js/Mousetrap "mod+e" (fn [] (d/action :hotkey-new nil) false))
    (.bindGlobal js/Mousetrap "mod+d" (fn [] (d/action :hotkey-delete nil) false))
    (.bindGlobal js/Mousetrap "mod+s" (fn [] (d/action :hotkey-save nil) false))
    (.bindGlobal js/Mousetrap "mod+r" (fn [] (d/action :hotkey-run nil) false))
    (.bindGlobal js/Mousetrap "mod+i" (fn [] (d/action :hotkey-tidy nil) false))
    (.bindGlobal js/Mousetrap "mod+h" (fn [] (d/action :hotkey-help nil) false))
  ))
