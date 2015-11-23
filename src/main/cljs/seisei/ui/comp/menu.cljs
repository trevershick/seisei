(ns seisei.ui.comp.menu
  (:require [om.core :as om :include-macros true]
            [seisei.ui.templates :as tmpl]
            [seisei.ui.dispatcher :as d]
            [sablono.core :as html :refer-macros [html]]))

(defn login-with-github [data owner]
  (om/component
    (html
      [:a { :className "btn btn-block btn-social btn-github" :onClick (fn [e] (d/action :login nil)) }
        [:i {:className "fa fa-github"}]
        "Sign in with Github" ])))
; <login-with-github>
; 	<a onclick={opts.login} class="btn btn-block btn-social btn-github" if={! opts.loggedin && opts.login }>
; 		<i class="fa fa-github"></i>
; 		Sign in with Github
; 	</a>
; 	<a onclick={ opts.logout } class="btn btn-block" if={ opts.logout && opts.loggedin }>
; 		Logout
; 	</a>
;
; </login-with-github>
;

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
              (if dyn-url [:a { :target "_new" :class "dynamic-link" :href dyn-url } [:span {:className "glyphicon glyphicon-flash"}]])
                [:a {:href (str "#template/" slug) :className "main-link"} title]]))))

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
  (let [ dynamically-shared (contains? data :dynamic-url)
         statically-shared (contains? data :static-url)
         dynamic-class-name (if dynamically-shared "glyphicon-check" "glyphicon-unchecked")
         static-class-name (if statically-shared "glyphicon-check" "glyphicon-unchecked")]
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
              [:a {:target "_new" :className "static-link"} [:span {:className (str "glyphicon " static-class-name)}]]
              [:a {:className "main-link" :href "#"} "Static Version"]]
            [:li
              [:a {:alt "View Dynamic Version" :className "static-link"} [:span {:className (str "glyphicon " dynamic-class-name)}]]
              [:a {:className "main-link" :href "#"} "Dynamic Version"]]]]))))


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
                  [:li
                    [:a {:href "#"}
                      [:span {:className "glyphicon glyphicon-plus"}] "N" [:u "e"] "w"]])
                (if (data :save-enabled)
                  [:li
                    [:a {:href "#"}
                      [:span {:className "glyphicon glyphicon-pencil"}] [:u "S"] "ave"]])
                (if (data :run-enabled)
                  [:li
                    [:a {:href "#"}
                      [:span {:className "glyphicon glyphicon-play"}] [:u "R"] "un" ]])
                (if (data :tidy-enabled)
                  [:li
                    [:a {:href "#"}
                      [:span {:className "glyphicon glyphicon-indent-left"}] "T" [:u "i"] "dy"]])
                (if (data :delete-enabled)
                  [:li
                    [:a {:href "#"}
                      [:span {:className "glyphicon glyphicon-trash"}] [:u "D"] "elete"]])
                (if (data :sharing-enabled)
                  (om/build sharing-submenu data))
                (om/build templates-submenu data)
                (if (data :help-enabled)
                  [:li [:a "Help"]])]

                [:ul {:className "nav navbar-nav navbar-right"}
                  (if (data :feedback-enabled) [:li [:a {:href "#"} [:span {:className "glyphicon glyphicon-feedback"}] " Feedback"]])
                  (if (not (data :logged-in))
                    [:li
                      (om/build login-with-github data)])]

              (if (data :template-title)
                [:ul {:className "nav navbar-nav navbar-right"}
                  [:li {:id "rename-menu-item"}
                    [:a (data :template-title) ]]])
          ]]]]
                    )))
