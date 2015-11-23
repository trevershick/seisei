(ns seisei.ui.comp.menu
  (:require [om.core :as om :include-macros true]
            [seisei.ui.templates :as tmpl]
            [sablono.core :as html :refer-macros [html]]))


(defn template-submenu [data owner]
  (println "template-submenu data is " data)
  (om/component
    (let [dyn-url   (data :dynamic-url false)
          sta-url   (data :static-url false)
          title     (data :title false)
          slug      (data :slug false)]
          (html
            [:li
              (if sta-url [:a { :target "_new" :className "static-link" :href sta-url} [:span {:className "glyphicon glyphicon-link"}]])
              (if dyn-url [:a { :target "_new" :class "dynamic-link" :href dyn-url } [:span {:className "glyphicon glyphicon-flash"}]])
                [:a {:href (str "#template/" slug) :className "main-link"} title]]))))

(defn templates-submenu [data owner]
  (println "templates-submenu data is " data)
	(om/component
    (if (contains? data :templates)
  		(html
  			[:li {:className "templates-menu"}
  					[:a {:href "#" :className "dropdown-toggle" :data-toggle "dropdown" :role "button" :aria-expanded "false"}
  						"Templates"
  						[:span {:className "caret"}]]
  					[:ul {:className "dropdown-menu" :role "menu"}
                (om/build-all template-submenu (data :templates) {:key :slug})
                ]])
      (html [:li]))))


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
                  [:li
                    [:a {:href "#"}
                      [:span {:className "glyphicon glyphicon-plus"}]
                      "N" [:u "e"] "w"]]
                  [:li
                    [:a {:href "#"}
                      [:span {:className "glyphicon glyphicon-pencil"}]
                      [:u "S"] "ave"]]
                  [:li
                    [:a {:href "#"}
                      [:span {:className "glyphicon glyphicon-play"}]
                      [:u "R"] "un" ]]
                  [:li
                    [:a {:href "#"}
                      [:span {:className "glyphicon glyphicon-indent-left"}]
                      "T" [:u "i"] "dy"]]
                  [:li
                    [:a {:href "#"}
                      [:span {:className "glyphicon glyphicon-trash"}]
                      [:u "D"] "elete"]]

                  [:li {:className "sharing-menu"}
                      [:a {:href "#" :className "dropdown-toggle" :data-toggle "dropdown" :role "button" :aria-expanded "false"}
                        "Sharing" [:span {:className "caret"}]]
	                      [:ul {:className "dropdown-menu" :role "menu"}
	                          [:li
	                              [:a {:target "_new" :className "static-link"}
	                                [:span :className "glyphicon glyphicon-check"]]
	                              [:a {:className "main-link" :href "#"} "Static Version"]]
	                          [:li]
	                              [:a {:alt "View Dynamic Version" :className "static-link"}
	                                [:span {:className "glyphicon glyphicon-check"}]]
	                              [:a {:alt "(Re)Publish the dynamic version of this template." :className "main-link"} "Dynamic Version"]]]

                  (om/build templates-submenu data)
                  [:li [:a "Help"]]

              [:ul {:className "nav navbar-nav navbar-right"}
                  [:li
                    [:a {:href "#"}
                      [:span {:className "glyphicon glyphicon-feedback"}] " Feedback"]]
                  [:li [:login-with-github]]]

              [:ul {:className "nav navbar-nav navbar-right"}
                  [:li {:id "rename-menu-item"}
                    [:a] "Template Title Here" ]]

                    ]]]]]
                    )))
