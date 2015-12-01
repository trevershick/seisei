(ns seisei.ui.leaderboard
  (:require [om.core :as om :include-macros true]
            [seisei.ui.leaderboard.store :as store]
            [seisei.ui.dispatcher :as d]
            [sablono.core :as html :refer-macros [html]]))

(defn- panel [opts & children]
    (html
      [:div { :className (str (opts :className) " panel panel-default") }
        [:div { :className "panel-heading" }
          [:h3 { :className "panel-title" } (opts :title) ]
        ]
        [:div { :className "panel-body" }
          children
        ]
      ]))

(defn- leader-list-item [data owner]
  (om/component
    (html
        [:ul {:className "row list-group-item leader-list-item"}
          [:li { :className "col-sm-6" } (data :user)]
          [:li { :className "col-sm-4" } (data :slug)]
          [:li { :className "col-sm-2" } (data :score)]
        ])))

(defn- leader-list [data owner]
  (om/component
    (html
      [:div {:className "list-group" }
        [:ul {:className "row list-group-item active leader-list-item"}
          [:li { :className "col-sm-6" } "Author"]
          [:li { :className "col-sm-4" } "Template"]
          [:li { :className "col-sm-2" } "Score"]
        ]
        (when (empty? data)
          [:span {:className "row"}"No Data."])
        (om/build-all leader-list-item data {:key :slug})
      ]
    )))

(defn- menu [data owner]
  (om/component
    (html
      [:nav { :className "navbar navbar-default" }
        [:div {:className "container-fluid"}
          [:div {:className "navbar-header"}
            [:a {:className "navbar-brand" :href "#"} "生成 - Leaderboard"]
          ]
          [:div {:className "collapse navbar-collapse" :id "bs-example-navbar-collapse-1"}
            [:div
              [:ul {:className "nav navbar-nav"}
                [:li [:a {:onClick (fn [e] (.stopPropagation e) (d/action :load-leaderboard))}
                  [:span {:className "glyphicon glyphicon-refresh"}]
                ]]
              ]
              [:ul {:className "nav navbar-nav navbar-right"}
                [:li [:a {:href "#/"} "Back to Editor" ]]
              ]
            ]
          ]
        ]
      ])))

(defn leaderboard [data owner]
  (reify
    om/IRender
    (render [_]
      (html
        [:div { :className "leaderboard" }
          (om/build menu data)
          [:div {:className "col-lg-6 col-md-6 col-sm-12"}
            (panel {:title "Static Endpoint Scores" :className ""}
              (om/build leader-list (data :static))
            )]
          [:div {:className "col-lg-6 col-md-6 col-sm-12"}
            (panel {:title "Dynamic Endpoint Scores" :className ""}
              (om/build leader-list (data :dynamic))
            )
          ]
        ]))
    om/IDidMount
    (did-mount [_] (d/action :load-leaderboard))
  ))
