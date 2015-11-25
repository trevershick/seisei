(ns seisei.ui.comp.modals
  (:require [om.core :as om :include-macros true]
            [seisei.ui.dispatcher :as d]
            [sablono.core :as html :refer-macros [html]]))

(defn- on-rename-save [data]
	(fn [e]
		(.stopPropagation e)
		(d/action :rename-template (data :value))
		(om/update! data :show false)))

(defn- on-rename-cancelled [data]
	(fn [e]
		(.stopPropagation e)
		(om/update! data :show false)))

(defn- on-change [data]
	(fn [e]
		(om/update! data :value (aget (aget e "target") "value"))))

(defn rename-modal [data owner]
	(om/component
		(let [show 					(data :show)
					modal-style 	(if show { :display "block" })
					value 				(data :value) ]
			(html
				[:div { :className "modal modal-backdrop" :style modal-style }
				  [:div { :className "modal-dialog" }
				    [:div { :className "modal-content" }
				      [:div { :className "modal-header" }
				        [:button  { :type "button" :className "close" :data-dismiss "modal" :aria-label "Close"}
									[:span { :aria-hidden true } "x" ]
								]
				        [:h4 { :className "modal-title" } "Rename Template" ]
							]
				      [:div { :className "modal-body" }
				      	[:div { :className "form-group" }
				      		[:label { :for "newName" } "Rename the template to : " ]
				      		[:input { :type "text" :name "newName" :id "newName" :value value :className "form-control" :onChange (on-change data) }]
								]
							]
							[:div { :className "modal-footer" }
								[:button { :className "btn btn-primary" :onClick (on-rename-save data) } "Save" ]
							  [:button { :className "btn btn-default" :onClick (on-rename-cancelled data) } "Cancel" ]
							]
						]
					]
				]
			))))
