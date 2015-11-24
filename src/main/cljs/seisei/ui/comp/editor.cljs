(ns seisei.ui.comp.editor
	(:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [seisei.ui.dispatcher :as d]
						[seisei.ui.util :refer [clj->json]]
            [sablono.core :as html :refer-macros [html]]))

(def *ace* (atom nil))
(defn editor [data owner]
	(reify
		om/IRender
		(render [_]
			(if @*ace*
				(let [new-val          (data :content)
							cur-val    (.getValue @*ace*) ]
							(if (not= new-val cur-val)
								(do
									(println "Updating editor with new content")
									(.setValue @*ace* new-val -1)))))
			(html [:div { :name "editorElement" :className "editor" }]))
		om/IWillUnmount
		(will-unmount [_]
			(println "editor will-unmount")
			(if @*ace*
				(do
					(.destroy @*ace*)
					(reset! *ace* nil))))
		om/IDidMount
		(did-mount [_]
			(let [ace-instance (.edit js/ace (.getDOMNode owner))]
				(.setTheme ace-instance "ace/theme/twilight")
				(.setMode (.getSession ace-instance) "ace/mode/javascript")
				(.setReadOnly ace-instance false)
				(.setHighlightActiveLine ace-instance false)
				(.setValue ace-instance (data :content))
				(.on ace-instance "change" (fn [] (go [] (d/action :editor-updated (.getValue @*ace*)))))
      	(reset! *ace* ace-instance)))
			)
		)

(def *ace-ro* (atom nil))
(defn editor-ro [data owner]
	(reify
		om/IRender
		(render [_]
			(if @*ace-ro*
				(let [new-val          (data :output)
							cur-val          (.getValue @*ace-ro*) ]
							(if (not= new-val cur-val)
								(do
									(println "Updating editor with new content")
									(.setValue @*ace-ro* new-val -1)))))
			(html [:div { :name "editorElement" :className "editor editor-ro" }]))
		om/IWillUnmount
		(will-unmount [_]
			(println "editor-ro will-unmount")
			(if @*ace-ro*
				(do
					(.destroy @*ace-ro*)
					(reset! *ace-ro* nil))))
		om/IDidMount
		(did-mount [_]
			(let [ace-instance (.edit js/ace (.getDOMNode owner))]
				(.setTheme ace-instance "ace/theme/twilight")
				(.setMode (.getSession ace-instance) "ace/mode/javascript")
				(.setValue ace-instance (data :output))
				(.setReadOnly ace-instance true)
      	(reset! *ace-ro* ace-instance)))
			)
		)

(defn- sample-li-input-output [io]
	(html
		[:div {:className "sample"}
			[:div {:className "input"} [:a (clj->json (io :input) 0)]] [:div {:className "output"} (clj->json (io :output) 0)]
		]))

(defn- sample-li [sample]
	(html
		[:li {:className "tag-samples"}
			[:div {:className "tag"} (sample :name) ]
			[:div {:className "samples"} (map sample-li-input-output (sample :samples)) ]
		]))
(defn editor-help [data owner]
	(om/component
		(html
			[:div {:className "editor-help help"}
			  [:h1 "Directives"]
				[:ul (map sample-li (data :samples)) ]
			  [:h1 "Other Examples"]
				[:ul (map sample-li (data :mixed)) ]]
			)))
; )))
