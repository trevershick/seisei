(ns seisei.ui.editor.ace
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [seisei.ui.dispatcher :as d]
            [seisei.ui.util :refer [clj->json debug]]
            [seisei.ui.editor.autocomplete :refer [autocomplete]]
            [sablono.core :as html :refer-macros [html]]))

(defn- onclick-handler
  ([action]
   (onclick-handler action nil))
  ([action opts]
   (fn [e]
     (.stopPropagation e)
     (d/action action opts)
     false)))

(def *ace* (atom nil))
(defn editor [data owner]
  (reify
    om/IRender
    (render [_]
      (if @*ace*
        (let [new-val    (data :content)
              cur-val    (.getValue @*ace*)]
          (when (not= new-val cur-val)
            (.setValue @*ace* new-val -1))))
      (html [:div {:name "editorElement" :className "editor"}]))
    om/IWillUnmount
    (will-unmount [_]
      (when @*ace*
        (.destroy @*ace*)
        (reset! *ace* nil)))
    om/IDidMount
    (did-mount [_]
      (let [ace-instance (.edit js/ace (.getDOMNode owner))]
        (.setTheme ace-instance "ace/theme/twilight")
        (.setMode (.getSession ace-instance) "ace/mode/javascript")
        (.setReadOnly ace-instance false)
        (set! (.-$blockScrolling ace-instance) Infinity)
        (def c (js-obj "getCompletions"
                       (fn [_editor _session _pos _prefix _callback]
                         (_callback nil autocomplete))))
        (set! (.-completers ace-instance) (clj->js [c]))
        (.setHighlightActiveLine ace-instance true)
        (.setOptions ace-instance (clj->js {:enableBasicAutocompletion true :enableSnippets false :enableLiveAutocompletion true}))
        (.setValue ace-instance (data :content) -1)
        (.on ace-instance "change" (fn [] (go [] (d/action :editor-updated (.getValue @*ace*)))))
        (reset! *ace* ace-instance)))))

(def *ace-ro* (atom nil))
(defn editor-ro [data owner]
  (reify
    om/IRender
    (render [_]
      (if @*ace-ro*
        (let [new-val          (data :output)
              cur-val          (.getValue @*ace-ro*)]
          (when (not= new-val cur-val)
            (.setValue @*ace-ro* new-val -1))))
      (html [:div {:name "editorElement" :className "editor editor-ro"}]))
    om/IWillUnmount
    (will-unmount [_]
      (when @*ace-ro*
        (.destroy @*ace-ro*)
        (reset! *ace-ro* nil)))
    om/IDidMount
    (did-mount [_]
      (let [ace-instance (.edit js/ace (.getDOMNode owner))]
        (.setTheme ace-instance "ace/theme/twilight")
        (.setMode (.getSession ace-instance) "ace/mode/javascript")
        (set! (.-$blockScrolling ace-instance) Infinity)
        (.setValue ace-instance (data :output))
        (.setReadOnly ace-instance true)
        (reset! *ace-ro* ace-instance)))))

(defn- sample-li-input-output [idx io]
  (html
   [:div {:className "sample" :key idx}
    [:div {:className "input"}
     [:a {:onClick (onclick-handler :apply-sample (io :input))} (clj->json (io :input) 0)]]
    [:div {:className "output"} (clj->json (io :output) 0)]]))

(defn- sample-li [sample]
  (html
   [:li {:className "tag-samples" :key (sample :name)}
    [:div {:className "tag"} (sample :name)]
    (if (sample :desc) [:div {:className "desc" :dangerouslySetInnerHTML {:__html (sample :desc)}}])
    [:div {:className "samples"} (map-indexed sample-li-input-output (sample :samples))]]))

(defn editor-help [data owner]
  (reify
    om/IRender
    (render [_]
      (html
       [:div {:className "editor-help help"}
        [:h1 "Directives"]
        [:ul (map sample-li (data :samples))]
        [:h1 "Other Examples"]
        [:ul (map sample-li (data :mixed))]]))
    om/IShouldUpdate
    (should-update [this next-props next-state]
      (not= (om/get-props owner) next-props))))
