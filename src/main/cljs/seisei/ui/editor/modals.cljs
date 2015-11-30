(ns seisei.ui.editor.modals
  (:require [om.core :as om :include-macros true]
            [seisei.ui.dispatcher :as d]
            [seisei.ui.util :refer [nnil?]]
            [sablono.core :as html :refer-macros [html]]))

(defn- on-rename-save [owner data]
  (fn [e]
    (.stopPropagation e)
    (d/action :rename-template (data :value))
    (om/update! data :show false)))

(defn- on-rename-cancelled [owner data]
  (fn [e]
    (.stopPropagation e)
    (om/update! data :show false)))

(defn- on-key-press [owner data]
  (fn [e]
    (let [cc (aget e "charCode")
          kc (aget e "keyCode")]
      (case (+ kc cc)
        27 ((on-rename-cancelled owner data) e)
        13 ((on-rename-save owner data) e)
        true
      ))))

(defn- on-change [data]
  (fn [e]
    (om/update! data :value (aget (aget e "target") "value"))))

(defn rename-modal [data owner]
  (om/component
    (let [show           (data :show)
          modal-style   (if show { :display "block" })
          value         (data :value) ]
      (html
        [:div { :className "modal modal-backdrop" :style modal-style }
          [:div { :ref "dialog" :className "modal-dialog" }
            [:div { :className "modal-content" }
              [:div { :className "modal-header" }
                ; [:button  { :type "button" :className "close" :data-dismiss "modal" :aria-label "Close"}
                ;   [:span { :aria-hidden true } "x" ]
                ; ]
                [:h4 { :className "modal-title" } "Rename Template" ]
              ]
              [:div { :className "modal-body" }
                [:div { :className "form-group" }
                  [:label { :for "newName" } "Rename the template to : " ]
                  [:input { :type "text" :name "newName" :id "newName" :value value :className "form-control" :onKeyDown (on-key-press owner data) :onChange (on-change data) }]
                ]
              ]
              [:div { :className "modal-footer" }
                [:button { :className "btn btn-primary" :onClick (on-rename-save owner data) } "Save" ]
                [:button { :className "btn btn-default" :onClick (on-rename-cancelled owner data) } "Cancel" ]
              ]
            ]
          ]
        ]
      ))))

(defn- on-confirm-yes [data]
  (fn [e]
    (.stopPropagation e)
    (om/update! data :show false)
    (let [ action     (data :confirm-action)
           data       (data :confirm-data)
           perform    (nnil? action)]
      (if perform
        (d/action action data)))))

(defn- on-confirm-no [data]
  (fn [e]
    (.stopPropagation e)
    (om/update! data :show false)
    (let [ action     (data :deny-action)
           data       (data :deny-data)
           perform    (nnil? action)]
      (if perform
        (d/action action data)))))

(defn confirm-modal [data owner]
  (om/component
    (let [show                (data :show)
          modal-style         (if show { :display "block" })
          question            (data :question "Are you sure?")
          title               (data :title "Confirm")
          yes                 (data :yes "Yes")
          no                  (data :no "No")
          action-on-confirm   (data :dispatch-on-confirm)]
          (html
            [:div { :className "modal modal-backdrop" :style modal-style }
              [:div { :className "modal-dialog" }
                [:div { :className "modal-content" }
                  [:div { :className "modal-header" }
                    ; [:button  { :type "button" :className "close" :data-dismiss "modal" :aria-label "Close"}
                    ;   [:span { :aria-hidden true } "x" ]
                    ; ]
                    [:h4 { :className "modal-title" } title ]
                  ]
                  [:div { :className "modal-body" } question ]
                  [:div { :className "modal-footer" }
                    [:button { :className "btn btn-primary" :onClick (on-confirm-yes data) } yes ]
                    [:button { :className "btn btn-default" :onClick (on-confirm-no data) } no ]
                  ]
                ]
              ]
            ]
          ))))
