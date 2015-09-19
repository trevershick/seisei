(ns seisei.web.template-handlers
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [compojure.core :refer [DELETE POST GET defroutes routes wrap-routes]]
            [seisei.web.user]
            [seisei.json]
            [seisei.engine]
            [seisei.web.db :as db]
            [seisei.web.s3 :as s3]
            [clojure.tools.logging :as log]))

(def not-nil? (comp not nil?))

(defn load-dynamic-template
  [slug r]
  (let [template    (db/find-dynamic-template slug)
        parsed-json (seisei.json/parse-with-error ( -> template :content ))
        processed   (seisei.engine/process (:output parsed-json))]
        {:body processed}))

(defn load-my-template
  [slug r]
  (let [session     (:session r)
        user-id     (seisei.web.user/user-id session)]
    (let [template    (db/user-template user-id slug)
          parsed-json (seisei.json/parse-with-error ( -> template :content ))
          processed   (seisei.engine/process (:output parsed-json))]
      {:body {:template template
              :processed processed
              :errors (:errors parsed-json)
              :input (:input parsed-json)}})))

(defn save-my-template
  [slug r]
  (let [session             (:session r)
        user-id             (seisei.web.user/user-id session)
        template-content    (-> r :body :template :content)
        template-title      (-> r :body :template :title)
        template            (db/update-user-template user-id slug template-content template-title)
        parsed-json         (seisei.json/parse-with-error ( -> template :content ))
        processed           (seisei.engine/process (:output parsed-json))]
    {:body {:template template
            :processed processed
            :errors (:errors parsed-json)
            :input (:input parsed-json)}}))

(defn delete-my-template
  [slug r]
  (let [session             (:session r)
        user-id             (seisei.web.user/user-id session)
        deleted             (db/delete-user-template user-id slug)
        _                   (s3/unpublish-from-s3 slug)]
    {:status 200
     :body { :messages [ (str "Template " slug " deleted.") ] }
    }))

(defn my-templates
  [{session :session}]
  (when-let [user-id   (seisei.web.user/user-id session)]
    {:body (or (db/user-templates user-id) []) }))

(defn run-template
  [request]
  (let [session (:session request)
        template (-> request :body :template :content)
        parsed-json (seisei.json/parse-with-error template)
        processed (seisei.engine/process (:output parsed-json))]
    {:body {:processed processed
            :errors (:errors parsed-json)
            :input (:input parsed-json)}}
    ))

(defn save-new-template
  [request]
  (let [session (:session request)
        template-id (-> request :body :template :id)
        template-content (-> request :body :template :content)
        user-id (seisei.web.user/user-id session)]
    ; at this point - insert only.
      (let [new-template (db/insert-template user-id template-content)]
        {:body {:messages [{:id "T0001" :text "Saved."}] :template new-template }})
      ))

(defn dynamic-url-for-slug
  [request slug]
  (str "http://seisei.elasticbeanstalk.com/templates/" slug))


(defn publish-dynamic
  [request]
  (let [session             (:session request)
        user-id             (seisei.web.user/user-id session)
        template            (-> request :body :template)
        template-content    (:content template)
        slug                (:slug template)
        exists              (not-nil? (db/find-dynamic-template slug))]
    ;; create the slug record
    ;; fill in the content
      (do
        (if exists
          (db/update-dynamic slug {:user user-id :content template-content})
          (db/insert-dynamic slug {:user user-id :content template-content}))
        (db/update-user-template-attrs user-id slug {:dynamic-url  (dynamic-url-for-slug request slug)})
        (load-my-template slug request))))

(defn publish-to-s3
  [request]
  (let [session             (:session request)
        user-id             (seisei.web.user/user-id session)
        template            (-> request :body :template)
        content-to-publish  (:processed template)
        slug                (:slug template)
        url                 (s3/publish-to-s3 slug content-to-publish)]
      (do
        (db/update-user-template-attrs user-id slug {:static-url url})
        (load-my-template slug request)
      )))

(defn unpublish-from-s3
  [slug request]
  (let [session             (:session request)
        user-id             (seisei.web.user/user-id session)
        template            (db/user-template user-id slug)]
    (if (and (not-nil? user-id) (not-nil? template))
      (do
        (s3/unpublish-from-s3 slug)
        (db/update-user-template-attrs user-id slug {:static-url nil})
        (load-my-template slug request)
      )
      {:status 403})
    ))

(defn unpublish-dynamic
  [slug request]
  (let [session             (:session request)
        user-id             (seisei.web.user/user-id session)
        template            (db/user-template user-id slug)]
    (if (not-nil? template)
      (do
        (db/delete-dynamic slug)
        (db/update-user-template-attrs user-id slug {:dynamic-url nil})
        (load-my-template slug request)
      )
      {:status 404})
    ))


(defn requires-user-middleware [handler]
  (fn [request]
    (let [session (:session request)
          logged-in (seisei.web.user/logged-in? session)]
      (if logged-in
        (handler request)
        {:status 403})
      )))

(defroutes unprotected-routes
  (POST "/template/process" r (run-template r))
  (GET "/templates/:slug" [slug :as r] (load-dynamic-template slug r)))

(defroutes protected-routes

  (POST "/my/templates" r (save-new-template r))
  (POST "/my/templates/:slug/publishdynamic" r (publish-dynamic r))
  (POST "/my/templates/:slug/publish" r (publish-to-s3 r))
  (DELETE "/my/templates/:slug/publishdynamic" [slug :as r] (unpublish-dynamic slug r))
  (DELETE "/my/templates/:slug/publish" [slug :as r] (unpublish-from-s3 slug r))
  (DELETE "/my/templates/:slug" [slug :as r] (delete-my-template slug r))
  (POST "/my/templates/:slug" [slug :as r] (save-my-template slug r))
  (GET "/my/templates/:slug" [slug :as r] (load-my-template slug r))
  (GET "/my/templates" r (my-templates r)))

(defroutes template-routes
  (routes
    (wrap-routes protected-routes requires-user-middleware )
     unprotected-routes  ))
