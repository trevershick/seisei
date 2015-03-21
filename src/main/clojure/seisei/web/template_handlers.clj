(ns seisei.web.template-handlers
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [compojure.core :refer [DELETE POST GET defroutes routes]]
            [seisei.web.user]
            [seisei.json]
            [seisei.engine]
            [seisei.web.db :as db]
            [seisei.web.s3 :as s3]
            [clojure.tools.logging :as log]))

(defn load-my-template
  [slug r]
  (let [session     (:session r)
        logged-in   (seisei.web.user/logged-in? session)
        user        (:user session)
        user-id     (:id user)
        logged-in   (not (nil? user))]
    (if logged-in
      (let [template    (db/user-template user-id slug)
            parsed-json (seisei.json/parse-with-error ( -> template :content ))
            processed   (seisei.engine/process (:output parsed-json))]
        {:body {:template template
                :processed processed 
                :errors (:errors parsed-json) 
                :input (:input parsed-json)}})
      {:status 403}
      )))

(defn save-my-template
  [slug r]
  (let [session             (:session r)
        logged-in           (seisei.web.user/logged-in? session)
        user                (:user session)
        user-id             (:id user)
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
  (let [user-id             (-> r :session :user :id)
        deleted             (db/delete-user-template user-id slug)]
    {:status 200
     :body { :messages [ (str "Template " slug " deleted.") ] }
    }))
    

(defn my-templates
  [{session :session}]
  (let [logged-in (seisei.web.user/logged-in? session)
        user-id   (seisei.web.user/user-id session)]
    (if logged-in 
          {:body (or (db/user-templates user-id) []) }
          {:status 403})
        ))

(defn run-template 
  [request]
  (let [session (:session request)
        logged-in (seisei.web.user/logged-in? session)
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
        logged-in (seisei.web.user/logged-in? session)
        template-id (-> request :body :template :id)
        template-content (-> request :body :template :content)
        user (:user session)
        user-id (:id user)]
    ; at this point - insert only.
    (log/infof "Session %s" (str session))
    (log/infof "User In Session %s" (str user))
    (let [new-template (db/insert-template user-id template-content)]
      {:body {:messages [{:id "T0001" :text "Saved."}] :template new-template }})      
    ))

(defn publish-to-s3
  [request]
  (let [session             (:session request)
        logged-in           (seisei.web.user/logged-in? session)
        user-id             (seisei.web.user/user-id session)
        template            (-> request :body :template)
        content-to-publish  (:processed template)
        slug                (:slug template)
        url                 (s3/publish-to-s3 slug content-to-publish)]
    (if logged-in 
      (do 
        (db/update-user-template-attrs user-id slug {:static-url url})
        (load-my-template slug request)
      )
      {:status 403})
    ))


(defroutes template-routes
  (POST "/my/templates" r (save-new-template r))
  (POST "/my/templates/:slug/publish" r (publish-to-s3 r))
  (DELETE "/my/templates/:slug" [slug :as r] (delete-my-template slug r))
  (POST "/my/templates/:slug" [slug :as r] (save-my-template slug r))
  (GET "/my/templates/:slug" [slug :as r] (load-my-template slug r))
  (POST "/template/process" r (run-template r))
  (GET "/my/templates" r (my-templates r)))