(ns seisei.web.session
  (:require [taoensso.faraday :as far]
            [ring.middleware.session.store :refer [SessionStore]]
            [clojure.tools.logging :as log]
            [seisei.web.db :as db]))


(def client-opts db/aws-dynamodb-client-opts)

(defn generate-new-random-key [] (str (java.util.UUID/randomUUID)))

(defn read-data [key]
  (if key 
    (:data (far/get-item client-opts db/table-sessions { :id key }))
    {}))

(defn save-data [key data]
  (log/debug "save-data " key data)
  (far/put-item client-opts db/table-sessions { :id key :data (far/freeze data) })
  )

(defn delete-data [key]
  (far/delete-item client-opts db/table-sessions { :id key })
  )

(deftype DynamoDbStore []
  SessionStore
  (read-session [_ key]
                (read-data key))
  (write-session [_ key data]
                 (let [key (or key (generate-new-random-key))]
                   (save-data key data)
                   key))
  (delete-session [_ key]
                  (delete-data key)
                  nil))

