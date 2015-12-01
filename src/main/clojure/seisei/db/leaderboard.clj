(ns seisei.db.leaderboard
  (:require [seisei.db.core :as db]
            [taoensso.faraday :as far]
            [seisei.web.user :as user]
            [clojure.tools.logging :as log]))


(defn get-leader-board
  [type] ;; :static or :dynamic
  (far/query
    db/aws-dynamodb-client-opts
    db/table-scores
    { :type [ :eq (name type) ] }
    { :index  (name db/index-scores-type-score)
      :order  :desc
      :limit  10 }))

(defn full-leader-board []
  (let [static      (get-leader-board :static)
        dynamic     (get-leader-board :dynamic)
        user-ids    (->> static (concat dynamic) (map :user) set vec)
        users       (user/get-users user-ids)
        users       (map user/public-user-attributes users)]
    { :static static :dynamic dynamic :users users }))
