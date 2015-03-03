(ns seisei.core
  (:require [clojure.data.json :as json])
  (:gen-class)
)




(defn fix-single-ticks [jsonString]
  (clojure.string/replace jsonString #"'" "\""))


(defn parse [jsonString]
  (let [fixed-string (fix-single-ticks jsonString)]
    (json/read-str fixed-string :key-fn keyword)
  )
)


(defn tag-contents [tag]
  (let [matcher (re-matcher #"\{\{(.+)\}\}" tag)]
    (if (re-find matcher)
      (get (re-groups matcher) 1) nil
    )
  )
)

(defn is-tag [clause]
  (if (re-matches #"\{\{(.+)\}\}" clause) true false)
)


(defn ^:private jsonify [str]
  json/read-str (fix-single-ticks str)
)


(defstruct operation :name :args)
(defn parse-operation [clause]
  (let [matcher (re-matcher #"([a-zA-Z]+)\(([^\)]+)\)" clause)]
    (if (re-find matcher)
      (struct-map operation
       :name (get (re-groups matcher) 1)
       :args (map jsonify (clojure.string/split (get (re-groups matcher) 2) #",") :key-fn keyword)
      )
      nil
    )
  )
)


(defmulti execute :name)
(defmethod execute "objectId" [operation]
  (str (java.util.UUID/randomUUID))
)
(defmethod execute :default [operation]
  (str "uknown method" operation)
)




(defmulti process class)
(defmethod process String [s]
  (if (is-tag s) (execute (parse-operation (tag-contents s))) s)
)
(defmethod process :default [s] s)
(defmethod process clojure.lang.PersistentArrayMap [m]
  (into {} (for [[k v] m] [k (process v)]))
)


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ("x"))







