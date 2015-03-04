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
	(json/read-str (fix-single-ticks str))
)


(defstruct operation :name :params :arg)
(defn ^:private parse-operation-params [paramsString]
	(map jsonify (clojure.string/split paramsString #","))
)
(defn parse-operation [clause & [arg]]
	(let [matcher (re-matcher #"([a-zA-Z]+)\(([^\)]+)\)" clause)]
		(if (re-find matcher)
			(struct-map operation
			 :name (get (re-groups matcher) 1)
			 :params (parse-operation-params (get (re-groups matcher) 2))
       :arg arg
			)
			nil
		)
	)
)


;; these have to be at the top : (
(defmulti execute :name)
(defmulti process (fn [clazz & other] (class clazz)))



(defmethod execute "objectId" [operation]
	(str (java.util.UUID/randomUUID))
)
(defmethod execute "repeat" [operation]
  (let [ n (first (:params operation)) 
    a (:arg operation) ]
    (if-not (nil? a) 
      (repeat n (process a))
      "You must follow a repeat statement with an element"
    )
  )
)
(defmethod execute "random" [operation]
	( let [
			params (:params operation)
			paramsz (count (:params operation)) ]
		(if (= 0 (count params)) "random value here" (rand-nth params))
	)
)

(defmethod execute :default [operation]
	(str "uknown method" operation)
)




(defmethod process String [s & {:keys [arg] :or [arg nil]}]
	(if (is-tag s) (execute (parse-operation (tag-contents s) arg)) s)
)
(defmethod process :default [s & {:keys [arg] :or [arg nil]}] s)
(defmethod process clojure.lang.PersistentArrayMap [m & {:keys [arg] :or [arg nil]}]
	(into {} (for [[k v] m] [k (process v)]))
)
(defmethod process clojure.lang.PersistentVector [v & {:keys [arg] :or [arg nil]}]
  (comment "iterate over items, and process each, pasing the tail to 'process'")  
  (let [ length (count v) head (get v 0) tail (nthrest v 2)]
    (cond
      (= 0 length) []
      (= 1 length) [(process head)]
      (and 
        (is-tag head) 
        (= "repeat" (:name (parse-operation head)))
      ) (concat (process head :arg (get v 1)) (process (nthrest v 2)))
      :else (map process v)
    )
  )
)


(defn -main
	"I don't do a whole lot ... yet."
	[& args]
	("x"))


