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
(defn parse-operation [clause & [arg i]]
	(let [matcher (re-matcher #"([a-zA-Z]+)\(([^\)]+)\)" clause)]
		(if (re-find matcher)
			(struct-map operation
				:name (get (re-groups matcher) 1)
				:params (parse-operation-params (get (re-groups matcher) 2))
				:arg arg
				:i i
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

(defmethod execute "index" [operation] 
	(:i operation)
)

(defmethod execute "repeat" [operation]
	(let [ n (first (:params operation)) 
		a (:arg operation) ]
		(if-not (nil? a)
			(for [i (range 0 n)] 
				(process a :i i))
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




(defmethod process String [s & {:keys [arg i]}]
	(if (is-tag s) (
		execute ( parse-operation (tag-contents s) arg i)) 
		s
	)
)
(defmethod process Long [s & {:keys [arg i]}] s)
(comment (defmethod process :default [s & {:keys [arg i]}] s))
(defmethod process clojure.lang.PersistentArrayMap [m & {:keys [arg i]}]
	(into {} (for [[k v] m] [k (process v :arg arg :i i)]))
)
(defmethod process clojure.lang.PersistentVector [v & {:keys [arg i]}]
	(comment "iterate over items, and process each, pasing the tail to 'process'")  
	(let [ length (count v) 
			head (get v 0) 
			arg (get v 1)
			tail2 (vec (nthrest v 2))
			tail (vec (rest v)) ]
		(cond
			(= 0 length) []
			(= 1 length) [(process head :arg arg :i i)]
			(and 
				(instance? String head)
				(is-tag head) 
				(= "repeat" (:name (parse-operation head)))
			) (concat (process head :arg arg :i i) (process tail2 :arg arg :i i))
			:else (cons head (process tail :arg arg :i i))
		)
	)
)


(defn -main
	"I don't do a whole lot ... yet."
	[& args]
	("x"))


