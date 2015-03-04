(ns seisei.engine
	(:require [seisei.json :as json])
	(:gen-class)
)


(def ^:private tag-regex #"\{\{(.+)\}\}")

(defn tag-contents
	"Given a string in tag format {{x}} will return 'x' as a String"
	[tag]
	(if-let [ [match contents] (re-find tag-regex tag)]
		contents
		nil))

(defn is-tag
	"Returns true/false if the given clause is a tag, i.e. '{{a}}' format"
	[clause]
	(if (re-matches tag-regex clause) true false)
)



(defstruct operation :name :params :arg)
(defn ^:private parse-operation-params [paramsString]
	(cond
		(clojure.string/blank? paramsString) '()
		:else (map json/jsonify (clojure.string/split paramsString #","))
	)
)
(defn parse-operation [clause & [arg i]]
	(let [matcher (re-matcher #"([a-zA-Z]+)\(([^\)]*)\)" clause)]
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

