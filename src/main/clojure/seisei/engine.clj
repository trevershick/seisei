(ns seisei.engine
  (:require [seisei.json :as json]
            [clj-time.format :as f]
            [seisei.generated.states]
            [seisei.generated.cities]
            [seisei.generated.companies]
            [seisei.generated.zips]
            [seisei.generated.names]
            [seisei.generated.surnames]
            [seisei.generated.streets])
  (:gen-class)
  )


(def tag-regex #"\{\{([^\}]+)\}\}")

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
    :else (map json/jsonify (clojure.string/split paramsString #","))))

(defn parse-operation [clause & [arg i]]
  (let [matcher (re-matcher #"([a-zA-Z]+)\(?([^\)]*)\)?" clause)]
    (if (re-find matcher)
      (struct-map operation
        :name (get (re-groups matcher) 1)
        :params (parse-operation-params (get (re-groups matcher) 2))
        :tag (str "{{" clause "}}")
        :text clause
        :arg arg
        :i i
        )
      nil
      )
    )
  )


;; these have to be at the top : (
(defmulti process (fn [clazz & other] (class clazz)))
(defmulti execute :name)




(defmethod execute "objectId" [operation]
  (str (java.util.UUID/randomUUID))
  )

(defmethod execute "bool" [operation]
  (rand-nth [true false])
  )

(defmethod execute "guid" [operation]
  (str (java.util.UUID/randomUUID))
  )

(defmethod execute "index" [operation]
  (:i operation)
  )

(defmethod execute "firstName" [op]
  (let [params (:params op)
        paramsz (count params)
        all (->> seisei.generated.names/names (map :name))
        m (->> seisei.generated.names/names (filter #(= "M" (:gender %))) (map :name))
        f (->> seisei.generated.names/names (filter #(= "F" (:gender %))) (map :name))
        byarg {"female" f "male" m}]
    (cond
      (= paramsz 0) (rand-nth all)
      (> paramsz 0) (rand-nth (get byarg (first params))))))


(defmethod execute "street" [_]
  (:full (rand-nth seisei.generated.streets/streets)))

(defmethod execute "surname" [_]
  (rand-nth seisei.generated.surnames/surnames))

(defmethod execute "company" [_]
  (rand-nth seisei.generated.companies/companies))

(defmethod execute "city" [_]
  (rand-nth seisei.generated.cities/cities))

(defmethod execute "zip" [_]
  (rand-nth seisei.generated.zips/zips))

(defmethod execute "state" [operation]
  ( ->> seisei.generated.states/states
   rand-nth
   :abbrev))

(defmethod execute "repeat" [operation]
  (let [n (first (:params operation))
        a (:arg operation) ]
    (if-not (nil? a)
      (let [ans (for [i (range 0 n)] (process a :i i))]
        (println "a->" a)
        (println "operation->" operation)
        (println "ans->" ans)
        (println "a.class->" (.getClass a))
        (println "Processed->" (process a))
        ans
      )
      "You must follow a repeat statement with an element"
      )
    )
  )

(defn yesterday
  []
  (java.util.Date. (- (.getTime (java.util.Date.)) 86400000)))

(defn now
  []
  (java.util.Date.))

(def default-format "yyyyMMdd")
(def default-date-formatter (f/formatter default-format))
(defn start-of-time
  []
  (.toDate (f/parse default-date-formatter "19700101")))

(defn date-fmt [in default-format]
  (if (nil? in) default-format in))

(defn date-val [in-str default-val]
  (cond
    (nil? in-str) default-val
    (= "today" (str in-str)) (now)
    (= "yesterday" (str in-str)) (yesterday)
    :else (.toDate (f/parse default-date-formatter (str in-str)))
    ))
(defn local-date [from-date]
  (org.joda.time.DateTime. from-date))

(defn rand-date
  [from to]
  (let [fms     (.getTime from)
        tms     (.getTime to)
        maxms   (Math/max fms tms)
        minms   (Math/min fms tms)
        rangems (- maxms minms)
        randms  (.longValue (* (rand) rangems))]
    (java.util.Date. (+ minms randms))
    ))

(defmethod execute "date"
  [op]
  (let [ps        (:params op)
        psz       (count (:params op))
        pfrom     (nth ps 0 nil)
        ptil      (nth ps 1 nil)
        pfmt      (nth ps 2 nil)
        fmt       (if (nil? pfmt) default-format pfmt)
        from      (if (nil? pfrom) (start-of-time) (date-val pfrom (now)))
        til       (if (nil? ptil) (now) (date-val ptil (now)))
        formatter (f/formatter fmt)]
    (f/unparse formatter (local-date (rand-date from til)))
    ))

(defmethod execute "integer"
  [op]
  (let [ps (:params op)
        psz (count (:params op))
        start (first ps)
        end (first (rest ps))]
    (cond
      (and (= 2 psz) (every? #(instance? Number %) ps))
      (let [n (- end start)] (+ start (rand-int n)))
      :else (rand-int Integer/MAX_VALUE)
      )))

(defmethod execute "random" [operation]
  "Usage {{random(a,b,c)}}
  Returns a random element from the provided list"
  ( let [params (:params operation)
         paramsz (count (:params operation)) ]
   (if (= 0 (count params)) "random value here" (rand-nth params))
   )
  )

(defmethod execute :default [operation]
  (str (:tag operation)))




(defmethod process String [s & {:keys [arg i]}]
  (let [tags (->> (re-seq tag-regex s) (map #(get % 0)))
        texts (clojure.string/split s tag-regex)
        processed-tags (map #(execute ( parse-operation (tag-contents %) arg i)) tags)
        zipped (clojure.string/join (interleave texts processed-tags))]
    (cond (= 0 (count tags)) s
          (and (= 0 (count texts)) (= 1 (count tags))) (first processed-tags)
          :else zipped)))



(defmethod process Long [s & {:keys [arg i]}] s)

(comment (defmethod process :default [s & {:keys [arg i]}] s))

(defmethod process clojure.lang.PersistentHashMap [m & {:keys [arg i]}]
  (into {} (for [[k v] m] [k (process v :arg arg :i i)])))

(defmethod process clojure.lang.PersistentArrayMap [m & {:keys [arg i]}]
  (into {} (for [[k v] m] [k (process v :arg arg :i i)])))

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
        (= "repeat" (:name (parse-operation head))))
      (do
        (concat (process head :arg arg :i i) (process tail2 :arg arg :i i))
      ) 
      :else (cons head (process tail :arg arg :i i))
      )
    )
  )

