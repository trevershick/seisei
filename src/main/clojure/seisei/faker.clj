(ns seisei.faker
  (require [clojure.core.reducers :as reducers]))


(defn ^{:private true} faker
  "Create a new faker instance"
  ([]  (com.github.javafaker.Faker.))
  ([locale-name] (com.github.javafaker.Faker. (java.util.Locale. locale-name))))


(defn ^{:private true} invoke-by-name
  [obj n]
  (let [clazz       (.getClass obj)
        method      (.getMethod clazz n (make-array Class 0))]
    (.invoke method obj (make-array Object 0))))

;; a 'fold'ing function.  the initial call will return
;; an instance of Faker. Subsequent calls will be invoked
;; with 'Faker' and a string like 'cat' which would return
;; the result of calling 'faker.cat()'
(defn ^{:private true} thread
  ([xs x] (invoke-by-name xs x))
  ([] (faker)))


(defn ^{:private true} coll-from-expression [expression]
  "Splits expression by a 'dot' and returns the split list excluding 'faker'"
  (let [vals             (clojure.string/split expression #"\.")
        filtered-vals    (filter #(not (= "faker" %)) vals)]
      filtered-vals))

(defn ^{:private true} is-returnable
  [obj]
  (let [clazz (.getClass obj)]
    (cond
      (= java.lang.String clazz) true
      (= java.lang.Boolean clazz) true
      (= java.lang.Float clazz) true
      (= java.lang.Double clazz) true
      (= java.lang.Integer clazz) true
      (= java.lang.Long clazz) true
      :else false)))

(defn faker-expr
  [expression]
  (try
    (let [chain     (coll-from-expression expression)
          result    (reducers/fold thread chain)
          result    (if (is-returnable result) result (str "bad result" result))]
      result
      )
  (catch Exception e (str "ERROR" " " expression))))
