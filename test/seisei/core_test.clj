(ns seisei.core-test
  (:require [clojure.test :refer :all]
			[clojure.data.json :as json]
            [seisei.core :refer :all]))

(def uuid-regex #"[a-f0-9]+-[a-f0-9]+-[a-f0-9]+-[a-f0-9]+-[a-f0-9]+")
(def not-nil? (complement nil?))


(def ^:private js1
	( json/read-str 
		( json/write-str { :id "{{objectId(3)}}" } ) 
		:key-fn keyword 
	)
)

(deftest uuid-test
	(testing "objectId should return a uuid"
		(let [ processed (process js1) ]
			(is (contains? processed :id))
			(is (not-nil? (re-matches uuid-regex (:id processed))))
		)
	)
)

(def ^:private js2 
	( json/read-str 
		( json/write-str { :x { :y 1 :z "{{objectId(3)}}"} } ) 
		:key-fn keyword 
	)
)

(deftest nested-uuid-test
	(testing "objectId should return a uuid even when nested"
		
		(let [ processed ( process js2 ) ]
			(is (not-nil? ( -> processed :x :z )))
			(is (not-nil? (re-matches uuid-regex ( -> processed :x :z ))))
		)
	)
)

(def ^:private js3
	( json/read-str 
		"{ \"x\": \"{{random('blue', 'brown', 'green')}}\" }"
		:key-fn keyword 
	)
)

(deftest random-test
	(testing "random() should return a random value"
		
		(let [ processed ( process js3 ) ]
			(println "processed -> " processed)
			(is (contains? #{"blue" "brown" "green"} (:x processed)))
		)
	)
)


