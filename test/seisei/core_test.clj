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
		"{ \"x\": \"{{random('blue','brown','green')}}\" }"
		:key-fn keyword
	)
)

(deftest random-test
	(testing "random() should return a random value"
		(let [ processed ( process js3 ) ]
			(is (contains? #{"blue" "brown" "green"} (:x processed)))
		)
	)
)

(def ^:private js4
	( json/read-str
		"{ \"x\": [\"{{repeat(5)}}\", { \"x\": 1 }, {\"x\":3}, \"{{repeat(7)}}\", { \"x\": 2 }, {\"x\":19}] }"
		:key-fn keyword
	)
)
(deftest repeat-test
	(testing "repeat should repeat the following item"
		(let [ processed ( process js4 ) ]
			(is (= 14 (count (:x processed))))
			(is (= {:x 1} (first (:x processed))))
			(is (= {:x 19} (last (:x processed))))
		)
	)
)
(def ^:private js5
	( json/read-str
		"{ \"x\": [\"{{repeat(5)}}\", { \"id\": \"{{objectId(3)}}\" }] }"
		:key-fn keyword
	)
)
(deftest repeat-test-with-nested
	(testing "repeat should process repeated items with tags properly"
		(let [ processed ( process js5 ) ]
			(is (= 5 (count (:x processed))))
			(is (not-nil? (re-matches uuid-regex (:id (first (:x processed))))))
			(is
				(not= (-> processed :x first :id)
					(-> processed :x last :id))
			)
		)
	)
)

(def ^:private js6
	( json/read-str
		"{ \"x\": [\"{{repeat(3)}}\", { \"idx\": \"{{index(3)}}\" }] }"
		:key-fn keyword
	)
)
(deftest repeat-test-with-index
	(testing "repeat should process and index should be updated"
		(let [ processed ( process js6 ) ]
			(println "processed->" processed)
			(is (= 3 (count (:x processed))))
			(is (= 0 (:idx (nth (:x processed) 0))))
			(is (= 1 (:idx (nth (:x processed) 1))))
			(is (= 2 (:idx (nth (:x processed) 2))))
			
		)
	)
)
