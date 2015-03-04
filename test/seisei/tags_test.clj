(ns seisei.tags-test
	(:use midje.sweet)
	(:require [clojure.data.json :as json]
			[seisei.engine :refer :all]
			[seisei.json :refer :all]
			[seisei.test-helpers :refer :all :as h]))


(def js1 (h/jsonfixture { :id "{{objectId(3)}}" } ))
(facts "about objectId()"
	(let [ processed (process js1) ]
		(fact "it should return a UUID"
			(:id processed) => truthy
			(:id processed) => h/uuid-regex
		)))

(def js3 (h/jsonfixture { :x "{{random('blue','brown','green')}}" }))
(facts "about random()"
	(let [ processed ( process js3 ) ]
		(fact "it should return a random value from a supplied list"
			(:x processed) => truthy
			(:x processed) => #(contains? #{"blue" "brown" "green"} %)
		)))

(def js4 (h/jsonfixture {:x [ "{{repeat(5)}}" {:x 1} {:x 3} "{{repeat(7)}}" {:x 2} {:x 19} ]}))
(def js5 (h/jsonfixture {:x ["{{repeat(5)}}" {:id "{{objectId(3)}}"}]}))
(facts "about repeat()"
	(let [ js4p ( process js4 ) js5p ( process js5 ) ]
		(fact "it should repeat the following item"
			(count (:x js4p)) => 14
			(first (:x js4p)) => {:x 1}
			(last (:x js4p)) => {:x 19}
		)
		(fact "it should process tags in repeated items"
			(count (:x js5p)) => 5
			(:id (first (:x js5p))) => h/uuid-regex
			(-> js5p :x first :id) =not=> (-> js5p :x last :id)
		)))


(def js6 (h/jsonfixture {:x ["{{repeat(3)}}" {:idx "{{index(3)}}"}]}))
(facts "about index()"
	(let [ processed ( process js6 ) ]
		(fact "it should process and index should be updated"
			(count (:x processed)) => 3
			(:idx (nth (:x processed) 0)) => 0
			(:idx (nth (:x processed) 1)) => 1
			(:idx (nth (:x processed) 2)) => 2
		)))
