(ns seisei.tags-test
	(:use midje.sweet)
	(:require [clojure.data.json :as json]
			[seisei.engine :refer :all]
			[seisei.json :refer :all]
			[seisei.test-helpers :refer :all :as h]))


(def js1 (h/jsonfixture { :id "{{objectId()}}" } ))
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




(facts "about company()"
	(let [ processed (process (h/jsonfixture { :id "{{company(3)}}" } )) ]
		(fact "it should return a value"
			(:id processed) => truthy
			(:id processed) => #".+"
		)))


(facts "about firstName()"
	(let [ processed (process (h/jsonfixture { :id "{{firstName()}}" } )) ]
		(fact "it should return a value"
			(:id processed) => truthy
			(:id processed) => #".+"
		)))

(facts "about firstName('gender')"
	(let [ processed (process (h/jsonfixture { :id "{{firstName('male')}}" } )) ]
		(fact "it should return a male value"
			(:id processed) => truthy
			(:id processed) => #".+"
		)))

(facts "about surname()"
	(let [ processed (process (h/jsonfixture { :id "{{surname()}}" } )) ]
		(fact "it should return a value"
			(:id processed) => truthy
			(:id processed) => #".+"
		)))

(facts "about company()"
	(let [ processed (process (h/jsonfixture { :id "{{company()}}" } )) ]
		(fact "it should return a value"
			(:id processed) => truthy
			(:id processed) => #".+"
		)))

(facts "about bool()"
	(let [ processed ( process (h/jsonfixture { :x "{{bool()}}" }) ) ]
		(fact "it should return a random boolean value"
			(:x processed) => truthy
			(:x processed) => #(contains? #{ "true" "false" } %)
		)))

(facts "about guid()"
	(let [ processed (process (h/jsonfixture { :id "{{guid()}}" } )) ]
		(fact "it should return a UUID"
			(:id processed) => truthy
			(:id processed) => h/uuid-regex
		)))

(facts "about street()"
	(let [ processed (process (h/jsonfixture { :id "{{street()}}" } )) ]
		(fact "it should return a value"
			(:id processed) => truthy
			(:id processed) => #".+"
		)))

(facts "about city()"
	(let [ processed (process (h/jsonfixture { :id "{{city()}}" } )) ]
		(fact "it should return a value"
			(:id processed) => truthy
			(:id processed) => #".+"
		)))

(facts "about state()"
	(let [ processed (process (h/jsonfixture { :id "{{state()}}" } )) ]
		(fact "it should return a state value"
			(:id processed) => truthy
			(:id processed) => #".+"
		)))

(facts "about state(true)"
	(let [ processed (process (h/jsonfixture { :id "{{state(true)}}" } )) ]
		(fact "it should return an abbreviated state value"
			(:id processed) => truthy
			(:id processed) => #".+"
		)))


(def all [
	"floating(1000, 4000, 2, '$0,0.00')"
	"integer(20, 40)"
	"phone([format])"
	"range([start], stop, [step])"
	"date(?)"
	"lorem(1, 'words|sentences|paragraphs')"
	"{{integer(100, 999)}} {{street()}}, {{city()}}, {{state()}}, {{integer(100, 10000)}}"
	"country(true)" (comment "abbreviated")
	"country()"
	"date([min], [max])"
	"domainZone()"
	"floating([min], [max], [fixed], [format])"
	"gauss([mu], [sigma])"
])

