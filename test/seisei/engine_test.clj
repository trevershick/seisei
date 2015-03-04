(ns seisei.engine-test
	(:use midje.sweet)
	(:require [clojure.data.json :as json]
			[seisei.engine :refer :all]
			[seisei.json :refer :all]
			[seisei.test-helpers :refer :all :as h]))


(facts "about `parse-operation`"
	(fact "it can handle parameters"
		(let [x (parse-operation "random(3)" "thearg" 3)]
			(:params x) => '(3)
			(:name x) => "random"
			(:arg x) => "thearg"
			(:i x) => 3
		))
	(fact "it can handle no params"
		(let [x (parse-operation "random()" "thearg" 3)]
			(:params x) => '()
			(:name x) => "random"
			(:arg x) => "thearg"
			(:i x) => 3
		)))

(def js2 (h/jsonfixture { :x { :y 1 :z "{{objectId(3)}}"} }))
(facts "about tag processing"
	(let [ processed ( process js2 ) ]
		(fact "it should work even when nested"
			( -> processed :x :z ) => truthy
			( -> processed :x :z ) => h/uuid-regex
		)))



