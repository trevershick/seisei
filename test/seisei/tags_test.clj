(ns seisei.tags-test
  (:use midje.sweet)
  (:require [clojure.data.json :as json]
            [seisei.engine :refer :all]
            [seisei.json :refer :all]
            [seisei.test-helpers :refer :all :as h]
            [seisei.generated.cities :as gencities]))


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
       (let [tag "{{company(3)}}"
             processed (process (h/jsonfixture { :id tag } )) ]
         (fact "it should return a value"
               (:id processed) =not=> tag
               (:id processed) => #".+"
               )))


(facts "about firstName()"
       (let [tag "{{firstName()}}"
             processed (process (h/jsonfixture { :id tag } )) ]
         (fact "it should return a value"
               (:id processed) =not=> tag
               (:id processed) => #".+"
               )))

(facts "about firstName('gender')"
       (let [tag "{{firstName('male')}}"
             processed (process (h/jsonfixture { :id tag } )) ]
         (fact "it should return a male value"
               (:id processed) =not=> tag
               (:id processed) => #".+"
               )))

(facts "about surname()"
       (let [tag "{{surname()}}"
             processed (process (h/jsonfixture { :id tag } )) ]
         (fact "it should return a value"
               (:id processed) =not=> tag
               (:id processed) => #".+"
               )))

(facts "about company()"
       (let [tag "{{company()}}"
             processed (process (h/jsonfixture { :id tag } )) ]
         (fact "it should return a value"
               (:id processed) =not=> tag
               (:id processed) => #".+"
               )))

(facts "about zip()"
       (let [tag "{{zip()}}"
             processed (process (h/jsonfixture { :id tag } )) ]
         (fact "it should return a value"
               (:id processed) =not=> tag
               (:id processed) => #".+"
               )))

(facts "about bool()"
       (let [tag "{{bool()}}"
             processed ( process (h/jsonfixture { :x tag }) ) ]
         (fact "it should return a random boolean value"
               (:x processed) =not=> tag
               (contains? #{ true false } (:x processed)) => true
               )))

(facts "about guid()"
       (let [tag "{{guid()}}"
             processed (process (h/jsonfixture { :id tag } )) ]
         (fact "it should return a UUID"
               (:id processed) =not=> tag
               (:id processed) => h/uuid-regex
               )))

(facts "about street()"
       (let [tag "{{street()}}"
             processed (process (h/jsonfixture { :id tag } )) ]
         (fact "it should return a value"
               (:id processed) =not=> tag
               (:id processed) => #".+"
               )))

(facts "about city()"
       (let [tag "{{city()}}"
             processed (process (h/jsonfixture { :city tag } )) ]
         (fact "it should return a value"
               (:city processed) =not=> tag
               (.indexOf gencities/cities(:city processed)) =not=> -1
               )))

(facts "about state()"
       (let [tag "{{state()}}"
             json { :state tag }
             processed (process (h/jsonfixture json ))
             state-abbrevs (map :abbrev seisei.generated.states/states)]
         (fact "it should return a state value"
               (:state processed) =not=> tag
               (.indexOf state-abbrevs (:state processed)) =not=> -1
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

