(ns seisei.tags-test
  (:use midje.sweet)
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest]]
            [seisei.engine :refer :all]
            [seisei.json :refer :all]
            [seisei.test-helpers :refer :all :as h]
            [seisei.generated.cities :as gencities]
            [seisei.generated.names]
            [seisei.generated.companies]
            [seisei.generated.states]
            [seisei.generated.surnames]
            [seisei.generated.streets]))

(deftest tags

(def repeat-factor 1)

(def streets
  (->> seisei.generated.streets/streets
       (map :full)))

(def male-names
  (->> seisei.generated.names/names
       (filter #(= "M" (:gender %)))
       (map :name)))

(def female-names
  (->> seisei.generated.names/names
       (filter #(= "F" (:gender %)))
       (map :name)))

(def all-names
  (->> seisei.generated.names/names
       (map :name)))


(def js1 (h/jsonfixture { :id "{{objectId()}}" } ))
(facts "about objectId()"
       (let [ processed (process js1) ]
         (fact "it should return a UUID"
               (:id processed) => truthy
               (:id processed) => h/uuid-regex
               )))


(def js3 (h/jsonfixture { :x "{{random('blue', 'brown', 'green')}}" }))
(facts "about random()"
       (let [ processed ( process js3 ) ]
         (fact "it should return a random value from a supplied list"
               (:x processed) => truthy
               (:x processed) => #(contains? #{"blue" "brown" "green"} %)
               )))

(def js4 (h/jsonfixture {:x ["{{repeat(5)}}" {:x 1} {:x 3} "{{repeat(7)}}" {:x 2} {:x 19} ]}))
(def js5 (h/jsonfixture {:x ["{{repeat(5)}}" {:id "{{objectId(3)}}"}]}))
(facts "about repeat()"
       (let [
             js4p ( process js4 )
             js5p ( process js5 ) ]
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



(facts "about complex tags"
       (let [tag "999 {{bool}}"
             json { :b tag }
             processed (process (h/jsonfixture json ))
             b (:b processed)]
         (fact "it should return a state value"
               b =not=> tag
               b => #"999 (true|false)"
               ))
       (let [tag "{{integer(100, 999)}} {{street()}}, {{city()}}, {{state()}}, {{integer(100, 10000)}}"
             json { :b tag }
             processed (process (h/jsonfixture json ))
             b (:b processed)]
         (fact "it should return a state value"
               b =not=> tag
               b => #"[0-9]{3} .+, .+, [A-Z][A-Z], [0-9]{3,5}"
               )))

(def all [
          "floating(1000, 4000, 2, '$0,0.00')"
          "phone([format])"
          "range([start], stop, [step])"
          "lorem(1, 'words|sentences|paragraphs')"
          "country(true)" (comment "abbreviated")
          "country()"
          "domainZone()"
          "floating([min], [max], [fixed], [format])"
          "gauss([mu], [sigma])"
          ])


(doall (repeatedly repeat-factor #(do
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
                 (.indexOf all-names (:id processed)) =not=> -1
                 )))



  (facts "about firstName('gender')"
         (let [tag "{{firstName('male')}}"
               processed (process (h/jsonfixture { :id tag } )) ]
           (fact "it should return a male value"
                 (:id processed) =not=> tag
                 (.indexOf male-names (:id processed)) =not=> -1
                 (.indexOf female-names (:id processed)) => -1
                 )))

  (facts "about surname()"
         (let [tag "{{surname()}}"
               processed (process (h/jsonfixture { :id tag } ))
               s (:id processed)]

           (fact "it should return a value"
                 s =not=> tag
                 (.indexOf seisei.generated.surnames/surnames s) =not=> -1
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

  (facts "about street"
         (let [tag "{{street()}}"
               processed (process (h/jsonfixture { :id tag } ))
               s (:id processed)]
           (fact "it should return a value"
                 (:id processed) =not=> tag
                 (.indexOf streets s) =not=> -1
                 )))

  (facts "about city()"
         (let [tag "{{city}}"
               processed (process (h/jsonfixture { :city tag } )) ]
           (fact "it should return a value"
                 (:city processed) =not=> tag
                 (.indexOf gencities/cities(:city processed)) =not=> -1
                 )))

  (facts "about state()"
         (let [tag "{{state}}"
               json { :state tag }
               processed (process (h/jsonfixture json ))
               state-abbrevs (map :abbrev seisei.generated.states/states)]
           (fact "it should return a state value"
                 (:state processed) =not=> tag
                 (.indexOf state-abbrevs (:state processed)) =not=> -1
                 )))

  (facts "about integer"
         (let [tag        "{{integer()}}"
               document   { :i tag }
               processed  (process (h/jsonfixture document))
               i          (:i processed)]
           (fact "it should return a positive random integer with no params"
                 i =not=> tag
                 (< i 0) => falsey
                 ))
         (let [tag        "{{integer(1,5)}}"
               document   { :i tag }
               processed  (process (h/jsonfixture document))
               i          (:i processed)]
           (fact "it should return a positive random integer with no params"
                 i =not=> tag
                 (< i 1) => falsey
                 (> i 5) => falsey
                 )))



  (facts "about date"
         (let [tag        "{{date()}}"
               document   { :i tag }
               processed  (process (h/jsonfixture document))
               i          (:i processed)]
           (fact "works with zero params"
                 i =not=> tag
                 i = #"[0-9]{8}"
                 ))
         (let [tag        "{{date('20150317','20150317','dd')}}"
               document   { :i tag }
               processed  (process (h/jsonfixture document))
               i          (:i processed)]
           (fact "works with zero params"
                 i =not=> tag
                 i => "17"
                 ))
         (let [tag        "{{date(20140101)}}"
               document   { :i tag }
               processed  (process (h/jsonfixture document))
               i          (Integer/parseInt (:i processed))]
           (fact "works with zero params"
                 i =not=> tag
                 (>= i 2014)
                 ))
         (let [tag        "{{date(20140101,20991231)}}"
               document   { :i tag }
               processed  (process (h/jsonfixture document))
               i          (Integer/parseInt (:i processed))]
           (fact "works with zero params"
                 i =not=> tag
                 (and (< i 20991231) (> i 2014010)) => true
                 ))
         (let [tag        "{{date(20140101,'today','yyyy')}}"
               document   { :i tag }
               processed  (process (h/jsonfixture document))
               i          (Integer/parseInt (:i processed))
               thisyear   (+ 1900 (.getYear (java.util.Date.)))]
           (fact "works with zero params"
                 i =not=> tag
                 i => (roughly thisyear (- thisyear 2014))
                 )))
)))
)
