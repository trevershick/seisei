(ns seisei.tags-test
  (:use midje.sweet)
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest]]
            [seisei.engine :refer [process]]
            [seisei.engine-test :refer [with-processed-json *processed*]]
            [seisei.json :refer :all]
            [seisei.test-helpers :refer :all :as h]
            [seisei.generated.names]))

(defn ^:private parse-int [s]
  (Integer. (re-find  #"\d+" s)))
(def ^:private repeat-factor 1)
(defonce ^:private thisyear (+ 1900 (.getYear (java.util.Date.))))

(defonce ^:private all ["floating(1000, 4000, 2, '$0,0.00')"
                        "phone([format])"
                        "range([start], stop, [step])"
                        "lorem(1, 'words|sentences|paragraphs')"
                        "country(true)" (comment "abbreviated")
                        "country()"
                        "domainZone()"
                        "floating([min], [max], [fixed], [format])"
                        "gauss([mu], [sigma])"])

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

(defmacro with-processed-tag [arg & body]
  `(let [t#        ~arg
         j#        {:b t#}
         p#        (process (h/jsonfixture j#))]
     (binding [*processed* (p# :b)]
       (do ~@body))))

(defonce ^:private js1 (h/jsonfixture {:id "{{objectId()}}"}))
(defonce ^:private js3 (h/jsonfixture {:x "{{random('blue', 'brown', 'green')}}"}))
(defonce ^:private js4 (h/jsonfixture {:x ["{{repeat(5)}}" {:x 1} {:x 3} "{{repeat(7)}}" {:x 2} {:x 19}]}))
(defonce ^:private js5 (h/jsonfixture {:x ["{{repeat(5)}}" {:id "{{objectId(3)}}"}]}))
(defonce ^:private js6 (h/jsonfixture {:x ["{{repeat(3)}}" {:idx "{{index(3)}}"}]}))

(deftest tags

  (facts "about objectId()"
         (with-processed-json js1
                              (fact "it should return a UUID"
                                    (:id *processed*) => truthy
                                    (:id *processed*) => h/uuid-regex)))

  (facts "about random()"
         (with-processed-json js3
                              (fact "it should return a random value from a supplied list"
                                    (*processed* :x) => truthy
                                    (*processed* :x) => #(contains? #{"blue" "brown" "green"} %))))

  (facts "about repeat()"
         (with-processed-json js4
                              (fact "it should repeat the following item"
                                    (count (*processed* :x))  => 14
                                    (first (*processed* :x))  => {:x 1}
                                    (last (*processed* :x))   => {:x 19}))
         (with-processed-json js5
                              (fact "it should process tags in repeated items"
                                    (count (*processed* :x))          =>      5
                                    (:id (first (*processed* :x)))    =>      h/uuid-regex
                                    (-> *processed* :x first :id)     =not=>  (-> *processed* :x last :id)))) (facts "about index()"
                                                                                                                     (with-processed-json js6
                                                                                                                                          (fact "it should process and index should be updated"
                                                                                                                                                (count (*processed* :x))          => 3
                                                                                                                                                (:idx (nth (*processed* :x) 0))   => 0
                                                                                                                                                (:idx (nth (*processed* :x) 1))   => 1
                                                                                                                                                (:idx (nth (*processed* :x) 2))   => 2))) (facts "about complex tags"
                                                                                                                                                                                                 (fact "it should return a state value"
                                                                                                                                                                                                       (with-processed-tag "999 {{bool}}"
                                                                                                                                                                                                                           *processed* =not=> "999 {{bool}}"
                                                                                                                                                                                                                           *processed* => #"999 (true|false)"))
                                                                                                                                                                                                 (fact "it should return a state value"
                                                                                                                                                                                                       (with-processed-tag "{{integer(100, 999)}} {{street()}}, {{city()}}, {{state()}}, {{integer(100, 10000)}}"
                                                                                                                                                                                                                           *processed* =not=> "{{integer(100, 999)}} {{street()}}, {{city()}}, {{state()}}, {{integer(100, 10000)}}"
                                                                                                                                                                                                                           *processed* => #"[0-9]{3} .+, .+, [A-Z][A-Z], [0-9]{3,5}"))) (facts "about company()"
                                                                                                                                                                                                                                                                                               (with-processed-tag "{{company(3)}}"
                                                                                                                                                                                                                                                                                                                   (fact "it should return a value"
                                                                                                                                                                                                                                                                                                                         *processed* =not=>  "{{company(3)}}"
                                                                                                                                                                                                                                                                                                                         *processed* =>      #".+")))

  (facts "about firstName()"
         (with-processed-tag "{{firstName()}}"
                             (fact "it should return a value"
                                   *processed* =not=> "{{firstName()}}"
                                   (.indexOf all-names *processed*) =not=> -1))) (facts "about firstName('gender')"
                                                                                        (with-processed-tag "{{firstName('male')}}"
                                                                                                            (fact "it should return a male value"
                                                                                                                  *processed*                         =not=> "{{firstName('male')}}"
                                                                                                                  (.indexOf male-names *processed*)   =not=> -1
                                                                                                                  (.indexOf female-names *processed*)     => -1)))

  (facts "about surname()"
         (with-processed-tag "{{surname()}}"
                             (fact "it should return a value"
                                   *processed* =not=> "{{surname()}}"
                                   *processed*     =>      #".+")))

  (facts "about company()"
         (with-processed-tag "{{company()}}"
                             (fact "it should return a value"
                                   *processed* =not=> "{{company()}}"
                                   *processed*     => #".+")))

  (facts "about zip()"
         (with-processed-tag "{{zip()}}"
                             (fact "it should return a value"
                                   *processed* =not=> "{{zip()}}"
                                   *processed*     => #".+")))

  (facts "about bool()"
         (with-processed-tag "{{bool()}}"
                             (fact "it should return a random boolean value"
                                   *processed*                             =not=> "{{bool()}}"
                                   (contains? #{true false} *processed*)     => true)))

  (facts "about guid()"
         (with-processed-tag "{{guid()}}"
                             (fact "it should return a UUID"
                                   *processed* =not=> "{{guid()}}"
                                   *processed*     => h/uuid-regex)))

  (facts "about street"
         (with-processed-tag "{{street()}}"
                             (fact "it should return a value"
                                   *processed*   =not=> "{{street()}}"
                                   *processed*       => #".+")))

  (facts "about city()"
         (with-processed-tag "{{city}}"
                             (fact "it should return a value"
                                   *processed*   =not=> "{{city}}"
                                   *processed*       => #".+")))

  (facts "about state()"
         (with-processed-tag "{{state}}"
                             (fact "it should return a state value"
                                   *processed*   =not=> "{{state}}"
                                   *processed*       => #"[A-Z]{2}")))

  (facts "about integer"
         (with-processed-tag "{{integer()}}"
                             (fact "it should return a positive random integer with no params"
                                   *processed*       =not=> "{{integer()}}"
                                   (< *processed* 0)     => falsey))
         (with-processed-tag "{{integer(1,5)}}"
                             (fact "it should return a positive random integer with no params"
                                   *processed*       =not=> "{{integer(1,5)}}"
                                   (< *processed* 1)     => falsey
                                   (> *processed* 5)     => falsey)))

  (facts "about {{date()}}"
         (with-processed-tag "{{date()}}"
                             (fact "works with zero params"
                                   *processed* =not=> "{{date()}}"
                                   *processed*     = #"[0-9]{8}")))
  (facts "about {{date('20150317','20150317','dd')}}"
         (with-processed-tag "{{date('20150317','20150317','dd')}}"
                             (fact "works with zero params"
                                   *processed* =not=> "{{date('20150317','20150317','dd')}}"
                                   *processed*     => "17")))

  (facts "about {{date(20140101)}}"
         (with-processed-tag "{{date(20140101)}}"
                             (fact "works with zero params"
                                   *processed*                           =not=> "{{date(20140101)}}"
                                   (>= (parse-int *processed*) thisyear)     => true)))
  (facts "about {{date(20140101,20991231)}}"
         (with-processed-tag "{{date(20140101,20991231)}}"
                             (fact "works with zero params"
                                   *processed*                                            =not=> "{{date(20140101,20991231)}}"
                                   (and (< (parse-int *processed*) 20991231) (> (parse-int *processed*) 2014010))     => true)))
  (facts "about {{date(20140101,'today','yyyy')}}"
         (with-processed-tag "{{date(20140101,'today','yyyy')}}"
                             (fact "works with zero params"
                                   *processed*             =not=> "{{date(20140101,'today','yyyy')}}"
                                   (parse-int *processed*)     => (roughly thisyear (- thisyear 2014))))))
