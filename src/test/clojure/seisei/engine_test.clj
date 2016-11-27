(ns seisei.engine-test
  (:use midje.sweet)
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest]]
            [seisei.engine :refer :all]
            [seisei.json :refer :all]
            [seisei.test-helpers :refer :all :as h])
  (:import (java.util Date)))

(defonce ^:dynamic *processed* nil)
(defmacro with-processed-json [arg & body]
  `(binding [*processed* (process ~arg)] (do ~@body)))

(defonce ^:private
  js2
  (h/jsonfixture {:x {:y 1 :z "{{objectId(3)}}"}}))

(deftest engine-test

  (facts "about `parse-operation`"
         (fact "it can handle parameters"
               (let [x (parse-operation "random(3)" "thearg" 3)]
                 (:params x) => '(3)
                 (:name x) => "random"
                 (:arg x) => "thearg"
                 (:i x) => 3))
         (fact "it can handle no params"
               (let [x (parse-operation "random()" "thearg" 3)]
                 (:params x) => '()
                 (:name x) => "random"
                 (:arg x) => "thearg"
                 (:i x) => 3))
         (fact "it can handle no params"
               (let [x (parse-operation "code.name" "thearg" 3)]
                 (:params x) => '()
                 (:name x) => "code.name"
                 (:arg x) => "thearg"
                 (:i x) => 3)))

  (facts "about yesterday"
         (fact "returns a date"
               (instance? java.util.Date (yesterday)) => true))

  (facts "about tag processing"
         (with-processed-json js2
                              (fact "it should work even when nested"
                                    (-> *processed* :x :z) => truthy
                                    (-> *processed* :x :z) => h/uuid-regex))))
