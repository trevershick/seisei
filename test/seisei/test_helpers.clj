(ns seisei.test-helpers
	(:use midje.sweet)
	(:require [clojure.data.json :as json]
			[seisei.json :refer :all]))

(def uuid-regex #"[a-f0-9]+-[a-f0-9]+-[a-f0-9]+-[a-f0-9]+-[a-f0-9]+")

(defn jsonfixture [data] ( json/read-str ( json/write-str data ) :key-fn keyword ))
