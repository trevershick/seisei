(ns seisei.json-test
  (:use midje.sweet)
  (:require   [clojure.test     :refer [testing is deftest]]
              [seisei.json      :as j]))

(deftest test-jsonify
  (facts "about jsonify"
    (fact "returns nil when nil passed in"
      (j/jsonify nil) => nil?)
    (fact "converts strings into json"
      (j/jsonify "{\"x\":1}") => {"x" 1})
    )
)
(deftest test-parse
  (facts "about parse"
    (fact "nil for nil"
      (j/parse nil) => nil)
    (fact "returns json structures with keywords"
      (j/parse "{\"x\":1}") => {:x 1})
  )
)

(deftest test-json-parse-with-error
  (facts "about parse-with-error"
    (fact "returns a 3 part structure using keywords for json structure"
      (j/parse-with-error "{\"x\":1}") => {:errors [] :input "{\"x\":1}" :output {:x 1}})
    (fact "returns errors in an array"
      (j/parse-with-error "{x:1}") => {:errors ["JSON error (unexpected character): x"] :input "{x:1}" :output nil})
  )
)
