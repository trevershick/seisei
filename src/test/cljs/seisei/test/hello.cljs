(ns seisei.test.hello
    (:require [cljs.test :refer-macros [deftest is testing run-tests]]))
(enable-console-print!)

(deftest test-sum-two-numbers
    (is (= (+ 2 2) 4))
    (is (= (+ 0 5) 5)))
