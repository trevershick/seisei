(ns seisei.test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [seisei.test.hello :as hello]))

(enable-console-print!)

;; used to notify of failure of the test suite
(defonce cb (atom nil))

(defn ^:export run [completion-callback]
  (reset! cb completion-callback)
  (println "Seisei test started.")
  (run-tests 'seisei.test.hello))

(defmethod cljs.test/report [:cljs.test/default :end-run-tests] [m]
  (when @cb
    (@cb (if (cljs.test/successful? m) 0 1))))
