(ns seisei.test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [seisei.test.hello :as hello]))
(enable-console-print!)
(def success 0)
; (defonce cb (atom nil))

(defn ^:export run [completion-callback]
  (reset! cb completion-callback)
  (println "Seisei test started.")
  (run-tests 'seisei.test.hello)
  success)

; (defmethod cljs.test/report [:cljs.test/default :end-run-tests] [m]
;   (if (cljs.test/successful? m)
;     (do (println "xxxxx Success!") (@cb 0))
;     (do (println "xxxxx FAIL") (@cb 1))))
