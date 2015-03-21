(ns seisei.core 
  (:require [ring.adapter.jetty :as jetty]
            [seisei.web.handler])
  (:gen-class))

; (defn -main []
;   (jetty/run-jetty seisei.web.handler/app {:port 8888}))

