(ns seisei.tools.all
  (:require
   [seisei.tools.names])
  (:gen-class))

(defn execute [what, with]
  (println (str "Generating " what))
  (let [results (with)
        total (:total-count results)
        out (:output results)
        in (:input results)]
    (println (str "Generated " total " " what " from " in " to " out))))

(defn -main
  "Runs all data generation programs"
  [& args]

  (execute "Given Names" seisei.tools.names/parse-file)
  (println "Done"))
