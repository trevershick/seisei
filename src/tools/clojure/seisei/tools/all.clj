(ns seisei.tools.all
  (:require
    [seisei.tools.cities]
    [seisei.tools.states]
    [seisei.tools.companies]
    [seisei.tools.zips]
    [seisei.tools.names]
    [seisei.tools.surnames]
    [seisei.tools.streets])
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

  (execute "Companies" seisei.tools.companies/parse-file)
  (execute "Cities" seisei.tools.cities/parse-file)
  (execute "States" seisei.tools.states/parse-file)
  (execute "Zip Codes" seisei.tools.zips/parse-file)
  (execute "Given Names" seisei.tools.names/parse-file)
  (execute "Surnames" seisei.tools.surnames/parse-file)
  (execute "Streets" seisei.tools.streets/parse-file)
  (println "Done"))
