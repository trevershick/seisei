(ns seisei.tools.states
  (:require
    [clojure.java.io :as io]
    [seisei.tools.util :as u]
    [clojure.string :as s])
  (:gen-class))


(def params {:input "./data/state_table.csv"
             :output "src/generated/clojure/seisei/generated/states.clj"
             :ns "seisei.generated.states" })


(defn to-map
  "Create a {:abbrev 'AL' :name 'Alabama' } style map"
  [cols]
  (hash-map :abbrev (nth cols 2) :name (nth cols 1)))

(defn parse-file []
  (with-open [rdr (io/reader (:input params))
              wrt (io/writer (:output params)) ]
    (.write wrt (s/join " " [ "(" "ns" (:ns params) ")" ]))
    (.write wrt (s/join " " [ "(" "def" "states" "[" ]))

    (let [total-count (->> rdr
                           line-seq
                           rest ;; skip the first line
                           (map u/replace-quotes)
                           (map u/split-csv)
                           (map to-map)
                           (map #(str % "\n" ))
                           (map #(.write wrt %))
                           count
                           )]
      (.write wrt (s/join " " [ "]" ")" ]))
      (assoc params :total-count total-count))))


(defn -main
  "Read the state_table.csv file and generate a states constant .clj file."
  [& args]
  (parse-file))
