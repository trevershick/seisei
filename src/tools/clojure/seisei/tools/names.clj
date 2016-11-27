(ns seisei.tools.names
  (:require
   [clojure.java.io :as io]
   [seisei.tools.util :as u]
   [clojure.string :as s])
  (:gen-class))

(def f 50)
(def params {:input "./data/names.csv"
             :output "src/generated/clojure/seisei/generated/names.clj"
             :ns "seisei.generated.names"})

(defn to-map
  "Create a {:name 'James' :gender 'F' } style map"
  [cols]
  (hash-map :name (nth cols 0) :gender (nth cols 1)))

(defn parse-file []
  (with-open [rdr (io/reader (:input params))
              wrt (io/writer (:output params))]
    (.write wrt (s/join " " ["(" "ns" (:ns params) ")"]))
    (.write wrt (s/join " " ["(" "def" "names" "["]))

    (let [total-count (->> rdr
                           line-seq
                           (map u/split-csv)
                           (filter (u/random-sample f))
                           (map to-map)
                           (map #(str % "\n"))
                           (map #(.write wrt %))
                           count)]
      (.write wrt (s/join " " ["]" ")"]))
      (assoc params :total-count total-count))))

(defn -main
  "Read the state_table.csv file and generate a names constant .clj file."
  [& args]
  (parse-file))
