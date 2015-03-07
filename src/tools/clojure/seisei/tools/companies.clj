(ns seisei.tools.companies
  (:require
    [clojure.java.io :as io]
    [seisei.tools.util :as u]
    [clojure.string :as s])
  (:gen-class))

(def f 3)
(def params {
             :input "./data/companies.csv"
             :output "src/generated/clojure/seisei/generated/companies.clj"
             :ns "seisei.generated.companies" })



(defn to-map
  [cols]
  (u/capitalize (nth cols 1)))

(defn parse-file []
  (with-open [rdr (io/reader (:input params))
              wrt (io/writer (:output params)) ]
    (.write wrt (s/join " " [ "(" "ns" (:ns params) ")" ]))
    (.write wrt (s/join " " [ "(" "def" "companies" "[" ]))

    (let [total-count (->> rdr
                           line-seq
                           rest ;; skip the first line
                           (filter (u/random-sample f))
                           (map u/replace-quotes)
                           (map u/split-csv)
                           (map to-map)
                           set
                           (map #(str "\"" % "\"\n" ))
                           (map #(.write wrt %))
                           count
                           )]
      (.write wrt (s/join " " [ "]" ")" ]))
      (assoc params :total-count total-count))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (parse-file))


