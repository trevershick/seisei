(ns seisei.tools.cities
  (:require
    [clojure.java.io :as io]
    [seisei.tools.util :as u]
    [clojure.string :as s])
  (:gen-class))

;; http://download.maxmind.com/download/worldcities/worldcitiespop.txt.gz

(def ^:private f 50)
(def ^:private params {
             :input "./data/cities.csv"
             :output "src/generated/clojure/seisei/generated/cities.clj"
             :ns "seisei.generated.cities" })



(defn ^:private to-map
  [cols]
  (u/capitalize (nth cols 11)))

(defn parse-file []
  (with-open [rdr (io/reader (:input params))
              wrt (io/writer (:output params)) ]
    (.write wrt (s/join " " [ "(" "ns" (:ns params) ")" ]))
    (.write wrt (s/join " " [ "(" "def" "cities" "[" ]))

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
  "Downloads a world cities file and generates seisei.generated.cities"
  [& args]
  (parse-file))
