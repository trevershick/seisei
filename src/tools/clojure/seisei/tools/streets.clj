(ns seisei.tools.streets
  (:require
    [clojure.java.io :as io]
    [seisei.tools.util :as u]
    [clojure.string :as s])
  (:gen-class))


(def f 50)
(def params {
             :input "./data/streets.csv"
             :output "src/generated/clojure/seisei/generated/streets.clj"
             :ns "seisei.generated.streets" })



(defn to-map
  [cols]
  (hash-map :full (u/capitalize (nth cols 0))
       :direction (u/capitalize (nth cols 1))
       :streetStreet (u/capitalize (nth cols 2))
       :suffix (u/capitalize (nth cols 3))
       :min (nth cols 5)
       :max (nth cols 6)))

(defn parse-file []
  (with-open [rdr (io/reader (:input params))
              wrt (io/writer (:output params)) ]
    (.write wrt (s/join " " [ "(" "ns" (:ns params) ")" ]))
    (.write wrt (s/join " " [ "(" "def" "streets" "[" ]))

    (let [total-count (->> rdr
                           line-seq
                           rest ;; skip the first line
                           (filter (u/random-sample f))
                           (map u/split-csv)
                           (map to-map)
                           (map #(str % "\n" ))
                           (map #(.write wrt %))
                           count
                           )]
      (.write wrt (s/join " " [ "]" ")" ]))
      (assoc params :total-count total-count))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (parse-file))


