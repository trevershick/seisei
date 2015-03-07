(ns seisei.tools.surnames
  (:require
    [clojure.java.io :as io]
    [seisei.tools.util :as u]
    [clojure.string :as s])
  (:gen-class))

(def f 200)
(def params {:input "./data/surnames.csv"
             :output "src/generated/clojure/seisei/generated/surnames.clj"
             :ns "seisei.generated.surnames" })


(defn to-map
  [cols]
  (u/capitalize (nth cols 0)))

(defn parse-file []
  (with-open [rdr (io/reader (:input params))
              wrt (io/writer (:output params)) ]
    (.write wrt (s/join " " [ "(" "ns" (:ns params) ")" ]))
    (.write wrt (s/join " " [ "(" "def" "surnames" "[" ]))

    (let [total-count (->> rdr
                           line-seq
                           (map u/split-csv)
                           (filter (u/random-sample f))
                           (map to-map)
                           (map #(str "\"" % "\"\n" ))
                           (map #(.write wrt %))
                           count
                           )]
      (.write wrt (s/join " " [ "]" ")" ]))
      (assoc params :total-count total-count))))


