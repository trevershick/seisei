(ns seisei.tools.util
  (:require [clojure.string :as s]))

(defn replace-quotes [in-string]
  (s/replace in-string #"\"" ""))

(defn split-csv [in-string]
  (s/split in-string #","))

(defn capitalize
  [in-string]
  (->> (clojure.string/split in-string #" ")
       (map clojure.string/capitalize)
       vec
       (clojure.string/join " ")))

(defn random-sample [r]
  (fn [x] (= 0 (rand-nth (range 0 r)))))
