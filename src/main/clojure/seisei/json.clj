(ns seisei.json
  (:require [clojure.data.json :as json])
  (:gen-class)
  )


(defn ^:private fix-single-ticks
  [jsonString]
  (if (nil? jsonString)
    nil
    (clojure.string/replace jsonString #"'" "\"")))

(defn jsonify [str]
  (if (nil? str)
    nil
    (json/read-str (fix-single-ticks str))))


(defn parse [jsonString]
  (let [fixed-string (fix-single-ticks jsonString)]
    (if (nil? fixed-string)
      nil
      (json/read-str fixed-string :key-fn keyword))))

(defn parse-with-error
  [jsonString]
  (if (nil? jsonString)
    {:errors [] :output nil :input nil}
    (try
      (let [fixed-string jsonString
            parsed (json/read-str fixed-string :key-fn keyword)]
        { :input jsonString
          :output parsed
          :errors []})
      (catch Exception e {:errors [(.getMessage e)]
                           :output nil
                           :input jsonString}))))
