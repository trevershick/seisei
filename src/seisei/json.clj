(ns seisei.json
	(:require [clojure.data.json :as json])
	(:gen-class)
)


(defn ^:private fix-single-ticks [jsonString]
	(clojure.string/replace jsonString #"'" "\""))

(defn jsonify [str]
	(json/read-str (fix-single-ticks str))
)

(defn parse [jsonString]
	(let [fixed-string (fix-single-ticks jsonString)]
		(json/read-str fixed-string :key-fn keyword)
	)
)
