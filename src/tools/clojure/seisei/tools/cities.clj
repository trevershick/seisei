(ns seisei.tools.cities
	(:require
		[clojure.java.io :as io]
		[clojure.string :as s])
	(:gen-class))

;; http://download.maxmind.com/download/worldcities/worldcitiespop.txt.gz

(def f 500)
(def params {
	:input "./worldcitiespop.txt"
	:output "src/generated/clojure/seisei/generated/cities.clj"
	:ns "seisei.generated" })

(defn parse-file []
	(with-open [ rdr (io/reader (:input params))
				 wrt (io/writer (:output params)) ]
	(.write wrt (s/join " " [ "(" "ns" (:ns params) ")" ]))
	(.write wrt (s/join " " [ "(" "def" "cities" "[" ]))
	(doseq [line (line-seq rdr)
			:when (and (.startsWith line "us,") (= 0 (rand-nth (range 0 f))))]
		(let [cols (s/split line #",")]
			(.write wrt (str "\"" (nth cols 2) "\"\n" ))
		))
	(.write wrt (s/join " " [ "]" ")" ]))
))


(defn -main
	"I don't do a whole lot ... yet."
	[& args]
	(parse-file))


