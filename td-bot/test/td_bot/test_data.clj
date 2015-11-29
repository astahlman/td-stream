(ns td-bot.test-data
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

;; Ground truth from Cowboys vs. Eagles - tweets.2014-12-15.01.json
(def dal-phi-touchdowns
  (map (partial zipmap [:t :player :team])
       [[1418607396778 "demarco murray" "dallas cowboys"]
        [1418608620457 "dez bryant" "dallas cowboys"]
        [1418609918817 "dez bryant" "dallas cowboys"]
        [1418610531857 "chris polk" "philadelphia eagles"]
        [1418614014826 "chris polk" "philadelphia eagles"]
        [1418614584563 "darren sproles" "philadelphia eagles"]
        [1418615124627 "demarco murray" "dallas cowboys"]
        [1418615763277 "dez bryant" "dallas cowboys"]]))

(def ^:const headers ["season" "week" "team" "player" "t" "reversed?" "nullified?" "file"])

(defn- read-tsv [file]
  (with-open [in-file (io/reader file)]
    (doall
     (csv/read-csv in-file :separator \tab))))

(defn- parse-row [row]
  (-> (zipmap (map keyword headers) row)
      (update-in [:week] #(Integer. ^String %))
      (update-in [:season] #(Integer. ^String %))
      (update-in [:team] keyword)
      (update-in [:t] #(Long. ^String %))
      (update-in [:reversed?] #(not (nil? (re-find #"(?i)true" ^String %))))
      (update-in [:nullified?] #(= "TRUE" ^String %))))

(defn load-truth-from-file [file]
  (map parse-row (rest (read-tsv file))))

(def ground-truth (load-truth-from-file "data/truth/ground-truth-tds.tsv"))
