(ns td-bot.fixture
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clj-time.format :as fmt]
            [td-bot.metrics :as metric]
            [clojure.test :refer :all]))

(def ^:const headers ["week" "home" "away" "start-time"])
(def files-by-year {2015 "data/fixtures/nfl-schedule-2015.tsv"
                    2014 "data/fixtures/nfl-schedule-2014.tsv"})

(defn- iso-str->epoch-ms [s]
  (.getMillis
   (try
     (fmt/parse (fmt/formatters :date-time-no-ms) s)
     (catch NullPointerException e
         (println (str "Null pointer: " s))))))

(defn- parse-row [row]
  (-> (zipmap (map keyword headers) row)
      (update-in [:week] #(Integer. ^String %))
      (update-in [:home] keyword)
      (update-in [:away] keyword)
      (update-in [:start-time] iso-str->epoch-ms)))

(defn- read-fixtures-tsv [file]
  (with-open [in-file (io/reader file)]
    (doall
     (csv/read-csv in-file :separator \tab))))

(defn- load-fixtures-from-file [file]
  (map parse-row (rest ;; skip headers
                  (read-fixtures-tsv file)))) 

(defn- load-fixtures [year]
  (if-let [file (get files-by-year year)]
    (load-fixtures-from-file file)))

(defn game-in-progress? [kickoff now]
  (let [delta (- now kickoff)
        four-and-a-half-hours (* 1000 60 60 4.5)]
    (and (>= delta 0)
         (< delta four-and-a-half-hours))))

(def all-fixtures
  (flatten
   (for [year [2014 2015]]
     (map #(assoc % :year year) (load-fixtures year)))))

(defn active-fixtures-at
  ([t]
   (active-fixtures-at t all-fixtures))
  ([t fixtures]
   (let [in-progress? (fn [fixture] (game-in-progress? (:start-time fixture) t))]
     (filter in-progress? fixtures))))

(deftest test-active-fixtures
  (testing "We call a game in progress if it started less than 4.5 hours ago"
    (is (= 1 (count (active-fixtures-at 1420322307784))))))
