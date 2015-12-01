(ns td-bot.identification
  (:require [clj-tokenizer.core :as tokenize]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [td-bot
             [metrics :as metric]
             [tweet :refer [is-retweet?]]]))

(defn- read-csv [file]
  (with-open [in-file (io/reader file)]
    (doall
     (csv/read-csv in-file))))

(with-test
  (defn- normalize [s]
    (.toLowerCase (clojure.string/replace s #"[\.\,]" "")))
  (is (= "st louis rams" (normalize "St. Louis Rams")))
  (is (= "odell beckham jr" (normalize "Odell Beckham, Jr."))))

(defn load-roster [file]
  (let [rows (read-csv file)
        headers (map keyword (first rows))
        content (map #(map normalize %) (rest rows))]
    (map #(zipmap headers %) content)))

(def latest-rosters
  (map #(hash-map :team (:team_name %)
                  :player (str (:first_name %) " " (:last_name %)))
       (load-roster "./scripts/rosters.csv")))

(def ^:private stop-words #{"td" "touchdown" "rt" "baby" "a" "an" "the" "and" "yeah"})

(defn- tokenize [tweet]
  "Relies on the Lucene text tokenizer - https://github.com/eandrejko/clj-tokenizer"
  (tokenize/token-seq (tokenize/token-stream-without-stopwords tweet)))

(defn bigrams [tweets]
  (let [counts2 (->> tweets
                     (remove is-retweet?)
                     (map tokenize)
                     (mapcat #(partition 2 1 %))
                     (frequencies)
                     (sort-by val >))]
    (let [unfiltered (reduce (fn [r [b c]]
                               (conj r (hash-map :bigram b :count c)))
                             []
                             counts2)]
      (filter #(not-any? stop-words (:bigram %)) unfiltered))))

(deftest bigram-generation
  (testing "We strip out common words"
    (is (= {:count 2
            :bigram ["less" "common"]}
           (first (bigrams ["td rt touchdown"
                            "td rt td rt td rt"
                            "look less common words remain untouched"
                            "some more less common things surviving"]))))
    (testing "We tokenize before generating bigrams"
      (is (= [{:count 3
               :bigram ["demarco" "murray"]}]
             (bigrams ["demarco!!! murray."
                       "demarco- murray,"
                       "demarco:     ([murray})_"]))))
    (testing "We don't remove our stop words until after partitioning into bigrams"
      (is (not=
           [{:count 1, :bigram ["demarco" "murray"]}]
           (bigrams ["demarco touchdown murray"]))))))

(defn identify-scorer
  ([tweets]
   (identify-scorer latest-rosters tweets))
  ([rosters tweets]
   (let [raw-pairs (metric/timed :bigrams (map :bigram (bigrams tweets)))
         player-names (metric/timed :player-names (set (map :player rosters)))
         best-match (some (fn [[w1 w2]]
                            (let [name (apply str w1 " " w2)]
                              (player-names name)))
                          raw-pairs)]
     (metric/timed :best-match (first (filter #(= (:player %) best-match) rosters))))))

;; The rosters as they were during the games in our test set
(def roster-snapshot
  [{:player "demarco murray"
    :team "dallas cowboys"}
   {:player "dez bryant"
    :team "dallas cowboys"}
   {:player "chris polk"
    :team "philadelphia eagles"}
   {:player "darren sproles"
    :team "philadelphia eagles"}
   {:player "andrew quarless"
    :team "green bay packers"},
   {:player "antonio brown"
    :team "pittsburgh steelers"},
   {:player "bernard pierce"
    :team "baltimore ravens"},
   {:player "crockett gillmore"
    :team "baltimore ravens"},
   {:player "danny amendola"
    :team "new england patriots"},
   {:player "darren fells"
    :team "arizona cardinals"},
   {:player "doug baldwin"
    :team "seattle seahawks"},
   {:player "fozzy whittaker"
    :team "carolina panthers"},
   {:player "garry gilliam"
    :team "seattle seahawks"},
   {:player "james develin"
    :team "new england patriots"},
   {:player "jermaine kearse"
    :team "seattle seahawks"},
   {:player "jonathan stewart"
    :team "carolina panthers"},
   {:player "justin forsett"
    :team "baltimore ravens"},
   {:player "kam chancellor"
    :team "seattle seahawks"},
   {:player "kamar aiken"
    :team "baltimore ravens"},
   {:player "kelvin benjamin"
    :team "carolina panthers"},
   {:player "legarrette blount"
    :team "new england patriots"},
   {:player "luke willson"
    :team "seattle seahawks"},
   {:player "marion grice"
    :team "arizona cardinals"},
   {:player "marshawn lynch"
    :team "seattle seahawks"},
   {:player "martavis bryant"
    :team "pittsburgh steelers"},
   {:player "nate solder"
    :team "new england patriots"},
   {:player "owen daniels"
    :team "baltimore ravens"},
   {:player "randall cobb"
    :team "green bay packers"},
   {:player "rob gronkowski"
    :team "new england patriots"},
   {:player "russell wilson"
    :team "seattle seahawks"},
   {:player "steve smith"
    :team "carolina panthers"},
   {:player "terrance williams"
    :team "dallas cowboys"},
   {:player "tom brady"
    :team "new england patriots"},
   {:player "torrey smith"
    :team "baltimore ravens"},
   {:player "tyler clutts"
    :team "dallas cowboys"},
   {:player "zurlon tipton"
    :team "indianapolis colts"}])

(deftest player-and-team-identification
  (testing "We can extract the scorer from a simple data set"
    (let [lines (clojure.string/split (slurp "data/demarco-identify.txt") #"\n")
          tweets (-> "data/demarco-identify.txt"
                     slurp
                     (clojure.string/replace "\"" "")
                     (clojure.string/split #"\n"))]
      (is (= {:player "demarco murray"
              :team "dallas cowboys"}
             (identify-scorer roster-snapshot tweets)))))
  (testing "We don't use retweets"
    (is (= {:player "darren sproles"
            :team "philadelphia eagles"}
           (identify-scorer roster-snapshot
                            ["RT: Demarco Murray scored five minutes ago"
                             "rt Did you see that Demarco Murray touchdown yesterday?"
                             "Darren Sproles *just* scored"]))))
  (testing "We don't just take the most recent name mentioned"
    (is (= {:player "demarco murray"
            :team "dallas cowboys"}
           (identify-scorer roster-snapshot
                            ["demarco murray scored"
                             "touchdown by demarco murray"
                             "yay demarco murray"
                             "oh and darren sproles too"])))))
