(ns td-bot.identification
  (:use [midje.sweet]
        [td-bot.tweet :only [is-retweet?]])
  (:require [clojure.data.json :as json]
            [clj-tokenizer.core :as tokenize]))

(def teams #{"arizona cardinals"
             "atlanta falcons"
             "baltimore ravens"
             "buffalo bills"
             "carolina panthers"
             "chicago bears"
             "cincinnati bengals"
             "cleveland browns"
             "dallas cowboys"
             "denver broncos"
             "detroit lions"
             "green bay packers"
             "houston texans"
             "indianapolis colts"
             "jacksonville jaguars"
             "kansas city chiefs"
             "miami dolphins"
             "minnesota vikings"
             "new england patriots"
             "new orleans saints"
             "ny giants"
             "ny jets"
             "oakland raiders"
             "philadelphia eagles"
             "pittsburgh steelers"
             "san diego chargers"
             "san francisco 49ers"
             "seattle seahawks"
             "st louis rams"
             "tampa bay buccaneers"
             "tennessee titans"
             "washington redskins"})

(facts "We haven't missed any of the 32 NFL teams"
       (count teams) => 32)

;; TODO: Populate rosters dynamically
(def players
  [{:player "demarco murray"
    :team "dallas cowboys"}
   {:player "dez bryant"
    :team "dallas cowboys"}
    {:player "chris polk"
     :team "philadelphia eagles"}
    {:player "darren sproles"
     :team "philadelphia eagles"}])

(def ^:private stop-words #{"td" "touchdown" "rt" "baby" "a" "an" "the" "and" "yeah"})

(defn- count-occurrences [coll]
  (reduce (fn [m x]
            (assoc m x (inc (m x 0)))) {} coll))

(defn- tokenize [tweet]
  "Relies on the Lucene text tokenizer - https://github.com/eandrejko/clj-tokenizer"
  (tokenize/token-seq (tokenize/token-stream-without-stopwords tweet)))

(defn- bigrams [tweets]
  (let [counts2 (->> tweets
                     (remove is-retweet?)
                     (map tokenize)
                     (mapcat #(partition 2 1 %))
                     (count-occurrences)
                     (sort-by val >))]
    (let [unfiltered (reduce (fn [r [b c]]
                               (conj r (hash-map :bigram b :count c)))
                             []
                             counts2)]
      (filter #(not-any? stop-words (:bigram %)) unfiltered))))

(facts "About how we generate bigrams"
       (fact "We strip out common words"
             (first (bigrams ["td rt touchdown"
                              "td rt td rt td rt"
                              "look less common words remain untouched"
                              "some more less common things surviving"])) => {:count 2
                                                                              :bigram ["less" "common"]})
       (fact "We tokenize before generating bigrams"
             (bigrams ["demarco!!! murray."
                       "demarco- murray,"
                       "demarco:     ([murray})_"]) => [{:count 3
                                                         :bigram ["demarco" "murray"]}])
       (fact "We don't remove our stop words until after partitioning into bigrams"
             (bigrams "demarco touchdown murray") =not=> [{:count 1
                                                           :bigram ["demarco" "murray"]}]))

(defn identify-scorer [tweets]
  (let [raw-pairs (map :bigram (bigrams tweets))
        player-names (into #{} (map :player players))
        best-match (some (fn [[w1 w2]] ;; TODO: this could be cleaner
                           (let [name (apply str w1 " " w2)]
                             (player-names name))) raw-pairs)]
    (first (filter #(= (:player %) best-match) players))))

(fact "We cross-reference our top-N bigrams against a list of players to pick the player. Then we choose the team using a deterministic lookup from player to team"
      (identify-scorer ..tweets..) => {:player "demarco murray"
                                       :team "dallas cowboys"}
      (provided
       (bigrams ..tweets..) => [{:count 3 :bigram ["touchdown" "baby"]}
                                {:count 2 :bigram ["touchdown" "cowboys"]}
                                {:count 1 :bigram ["demarco" "murray"]}]))

(fact "We can extract the scorer from a simple data set"
      (let [lines (clojure.string/split (slurp "data/demarco-identify.txt") #"\n")
            tweets (-> "data/demarco-identify.txt"
                        slurp
                        (clojure.string/replace "\"" "")
                        (clojure.string/split #"\n"))]
        (identify-scorer tweets) => {:player "demarco murray"
                                     :team "dallas cowboys"}))
(fact "We don't use retweets"
      (identify-scorer ["RT: Demarco Murray scored five minutes ago"
                        "rt Did you see that Demarco Murray touchdown yesterday?"
                        "Darren Sproles *just* scored"]) => {:player "darren sproles"
                                                             :team "philadelphia eagles"})

(fact "We don't just take the most recent name mentioned"
      (identify-scorer ["demarco murray scored"
                        "touchdown by demarco murray"
                        "yay demarco murray"
                        "oh and darren sproles too"]) => {:player "demarco murray"
                                                          :team "dallas cowboys"})



