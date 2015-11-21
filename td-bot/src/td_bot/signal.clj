(ns td-bot.signal
  (:require [clojure.data.csv :as csv]
            [td-bot.tweet :as tweet]
            [td-bot.metrics :as metric]
            [clojure.test :refer :all]
            [incanter.charts :refer [line-chart]]
            [incanter.core :refer [save]]))

(declare all-signals)

(defn update-signals [signals tweets]
  (if-let [tweets (seq tweets)]
    ((:update signals) tweets)
    signals))

(defn create-signals
  ([] (all-signals nil))
  ([tweets] (update-signals (create-signals) tweets)))

(defn read-signal [signals team] ((:read signals) team))

(defrecord DataPoint [t magnitude])

(defn plot-signal [data-points]
  "Plot the given data points. Save to disk like this:
   (save (plot-signal $tweets) $filename)"
  (let [[x y] ((juxt #(map :t %) #(map :magnitude %)) data-points)]
    (line-chart x y)))

(declare teams)

(def teams
  [{:location "Arizona" :full-name "Arizona Cardinals" :simple-name "Cardinals" :abbrev "ARI"}
   {:location "Atlanta" :full-name "Atlanta Falcons" :simple-name "Falcons" :abbrev "ATL"}
   {:location "Baltimore" :full-name "Baltimore Ravens" :simple-name "Ravens" :abbrev "BAL"}
   {:location "Buffalo" :full-name "Buffalo Bills" :simple-name "Bills" :abbrev "BUF"}
   {:location "Carolina" :full-name "Carolina Panthers" :simple-name "Panthers" :abbrev "CAR"}
   {:location "Chicago" :full-name "Chicago Bears" :simple-name "Bears" :abbrev "CHI"}
   {:location "Cincinnati" :full-name "Cincinnati Bengals" :simple-name "Bengals" :abbrev "CIN"}
   {:location "Cleveland" :full-name "Cleveland Browns" :simple-name "Browns" :abbrev "CLE"}
   {:location "Dallas" :full-name "Dallas Cowboys" :simple-name "Cowboys" :abbrev "DAL"}
   {:location "Denver" :full-name "Denver Broncos" :simple-name "Broncos" :abbrev "DEN"}
   {:location "Detroit" :full-name "Detroit Lions" :simple-name "Lions" :abbrev "DET"}
   {:location "Green Bay" :full-name "Green Bay Packers" :simple-name "Packers" :abbrev "GB"}
   {:location "Houston" :full-name "Houston Texans" :simple-name "Texans" :abbrev "HOU"}
   {:location "Indianapolis" :full-name "Indianapolis Colts" :simple-name "Colts" :abbrev "IND"}
   {:location "Jacksonville" :full-name "Jacksonville Jaguars" :simple-name "Jaguars" :abbrev "JAC"}
   {:location "Kansas City" :full-name "Kansas City Chiefs" :simple-name "Chiefs" :abbrev "KC"}
   {:location "Miami" :full-name "Miami Dolphins" :simple-name "Dolphins" :abbrev "MIA"}
   {:location "Minnesota" :full-name "Minnesota Vikings" :simple-name "Vikings" :abbrev "MIN"}
   {:location "New England" :full-name "New England Patriots" :simple-name "Patriots" :abbrev "NE"}
   {:location "New Orleans" :full-name "New Orleans Saints" :simple-name "Saints" :abbrev "NO"}
   {:location "NY" :full-name "NY Giants" :simple-name "Giants" :abbrev "NYG"}
   {:location "NY" :full-name "NY Jets" :simple-name "Jets" :abbrev "NYJ"}
   {:location "Oakland" :full-name "Oakland Raiders" :simple-name "Raiders" :abbrev "OAK"}
   {:location "Philadelphia" :full-name "Philadelphia Eagles" :simple-name "Eagles" :abbrev "PHI"}
   {:location "Pittsburgh" :full-name "Pittsburgh Steelers" :simple-name "Steelers" :abbrev "PIT"}
   {:location "San Diego" :full-name "San Diego Chargers" :simple-name "Chargers" :abbrev "SD"}
   {:location "San Francisco" :full-name "San Francisco 49ers" :simple-name "49ers" :abbrev "SF"}
   {:location "Seattle" :full-name "Seattle Seahawks" :simple-name "Seahawks" :abbrev "SEA"}
   {:location "St. Louis" :full-name "St. Louis Rams" :simple-name "Rams" :abbrev "STL"}
   {:location "Tampa Bay" :full-name "Tampa Bay Buccaneers" :simple-name "Buccaneers" :abbrev "TB"}
   {:location "Tennessee" :full-name "Tennessee Titans" :simple-name "Titans" :abbrev "TEN"}
   {:location "Washington" :full-name "Washington Redskins" :simple-name "Redskins" :abbrev "WAS"}])

(def sample-tweets [{:text "touchdown steelers" :t 1000}
                    {:text "touchdown baby" :t 2000}
                    {:text "seahawks touchdown yeah" :t 3000}
                    {:text "that's a touchdown falcons" :t 4000}
                    {:text "touchdown bears" :t 5000}
                    {:text "a 49ers touchdown" :t 6000}
                    {:text "RT: yay a touchdown for the patriots" :t 11000}
                    {:text "falcons defense gave up a touchdown" :t 16000}])

;; These are expensive to create
(def team-patterns
  (into {}
        (for [{:keys [abbrev simple-name]} teams]
          [abbrev (re-pattern (str "(?i)" simple-name))])))

(defn- num-mentions-by-team [tweets]
  (let [tweet-text (map :text tweets)]
    (into {}
          (for [{:keys [simple-name abbrev]} (filter #(contains? #{"DAL" "PHI"} (:abbrev %)) teams)]
            [(keyword abbrev)
             (metric/timed :fill-team-partition
                           (count 
                            (filter (fn [tweet]
                                      (metric/timed :re-find
                                                    (re-find
                                                     (get team-patterns abbrev)
                                                     tweet)))
                                    tweet-text)))]))))

(defn- discretize
  "Round the :timestamp of all tweets up to the nearest bucket-sz-ms"
  ([tweets] (discretize 5000 tweets))
  ([bucket-sz-ms tweets]
   (let [round-up #(* bucket-sz-ms (int (Math/ceil (/ % bucket-sz-ms))))]
     (map (fn [tweet]
            (update-in
             tweet
             [:t]
             round-up)) tweets))))

(declare bucket)

(defn empty-bucket [] (bucket 0 0 nil))

(defn- create-bucket [tweets]
  (bucket tweets))

(defn- bucket
  "The magnitude of every team's signal in a given time bucket"
  ([tweets]
   ((:update (bucket 0 0 nil)) tweets))
  ([num-tweets total-chars mentions-by-team]
   (let [avg-tweet-length (if (> num-tweets 0)
                            (/ total-chars num-tweets)
                            Double/NaN)]
     {:magnitude (fn [team]
                   (if (not (Double/isNaN avg-tweet-length))
                     (/ (get mentions-by-team team) avg-tweet-length)
                     0.0))
      :update (fn [tweets]
                (let [num-tweets (+ (count tweets) (or num-tweets 0))
                      total-chars (reduce +
                                          (or total-chars 0)
                                          (map (comp count :text) tweets))
                      mentions-by-team (metric/timed :mentions-by-team (num-mentions-by-team tweets))]
                  (bucket
                   num-tweets
                   total-chars
                   mentions-by-team)))})))

(defn- all-signals [buckets-by-time]
  (let [bucket-sz-ms 5000
        window-sz-ms 100000
        ;; We intentionally drop the most recent bucket, as its value
        ;; will likely change next on the next update
        window-range (fn [buckets]
                       (let [latest-t (apply max (keys buckets))]
                         (range (- latest-t window-sz-ms) latest-t bucket-sz-ms)))
        trim-to-window (fn [buckets]
                         (metric/timed :trim-to-window
                                       (into {}
                                             (for [t (metric/timed :window-range (window-range buckets))]
                                               [t (get buckets t (empty-bucket))]))))]
    {:read (fn [team]
             (sort-by :t
                      (for [[t bucket] buckets-by-time]
                        (DataPoint. t ((:magnitude bucket) team)))))
     :update (fn [tweets]
               (let [tweets-by-time (group-by :t
                                              (->> tweets
                                                   (remove tweet/is-retweet?)
                                                   (discretize bucket-sz-ms)))]
                 (all-signals
                  
                  (trim-to-window
                   (reduce (fn [buckets [t tweetz]]
                             (if-let [bucket (get buckets t)]
                               (assoc buckets t (metric/timed :update-bucket
                                                              ((:update bucket) tweetz)))
                               (assoc buckets t (metric/timed :create-buckets (create-bucket tweetz)))))
                           buckets-by-time
                           tweets-by-time)))))}))
