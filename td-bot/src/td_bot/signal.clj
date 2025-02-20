(ns td-bot.signal
  (:require [clojure.data.csv :as csv]
            [td-bot.tweet :as tweet]
            [td-bot.metrics :as metric]
            [clojure.test :refer :all]
            ;[incanter.charts :refer [line-chart]]
            ;[incanter.core :refer [save]]
            ))

(def bucket-sz-ms 5000)
;; TODO: Stop hard-coding this in two different places
(def window-sz 38) ;; num data points

(defrecord DataPoint [t magnitude])

(declare update-signals)

(defn create-signals [tweets]
  (update-signals nil tweets))

(declare discretize update-bucket)

(defn update-signals
  "Signals is a map where the keys are timestamps and the vals are buckets"
  [signals tweets]
  (let [bucket-sz-ms 5000
        is-rt? (fn [tweet] (tweet/is-retweet? (:text tweet)))
        is-td? (fn [tweet] (tweet/is-touchdown? (:text tweet)))
        tweets-by-time (group-by :t
                                 (->> tweets
                                      (remove is-rt?)
                                      (filter is-td?) ;; b/c of 2014 datasets
                                      (discretize bucket-sz-ms)))]
    (reduce (fn [buckets [t tweetz]]
              (update-in buckets [t] update-bucket tweetz))
            signals
            tweets-by-time)))

(defn- fill-gaps [points]
  (if points
    (let [all-points (->> points
                          (map :t)
                          (apply max)
                          (iterate #(- % 5000))
                          (take window-sz)
                          (into #{}))
          missing-points (clojure.set/difference all-points (set (map :t points)))]
      (sort-by :t (concat (for [t missing-points] (DataPoint. t 0.0)) points)))))

(defn- to-data-points [signals team]
  (butlast
   (for [[t {:keys [num-tweets total-chars mentions-by-team]}] (into (sorted-map) signals)]
     (DataPoint. t (/ (get mentions-by-team team 0) (/ total-chars num-tweets))))))

(defn read-signal [signals team]
  (-> signals
   (to-data-points team)
   (fill-gaps)))

(comment (defn plot-signal
           "Plot the given data points. Save to disk like this:
   (save (plot-signal $tweets) $filename)"
           [data-points]
           (let [[x y] ((juxt #(map :t %) #(map :magnitude %)) data-points)]
             (line-chart x y))))

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

;; These are expensive to create
(def team-patterns
  (into {}
        (for [{:keys [abbrev simple-name]} teams]
          [abbrev (re-pattern (str "(?i)" simple-name))])))

(defn- num-mentions-by-team [tweets]
  (let [tweet-text (map :text tweets)]
    (into {}
          (for [{:keys [simple-name abbrev]} teams]
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

(defn- bucket-times-in-window
  [signals]
  (->> (keys signals)
       (apply max)
       (iterate #(- % bucket-sz-ms))
       (take (inc window-sz)))) ;; intentionally take an extra data point

(defn trim-signals [signals]
  (if signals
    (select-keys signals (bucket-times-in-window signals))))

(declare num-mentions-by-team)

(defn- update-bucket [{:keys [num-tweets total-chars mentions-by-team] :as bucket} tweets]
  (-> bucket
      (update-in [:num-tweets] #(+ (count tweets) (or % 0)))
      (update-in [:total-chars] #(reduce +
                                         (or % 0)
                                         (map (comp count :text) tweets)))
      (update-in [:mentions-by-team] #(merge-with + (num-mentions-by-team tweets) %))))

