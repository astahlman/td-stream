(ns td-bot.acceptance-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [td-bot
             [bot :as bot]
             [test-data :refer [ground-truth]]
             [tweet :refer [file-stream json->tweet]]])
  (:import td_bot.bot.TestClock))

(with-test
  (defn precision [{:keys [true-pos false-pos]}]
    (let [denom (+ true-pos false-pos)]
      (if (zero? denom)
        Double/NaN
        (/ true-pos denom))))
  (is (= 3/4 (precision {:true-pos 3 :false-pos 1})))
  (is (Double/isNaN (precision {:true-pos 0 :false-pos 0}))))

(with-test
  (defn recall [{:keys [true-pos false-neg]}]
    (/ true-pos (+ true-pos false-neg)))
  (is (= 2/5 (recall {:true-pos 2 :false-neg 3}))))

(with-test
  (defn f1-score
    "Results should have the following keys
   1. :true-pos
   2. :false-pos
   3. :false-neg"
    [results]
    (let [p (precision results)
          r (recall results)]
      (cond
        (Double/isNaN p) 0
        (zero? (+ p r)) 0
        :else (* 2 (/ (* p r) (+ p r))))))
  (is (= 1 (f1-score {:true-pos 1 :false-pos 0 :false-neg 0})))
  (is (= 0.6 (double (f1-score {:true-pos 3 :false-pos 3 :false-neg 1}))))
  (testing "If precision and recall are 0, f-1 is 0"
    (is (zero? (f1-score {:true-pos 0 :false-pos 10 :false-neg 10}))))
  (testing "If precision is undefined, f-1 is 0"
    (is (zero? (f1-score {:true-pos 0 :false-pos 0 :false-neg 10})))))

(defn test-clock-with-start
  "Return a clock that increments one-second on every tick with the given start time"
  [start]
  (TestClock. start 5000))

;; starts at beginning of DAL-PHI game
(def test-clock
  (test-clock-with-start 1418606922542))

(def ^:private dal-phi-file "data/raw/tweets.2014-12-15.01.json")
(def ^:private car-ari-file "data/raw/tweets.2015-01-03.21.json")

(defn- simple-alert [{:keys [team happened-at]}]
  (println (str "TOUCHDOWN!!! By the " team " @ " happened-at ". Hooray!")))

(defn test-bot
  ([] (test-bot dal-phi-file))
  ([file]
   (conj (bot/system) {:clock test-clock
                       :tweet-stream (file-stream file)
                       :td-hook simple-alert
                       ;:scorer-ider (partial id/identify-scorer
                       ;id/roster-snapshot)
                       })))

(defn find-start-time
  "Return the timestamp of the first tweet in the file"
  [file]
  (let [rdr (io/reader file)
        line (try (.readLine rdr) (catch Exception e) (finally (.close rdr)))
        tweet (json->tweet line)]
    (:t tweet)))

(declare false-neg-and-true-pos)

(defn- map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn label-detections [det tds]
  (let [r (false-neg-and-true-pos det tds)
        tp (set (map :identified-at (:true-pos r)))
        fp (filter #(not (contains? tp (:identified-at %))) det)
        fp (map #(clojure.set/rename-keys %
                                           {:team :identified-team
                                            :player :identified-player}) fp)]
    (map-vals
     (assoc r :false-pos fp)
     #(seq (sort-by :t %)))))

(defn false-neg-and-true-pos [det tds]
  (letfn [(match [d td]
            (let [diff (- (:identified-at d) (:t td))]
                 (and (pos? diff) (<= diff 30000))))
          (merge-det-and-td [d td]
            (merge
             td
             (clojure.set/rename-keys
              d
              {:team :identified-team, :player :identified-player})))]
    (reduce (fn [r td]
              (if-let [m (first (filter #(match % td) det))]
                (update-in r [:true-pos] conj (merge-det-and-td m td))
                (update-in r [:false-neg] conj td)))
            {:true-pos nil :false-neg nil} tds)))

(deftest label-detections-test
  (testing "We count touchdowns as detected if they happened in the previous 30 seconds"
    (let [detected [{:identified-at 7000 :player "foo" :team "bar"} ;;; true positive
                    {:identified-at 41000 :player "bar" :team "baz"} ;;; false positive
                    {:identified-at 65000 :player "baz" :team "buzz"} ;;; true positive
                    {:identified-at 66000 :player "buzz" :team "bop"}] ;;; false positive
          truth [{:t 2000
                  :player "_foo"
                  :team "_bar"} ;;; latency = 5000
                 {:t 10000
                  :player "_foo"
                  :team "_bar"} ;;; false negative
                 {:t 60000
                  :player "_baz"
                  :team "_buzz"}]] ;;; latency = 5000
      (is (= {:true-pos [{:t 2000
                          :identified-at 7000
                          :player "_foo" :team "_bar"
                          :identified-player "foo"
                          :identified-team "bar"}
                         {:t 60000
                          :identified-at 65000
                          :player "_baz"
                          :team "_buzz"
                          :identified-player "baz"
                          :identified-team "buzz"}]
              :false-pos [{:identified-at 41000
                           :identified-player "bar"
                           :identified-team "baz"}
                          {:identified-at 66000
                           :identified-player "buzz"
                           :identified-team "bop"}]
              :false-neg [{:t 10000
                           :player "_foo"
                           :team "_bar"}]}
             (label-detections detected truth))))))

(deftest deduping-alerts
  (testing "We don't associate multiple alerts with the same touchdown"
    (let [detected [{:identified-at 1000}
                    {:identified-at 2000}]
          truth [{:t 0}]]
      (is (= {:true-pos [{:t 0 :identified-at 1000}]
              :false-pos [{:identified-at 2000}]
              :false-neg nil}
             (label-detections detected truth))))))

(defn score-output
  "Assigns an F-1 score to labelled detections"
  [out]
  (let [counts (reduce (fn [m [k v]]
                         (assoc m k (count v)))
                       {} out)]
    (f1-score counts)))

(deftest scoring-output
  (testing "A perfect bot results in a perfect score"
    (is (= 1 (score-output {:true-pos [{:t 1000 :latency 1000}
                                       {:t 2000 :latency 1000}]
                            :false-pos nil
                            :false-neg nil}))))
  (testing "The timings of the detections don't matter, only count of each lambel"
    (is (= 2/3 (score-output {:true-pos [{:t 1 :latency 2}
                                         {:t 2 :latency 2}
                                         {:t 3 :latency 2}]
                              :false-neg [{:t 1}
                                          {:t 2}]
                              :false-pos [{:t 1}]})))))

(defn- score-player-id
  "Return the ratio of correct identifications for player and team"
  [labelled-results]
  (let [tp (:true-pos labelled-results)]
    (if (zero? (count tp))
      0
      (letfn [(match? [k1 k2]
                #(apply = ((juxt k1 k2) %)))]
        (hash-map :player-score (/ (count
                                    (filter (match? :player :identified-player) tp))
                                   (count tp))
                  :team-score (/ (count
                                  (filter (match? :team :identified-team) tp))
                                 (count tp)))))))

(deftest identification
  (testing "The identification function scores along both the player and team dimensions"
    (let [labelled-results {:true-pos [{:team "dallas cowboys"
                                        :identified-team "dallas cowboys"
                                        :player "demarco murray"
                                        :identified-player "dez bryant"}
                                       {:team "pittsburgh steelers"
                                        :identified-team "pittsburgh steelers"
                                        :player "antonio brown"
                                        :identified-player "antonio brown"}
                                       {:team "seattle seahawks"
                                        :identified-team "miami dolphins"
                                        :player "marshawn lynch"
                                        :identified-player "marshawn lynch"}]}]
      (is (= {:player-score (/ 2 3)
              :team-score (/ 2 3)}
             (score-player-id labelled-results))))))

(defn run-test
  "Run bot and label results according to ground truth supplied in true-tds"
  ([true-tds]
   (run-test true-tds (test-bot)))
  ([true-tds bot]
   (let [detections (atom ())
         save-detection (fn [td] (swap! detections conj td))
         bot (update-in bot [:td-hook] (fn [old-hook]
                                        (fn [td]
                                          (do
                                            (old-hook td)
                                            (save-detection td)))))
         indefinitely (fn [_] true)]
     (bot/main-loop indefinitely bot)
     (let [raw-detections @detections
           results (label-detections raw-detections true-tds)]
       (println (str "Raw detections:" (clojure.pprint/pprint raw-detections)))
       (println (str "Results: " (clojure.pprint/pprint results)))
       (assoc results
              :detections raw-detections
              :f1-score (score-output results)
              :player-id-score (score-player-id results))))))

(defn run-game
  "Instantiate and run a bot against the game capture in a specific file"
  [file]
  (let [truth (filter #(= file (str "data/raw/" (:file %))) ground-truth)
        true-tds (map #(select-keys % [:t :player :team]) truth)
        start-time (find-start-time file)
        bot (assoc (test-bot file)
                   :clock (test-clock-with-start start-time))]
    (run-test true-tds bot)))

(deftest ^:integration cowboys-eagles-game
  (testing "Our touchdown scores 'pretty well' on our test data set from
            the Cowboys vs. Eagles game"
    (let [results (run-game dal-phi-file)]
      (is (>= (:f1-score results) 14/17))
      ;; ignore player score until we bring back scorer identification
      (comment (is (= {:player-score 1
                       :team-score 1}
                      (:player-id-score results)))))))

(defn run-entire-test-set
  "Run the bot against our entire test-set"
  []
  (let [games (partition-by :file ground-truth)
        bots (map (fn [game]
                    (let [file (str "data/raw/" (:file (first game)))
                          start-time (find-start-time file)]
                      (assoc (test-bot file)
                             :clock (test-clock-with-start start-time)))) games)
        ground-truth (map (fn [game]
                            #(select-keys % [:t :player :team]) game)
                          games)]
    (pmap (fn [truth bot]
            (run-test truth bot)) ground-truth bots)))
