(ns td-bot.acceptance-test
  (:use midje.sweet
        [td-bot.test-data :only [dal-phi-touchdowns ground-truth]]
        [td-bot.tweet :only [file-stream find-start-time]])
  (:require [td-bot.bot :as bot]))

(defn precision [{:keys [true-pos false-pos]}]
  (let [denom (+ true-pos false-pos)]
    (if (zero? denom)
      Double/NaN
      (/ true-pos denom))))

(defn recall [{:keys [true-pos false-neg]}]
  (/ true-pos (+ true-pos false-neg)))

(defn f1-score [results]
  "Results should have the following keys
   1. :true-pos
   2. :false-pos
   3. :false-neg"
  (let [p (precision results)
        r (recall results)]
    (cond
      (Double/isNaN p) 0
      (zero? (+ p r)) 0
      :else (* 2 (/ (* p r) (+ p r))))))

(fact "We can calculate a perfect F1 score"
      (f1-score ..results..) => 1
      (provided
       (precision ..results..) => 1
       (recall ..results..) => 1))

(fact "We can calculate an imperfect F1 score"
      (f1-score ..results..) => 0.6
      (provided
       (precision ..results..) => 0.5
       (recall ..results..) => 0.75))

(facts "About what we do in exceptional cases"
       (fact "We count an undefined F1 as 0"
             (f1-score ..results..) => 0
             (provided
              (precision ..results..) => 0
              (recall ..results..) => 0))
       (fact "If precision is undefined, the F1 score is 0"
             (f1-score ..results..) => 0
             (provided
              (precision ..results..) => Double/NaN
              (recall ..results..) => 1)))

(fact "We can calculate the precision"
      (precision {:true-pos 3
                  :false-pos 1
                  :false-neg anything}) => 3/4)

(fact "If there are no positives, precision is undefined"
      (precision {:false-neg 10
                  :true-pos 0
                  :false-pos 0}) => #(Double/isNaN %))

(fact "We can calculate the recall"
      (recall {:true-pos 2
               :false-pos anything
               :false-neg 3}) => 2/5)

(defn test-clock-with-start [start]
  "Return a clock that increments one-second on every tick with the given start time"
  (fn [_ last-t]
    (+ 1000 (or last-t start))))

(defn test-clock []
  "Starts at beginning of DAL-PHI game"
  (test-clock-with-start 1418606922542))

;(def ^:private dal-phi-file "data/cowboys-eagles.txt")
(def ^:private dal-phi-file "data/raw/tweets.2015-01-11.00.json")
(def ^:private car-ari-file "data/raw/tweets.2015-01-03.21.json")

(defn test-bot
  ([] (test-bot dal-phi-file))
  ([file]
   (conj (bot/system) {:clock test-clock
                       :tweet-stream (file-stream file)})))

(declare false-neg-and-true-pos)

(defn label-detections [det tds]
  (let [r (false-neg-and-true-pos det tds)
        tp (into #{} (map :identified-at (:true-pos r)))
        fp (filter (fn [d] (not (contains? tp (:identified-at d)))) det)]
    (utilize.map/map-vals
     (conj r {:false-pos (map #(clojure.set/rename-keys %
                                                        {:team :identified-team
                                                         :player :identified-player}) fp)})
     #(seq (sort-by :t %)))))

(defn false-neg-and-true-pos [det tds]
  (letfn [(match [d td]
            (let [diff (- (:identified-at d) (:t td))]
                 (and (pos? diff) (<= diff 30000))))
          (merge-det-and-td [d td]
            (-> td
                (merge (clojure.set/rename-keys d {:team :identified-team
                                                   :player :identified-player}))))]
    (reduce (fn [r td]
              (if-let [m (first (filter #(match % td) det))]
                (update-in r [:true-pos] conj (merge-det-and-td m td))
                (update-in r [:false-neg] conj td)))
            {:true-pos nil :false-neg nil} tds)))

(fact "We count touchdowns as detected if they happened in the previous 30 seconds"
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
        (label-detections detected truth)) => {:true-pos [{:t 2000
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
                                                            :team "_bar"}]})

(fact "We don't associate multiple alerts with the same touchdown"
      (let [detected [{:identified-at 1000}
                      {:identified-at 2000}]
            truth [{:t 0}]]
        (label-detections detected truth) => {:true-pos [{:t 0 :identified-at 1000}]
                                              :false-pos [{:identified-at 2000}]
                                              :false-neg nil}))

(defn score-output [out]
  "Assigns an F-1 score to labelled detections"
  (let [counts (reduce (fn [m [k v]]
                         (assoc m k (count v)))
                       {} out)]
    (f1-score counts)))

(facts "About how we score labelled results"
       (fact "A perfect bot results in a perfect score"
             (score-output {:true-pos [{:t 1000 :latency 1000}
                                       {:t 2000 :latency 1000}]
                            :false-pos nil
                            :false-neg nil}) => 1)
       (fact "The timings of the detections don't matter, only count of each label"
             (score-output {:true-pos [{:t 1 :latency 2}
                                       {:t 2 :latency 2}
                                       {:t 3 :latency 2}]
                            :false-neg [{:t 1}
                                        {:t 2}]
                            :false-pos [{:t 1}]}) => 2/3))

(defn- score-player-id [labelled-results]
  "Return the ratio of correct identifications for player and team"
  (let [tp (:true-pos labelled-results)]
    (letfn [(match? [k1 k2]
              #(apply = ((juxt k1 k2) %)))]
      (hash-map :player-score (/ (count
                                  (filter (match? :player :identified-player) tp))
                                 (count tp))
                :team-score (/ (count
                                (filter (match? :team :identified-team) tp))
                               (count tp))))))

(facts "The identification function scores along both the player
        and team dimensions"
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
         (score-player-id labelled-results) => {:player-score (/ 2 3)
                                                :team-score (/ 2 3)}))

(defn run-test
  ([true-tds] (run-test true-tds (test-bot)))
  ([true-tds bot]
   (let [detections (atom ())
         save-detection (fn [td] (swap! detections conj td))
         bot (assoc bot :id-hook save-detection)
         indefinitely (fn [_] true)]
     (bot/main-loop indefinitely bot)
     (let [raw-detections @detections
           results (label-detections raw-detections true-tds)]
       (println (str "Raw detections:" raw-detections))
       (println (str "Results: " results))
       (assoc results
              :detections raw-detections
              :f1-score (score-output results)
              :player-id-score (score-player-id results))))))

(comment (facts "Our touchdown scores 'pretty well' on our test data set from
        the Cowboys vs. Eagles game"
                (let [results (run-test dal-phi-touchdowns)]
                  (fact "We're pretty good at detecting touchdowns"
                        (:f1-score results) => (partial <= (/ 8 9)))
                  (fact "But given a touchdown, we're really good at figuring out who scored"
                        (:player-id-score results) => {:player-score 1
                                                       :team-score 1}))))

(defn run-game [file]
  (let [truth (filter #(= file (:file %)) ground-truth)
        true-tds (map #(select-keys % [:t :player :team]) truth)
        start-time (find-start-time file)
        bot (assoc (test-bot file)
                   :clock (test-clock-with-start start-time))]
    (run-test true-tds bot)))

(defn heavy-test []
  "Run the bot against our entire test-set"
  (let [games (partition-by :file ground-truth)
        bots (map (fn [game]
                    (let [file (:file (first game))
                          start-time (find-start-time file)]
                      (assoc (test-bot file)
                             :clock (test-clock-with-start start-time)))) games)
        ground-truth (map (fn [game]
                            #(select-keys % [:t :player :team]) game)
                          games)]
    (pmap (fn [truth bot]
            (run-test truth bot)) ground-truth bots)))

