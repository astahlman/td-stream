(ns td-bot.acceptance-test
  (:use midje.sweet
        [td-bot.test-data :only [touchdowns]])
  (:require [td-bot.bot :as bot]
            [td-bot.detection :as detect]
            [clojure.java.io :as io]
            [clojure.data.json :as json]))

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

(defn test-clock [_ last-tick]
  "Increments one-second on every tick, or starts at beginning of DAL-PHI game"
  (+ 1000 (or last-tick 1418606922542)))

;; Put our broadcasted touchdowns here
(def results (atom ()))

(defn save-result [tds]
  (map bot/simple-alert tds)
  (swap! results conj tds))

(defn json->tweet [raw-json]
  (->
   raw-json
   (json/read-str :key-fn keyword)
   (clojure.set/rename-keys {:timestamp_ms :t})
   (utilize.map/update :t read-string)))

(defn file-stream [file]
  "Return a function of one-argument (now) which consumes and returns
   all the tweets up until time 'now'. File stream is closed once EOF
   is reached and nil is returned."
  (let [rdr (io/reader file)
        closed (atom false)
        buff (atom [])
        first-pending (first @buff)
        continue? (fn [tweet-t t] (<= tweet-t t))]
    (fn [now]
      (let [first-pending (first @buff)]
        (if (and
             (not (nil? first-pending))
             (< now (:t first-pending)))
          []
          (loop [ret @buff]
            (let [line (try (.readLine rdr) (catch Exception e))
                  tweet (and (not (nil? line)) (json->tweet line))]
              (cond
                @closed nil
                (not tweet) (do
                             (println "closing...")
                             (.close rdr)
                             (reset! closed true)
                             ret)
                (not (continue? (:t tweet) now)) (do
                                                  (reset! buff [tweet])
                                                  ret)
                :else (recur (conj ret tweet))))))))))

(defn stupid-td-detector [tweet-window]
  "1/1000 chance of identifying a touchdown at the beginning of the window"
  (let [is-td? #(zero? (rand-int 100000))]
    (filter
     (complement nil?)
     (map #(if (is-td?) (:t %) nil) tweet-window))))

(defn stupid-scorer-ider [post-td-tweets]
  {:player "demarco murray"
   :team "dallas cowboys"})

(def test-file "/Users/astahlman/Documents/Programming/ML/td-stream/data/json/raw/tweets.2014-12-15.01.json")

(defn test-bot []
  (conj (bot/system) {:id-hook save-result
                      :clock test-clock
                      :tweet-stream (file-stream test-file)
                      :td-detector detect/detect-tds
                      :scorer-ider stupid-scorer-ider}))

(declare false-neg-and-true-pos)

(defn label-detections [det tds]
  (println (str "Detections: " (into [] det)))
  (let [det (map :identified-at det)
        tds (map :t tds)
        r (false-neg-and-true-pos det tds)
        tp (into #{} (map #(+ (:t %) (:latency %)) (:true-pos r)))]
    (utilize.map/map-vals
     (conj r {:false-pos (map (partial hash-map :t)
                              (clojure.set/difference (set det) tp))})
     #(seq (sort-by :t %)))))

(defn false-neg-and-true-pos [det tds]
  (println (str "Detections: " (into [] det) "\nTds: " (into [] tds)))
  (letfn [(match [d td]
            (let [diff (- d td)]
                 (and (pos? diff) (<= diff 30000))))]
    (reduce (fn [r td]
              (if-let [m (first (filter #(match % td) det))]
                (update-in r [:true-pos] conj {:t td
                                               :latency (- m td)})
                (update-in r [:false-neg] conj {:t td})))
            {:true-pos nil :false-neg nil} tds)))

(fact "We count touchdowns as detected if they happened in the previous 30 seconds"
      (let [detected [{:identified-at 7000} ;;; true positive
                      {:identified-at 41000} ;;; false positive
                      {:identified-at 65000} ;;; true positive
                      {:identified-at 66000}] ;;; false positive
            truth [{:t 2000} ;;; latency = 5000
                   {:t 10000} ;;; false negative
                   {:t 60000}]] ;;; latency = 5000
        (label-detections detected truth)) => {:true-pos [{:t 2000 :latency 5000}
                                                          {:t 60000 :latency 5000}]
                                               :false-pos [{:t 41000}
                                                           {:t 66000}]
                                               :false-neg [{:t 10000}]})

(fact "We don't associate multiple alerts with the same touchdown"
      (let [detected [{:identified-at 1000}
                      {:identified-at 2000}]
            truth [{:t 0}]]
        (label-detections detected truth) => {:true-pos [{:t 0 :latency 1000}]
                                              :false-pos [{:t 2000}]
                                              :false-neg nil}))

(defn score-output [out]
  "out has keys :true-pos, :false-pos, and :false-neg"
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

(defn- score-player-id [ground-truth detections]
  "Return the ratio of correct identifications for player and team"
  nil)

(facts "The identification function scores along both the player
        and team dimensions"
       (let [ground-truth [{:team "dallas cowboys"
                            :player "demarco murray"
                            :t 1000}
                           {:team "pittsburgh steelers"
                            :player "antonio brown"
                            :t 7000}
                           {:team "seattle seahawks"
                            :player "marshawn lynch"
                            :t 19000}]
             detections [{:team "dallas cowboys"
                          :player "dez bryant"
                          :detected-at 1500}
                         {:team "miami dolphins"
                          :player "antonio brown"
                          :detected-at 15000}
                         {:team "seattle seahawks"
                          :player "marshawn lynch"
                          :detected-at 21000}]]
         (score-player-id ground-truth detections) => {:player-score (/ 2 3)
                                                       :team-score (/ 2 3)}))


(facts "Our touchdown scores 'pretty well' on our test data set from
        the Cowboys vs. Eagles game"
       (let [results (run)]
         (:f1-score results) => (partial <= (/ 8 9))))

(def ^:private test-file "/Users/astahlman/Documents/Programming/ML/td-stream/td-bot/data/demarco-first-td.txt")

(defn run []
  (let [detections (atom ())
        save-detection (fn [td] (swap! detections conj td))
        bot (assoc (test-bot) :id-hook save-detection)
        indefinitely (fn [_] true)]
    (bot/main-loop indefinitely bot)
    (let [raw-detections @detections
          results (label-detections raw-detections touchdowns)]
      (println (str "Raw detections:" raw-detections))
      (println (str "Results: " results))
      (assoc results
             :detections raw-detections
             :f1-score (score-output results)))))


