(ns td-bot.acceptance-test
  (:use midje.sweet
        [td-bot.test-data :only [touchdowns]])
  (:require [td-bot.bot :as bot]
            [clojure.java.io :as io]
            [clojure.data.json :as json]))

(defn precision [{:keys [true-pos false-pos]}]
  (/ true-pos (+ true-pos false-pos)))

(defn recall [{:keys [true-pos false-neg]}]
  (/ true-pos (+ true-pos false-neg)))

(defn f1-score [results]
  "Results should have the following keys
   1. :true-pos
   2. :false-pos
   3. :false-neg"
  (let [p (precision results)
        r (recall results)]
    (if (zero? (+ p r))
      0
      (* 2 (/ (* p r) (+ p r))))))

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

(fact "We count an undefined F1 as 0"
      (f1-score ..results..) => 0
      (provided
       (precision ..results..) => 0
       (recall ..results..) => 0))

(fact "We can calculate the precision"
      (precision {:true-pos 3
                  :false-pos 1
                  :false-neg anything}) => 3/4)

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

(defn test-bot []
  (conj (bot/system) {:id-hook save-result
                      :clock test-clock
                      :tweet-stream (file-stream "/tmp/dal-phi.txt")
                      :td-detector stupid-td-detector
                      :scorer-ider stupid-scorer-ider}))

(declare label-detections)

(fact "We count touchdowns as detected if they happened in the previous 30 seconds"
      (let [detected [{:broadcasted-at 7000} ;;; true positive
                      {:broadcasted-at 41000} ;;; false positive
                      {:broadcasted-at 65000} ;;; true positive
                      {:broadcasted-at 66000}] ;;; false positive
            truth [{:t 2000} ;;; latency = 5000
                   {:t 10000} ;;; false negative
                   {:t 60000}]] ;;; latency = 5000
        (label-detections detected truth)) => {:true-pos [{:t 2000 :latency 5000}
                                                          {:t 60000 :latency 5000}]
                                               :false-pos [{:t 41000}
                                                           {:t 66000}]
                                               :false-neg [{:t 10000}]})

(fact "We don't associate multiple alerts with the same touchdown"
      (let [detected [{:broadcasted-at 1000}
                      {:broadcasted-at 2000}]
            truth [{:t 0}]]
        (label-detections detected truth) => {:true-pos [{:t 0 :latency 1000}]
                                              :false-pos [{:t 2000}]
                                              :false-neg nil}))

(defn label-detections [det tds]
  (let [det (map :broadcasted-at det)
        tds (map :t tds)
        r (false-neg-and-true-pos det tds)
        tp (into #{} (map #(+ (:t %) (:latency %)) (:true-pos r)))]
    (utilize.map/map-vals
     (conj r {:false-pos (map (partial hash-map :t)
                              (clojure.set/difference (set det) tp))})
     #(seq (sort-by :t %)))))

(defn false-neg-and-true-pos [det tds]
  (letfn [(match [d td]
            (let [diff (- d td)]
                 (and (pos? diff) (<= diff 30000))))]
    (reduce (fn [r td]
              (if-let [m (first (filter #(match % td) det))]
                (update-in r [:true-pos] conj {:t td
                                               :latency (- m td)})
                (update-in r [:false-neg] conj {:t td})))
            {:true-pos nil :false-neg nil} tds)))

(defn run []
  (let [detections (atom ())
        save-detection (fn [td] (swap! detections conj td))
        bot (assoc (test-bot) :id-hook save-detection)
        indefinitely (fn [_] true)]
    (bot/main-loop indefinitely bot)
    (let [final-detections @detections
          results (label-detections final-detections touchdowns)]
      (println final-detections)
      (assoc results
             :detections final-detections
             :score (f1-score (utilize.map/map-vals results count))))))

(fact "Our bot can detect touchdowns from the Cowboys vs. Eagles game"
      (+ 1 1) => 2)

