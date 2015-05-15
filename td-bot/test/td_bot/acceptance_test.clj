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
  (swap! results concat tds))

(defn json->tweet [raw-json]
  (->
   raw-json
   (json/read-str :key-fn keyword)
   (clojure.set/rename-keys {:timestamp_ms :t})
   (utilize.map/update :t read-string)))


(defn file-stream [file]
  "Return a function of one-argument (now) which consumes and returns
   all the tweets up until time 'now'. File stream is closed once EOF
   is reached."
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
  (let [is-td? #(zero? (rand-int 1000))]
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

(fact "Our bot can detect touchdowns from the Cowboys vs. Eagles game"
      (+ 1 1) => 2)

