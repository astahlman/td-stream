(ns td-bot.bot
  (:require
   [td-bot.detect :as detect]
   [td-bot.signal :as signal]
   [td-bot.tweet :as tweet]
   [td-bot.clock :refer [tick now]]
   [td-bot.identification :as identify]
   [td-bot.fixture :as fixture]
   [td-bot.metrics :as metric]
   [metrics.gauges :as gauges]
   [clojure.tools.logging :as log]
   [td-bot.notify :as notify])
  (:import td_bot.clock.SystemClock))

(declare simple-alert)

(defn system
  "Returns a new instance of the application."
  []
  (metric/reset-metrics!)
  (hash-map
   :td-hook (fn [td] (doall (map #(% td) [simple-alert notify/sns-td-hook])))
   :clock (SystemClock.)
   :tweet-stream (tweet/create-stream)
   :td-detector detect/detect-tds))

(defn- simple-alert [{:keys [team happened-at]}]
  (println (str "TOUCHDOWN!!! By the " team " @ " happened-at ". Hooray!"))
  (metric/mark-meter! "touchdowns"))

(def latest-signals (atom nil))
(def signal-gauges
  (letfn [(read-team-signal [team]
            (if-let [signals @latest-signals]
              (-> signals
                  (signal/read-signal team)
                  (last)
                  (:magnitude))))]
    (doall
     (for [team (map :abbrev signal/teams)]
       (gauges/gauge-fn (str "signal-" team)
                        #(read-team-signal (keyword team)))))))

(defn main-loop
  "Do this constantly until continue? returns false or we run out of tweets"
  ([]
   (main-loop (constantly true)))
  ([continue?]
   (main-loop continue? (system)))
  ([continue? {:keys [tweet-stream td-detector td-hook clock]}]
   (log/info "Starting bot...")
   (let [{close-stream :close read-stream :read} tweet-stream]
     (loop [detection-log nil
            signals nil
            clock clock]
       (if-let [new-tweets (and continue?
                                (metric/timed :read-tweets
                                              (read-stream (now clock))))]
         (let [signals (metric/timed :update-signals
                                     (signal/update-signals signals new-tweets))
               extract-new-tds (fn [new-detection-log]
                                 (seq (clojure.set/difference
                                       (set new-detection-log)
                                       (set detection-log))))
               active-fixtures (metric/timed :fixtures
                                             (doall (fixture/active-fixtures-at (now clock))))
               detection-log (metric/timed :detection
                                           (td-detector signals detection-log active-fixtures))
               new-tds (map #(assoc % :identified-at (now clock))
                            (extract-new-tds detection-log))]
           (reset! latest-signals signals)
           (dorun (map td-hook new-tds))
           (recur detection-log
                  (signal/trim-signals signals)
                  (tick clock)))
         (do
           (log/info "Closing bot...")
           (close-stream)
           detection-log))))))
