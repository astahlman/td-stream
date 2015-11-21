(ns td-bot.bot
  (:require
   [td-bot.detect :as detect]
   [td-bot.signal :as signal]
   [td-bot.tweet :as tweet]
   [td-bot.identification :as identify]
   [td-bot.metrics :as metric]
   [clojure.tools.logging :as log]))

(declare simple-alert)

(defprotocol Clock
  (tick [this])
  (now [this]))

(deftype SystemClock []
  Clock
  (tick [this])
  (now [this] (System/currentTimeMillis)))

(deftype TestClock [t increment]
  Clock
  (tick [this] (TestClock. (+ t increment) increment))
  (now [this] t))

(defn system []
  "Returns a new instance of the application."
  (metric/reset-metrics!)
  (hash-map
   :td-hook simple-alert
   :clock (SystemClock.)
   :tweet-stream (tweet/create-stream)
   :td-detector detect/detect-tds))

(defn- simple-alert [{:keys [team happened-at]}]
  (println (str "TOUCHDOWN!!! By the " team " @ " happened-at ". Hooray!"))
  (metric/mark-meter! "touchdowns"))

(def captured-td-signals (atom []))
(defn- capture-signal [signals td-alerts]
  (swap! captured-td-signals
         (fn [captures]
           (reduce (fn [result {:keys [team happened-at]}]
                     (conj result {:team team
                                   :t happened-at
                                   :signal (signal/read-signal signals team)}))
                   captures
                   td-alerts))))

(defn main-loop 
  "Do this constantly until continue? returns false or we run out of tweets"
  ([]
   (main-loop (constantly true)))
  ([continue?]
   (main-loop continue? (system)))
  ([continue? {:keys [tweet-stream td-detector td-hook clock]}]
   (log/info "Starting bot...")
   (let [{close-stream :close read-stream :read} tweet-stream]
     (def the-td-hook td-hook)
     (loop [detection-log nil
            signals (signal/create-signals)
            clock clock]
       (if-let [new-tweets (and continue?
                                (metric/timed :read-tweets
                                              (read-stream (now clock))))]
         (let [signals (metric/timed :update-signals
                                     (signal/update-signals signals new-tweets))
               extract-new-tds (fn [new-detection-log]
                                 (seq (clojure.set/difference
                                       (into #{} new-detection-log)
                                       (into #{} detection-log))))
               old-count (count detection-log)
               detection-log (metric/timed :detection
                                           (td-detector signals detection-log))
               new-tds (map #(assoc % :identified-at (now clock)) (extract-new-tds detection-log))]

           (doall (map td-hook new-tds))
           (capture-signal signals new-tds)
           (recur detection-log
                  signals
                  (tick clock)))
         (do
           (log/info "Closing bot...")
           (close-stream)
           (def the-detection-log detection-log)
           detection-log))))))
