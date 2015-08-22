(ns td-bot.bot
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout sliding-buffer]]  ;; Trim...
            [td-bot.detection :as detect]
            [td-bot.identification :as identify]
            [td-bot.metrics :as metric]
            [clojure.tools.logging :as log]))

(declare simple-alert)

(defn system []
  "Returns a new instance of the application."
  (metric/reset-metrics!)
  (hash-map
   :id-hook #(map simple-alert %)
   :clock (fn [_ _] (System/currentTimeMillis))
   :tweet-stream nil
   :td-detector detect/detect-tds
   :scorer-ider identify/identify-scorer))

(defn loop-step [now new-tweets signal pending-id & {:keys [td-detector alarm-val scorer-ider id-hook]}]
  "Takes the current time, a list of tweets, and a buffer of touchdowns
   pending identification, and mark any new touchdowns
   occurring in the last 30 seconds as pending identification. Mark any
   pending touchdowns as identified if their team and player can be identified."
  (let [detection-results (metric/timed :detection (td-detector {:alarm-val alarm-val
                                                                 :new-tweets new-tweets
                                                                 :signal signal}))
        tds (map #(hash-map :happened-at % :detected-at now) (:detections detection-results))
        pending-id (concat tds pending-id)
        maybe-identified (map #(conj %
                                     (scorer-ider (map :text new-tweets)))
                              pending-id)
        identified? #(contains? % :player)
        got-identified (filter identified? maybe-identified)
        still-pending (filter (complement identified?) maybe-identified)
        to-broadcast (map #(assoc % :identified-at now) got-identified)]
    (when (seq to-broadcast)
      (dorun (println (str "broadcasting: " (seq to-broadcast))))
      (dorun (map id-hook to-broadcast)))
    {:signal (:signal detection-results)
     :pending (seq still-pending)
     :identified (seq to-broadcast)
     :alarm-val (:alarm-val detection-results)}))

(defn- simple-alert [{:keys [player team]}]
  (let [msg (str player " just scored a touchdown for the " team ". Hooray!")]
    (println msg)
    msg))

(defn main-loop 
  "Do this constantly until continue? returns false or we run out of tweets"
  ([continue?]
   (main-loop continue? (system)))
  ([continue? {:keys [tweet-stream td-detector scorer-ider id-hook clock]}]
   (log/info "Starting bot...")
   (loop [pending nil
          broadcasted 0
          signal nil
          i 1
          last-time nil
          alarm-val nil]
     (let [now (clock i last-time)
           new-tweets (metric/timed :read-tweets (tweet-stream now))]
       (if (and new-tweets (continue? i))
         (let [results (loop-step
                        now
                        new-tweets
                        signal
                        pending
                        :td-detector td-detector
                        :alarm-val alarm-val
                        :scorer-ider scorer-ider
                        :id-hook id-hook)
               identified (:identified results)
               pending (:pending results)]
           (recur pending
                  (+ broadcasted (count identified))
                  (:signal results)
                  (inc i)
                  now
                  (:alarm-val results)))
         {:broadcasted broadcasted
          :pending (count pending)})))))


