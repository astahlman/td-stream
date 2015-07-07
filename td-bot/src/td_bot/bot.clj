(ns td-bot.bot
  (:use [midje.sweet :only [unfinished]])
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout sliding-buffer]]  ;; Trim...
            [td-bot.detection :as detect]
            [td-bot.identification :as identify]
            [td-bot.metrics :as metric]))

(declare simple-alert tweets-since)

(defn system []
  "Returns a new instance of the application."
  (hash-map
   :id-hook #(map simple-alert %)
   :clock (fn [_ _] (System/currentTimeMillis))
   :tweet-stream nil
   :td-detector detect/detect-tds
   :scorer-ider identify/identify-scorer
   :metrics (atom {})))

(defn identify-scorer [tweets happened-at scorer-ider]
  "Identify the scoring team and player given a tweet buffer and the 
   time the touchdown occurred"
  (->> tweets
       (tweets-since happened-at)
       (map :text)
       (scorer-ider)))

(defn loop-step [now tweets pending-id & {:keys [td-detector alarm-val scorer-ider id-hook metrics]}]
  "Takes the current time, a list of tweets, and a buffer of touchdowns
   pending identification, and mark any new touchdowns
   occurring in the last 30 seconds as pending identification. Mark any
   pending touchdowns as identified if their team and player can be identified."
  (let [detection-results (metric/timed metrics :detection (td-detector {:alarm-val alarm-val
                                                                         :tweet-buff tweets}))
        tweet-buff (:tweet-buff detection-results)
        tds (map #(hash-map :happened-at % :detected-at now) (:detections detection-results))
        pending-id (concat tds pending-id)
        maybe-identified (map #(conj % 
                                     (metric/timed metrics :identification (identify-scorer tweet-buff
                                                                                            (:happened-at %)
                                                                                            scorer-ider)))
                              pending-id)
        identified? #(contains? % :player)
        got-identified (filter identified? maybe-identified)
        still-pending (filter (complement identified?) maybe-identified)
        to-broadcast (map #(assoc % :identified-at now) got-identified)]
    (when (seq to-broadcast)
      (dorun (println (str "broadcasting: " (seq to-broadcast))))
      (dorun (map id-hook to-broadcast)))
    {:tweet-buff tweet-buff
     :pending (seq still-pending)
     :identified (seq to-broadcast)
     :alarm-val (:alarm-val detection-results)}))

(defn simple-alert [{:keys [player team]}]
  (let [msg (str player " just scored a touchdown for the " team ". Hooray!")]
    (println msg)
    msg))

;; TODO: This is duplicated in detection...
(defn tweets-since [t tweets]
  "Take tweets at-or-after t"
  (filter #(>= (:t %) t) tweets))

(defn main-loop 
  "Do this constantly until continue? returns false or we run out of tweets"
  ([continue?]
   (main-loop continue? (system)))
  ([continue? {:keys [tweet-stream td-detector scorer-ider id-hook clock metrics]}]
   (loop [pending nil
          broadcasted 0
          tweet-buff nil
          i 1
          last-time nil
          alarm-val nil]
     (let [now (clock i last-time)
           new-tweets (metric/timed metrics :read-tweets (tweet-stream now))]
       (if (and new-tweets (continue? i))
         (let [tweet-buff (concat tweet-buff new-tweets)
               results (loop-step
                        now
                        tweet-buff
                        pending
                        :td-detector td-detector
                        :alarm-val alarm-val
                        :scorer-ider scorer-ider
                        :id-hook id-hook
                        :metrics metrics)
               identified (:identified results)
               pending (:pending results)]
           (recur pending
                  (+ broadcasted (count identified))
                  (:tweet-buff results)
                  (inc i)
                  now
                  (:alarm-val results)))
         {:broadcasted broadcasted
          :pending (count pending)})))))


