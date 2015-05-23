(ns td-bot.bot
  (:use [midje.sweet :only [unfinished]])
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout sliding-buffer]])) ;; Trim...

(declare simple-alert tweets-since)

(defn system
  "Returns a new instance of the application"
  [& {:keys [tweet-stream td-detector scorer-identifier clock]}]
  {:tweet-stream tweet-stream
   :td-detector td-detector
   :scorer-ider scorer-identifier
   :id-hook simple-alert
   :clock clock})

(def default-bot (system))

(unfinished cur-time)

(defn identify-scorer [tweets happened-at scorer-ider]
  "Identify the scoring team and player given a tweet buffer and the 
   time the touchdown occurred"
  (->> tweets
       (tweets-since happened-at)
       (scorer-ider)))

(defn loop-step [now tweets pending-id & {:keys [td-detector scorer-ider id-hook]}]
  "Takes the current time, a list of tweets, and a buffer of touchdowns
   pending identification, and mark any new touchdowns
   occurring in the last 30 seconds as pending identification. Mark any
   pending touchdowns as identified if their team and player can be identified."
  (let [buffered-tweets (tweets-since (- now 30000) tweets)
        new-tds (td-detector buffered-tweets)
        tds (map #(hash-map :happened-at % :detected-at now) new-tds)
        pending-id (concat tds pending-id)
        maybe-identified (map #(conj % 
                               (identify-scorer buffered-tweets 
                                                (:happened-at %)
                                                scorer-ider))
                        pending-id)
        identified? #(contains? % :player)
        got-identified (filter identified? maybe-identified)
        still-pending (filter (complement identified?) maybe-identified)
        to-broadcast (map #(conj % {:identified-at now}) got-identified)]
    (when (seq to-broadcast)
      (dorun (println (str "broadcasting: " (seq to-broadcast))))
      (dorun (map id-hook to-broadcast)))
    {:tweet-buffer buffered-tweets
     :pending (seq still-pending)
     :identified (seq to-broadcast)}))

(defn simple-alert [{:keys [player team]}]
  (let [msg (str player " just scored a touchdown for the " team ". Hooray!")]
    (println msg)
    msg))

(defn tweets-since [t tweets]
  "Take tweets at-or-after t"
  (filter #(>= (:t %) t) tweets))

(defn main-loop 
  "Do this constantly until continue? returns false or we run out of tweets"
  ([continue?]
   (main-loop continue? default-bot))
  ([continue? {:keys [tweet-stream td-detector scorer-ider id-hook clock]}]
   (loop [pending nil
          broadcasted 0
          tweet-buff nil
          i 1
          last-time nil]
     (let [now (clock i last-time)
           ;;;window-start (- now (* 30 1000))
           new-tweets (tweet-stream now)]
       (if (and new-tweets (continue? i))
         (let [tweet-buff (concat tweet-buff new-tweets)
               results (loop-step
                        now
                        tweet-buff
                        pending
                        :td-detector td-detector
                        :scorer-ider scorer-ider
                        :id-hook id-hook)
               identified (:identified results)
               pending (:pending results)]
           (recur pending
                  (+ broadcasted (count identified))
                  (:tweet-buffer results)
                  (inc i)
                  now))
         {:broadcasted broadcasted
          :pending (count pending)})))))
