(ns td-bot.bot
  (:use [midje.sweet :only [unfinished]])
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout sliding-buffer]])) ;; Trim...

(declare simple-alert)

(defn system
  "Returns a new instance of the application"
  [& {:keys [tweet-stream td-detector scorer-identifier]}]
  {:tweet-stream tweet-stream
   :td-detector td-detector
   :scorer-ider scorer-identifier
   :id-hook simple-alert})

(def default-bot system)

(unfinished td-detector tweets-since scorer-ider read-tweets cur-time)

(defn identify-scorer [tweets happened-at]
  "Identify the scoring team and player given a tweet buffer and the 
   time the touchdown occurred"
  (->> tweets
       (tweets-since happened-at)
       (scorer-ider)))

(defn loop-step [now tweets pending-id]
  "Takes the current time, a list of tweets, and a buffer of touchdowns
   pending identification, and mark any new touchdowns
   occurring in the last 30 seconds as pending identification. Mark any
   pending touchdowns as identified if their team and player can be identified."
  (let [buffered-tweets (tweets-since (- now 30) tweets)
        new-tds (td-detector buffered-tweets)
        tds (map #(hash-map :happened-at % :detected-at now) new-tds)
        pending-id (concat tds pending-id)
        maybe-identified (map #(conj % 
                               (identify-scorer buffered-tweets 
                                                (:happened-at %)))
                        pending-id)
        identified? #(contains? % :player)
        got-identified (filter identified? maybe-identified)
        still-pending (filter (complement identified?) maybe-identified)
        to-broadcast (map #(conj % {:identified-at now}) got-identified)]
    (when (seq to-broadcast)
      (dorun (map simple-alert to-broadcast)))
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
  "Do this constantly until continue? returns false"
  ([continue?]
   (main-loop continue? default-bot))
  ([continue? {:keys [tweet-stream td-detector scorer-ider id-hook]}]
   (loop [pending nil
          broadcasted 0
          tweets nil
          i 1]
     (let [now (cur-time i)
           window-start (- now 5000)
           tweets (concat tweets (read-tweets now))
           tweet-buffer (tweets-since window-start tweets)
           results (loop-step 
                    now
                    tweet-buffer
                    pending)
           identified (:identified results)
           pending (:pending results)]
       (when (seq identified)
         (dorun (map simple-alert identified)))
       (if (continue? i)
         (recur pending
                (+ broadcasted (count identified))
                tweet-buffer
                (inc i))
         {:broadcasted (+ broadcasted (count identified))
          :pending (count pending)})))))
