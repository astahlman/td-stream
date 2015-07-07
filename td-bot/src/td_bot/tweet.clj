(ns td-bot.tweet
  (:use midje.sweet)
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]))

(defn is-retweet? [tweet]
  (.startsWith (clojure.string/lower-case tweet) "rt"))

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

(defn find-start-time [file]
  "Return the timestamp of the first tweet in the file"
  (let [rdr (io/reader file)
        line (try (.readLine rdr) (catch Exception e) (finally (.close rdr)))
        tweet (json->tweet line)]
    (:t tweet)))

(def ^:private dal-phi-file "data/cowboys-eagles.txt")

(fact "We can get the start time of a stream"
      (find-start-time dal-phi-file) => 1418606430585)
