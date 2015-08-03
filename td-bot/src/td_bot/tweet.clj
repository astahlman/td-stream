(ns td-bot.tweet
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [twitter-streaming-client.core :as client]
            [twitter.oauth :as oauth]
            [clojure.tools.logging :as log]
            [utilize.map]))

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

(comment (fact "We can get the start time of a stream"
               (find-start-time dal-phi-file) => 1418606430585))

(def consumer-key "ogGhbolY2La58YANSCbE81LZo")
(def consumer-secret "VnX0eZMuvGGCSMUhS1l6QcnKSOj0UlgCuTieWMxpPIEfht90id")
(def user-access-token "269524233-TBevYjjO9yqfr2zhjrwBY9poxgVQ5fCieJBB6HdR")
(def user-access-token-secret "Io1xqgAyUCgVWufeMCKgmUr5gFj2HTIYN8hfbijac1rA8")

(def creds (oauth/make-oauth-creds consumer-key consumer-secret
                                   user-access-token user-access-token-secret))

(defn- create-tweet-client [keywords]
  "Return a twitter client tracking the given keywords"
  (client/create-twitter-stream
   twitter.api.streaming/statuses-filter
   :oauth-creds creds
   :params {:track (clojure.string/join "," keywords)}))

(defn schedule-repeat [callback ms]
  "Schedule callback to run every ms milliseconds"
  (future (while true (do (Thread/sleep ms) (callback)))))

(defn- format-tweet [t]
  (-> t
      (select-keys [:timestamp_ms :text])
      (clojure.set/rename-keys {:timestamp_ms :t})
      (utilize.map/update :t read-string)))

(defn create-stream
  ([] (create-stream ["touchdown"
                      "TD"
                      "field goal"
                      "FG"
                      "interception"
                      "INT"]))
  ([keywords]
   (let [stream (create-tweet-client keywords)
         closed (atom false)
         buff (atom '())
         poll-job (schedule-repeat
                   (fn []
                     (let [q (client/retrieve-queues stream)]
                       (when-let [err (:error q)]
                         (log/error err))
                       (swap! buff concat (map format-tweet (:tweet q)))))
                   5000)]
     (client/start-twitter-stream stream)
     {:close (fn []
               (do (future-cancel poll-job)
                   (client/cancel-twitter-stream stream)))
      :read (fn [_]
              (let [tweets @buff]
                (do (reset! buff '())
                    tweets)))})))
