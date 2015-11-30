(ns td-bot.tweet
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [twitter-streaming-client.core :as client]
            [twitter.oauth :as oauth]
            [clojure.tools.logging :as log]
            [utilize.map]))

(defn is-retweet? [tweet]
  (.startsWith (clojure.string/lower-case tweet) "rt"))

(defn is-touchdown? [tweet]
  (re-find #"(?i)touchdown" tweet))

(defn json->tweet [raw-json]
  (->
   raw-json
   (json/read-str :key-fn keyword)
   (clojure.set/rename-keys {:timestamp_ms :t})
   (utilize.map/update :t read-string)))

(defn ls [dir-name]
  (map #(.getPath %)
       (remove #(= % (clojure.java.io/file dir-name)) (file-seq (clojure.java.io/file dir-name)))))

(defn raw-tweet-log->json-file [path]
  (let [lines (clojure.string/split (slurp path) #"\n")
        start-of-tweets-re #"\[\{.*"
        line->tweets #(read-string (re-find start-of-tweets-re %))
        tweet->json (fn [tweet] (->
                                tweet
                                (clojure.set/rename-keys {:t :timestamp_ms})
                                (utilize.map/update :timestamp_ms str)
                                (json/write-str)))
        content (reduce #(concat %1 (line->tweets %2)) [] lines)]
    (spit
     (str path ".json")
     (clojure.string/join "\n" (map tweet->json content))
     :append true)))

(defn file->tweets
  "Load all the tweets from a file (for debugging)"
  [file]
  (let [lines (-> file
                  (slurp)
                  (clojure.string/split #"\n"))]
    (map td-bot.tweet/json->tweet lines)))

(defn file-stream [file]
  "Return a function of one-argument (now) which consumes and returns
   all the tweets up until time 'now'. File stream is closed once EOF
   is reached and nil is returned."
  (let [rdr (io/reader file)
        closed (atom false)
        buff (atom [])
        first-pending (first @buff)
        continue? (fn [tweet-t t] (<= tweet-t t))
        close #(do
                 (println "Closing stream...")
                 (.close rdr)
                 (reset! closed true))]
    {:close close
     :read (fn [now]
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
                       (not tweet) (do (close) ret)
                       (not (continue? (:t tweet) now))
                       (do
                         (reset! buff [tweet])
                         ret)
                       :else (recur (conj ret tweet))))))))}))



(defn create-creds []
  (let [{:keys [consumer-key consumer-secret user-access-token user-access-token-secret]}
        (json/read-str (slurp "keys.json") :key-fn (comp keyword #(clojure.string/replace % #"_" "-")))]
    (oauth/make-oauth-creds
     consumer-key
     consumer-secret
     user-access-token
     user-access-token-secret)))

(defn create-tweet-client [keywords]
  "Return a twitter client tracking the given keywords"
  (client/create-twitter-stream
   twitter.api.streaming/statuses-filter
   :oauth-creds (create-creds)
   :params {:track (clojure.string/join "," keywords)}))

(defn schedule-repeat [callback ms]
  "Schedule callback to run every ms milliseconds"
  (future (while true (do (Thread/sleep ms) (callback)))))

(defn- format-tweet [t]
  (-> t
      (select-keys [:timestamp_ms :text])
      (clojure.set/rename-keys {:timestamp_ms :t})
      (utilize.map/update :t read-string)))

(defn log-tweets [tweets]
  (when (seq tweets)
    (log/log "tweetsLogger" :info nil (str (vec tweets)))))

(defn create-stream
  ([] (create-stream ["touchdown"]))
  ([keywords]
   (let [stream (create-tweet-client keywords)
         closed (atom false)
         buff (atom '())
         poll-interval-millis 5000
         poll-job (schedule-repeat
                   (fn []
                     (let [q (client/retrieve-queues stream)]
                       (when-let [err (:error q)]
                         (log/error err))
                       (swap! buff concat (map format-tweet (:tweet q)))))
                   poll-interval-millis)]
     (client/start-twitter-stream stream)
     {:close (fn []
               (do (future-cancel poll-job)
                   (client/cancel-twitter-stream stream)))
      :read (fn [_]
              (let [tweets @buff]
                (do
                  (reset! buff '())
                  (log-tweets tweets)
                    tweets)))})))
