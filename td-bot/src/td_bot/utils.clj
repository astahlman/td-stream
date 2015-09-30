(ns td-bot.utils
  (:require [td-bot.tweet :as tweet]
            [td-bot.detection :as detection]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.data.json :as json]))

(defn ls [dir-name]
  (map #(.getPath %)
       (remove #(= % (clojure.java.io/file dir-name)) (file-seq (clojure.java.io/file dir-name)))))

(defn find-start-time [file]
  "Return the timestamp of the first tweet in the file"
  (let [rdr (io/reader file)
        line (try (.readLine rdr) (catch Exception e) (finally (.close rdr)))
        tweet (tweet/json->tweet line)]
    (:t tweet)))

(def ^:private dal-phi-file "data/cowboys-eagles.txt")
(comment (fact "We can get the start time of a stream"
               (find-start-time dal-phi-file) => 1418606430585))

;; TODO: Shouldn't have to take end-time
(defn load-signal [json-file-name end-time]
  "Create a signal from a raw-tweets.json file"
  (let [stream (tweet/file-stream (clojure.java.io/file json-file-name))
        tweets ((:read stream) end-time)]
    (detection/new-signal tweets)))

(defn dump-signal [signal]
  "Write a signal's buckets as a csv."
  (with-open [out-file (io/writer "/tmp/out.csv")]
                (csv/write-csv out-file
                               (map (comp #(vals (select-keys % [:start-t :val])) #(update-in % [:val] double))
                                    (:buckets signal)))))

(defn raw-tweet-log->json-file [path]
  "Convert the raw-tweets.log produced by the server to the record per line 
   JSON expected by our file-based tweet stream"
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
