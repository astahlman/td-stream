(ns td-bot.twitter-test
  (:require [td-bot.tweet :refer [create-stream]]
            [clojure.java.io :as io])
  (:gen-class))

(defn -main [& args]
  (let [stream (create-stream)
        until (+ (* 1000 60 60 5) (System/currentTimeMillis))]
    (with-open [out (io/writer "/tmp/tweet-test.txt" :append true)]
      (while (<= (System/currentTimeMillis) until)
        (doseq [line ((:read stream) "who cares")]
          (.write out (str line "\n"))))
      ((:close stream)))))
