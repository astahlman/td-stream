(ns td-bot.detection-test
  (:use midje.sweet
        td-bot.detection))

(comment
"The following test was put in place after the discovery of a nasty bug in which the detection algorithm discarded too much of the tweet buffer so that the invocation on the subsequent clock cycle could receive a buffer of insufficient size and reset the alarm value without running.

Steps to reproduce:

Let W = 100, where W is the width of the buffer.
Let t = 103, where t is the current clock tick.

At t = 102, check whether the buffer is wide enough. The first tweet in the buffer has timestamp 1, and the last tweet in the buffer has timestamp 102, and (102 - 1) > 100. Now discard everything before time t - W (= 2), so that the first tweet in the buffer has timestamp 3.

At t = 103, receive no new tweets from the stream. Check that the buffer is wide enough. First tweet has timestamp 3, last tweet has timestamp 102. (102 - 3) < W, So we don't have enough tweets in the buffer to run the detection algorithm.")

(fact "We ensure that the time range of the tweets in the buffer is at least the width of the detection algorithm window when discarding old tweets"
      (let [tweets (map #(hash-map :t (* 1000 (inc %))) (range 102))
            buff (filter #(not= 2000 (:t %)) tweets)
            [old-window new-window] (partition-window buff)]
        (:t (first old-window)) => 1000
        (:t (first new-window)) => 93000
        (- (:t (last new-window)) (:t (first old-window))) => #(> % 10000)))

