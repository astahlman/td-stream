(ns td-bot.detection-test
  (:require [clojure.test :refer :all]
            [td-bot.detection :refer :all]
            [td-bot.tweet :refer [json->tweet]]))

;; ##################################################
;;
;; The following test was put in place after the discovery of a nasty
;; bug in which the detection algorithm discarded too much of the
;; tweet buffer so that the invocation on the subsequent clock cycle
;; could receive a buffer of insufficient size and reset the alarm
;; value without running.
;; 
;; Steps to reproduce:
;; 
;; Let W = 100, where W is the width of the buffer.
;; Let t = 103, where t is the current clock tick.
;; 
;; At t = 102, check whether the buffer is wide enough. The first
;; tweet in the buffer has timestamp 1, and the last tweet in the
;; buffer has timestamp 102, and (102 - 1) > 100. Now discard
;; everything before time t - W (= 2), so that the first tweet in the
;; buffer has timestamp 3.
;; 
;; At t = 103, receive no new tweets from the stream. Check that the
;; buffer is wide enough. First tweet has timestamp 3, last tweet has
;; timestamp 102. (102 - 3) < W, So we don't have enough tweets in the
;; buffer to run the detection algorithm.
;;
;; ##################################################

(deftest demarco-murray-example
  (testing "We can detect the first tweet in the DAL-PHI game"
    (let [lines (-> "data/demarco-first-td.txt"
                     clojure.java.io/file
                     slurp
                     (clojure.string/split #"\n"))
          {:keys [detections]} (detect-tds {:alarm-val nil
                                            :tweet-buff (map json->tweet lines)})]
      (is (= 1 (count detections))))))

(deftest ensures-min-buffer-width
  (testing "We ensure that the time range of the tweets in the buffer is at least the width of the detection algorithm window when discarding old tweets"
    (let [tweets (map #(hash-map :t (* 1000 (inc %))) (range 102))
          buff (filter #(not= 2000 (:t %)) tweets)
          [old-window new-window] (partition-window buff)]
      (is (= 1000 (:t (first old-window))))
      (is (=  93000 (:t (first new-window))))
      (is (> (- (:t (last new-window)) (:t (first old-window))) 100000)))))

(deftest tweets->signal
  (testing "How we generate a signal from a stream of tweets. "
    (let [tweets [{:t 1000 :text "swish"}
                  {:t 2000 :text "hole-in-one"}
                  {:t 3000 :text "ace"}
                  {:t 4000 :text "slam-dunk"}
                  {:t 5000 :text "strike"}
                  {:t 6000 :text "this tweet has 7 words including touchdown"}
                  {:t 7000 :text "this tweet is still pretty long so I could be talking about some touchdown that happened a long time ago with 24 words"}
                  {:t 8000 :text "here are lots of words oh and touchdown where lots of equals 13"}
                  {:t 9000 :text "foo"}
                  {:t 10000 :text "bar"}
                  {:t 11000 :text "TOUCHDOWN"}
                  {:t 11000 :text "TOUCHDOWN"}
                  {:t 11000 :text "TOUCHDOWN"}]
          signal (td-signal tweets)]
      (testing "The signal is discretized in intervals of 5000"
        (is (= 3 (count signal))))
      (testing "The amplitude of the signal depends on the average tweet length (in characters) and the frequency of occurrence of the text 'touchdown'"
        (is (= 0 (nth signal 0)))
        (is (= (/ 3 (/ 229 5)) (nth signal 1)))
        (is (= (/ 3 9) (nth signal 2))))))
  (let [tweets [{:t 4000 :text "group 1"}
                {:t 1000 :text "group 1"}
                {:t 5000 :text "group 1"}
                {:t 6000 :text "group 2"}
                {:t 2000 :text "group 1"}
                {:t 7000 :text "group 2"}]
        signal (td-signal tweets)]
    (testing "The tweets get sorted before we discretize into buckets"
      (is (= 2 (count signal))))))

