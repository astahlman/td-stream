(ns td-bot.detection-test
  (:require [clojure.test :refer :all]
            [td-bot.detection :refer :all]
            [td-bot.tweet :refer [json->tweet]]))

(deftest demarco-murray-example
  (testing "We can detect the first tweet in the DAL-PHI game"
    (let [lines (-> "data/demarco-first-td.txt"
                     clojure.java.io/file
                     slurp
                     (clojure.string/split #"\n"))
          tweets (map json->tweet lines)
          {:keys [detections]} (detect-tds {:alarm-val nil
                                            :new-tweets tweets})]
      (is (= 1 (count detections))))))

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
                  {:t 11000 :text "TOUCHDOWN"}
                  {:t 12000 :text "buzzz"}]
          {:keys [buckets]} (new-signal tweets)
          signal-ts (map :val buckets)]
      (testing "The signal is discretized in intervals of 5000"
        (is (= 3 (count signal-ts))))
      (testing "The amplitude of the signal depends on the average tweet length (in characters) and the frequency of occurrence of the text 'touchdown'"
        (is (= 0 (nth signal-ts 0)))
        (is (= (/ 3 (/ 232 5)) (nth signal-ts 1)))
        (is (= (/ 3 7) (nth signal-ts 2))))))
  (let [tweets [{:t 4000 :text "group 1"}
                {:t 1000 :text "group 1"}
                {:t 5000 :text "group 2"}
                {:t 6000 :text "group 2"}
                {:t 2000 :text "group 1"}
                {:t 7000 :text "group 2"}]
        signal (new-signal tweets)]
    (testing "The tweets get sorted before we discretize into buckets"
      (is (= 2 (count (:buckets signal)))))))

