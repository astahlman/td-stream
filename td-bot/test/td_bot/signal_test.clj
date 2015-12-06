(ns td-bot.signal-test
  (:require [td-bot.signal :as signal]
            [clojure.test :refer :all])
  (:import td_bot.signal.DataPoint))

(def sample-bucket-1
  {:num-tweets 5
   :total-chars 437
   :mentions-by-team {:PHI 4
                      :DAL 11}})

(def sample-bucket-2
  {:num-tweets 7
   :total-chars 531
   :mentions-by-team {:PHI 2
                      :DAL 6}})

(def sample-signals
  {5000 sample-bucket-1
   10000 sample-bucket-2})

(defn- subset? [sub super]
  (every? (fn [k] (= (get sub k) (get super k))) (keys sub)))

(deftest test-update-signals
  (testing "We update existing buckets with new tweets"
    (let [{:keys [num-tweets total-chars mentions-by-team]}
          (get (signal/update-signals sample-signals
                                      [{:t 4000
                                        :text "yay the eagles scored a touchdown"}])
               5000)]
      (is (and (= num-tweets 6)
               (= total-chars (+ 437 (count "yay the eagles scored a touchdown")))
               (= 5 (:PHI mentions-by-team))))))
  (testing "We populate new buckets on new tweets"
    (let [tweets [{:t 1000 :text "touchdown eagles"}
                  {:t 2000 :text "yay cowboys touchdown"}
                  {:t 6000 :text "cowboys woohoo touchdown"}]
          signals (signal/create-signals tweets)
          bucket= (fn [exp act]
                    (and (= (:num-tweets exp) (:num-tweets act))
                         (= (:total-chars exp) (:total-chars act))
                         (subset? (:mentions-by-team exp) (:mentions-by-team act))))]
      (is (bucket= {:num-tweets 2
                    :total-chars (+ (count "touchdown eagles")
                                    (count "yay cowboys touchdown"))
                    :mentions-by-team {:DAL 1 :PHI 1}}
                   (get signals 5000)))
      (is (bucket= {:num-tweets 1
                    :total-chars (count "cowboys woohoo touchdown")
                    :mentions-by-team {:DAL 1 :PHI 0}}
                   (get signals 10000))))))

(deftest test-read-signals
  (let [incomplete-bucket {:num-tweets 1
                           :total-chars 10
                           :mentions-by-team {:DAL 1 :PHI 0}}
        signals (assoc sample-signals 15000 incomplete-bucket)]
    (testing "We can read the signal value for a team"
      (is (= [(DataPoint. 5000 (/ 4 (/ 437 5)))
              (DataPoint. 10000 (/ 2 (/ 531 7)))]
             (filter #(pos? (:t %)) (signal/read-signal signals :PHI)))))))
