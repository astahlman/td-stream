(ns td-bot.loop-test
  (:use midje.sweet)
  (:require [td-bot.bot :as bot]))

(def tweets-1 [{:t 1000 :text "foo"} {:t 3000 :text "touchdown"}])
(def tweets-2 [{:t 5000 :text "bar"} {:t 8000 :text "baz"}])
(def all-tweets (concat tweets-1 tweets-2))

(def first-touchdown {:detected-at 5000
                      :happened-at 3000
                      :identified-at 11000
                      :player "Marshawn Lynch"
                      :team "Seattle Seahawks"})

(unfinished tweet-stream-fn test-clock td-detector-fn scorer-ider-fn)

(defn test-system []
  (bot/system :tweet-stream tweet-stream-fn
              :td-detector td-detector-fn
              :scorer-ider scorer-ider-fn
              :id-hook bot/simple-alert
              :clock test-clock))

(defn twice [i]
     (<= i 2))

(facts "About the main loop"
       (fact "If tweets are detected, we log and broadcast them"
             (bot/main-loop twice (test-system)) => {:broadcasted 1 :pending 0}
              (provided
               (test-clock 1 anything) => 5000
               (test-clock 2 anything) => 11000
               (test-clock 3 anything) => 15000
               (tweet-stream-fn 5000) => tweets-1 :times 1
               (tweet-stream-fn 11000) => tweets-2 :times 1
               (tweet-stream-fn 15000) => ..whatever.. :times 1
               (bot/loop-step 5000
                              tweets-1
                              nil
                              :td-detector td-detector-fn
                              :scorer-ider anything
                              :id-hook anything) => {:tweet-buffer tweets-1
                                                     :pending [{:detected-at 5000
                                                                :happened-at 3000}]
                                                     :identified nil}
               (bot/loop-step 11000
                              all-tweets
                              [{:detected-at 5000
                                :happened-at 3000}]
                              :td-detector td-detector-fn
                              :scorer-ider anything
                              :id-hook anything) => {:tweet-buffer all-tweets
                                                     :pending nil
                                                     :identified [first-touchdown]})))

(facts "About each loop step"
       (fact "If we can't identify the scorer, we keep the touchdown as pending"
             (bot/loop-step
              45000
              ..tweets..
              nil
              :td-detector td-detector-fn
              :scorer-ider scorer-ider-fn
              :id-hook bot/simple-alert) => {:tweet-buffer ..buffered-tweets..
                                             :identified nil
                                             :pending '({:detected-at 45000
                                                         :happened-at 42000})}
              (provided
               (bot/tweets-since 15000 ..tweets..) => ..buffered-tweets..
               (td-detector-fn ..buffered-tweets..) => [42000]
               (bot/tweets-since 42000 ..buffered-tweets..) => ..post-td-tweets..
               (scorer-ider-fn ..post-td-tweets..) => nil))
       (fact "If we can identify the scorer, we broadcast it and mark the touchdown as identified"
             (bot/loop-step
              50000
              ..tweets..
              '({:detected-at 45000
                 :happened-at 42000})
              :td-detector td-detector-fn
              :scorer-ider scorer-ider-fn
              :id-hook bot/simple-alert) => {:tweet-buffer ..buffered-tweets..
                                             :identified '({:happened-at 42000
                                                            :detected-at 45000
                                                            :identified-at 50000
                                                            :player "marshawn lynch"
                                                            :team "seattle seahawks"})
                                             :pending nil}
              (provided
               (bot/tweets-since 20000 ..tweets..) => ..buffered-tweets..
               (td-detector-fn ..buffered-tweets..) => nil
               (bot/tweets-since 42000 ..buffered-tweets..) => ..post-td-tweets..
               (scorer-ider-fn ..post-td-tweets..) => {:player "marshawn lynch"
                                                       :team "seattle seahawks"}
               (bot/simple-alert {:happened-at 42000
                                  :detected-at 45000
                                  :identified-at 50000
                                  :player "marshawn lynch"
                                  :team "seattle seahawks"}) => nil :times 1))
       (fact "We can detect multiple touchdowns in a single step"
             (bot/loop-step
              60000
              ..tweets..
              nil
              :td-detector td-detector-fn
              :scorer-ider scorer-ider-fn
              :id-hook bot/simple-alert) => {:tweet-buffer ..buffered-tweets..
                                               :identified nil
                                               :pending '({:detected-at 60000
                                                           :happened-at 35000}
                                                          {:detected-at 60000
                                                           :happened-at 40000})}
              (provided
               (bot/tweets-since 30000 ..tweets..) => ..buffered-tweets..
               (bot/tweets-since 35000 ..buffered-tweets..) => ..post-td-1-tweets..
               (bot/tweets-since 40000 ..buffered-tweets..) => ..post-td-2-tweets..
               (td-detector-fn ..buffered-tweets..) => [35000 40000]
               (scorer-ider-fn anything) => nil)))
