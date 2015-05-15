(ns td-bot.loop-test
  (:use midje.sweet)
  (:require [td-bot.bot :as bot]))

(def tweets-1 [{:t 1000 :text "foo"} {:t 3000 :text "touchdown"}])
(def tweets-in-window-1 tweets-1)
(def tweets-2 [{:t 5000 :text "bar"} {:t 8000 :text "baz"}])
(def tweets-in-window-2 (vector (second tweets-2)))

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
  (< i 2))

(facts "About the main loop"
       (fact "If tweets are detected, we log and broadcast them"
             (bot/main-loop twice (test-system)) => {:broadcasted 1 :pending 0}
              (provided
               (test-clock 1 anything) => 5000
               (test-clock 2 anything) => 11000
               (tweet-stream-fn 5000) => tweets-1 :times 1
               (tweet-stream-fn 11000) => tweets-2 :times 1
               (bot/simple-alert first-touchdown) => nil :times 1
               (bot/loop-step 5000
                              tweets-in-window-1
                              nil
                              :td-detector td-detector-fn
                              :scorer-ider anything
                              :id-hook anything) => {:tweet-buffer anything
                                                     :pending [{:detected-at 5000
                                                                :happened-at 3000}]
                                                     :identified nil}
               (bot/loop-step 11000
                              tweets-in-window-2
                              [{:detected-at 5000
                                :happened-at 3000}]
                              :td-detector td-detector-fn
                              :scorer-ider anything
                              :id-hook anything) => {:tweet-buffer anything
                                                     :pending nil
                                                     :identified [first-touchdown]})))

(facts "About each loop step"
       (fact "If we can't identify the scorer, we keep the touchdown as pending"
             (bot/loop-step
              45
              ..tweets..
              nil
              :td-detector td-detector-fn
              :scorer-ider scorer-ider-fn
              :id-hook bot/simple-alert) => {:tweet-buffer ..buffered-tweets..
                                             :identified nil
                                             :pending '({:detected-at 45
                                                         :happened-at 42})}
              (provided
               (bot/tweets-since 15 ..tweets..) => ..buffered-tweets..
               (td-detector-fn ..buffered-tweets..) => [42]
               (bot/tweets-since 42 ..buffered-tweets..) => ..post-td-tweets..
               (scorer-ider-fn ..post-td-tweets..) => nil))
       (fact "If we can identify the scorer, we mark the touchdown as identified"
             (bot/loop-step
              50
              ..tweets..
              '({:detected-at 45
                 :happened-at 42})
              :td-detector td-detector-fn
              :scorer-ider scorer-ider-fn
              :id-hook bot/simple-alert) => {:tweet-buffer ..buffered-tweets..
                                             :identified '({:happened-at 42
                                                            :detected-at 45
                                                            :identified-at 50
                                                            :player "marshawn lynch"
                                                            :team "seattle seahawks"})
                                             :pending nil}
              (provided
               (bot/tweets-since 20 ..tweets..) => ..buffered-tweets..
               (td-detector-fn ..buffered-tweets..) => nil
               (bot/tweets-since 42 ..buffered-tweets..) => ..post-td-tweets..
               (scorer-ider-fn ..post-td-tweets..) => {:player "marshawn lynch"
                                                       :team "seattle seahawks"}))
       (fact "We can detect multiple touchdowns in a single step"
             (bot/loop-step
              60
              ..tweets..
              nil
              :td-detector td-detector-fn
              :scorer-ider scorer-ider-fn
              :id-hook bot/simple-alert) => {:tweet-buffer ..buffered-tweets..
                                               :identified nil
                                               :pending '({:detected-at 60
                                                           :happened-at 35}
                                                          {:detected-at 60
                                                           :happened-at 40})}
              (provided
               (bot/tweets-since 30 ..tweets..) => ..buffered-tweets..
               (bot/tweets-since 35 ..buffered-tweets..) => ..post-td-1-tweets..
               (bot/tweets-since 40 ..buffered-tweets..) => ..post-td-2-tweets..
               (td-detector-fn ..buffered-tweets..) => [35 40]
               (scorer-ider-fn anything) => nil)))
