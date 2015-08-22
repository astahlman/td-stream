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
                              :alarm-val nil
                              :scorer-ider anything
                              :id-hook anything) => {:tweet-buff tweets-1
                                                     :alarm-val 13.0
                                                     :pending [{:detected-at 5000
                                                                :happened-at 3000}]
                                                     :identified nil}
               (bot/loop-step 11000
                              all-tweets
                              [{:detected-at 5000
                                :happened-at 3000}]
                              :td-detector td-detector-fn
                              :alarm-val 13.0
                              :scorer-ider anything
                              :id-hook anything) => {:tweet-buff all-tweets
                                                     :alarm-val 13.0
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
              :id-hook bot/simple-alert) => {:tweet-buff ..tweet-buff..
                                             :alarm-val 13.0
                                             :identified nil
                                             :pending '({:detected-at 45000
                                                         :happened-at 42000})}
              (provided
               (td-detector-fn {:alarm-val nil
                                :tweet-buff ..tweets..
                                }) => {:alarm-val 13.0
                                       :tweet-buff ..tweet-buff..
                                       :detections [42000]}
               (bot/tweets-since 42000 ..tweet-buff..) => ..post-td-tweets..
               (scorer-ider-fn ..post-td-tweets..) => nil))
       
       (fact "If we can identify the scorer, we broadcast it and mark the touchdown as identified"
             (bot/loop-step
              50000
              ..tweets..
              '({:detected-at 45000
                 :happened-at 42000})
              :td-detector td-detector-fn
              :alarm-val 13.0
              :scorer-ider scorer-ider-fn
              :id-hook bot/simple-alert) => {:tweet-buff ..tweet-buff..
                                             :identified '({:happened-at 42000
                                                            :detected-at 45000
                                                            :identified-at 50000
                                                            :player "marshawn lynch"
                                                            :team "seattle seahawks"})
                                             :pending nil
                                             :alarm-val 13.0}
              (provided
               (td-detector-fn {:alarm-val 13.0
                                :tweet-buff ..tweets..}) => {:alarm-val 13.0
                                                             :tweet-buff ..tweet-buff..}
               (bot/tweets-since 42000 ..tweet-buff..) => ..post-td-tweets..
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
              :id-hook bot/simple-alert) => {:tweet-buff ..tweet-buff..
                                             :identified nil
                                             :pending '({:detected-at 60000
                                                         :happened-at 35000}
                                                        {:detected-at 60000
                                                         :happened-at 40000})
                                             :alarm-val ..positive-number..}
              (provided
               (bot/tweets-since 35000 ..tweet-buff..) => ..post-td-1-tweets..
               (bot/tweets-since 40000 ..tweet-buff..) => ..post-td-2-tweets..
               (td-detector-fn {:alarm-val nil
                                :tweet-buff ..tweets..}) => {:alarm-val ..positive-number..
                                                             :tweet-buff ..tweet-buff..
                                                             :detections [35000 40000]}
               (scorer-ider-fn anything) => nil)))
