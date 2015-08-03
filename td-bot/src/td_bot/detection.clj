(ns td-bot.detection
  (:require [clojure.test :refer [with-test is]])
  (:use [td-bot.tweet :only [is-retweet?]]
        [incanter.charts :only [line-chart]]
        [incanter.core :only [save]]))

(with-test
  (defn- mean [xs]
    (/ (reduce + xs) (count xs)))
  (is (= 3 (mean [1 3 5])))
  (is (zero? (mean [0])))
  (is (zero? (mean [-1 0 1])))
  (is (= 4000 (mean [1000 2000 9000]))))

(with-test
  (defn- std-dev [xs]
    (let [n (count xs)
          m (mean xs)
          dev (map #(-> (- % m) (Math/pow 2)) xs)]
      (Math/sqrt (mean dev))))
  (is (= 2.0 (std-dev [2 4 4 4 5 5 7 9])))
  (is (= 3.0 (std-dev [3 5 5 8 9 12]))))

(defn enough? [tweets]
  "We need 100 seconds worth of tweets"
  (let [old-sz (* 1000 90)
        new-sz (* 1000 10)
        latest (apply max (map :t tweets))
        first (apply min (map :t tweets))]
    (>= (- latest first) (+ old-sz new-sz))))

(defn most-recent-tweet-before [t tweets]
  (let [delta (fn [tweet]
               (- t (:t tweet)))]
    (loop [best nil
           [x & xs] tweets]
      (cond
        (not x) best
        (not best) (recur x xs)
        (and (pos? (delta x)) (< (delta x) (delta best))) (recur x xs)
        :else (recur best xs)))))

(defn partition-window [tweets]
  "Split the tweet buffer in two"
  (let [old-sz (* 1000 90)
        new-sz (* 1000 10)
        latest (apply max (map :t tweets))
        old-window-start (- latest new-sz old-sz 1) ;; - 1 because
        ;; window start is exclusive
        old-window-end (- latest new-sz)
        make-win (fn [start end]
                   (filter #(and (> (:t %) start)
                                 (<= (:t %) end)) tweets))]
    [(cons
      (most-recent-tweet-before old-window-start tweets)
      (make-win old-window-start old-window-end))
     (make-win old-window-end latest)]))

(def ^:private sig-interval 5000)

(with-test
  (defn- is-td? [text]
    (re-find #"(?i)touchdown" text))
  (is (is-td? "TOUCHDOWN!!!!"))
  (is (is-td? "That's a touchdown"))
  (is (is-td? "~~~~tOuChDoWn~~~~"))
  (is (not (is-td? "Great TD by Demarco Murray")))
  (is (not (is-td? "Errbody in the club getting tipsy"))))

(defn- bucketize [tweets]
  (->> tweets
      (sort-by :t)
      (partition-by #(Math/ceil (/ (:t %) sig-interval)))))

(defn td-signal [tweets]
  (let [buckets (->> tweets
                       (remove is-retweet?)
                       (bucketize))
        sig-val (fn [bucket]
                  (/ (count (filter #(is-td? (:text %)) bucket))
                     (mean (map #(count (:text %)) bucket))))]
    (map sig-val buckets)))

(defn- roughly? [x y]
  (< (Math/abs (- x y)) 0.001))

(with-test
  (defn- thresh [signal]
    (+ (mean signal) (* 10 (std-dev signal))))
  (is (= 25.0 (thresh [2 4 4 4 5 5 7 9])))
  (is (roughly? 112.882 (thresh [10 29 38 25 31 14 17 28]))))

(defn detect-tds [{:keys [alarm-val tweet-buff]}]
  "Detect touchdowns given a buffer of tweets and the current state
   Return the new state."
  (if (enough? tweet-buff)
    (let [[old-w new-w] (partition-window tweet-buff)
          thresh-v (thresh (td-signal old-w))
          cur-v (mean (td-signal new-w))]
      (do (spit "/tmp/debug.txt"
                (str "t: " (:t (first old-w))
                     ",alarm-val: " alarm-val
                     ",thresh-v: " thresh-v
                     ",cur-v: " (double cur-v)
                     "\n")
                :append true))
      (cond
        (zero? thresh-v) (spit "/tmp/debug.txt"
                               (str "Threshold is 0!\n"
                                    "(first tweet-buff) = " (first tweet-buff)
                                    "\n")
                               :append true)
        (and alarm-val (<= cur-v alarm-val))
        {:alarm-val nil
         :tweet-buff (concat old-w new-w)}
        (and (nil? alarm-val) (> cur-v thresh-v))
        (do (spit "/tmp/debug.txt" "TOUCHDOWN!!!\n" :append true)
            {:alarm-val thresh-v
             :tweet-buff (concat old-w new-w)
             :detections [(:t (first new-w))]}) ;;; For now we always return <= 1
        :else
        {:alarm-val alarm-val
         :tweet-buff (concat old-w new-w)}))
    {:alarm-val nil
     :tweet-buff tweet-buff}))

(defn plot-signal [tweets]
  "Visualize the td-signal for the given tweets. Save to disk like this:
   (save (plot-signal $tweets) $filename)"
  (let [t (map (comp :t first) (bucketize tweets))
        x (map #(Math/ceil (/ % sig-interval)) t)
        y (td-signal tweets)]
    (line-chart x y)))
