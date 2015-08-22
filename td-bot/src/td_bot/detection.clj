(ns td-bot.detection
  (:require [clojure.test :refer [with-test is]]
            [td-bot.metrics :as metric]
            [clojure.tools.logging :as log])
  (:use [td-bot.tweet :only [is-retweet?]]
        [incanter.charts :only [line-chart]]
        [incanter.core :only [save]]))

(with-test
  (defn mean [xs]
    (/ (reduce + xs) (count xs)))
  (is (= 3 (mean [1 3 5])))
  (is (zero? (mean [0])))
  (is (zero? (mean [-1 0 1])))
  (is (= 4000 (mean [1000 2000 9000]))))

(with-test
  (defn std-dev [xs]
    (let [n (count xs)
          m (mean xs)
          dev (map #(-> (- % m) (Math/pow 2)) xs)]
      (Math/sqrt (mean dev))))
  (is (= 2.0 (std-dev [2 4 4 4 5 5 7 9])))
  (is (= 3.0 (std-dev [3 5 5 8 9 12]))))

(def ^:private sig-interval 5000)
(def ^:private min-buckets 20)
(def ^:private new-window-sz 2)

(with-test
  (defn is-td? [text]
    (re-find #"(?i)touchdown" text))
  (is (is-td? "TOUCHDOWN!!!!"))
  (is (is-td? "That's a touchdown"))
  (is (is-td? "~~~~tOuChDoWn~~~~"))
  (is (not (is-td? "Great TD by Demarco Murray")))
  (is (not (is-td? "Errbody in the club getting tipsy"))))

(defn- roughly? [x y]
  (< (Math/abs (- x y)) 0.001))

(with-test
  (defn- thresh [signal]
    (let [dev (metric/timed :std-dev (std-dev signal))
          avg (mean signal)]
      (+ avg (* 10 dev))))
  (is (= 25.0 (thresh [2 4 4 4 5 5 7 9])))
  (is (roughly? 112.882 (thresh [10 29 38 25 31 14 17 28]))))

(declare make-signal)

(defn detect-tds [{:keys [alarm-val new-tweets signal]}]
  (let [signal (or signal (make-signal nil))
        signal ((:update signal) new-tweets)]
    (if (< (count (:buckets signal)) min-buckets)
      (do
        (println "Not enough!!!")
        {:alarm-val nil
         :signal signal})
      (let
          [[new-window old-window] (split-at new-window-sz (reverse (:buckets signal)))
           cur-v (mean (map :val new-window))
           thresh-v (thresh (map :val old-window))]
        (cond
          (zero? thresh-v) (throw (IllegalArgumentException. "Threshold == 0!"))
          (and alarm-val (<= cur-v alarm-val))
          {:alarm-val nil
           :signal signal}
          (and (nil? alarm-val) (> cur-v thresh-v))
          {:alarm-val thresh-v
           :signal signal
           :detections [(apply max (map :end-t new-window))]} ;;; For now we always return <= 1
          :else
          {:alarm-val alarm-val
           :signal signal})))))

(defn plot-signal [tweets]
  "Visualize the td-signal for the given tweets. Save to disk like this:
   (save (plot-signal $tweets) $filename)"
  (let [signal ((:update (make-signal nil)) tweets)
        points (for [{:keys [start-t val]} (:buckets signal)] [start-t val])
        [x y] ((juxt #(map first %) #(map second %)) points)]
    (line-chart x y)))

(defn make-bucket [{:keys [signal-v total-tds num-tweets total-chars start-t end-t]}]
  {:update (fn [tweets]
             (let [total-tds (+ (count (filter is-td? tweets)) (or total-tds 0))
                   num-tweets (+ (count tweets) (or num-tweets 0))
                   total-chars (reduce + (or total-chars 0) (map count tweets))
                   avg-tweet-len (/ total-chars num-tweets)
                   signal-v (/ total-tds avg-tweet-len)]
               (make-bucket {:signal-v signal-v
                             :total-tds total-tds
                             :num-tweets num-tweets
                             :total-chars total-chars
                             :start-t start-t
                             :end-t end-t})))
   :signal-v signal-v
   :start-t start-t
   :end-t end-t})

(defn make-signal [buckets]
  (let [bucket-start-t #(* sig-interval (int (Math/floor (/ (:t %) sig-interval))))
        take-20-most-recent #(into {} (reverse (take 20 (reverse (into (sorted-map) %)))))
        new-bucket #(make-bucket {:start-t %
                                  :end-t (+ % sig-interval)})]
    {:update (fn [tweets]
               (let [tweets (remove is-retweet? tweets)
                     bucketized (group-by bucket-start-t tweets)
                     updated-buckets (reduce (fn [buckets-to-update [t new-tweets]]
                                               (update-in buckets-to-update
                                                          [t]
                                                          (fn [bucket]
                                                            (let [b (or bucket (new-bucket t))]
                                                              ((:update b) (map :text new-tweets))))))
                                             buckets
                                             bucketized)]
                 (make-signal (take-20-most-recent updated-buckets))))
     ;; TODO: Can probably avoid the sort here, since we do it on the update
     :buckets (for [[t bucket] (into (sorted-map) buckets)] {:start-t t
                                                             :end-t (+ t sig-interval)
                                                             :val (:signal-v bucket)})}))
