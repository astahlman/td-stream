(ns td-bot.detection
  (:use [midje.sweet]
        [incanter.charts :only [line-chart]]))

(defn- mean [xs]
     (/ (reduce + xs) (count xs)))

(defn- std-dev [xs]
  (let [n (count xs)
        m (mean xs)
        dev (map #(-> (- % m) (Math/pow 2)) xs)]
    (Math/sqrt (mean dev))))

(facts "We can calculate averages"
       (fact (mean [1 3 5]) => 3)
       (fact (mean [0]) => 0)
       (fact (mean [-1 0 1]) => 0)
       (fact (mean [1000 2000 9000]) => 4000))

(facts "We can calculate standard deviations"
       (fact (std-dev [2 4 4 4 5 5 7 9]) => 2.0)
       (fact (std-dev [3 5 5 8 9 12]) => 3.0))

(defn- enough? [tweets]
  "We need 100 seconds worth of tweets"
  (let [old-sz (* 1000 90)
        new-sz (* 1000 10)
        latest (apply max (map :t tweets))
        first (apply min (map :t tweets))]
    (>= (- latest first) (+ old-sz new-sz))))

;; TODO: Don't need to do this twice (see enough?)
(defn- partition-window [tweets]
  "Split the tweet buffer in two"
  (let [old-sz (* 1000 90)
        new-sz (* 1000 10)
        latest (apply max (map :t tweets))
        make-win (fn [start end]
                   (filter #(and (> (:t %) start)
                                 (<= (:t %) end)) tweets))]
    [(make-win (- latest new-sz old-sz) (- latest new-sz))
     (make-win (- latest new-sz) latest)]))

(def ^:private sig-interval 5000)

(defn- is-td? [text]
  (re-find #"(?i)touchdown" text))

(facts "About what text counts as a touchdown"
       (is-td? "TOUCHDOWN!!!!") => truthy
       (is-td? "That's a touchdown") => truthy
       (is-td? "~~~~tOuChDoWn~~~~") => truthy
       (is-td? "Great TD by Demarco Murray") => falsey
       (is-td? "Errbody in the club getting tipsy") => falsey)

(defn- bucketize [tweets]
  (->> tweets
      (sort-by :t)
      (partition-by #(Math/ceil (/ (:t %) sig-interval)))))

(defn- td-signal [tweets]
  (let [intervals (bucketize tweets)
        sig-val (fn [interval]
                  (/ (count (filter #(is-td? (:text %)) interval))
                     (mean (map #(count (:text %)) interval))))]
    (map sig-val intervals)))

(facts "About how we generate a signal from a stream of tweets"
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
         (fact "The signal is discretized in intervals of 5000"
               (count signal) => 3)
         (fact "The amplitude of the signal depends on the average tweet length (in characters) and the frequency of occurrence of the text 'touchdown'"
               (nth signal 0) => 0
               (nth signal 1) => (/ 3 (/ 229 5))
               (nth signal 2) => (/ 3 9)))
       (let [tweets [{:t 4000 :text "group 1"}
                     {:t 1000 :text "group 1"}
                     {:t 5000 :text "group 1"}
                     {:t 6000 :text "group 2"}
                     {:t 2000 :text "group 1"}
                     {:t 7000 :text "group 2"}]
             signal (td-signal tweets)]
         (fact "The tweets get sorted before we discretize into buckets"
               (count signal) => 2)))

(defn- thresh [signal]
  (+ (mean signal) (* 10 (std-dev signal))))

(facts "About our threshold calculation"
       (fact (thresh [2 4 4 4 5 5 7 9]) => 25.0)
       (fact (thresh [10 29 38 25 31 14 17 28]) => (roughly 112.882)))

(defn- tweets-since [t tweets]
  "Take tweets at-or-after t"
  (filter #(>= (:t %) t) tweets))

(defn detect-tds [{:keys [alarm-val tweet-buff]}]
  "Detect touchdowns given a buffer of tweets and the current state
   Return the new state."
  (if (enough? tweet-buff)
    (let [[old-w new-w] (partition-window tweet-buff)
          thresh-v (thresh (td-signal old-w))
          cur-v (mean (td-signal new-w))]
      (cond
        (and alarm-val (<= cur-v alarm-val))
        {:alarm-val nil
         :tweet-buff (concat old-w new-w)}
        (and (nil? alarm-val) (> cur-v thresh-v))
        {:alarm-val thresh-v
         :tweet-buff (concat old-w new-w)
         :detections [(:t (first new-w))]} ;;; For now we always return <= 1
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
