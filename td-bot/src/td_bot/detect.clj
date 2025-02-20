(ns td-bot.detect
  (:require [clojure.test :refer :all]
            [td-bot.metrics :as metrics]
            [clojure.tools.logging :as log]
            [td-bot.stats :as stats]
            [td-bot.signal :as signal])
  (:import [td_bot.signal DataPoint]))

(def ^:const old-window-sz 36) ;; 36 data points
(def ^:const new-window-sz 2) ;; 2 data points
(def ^:const detection-freeze-ms 180000) ;; three minutes

(with-test
  (defn- thresh
    ([xs] (thresh xs 6 0.05))
    ([xs multiplier min-val]
     (let [m (stats/median xs)
           abs-dev-from-median (map #(Math/abs ^Double (- % m)) (map float xs))]
       (max min-val
            (+ m (* multiplier (stats/median abs-dev-from-median)))))))
  (testing "Our threshold for a series is the median plus the median absolute deviation times 6"
    (is (= 9.0 (thresh [1 2 3 4 5]))))
  (testing "The threshold can't be below some lower-bound (0.05 by default)"
    (is (= 0.05 (thresh [0 0 1])))))

(declare detect-in-fixture)
(declare find-td)

(defn detect-tds [signals detection-log active-fixtures]
  (reduce (fn [detection-log {:keys [home away]}]
            (metrics/timed :detect-in-fixture
                           (detect-in-fixture {:team home
                                               :signal (metrics/timed :read-home
                                                                      (signal/read-signal signals home))}
                                              {:team away
                                               :signal (metrics/timed :read-away
                                                                      (signal/read-signal signals away))}
                                              detection-log)))
          detection-log
          active-fixtures))

(defn detect-in-fixture [home away detection-log]
  (let [relevant-detections (filter #(#{(:team home) (:team away)} (:team %)) detection-log)
        now (last (sort (map :t (concat (:signal home) (:signal away)))))
        last-td (last (sort (map :happened-at relevant-detections)))
        time-since-last-td (if (and now last-td) (- now last-td))
        in-freeze? (and time-since-last-td (<= time-since-last-td detection-freeze-ms))]
    (if-let [new-td (and (not in-freeze?) (find-td home away))]
      (conj detection-log new-td)
      detection-log)))

(with-test
  (defn- noisier [home away]
    (map #(if (> %1 %2) :home :away) home away))
  (is (= [:home :home :away]
         (noisier [10 20 3]
                  [1 2 30]))))

(with-test
  (defn split-window [sig]
    (let [[new-window old-window] (map reverse (split-at new-window-sz (reverse sig)))]
      [(take old-window-sz old-window) new-window]))
  (is (= [(range 1 35) [35 36]]
         (split-window (range 1 37)))))

(with-test
  (defn the-only
    [xs]
    (let [s (set xs)]
      (if (= 1 (count s))
        (first s))))
  (is (= :a (the-only [:a :a :a :a])))
  (is (nil? (the-only [:a :a :a :b]))))

;; We make a simplifying assumption here, which is:
;; No touchdown happened more than one data point ago.
;; That could turn out to be totally wrong.
(defn- find-td
  "Takes two team-signals and returns a touchdown or nil,
   where a touchdown has keys [:happened-at :team]"
  [home away]
  (if (and (>= (count (:signal home)) (+ old-window-sz new-window-sz))
           (>= (count (:signal away)) (+ old-window-sz new-window-sz)))
    (let [home-sig (:signal home)
          away-sig (:signal away)
          [home-old-window home-new-window] (split-window home-sig)
          [away-old-window away-new-window] (split-window away-sig)]
      (if-let [noisy-team (the-only (noisier
                                     (map :magnitude home-new-window)
                                     (map :magnitude away-new-window)))]
        (cond
          (and (= :home noisy-team)
               (every? #(>= % (thresh (map :magnitude home-old-window)))
                       (map :magnitude home-new-window)))
          {:team (:team home)
           :happened-at (:t (first home-new-window))}
          
          (and (= :away noisy-team)
               (every? #(>= % (thresh (map :magnitude away-old-window)))
                       (map :magnitude away-new-window)))
          {:team (:team away)
           :happened-at (:t (first away-new-window))})))))
