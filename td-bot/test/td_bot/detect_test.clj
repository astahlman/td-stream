(ns td-bot.detect-test
  (:require [td-bot.detect :as detect]
            [clojure.test :refer :all])
  (:import td_bot.signal.DataPoint))

(deftest test-detection
  (letfn [(flatline
              [start end]
              (for [t (range start (inc end) 5000)]
                (DataPoint. t 0)))
          (overlay
              [pts base]
              (let [m (into (sorted-map) (for [{:keys [t] :as point} base] [t point]))]
                (vals (reduce (fn [m pt]
                                (assoc m (:t pt) pt))
                              m
                              pts))))
          (big-spike-at
              [t signal]
              (let [t1 t
                    t2 (+ 5000 t)]
                (overlay [(DataPoint. t1 10) (DataPoint. t2 10)] signal)))]
    (testing "We can detect really obvious touchdowns"
      (is (= '({:happened-at 295000
                :team :PIT})
             (detect/detect-in-fixture {:signal (big-spike-at 295000 (flatline 0 300000))
                                        :team :PIT}
                                       {:signal (flatline 0 300000)
                                        :team :CIN}
                                       nil))))
    (testing "We don't detect touchdowns if the signals are flat"
      (is (nil? (detect/detect-in-fixture {:signal (flatline 0 300000)
                                           :team :PIT}
                                          {:signal (flatline 0 300000)
                                           :team :CIN}
                                          nil))))
    (testing "We ignore huge spikes during the freeze period after a touchdown"
      (let [detection-log [{:happened-at 250000
                            :team :PIT}]]
        (is (= detection-log
               (detect/detect-in-fixture {:signal (big-spike-at 295000 (flatline 0 300000))
                                          :team :PIT}
                                         {:signal (flatline 0 300000)
                                          :team :CIN}
                                         detection-log)))))))
