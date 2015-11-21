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

