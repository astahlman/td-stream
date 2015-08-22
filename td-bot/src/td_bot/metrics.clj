(ns td-bot.metrics
  (:require [clojure.test :refer :all]
            [td-bot.detection :refer [mean]]))

(def ^{:dynamic true} *metrics* (atom {}))

(defn reset-metrics! []
  (reset! *metrics* {}))

(defn print-metrics []
  (clojure.pprint/pprint
   (sort-by (comp :total second)
            (let [m @td-bot.metrics/*metrics*]
              (into {}
                    (for [[label times] m]
                      [label {:mean (double (mean times))
                              :total (reduce + times)}]))))))
(defmacro timed
  "Execute body and conj the execution time to the metrics with the given label"
  [label & body]
  `(let [start# (System/currentTimeMillis)
         result# ~@body
         t# (- (System/currentTimeMillis) start#)]
     (do (swap! *metrics* (fn [m#]
                            (update-in m# [~label] #(conj % t#))))
         result#)))

(deftest timed-operations
  (let [result1 (timed :test-op (do (Thread/sleep 100) (inc 42)))
        result2 (timed :test-op (do (Thread/sleep 20) :foo))]
    (testing "We evaluate the body of a timed operation and return the result"
      (is (= '(43 :foo) (list result1 result2))))
    (testing "And we also append timing information to the metrics, front-to-back"
      (is (= 2 (count (:test-op (deref *metrics*)))))
      (is (<= 100 (second (:test-op (deref *metrics*)))))
      (is (<= 20 (first (:test-op (deref *metrics*))))))))
