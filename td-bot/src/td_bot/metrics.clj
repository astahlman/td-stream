(ns td-bot.metrics
  (:require [clojure.test :refer :all]
            [td-bot.stats :refer [mean median]]
            [metrics.gauges :refer [gauge-fn]]
            [metrics.meters :refer [meter mark!]]
            [metrics.reporters.csv :as csv]))

(def metrics (atom {}))
(def CR (csv/reporter "/tmp/csv_reporter" {}))


(defn register-gauge [title f]
  (let [new-gauge (gauge-fn title f)]
    (swap! metrics (fn [m]
                     (update-in m [:gauges] #(assoc % title new-gauge))))))

(defn mark-meter! [title]
  (let [met (or (get-in @metrics [:meters title])
                (get-in (swap! metrics (fn [m]
                                          (update-in m [:meters]
                                                     #(assoc % title (meter title)))))
                        [:meters title]))]
    (mark! met)))

(defn reset-metrics! []
  (reset! metrics {})
  (csv/start CR 1))

(defn print-metrics []
  (clojure.pprint/pprint
   (sort-by (comp :total second)
            (let [m @metrics]
              (into {}
                    (for [[label times] m]
                      [label {:mean (double (mean times))
                              :median (median times)
                              :n (count times)
                              :max (apply max times)
                              :total (reduce + times)}]))))))
(defmacro timed
  "Execute body and conj the execution time to the metrics with the given label"
  [label & body]
  `(let [start# (System/currentTimeMillis)
         result# ~@body
         t# (- (System/currentTimeMillis) start#)]
     (do (swap! metrics (fn [m#]
                            (update-in m# [:timers ~label] #(conj % t#))))
         result#)))

(deftest timed-operations
  (let [result1 (timed :test-op (do (Thread/sleep 100) (inc 42)))
        result2 (timed :test-op (do (Thread/sleep 20) :foo))]
    (testing "We evaluate the body of a timed operation and return the result"
      (is (= '(43 :foo) (list result1 result2))))
    (testing "And we also append timing information to the metrics, front-to-back"
      (is (= 2 (count (:test-op (deref metrics)))))
      (is (<= 100 (second (:test-op (deref metrics)))))
      (is (<= 20 (first (:test-op (deref metrics))))))))
