(ns td-bot.metrics
  (:require [clojure.test :refer :all]
            [td-bot.stats :as stats]
            [clojure.pprint :refer [pprint]]
            [metrics.meters :as meters]
            [metrics.gauges :as gauges]
            [metrics.reporters.csv :as csv]))

(def metrics (atom {}))
(def CR (csv/reporter "/var/log/td-bot" {}))

(defn with-gauge [title val]
  (let [gauge (or (get-in @metrics [:gauges title])
                  (get-in (swap! metrics
                                 (fn [m]
                                   (let [backing-atom (atom nil)]
                                     (update-in m [:gauges]
                                                #(assoc % title {:gauge (gauges/gauge-fn title (fn [] @backing-atom))
                                                                 :backing-atom backing-atom})))))
                          [:gauges title]))]
    (reset! (:backing-atom gauge) val)))


(defn mark-meter! [title]
  (let [met (or (get-in @metrics [:meters title])
                (get-in (swap! metrics (fn [m]
                                         (update-in m [:meters]
                                                    #(assoc % title (meters/meter title)))))
                        [:meters title]))]
    (meters/mark! met)))

(defn reset-metrics! []
  (reset! metrics {})
  (csv/start CR 1))

(defn print-metrics []
  (clojure.pprint/pprint
   (sort-by (comp :total second)
            (let [m @metrics]
              (into {}
                    (for [[label times] (:timers m)]
                      [label {:mean (double (stats/mean times))
                              :median (stats/median times)
                              :n (count times)
                              :max (apply max times)
                              :total (reduce + times)}]))))))

;; We aren't being smart about how many data points to keep,
;; so keeping this on in production would eventually exhaust the heap
(def should-instrument true)

;; TODO: Use the Metrics library for this
(defmacro timed
  "Execute body and conj the execution time to the metrics with the given label"
  [label & body]
  `(let [start# (System/currentTimeMillis)
         result# ~@body
         t# (- (System/currentTimeMillis) start#)]
     (when ~should-instrument
       (do (swap! metrics (fn [m#]
                            (update-in m# [:timers ~label] #(conj % t#))))
           result#))
     result#))

(deftest timed-operations
  (with-redefs [should-instrument true]
    (let [result1 (timed :test-op (do (Thread/sleep 100) (inc 42)))
          result2 (timed :test-op (do (Thread/sleep 20) :foo))]
      (testing "We evaluate the body of a timed operation and return the result"
        (is (= '(43 :foo) (list result1 result2))))
      (testing "And we also append timing information to the metrics, front-to-back"
        (is (= 2 (count (:test-op (:timers (deref metrics))))))
        (is (<= 100 (second (:test-op (:timers (deref metrics))))))
        (is (<= 20 (first (:test-op (:timers (deref metrics))))))))))
