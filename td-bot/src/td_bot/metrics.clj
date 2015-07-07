(ns td-bot.metrics
  (:use midje.sweet))

(defmacro timed [metrics label & body]
  "Execute body and conj the execution time to an atomic hash-map, 
   metrics, under the given label"
  `(let [start# (System/currentTimeMillis)
         result# ~@body
         t# (- (System/currentTimeMillis) start#)]
     (do (swap! ~metrics (fn [m#]
                           (update-in m# [~label] #(conj % t#))))
         result#)))

(facts "About how we time arbitrary operations"
      (let [metrics (atom {})
            result1 (timed metrics :test-op (do (Thread/sleep 100) (inc 42)))
            result2 (timed metrics :test-op (do (Thread/sleep 20) :foo))]
        (fact "We evaluate the body of a timed operation and return the result"
              (list result1 result2) => '(43 :foo))
        (fact "And we also append timing information to the metrics, front-to-back"
              (count (:test-op @metrics)) => 2
              (second (:test-op @metrics)) => (partial <= 100)
              (first (:test-op @metrics)) => (partial <= 20))))
