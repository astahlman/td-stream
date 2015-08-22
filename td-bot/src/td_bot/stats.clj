(ns td-bot.stats
  (:require [clojure.test :refer :all]))

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

(with-test
  (defn median [xs]
    (loop [x (sort < xs)
           i 1]
      (cond (= i (count x))
            (first x)                
            (= i (dec (count x)))
            (/ (+ (first x) (second x)) 2)
            :else
            (recur (rest x) (inc i)))))
  (is (= -5 (median [50000 -6 43 -99 -5])))
  (is (= 2 (median [1 2 3])))
  (is (= 2.5 (float (median [1 2 3 4])))))
