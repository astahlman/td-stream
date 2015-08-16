(ns td-bot.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))

(defn bot
  [{:keys [domain]}]
  (domain {:test {:tweet-stream nil
                  :clock nil}
           :prod {:tweet-stream nil
                  :clock nil}}))
